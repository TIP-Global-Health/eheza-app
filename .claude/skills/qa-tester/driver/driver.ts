/**
 * QA driver: one visible browser, kept open for a whole QA run, that takes
 * commands over HTTP on 127.0.0.1 and runs them with the e2e helpers.
 *
 * Each command is the body of an async function `(page, h, qa) => ...`:
 *   page  the Playwright page showing the app
 *   h     every e2e helper module, e.g. h.prenatal.completeDangerSigns(page)
 *   qa    the extras below: state, shot, log, click, freshDevice
 * The reply is JSON: the command's return value, its error if any, how long it
 * took, and a short summary of the screen it left behind.
 *
 * Recording uses the browser's own screencast, so the video runs in real time
 * and can be started and stopped without reloading the page.
 *
 * Started and talked to by scripts/qa.sh; see SKILL.md for the commands.
 */
import { test, chromium, devices, BrowserContext, CDPSession, Locator, Page } from '@playwright/test';
import { execFileSync } from 'child_process';
import { randomBytes } from 'crypto';
import * as fs from 'fs';
import * as http from 'http';
import * as path from 'path';

import * as acuteIllness from '../../../../client/e2e/helpers/acute-illness';
import * as auth from '../../../../client/e2e/helpers/auth';
import * as caseManagement from '../../../../client/e2e/helpers/case-management';
import * as childScoreboard from '../../../../client/e2e/helpers/child-scoreboard';
import * as common from '../../../../client/e2e/helpers/common';
import * as device from '../../../../client/e2e/helpers/device';
import * as educationSession from '../../../../client/e2e/helpers/education-session';
import * as familyNutrition from '../../../../client/e2e/helpers/family-nutrition';
import * as groupSession from '../../../../client/e2e/helpers/group-session';
import * as hiv from '../../../../client/e2e/helpers/hiv';
import * as homeVisit from '../../../../client/e2e/helpers/home-visit';
import * as ncd from '../../../../client/e2e/helpers/ncd';
import * as nutrition from '../../../../client/e2e/helpers/nutrition';
import * as prenatal from '../../../../client/e2e/helpers/prenatal';
import * as progressReport from '../../../../client/e2e/helpers/progress-report';
import * as reports from '../../../../client/e2e/helpers/reports';
import * as stockManagement from '../../../../client/e2e/helpers/stock-management';
import * as tuberculosis from '../../../../client/e2e/helpers/tuberculosis';
import * as wellChild from '../../../../client/e2e/helpers/well-child';
import { installCursorScript } from '../../../../client/e2e/helpers/cursor';
import { getClientPort } from '../../../../client/e2e/helpers/client-port';

const h = {
  acuteIllness, auth, caseManagement, childScoreboard, common, device, educationSession,
  familyNutrition, groupSession, hiv, homeVisit, ncd, nutrition, prenatal, progressReport,
  reports, stockManagement, tuberculosis, wellChild,
};

const CLIENT = path.resolve(__dirname, '../../../../client');
const STATE = path.join(CLIENT, 'qa-recordings/.driver');
const PROFILE = path.join(STATE, 'profile');
const SHOTS = path.join(STATE, 'shots');
const PORT = Number(process.env.QA_PORT || 9323);
const RECORDINGS = path.join(CLIENT, 'qa-recordings');
// Commands run code, so only a caller that can read this owner-only file may send them.
const TOKEN = randomBytes(32).toString('hex');
// Not the e2e code, so an e2e run re-keying its own device never touches this one.
const QA_PAIRING_CODE = '88888888';
const BASE_URL = `http://localhost:${getClientPort()}`;

// The same device the e2e runs use: iPad Mini metrics, mouse rather than touch, UTC.
const { defaultBrowserType: _ignored, ...ipadMini } = devices['iPad Mini'];

// AsyncFunction is not a global; take it from any async function.
const AsyncFunction = Object.getPrototypeOf(async () => {}).constructor;

/**
 * A short account of what the screen shows, so a command's result can be read
 * without a screenshot. Only visible elements are listed.
 */
async function screenState(page: Page) {
  // Elm draws on the next animation frame, so wait two before reading the screen.
  await page.evaluate(() => new Promise((r) => requestAnimationFrame(() => requestAnimationFrame(r))));
  return page.evaluate(() => {
    const visible = (el: Element) => {
      const r = el.getBoundingClientRect();
      return r.width > 0 && r.height > 0 && getComputedStyle(el).visibility !== 'hidden';
    };
    const text = (el: Element) =>
      ((el as HTMLElement).innerText || '').trim().replace(/\s+/g, ' ').slice(0, 80);
    const all = (selector: string) => Array.from(document.querySelectorAll(selector)).filter(visible);
    const unique = (list: string[]) => Array.from(new Set(list.filter(Boolean)));

    const marked = (el: Element, label: string) => {
      const flags = ['active', 'completed', 'disabled'].filter((c) => el.classList.contains(c));
      if ((el as HTMLButtonElement).disabled && !flags.includes('disabled')) flags.push('disabled');
      return flags.length ? `${label} [${flags.join(', ')}]` : label;
    };

    const dialogs = unique(all('.ui.modal, .modal.visible, .ui.dimmer.active .content').map(text));
    const headings = unique(all('h1, h2, h3, .ui.header').map(text)).slice(0, 8);
    const tabs = all('.link-section').map((el) => marked(el, text(el)));
    const buttons = unique(
      all('button, .ui.button, .card .image + .content, .link-back, .action-icon')
        .map((el) => marked(el, text(el) || el.className.toString().split(' ').slice(-1)[0])),
    ).slice(0, 40);
    // Radios and checkboxes are drawn by CSS over a zero-size input, so judge
    // them by their label: "(x) Yes" is chosen, "( ) No" is not.
    const choices = Array.from(document.querySelectorAll('input[type=radio], input[type=checkbox]'))
      .map((el) => ({ input: el as HTMLInputElement, box: el.parentElement as Element }))
      .filter(({ box }) => box && visible(box))
      .map(({ input, box }) => `${input.checked ? '(x)' : '( )'} ${text(box)}`)
      .slice(0, 40);
    const fields = all('input:not([type=hidden]):not([type=radio]):not([type=checkbox]):not([type=file]), select, textarea')
      .slice(0, 40)
      .map((el) => {
        const input = el as HTMLInputElement;
        const box = input.closest('.form-input, .field');
        const name = input.name || (box ? box.className.toString() : input.className.toString());
        return `${name} = ${JSON.stringify(input.value)}`;
      });
    const counters = unique(all('.tasks-count, .count, .progress-bar').map(text)).slice(0, 5);
    return { url: location.hash || location.pathname, headings, dialogs, counters, tabs, buttons, choices, fields };
  });
}

/** Records the page in real time from the browser's screencast. */
class Recorder {
  private session: CDPSession | null = null;
  private frames: { file: string; at: number }[] = [];
  private dir = '';
  private output = '';

  get active() {
    return this.session !== null;
  }

  async start(page: Page, output: string) {
    if (this.session) throw new Error(`already recording ${this.output}`);
    this.output = output;
    this.dir = path.join(STATE, 'frames', path.basename(output, '.mp4'));
    fs.rmSync(this.dir, { recursive: true, force: true });
    fs.mkdirSync(this.dir, { recursive: true });
    this.frames = [];

    const session = await page.context().newCDPSession(page);
    session.on('Page.screencastFrame', (frame) => {
      const file = path.join(this.dir, `${String(this.frames.length).padStart(6, '0')}.jpg`);
      fs.writeFileSync(file, Buffer.from(frame.data, 'base64'));
      this.frames.push({ file, at: frame.metadata.timestamp ?? Date.now() / 1000 });
      session.send('Page.screencastFrameAck', { sessionId: frame.sessionId }).catch(() => {});
    });
    await session.send('Page.startScreencast', { format: 'jpeg', quality: 85, everyNthFrame: 1 });
    this.session = session;
    // The e2e click helper hovers and pauses before each click while this is set.
    process.env.RECORD = '1';
  }

  /** Stops, writes the .mp4, and returns its path and length in seconds. */
  async stop() {
    if (!this.session) throw new Error('not recording');
    const session = this.session;
    this.session = null;
    delete process.env.RECORD;
    await session.send('Page.stopScreencast').catch(() => {});
    await session.detach().catch(() => {});
    if (this.frames.length === 0) throw new Error('no frames were captured');

    // A frame lasts until the next one arrives; the screencast sends frames
    // only when the screen changes, so a still screen is one long frame.
    const lines: string[] = [];
    this.frames.forEach((frame, i) => {
      const next = this.frames[i + 1];
      const seconds = next ? Math.max(next.at - frame.at, 0.001) : 1;
      lines.push(`file '${frame.file}'`, `duration ${seconds.toFixed(3)}`);
    });
    // The concat demuxer ignores the last duration unless the file repeats.
    lines.push(`file '${this.frames[this.frames.length - 1].file}'`);
    const list = path.join(this.dir, 'list.txt');
    fs.writeFileSync(list, lines.join('\n') + '\n');

    fs.mkdirSync(path.dirname(this.output), { recursive: true });
    execFileSync('ffmpeg', [
      '-y', '-loglevel', 'error', '-f', 'concat', '-safe', '0', '-i', list,
      '-vf', 'scale=trunc(iw/2)*2:trunc(ih/2)*2', '-fps_mode', 'vfr',
      '-pix_fmt', 'yuv420p', '-c:v', 'libx264', '-crf', '23', '-movflags', '+faststart',
      this.output,
    ]);
    fs.rmSync(this.dir, { recursive: true, force: true });
    const total = this.frames[this.frames.length - 1].at - this.frames[0].at + 1;
    return { video: this.output, seconds: Math.round(total), frames: this.frames.length };
  }
}

test('qa driver', async () => {
  fs.mkdirSync(SHOTS, { recursive: true });
  const tokenFile = path.join(STATE, 'token');
  fs.rmSync(tokenFile, { force: true });
  fs.writeFileSync(tokenFile, TOKEN, { mode: 0o600 });
  const context: BrowserContext = await chromium.launchPersistentContext(PROFILE, {
    ...ipadMini,
    headless: false,
    hasTouch: false,
    isMobile: false,
    timezoneId: 'UTC',
    ignoreHTTPSErrors: true,
    baseURL: BASE_URL,
  });
  await context.addInitScript(installCursorScript());
  // The runner's timeout is off, so without these a missing element waits forever.
  // Helpers that need longer waits pass their own timeout.
  context.setDefaultTimeout(10000);
  context.setDefaultNavigationTimeout(30000);

  let page = context.pages()[0] ?? (await context.newPage());
  // Each distinct error once, with how often it came, so repeats do not bury the rest.
  let consoleErrors = new Map<string, number>();
  const noteError = (message: string) =>
    consoleErrors.set(message.slice(0, 200), (consoleErrors.get(message.slice(0, 200)) ?? 0) + 1);
  const watch = (p: Page) => {
    p.on('console', (m) => m.type() === 'error' && noteError(`${m.text()} ${m.location().url}`));
    p.on('pageerror', (e) => noteError(String(e)));
  };
  watch(page);
  // Reopen where the last driver left off; the app restores a page from its URL.
  const lastUrlFile = path.join(STATE, 'last-url');
  await page.goto(fs.existsSync(lastUrlFile) ? fs.readFileSync(lastUrlFile, 'utf-8') : '/');

  const recorder = new Recorder();
  let shotCount = 0;
  let logs: string[] = [];

  const qa = {
    state: () => screenState(page),
    /** Saves a screenshot of the viewport and returns its path. */
    shot: async (name = '') => {
      // Named by time, so a restarted driver never overwrites an earlier run's shots.
      const stamp = new Date().toISOString().replace(/[-:]/g, '').replace(/\..*/, '');
      shotCount += 1;
      const file = path.join(SHOTS, `${stamp}-${shotCount}${name ? '-' + name : ''}.png`);
      await page.screenshot({ path: file, scale: 'css' });
      return file;
    },
    log: (...parts: unknown[]) => {
      logs.push(parts.map((p) => (typeof p === 'string' ? p : JSON.stringify(p))).join(' '));
    },
    /**
     * Pairs a new device, signs in and syncs the health centre. Not titled "E2E...",
     * so the next e2e run's cleanup leaves it alone.
     */
    freshDevice: async (
      { pin = '1234', location = 'Nyange Health Center', healthCenter = 'Nyange Health Center' } = {},
    ) => {
      device.resetDevice(QA_PAIRING_CODE, 'QA Device');
      await auth.setupDevice(page, pin, location, healthCenter, QA_PAIRING_CODE);
    },
    /** Clicks the way a person does: a locator, or the first visible element with this text. */
    click: async (target: Locator | string) => {
      const locator = typeof target === 'string'
        ? page.getByText(target, { exact: false }).filter({ visible: true }).first()
        : target;
      await auth.click(locator, page);
    },
  };

  let stopped: () => void = () => {};
  const finished = new Promise<void>((resolve) => (stopped = resolve));

  // One command at a time, in the order they arrive.
  let queue: Promise<unknown> = Promise.resolve();
  const serial = <T>(work: () => Promise<T>) => {
    const result = queue.then(work, work);
    queue = result.catch(() => {});
    return result;
  };

  const reply = async (withShot: boolean, extra: Record<string, unknown>) => {
    let state: unknown = null;
    try {
      state = await screenState(page);
    } catch (error) {
      state = `unreadable: ${String(error).slice(0, 200)}`;
    }
    fs.writeFileSync(lastUrlFile, page.url());
    const out: Record<string, unknown> = { ...extra, state };
    if (withShot) out.shot = await qa.shot().catch((e) => `failed: ${e}`);
    if (logs.length) out.log = logs;
    if (consoleErrors.size) {
      out.consoleErrors = Array.from(consoleErrors, ([message, n]) => (n > 1 ? `${n}x ${message}` : message));
    }
    if (recorder.active) out.recording = true;
    logs = [];
    consoleErrors = new Map();
    return out;
  };

  const handle = async (req: http.IncomingMessage, body: string) => {
    const url = new URL(req.url ?? '/', 'http://driver');
    const withShot = url.searchParams.get('shot') === '1';

    switch (`${req.method} ${url.pathname}`) {
      case 'POST /run': {
        const started = Date.now();
        try {
          const value = await new AsyncFunction('page', 'h', 'qa', body)(page, h, qa);
          return reply(withShot, { ok: true, ms: Date.now() - started, value: value ?? null });
        } catch (error) {
          const message = String((error as Error)?.message ?? error).split('\n').slice(0, 12).join('\n');
          return reply(true, { ok: false, ms: Date.now() - started, error: message });
        }
      }
      case 'GET /state':
        return reply(withShot, { ok: true });
      case 'POST /record/start': {
        const output = path.resolve(url.searchParams.get('output') ?? '');
        if (!output.startsWith(RECORDINGS + path.sep) || !output.endsWith('.mp4')) {
          throw new Error(`output must be an .mp4 under ${RECORDINGS}`);
        }
        await recorder.start(page, output);
        return { ok: true, recording: output };
      }
      case 'POST /record/stop':
        return { ok: true, ...(await recorder.stop()) };
      case 'POST /stop':
        if (recorder.active) await recorder.stop().catch(() => {});
        stopped();
        return { ok: true };
      default:
        throw new Error(`unknown command ${req.method} ${url.pathname}`);
    }
  };

  // A web page can reach a localhost port too: refuse anything sent by a browser
  // (it carries Origin, or a rebound Host) or without the token.
  const trusted = (req: http.IncomingMessage) =>
    !req.headers.origin &&
    [`127.0.0.1:${PORT}`, `localhost:${PORT}`].includes(req.headers.host ?? '') &&
    req.headers.authorization === `Bearer ${TOKEN}`;

  const server = http.createServer((req, res) => {
    if (!trusted(req)) {
      res.statusCode = 403;
      res.end(JSON.stringify({ ok: false, error: 'forbidden' }));
      req.resume();
      return;
    }
    let body = '';
    req.on('data', (chunk) => (body += chunk));
    req.on('end', () => {
      serial(() => handle(req, body))
        .then((out) => res.end(JSON.stringify(out, null, 1)))
        .catch((error) => res.end(JSON.stringify({ ok: false, error: String(error) })));
    });
  });
  await new Promise<void>((resolve) => server.listen(PORT, '127.0.0.1', resolve));
  console.log(`QA driver listening on 127.0.0.1:${PORT}`);

  // A page closed by hand is replaced, so the run can go on.
  context.on('page', (p) => {
    page = p;
    watch(p);
  });

  await finished;
  server.close();
  await context.close();
});
