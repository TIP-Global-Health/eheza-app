import { resetDevice } from './helpers/device';

/**
 * Playwright global setup: creates a fresh device node with a known
 * pairing code before the E2E tests run.
 *
 * Device pairing codes are single-use (deleted after pairing), so we
 * need a fresh device for each test run.
 */
export default function globalSetup() {
  resetDevice();
}
