/**
 * Inject a visual cursor indicator into the page so that clicks
 * are visible in video recordings. Re-creates the cursor element
 * if the SPA removes it during navigation.
 */
export function installCursorScript(): string {
  return `
    function createCursorEl() {
      const el = document.createElement('div');
      el.id = 'pw-cursor';
      Object.assign(el.style, {
        position: 'fixed',
        top: '-50px',
        left: '-50px',
        width: '20px',
        height: '20px',
        borderRadius: '50%',
        backgroundColor: 'rgba(255, 0, 0, 0.5)',
        border: '2px solid red',
        pointerEvents: 'none',
        zIndex: '999999',
        transform: 'translate(-50%, -50%)',
        transition: 'none',
      });
      return el;
    }

    function getCursor() {
      let el = document.getElementById('pw-cursor');
      if (!el && document.body) {
        el = createCursorEl();
        document.body.appendChild(el);
      }
      return el;
    }

    function moveTo(x, y) {
      const c = getCursor();
      if (c) { c.style.left = x + 'px'; c.style.top = y + 'px'; }
    }

    function flash(x, y) {
      const c = getCursor();
      if (!c) return;
      c.style.left = x + 'px';
      c.style.top = y + 'px';
      c.style.backgroundColor = 'rgba(255, 0, 0, 0.9)';
      c.style.transform = 'translate(-50%, -50%) scale(1.5)';
      setTimeout(() => {
        c.style.backgroundColor = 'rgba(255, 0, 0, 0.5)';
        c.style.transform = 'translate(-50%, -50%) scale(1)';
      }, 300);
    }

    document.addEventListener('mousemove', (e) => moveTo(e.clientX, e.clientY), true);
    document.addEventListener('mousedown', (e) => flash(e.clientX, e.clientY), true);
    document.addEventListener('pointermove', (e) => moveTo(e.clientX, e.clientY), true);
    document.addEventListener('pointerdown', (e) => flash(e.clientX, e.clientY), true);
  `;
}
