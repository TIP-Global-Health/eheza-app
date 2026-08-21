/**
 * Visible cursor and click marks for QA recordings.
 *
 * Same idea as the e2e `installCursorScript` (client/e2e/helpers/cursor.ts): draw the
 * pointer into the page so the recording shows where the clicks land. Two things differ,
 * both because a QA run captures one frame per action rather than continuous video:
 *
 * - The e2e cursor flashes for 300ms on mousedown. A QA frame is taken a second or more
 *   after the click, so that flash would always be over. Here the mark of the last click
 *   STAYS until the next click replaces it, and the two before it stay in a dimmer trail.
 * - Nothing is driven by a timer or requestAnimationFrame. A hidden tab throttles timers
 *   and stops rAF entirely (see pitfalls), which would leave animated marks frozen or
 *   never drawn. Everything here is set inline the moment the event arrives.
 *
 * Elm rewrites the DOM as it renders, so every element is re-attached if it goes missing.
 * Re-inject after each navigation — a page load wipes it.
 */
(function () {
  var ID = 'qa-cursor';
  var TRAIL = 3; // the last click, plus two dimmer ones behind it

  function styleEl(el, s) {
    for (var k in s) el.style[k] = s[k];
    return el;
  }

  function attach(el) {
    if (document.body && el.parentNode !== document.body) document.body.appendChild(el);
    return el;
  }

  function cursor() {
    var c = document.getElementById(ID);
    if (!c) {
      c = styleEl(document.createElement('div'), {
        position: 'fixed', left: '-100px', top: '-100px',
        width: '22px', height: '22px', borderRadius: '50%',
        background: 'rgba(255,0,0,0.45)', border: '2px solid red',
        transform: 'translate(-50%,-50%)',
        pointerEvents: 'none', zIndex: '2147483647',
      });
      c.id = ID;
    }
    return attach(c);
  }

  var marks = [];

  function paint() {
    // Newest first: bright and labelled, then fading behind it.
    for (var i = 0; i < marks.length; i++) {
      var m = marks[i];
      var alpha = i === 0 ? 1 : (i === 1 ? 0.45 : 0.2);
      var size = i === 0 ? 46 : 34;
      styleEl(m.ring, {
        width: size + 'px', height: size + 'px',
        borderColor: 'rgba(255,0,0,' + alpha + ')',
        background: 'rgba(255,0,0,' + (alpha * 0.18) + ')',
      });
      m.label.style.opacity = i === 0 ? '1' : '0';
      attach(m.ring);
      attach(m.label);
    }
  }

  function mark(x, y) {
    var ring = styleEl(document.createElement('div'), {
      position: 'fixed', left: x + 'px', top: y + 'px',
      borderRadius: '50%', borderStyle: 'solid', borderWidth: '3px',
      transform: 'translate(-50%,-50%)',
      pointerEvents: 'none', zIndex: '2147483646',
    });
    var label = styleEl(document.createElement('div'), {
      position: 'fixed', left: (x + 30) + 'px', top: (y - 10) + 'px',
      font: '600 13px system-ui, sans-serif', color: '#fff',
      background: 'rgba(200,0,0,0.92)', padding: '2px 7px', borderRadius: '4px',
      pointerEvents: 'none', zIndex: '2147483646', whiteSpace: 'nowrap',
    });
    label.textContent = 'click';

    marks.unshift({ ring: ring, label: label });
    while (marks.length > TRAIL) {
      var old = marks.pop();
      if (old.ring.parentNode) old.ring.parentNode.removeChild(old.ring);
      if (old.label.parentNode) old.label.parentNode.removeChild(old.label);
    }
    paint();
  }

  function move(e) {
    var c = cursor();
    c.style.left = e.clientX + 'px';
    c.style.top = e.clientY + 'px';
    paint(); // re-attach anything Elm removed on its last render
  }

  document.addEventListener('mousemove', move, true);
  document.addEventListener('pointermove', move, true);
  document.addEventListener('mousedown', function (e) { move(e); mark(e.clientX, e.clientY); }, true);
  document.addEventListener('pointerdown', function (e) { move(e); mark(e.clientX, e.clientY); }, true);

  cursor();
  return 'qa-cursor installed';
})();
