// Small, delegated helpers so HTML rendered by the server needs no inline JS.
(function () {
  function send(id, value) {
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue(id, value, { priority: 'event' });
  }

  // <button data-sn-input="ns-id" data-sn-value="...">: send the value to that input.
  $(document).on('click', '[data-sn-input]', function (e) {
    e.preventDefault();
    send(this.getAttribute('data-sn-input'), this.getAttribute('data-sn-value'));
  });

  // Buttons with data-sn-busy="Lagrer": show at once that something is
  // happening, and block double clicks, until the server is idle again.
  $(document).on('click', 'button[data-sn-busy]', function () {
    var b = this;
    if (b.classList.contains('is-busy')) return;
    b.setAttribute('data-sn-label', b.innerHTML);
    b.classList.add('is-busy');
    b.disabled = true;
    b.innerHTML = '<span class="spinner-border spinner-border-sm" aria-hidden="true"></span> ' +
      $('<div>').text(b.getAttribute('data-sn-busy')).html() + ' \u2026';
  });
  $(document).on('shiny:idle', function () {
    document.querySelectorAll('button.is-busy').forEach(function (b) {
      b.classList.remove('is-busy');
      b.disabled = false;
      if (b.hasAttribute('data-sn-label')) b.innerHTML = b.getAttribute('data-sn-label');
    });
  });

  // Text field + button: Enter or the button sends the typed text in one go
  // (avoids losing the last characters to Shiny's input debounce).
  // With data-sn-key, {key, value} is sent, so one input can serve many fields
  // (e.g. a comment field per proposal).
  function submitValue(el, value) {
    var key = el.getAttribute('data-sn-key');
    return key === null ? value : { key: key, value: value, at: Date.now() };
  }
  $(document).on('keydown', 'input[data-sn-submit]', function (e) {
    if (e.key === 'Enter') {
      e.preventDefault();
      send(this.getAttribute('data-sn-submit'), submitValue(this, this.value));
    }
  });
  $(document).on('click', 'button[data-sn-submit-for]', function () {
    var field = document.getElementById(this.getAttribute('data-sn-submit-for'));
    if (field) send(this.getAttribute('data-sn-submit'), submitValue(this, field.value));
  });

  // Text typed in fields marked .sn-keep survives when the server redraws the
  // part of the page they are in (e.g. a comment being written while another
  // trainer's change arrives).
  $(document).on('shiny:value', function (e) {
    var kept = [];
    $(e.target).find('input.sn-keep').each(function () {
      if (this.id && this.value) {
        kept.push({ id: this.id, value: this.value, focus: document.activeElement === this,
                    pos: this.selectionStart });
      }
    });
    if (!kept.length) return;
    setTimeout(function () {
      kept.forEach(function (k) {
        var el = document.getElementById(k.id);
        if (!el || el.value) return;
        el.value = k.value;
        if (k.focus) { el.focus(); try { el.setSelectionRange(k.pos, k.pos); } catch (err) {} }
      });
    }, 0);
  });

  // <details data-sn-toggle="ns-id">: report open/closed so a re-render keeps it.
  document.addEventListener('toggle', function (e) {
    var d = e.target;
    if (d && d.getAttribute && d.getAttribute('data-sn-toggle') && window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(d.getAttribute('data-sn-toggle'), d.open);
    }
  }, true);

  $(document).on('shiny:connected', function () {
    Shiny.addCustomMessageHandler('sn-clear-input', function (id) {
      var field = document.getElementById(id);
      if (field) { field.value = ''; field.focus(); }
    });
  });

  // Focus the tag field when the tag dialog opens.
  $(document).on('shown.bs.modal', function () {
    var field = document.querySelector('.modal input.sn-tag-input');
    if (field) field.focus();
  });
  // ---------------------------------------------------------------------------
  // Group editor board (.sn-board). Works two ways on the same HTML:
  //  - mouse/pen: drag a card (.sn-mcard) onto a column (.sn-col);
  //  - touch, or a click without dragging: pick a card, then "Plasser her".
  // The card is moved in the page at once, and the server is told with
  // {member, group} on the input named in the board's data-sn-move.
  var drag = null;

  function updateBoard(board) {
    board.querySelectorAll('.sn-col[data-group]').forEach(function (col) {
      var n = col.querySelectorAll('.sn-col-body .sn-mcard').length;
      var count = col.querySelector('.sn-col-count');
      if (count) count.textContent = n;
      var body = col.querySelector('.sn-col-body');
      if (body) body.classList.toggle('is-empty', n === 0);
    });
  }

  function moveCard(card, col) {
    var board = card.closest('.sn-board');
    var body = col.querySelector('.sn-col-body');
    if (!board || !body || card.parentNode === body) return;
    // Keep each column sorted by name.
    var key = card.getAttribute('data-sort') || '';
    var before = null;
    body.querySelectorAll('.sn-mcard').forEach(function (c) {
      if (!before && (c.getAttribute('data-sort') || '') > key) before = c;
    });
    if (before) body.insertBefore(card, before);
    else body.insertBefore(card, body.querySelector('.sn-col-empty'));
    updateBoard(board);
    send(board.getAttribute('data-sn-move'), {
      member: card.getAttribute('data-member'),
      group: col.getAttribute('data-group'),
      at: Date.now()
    });
  }

  function disarm(board) {
    if (!board) return;
    board.classList.remove('has-armed');
    board.querySelectorAll('.sn-mcard.armed').forEach(function (c) { c.classList.remove('armed'); });
    board.querySelectorAll('.sn-col.has-armed-card').forEach(function (c) { c.classList.remove('has-armed-card'); });
  }

  function arm(card) {
    var board = card.closest('.sn-board');
    var was = card.classList.contains('armed');
    disarm(board);
    if (was) return;
    card.classList.add('armed');
    board.classList.add('has-armed');
    var here = card.closest('.sn-col');
    if (here) here.classList.add('has-armed-card');
    var name = card.querySelector('.sn-mcard-name');
    var text = board.querySelector('.sn-armed-text');
    if (text) text.textContent = (name ? name.textContent : '') + ' er valgt. Flytt til:';
    // One button per other column in the (sticky) banner, so a player can be
    // placed without scrolling to the group.
    var targets = board.querySelector('.sn-armed-targets');
    if (targets) {
      targets.innerHTML = '';
      board.querySelectorAll('.sn-col[data-group]').forEach(function (col) {
        if (col === here) return;
        var b = document.createElement('button');
        b.type = 'button';
        b.className = 'btn btn-sm btn-primary sn-armed-target';
        b.setAttribute('data-target-group', col.getAttribute('data-group'));
        var label = col.getAttribute('data-group') || 'Ikke fordelt';
        b.textContent = label;
        targets.appendChild(b);
      });
    }
  }

  $(document).on('click', '.sn-board .sn-armed-target', function () {
    var board = this.closest('.sn-board');
    var card = board.querySelector('.sn-mcard.armed');
    var group = this.getAttribute('data-target-group');
    var col = null;
    board.querySelectorAll('.sn-col[data-group]').forEach(function (c) {
      if (c.getAttribute('data-group') === group) col = c;
    });
    if (card && col) moveCard(card, col);
    disarm(board);
  });

  $(document).on('click', '.sn-board .sn-place', function () {
    var board = this.closest('.sn-board');
    var card = board.querySelector('.sn-mcard.armed');
    if (card) moveCard(card, this.closest('.sn-col'));
    disarm(board);
  });
  $(document).on('click', '.sn-board .sn-disarm', function () { disarm(this.closest('.sn-board')); });
  $(document).on('keydown', '.sn-board .sn-mcard', function (e) {
    if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); arm(this); }
    if (e.key === 'Escape') disarm(this.closest('.sn-board'));
  });

  // Renaming a group: send {from, to} when the field is left or Enter is pressed.
  $(document).on('change', '.sn-board .sn-col-name', function () {
    send(this.getAttribute('data-sn-rename'), { from: this.getAttribute('data-label'), to: this.value, at: Date.now() });
  });
  $(document).on('keydown', '.sn-board .sn-col-name', function (e) {
    if (e.key === 'Enter') { e.preventDefault(); this.blur(); }
  });

  document.addEventListener('pointerdown', function (e) {
    var card = e.target.closest && e.target.closest('.sn-board .sn-mcard');
    if (!card || e.button > 0) return;
    drag = { card: card, x: e.clientX, y: e.clientY, type: e.pointerType, moving: false, ghost: null };
    window.addEventListener('pointermove', onMove, { passive: false });
    window.addEventListener('pointerup', onUp);
    window.addEventListener('pointercancel', onCancel);
  });

  function columnAt(x, y) {
    var el = document.elementFromPoint(x, y);
    return el && el.closest('.sn-board .sn-col[data-group]');
  }

  function onMove(e) {
    if (!drag) return;
    var dx = e.clientX - drag.x, dy = e.clientY - drag.y;
    if (!drag.moving) {
      // Fingers scroll the page; only mouse and pen drag.
      if (drag.type === 'touch' || (Math.abs(dx) < 6 && Math.abs(dy) < 6)) return;
      drag.moving = true;
      disarm(drag.card.closest('.sn-board'));
      var r = drag.card.getBoundingClientRect();
      drag.dx = drag.x - r.left; drag.dy = drag.y - r.top;
      drag.ghost = drag.card.cloneNode(true);
      drag.ghost.classList.add('sn-ghost');
      drag.ghost.style.width = r.width + 'px';
      document.body.appendChild(drag.ghost);
      drag.card.classList.add('sn-dragging');
    }
    e.preventDefault();
    drag.ghost.style.left = (e.clientX - drag.dx) + 'px';
    drag.ghost.style.top = (e.clientY - drag.dy) + 'px';
    document.querySelectorAll('.sn-col.dragover').forEach(function (c) { c.classList.remove('dragover'); });
    var col = columnAt(e.clientX, e.clientY);
    if (col) col.classList.add('dragover');
    // Scroll the board sideways near its edges, and the page near the top/bottom.
    var cols = drag.card.closest('.sn-board-cols');
    if (cols) {
      var b = cols.getBoundingClientRect();
      if (e.clientX < b.left + 40) cols.scrollLeft -= 14;
      else if (e.clientX > b.right - 40) cols.scrollLeft += 14;
    }
    if (e.clientY < 40) window.scrollBy(0, -14);
    else if (e.clientY > window.innerHeight - 40) window.scrollBy(0, 14);
  }

  function stop() {
    window.removeEventListener('pointermove', onMove);
    window.removeEventListener('pointerup', onUp);
    window.removeEventListener('pointercancel', onCancel);
    if (drag && drag.ghost) drag.ghost.remove();
    if (drag) drag.card.classList.remove('sn-dragging');
    document.querySelectorAll('.sn-col.dragover').forEach(function (c) { c.classList.remove('dragover'); });
  }

  function onUp(e) {
    if (!drag) return;
    var d = drag;
    stop();
    drag = null;
    if (d.moving) {
      var col = columnAt(e.clientX, e.clientY);
      if (col) moveCard(d.card, col);
    } else if (Math.abs(e.clientX - d.x) < 10 && Math.abs(e.clientY - d.y) < 10) {
      arm(d.card);   // a tap or a click without dragging
    }
  }

  function onCancel() { stop(); drag = null; }

  // A tap on a card is handled on pointerup. Stop the browser's extra "click"
  // after the tap, which could otherwise land on a button that just appeared.
  document.addEventListener('touchend', function (e) {
    if (e.target.closest && e.target.closest('.sn-board .sn-mcard')) e.preventDefault();
  }, { passive: false });
})();
