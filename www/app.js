/* =====================================================================
   원유가격 정책 시뮬레이터: 브라우저 동작
   1) 한/영 전환: data-ko / data-en 문구를 바꾼다 (입력값은 그대로)
   2) 상단 메뉴: 마우스를 올리면 탭 안의 구역 목록이 펼쳐지고, 누르면 그 구역으로 이동
   3) 파라미터 숫자 칸 ↔ 슬라이더 동기화
   4) 상단 아이콘·제목을 누르면 홈(시뮬레이터)으로
   5) 사이드바 여닫기 버튼 이름
   ===================================================================== */
(function () {
  var TITLE = { ko: '원유가격 정책 시뮬레이터', en: 'Raw Milk Price Policy Simulator' };
  var SIDEBAR = { ko: ['설정 메뉴 닫기', '설정 메뉴 열기'], en: ['Close settings', 'Open settings'] };
  var lang = 'ko';

  /* ---- 1) 언어 ---- */
  function applyLang(l) {
    var changed = (l === 'en' ? 'en' : 'ko') !== lang;
    lang = (l === 'en') ? 'en' : 'ko';
    if (changed) { document.body.classList.add('lang-swap'); setTimeout(function () { document.body.classList.remove('lang-swap'); }, 260); }
    document.documentElement.lang = lang;
    document.title = TITLE[lang];
    document.querySelectorAll('[data-en]').forEach(function (el) {
      var v = el.getAttribute('data-' + lang);
      if (v === null) return;
      if (el.hasAttribute('data-html')) el.innerHTML = v; else el.textContent = v;
    });
    labelToggle();
  }

  /* ---- 5) 사이드바 버튼 이름 ---- */
  function labelToggle() {
    document.querySelectorAll('.bslib-sidebar-layout > .collapse-toggle').forEach(function (btn) {
      var t = SIDEBAR[lang][btn.getAttribute('aria-expanded') === 'true' ? 0 : 1];
      btn.setAttribute('title', t); btn.setAttribute('aria-label', t);
    });
  }

  /* ---- 2) 상단 메뉴 펼침 목록 ---- */
  // 묶인 카드 안의 탭 열기: "카드id:탭값"
  function openSubtab(sub) {
    if (!sub) return;
    var parts = sub.split(':');
    var a = document.querySelector('#' + parts[0] + ' a[data-value="' + parts[1] + '"]') ||
            document.querySelector('[data-tabsetid] a[data-value="' + parts[1] + '"]');
    if (a && !a.classList.contains('active')) bootstrap.Tab.getOrCreateInstance(a).show();
  }
  function goTab(tab, target, sub) {
    var link = document.querySelector('.navbar a.nav-link[data-value="' + tab + '"]');
    if (!link) return;
    openSubtab(sub);
    var go = function () {
      var el = target && document.getElementById(target);
      if (el) el.scrollIntoView({ behavior: 'smooth', block: 'start' });
      else { var m = document.querySelector('.bslib-sidebar-layout > .main'); if (m) m.scrollTo({ top: 0, behavior: 'smooth' }); window.scrollTo({ top: 0, behavior: 'smooth' }); }
    };
    if (link.classList.contains('active')) { go(); return; }
    // 숨어 있던 탭의 그래프·표는 탭이 열린 뒤에 그려져 높이가 바뀐다. 다 그려지면(shiny:idle) 한 번 더 맞춘다.
    link.addEventListener('shown.bs.tab', function once() {
      link.removeEventListener('shown.bs.tab', once);
      setTimeout(go, 60);
      if (window.jQuery) jQuery(document).one('shiny:idle', function () { setTimeout(go, 50); });
      setTimeout(go, 900);
    });
    bootstrap.Tab.getOrCreateInstance(link).show();
  }
  function buildNavDrops() {
    var mega = document.getElementById('nav-mega');
    var nav = document.querySelector('.bslib-page-navbar > .navbar');
    if (!mega || !nav) return;
    nav.appendChild(mega);
    mega.removeAttribute('hidden');
    // 탭마다 data-tab을 달아, 마우스를 올린 탭의 열을 강조한다
    document.querySelectorAll('.navbar a.nav-link[data-value]').forEach(function (link) {
      var li = link.closest('.nav-item'); if (li) { li.setAttribute('data-tab', link.getAttribute('data-value')); li.classList.add('has-mega'); }
      link.setAttribute('aria-haspopup', 'true');
    });
    // 각 열을 해당 탭의 왼쪽 끝에 맞춘다
    var place = function () {
      // 각 열의 왼쪽 끝과 폭을 해당 탭과 같게 한다 (탭 사이 간격도 열 사이 간격으로 옮긴다)
      var navRect = nav.getBoundingClientRect(), x = null;
      mega.querySelector('.mega-cols').style.setProperty('--mega-left', '0px');
      mega.querySelectorAll('.mega-col').forEach(function (col) {
        var li = document.querySelector('.navbar .nav-item[data-tab="' + col.getAttribute('data-tab') + '"]');
        if (!li) return;
        var r = li.getBoundingClientRect(), left = r.left - navRect.left;
        col.style.marginLeft = (x === null ? left : left - x) + 'px';
        col.style.width = r.width + 'px';
        x = left + r.width;
      });
    };
    place();
    window.addEventListener('resize', place);
    if (document.fonts && document.fonts.ready) document.fonts.ready.then(place);
    setTimeout(place, 500);
    var close = function () { nav.classList.add('mega-closed'); setTimeout(function () { nav.classList.remove('mega-closed'); }, 400); };
    document.addEventListener('click', function (e) {
      var a = e.target.closest('.nav-drop-item');
      if (!a) return;
      e.preventDefault();
      goTab(a.getAttribute('data-tab'), a.getAttribute('data-target'), a.getAttribute('data-subtab'));
      a.blur(); close();
    });
  }

  /* ---- 6) 가로로 넘기는 카드 ----
     이름 단추, 좌우 화살표, 키보드 ← →로 넘긴다. 트랙패드 가로 쓸기와 Shift + 휠은 브라우저 기본 동작으로 된다.
     세로 휠은 페이지 스크롤에 그대로 두어, 카드 위에서 스크롤이 막히지 않게 한다. */
  function initCarousels() {
    document.querySelectorAll('.carousel-x').forEach(function (c) {
      var track = c.querySelector('.cx-track');
      var slides = track.children, n = slides.length;
      var dots = c.querySelectorAll('.cx-dot');
      var prev = c.querySelector('.cx-prev'), next = c.querySelector('.cx-next'), count = c.querySelector('.cx-count');
      var cur = 0;
      var idx = function () { var w = track.clientWidth; return w ? Math.round(track.scrollLeft / w) : cur; };
      var paint = function (i) {
        cur = i;
        dots.forEach(function (d, j) { d.classList.toggle('on', j === i); d.setAttribute('aria-selected', j === i ? 'true' : 'false'); });
        prev.disabled = (i === 0); next.disabled = (i === n - 1);
        count.textContent = (i + 1) + ' / ' + n;
      };
      var go = function (i, smooth) {
        i = Math.max(0, Math.min(n - 1, i));
        track.scrollTo({ left: i * track.clientWidth, behavior: smooth === false ? 'auto' : 'smooth' });
        paint(i);
      };
      prev.addEventListener('click', function () { go(cur - 1); });
      next.addEventListener('click', function () { go(cur + 1); });
      dots.forEach(function (d, j) { d.addEventListener('click', function () { go(j); }); });
      var t; track.addEventListener('scroll', function () { clearTimeout(t); t = setTimeout(function () { paint(idx()); }, 80); });
      track.addEventListener('keydown', function (e) {
        if (e.key === 'ArrowRight') { e.preventDefault(); go(cur + 1); }
        if (e.key === 'ArrowLeft')  { e.preventDefault(); go(cur - 1); }
      });
      // 폭이 바뀌면(사이드바 여닫기, 창 크기) 보던 장을 그대로 맞춘다
      if (window.ResizeObserver) new ResizeObserver(function () { if (track.clientWidth) track.scrollLeft = cur * track.clientWidth; }).observe(track);
      paint(0);
    });
  }

  /* ---- 4) 홈 ---- */
  function bindHome() {
    var brand = document.querySelector('.navbar .navbar-brand');
    if (!brand) return;
    brand.setAttribute('role', 'link'); brand.setAttribute('tabindex', '0');
    brand.style.cursor = 'pointer';
    var home = function (e) { e.preventDefault(); goTab('sim', null); };
    brand.addEventListener('click', home);
    brand.addEventListener('keydown', function (e) { if (e.key === 'Enter' || e.key === ' ') home(e); });
  }

  /* ---- 3) 숫자 칸 ↔ 슬라이더 ---- */
  function decimals(step) { var s = String(step); return s.indexOf('.') < 0 ? 0 : s.length - s.indexOf('.') - 1; }
  function syncNumFromSlider(id, value) {
    document.querySelectorAll('input.param-num[data-for="' + id + '"]').forEach(function (n) {
      if (document.activeElement === n) return;
      var v = (+value).toFixed(decimals(n.step || 1));
      if (n.value !== v) { n.value = v; n.classList.remove('flash'); void n.offsetWidth; n.classList.add('flash'); }
    });
  }
  function bindParams() {
    document.addEventListener('change', function (e) {
      var n = e.target;
      if (!n.classList || !n.classList.contains('param-num')) return;
      var id = n.getAttribute('data-for');
      var $s = window.jQuery && jQuery('#' + id);
      var s = $s && $s.data('ionRangeSlider');
      if (!s) return;
      var o = s.options, v = parseFloat(String(n.value).replace('−', '-'));
      if (isNaN(v)) { n.value = (+s.result.from).toFixed(decimals(o.step)); return; }
      v = Math.min(o.max, Math.max(o.min, v));                      // 범위를 벗어나면 끝값으로
      v = Math.round((v - o.min) / o.step) * o.step + o.min;          // 단위에 맞춤
      v = +v.toFixed(decimals(o.step));
      n.value = v.toFixed(decimals(o.step));
      s.update({ from: v });
      $s.trigger('change');
    });
    document.addEventListener('keydown', function (e) {
      if (e.key === 'Enter' && e.target.classList && e.target.classList.contains('param-num')) e.target.blur();
    });
  }

  function formatNums() {
    document.querySelectorAll('input.param-num').forEach(function (n) {
      if (n.value !== '') n.value = (+n.value).toFixed(decimals(n.step || 1));
    });
  }
  document.addEventListener('DOMContentLoaded', function () {
    formatNums();
    buildNavDrops();
    initCarousels();
    bindHome();
    bindParams();
    document.querySelectorAll('.bslib-sidebar-layout > .collapse-toggle').forEach(function (btn) {
      new MutationObserver(labelToggle).observe(btn, { attributes: true, attributeFilter: ['aria-expanded'] });
    });
    var checked = document.querySelector('input[name="lang"]:checked');
    applyLang(checked ? checked.value : 'ko');
  });

  /* ---- 7) 값이 바뀌면 숫자가 이전 값에서 새 값으로 굴러간다 ---- */
  var REDUCED = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  function parseNum(t) {
    var m = String(t).match(/^([\s\S]*?)([+\u2212-]?)(\u20a9?)([\d,]+(?:\.\d+)?)([\s\S]*)$/);
    if (!m) return null;
    var n = m[4], d = n.indexOf('.') < 0 ? 0 : n.length - n.indexOf('.') - 1;
    return { pre: m[1], plus: m[2] === '+', cur: m[3], val: (m[2] === '\u2212' || m[2] === '-' ? -1 : 1) * parseFloat(n.replace(/,/g, '')), d: d, post: m[5] };
  }
  function fmtNum(v, tpl) {
    var a = Math.abs(v).toLocaleString('en-US', { minimumFractionDigits: tpl.d, maximumFractionDigits: tpl.d });
    var sign = v < 0 && +a.replace(/,/g, '') !== 0 ? '\u2212' : (tpl.plus && v > 0 ? '+' : '');
    return tpl.pre + sign + tpl.cur + a + tpl.post;
  }
  function tween(el, fromText, toText) {
    var a = parseNum(fromText), b = parseNum(toText);
    if (REDUCED || !a || !b || a.pre !== b.pre || a.post !== b.post || a.cur !== b.cur || a.val === b.val) return;
    el.textContent = fmtNum(a.val, b);   // 새 값이 한 프레임 먼저 보이지 않도록 이전 값에서 시작
    var t0 = performance.now(), dur = 420;
    var step = function (t) {
      var k = Math.min(1, (t - t0) / dur), e = 1 - Math.pow(1 - k, 3);
      el.textContent = fmtNum(a.val + (b.val - a.val) * e, b);
      if (k < 1) requestAnimationFrame(step); else el.textContent = toText;
    };
    requestAnimationFrame(step);
  }
  var KPI = /^vb_(ps|cs|g|dw)$/, HERO = /^(hero_title|welfare_title)$/;
  function bindTweens() {
    jQuery(document).on('shiny:value', function (e) {
      var el = document.getElementById(e.name);
      if (!el) return;
      if (KPI.test(e.name)) {
        var old = el.textContent;
        setTimeout(function () {
          if (old && old !== el.textContent) {
            tween(el, old, el.textContent);
            var card = el.closest('.kpi-card');
            if (card) { card.classList.remove('updated'); void card.offsetWidth; card.classList.add('updated'); }
          }
        }, 0);
      } else if (HERO.test(e.name)) {
        var olds = [].map.call(el.querySelectorAll('.hl-gov, .hl-dwl'), function (x) { return x.textContent; });
        setTimeout(function () {
          el.querySelectorAll('.hl-gov, .hl-dwl').forEach(function (x, i) { if (olds[i]) tween(x, olds[i], x.textContent); });
        }, 0);
      }
    });
  }

  /* ---- 8) 시작 화면: 첫 계산이 끝나면 걷어 낸다 ---- */
  function hideLoading() {
    var l = document.getElementById('app-loading');
    if (!l || l.classList.contains('done')) return;
    l.classList.add('done');
    setTimeout(function () { if (l.parentNode) l.parentNode.removeChild(l); }, 400);
  }
  setTimeout(hideLoading, 20000);   // 무슨 일이 있어도 20초 뒤에는 걷어 낸다

  if (window.jQuery) {
    bindTweens();
    jQuery(document).one('shiny:idle', function () { setTimeout(hideLoading, 150); });
    jQuery(document).on('shiny:inputchanged', function (e) {
      if (e.name === 'lang') applyLang(e.value);
      else if (document.querySelector('input.param-num[data-for="' + e.name + '"]')) syncNumFromSlider(e.name, e.value);
    });
    jQuery(document).on('shiny:connected', function () {
      var checked = document.querySelector('input[name="lang"]:checked');
      applyLang(checked ? checked.value : 'ko');
    });
  }
})();
