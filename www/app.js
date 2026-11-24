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
    lang = (l === 'en') ? 'en' : 'ko';
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
    var holder = document.getElementById('nav-drops');
    if (!holder) return;
    holder.querySelectorAll('.nav-drop').forEach(function (drop) {
      var tab = drop.getAttribute('data-tab');
      var link = document.querySelector('.navbar a.nav-link[data-value="' + tab + '"]');
      if (!link) return;
      var li = link.closest('.nav-item') || link.parentElement;
      li.classList.add('has-drop');
      link.setAttribute('aria-haspopup', 'true');
      li.appendChild(drop);
    });
    holder.remove();
    document.addEventListener('click', function (e) {
      var a = e.target.closest('.nav-drop-item');
      if (!a) return;
      e.preventDefault();
      goTab(a.getAttribute('data-tab'), a.getAttribute('data-target'), a.getAttribute('data-subtab'));
      a.blur();
      var li = a.closest('.nav-item'); if (li) { li.classList.add('drop-closed'); setTimeout(function () { li.classList.remove('drop-closed'); }, 400); }
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
      if (document.activeElement !== n) n.value = (+value).toFixed(decimals(n.step || 1));
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

  if (window.jQuery) {
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
