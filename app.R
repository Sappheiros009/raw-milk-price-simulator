# =====================================================================
# 원유가격 연동제 정책 시뮬레이터 (LESE412 기말 프로젝트)
# 실행: R에서 shiny::runApp("이 폴더 경로")   (한글 자료 때문에 UTF-8 로캘 권장)
# 계산은 R/model.R 의 simulate() 하나만 사용한다.
# 기준연도 2013년, 자료는 낙농진흥회 낙농통계광장.
#
# 화면 설계 원칙
#   - 사용자: 탄력성과 원유가격을 바꿔 가며 결과를 예측하는 정책 분석가
#   - 핵심 메시지: "가격이 이러면, 탄력성이 이러면 → 결과는 이렇다"
#   - 일관성: 후생은 모두 '변화량(Δ, 부호 포함)'으로, 개념마다 색 하나
#       낙농가 = 초록, 유업체 = 빨강, 잉여 원유 처리 = 청회색, 사회 전체 = 금색
#   - 글꼴은 Pretendard 하나 (화면: www/webfonts, 그래프: fonts/SimSans = Pretendard 부분집합)
#
# 한/영 전환
#   - 고정 문구: L("한국어", "English")로 두 언어를 함께 넣고, 브라우저에서 바로 바꾼다(입력값 유지).
#   - 계산 결과 문구·그래프·표: 서버가 input$lang에 따라 다시 만든다.
#   - 영어 자료는 R/i18n.R
# =====================================================================
library(shiny)
library(bslib)
library(bsicons)
library(ggplot2)

source("R/data.R",  encoding = "UTF-8")
source("R/i18n.R",  encoding = "UTF-8")
source("R/model.R", encoding = "UTF-8")

# ---- 색 (www/style.css의 :root 변수와 같은 값) ------------------------------
COL <- list(brand = "#1C4536", brand800 = "#24584A", brand100 = "#E2F0E8",
            ink = "#1C2724", sub = "#3E4945", muted = "#5C6763", line = "#E1E6E3", line_strong = "#C9D1CD",
            period_sr = "#A3AFAA", period_lr = "#4E5A55")
CONCEPT <- list(
  farm = list(line = "#4A9A75", light = "#A9D6BF", ink = "#1E6B4B"),
  proc = list(line = "#D47660", light = "#F0B9AB", ink = "#A8452F"),
  gov  = list(line = "#7891AC", light = "#BFCDDB", ink = "#46607A"),
  dwl  = list(line = "#C2952F", light = "#EBD08F", ink = "#7A5700")
)

# ---- 그래프 글꼴 ----------------------------------------------------------------
PLOT_FONT <- "SimSans"
if (R.version$os == "emscripten") {
  font_dir <- "/home/web_user/fonts"
  dir.create(font_dir, recursive = TRUE, showWarnings = FALSE)
  if (dir.exists("fonts")) {
    file.copy(list.files("fonts", pattern = "\\.ttf$", full.names = TRUE), font_dir, overwrite = TRUE)
  } else {
    for (f in c("Pretendard-Regular.ttf", "Pretendard-SemiBold.ttf")) try(download.file(
      paste0("https://cdn.jsdelivr.net/npm/pretendard@1.3.9/dist/public/static/alternative/", f),
      file.path(font_dir, f), mode = "wb", quiet = TRUE), silent = TRUE)
    PLOT_FONT <- "Pretendard"
  }
} else if (!any(grepl("SimSans", tryCatch(system("fc-list", intern = TRUE, ignore.stderr = TRUE),
                                          error = function(e) "")))) {
  PLOT_FONT <- if (.Platform$OS.type == "windows") "Malgun Gothic" else "sans"
}

theme_app <- bs_theme(
  version = 5, primary = COL$brand800, success = "#2E7D5B", danger = CONCEPT$proc$ink,
  bg = "#FFFFFF", fg = COL$ink,
  base_font = font_collection("Pretendard Variable", "Pretendard", "Malgun Gothic", "Apple SD Gothic Neo", "sans-serif"),
  "border-radius" = "0.75rem", "font-size-base" = "0.875rem"
)

# ---- 두 언어 문구 ------------------------------------------------------------------
# 화면에는 한국어를 먼저 넣고, 브라우저가 data-ko / data-en으로 바꾼다.
L <- function(ko, en, html = FALSE)
  span(class = "i18n", `data-ko` = ko, `data-en` = en, `data-html` = if (html) "1", if (html) HTML(ko) else ko)

# 정보 아이콘 툴팁: 툴팁 내용은 브라우저에서 직접 바꿀 수 없으므로 서버가 언어에 맞게 갱신한다
TIPS <- new.env()
tip_icon <- function(ko, en) {
  id <- paste0("tip_", length(ls(TIPS)) + 1)
  assign(id, c(ko = ko, en = en), envir = TIPS)
  tooltip(span(class = "tip-icon", tabindex = "0", bs_icon("info-circle", a11y = "sem", title = "i")), ko, id = id)
}
info_tip <- function(id) tooltip(span(class = "tip-icon", tabindex = "0", bs_icon("info-circle", a11y = "sem", title = "i")),
                                 "", id = id, placement = "bottom")
lab_tip <- function(ko, en, tko, ten) div(class = "lab", L(ko, en), tip_icon(tko, ten))

# ---- 선 아이콘 -----------------------------------------------------------------------
ICON <- list(
  cow = '<path d="M8 6.6 6.3 3.9M16 6.6l1.7-2.7"/><path d="M7.3 8.3C5.4 8.7 3.4 8.2 2.5 6.9c1.5-.6 3.3-.7 4.9-.2"/><path d="M16.7 8.3c1.9.4 3.9-.1 4.8-1.4-1.5-.6-3.3-.7-4.9-.2"/><path d="M7.6 13.4 7 8.6C7 6.9 9.2 5.6 12 5.6s5 1.3 5 3l-.6 4.8"/><rect x="6.5" y="13" width="11" height="7.5" rx="3.7"/><path d="M10 16.6v.1M14 16.6v.1M9.6 10.3v.1M14.4 10.3v.1"/>',
  milk = '<path d="M9 2.5h6"/><path d="M9.6 2.5v3.3c0 .5-.2 1-.6 1.3C7.7 8.1 7 9.5 7 11.1v8.4a2 2 0 0 0 2 2h6a2 2 0 0 0 2-2v-8.4c0-1.6-.7-3-2-4-.4-.3-.6-.8-.6-1.3V2.5"/><path d="M7 13.5h10"/>',
  factory = '<path d="M3 20.5v-10l5 3v-3l5 3v-3l5 3V3.5h3v17z"/><path d="M2 20.5h20"/><path d="M7 17h1.5M11.5 17H13M16 17h1.5"/>',
  truck = '<path d="M2.5 6.5h11v10h-11z"/><path d="M13.5 9.5h4l3 3.5v3.5h-7"/><circle cx="6.5" cy="17.5" r="2"/><circle cx="17" cy="17.5" r="2"/>',
  cart = '<path d="M2.5 3.5h3l2.2 11h10.8l2-7.5H6.4"/><circle cx="9.5" cy="19" r="1.5"/><circle cx="17" cy="19" r="1.5"/>',
  house = '<path d="M3.5 11 12 4l8.5 7"/><path d="M5.5 9.5v11h13v-11"/><path d="M10 20.5v-5h4v5"/>',
  sack = '<path d="M9 3.5h6l-1.6 3.2C16.6 8.3 19 11.8 19 15.5c0 2.8-1.7 5-4.5 5h-5C6.7 20.5 5 18.3 5 15.5c0-3.7 2.4-7.2 5.6-8.8z"/><path d="M10.4 6.7h3.2M9.5 15.5h5"/>',
  scale = '<path d="M12 3.5v17M8 20.5h8M4.5 7h15"/><path d="M4.5 7 2 13a2.5 2.5 0 0 0 5 0zM19.5 7 17 13a2.5 2.5 0 0 0 5 0z"/>',
  cycle = '<path d="M20 11a8 8 0 0 0-14.3-4.9L4 8"/><path d="M4 3.5V8h4.5"/><path d="M4 13a8 8 0 0 0 14.3 4.9L20 16"/><path d="M20 20.5V16h-4.5"/>'
)
li <- function(name, size = 24, cls = "")
  HTML(sprintf('<svg class="li %s" width="%d" height="%d" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">%s</svg>',
               cls, size, size, ICON[[name]]))

asset_src <- function(f) paste0(f, "?v=", substr(unname(tools::md5sum(file.path("www", f))), 1, 8))
ART_FILES <- sub("^il-(.*)\\.png$", "\\1", list.files("www", pattern = "^il-.*\\.png$"))
ART_SRC <- setNames(vapply(paste0("il-", ART_FILES, ".png"), asset_src, ""), ART_FILES)
art <- function(name, w, h = w, fallback = name, cls = "") {
  if (name %in% ART_FILES) tags$img(src = ART_SRC[[name]], class = trimws(paste("art", cls)),
                                     width = w, height = h, alt = "")
  else li(fallback, min(w, h), cls)
}
HERO_FILE <- c("hero.png", "hero.webp")[file.exists(file.path("www", c("hero.png", "hero.webp")))][1]
HERO_ART <- if (!is.na(HERO_FILE)) tags$img(src = asset_src(HERO_FILE), class = "hero-art", alt = "") else NULL

# ---- 숫자 표시 -----------------------------------------------------------------------
num <- function(x, d = 0) {
  x <- round(x, d); x[x == 0] <- 0
  sub("^-", "−", formatC(x, big.mark = ",", format = "f", digits = d))
}
sgn <- function(x, d = 0) paste0(ifelse(round(x, d) > 0, "+", ""), num(x, d))
# 언어별 단위: 억 원 ↔ ₩ bn (1억 원 = ₩0.1 bn), 천 톤 ↔ kt, 원/L ↔ KRW/L
money <- function(x, lg) if (lg == "en") {
  v <- x / 10; paste0(ifelse(round(v, 1) > 0, "+", ifelse(round(v, 1) < 0, "−", "")), "₩", num(abs(v), 1), " bn")
} else paste0(sgn(x), "억 원")
kt    <- function(x, lg) if (lg == "en") paste0(num(x), " kt") else paste0(num(x), "천 톤")
wonL  <- function(x, lg, d = 0) if (lg == "en") paste0(num(x, d), " KRW/L") else paste0(num(x, d), "원/L")
won   <- function(x, lg) if (lg == "en") paste0("₩", num(x)) else paste0(num(x), "원")
pct   <- function(x, d = 1) paste0(num(x, d), "%")
tr2   <- function(lg, ko, en) if (lg == "en") en else ko

status_chip <- function(s, lg = "ko") {
  cls <- switch(s, "확인 완료" = "ok", "확인 필요" = "check", "가상" = "virtual", "임시 값" = "temp", "-" = "none", "temp")
  as.character(tags$span(class = paste("st", cls), tags$i(`aria-hidden` = "true"),
                         if (lg == "en") STATUS_EN[[s]] else s))
}
# 표: 열 수가 바뀌어도(상태 열 숨김) 정렬이 맞도록 직접 만든다. align은 열마다 l/r/c
tbl_ui <- function(df, align) {
  al <- rep_len(strsplit(align, "")[[1]], ncol(df))
  cls <- c(l = "text-start", r = "text-end", c = "text-center")[al]
  div(class = "tbl-wrap", tags$table(class = "table table-hover",
    tags$thead(tags$tr(lapply(seq_along(df), function(j) tags$th(scope = "col", class = cls[j], names(df)[j])))),
    tags$tbody(lapply(seq_len(nrow(df)), function(i) tags$tr(lapply(seq_along(df), function(j)
      tags$td(class = cls[j], HTML(as.character(df[i, j])))))))))
}
# 상태 열: 모든 행이 '확인 완료'가 되면 열 자체를 없앤다
add_status <- function(df, st, lg) {
  if (all(st == "확인 완료")) return(df)
  df[[if (lg == "en") "Status" else "상태"]] <- vapply(st, status_chip, "", lg = lg)
  df
}
status_legend <- function() div(class = "st-legend",
  span(class = "st ok", tags$i(), L("확인 완료", "Verified")),
  span(class = "st check", tags$i(), L("확인 필요", "To verify")),
  span(class = "st temp", tags$i(), L("임시 값", "Placeholder")),
  L("임시 값은 출처 확인 전 자리표시 값입니다", "Placeholders are stand-in values until a source is confirmed"))

source_line <- function(...) div(class = "src-line", bs_icon("database"), span(...))

xcard <- function(title, ..., aside = NULL, full_screen = FALSE, id = NULL)
  card(full_screen = full_screen, id = id,
       card_header(span(class = "ch-title", title), if (!is.null(aside)) span(class = "ch-aside", aside)),
       ...)

hero <- function(tag, title, sub = NULL, cond = NULL, art = NULL)
  div(class = "hero",
      div(class = "hero-text",
          span(class = "hero-tag", tag),
          if (!is.null(cond)) div(class = "hero-cond", cond),
          h1(class = "hero-title", title),
          if (!is.null(sub)) div(class = "hero-sub", sub)),
      art)

page_foot <- function() div(class = "page-foot", L(
  "LESE412 기말 프로젝트입니다. 자료는 낙농진흥회 낙농통계광장의 원유생산통계와 원유수취가격(2013-2025년, 2026년 11월 19일 조회)이고, 기준연도는 2013년입니다. 수식과 검증은 방법론 탭에 있습니다.",
  "LESE412 final project. Data: Korea Dairy Committee dairy statistics on raw milk production and farm-gate prices (2013-2025, retrieved 19 Nov 2026). Base year 2013. Formulas and validation are in the Methodology tab."))

side_info_card <- function() div(class = "side-info",
  span(class = "si-icon", art("milk", 40)),
  div(div(class = "si-title", L("원유 = 가공 전 우유", "Raw milk = unprocessed milk")),
      p(L("이 앱의 '원유'는 가공(살균, 가공유, 치즈 등) 이전의 낙농가 출하 우유를 의미합니다.",
          "In this app 'raw milk' means milk shipped by dairy farms before processing (pasteurisation, flavoured milk, cheese, etc.)."))))

# ---- 슬라이더 눈금자 ------------------------------------------------------------------
# 슬라이더 아래에 자 눈금을 그린다. major = 숫자를 쓰는 눈금, minor = 짧은 눈금만.
# 슬라이더의 값 위치는 손잡이 반지름(12px)만큼 양끝을 비운 구간에 놓이므로 같은 식으로 맞춘다.
slider_scale <- function(min, max, major, minor = NULL, d = 0) {
  tick <- function(v, cls, lab = NULL)
    span(class = paste("tick", cls), style = sprintf("left: calc(12px + (100%% - 24px) * %.5f)", (v - min) / (max - min)),
         if (!is.null(lab)) span(class = "tick-lab", lab))
  minor <- setdiff(round(minor, 6), round(major, 6))
  div(class = "slider-scale", `aria-hidden` = "true",
      lapply(minor, tick, cls = "minor"),
      lapply(major, function(v) tick(v, "major", num(v, d))))
}

# ---- 파라미터 입력: 숫자 칸 + 슬라이더 + 눈금자 -------------------------------------------
# 숫자 칸은 Shiny 입력이 아니라 슬라이더의 보조 칸이다(id 없음 → 별도 입력값이 생기지 않음).
# 숫자를 입력하면 슬라이더가 움직이고, 슬라이더를 움직이면 숫자가 바뀐다 (www/app.js).
param <- function(id, ko, en, min, max, value, step, major, minor = NULL, d = 1, note = NULL)
  div(class = "param",
      tags$label(class = "param-head",
                 span(class = "param-lab", L(ko, en)),
                 tags$input(type = "number", class = "param-num form-control", `data-for` = id,
                            min = min, max = max, step = step, value = value, inputmode = "decimal")),
      div(class = "param-slider", sliderInput(id, NULL, min = min, max = max, value = value, step = step, ticks = FALSE)),
      slider_scale(min, max, major, minor, d),
      if (!is.null(note)) div(class = "param-note", note))

# ---- 사이드바: 시뮬레이션 설정 --------------------------------------------------
CONTROLS_UI <- tagList(
  div(class = "side-head",
      h2(L("시뮬레이션 설정", "Simulation settings")),
      p(L("값을 바꾸면 결과가 바로 다시 계산됩니다. 숫자 칸에 직접 입력해도 됩니다.",
          "Results update as soon as you change a value. You can also type numbers directly."))),
  div(class = "side-field",
      lab_tip("시나리오", "Scenario",
              "2013년 실제 도입 수준, 도입 전 수준, 가상 인상 시나리오 중에서 고르면 가격과 기간이 함께 바뀝니다. 직접 바꾸면 '직접 설정'이 됩니다.",
              "Pick the actual 2013 level, the pre-scheme level or a hypothetical increase; price and horizon change together. Editing them yourself switches to 'Custom'."),
      selectInput("scenario", NULL, choices = setNames(SCENARIOS$id, SCENARIOS$name), selected = "base"),
      uiOutput("scenario_meta")),
  div(class = "side-section",
      lab_tip("원유가격 (연동제 가격 Ps)", "Raw milk price (linked price Ps)",
              "연동제로 정해지는 원유 1L 가격입니다. 도입 전 균형가격 P*(980.29원/L)보다 높으면 가격하한으로 작동합니다.",
              "The price of 1 L of raw milk set by the scheme. Above the pre-scheme equilibrium P* (980.29 KRW/L) it acts as a price floor."),
      uiOutput("price_big"),
      div(class = "price-slider",
          sliderInput("policy_price", NULL, min = 980, max = 1300, value = BASE$policy_default, step = 1, ticks = FALSE),
          slider_scale(980, 1300, major = c(1000, 1100, 1200, 1300), minor = seq(1000, 1300, 50))),
      div(class = "slider-note", L("980원은 도입 전 P*, 1,083원은 2013년 도입 후 가격입니다.", "980 is the pre-scheme P*; 1,083 is the price after the 2013 introduction."))),
  div(class = "side-section",
      lab_tip("탄력성", "Elasticities",
              "단기(1년)는 낙농가가 생산을 거의 바꾸지 못하는 경우, 장기(5년+)는 착유우 수를 조정한 뒤입니다. 버튼은 기본값을 불러오고, 숫자 칸이나 슬라이더로 직접 바꿀 수 있습니다.",
              "Short run (1 year): farms can barely change output. Long run (5+ years): after adjusting herd size. The buttons load default values; edit them with the number boxes or sliders."),
      div(class = "seg", radioButtons("run", NULL, inline = TRUE,
                                      choiceNames = list(L("단기 기본값", "Short-run default"), L("장기 기본값", "Long-run default")),
                                      choiceValues = c("SR", "LR"))),
      uiOutput("preset_state"),
      param("eps_s", "원유 공급탄력성 ε (낙농가)", "Raw milk supply elasticity ε (farms)",
            0.05, 2, ELAST_PRESET$SR$eps_s, 0.05,
            major = c(0.05, 0.5, 1, 1.5, 2), minor = seq(0.25, 1.75, 0.25), d = 2),
      param("eta_r", "흰 우유 소매 수요탄력성 η", "Retail white milk demand elasticity η",
            -1.5, -0.05, ELAST_PRESET$SR$eta_r, 0.05,
            major = c(-1.5, -1, -0.5, -0.05), minor = seq(-1.25, -0.25, 0.25), d = 2),
      div(class = "derived", uiOutput("eta_farm_txt"))),
  accordion(
    open = FALSE, class = "side-acc",
    accordion_panel(
      L("고급: 파생수요 계수 (Sa, σ)", "Advanced: derived demand (Sa, σ)"), value = "adv_derived",
      p(class = "help", L("원유 수요탄력성 ηa = Sa·η − Sb·σ (Muth). Sb = 1 − Sa",
                          "Raw milk demand elasticity ηa = Sa·η − Sb·σ (Muth). Sb = 1 − Sa")),
      param("Sa", "소매가 중 원유 몫 Sa", "Raw milk share of retail price Sa", 0.2, 0.8, ELAST_DEFAULT$Sa, 0.01,
            major = seq(0.2, 0.8, 0.1), minor = seq(0.25, 0.75, 0.1)),
      param("sigma", "원유와 기타 투입재의 대체탄력성 σ", "Substitution, raw milk vs. other inputs σ", 0, 1, ELAST_DEFAULT$sigma, 0.05,
            major = seq(0, 1, 0.2), minor = seq(0.1, 0.9, 0.2))
    ),
    accordion_panel(
      L("고급: 잉여 원유 처리", "Advanced: surplus milk handling"), value = "adv_surplus",
      p(class = "help", L("남는 원유를 누가 사서 얼마에 처분하는지 정합니다. 부담 주체는 분배만 바꾸고 사회 전체 변화는 같습니다.",
                          "Who buys the surplus milk and what it is sold for. The payer changes only the distribution; total welfare is the same.")),
      div(class = "seg", radioButtons("payer", NULL, inline = TRUE,
                                      choiceNames = list(L("정부 매입", "Government"), L("유업체 매입", "Processors")),
                                      choiceValues = c("gov", "proc"), selected = SURPLUS_DEFAULT$payer)),
      param("Pv", "잉여 원유 처분가치 Pv (원/L)", "Salvage value of surplus Pv (KRW/L)", 0, 900, SURPLUS_DEFAULT$Pv, 10,
            major = seq(0, 900, 300), minor = seq(100, 800, 100), d = 0, note = L("0 = 회수 가치 없음 (강의 모형)", "0 = no salvage (lecture model)"))
    )
  ),
  div(class = "side-actions",
      actionButton("bookmark", L("설정 링크 복사", "Copy settings link"), icon = icon("link"), class = "btn-primary btn-main w-100"),
      actionLink("reset", tagList(bs_icon("arrow-counterclockwise"), L("설정 초기화", "Reset settings")), class = "reset-link"))
)

SIDE_INFO_UI <- tagList(
  div(class = "side-head",
      h2(L("이 앱에 대해", "About this app")),
      p(L("원유가격 연동제가 낙농가, 유가공업체, 가계에 미치는 영향을 계산하는 정책 분석 도구입니다.",
          "A policy analysis tool that calculates how Korea's raw milk price-linking scheme affects dairy farms, processors and households."))),
  div(class = "side-note", bs_icon("sliders"),
      div(L("가격과 탄력성 설정은 <b>시뮬레이터</b>와 <b>후생 결과</b> 탭의 사이드바에 있습니다.",
            "Price and elasticity settings are in the sidebar of the <b>Simulator</b> and <b>Welfare</b> tabs.", html = TRUE),
          actionLink("to_sim", L("시뮬레이터로 이동", "Go to Simulator"), class = "d-block mt-1"))),
  div(class = "side-block",
      div(class = "sb-title", L("사용자", "User")),
      p(L("탄력성과 원유가격을 바꿔 가며 정책 결과를 예측하는 농림축산식품부 낙농 정책 담당 분석가",
          "A dairy policy analyst at the Ministry of Agriculture who forecasts policy outcomes by changing elasticities and the raw milk price"))),
  div(class = "side-block",
      div(class = "sb-title", L("분석 질문", "Research question")),
      p(L("연동제로 원유가격이 오르면 낙농가 소득 보전(ΔPS)에 비해 유가공업체와 소비자의 손실과 잉여 원유 처리 비용은 단기와 장기에 각각 얼마이며, 저소득층과 고소득층 가계의 실질 부담은 어느 수준인가?",
          "When the scheme raises the raw milk price, how large are processor and consumer losses and surplus handling costs relative to farm income support (ΔPS) in the short and long run, and what is the real burden on low- and high-income households?"))),
  div(class = "side-block",
      div(class = "sb-title", L("분석 시장과 기준연도", "Market and base year")),
      p(L("농가 출하 단계의 원유 시장(원/L, 천 톤), 2013년. 연동제가 처음 적용되어 도입 전(1-7월)과 도입 후(8-12월) 가격을 모두 관측할 수 있습니다.",
          "Farm-gate raw milk market (KRW/L, kt), 2013. The scheme was first applied that year, so prices before (Jan-Jul) and after (Aug-Dec) are both observed."))),
  side_info_card()
)

kpi <- function(icon, out, tip, cap_ko, cap_en, cls)
  div(class = paste("kpi-card", cls),
      div(class = "kpi-head", span(class = "kpi-icon", art(icon, 40)),
          div(class = "kpi-title", uiOutput(paste0(out, "_title"), inline = TRUE), info_tip(tip))),
      div(class = "kpi-value", textOutput(out, inline = TRUE)),
      div(class = "kpi-cap", L(cap_ko, cap_en)))

METRIC_IDS <- c("dW", "dPS", "dCS", "G", "stock")
METRIC_NAMES <- list(
  ko = c("사회 전체 ΔW (억 원)", "낙농가 ΔPS (억 원)", "유업체 ΔCS (억 원)", "잉여 원유 처리 −G (억 원)", "남는 원유 Qs − Qd (천 톤)"),
  en = c("Total welfare ΔW (₩ bn)", "Farms ΔPS (₩ bn)", "Processors ΔCS (₩ bn)", "Surplus handling −G (₩ bn)", "Surplus milk Qs − Qd (kt)"))
METRIC_CONCEPT <- c(dW = "dwl", dPS = "farm", dCS = "proc", G = "gov", stock = "gov")
metric_val <- function(r, m) switch(m, dW = r$dW, dPS = r$dPS, dCS = r$dCS, G = -r$G, stock = r$stock)

# ---- 탭: 시뮬레이터 -------------------------------------------------------
sim_ui <- tagList(
  hero(L("시뮬레이터", "Simulator"), uiOutput("hero_title", inline = TRUE), uiOutput("hero_sub", inline = TRUE),
       cond = uiOutput("hero_cond_sim", inline = TRUE)),
  div(id = "sec-kpi", class = "anchor",
      layout_column_wrap(
        width = "200px", fill = FALSE, class = "kpi-row",
        kpi("cow", "vb_ps", "tt_ps", "생산자잉여 변화 ΔPS", "Producer surplus change ΔPS", "farm"),
        kpi("factory", "vb_cs", "tt_cs", "구매자잉여 변화 ΔCS", "Buyer surplus change ΔCS", "proc"),
        kpi("sack", "vb_g", "tt_g", "처리 비용 G를 뺀 값 (−G)", "Minus the handling cost (−G)", "gov"),
        kpi("scale", "vb_dw", "tt_dw", "순후생 변화 ΔW = −사중손실", "Net welfare change ΔW = −deadweight loss", "dwl")
      )),
  div(id = "sec-story", class = "callout anchor",
      div(class = "callout-head", bs_icon("lightbulb"), L("해석", "Interpretation")),
      div(class = "prose", uiOutput("story"))),
  layout_columns(
    col_widths = c(7, 5),
    xcard(L("수요와 공급, 후생 면적", "Supply, demand and welfare areas"), aside = L("단위: 원/L, 천 톤", "Units: KRW/L, kt"),
          full_screen = TRUE, id = "sec-sd",
          div(class = "card-lead", uiOutput("sd_title", inline = TRUE)),
          plotOutput("sd_plot", height = 400),
          source_line(L("기준선은 2013년 낙농진흥회 자료(P* 980.29원/L, Q* 2,093천 톤)입니다. ΔCS = −A, ΔPS = A + B, G = B + D",
                        "Baseline: Korea Dairy Committee 2013 (P* 980.29 KRW/L, Q* 2,093 kt). ΔCS = −A, ΔPS = A + B, G = B + D"))),
    xcard(L("가격과 탄력성이 바뀌면", "If price or elasticity changes"), full_screen = TRUE, id = "sec-whatif",
          div(class = "whatif-pick",
              selectInput("metric", NULL, choices = setNames(METRIC_IDS, METRIC_NAMES$ko), selected = "dW", width = "100%")),
          plotOutput("whatif_plot", height = 360),
          p(class = "foot", L("점이 현재 설정입니다. 위 그래프는 탄력성을, 아래 그래프는 가격을 현재 값에 고정했습니다. 두 그래프의 세로축은 같습니다.",
                              "The dot is the current setting. The top panel holds elasticities at their current values; the bottom panel holds the price. Both share the same vertical axis.")))
  ),
  layout_columns(
    col_widths = c(5, 7),
    xcard(L("원유 시장 결과", "Raw milk market outcome"), aside = L("기준 = 2013년 도입 전", "Base = 2013 pre-scheme"), id = "sec-market",
          div(class = "tbl-wrap", tableOutput("market_tbl")),
          p(class = "foot", L("괄호는 기준 대비 변화율입니다. 흰 우유 소매가는 전가율 Sa로 계산한 참고값입니다.",
                              "Brackets show % change from base. The white milk retail price is a reference value using pass-through Sa."))),
    xcard(L("사용 중인 탄력성", "Elasticities in use"), id = "sec-elnow",
          uiOutput("elast_now_tbl"),
          status_legend())
  ),
  div(id = "sec-assume", class = "anchor",
      accordion(
        open = FALSE, class = "assume-box",
        accordion_panel(L("이 결과의 가정", "Assumptions behind this result"), value = "assume",
                        tags$ul(class = "assume", lapply(seq_along(ASSUMPTIONS), function(i) tags$li(L(ASSUMPTIONS[i], ASSUMPTIONS_EN[i])))),
                        p(class = "foot", L("수식, 탄력성 출처, 검증은 방법론 탭에 있습니다.",
                                            "Formulas, elasticity sources and validation are in the Methodology tab."))))),
  page_foot()
)

# ---- 탭: 후생 결과 ---------------------------------------------------------
welfare_ui <- tagList(
  hero(L("후생 결과", "Welfare"), uiOutput("welfare_title", inline = TRUE),
       L("낙농가 1원 이득을 만드는 데 다른 주체가 치르는 비용입니다.", "What others pay to give farms ₩1 of gain."),
       cond = uiOutput("hero_cond_welfare", inline = TRUE)),
  layout_columns(
    col_widths = c(7, 5),
    xcard(L("이해관계자별 후생 변화", "Welfare change by stakeholder"), aside = L("억 원", "₩ bn"), id = "sec-stake",
          uiOutput("stake_rows")),
    xcard(L("주요 지표", "Key indicators"), id = "sec-metric",
          uiOutput("metric_rows"),
          actionButton("to_method", L("산출 근거는 방법론에서 확인", "See the derivation in Methodology"), class = "btn-soft w-100"))
  ),
  layout_columns(
    col_widths = c(6, 6),
    xcard(L("단기와 장기 비교", "Short run vs. long run"), aside = L("단위: 억 원, 현재 가격 기준", "₩ bn at the current price"), id = "sec-srlr",
          plotOutput("srlr_plot", height = 230),
          uiOutput("reality_bar")),
    xcard(L("가계 파급 효과", "Household impact"), aside = L("월 추가 지출", "Extra monthly spending"), id = "sec-hh",
          uiOutput("hh_rows"))
  ),
  xcard(L("공급탄력성과 인상폭이 클수록 사회 전체 손실이 커집니다", "Larger supply elasticity and price increases mean larger total welfare losses"),
        aside = L("사회 전체 ΔW, 억 원", "Total ΔW, ₩ bn"), id = "sec-sens",
        uiOutput("sens_tbl"),
        p(class = "foot", L("공급탄력성 낮음, 기준, 높음은 각각 ε 0.2, 0.5, 1.0이고, 인상폭은 도입 전 P* 대비입니다. 수요탄력성과 잉여 원유 처리는 현재 설정값을 씁니다. 굵은 테두리는 현재 설정과 가장 가까운 칸이고, 색이 진할수록 손실이 큽니다.",
                            "Low, base and high supply elasticity are ε 0.2, 0.5 and 1.0; increases are relative to pre-scheme P*. Demand elasticity and surplus handling use the current settings. The bold border marks the cell closest to the current setting; darker means a larger loss."))),
  page_foot()
)

# ---- 탭: 정책과 시장 -------------------------------------------------------
market_ui <- tagList(
  hero(L("정책과 시장", "Policy & market"),
       L("원유가격 연동제는<br>생산비가 오르면 수요와 관계없이 가격을 올립니다",
         "The price-linking scheme raises the raw milk price<br>when costs rise, regardless of demand", html = TRUE), art = HERO_ART),
  layout_columns(
    col_widths = c(6, 6),
    xcard(L("정책 개요", "Policy overview"), id = "sec-policy",
          div(class = "policy-list", lapply(seq_along(POLICY_INFO), function(i) {
            x <- POLICY_INFO[[i]]; e <- POLICY_INFO_EN[[i]]
            div(class = "policy-item",
                span(class = "policy-icon", bs_icon(x$bs)),
                div(div(class = "policy-title", L(x$title, e$title)),
                    tags$ul(class = "note-list", lapply(seq_along(x$points), function(j)
                      tags$li(L(x$points[j], e$points[j], html = TRUE))))))
          })),
          source_line(L(POLICY_SOURCE, POLICY_SOURCE_EN))),
    xcard(L("원유 시장 구조와 가치사슬", "Raw milk market structure and value chain"), id = "sec-structure",
          div(class = "chain", lapply(seq_len(nrow(VALUE_CHAIN)), function(i)
            div(class = paste("chain-step", VALUE_CHAIN$scope[i]),
                div(class = "chain-icon", art(VALUE_CHAIN$art[i], 112, 84, fallback = VALUE_CHAIN$li[i])),
                div(class = "chain-name", L(VALUE_CHAIN$step[i], VALUE_CHAIN_EN$step[i])),
                div(class = "chain-note", L(VALUE_CHAIN$note[i], VALUE_CHAIN_EN$note[i]))))),
          div(class = "chain-bracket", L("분석 대상: 원유 거래 (낙농가 → 유가공업체)", "Analysed: raw milk trade (farms → processors)")),
          uiOutput("structure_tbl"),
          status_legend())
  ),
  layout_columns(
    col_widths = c(6, 6),
    xcard(L("2013년 8월, 원유가격이 한 달 만에 96원/L 올랐습니다", "In August 2013 the raw milk price rose 96 KRW/L in one month"),
          aside = L("원/L", "KRW/L"), id = "sec-m2013",
          plotOutput("m2013_plot", height = 300),
          source_line(L("낙농진흥회 원유수취가격(전국, 2013년) 자료입니다. 점선은 모형의 P*와 연동제 가격입니다.",
                        "Korea Dairy Committee farm-gate price (national, 2013). Dashed lines are the model's P* and linked price."))),
    div(id = "sec-trend", class = "anchor",
        navset_card_underline(
          title = span(class = "ch-title", L("가격은 올랐고 생산량은 줄었습니다 (2013-2025)", "Prices rose and output fell (2013-2025)")),
          nav_panel(L("그래프", "Chart"), plotOutput("trend_plot", height = 300)),
          nav_panel(L("표", "Table"), div(class = "tbl-wrap", tableOutput("trend_tbl"))),
          footer = source_line(L("낙농진흥회 원유수취가격 연평균과 원유생산량(2013-2025년) 자료입니다. 그래프는 2013년을 100으로 둔 지수이고, 실제 값은 표에 있습니다.",
                                 "Korea Dairy Committee annual average farm-gate price and raw milk production, 2013-2025. The chart is an index with 2013 = 100; actual values are in the table."))))
  ),
  xcard(L("2013년 기준선", "2013 baseline"), id = "sec-baseline",
        uiOutput("baseline_tbl"),
        div(class = "st-row", status_legend(),
            span(class = "foot m-0", L("결론에는 '확인 완료' 값만 씁니다.", "Only 'Verified' values are used for conclusions.")))),
  page_foot()
)

# ---- 탭: 방법론 ------------------------------------------------------------
eqn <- function(...) div(class = "eq-line", HTML(paste0(...)))
method_ui <- tagList(
  hero(L("방법론", "Methodology"), L("수식, 가정, 자료, 그리고 검증", "Formulas, assumptions, data and validation")),
  layout_columns(
    col_widths = c(7, 5),
    xcard(L("수식", "Formulas"), id = "sec-eq",
          div(class = "eq",
              h3(L("1. 선형 수요와 공급 (2013년 기준점에서 보정)", "1. Linear demand and supply (calibrated at the 2013 base point)")),
              eqn("Q<sup>d</sup> = a − bP,&nbsp;&nbsp; Q<sup>s</sup> = c + dP"),
              eqn("b = |η<sub>a</sub>| · Q* / P*,&nbsp;&nbsp; d = ε · Q* / P*"),
              h3(L("2. 원유 수요탄력성 (유업체의 파생수요, Muth)", "2. Raw milk demand elasticity (processors' derived demand, Muth)")),
              eqn("η<sub>a</sub> = S<sub>a</sub> · η − S<sub>b</sub> · σ,&nbsp;&nbsp; S<sub>b</sub> = 1 − S<sub>a</sub>"),
              h3(L("3. 연동제 가격 = 가격하한", "3. Linked price = price floor")),
              eqn("P<sub>s</sub> &gt; P* &nbsp;⇒&nbsp; ", as.character(L("잉여", "Surplus")),
                  " = Q<sup>s</sup>(P<sub>s</sub>) − Q<sup>d</sup>(P<sub>s</sub>)"),
              h3(L("4. 후생 (수급 그래프의 면적 A, B, D)", "4. Welfare (areas A, B, D in the supply-demand chart)")),
              eqn("ΔCS = −A = −(P<sub>s</sub> − P*) · (Q* + Q<sup>d</sup>) / 2"),
              eqn("ΔPS = A + B = (P<sub>s</sub> − P*) · (Q* + Q<sup>s</sup>) / 2"),
              eqn("G = (P<sub>s</sub> − P<sub>v</sub>) · (Q<sup>s</sup> − Q<sup>d</sup>) = B + D"),
              eqn("ΔW = ΔCS + ΔPS − G = −D"),
              eqn(as.character(L("이전효율", "Transfer efficiency")), " = ΔPS / (G − ΔCS)"),
              h3(L("5. 가계 파급 (원유가 → 소매가 전가)", "5. Household impact (raw milk → retail pass-through)")),
              eqn("%ΔP<sub>retail</sub> ≈ S<sub>a</sub> × %ΔP<sub>farm</sub>"))),
    xcard(L("가정", "Assumptions"), id = "sec-assume2",
          tags$ul(class = "assume", lapply(seq_along(ASSUMPTIONS), function(i) tags$li(L(ASSUMPTIONS[i], ASSUMPTIONS_EN[i])))))
  ),
  layout_columns(
    col_widths = c(6, 6),
    xcard(L("모형 검증: 강의 L11 공식과 일치합니다", "Model validation: matches the L11 lecture formulas"), id = "sec-valid",
          p(class = "card-lead", L(VALIDATION_CASE$label, VALIDATION_LABEL_EN)),
          div(class = "tbl-wrap", tableOutput("valid_tbl")),
          p(class = "foot", L("L11 Supply Policy 6쪽: ΔCS = −(a+b), ΔPS = a+b+c, G = b+c+d+e, Net = −(b+d+e), 재판매 시 G = (Ps − v)(Qs − Qd)",
                              "L11 Supply Policy p.6: ΔCS = −(a+b), ΔPS = a+b+c, G = b+c+d+e, Net = −(b+d+e), with resale G = (Ps − v)(Qs − Qd)"))),
    div(class = "stack",
        xcard(L("현재 설정의 독립 검산", "Independent check of the current setting"), aside = L("억 원", "₩ 100M"), id = "sec-check",
              p(class = "card-lead", L("같은 ΔW를 면적 공식과 곡선 아래 적분, 두 방법으로 계산해 비교합니다.",
                                       "The same ΔW computed two ways: area formulas and integration under the curves.")),
              div(class = "tbl-wrap", tableOutput("check_tbl"))),
        xcard(L("자료 출처", "Data sources"), id = "sec-sources",
              uiOutput("source_tbl"),
              source_line(L("낙농진흥회 낙농통계광장: dairy.or.kr › 낙농통계광장 › 국내통계",
                            "Korea Dairy Committee dairy statistics: dairy.or.kr › statistics › domestic"))))
  ),
  xcard(L("탄력성 표", "Elasticity table"), id = "sec-elast",
        uiOutput("elast_tbl"),
        status_legend()),
  page_foot()
)

# ---- 상단 메뉴: 마우스를 올리면 탭 안의 구역 목록이 펼쳐진다 -----------------------------
NAV_SECTIONS <- list(
  sim = list(c("sec-kpi", "핵심 지표", "Key results"), c("sec-story", "해석", "Interpretation"),
             c("sec-sd", "수요와 공급, 후생 면적", "Supply, demand and welfare areas"),
             c("sec-whatif", "가격과 탄력성이 바뀌면", "If price or elasticity changes"),
             c("sec-market", "원유 시장 결과", "Raw milk market outcome"),
             c("sec-elnow", "사용 중인 탄력성", "Elasticities in use"), c("sec-assume", "이 결과의 가정", "Assumptions")),
  welfare = list(c("sec-stake", "이해관계자별 후생 변화", "Welfare change by stakeholder"),
                 c("sec-metric", "주요 지표", "Key indicators"), c("sec-srlr", "단기와 장기 비교", "Short run vs. long run"),
                 c("sec-hh", "가계 파급 효과", "Household impact"), c("sec-sens", "민감도 표", "Sensitivity table")),
  market = list(c("sec-policy", "정책 개요", "Policy overview"), c("sec-structure", "시장 구조와 가치사슬", "Market structure"),
                c("sec-m2013", "2013년 월별 원유가격", "2013 monthly price"), c("sec-trend", "가격과 생산량 추이", "Price and output trend"),
                c("sec-baseline", "2013년 기준선", "2013 baseline")),
  method = list(c("sec-eq", "수식", "Formulas"), c("sec-assume2", "가정", "Assumptions"), c("sec-valid", "모형 검증", "Model validation"),
                c("sec-check", "독립 검산", "Independent check"), c("sec-sources", "자료 출처", "Data sources"),
                c("sec-elast", "탄력성 표", "Elasticity table"))
)
NAV_DROPS <- div(id = "nav-drops", hidden = NA, lapply(names(NAV_SECTIONS), function(tab)
  div(class = "nav-drop", `data-tab` = tab, role = "menu",
      lapply(NAV_SECTIONS[[tab]], function(s)
        tags$a(href = paste0("#", s[1]), class = "nav-drop-item", role = "menuitem", `data-tab` = tab, `data-target` = s[1],
               L(s[2], s[3]))))))

# ---- 전체 화면 ----------------------------------------------------------------------
ui <- function(request) {
  page_navbar(
    id = "nav",
    window_title = "원유가격 정책 시뮬레이터",
    title = tags$span(class = "brand",
                      span(class = "brand-mark", li("cow", 30), li("milk", 24)),
                      span(class = "brand-text",
                           span(class = "brand-name", L("원유가격 정책 시뮬레이터", "Raw Milk Price Policy Simulator")),
                           span(class = "brand-sub", L("가공 전 우유의 가격과 낙농 정책", "Farm-gate milk prices and dairy policy")))),
    theme = theme_app,
    fillable = FALSE,
    header = tagList(
      tags$head(
        tags$link(rel = "stylesheet", href = "webfonts/pretendard/pretendardvariable-dynamic-subset.css"),
        tags$link(rel = "stylesheet", href = asset_src("style.css")),
        tags$script(src = asset_src("app.js"))),
      if (exists("useBusyIndicators", where = asNamespace("shiny"), inherits = FALSE))
        shiny::useBusyIndicators(spinners = TRUE, pulse = TRUE),
      NAV_DROPS
    ),
    sidebar = sidebar(
      width = 320, open = list(desktop = "open", mobile = "closed"),
      conditionalPanel("input.nav == 'sim' || input.nav == 'welfare'", CONTROLS_UI),
      conditionalPanel("input.nav == 'market' || input.nav == 'method'", SIDE_INFO_UI)
    ),
    nav_panel(L("시뮬레이터", "Simulator"), value = "sim",     sim_ui),
    nav_panel(L("후생 결과", "Welfare"),     value = "welfare", welfare_ui),
    nav_panel(L("정책과 시장", "Policy & market"), value = "market", market_ui),
    nav_panel(L("방법론", "Methodology"),    value = "method",  method_ui),
    nav_spacer(),
    nav_item(tags$span(class = "nav-note", L("LESE412 2026년 가을", "LESE412 Fall 2026"))),
    nav_item(div(class = "lang-seg", role = "group", `aria-label` = "언어 / Language",
                 radioButtons("lang", NULL, inline = TRUE, choices = c("한국어" = "ko", "English" = "en"), selected = "ko")))
  )
}

# ---- 그래프 ----------------------------------------------------------------------
theme_plot <- function() {
  theme_minimal(base_family = PLOT_FONT, base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "#EDF0EE", linewidth = 0.4),
          axis.title = element_text(color = COL$muted, size = 11),
          axis.text = element_text(color = COL$muted, size = 11),
          strip.text = element_text(color = COL$ink, size = 12, face = "bold", hjust = 0, margin = margin(2, 0, 6, 0)),
          legend.position = "bottom", legend.title = element_blank(),
          legend.text = element_text(color = COL$sub, size = 11),
          plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(8, 14, 8, 8))
}
TXT <- 3.6

plot_sd <- function(r, lg) {
  k <- r$curves
  xlim <- c(r$Q0 * 0.6, r$Q0 * 1.4)
  ylim <- c(r$P0 * 0.6, r$P0 * 1.4)
  qx <- seq(xlim[1], xlim[2], length.out = 200)
  dem <- data.frame(Q = qx, P = inv_demand(k, qx))
  sup <- data.frame(Q = qx, P = inv_supply(k, qx))
  area_lab <- if (lg == "en") c(A = "A  Processors → farms", B = "B  Surplus purchase → farms", D = "D  Deadweight loss")
              else c(A = "A  유업체 → 낙농가", B = "B  잉여 매입비 → 낙농가", D = "D  사중손실")
  area_col <- setNames(c(CONCEPT$proc$light, CONCEPT$gov$light, CONCEPT$dwl$light), area_lab)
  areas <- r$areas
  areas$area <- factor(area_lab[as.character(areas$area)], levels = area_lab)
  clampx <- function(q) min(max(q, xlim[1] + 0.02 * diff(xlim)), xlim[2] - 0.02 * diff(xlim))
  ys <- ylim[1] + 0.9 * diff(ylim); qs <- clampx(k$c + k$d * ys)
  yd <- ylim[1] + 0.1 * diff(ylim); qd <- clampx(k$a - k$b * yd)
  p <- ggplot() +
    geom_polygon(data = areas, aes(x, y, fill = area), color = NA) +
    geom_hline(yintercept = r$P0, linetype = "dotted", color = COL$muted, linewidth = 0.5) +
    geom_line(data = dem, aes(Q, P), color = CONCEPT$proc$ink, linewidth = 1.1) +
    geom_line(data = sup, aes(Q, P), color = CONCEPT$farm$ink, linewidth = 1.1) +
    annotate("text", x = qs, y = inv_supply(k, qs), label = tr2(lg, "공급 (낙농가)", "Supply (farms)"),
             hjust = if (qs > xlim[1] + 0.75 * diff(xlim)) 1.1 else -0.12, vjust = 0.5,
             family = PLOT_FONT, size = TXT, color = CONCEPT$farm$ink, fontface = "bold") +
    annotate("text", x = qd, y = inv_demand(k, qd), label = tr2(lg, "수요 (유업체)", "Demand (processors)"),
             hjust = if (qd > xlim[1] + 0.75 * diff(xlim)) 1.1 else -0.12, vjust = 0.5,
             family = PLOT_FONT, size = TXT, color = CONCEPT$proc$ink, fontface = "bold") +
    annotate("label", x = xlim[1] + 0.01 * diff(xlim), y = r$P0,
             label = paste0(tr2(lg, "도입 전 P* ", "Pre-scheme P* "), num(r$P0, 2)),
             hjust = 0, vjust = 1.25, size = TXT, family = PLOT_FONT, linewidth = 0, fill = "white", color = COL$muted)
  if (r$binding) {
    p <- p +
      geom_hline(yintercept = r$Ps, linetype = "dashed", color = COL$brand800, linewidth = 0.6) +
      annotate("label", x = xlim[1] + 0.01 * diff(xlim), y = r$Ps,
               label = paste0(tr2(lg, "연동제 가격 Ps ", "Linked price Ps "), num(r$Ps)),
               hjust = 0, vjust = -0.3, size = TXT, family = PLOT_FONT, linewidth = 0, fill = "white",
               color = COL$brand800, fontface = "bold") +
      geom_point(aes(x = c(r$Qd, r$Qs), y = c(r$Ps, r$Ps)), size = 2.6, color = COL$ink)
    if (r$stock > 0.02 * r$Q0) {
      lab <- data.frame(
        x = c(xlim[1] + 0.06 * diff(xlim), (r$Qd + r$Qs + r$Q0) / 3, (r$Qd + r$Q0) / 2),
        y = c((r$P0 + r$Ps) / 2, (2 * r$Ps + r$P0) / 3, (ylim[1] + r$P0) / 2),
        t = c("A", "B", "D"))
      p <- p + geom_text(data = lab, aes(x, y, label = t), family = PLOT_FONT, fontface = "bold", size = 4, color = COL$ink)
    }
  }
  if (nrow(areas) > 0) p <- p + scale_fill_manual(values = area_col, drop = FALSE) + guides(fill = guide_legend(nrow = 1))
  p + geom_point(aes(x = r$Q0, y = r$P0), size = 2.6, color = COL$ink) +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    scale_x_continuous(labels = function(x) num(x)) +
    scale_y_continuous(labels = function(x) num(x)) +
    labs(x = tr2(lg, "원유 수량 (천 톤)", "Raw milk quantity (kt)"), y = tr2(lg, "원유 가격 (원/L)", "Raw milk price (KRW/L)")) +
    theme_plot() +
    theme(panel.grid.major.x = element_line(color = "#EDF0EE", linewidth = 0.4))
}

plot_whatif <- function(df, cur, m, lg) {
  cc <- CONCEPT[[METRIC_CONCEPT[[m]]]]
  f <- if (lg == "en" && m != "stock") 0.1 else 1          # 영어: 억 원 → ₩ bn
  df$y <- df$y * f; cur$y <- cur$y * f
  rng <- range(c(df$y, 0)); pad <- diff(rng) * 0.18 + 1e-9
  cur$lab <- if (m == "stock") kt(cur$y, lg) else money(cur$y / f, lg)
  cur$hj <- ifelse(cur$xpos > 0.6, 1.1, -0.1)
  rising <- m %in% c("dPS", "stock")
  cur$vj <- ifelse(xor(cur$xpos > 0.6, rising), 1.5, -0.5)
  ggplot(df, aes(x, y)) +
    geom_hline(yintercept = 0, color = COL$line_strong, linewidth = 0.5) +
    geom_line(color = cc$line, linewidth = 1.2) +
    geom_point(data = cur, shape = 21, size = 3.4, stroke = 1.4, color = cc$ink, fill = "white") +
    geom_label(data = cur, aes(label = lab, hjust = hj, vjust = vj), family = PLOT_FONT, fontface = "bold",
               size = TXT, color = cc$ink, fill = "white", linewidth = 0, label.padding = unit(2, "pt")) +
    facet_wrap(~panel, ncol = 1, scales = "free_x") +
    scale_y_continuous(labels = function(x) num(x, if (f < 1) 0 else 0), limits = c(rng[1] - pad, rng[2] + pad)) +
    scale_x_continuous(labels = function(x) num(x, if (max(abs(x), na.rm = TRUE) < 10) 1 else 0)) +
    labs(x = NULL, y = NULL) +
    theme_plot() +
    theme(panel.spacing.y = unit(18, "pt"))
}

# ---- 서버 ----------------------------------------------------------------------
server <- function(input, output, session) {

  lang <- reactive(if (identical(input$lang, "en")) "en" else "ko")
  tr <- function(ko, en) if (lang() == "en") en else ko

  # 언어가 바뀌면: 정보 아이콘 툴팁과 선택 목록의 이름을 바꾼다 (선택값은 유지)
  observeEvent(lang(), {
    lg <- lang()
    for (id in ls(TIPS)) update_tooltip(id, TIPS[[id]][[lg]])
    updateSelectInput(session, "scenario",
                      choices = setNames(SCENARIOS$id, if (lg == "en") SCENARIOS_EN$name else SCENARIOS$name),
                      selected = isolate(input$scenario))
    updateSelectInput(session, "metric", choices = setNames(METRIC_IDS, METRIC_NAMES[[lg]]),
                      selected = isolate(input$metric))
  }, ignoreInit = TRUE)

  setBookmarkExclude(c("bookmark", "reset", "to_method", "to_sim", "tt_ps", "tt_cs", "tt_g", "tt_dw", ls(TIPS)))
  observeEvent(input$bookmark, session$doBookmark())
  onBookmarked(function(url) {
    updateQueryString(url)
    showModal(modalDialog(
      title = tr("이 설정 링크", "Link to these settings"),
      p(tr("아래 링크를 열면 지금의 시나리오, 원유가격, 탄력성, 잉여 원유 처리 설정, 탭, 언어가 그대로 재현됩니다. 발표 Q&A용으로 미리 저장해 두세요.",
           "Opening this link restores the current scenario, raw milk price, elasticities, surplus handling, tab and language. Save it ahead of a presentation Q&A.")),
      tags$textarea(id = "bm_url", class = "form-control", rows = 3, readonly = NA, `aria-label` = "URL", url),
      footer = tagList(
        tags$button(type = "button", class = "btn btn-primary",
                    onclick = sprintf("navigator.clipboard.writeText(document.getElementById('bm_url').value); this.textContent='%s';",
                                      tr("복사됨", "Copied")),
                    tr("링크 복사", "Copy link")),
        modalButton(tr("닫기", "Close"))),
      easyClose = TRUE))
  })
  observeEvent(input$to_method, nav_select("nav", "method"))
  observeEvent(input$to_sim, nav_select("nav", "sim"))

  observeEvent(input$scenario, {
    s <- SCENARIOS[SCENARIOS$id == input$scenario, ]
    if (!is.na(s$price)) updateSliderInput(session, "policy_price", value = s$price)
    if (!is.na(s$run))   updateRadioButtons(session, "run", selected = s$run)
  }, ignoreInit = TRUE)
  observeEvent(input$policy_price, {
    s <- SCENARIOS[SCENARIOS$id == input$scenario, ]
    if (!is.na(s$price) && input$policy_price != s$price) updateSelectInput(session, "scenario", selected = "custom")
  }, ignoreInit = TRUE)

  output$scenario_meta <- renderUI({
    i <- which(SCENARIOS$id == input$scenario)
    if (SCENARIOS$id[i] == "custom") return(NULL)
    div(class = "scenario-meta", span(tr("출처: ", "Source: "), tr(SCENARIOS$source[i], SCENARIOS_EN$source[i])),
        HTML(status_chip(SCENARIOS$상태[i], lang())))
  })

  observeEvent(input$run, {
    p <- ELAST_PRESET[[input$run]]
    updateSliderInput(session, "eta_r", value = p$eta_r)
    updateSliderInput(session, "eps_s", value = p$eps_s)
  }, ignoreInit = TRUE)

  preset_now <- reactive({
    p <- ELAST_PRESET[[input$run]]
    if (isTRUE(all.equal(c(input$eta_r, input$eps_s), c(p$eta_r, p$eps_s)))) input$run else "custom"
  })
  run_label <- reactive(switch(preset_now(), SR = tr("단기 기본값", "Short-run default"),
                               LR = tr("장기 기본값", "Long-run default"), tr("직접 조정", "Custom")))
  output$preset_state <- renderUI({
    if (preset_now() != "custom") return(NULL)
    p <- ELAST_PRESET[[input$run]]
    div(class = "preset-note", bs_icon("pencil"),
        paste0(tr("직접 조정 중입니다. ", "Custom values. "),
               if (input$run == "SR") tr("단기", "short-run") else tr("장기", "long-run"),
               tr(" 기본값은 ε ", " default is ε "), num(p$eps_s, 2), ", η ", num(p$eta_r, 2), tr("입니다.", ".")))
  })

  observeEvent(input$reset, {
    updateSelectInput(session, "scenario", selected = "base")
    updateSliderInput(session, "policy_price", value = BASE$policy_default)
    updateRadioButtons(session, "run", selected = "SR")
    updateSliderInput(session, "eta_r", value = ELAST_PRESET$SR$eta_r)
    updateSliderInput(session, "eps_s", value = ELAST_PRESET$SR$eps_s)
    updateSliderInput(session, "Sa", value = ELAST_DEFAULT$Sa)
    updateSliderInput(session, "sigma", value = ELAST_DEFAULT$sigma)
    updateRadioButtons(session, "payer", selected = SURPLUS_DEFAULT$payer)
    updateSliderInput(session, "Pv", value = SURPLUS_DEFAULT$Pv)
  })

  make_inp <- function(policy_price, eta_r, eps_s, Sa = input$Sa, sigma = input$sigma) {
    list(policy_price = policy_price, P0 = BASE$P0, Q0 = BASE$Q0,
         eta_farm = derived_eta(eta_r, Sa, sigma), eps_s = eps_s, Sa = Sa,
         Pv = if (is.null(input$Pv)) SURPLUS_DEFAULT$Pv else input$Pv,
         payer = if (is.null(input$payer)) SURPLUS_DEFAULT$payer else input$payer,
         Pr0 = BASE$Pr0, household = HOUSEHOLD)
  }
  run_preset <- function(run, price = input$policy_price) {
    p <- ELAST_PRESET[[run]]
    simulate(make_inp(price, p$eta_r, p$eps_s))
  }

  inp <- reactive(make_inp(input$policy_price, input$eta_r, input$eps_s))
  res <- reactive(simulate(inp()))
  chk <- reactive(identity_check(res()))
  why <- reactive({
    w <- explain(res())
    if (lang() == "en") {
      w <- lapply(w, function(s) paste0(s, "  (KRW 100M)"))
      w$TE <- sub("이전효율", "Transfer efficiency", w$TE)
    }
    w
  })
  payer_name <- reactive(if (res()$payer == "gov") tr("정부", "government") else tr("유업체", "processors"))
  M  <- function(x) money(x, lang())
  KT <- function(x) kt(x, lang())

  output$price_big <- renderUI({
    r <- res()
    div(class = "price-readout",
        div(class = "price-big", span(class = "price-num", num(input$policy_price)),
            span(class = "price-unit", tr("원/L", "KRW/L"))),
        div(class = "price-delta",
            if (r$binding) paste0(tr("도입 전 P* 대비 ", "vs. pre-scheme P*: "), sgn(r$dp), tr("원/L (", " KRW/L ("), sgn(r$farm_pct, 1), "%)")
            else tr("도입 전 P* 이하라서 가격하한이 효력을 갖지 않습니다", "At or below pre-scheme P*, so the floor does not bind")))
  })

  output$eta_farm_txt <- renderUI({
    HTML(paste0(tr("→ 원유 수요탄력성 η<sub>a</sub> = <b>", "→ Raw milk demand elasticity η<sub>a</sub> = <b>"),
                num(inp()$eta_farm, 2), "</b>",
                "<span class='derived-sub'>", tr("Sa·η − Sb·σ로 계산", "Computed as Sa·η − Sb·σ"),
                " (Sa ", num(input$Sa, 2), ", σ ", num(input$sigma, 2), ")</span>"))
  })

  # ---- 머리글 ----
  cond_ui <- function() {
    r <- res()
    item <- function(k, v) span(class = "cond", span(class = "cond-k", k), span(class = "cond-v", v))
    tagList(span(class = "cond-lead", tr("이 조건이면", "With")),
            item(tr("원유가격", "Raw milk price"), wonL(r$Ps, lang())),
            item(tr("공급탄력성 ε", "Supply elasticity ε"), num(input$eps_s, 2)),
            item(tr("원유 수요탄력성 ηa", "Raw milk demand elasticity ηa"), num(inp()$eta_farm, 2)),
            item(tr("기간", "Horizon"), run_label()))
  }
  output$hero_cond_sim     <- renderUI(cond_ui())
  output$hero_cond_welfare <- renderUI(cond_ui())

  output$hero_title <- renderUI({
    r <- res()
    if (!r$binding) return(HTML(tr("가격하한 효력이 없어<br>시장은 도입 전 균형에서 거래됩니다",
                                   "The price floor does not bind,<br>so the market trades at the pre-scheme equilibrium")))
    if (lang() == "en")
      HTML(paste0("<span class='hl-gov'>", KT(r$stock), "</span> of raw milk is left over,<br>",
                  "and total welfare falls by <span class='hl-dwl'>", money(-r$dW, "en") |> sub(pattern = "^\\+", replacement = ""), "</span>"))
    else
      HTML(paste0("원유 <span class='hl-gov'>", KT(r$stock), "</span>이 남고,<br>",
                  "사회 전체 후생은 <span class='hl-dwl'>", num(-r$dW), "억 원</span> 줄어듭니다"))
  })
  output$hero_sub <- renderUI({
    r <- res()
    if (!r$binding) tr("연동제 가격이 도입 전 균형가격 P*(980.29원/L) 이하입니다. 가격을 올려 보세요.",
                       "The linked price is at or below the pre-scheme equilibrium P* (980.29 KRW/L). Try raising it.")
    else if (lang() == "en") paste0("The price is ", num(r$dp), " KRW/L (", num(r$farm_pct, 1), "%) above pre-scheme P*. Results use 2013 annual raw milk output and show the ",
                                    if (input$run == "SR") "response within one year." else "response after herd adjustment.")
    else paste0("도입 전 P*보다 ", num(r$dp), "원/L(", num(r$farm_pct, 1), "%) 올린 경우입니다. 2013년 연간 원유 생산량을 기준으로 ",
                if (input$run == "SR") "1년 안의 반응" else "착유우 수를 조정한 뒤의 반응", "을 계산했습니다.")
  })

  # ---- 지표 카드 ----
  output$vb_ps <- renderText(M(res()$dPS))
  output$vb_cs <- renderText(M(res()$dCS))
  output$vb_g  <- renderText(M(-res()$G))
  output$vb_dw <- renderText(M(res()$dW))
  output$vb_ps_title <- renderUI(tr("낙농가", "Farms"))
  output$vb_cs_title <- renderUI(tr("유업체", "Processors"))
  output$vb_g_title  <- renderUI(paste0(tr("잉여 원유 처리 (", "Surplus handling ("), payer_name(), ")"))
  output$vb_dw_title <- renderUI(tr("사회 전체", "Total"))

  observe({
    w <- why()
    update_tooltip("tt_ps", w$dPS)
    update_tooltip("tt_cs", w$dCS)
    update_tooltip("tt_g",  paste0(w$G, tr(" → 후생 변화는 −G", " → welfare change is −G")))
    update_tooltip("tt_dw", paste0(w$dW, tr(" = −사중손실. ", " = −deadweight loss. "), w$DWL))
  })

  output$story <- renderUI({
    r <- res(); h <- r$household; c <- chk(); lg <- lang()
    if (!r$binding) return(p(tr("연동제 가격이 도입 전 균형가격 P* 이하이므로 가격하한이 효력을 갖지 않습니다. 시장은 P*와 Q*에서 거래되고 후생 변화는 없습니다.",
                                "The linked price is at or below the pre-scheme equilibrium P*, so the floor does not bind. The market trades at P*, Q* and welfare does not change.")))
    txt <- if (lg == "en") paste0(
      "Processors pay <b>", sgn(r$dp), " KRW</b> more per litre and cut purchases to <b>", KT(r$Qd),
      "</b>. Farms produce <b>", KT(r$Qs), "</b>, leaving <b>", KT(r$stock), "</b> of surplus that the ",
      payer_name(), " buys and turns into milk powder. If the cost passes into retail prices, white milk rises by about <b>",
      won(r$retail_won, lg), "</b> per litre and a household in ", tolower(HOUSEHOLD_GROUP_EN[1]), " pays about <b>",
      won(h$monthly_won[1], lg), "</b> more per month.")
    else paste0(
      "유업체는 원유를 리터당 <b>", sgn(r$dp), "원</b> 더 주고 사며 구매량을 <b>", KT(r$Qd),
      "</b>으로 줄입니다. 낙농가는 <b>", KT(r$Qs), "</b>을 생산해 <b>", KT(r$stock),
      "</b>이 남고, ", payer_name(), "가 이를 사서 분유로 전환합니다. ",
      "부담이 소매가로 전가되면 흰 우유는 리터당 약 <b>", won(r$retail_won, lg), "</b> 오르고, ",
      h$group[1], " 가구는 한 달에 약 <b>", won(h$monthly_won[1], lg), "</b>을 더 냅니다.")
    tagList(
      p(HTML(txt)),
      p(class = "check-line",
        if (c$ok) tagList(bs_icon("check-circle"), tr(" 검산: ΔCS + ΔPS − G = ΔW, 곡선 아래 적분값과 일치",
                                                      " Check: ΔCS + ΔPS − G = ΔW matches integration under the curves"))
        else tagList(bs_icon("x-circle"), paste0(tr(" 검산 불일치: 차이 ", " Check failed: gap "), num(c$gap, 1)))))
  })

  output$sd_title <- renderUI({
    r <- res()
    if (!r$binding) tr("연동제 가격이 P* 이하이므로 가격하한 효력이 없습니다", "The linked price is at or below P*, so the floor does not bind")
    else if (lang() == "en") paste0("At ", wonL(r$Ps, "en"), ", farm output of ", KT(r$Qs), " exceeds processor purchases of ",
                                    KT(r$Qd), ", leaving ", KT(r$stock), " of surplus")
    else paste0("원유 ", wonL(r$Ps, "ko"), "에서 낙농가 생산량 ", KT(r$Qs), "이 유업체 구매량 ",
                KT(r$Qd), "을 넘어 ", KT(r$stock), "이 남습니다")
  })
  output$sd_plot <- renderPlot(plot_sd(res(), lang()), res = 96, alt = reactive({
    r <- res()
    tr(paste0("수요와 공급 그래프. 원유가격 ", num(r$Ps), "원/L에서 공급량 ", num(r$Qs), "천 톤, 수요량 ", num(r$Qd), "천 톤, 잉여 ", num(r$stock), "천 톤."),
       paste0("Supply-demand chart. At ", num(r$Ps), " KRW/L, supply ", num(r$Qs), " kt, demand ", num(r$Qd), " kt, surplus ", num(r$stock), " kt."))
  }))

  # ---- 가격과 탄력성이 바뀌면 ----
  whatif <- reactive({
    m <- input$metric; lg <- lang()
    pg <- seq(980, 1300, by = 10)
    eg <- seq(0.05, 2, by = 0.05)
    lab_p <- tr2(lg, "원유가격(원/L)에 따라", "By raw milk price (KRW/L)")
    lab_e <- tr2(lg, "원유 공급탄력성 ε에 따라", "By supply elasticity ε")
    df <- rbind(
      data.frame(panel = lab_p, x = pg,
                 y = vapply(pg, function(p) metric_val(simulate(make_inp(p, input$eta_r, input$eps_s)), m), 0)),
      data.frame(panel = lab_e, x = eg,
                 y = vapply(eg, function(e) metric_val(simulate(make_inp(input$policy_price, input$eta_r, e)), m), 0)))
    df$panel <- factor(df$panel, levels = c(lab_p, lab_e))
    v <- metric_val(res(), m)
    cur <- data.frame(panel = factor(c(lab_p, lab_e), levels = c(lab_p, lab_e)),
                      x = c(input$policy_price, input$eps_s), y = c(v, v),
                      xpos = c((input$policy_price - 980) / 320, (input$eps_s - 0.05) / 1.95))
    list(df = df, cur = cur, m = m, lg = lg)
  })
  output$whatif_plot <- renderPlot({
    w <- whatif(); plot_whatif(w$df, w$cur, w$m, w$lg)
  }, res = 96, alt = reactive({
    w <- whatif()
    paste0(METRIC_NAMES[[w$lg]][METRIC_IDS == w$m],
           tr2(w$lg, ": 원유가격과 공급탄력성에 따른 변화를 보여 주는 선 그래프.", ": line chart of changes by raw milk price and supply elasticity."))
  }))

  output$market_tbl <- renderTable({
    r <- res()
    base  <- c(r$P0, r$Q0, r$Q0, 0, BASE$Pr0)
    after <- c(r$Ps, r$Qs, r$Qd, r$stock, BASE$Pr0 + r$retail_won)
    chg   <- after - base
    rate  <- ifelse(base > 0, paste0(" (", sgn(chg / base * 100, 1), "%)"), "")
    df <- data.frame(
      a = tr(c("원유가격 (원/L)", "낙농가 생산량 (천 톤)", "유업체 구매량 (천 톤)", "남는 원유 (천 톤)", "참고: 흰 우유 소매가 (원/L)"),
             c("Raw milk price (KRW/L)", "Farm output (kt)", "Processor purchases (kt)", "Surplus milk (kt)", "Ref.: white milk retail (KRW/L)")),
      b = num(base), c = num(after), d = paste0(sgn(chg), rate))
    names(df) <- tr(c("항목", "기준", "현재", "변화"), c("Item", "Base", "Current", "Change"))
    df
  }, hover = TRUE, width = "100%", align = "lrrr")

  output$elast_now_tbl <- renderUI({
    rng <- ELAST_TBL$`타당 범위`
    df <- data.frame(
      a = tr(c("ε 원유 공급 (낙농가)", "η 흰 우유 소매 수요", "Sa 소매가 중 원유 몫", "σ 원유와 기타 투입재 대체", "ηa 원유 수요 (계산)"),
             c("ε raw milk supply (farms)", "η retail white milk demand", "Sa raw milk share of retail", "σ raw milk vs. other inputs", "ηa raw milk demand (derived)")),
      b = c(num(input$eps_s, 2), num(input$eta_r, 2), num(input$Sa, 2), num(input$sigma, 2), paste0("<b>", num(inp()$eta_farm, 2), "</b>")),
      c = c(num(ELAST_PRESET$SR$eps_s, 2), num(ELAST_PRESET$SR$eta_r, 2), num(ELAST_DEFAULT$Sa, 2), num(ELAST_DEFAULT$sigma, 2), tr("계산", "derived")),
      d = c(num(ELAST_PRESET$LR$eps_s, 2), num(ELAST_PRESET$LR$eta_r, 2), num(ELAST_DEFAULT$Sa, 2), num(ELAST_DEFAULT$sigma, 2), tr("계산", "derived")),
      e = c(rng[2], rng[3], rng[4], rng[5], "-"))
    names(df) <- tr(c("탄력성", "현재", "단기 기본", "장기 기본", "타당 범위"),
                    c("Elasticity", "Current", "SR default", "LR default", "Plausible range"))
    tbl_ui(add_status(df, ELAST_TBL$상태[c(2, 3, 4, 5, 1)], lang()), "lrrrrl")
  })


  # ---- 후생 결과 ----
  output$welfare_title <- renderUI({
    r <- res()
    if (!r$binding || is.na(r$TE) || r$TE <= 0) return(tr("가격하한 효력이 없어 후생 변화가 없습니다", "The floor does not bind, so welfare does not change"))
    if (lang() == "en")
      HTML(paste0("For every ₩1 farms gain,<br>", if (r$payer == "gov") "processors and government pay " else "processors pay ",
                  "<span class='hl-dwl'>₩", num(1 / r$TE, 2), "</span>"))
    else HTML(paste0("낙농가가 1원을 얻을 때<br>", if (r$payer == "gov") "유업체와 정부가" else "유업체가",
                     " <span class='hl-dwl'>", num(1 / r$TE, 2), "원</span>을 냅니다"))
  })

  output$stake_rows <- renderUI({
    r <- res(); w <- why()
    vals <- c(r$dPS, r$dCS, -r$G, r$dW)
    mx <- max(abs(vals), 1e-9)
    row <- function(icon, name, sub, v, cls, tip, total = FALSE) {
      wd <- abs(v) / mx * 48
      pos <- if (v >= 0) sprintf("left:50%%;width:%.1f%%", wd) else sprintf("right:50%%;width:%.1f%%", wd)
      div(class = paste("srow", if (total) "total"),
          span(class = "srow-icon", art(icon, 40)),
          div(div(class = "srow-name", name, tooltip(span(class = "tip-icon", tabindex = "0", bs_icon("info-circle")), tip)),
              div(class = "srow-sub", sub)),
          div(class = "srow-track", div(class = "axis"), div(class = paste("dbar", cls), style = pos)),
          div(class = paste("srow-val", cls), M(v)))
    }
    div(class = "srows",
      row("cow", tr("낙농가", "Farms"), tr("생산자잉여 변화 ΔPS", "Producer surplus change ΔPS"), r$dPS, "farm", w$dPS),
      row("factory", tr("유업체", "Processors"), tr("구매자잉여 변화 ΔCS", "Buyer surplus change ΔCS"), r$dCS, "proc", w$dCS),
      row("sack", paste0(tr("잉여 원유 처리 (", "Surplus handling ("), payer_name(), ")"),
          tr("처리 비용 G를 뺀 값 (−G)", "Minus the handling cost (−G)"), -r$G, "gov", w$G),
      row("scale", tr("사회 전체", "Total"), tr("순후생 변화 ΔW = 위 세 줄의 합", "Net welfare change ΔW = sum of the rows above"),
          r$dW, "dwl", w$dW, total = TRUE)
    )
  })

  output$metric_rows <- renderUI({
    r <- res(); w <- why()
    row <- function(icon, name, sub, val, tip = NULL)
      div(class = "mrow",
          span(class = "mrow-icon", art(icon, 36)),
          div(div(class = "mrow-name", name, if (!is.null(tip)) tooltip(span(class = "tip-icon", tabindex = "0", bs_icon("info-circle")), tip)),
              div(class = "mrow-sub", sub)),
          div(class = "mrow-val", val))
    div(class = "mbox",
        row("cycle", tr("이전효율", "Transfer efficiency"),
            tr("낙농가가 얻은 몫 ÷ 다른 주체가 낸 몫 (1에 가까울수록 효율적)", "Farm gain ÷ what others pay (closer to 1 is more efficient)"),
            if (is.na(r$TE)) "-" else num(r$TE, 2), w$TE),
        row("sack", tr("남는 원유", "Surplus milk"), tr("낙농가 생산량 − 유업체 구매량", "Farm output − processor purchases"), KT(r$stock)),
        row("milk", tr("흰 우유 소매가", "White milk retail price"),
            paste0(tr("원유가 변화의 Sa(", "Sa ("), num(input$Sa, 2), tr(")만큼 전가", ") of the raw milk change passed through")),
            paste0(sgn(r$retail_won), tr("원/L", " KRW/L"))))
  })

  srlr <- reactive(list(SR = run_preset("SR"), LR = run_preset("LR")))

  output$srlr_plot <- renderPlot({
    s <- srlr(); lg <- lang(); f <- if (lg == "en") 0.1 else 1
    lv <- tr2(lg, c("낙농가", "유업체", "잉여 처리", "사회 전체"), c("Farms", "Processors", "Surplus handling", "Total"))
    pl <- tr2(lg, c("단기 기본값", "장기 기본값"), c("Short-run default", "Long-run default"))
    df <- do.call(rbind, lapply(names(s), function(n) data.frame(
      기간 = ifelse(n == "SR", pl[1], pl[2]), 항목 = lv,
      값 = c(s[[n]]$dPS, s[[n]]$dCS, -s[[n]]$G, s[[n]]$dW) * f)))
    df$항목 <- factor(df$항목, levels = rev(lv))
    df$기간 <- factor(df$기간, levels = rev(pl))
    lim <- max(abs(df$값)) * 1.35
    ggplot(df, aes(값, 항목, fill = 기간)) +
      geom_col(position = position_dodge(0.8), width = 0.74) +
      geom_vline(xintercept = 0, color = COL$ink, linewidth = 0.5) +
      geom_text(aes(label = num(값, if (f < 1) 1 else 0), hjust = ifelse(값 >= 0, -0.15, 1.15)), position = position_dodge(0.8),
                family = PLOT_FONT, size = 3.4, color = COL$sub) +
      scale_fill_manual(values = setNames(c(COL$period_sr, COL$period_lr), pl), breaks = pl) +
      scale_x_continuous(labels = function(x) num(x), limits = c(-lim, lim)) +
      labs(x = NULL, y = NULL) + theme_plot() +
      theme(legend.position = "top", legend.justification = "left",
            panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "#EDF0EE", linewidth = 0.4),
            axis.text.y = element_text(color = COL$ink, size = 12))
  }, res = 96, alt = reactive(tr("단기와 장기 기본 탄력성에서 이해관계자별 후생 변화를 비교한 막대그래프.",
                                 "Bar chart comparing welfare changes by stakeholder under short- and long-run default elasticities.")))

  output$reality_bar <- renderUI({
    q13 <- TREND$qty[TREND$year == 2013]; q14 <- TREND$qty[TREND$year == 2014]
    sr <- run_preset("SR", BASE$P_policy)$Qs; lr <- run_preset("LR", BASE$P_policy)$Qs
    f <- function(q) paste0(sgn((q / q13 - 1) * 100, 1), "%")
    div(class = "note-bar", HTML(tr(
      paste0("<b>실제와 비교</b><br>2014년 실제 원유 생산량은 <b>", f(q14), "</b> 변했습니다. 같은 가격(2013년 실제 연동제 가격)에서 모형은 단기 ", f(sr), ", 장기 ", f(lr), "로 계산합니다."),
      paste0("<b>Reality check</b><br>Actual raw milk output changed by <b>", f(q14), "</b> in 2014. At the same price (the actual 2013 linked price) the model gives ", f(sr), " in the short run and ", f(lr), " in the long run."))))
  })

  output$hh_rows <- renderUI({
    r <- res(); h <- r$household; lg <- lang()
    burden <- h$income_pct
    mx <- max(burden, 1e-12)
    lab <- tr(c("저소득 가구", "고소득 가구"), c("Low-income", "High-income"))
    grp <- if (lg == "en") HOUSEHOLD_GROUP_EN else h$group
    rows <- lapply(seq_len(nrow(h)), function(i)
      div(class = "hrow",
          span(class = "hrow-icon", art("house", 36)),
          div(div(class = "hrow-name", lab[i]), div(class = "hrow-sub", grp[i])),
          div(div(class = "hrow-val", paste0(tr("월 ", ""), won(h$monthly_won[i], lg), tr("", " /mo"))),
              div(class = "hrow-sub text-end", paste0(tr("연 ", ""), won(h$monthly_won[i] * 12, lg), tr("", " /yr")))),
          div(div(class = "hbar", div(style = sprintf("width:%.0f%%", burden[i] / mx * 100))),
              div(class = "hbar-cap", paste0(tr("소비지출 대비 ", "Share of spending "), pct(burden[i], 3))))))
    ratio <- if (burden[2] > 0) burden[1] / burden[2] else NA
    tagList(div(class = "hrows", rows),
            div(class = "note-bar", HTML(if (lg == "en")
              paste0("Relative to spending, the low-income burden is ", if (is.na(ratio)) "-" else paste0("<b>", num(ratio, 1), "×</b>"), " the high-income burden.")
              else paste0("소비지출 대비 부담률은 저소득층이 고소득층의 ", if (is.na(ratio)) "-" else paste0("<b>", num(ratio, 1), "배</b>"), "입니다.")),
              div(class = "note-sub", tr(
                paste0("소매가가 ", sgn(r$retail_pct, 1), "% 오른다고 가정했습니다. 전가율로만 계산하므로 단기와 장기 결과가 같습니다. 가계자료는 아직 임시 값입니다."),
                paste0("Assumes retail prices rise by ", sgn(r$retail_pct, 1), "%. Computed from pass-through only, so short and long run are the same. Household data are still placeholders.")))))
  })

  output$sens_tbl <- renderUI({
    eps_grid <- c(0.2, 0.5, 1.0)
    dp_grid  <- c(50, 100, 150)
    m <- outer(eps_grid, dp_grid, Vectorize(function(e, d)
      simulate(make_inp(BASE$P0 + d, input$eta_r, e))$dW))
    cur_i <- which.min(abs(eps_grid - input$eps_s))
    cur_j <- which.min(abs(dp_grid - (input$policy_price - BASE$P0)))
    mx <- max(abs(m), 1e-9)
    rlab <- tr(c("낮음", "기준", "높음"), c("Low", "Base", "High"))
    clab <- tr(c("낮은 인상폭", "기준 인상폭", "높은 인상폭"), c("Small increase", "Base increase", "Large increase"))
    tags$table(class = "sens2",
      tags$caption(class = "visually-hidden", tr("공급탄력성과 인상폭에 따른 사회 전체 후생 변화", "Total welfare change by supply elasticity and increase")),
      tags$thead(tags$tr(tags$th(scope = "col", tr("공급탄력성 \\ 인상폭", "Supply elasticity \\ increase")),
                         lapply(seq_along(dp_grid), function(j)
                           tags$th(scope = "col", clab[j], span(class = "th-sub", paste0("+", dp_grid[j], tr("원/L", " KRW/L"))))))),
      tags$tbody(lapply(seq_along(eps_grid), function(i)
        tags$tr(tags$th(scope = "row", class = "rh", rlab[i], span(class = "th-sub", paste0("ε ", num(eps_grid[i], 1)))),
                lapply(seq_along(dp_grid), function(j)
                  tags$td(class = if (i == cur_i && j == cur_j) "cur",
                          style = sprintf("background: rgba(194,149,47,%.2f)", 0.08 + 0.42 * abs(m[i, j]) / mx),
                          M(m[i, j])))))))
  })

  # ---- 정책과 시장 ----
  output$structure_tbl <- renderUI({
    lg <- lang()
    df <- if (lg == "en") STRUCTURE_TBL_EN else STRUCTURE_TBL[, c("항목", "내용")]
    tbl_ui(add_status(df, STRUCTURE_TBL$상태, lg), "lll")
  })

  output$baseline_tbl <- renderUI({
    lg <- lang()
    df <- if (lg == "en") cbind(BASELINE_TBL_EN[, c("Item", "Value", "Unit")], Year = BASELINE_TBL$연도, Source = BASELINE_TBL_EN$Source)
          else BASELINE_TBL[, c("항목", "값", "단위", "연도", "출처")]
    tbl_ui(add_status(df, BASELINE_TBL$상태, lg), "lrllll")
  })

  output$m2013_plot <- renderPlot({
    lg <- lang(); m <- MONTHLY_2013
    per <- tr2(lg, c("도입 전 (1-7월)", "도입 후 (8-12월)"), c("Pre-scheme (Jan-Jul)", "Post-scheme (Aug-Dec)"))
    m$기간 <- ifelse(m$month <= 7, per[1], per[2])
    ggplot(m, aes(month, price)) +
      annotate("rect", xmin = 7.5, xmax = 12.6, ymin = -Inf, ymax = Inf, fill = COL$brand100, alpha = 0.8) +
      annotate("segment", x = 0.6, xend = 7.4, y = BASE$P0, yend = BASE$P0, linetype = "dashed", color = COL$muted) +
      annotate("segment", x = 7.6, xend = 12.4, y = BASE$P_policy, yend = BASE$P_policy, linetype = "dashed", color = COL$brand800) +
      geom_line(color = COL$ink, linewidth = 0.8) +
      geom_point(aes(color = 기간), size = 2.8) +
      annotate("text", x = 4, y = BASE$P0 - 18,
               label = paste0("P* ", wonL(BASE$P0, lg, 2), tr2(lg, " (1-7월 평균)", " (Jan-Jul avg.)")),
               family = PLOT_FONT, size = TXT, color = COL$muted) +
      annotate("text", x = 10.3, y = 1010,
               label = paste0(tr2(lg, "연동제 가격\n", "Linked price\n"), wonL(BASE$P_policy, lg, 2), tr2(lg, "\n(8-12월 평균)", "\n(Aug-Dec avg.)")),
               family = PLOT_FONT, size = TXT, color = COL$brand800, lineheight = 1.1) +
      annotate("text", x = 7.4, y = 1080,
               label = tr2(lg, "8월 연동제 적용\n전월 대비 +96.45원/L", "Scheme applied in Aug\n+96.45 KRW/L vs. Jul"),
               family = PLOT_FONT, size = TXT, color = COL$ink, hjust = 1, lineheight = 1.1) +
      annotate("text", x = c(4, 10), y = 1117, label = per, family = PLOT_FONT, size = TXT, fontface = "bold",
               color = c(COL$muted, COL$brand800)) +
      scale_color_manual(values = setNames(c(COL$period_sr, COL$brand800), per), guide = "none") +
      scale_x_continuous(NULL, breaks = 1:12, labels = if (lg == "en") month.abb else paste0(1:12, "월")) +
      scale_y_continuous(NULL, limits = c(940, 1120), labels = function(x) num(x)) +
      theme_plot()
  }, res = 96, alt = reactive(tr("2013년 월별 원유수취가격 선 그래프입니다. 7월 965.10원에서 8월 1,061.55원으로 올랐습니다.",
                                 "Line chart of 2013 monthly farm-gate prices, rising from 965.10 KRW in July to 1,061.55 KRW in August.")))

  # 단위가 다른 두 계열(가격, 생산량)의 '방향'을 비교하는 그래프이므로 2013년 = 100 지수로 한 축에 그린다.
  # 이중 축과 달리 축 눈금을 임의로 맞출 수 없어 정직하다. 절댓값은 '표' 탭에 있다.
  output$trend_plot <- renderPlot({
    lg <- lang()
    nm <- tr2(lg, c("원유가격", "생산량"), c("Price", "Output"))
    df <- rbind(data.frame(s = nm[1], year = TREND$year, y = TREND$price / TREND$price[1] * 100),
                data.frame(s = nm[2], year = TREND$year, y = TREND$qty / TREND$qty[1] * 100))
    df$s <- factor(df$s, levels = nm)
    end <- df[df$year == max(df$year), ]
    end$lab <- paste0(end$s, " ", num(end$y))
    ev <- TREND_EVENTS
    # 두 사건 이름이 겹치지 않도록 높이를 다르게 둔다
    ev_lab <- data.frame(year = ev$year, label = if (lg == "en") TREND_EVENTS_EN else ev$label, hj = c(-0.04, 1.04), y = c(127, 120))
    cols <- setNames(c(COL$ink, CONCEPT$farm$line), nm)
    ggplot(df, aes(year, y, color = s)) +
      geom_hline(yintercept = 100, color = COL$line_strong, linewidth = 0.5) +
      geom_vline(data = ev, aes(xintercept = year), linetype = "dashed", color = COL$muted) +
      geom_text(data = ev_lab, aes(x = year, y = y, label = label, hjust = hj), inherit.aes = FALSE,
                family = PLOT_FONT, size = 3.3, color = COL$sub) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 1.8) +
      geom_text(data = end, aes(label = lab), hjust = -0.12, family = PLOT_FONT, fontface = "bold", size = TXT, show.legend = FALSE) +
      scale_color_manual(values = cols, guide = "none") +
      scale_x_continuous(NULL, breaks = TREND$year, labels = paste0("'", substr(TREND$year, 3, 4)),
                         expand = expansion(add = c(0.4, 3.4))) +
      scale_y_continuous(tr2(lg, "지수 (2013년 = 100)", "Index (2013 = 100)"), limits = c(88, 128), breaks = seq(90, 125, 5)) +
      theme_plot()
  }, res = 96, alt = reactive(tr("2013년을 100으로 둔 원유가격과 생산량 지수. 2025년 가격은 122, 생산량은 93입니다.",
                                 "Index of raw milk price and output with 2013 = 100. In 2025 price is 122 and output is 93.")))
  output$trend_tbl <- renderTable({
    lg <- lang()
    ev <- setNames(if (lg == "en") TREND_EVENTS_EN else TREND_EVENTS$label, TREND_EVENTS$year)
    df <- data.frame(a = as.character(TREND$year), b = num(TREND$price, 2), c = num(TREND$qty, 1),
                     d = ifelse(as.character(TREND$year) %in% names(ev), ev[as.character(TREND$year)], ""))
    names(df) <- tr(c("연도", "원유수취가격 (원/L)", "생산량 (천 톤)", "정책 변경"),
                    c("Year", "Farm-gate price (KRW/L)", "Production (kt)", "Policy change"))
    df
  }, hover = TRUE, width = "100%", align = "lrrl")

  # ---- 방법론 ----
  output$valid_tbl <- renderTable({
    v <- run_validation(VALIDATION_CASE)
    v$기대값 <- num(v$기대값); v$앱 <- num(v$앱)
    if (lang() == "en") { v$항목 <- VALIDATION_ITEMS_EN; names(v) <- c("Item", "Expected", "App", "Match") }
    v
  }, width = "100%", hover = TRUE, align = "lrrc")
  output$check_tbl <- renderTable({
    r <- res(); a <- integrate_dW(r)
    df <- data.frame(a = tr(c("면적 공식 ΔCS + ΔPS − G", "곡선 아래 적분", "차이"),
                            c("Area formulas ΔCS + ΔPS − G", "Integration under the curves", "Difference")),
                     b = c(num(r$dW, 1), num(a, 1), num(a - r$dW, 2)))
    names(df) <- c(tr("방법", "Method"), "ΔW")
    df
  }, width = "100%", hover = TRUE, align = "lr")
  output$source_tbl <- renderUI({
    lg <- lang()
    df <- if (lg == "en") SOURCES_TBL_EN else SOURCES_TBL[, c("자료", "기관")]
    tbl_ui(add_status(df, SOURCES_TBL$상태, lg), "lll")
  })
  output$elast_tbl <- renderUI({
    lg <- lang(); e <- ELAST_TBL
    df <- if (lg == "en")
      data.frame(Symbol = ELAST_TBL_EN$Symbol, `Short run` = e$단기, `Long run` = e$장기,
                 `Plausible range` = sub("계산값", "Derived", e$`타당 범위`),
                 Source = ELAST_TBL_EN$Source, `Market level` = ELAST_TBL_EN$Level, check.names = FALSE)
    else e[, setdiff(names(e), "상태")]
    tbl_ui(add_status(df, e$상태, lg), "lrrrlll")
  })
}

shinyApp(ui, server, enableBookmarking = "url")
