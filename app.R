# =====================================================================
# 원유가격 연동제 정책 시뮬레이터 (LESE412 기말 프로젝트)
# 실행: R에서 shiny::runApp("이 폴더 경로")   (한글 경로·자료 때문에 UTF-8 로캘 권장)
# 계산은 R/model.R 의 simulate() 하나만 사용한다.
# 기준연도 2013년, 자료는 낙농진흥회 낙농통계광장.
#
# 화면 설계 원칙
#   - 사용자: 탄력성과 원유가격을 바꿔 가며 결과를 예측하는 정책 분석가
#   - 핵심 메시지: "가격이 이러면, 탄력성이 이러면 → 결과는 이렇다"
#     → 입력(사이드바)을 열어 두고, 머리글이 입력 조건과 결과를 한 문장으로 말한다.
#   - 일관성: 후생은 모두 '변화량(Δ, 부호 포함)'으로 표시하고, 개념마다 색 하나를 쓴다.
#       낙농가 = 초록, 유업체 = 빨강, 잉여 원유 처리 = 청회색, 사회 전체 = 금색
#   - 글꼴은 Pretendard 하나 (화면: www/fonts, 그래프: fonts/SimSans = Pretendard 부분집합)
# =====================================================================
library(shiny)
library(bslib)
library(bsicons)
library(ggplot2)

source("R/data.R",  encoding = "UTF-8")
source("R/model.R", encoding = "UTF-8")

# ---- 색 (www/style.css의 :root 변수와 같은 값) ------------------------------
COL <- list(brand = "#1C4536", brand800 = "#24584A", brand100 = "#E2F0E8",
            ink = "#1C2724", sub = "#3E4945", muted = "#5C6763", line = "#E1E6E3", line_strong = "#C9D1CD",
            period_sr = "#A3AFAA", period_lr = "#4E5A55")
# 개념 색: line = 선·막대, light = 면적, ink = 글자
CONCEPT <- list(
  farm = list(line = "#4A9A75", light = "#A9D6BF", ink = "#1E6B4B"),
  proc = list(line = "#D47660", light = "#F0B9AB", ink = "#A8452F"),
  gov  = list(line = "#7891AC", light = "#BFCDDB", ink = "#46607A"),
  dwl  = list(line = "#C2952F", light = "#EBD08F", ink = "#7A5700")
)
AREA_LAB <- c(A = "A  유업체 → 낙농가", B = "B  잉여 매입비 → 낙농가", D = "D  사중손실")
AREA_COL <- setNames(c(CONCEPT$proc$light, CONCEPT$gov$light, CONCEPT$dwl$light), AREA_LAB)

# ---- 그래프 글꼴 ----------------------------------------------------------------
# fonts/SimSans-*.ttf: Pretendard의 한글 2,350자 부분집합 (화면 글꼴과 같은 모양)
PLOT_FONT <- "SimSans"
if (R.version$os == "emscripten") {
  # 웹 배포판(shinylive/webR)에는 한글 폰트가 없으므로 동봉한 글꼴을 폰트 폴더에 복사해 쓴다
  font_dir <- "/home/web_user/fonts"
  dir.create(font_dir, recursive = TRUE, showWarnings = FALSE)
  if (dir.exists("fonts")) {
    file.copy(list.files("fonts", pattern = "\\.ttf$", full.names = TRUE), font_dir, overwrite = TRUE)
  } else {
    # 폰트 폴더가 없는 실행(shinylive.io 링크 등)에서는 원본 Pretendard를 받아 쓴다
    for (f in c("Pretendard-Regular.ttf", "Pretendard-SemiBold.ttf")) try(download.file(
      paste0("https://cdn.jsdelivr.net/npm/pretendard@1.3.9/dist/public/static/alternative/", f),
      file.path(font_dir, f), mode = "wb", quiet = TRUE), silent = TRUE)
    PLOT_FONT <- "Pretendard"
  }
} else if (!any(grepl("SimSans", tryCatch(system("fc-list", intern = TRUE, ignore.stderr = TRUE),
                                          error = function(e) "")))) {
  # 로컬 실행에서 글꼴이 설치되지 않았으면 운영체제 한글 글꼴로 대신한다
  PLOT_FONT <- if (.Platform$OS.type == "windows") "Malgun Gothic" else "sans"
}

theme_app <- bs_theme(
  version = 5, primary = COL$brand800, success = "#2E7D5B", danger = CONCEPT$proc$ink,
  bg = "#FFFFFF", fg = COL$ink,
  base_font = font_collection("Pretendard Variable", "Pretendard", "Malgun Gothic", "Apple SD Gothic Neo", "sans-serif"),
  "border-radius" = "0.75rem", "font-size-base" = "0.875rem"
)

# ---- 선 아이콘 (24×24, stroke) -------------------------------------------
ICON <- list(
  cow = '<path d="M8 6.6 6.3 3.9M16 6.6l1.7-2.7"/><path d="M7.3 8.3C5.4 8.7 3.4 8.2 2.5 6.9c1.5-.6 3.3-.7 4.9-.2"/><path d="M16.7 8.3c1.9.4 3.9-.1 4.8-1.4-1.5-.6-3.3-.7-4.9-.2"/><path d="M7.6 13.4 7 8.6C7 6.9 9.2 5.6 12 5.6s5 1.3 5 3l-.6 4.8"/><rect x="6.5" y="13" width="11" height="7.5" rx="3.7"/><path d="M10 16.6v.1M14 16.6v.1M9.6 10.3v.1M14.4 10.3v.1"/>',
  milk = '<path d="M9 2.5h6"/><path d="M9.6 2.5v3.3c0 .5-.2 1-.6 1.3C7.7 8.1 7 9.5 7 11.1v8.4a2 2 0 0 0 2 2h6a2 2 0 0 0 2-2v-8.4c0-1.6-.7-3-2-4-.4-.3-.6-.8-.6-1.3V2.5"/><path d="M7 13.5h10"/>',
  factory = '<path d="M3 20.5v-10l5 3v-3l5 3v-3l5 3V3.5h3v17z"/><path d="M2 20.5h20"/><path d="M7 17h1.5M11.5 17H13M16 17h1.5"/>',
  sack = '<path d="M9 3.5h6l-1.6 3.2C16.6 8.3 19 11.8 19 15.5c0 2.8-1.7 5-4.5 5h-5C6.7 20.5 5 18.3 5 15.5c0-3.7 2.4-7.2 5.6-8.8z"/><path d="M10.4 6.7h3.2M9.5 15.5h5"/>',
  scale = '<path d="M12 3.5v17M8 20.5h8M4.5 7h15"/><path d="M4.5 7 2 13a2.5 2.5 0 0 0 5 0zM19.5 7 17 13a2.5 2.5 0 0 0 5 0z"/>',
  coins = '<ellipse cx="9" cy="6" rx="6" ry="2.5"/><path d="M3 6v4c0 1.4 2.7 2.5 6 2.5s6-1.1 6-2.5V6M3 10v4c0 1.4 2.7 2.5 6 2.5"/><ellipse cx="15" cy="14" rx="6" ry="2.5"/><path d="M9 14v4c0 1.4 2.7 2.5 6 2.5s6-1.1 6-2.5v-4"/>',
  cycle = '<path d="M20 11a8 8 0 0 0-14.3-4.9L4 8"/><path d="M4 3.5V8h4.5"/><path d="M4 13a8 8 0 0 0 14.3 4.9L20 16"/><path d="M20 20.5V16h-4.5"/>',
  house = '<path d="M3.5 11 12 4l8.5 7"/><path d="M5.5 9.5v11h13v-11"/><path d="M10 20.5v-5h4v5"/>',
  truck = '<path d="M2.5 6.5h11v10h-11z"/><path d="M13.5 9.5h4l3 3.5v3.5h-7"/><circle cx="6.5" cy="17.5" r="2"/><circle cx="17" cy="17.5" r="2"/>',
  cart = '<path d="M2.5 3.5h3l2.2 11h10.8l2-7.5H6.4"/><circle cx="9.5" cy="19" r="1.5"/><circle cx="17" cy="19" r="1.5"/>'
)
li <- function(name, size = 24, cls = "")
  HTML(sprintf('<svg class="li %s" width="%d" height="%d" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">%s</svg>',
               cls, size, size, ICON[[name]]))

# 그림 주소에 파일 내용으로 만든 버전 표시를 붙인다 (그림을 바꾸면 브라우저 캐시의 옛 그림 대신 새 그림을 받는다)
asset_src <- function(f) paste0(f, "?v=", substr(unname(tools::md5sum(file.path("www", f))), 1, 8))

# 아이콘 일러스트: www/il-<이름>.png가 있으면 그 그림을, 없으면 같은 이름의 선 아이콘을 쓴다
ART_FILES <- sub("^il-(.*)\\.png$", "\\1", list.files("www", pattern = "^il-.*\\.png$"))
ART_SRC <- setNames(vapply(paste0("il-", ART_FILES, ".png"), asset_src, ""), ART_FILES)
art <- function(name, w, h = w, fallback = name, cls = "") {
  if (name %in% ART_FILES) tags$img(src = ART_SRC[[name]], class = trimws(paste("art", cls)),
                                     width = w, height = h, alt = "")
  else li(fallback, min(w, h), cls)
}

# 머리글 일러스트 (설명 탭인 '정책·시장'에만 쓴다. 숫자를 보는 탭은 결과 문장에 자리를 준다)
HERO_FILE <- c("hero.png", "hero.webp")[file.exists(file.path("www", c("hero.png", "hero.webp")))][1]
HERO_ART <- if (!is.na(HERO_FILE)) tags$img(src = asset_src(HERO_FILE), class = "hero-art", alt = "") else NULL

# ---- 숫자 표시 도우미 ---------------------------------------------------
# 음수는 하이픈(-)이 아니라 빼기 기호(−)로 쓴다
num  <- function(x, d = 0) {
  x <- round(x, d); x[x == 0] <- 0                     # −0 → 0
  sub("^-", "−", formatC(x, big.mark = ",", format = "f", digits = d))
}
sgn  <- function(x, d = 0) paste0(ifelse(round(x, d) > 0, "+", ""), num(x, d))
eok  <- function(x) paste0(sgn(x), "억 원")          # 후생 변화 (부호 포함)
wonL <- function(x, d = 0) paste0(num(x, d), "원/L")
won  <- function(x) paste0(num(x), "원")
pct  <- function(x, d = 1) paste0(num(x, d), "%")

# 자료 상태: 점 + 글자 (색만으로 구분하지 않는다)
status_chip <- function(s) {
  cls <- switch(s, "확인 완료" = "ok", "확인 필요" = "check", "가상" = "virtual", "임시 값" = "temp",
                "-" = "none", "temp")
  as.character(tags$span(class = paste("st", cls), tags$i(`aria-hidden` = "true"), s))
}
chipify <- function(df) {
  if ("상태" %in% names(df)) df$상태 <- vapply(df$상태, status_chip, "")
  df
}
status_legend <- function() div(class = "st-legend", HTML(paste(
  vapply(c("확인 완료", "확인 필요", "임시 값"), status_chip, ""), collapse = " ")),
  span("임시 값은 출처 확인 전 자리표시 값입니다"))

# ---- 화면 부품 ------------------------------------------------------------
tip_icon <- function(text) tooltip(span(class = "tip-icon", tabindex = "0", bs_icon("info-circle", a11y = "sem", title = "설명")), text)
info_tip <- function(id) tooltip(span(class = "tip-icon", tabindex = "0", bs_icon("info-circle", a11y = "sem", title = "산출 근거")),
                                 "", id = id, placement = "bottom")
lab_tip <- function(text, tip) div(class = "lab", text, tip_icon(tip))
source_line <- function(...) div(class = "src-line", bs_icon("database"), span(...))

# 카드: 제목 + 오른쪽 보조 정보(단위 등). 상태 배지는 쓰지 않고, 자료 상태는 표 안의 점으로만 표시한다.
xcard <- function(title, ..., aside = NULL, full_screen = FALSE, class = NULL)
  card(full_screen = full_screen, class = class,
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

page_foot <- function() div(class = "page-foot",
  "LESE412 기말 프로젝트 · 자료: 낙농진흥회 낙농통계광장(원유생산통계·원유수취가격, 2013-2025, 2026-11-19 조회) · 기준연도 2013년 · 수식과 검증은 방법론 탭")

side_info_card <- function() div(class = "side-info",
  span(class = "si-icon", art("milk", 40)),
  div(div(class = "si-title", "원유 = 가공 전 우유"),
      p("이 앱의 '원유'는 가공(살균, 가공유·치즈 등) 이전의 낙농가 출하 우유를 의미합니다.")))

# ---- 사이드바: 시뮬레이션 설정 --------------------------------------------------
# 자주 바꾸는 입력(가격, 탄력성)은 열어 두고, 드물게 바꾸는 것(Sa, σ, 잉여 처리)은 '고급 설정'에 접어 둔다.
controls_ui <- function() tagList(
  div(class = "side-head",
      h2("시뮬레이션 설정"),
      p("값을 바꾸면 결과가 바로 다시 계산됩니다.")),
  div(class = "side-field",
      lab_tip("시나리오", "2013년 실제 도입 수준, 도입 전 수준, 가상 인상 시나리오 중에서 고르면 가격과 기간이 함께 바뀝니다. 직접 바꾸면 '직접 설정'이 됩니다."),
      selectInput("scenario", NULL, choices = setNames(SCENARIOS$id, SCENARIOS$name), selected = "base"),
      uiOutput("scenario_meta")),
  div(class = "side-section",
      lab_tip("원유가격 (연동제 가격 Ps)", "연동제로 정해지는 원유 1L 가격입니다. 도입 전 균형가격 P*(980.29원/L)보다 높으면 가격하한으로 작동합니다."),
      uiOutput("price_big"),
      div(class = "price-slider",
          sliderInput("policy_price", NULL, min = 980, max = 1300, value = BASE$policy_default,
                      step = 1, ticks = FALSE)),
      div(class = "slider-note", "980 = 도입 전 P* · 1,083 = 2013년 도입 후")),
  div(class = "side-section",
      lab_tip("탄력성", "단기(1년)는 낙농가가 생산을 거의 바꾸지 못하는 경우, 장기(5년+)는 착유우 수를 조정한 뒤입니다. 버튼은 기본값을 불러오고, 슬라이더로 직접 바꿀 수 있습니다."),
      div(class = "seg", radioButtons("run", NULL, inline = TRUE,
                                      choices = c("단기 기본값" = "SR", "장기 기본값" = "LR"))),
      uiOutput("preset_state"),
      sliderInput("eps_s", "원유 공급탄력성 ε (낙농가)", min = 0.05, max = 2, value = ELAST_PRESET$SR$eps_s,
                  step = 0.05, ticks = FALSE),
      sliderInput("eta_r", "흰 우유 소매 수요탄력성 η", min = -1.5, max = -0.05, value = ELAST_PRESET$SR$eta_r,
                  step = 0.05, ticks = FALSE),
      div(class = "derived", uiOutput("eta_farm_txt"))),
  accordion(
    open = FALSE, class = "side-acc",
    accordion_panel(
      "고급: 파생수요 계수 (Sa, σ)",
      p(class = "help", "원유 수요탄력성 ηa = Sa·η − Sb·σ (Muth). Sb = 1 − Sa"),
      sliderInput("Sa", "소매가 중 원유 몫 Sa", min = 0.2, max = 0.8, value = ELAST_DEFAULT$Sa, step = 0.01, ticks = FALSE),
      sliderInput("sigma", "원유·기타 투입 대체탄력성 σ", min = 0, max = 1, value = ELAST_DEFAULT$sigma, step = 0.05, ticks = FALSE)
    ),
    accordion_panel(
      "고급: 잉여 원유 처리",
      p(class = "help", "남는 원유를 누가 사서 얼마에 처분하는지 정합니다. 부담 주체는 분배만 바꾸고 사회 전체 변화는 같습니다."),
      div(class = "seg", radioButtons("payer", NULL, inline = TRUE, choices = PAYER_CHOICES,
                                      selected = SURPLUS_DEFAULT$payer)),
      sliderInput("Pv", "잉여 원유 처분가치 Pv (원/L)", min = 0, max = 900,
                  value = SURPLUS_DEFAULT$Pv, step = 10, ticks = FALSE),
      div(class = "slider-note", "0 = 회수 가치 없음 (강의 모형)")
    )
  ),
  div(class = "side-actions",
      actionButton("bookmark", "설정 링크 복사", icon = icon("link"), class = "btn-primary btn-main w-100"),
      actionLink("reset", tagList(bs_icon("arrow-counterclockwise"), "설정 초기화"), class = "reset-link"))
)

side_info_ui <- tagList(
  div(class = "side-head",
      h2("이 앱에 대해"),
      p("원유가격 연동제가 낙농가, 유가공업체, 가계에 미치는 영향을 계산하는 정책 분석 도구입니다.")),
  div(class = "side-note",
      bs_icon("sliders"),
      div("가격과 탄력성 설정은 ", tags$b("시뮬레이터"), "와 ", tags$b("후생 결과"), " 탭의 사이드바에 있습니다.",
          actionLink("to_sim", "시뮬레이터로 이동", class = "d-block mt-1"))),
  div(class = "side-block",
      div(class = "sb-title", "사용자"),
      p("탄력성과 원유가격을 바꿔 가며 정책 결과를 예측하는 농림축산식품부 낙농 정책 담당 분석가")),
  div(class = "side-block",
      div(class = "sb-title", "분석 질문"),
      p("연동제로 원유가격이 오르면 낙농가 소득 보전(ΔPS)에 비해 유가공업체·소비자의 손실과 잉여 원유 처리 비용은 단기와 장기에 각각 얼마이며, 저소득층과 고소득층 가계의 실질 부담은 어느 수준인가?")),
  div(class = "side-block",
      div(class = "sb-title", "분석 시장과 기준연도"),
      p("농가 출하 단계의 원유 시장(원/L, 천 톤), 2013년. 연동제가 처음 적용되어 도입 전(1-7월)과 도입 후(8-12월) 가격을 모두 관측할 수 있습니다.")),
  side_info_card()
)

kpi <- function(icon, title, out, tip, caption, cls)
  div(class = paste("kpi-card", cls),
      div(class = "kpi-head", span(class = "kpi-icon", art(icon, 40)),
          div(class = "kpi-title", uiOutput(paste0(out, "_title"), inline = TRUE), info_tip(tip))),
      div(class = "kpi-value", textOutput(out, inline = TRUE)),
      div(class = "kpi-cap", caption))

METRICS <- c("사회 전체 ΔW (억 원)" = "dW", "낙농가 ΔPS (억 원)" = "dPS", "유업체 ΔCS (억 원)" = "dCS",
             "잉여 원유 처리 −G (억 원)" = "G", "남는 원유 Qs − Qd (천 톤)" = "stock")
METRIC_CONCEPT <- c(dW = "dwl", dPS = "farm", dCS = "proc", G = "gov", stock = "gov")
metric_val <- function(r, m) switch(m, dW = r$dW, dPS = r$dPS, dCS = r$dCS, G = -r$G, stock = r$stock)
metric_fmt <- function(v, m) if (m == "stock") paste0(num(v), "천 톤") else eok(v)

# ---- 탭: 시뮬레이터 -------------------------------------------------------
sim_ui <- tagList(
  hero("시뮬레이터", uiOutput("hero_title", inline = TRUE), uiOutput("hero_sub", inline = TRUE),
       cond = uiOutput("hero_cond_sim", inline = TRUE)),
  layout_column_wrap(
    width = "200px", fill = FALSE, class = "kpi-row",
    kpi("cow", "낙농가", "vb_ps", "tt_ps", "생산자잉여 변화 ΔPS", "farm"),
    kpi("factory", "유업체", "vb_cs", "tt_cs", "구매자잉여 변화 ΔCS", "proc"),
    kpi("sack", "잉여 원유 처리", "vb_g", "tt_g", "처리 비용 G만큼 감소 (−G)", "gov"),
    kpi("scale", "사회 전체", "vb_dw", "tt_dw", "순후생 변화 ΔW = −사중손실", "dwl")
  ),
  div(class = "callout",
      div(class = "callout-head", bs_icon("lightbulb"), "해석"),
      div(class = "prose", uiOutput("story"))),
  layout_columns(
    col_widths = c(7, 5),
    xcard("수요·공급과 후생 면적", aside = "원/L · 천 톤", full_screen = TRUE,
          div(class = "card-lead", uiOutput("sd_title", inline = TRUE)),
          plotOutput("sd_plot", height = 400),
          source_line("기준선: 2013년 낙농진흥회 (P* 980.29원/L, Q* 2,093천 톤) · ΔCS = −A, ΔPS = A + B, G = B + D")),
    xcard("가격·탄력성이 바뀌면", full_screen = TRUE,
          div(class = "whatif-pick",
              selectInput("metric", NULL, choices = METRICS, selected = "dW", width = "100%")),
          plotOutput("whatif_plot", height = 360),
          p(class = "foot", "점이 현재 설정입니다. 위는 탄력성을, 아래는 가격을 현재 값에 고정했습니다. 두 그래프의 세로축은 같습니다."))
  ),
  layout_columns(
    col_widths = c(5, 7),
    xcard("원유 시장 결과", aside = "기준 = 2013년 도입 전",
          div(class = "tbl-wrap", tableOutput("market_tbl")),
          p(class = "foot", "괄호는 기준 대비 변화율입니다. 흰 우유 소매가는 전가율 Sa로 계산한 참고값입니다.")),
    xcard("사용 중인 탄력성",
          div(class = "tbl-wrap", tableOutput("elast_now_tbl")),
          status_legend())
  ),
  accordion(
    open = FALSE, class = "assume-box",
    accordion_panel("이 결과의 가정",
                    tags$ul(class = "assume", lapply(ASSUMPTIONS, tags$li)),
                    p(class = "foot", "수식, 탄력성 출처, 검증은 방법론 탭에 있습니다."))
  ),
  page_foot()
)

# ---- 탭: 후생 결과 ---------------------------------------------------------
welfare_ui <- tagList(
  hero("후생 결과", uiOutput("welfare_title", inline = TRUE),
       "낙농가 1원 이득을 만드는 데 다른 주체가 치르는 비용입니다.",
       cond = uiOutput("hero_cond_welfare", inline = TRUE)),
  layout_columns(
    col_widths = c(7, 5),
    xcard("이해관계자별 후생 변화", aside = "억 원", uiOutput("stake_rows")),
    xcard("주요 지표",
          uiOutput("metric_rows"),
          actionButton("to_method", "산출 근거는 방법론에서 확인", class = "btn-soft w-100"))
  ),
  layout_columns(
    col_widths = c(6, 6),
    xcard("단기와 장기 비교", aside = "억 원 · 현재 가격",
          plotOutput("srlr_plot", height = 230),
          uiOutput("reality_bar")),
    xcard("가계 파급 효과", aside = "월 추가 지출",
          uiOutput("hh_rows"))
  ),
  xcard("공급탄력성과 인상폭이 클수록 사회 전체 손실이 커집니다", aside = "사회 전체 ΔW, 억 원",
        uiOutput("sens_tbl"),
        p(class = "foot", "공급탄력성 낮음·기준·높음은 ε 0.2·0.5·1.0이고, 인상폭은 도입 전 P* 대비입니다. 수요탄력성과 잉여 원유 처리는 현재 설정값을 씁니다. 굵은 테두리는 현재 설정과 가장 가까운 칸이고, 색이 진할수록 손실이 큽니다.")),
  page_foot()
)

# ---- 탭: 정책·시장 -------------------------------------------------------
market_ui <- tagList(
  hero("정책·시장", HTML("원유가격 연동제는<br>생산비가 오르면 수요와 관계없이 가격을 올립니다"), art = HERO_ART),
  layout_columns(
    col_widths = c(6, 6),
    xcard("정책 개요",
          div(class = "policy-list", lapply(POLICY_INFO, function(x)
            div(class = "policy-item",
                span(class = "policy-icon", bs_icon(x$bs)),
                div(div(class = "policy-title", x$title), div(class = "policy-body", x$body))))),
          source_line(POLICY_SOURCE)),
    xcard("원유 시장 구조와 가치사슬",
          div(class = "chain", lapply(seq_len(nrow(VALUE_CHAIN)), function(i)
            div(class = paste("chain-step", VALUE_CHAIN$scope[i]),
                div(class = "chain-icon", art(VALUE_CHAIN$art[i], 112, 84, fallback = VALUE_CHAIN$li[i])),
                div(class = "chain-name", VALUE_CHAIN$step[i]),
                div(class = "chain-note", VALUE_CHAIN$note[i])))),
          div(class = "chain-bracket", span("분석 대상: 원유 거래 (낙농가 → 유가공업체)")),
          div(class = "tbl-wrap", tableOutput("structure_tbl")),
          status_legend())
  ),
  layout_columns(
    col_widths = c(6, 6),
    xcard("2013년 8월, 원유가격이 한 달 만에 96원/L 올랐습니다", aside = "원/L",
          plotOutput("m2013_plot", height = 300),
          source_line("낙농진흥회 원유수취가격(전국), 2013년 · 점선은 모형의 P*와 연동제 가격")),
    navset_card_underline(
      title = span(class = "ch-title", "가격은 올랐고 생산량은 줄었습니다 (2013-2025)"),
      nav_panel("그래프", plotOutput("trend_plot", height = 300)),
      nav_panel("표", div(class = "tbl-wrap", tableOutput("trend_tbl"))),
      footer = source_line("낙농진흥회 원유수취가격 연평균·원유생산량, 2013-2025")
    )
  ),
  xcard("2013년 기준선",
        div(class = "tbl-wrap", tableOutput("baseline_tbl")),
        div(class = "st-row", status_legend(), span(class = "foot m-0", "결론에는 '확인 완료' 값만 씁니다."))),
  page_foot()
)

# ---- 탭: 방법론 ------------------------------------------------------------
# 수식은 외부 수식 엔진(MathJax) 없이 HTML로 쓴다: 오프라인에서도 깨지지 않고 화면 글꼴과 같다.
eqn <- function(...) div(class = "eq-line", HTML(paste0(...)))
method_ui <- tagList(
  hero("방법론", "수식, 가정, 자료, 그리고 검증"),
  layout_columns(
    col_widths = c(7, 5),
    xcard("수식",
          div(class = "eq",
              h3("1. 선형 수요·공급 (2013년 기준점에서 보정)"),
              eqn("Q<sup>d</sup> = a − bP,&nbsp;&nbsp; Q<sup>s</sup> = c + dP"),
              eqn("b = |η<sub>a</sub>| · Q* / P*,&nbsp;&nbsp; d = ε · Q* / P*"),
              h3("2. 원유 수요탄력성 (유업체의 파생수요, Muth)"),
              eqn("η<sub>a</sub> = S<sub>a</sub> · η − S<sub>b</sub> · σ,&nbsp;&nbsp; S<sub>b</sub> = 1 − S<sub>a</sub>"),
              h3("3. 연동제 가격 = 가격하한"),
              eqn("P<sub>s</sub> &gt; P* &nbsp;⇒&nbsp; 잉여 = Q<sup>s</sup>(P<sub>s</sub>) − Q<sup>d</sup>(P<sub>s</sub>)"),
              h3("4. 후생 (수급 그래프의 면적 A, B, D)"),
              eqn("ΔCS = −A = −(P<sub>s</sub> − P*) · (Q* + Q<sup>d</sup>) / 2"),
              eqn("ΔPS = A + B = (P<sub>s</sub> − P*) · (Q* + Q<sup>s</sup>) / 2"),
              eqn("G = (P<sub>s</sub> − P<sub>v</sub>) · (Q<sup>s</sup> − Q<sup>d</sup>) = B + D"),
              eqn("ΔW = ΔCS + ΔPS − G = −D"),
              eqn("이전효율 = ΔPS / (G − ΔCS)"),
              h3("5. 가계 파급 (원유가 → 소매가 전가)"),
              eqn("%ΔP<sub>소매</sub> ≈ S<sub>a</sub> × %ΔP<sub>원유</sub>"))),
    xcard("가정", tags$ul(class = "assume", lapply(ASSUMPTIONS, tags$li)))
  ),
  layout_columns(
    col_widths = c(6, 6),
    xcard("모형 검증: 강의 L11 공식과 일치합니다",
          p(class = "card-lead", VALIDATION_CASE$label),
          div(class = "tbl-wrap", tableOutput("valid_tbl")),
          p(class = "foot", "L11 Supply Policy 6쪽: ΔCS = −(a+b), ΔPS = a+b+c, G = b+c+d+e, Net = −(b+d+e), 재판매 시 G = (Ps − v)(Qs − Qd)")),
    div(class = "stack",
        xcard("현재 설정의 독립 검산", aside = "억 원",
              p(class = "card-lead", "같은 ΔW를 면적 공식과 곡선 아래 적분, 두 방법으로 계산해 비교합니다."),
              div(class = "tbl-wrap", tableOutput("check_tbl"))),
        xcard("자료 출처",
              div(class = "tbl-wrap", tableOutput("source_tbl")),
              source_line("낙농진흥회 낙농통계광장: dairy.or.kr › 낙농통계광장 › 국내통계")))
  ),
  xcard("탄력성 표",
        div(class = "tbl-wrap", tableOutput("elast_tbl")),
        status_legend()),
  page_foot()
)

# ---- 전체 화면 (설정 링크 저장을 위해 함수 형태) ------------------------------------
ui <- function(request) {
  page_navbar(
    id = "nav",
    window_title = "원유가격 정책 시뮬레이터",
    title = tags$span(class = "brand",
                      span(class = "brand-mark", li("cow", 30), li("milk", 24)),
                      span(class = "brand-text",
                           span(class = "brand-name", "원유가격 정책 시뮬레이터"),
                           span(class = "brand-sub", "가공 전 우유의 가격과 낙농 정책"))),
    theme = theme_app,
    fillable = FALSE,
    header = tagList(
      tags$head(
        tags$link(rel = "stylesheet", href = "webfonts/pretendard/pretendardvariable-dynamic-subset.css"),
        tags$link(rel = "stylesheet", href = asset_src("style.css")),
        # 사이드바 여닫기 버튼(세 줄 아이콘)의 이름을 한국어로, 열림 상태에 맞게 바꾼다
        tags$script(HTML("
          document.addEventListener('DOMContentLoaded', function () {
            document.querySelectorAll('.bslib-sidebar-layout > .collapse-toggle').forEach(function (btn) {
              var label = function () {
                var t = btn.getAttribute('aria-expanded') === 'true' ? '설정 메뉴 닫기' : '설정 메뉴 열기';
                btn.setAttribute('title', t); btn.setAttribute('aria-label', t);
              };
              label();
              new MutationObserver(label).observe(btn, { attributes: true, attributeFilter: ['aria-expanded'] });
            });
          });"))),
      if (exists("useBusyIndicators", where = asNamespace("shiny"), inherits = FALSE))
        shiny::useBusyIndicators(spinners = TRUE, pulse = TRUE)
    ),
    sidebar = sidebar(
      width = 320, open = list(desktop = "open", mobile = "closed"),
      conditionalPanel("input.nav == 'sim' || input.nav == 'welfare'", controls_ui()),
      conditionalPanel("input.nav == 'market' || input.nav == 'method'", side_info_ui)
    ),
    nav_panel("시뮬레이터", value = "sim",     sim_ui),
    nav_panel("후생 결과",  value = "welfare", welfare_ui),
    nav_panel("정책·시장",  value = "market",  market_ui),
    nav_panel("방법론",     value = "method",  method_ui),
    nav_spacer(),
    nav_item(tags$span(class = "nav-note", "LESE412 · 2026 가을"))
  )
}

# ---- 그래프 ----------------------------------------------------------------------
# 글자 크기: 축 11pt, 주석 3.6mm(≈10pt), 패널 제목 12pt
theme_plot <- function() {
  theme_minimal(base_family = PLOT_FONT, base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "#EDF0EE", linewidth = 0.4),
          axis.title = element_text(color = COL$muted, size = 11),
          axis.text = element_text(color = COL$muted, size = 11),
          strip.text = element_text(color = COL$ink, size = 12, face = "bold", hjust = 0,
                                    margin = margin(2, 0, 6, 0)),
          legend.position = "bottom", legend.title = element_blank(),
          legend.text = element_text(color = COL$sub, size = 11),
          plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(8, 14, 8, 8))
}
TXT <- 3.6   # 주석 글자 (mm)

plot_sd <- function(r) {
  k <- r$curves
  xlim <- c(r$Q0 * 0.6, r$Q0 * 1.4)
  ylim <- c(r$P0 * 0.6, r$P0 * 1.4)
  qx <- seq(xlim[1], xlim[2], length.out = 200)
  dem <- data.frame(Q = qx, P = inv_demand(k, qx))
  sup <- data.frame(Q = qx, P = inv_supply(k, qx))
  areas <- r$areas
  areas$area <- factor(AREA_LAB[as.character(areas$area)], levels = AREA_LAB)
  # 곡선 이름은 범례 대신 선 옆에 직접 쓴다
  clampx <- function(q) min(max(q, xlim[1] + 0.02 * diff(xlim)), xlim[2] - 0.02 * diff(xlim))
  ys <- ylim[1] + 0.9 * diff(ylim); qs <- clampx(k$c + k$d * ys)
  yd <- ylim[1] + 0.1 * diff(ylim); qd <- clampx(k$a - k$b * yd)
  p <- ggplot() +
    geom_polygon(data = areas, aes(x, y, fill = area), color = NA) +
    geom_hline(yintercept = r$P0, linetype = "dotted", color = COL$muted, linewidth = 0.5) +
    geom_line(data = dem, aes(Q, P), color = CONCEPT$proc$ink, linewidth = 1.1) +
    geom_line(data = sup, aes(Q, P), color = CONCEPT$farm$ink, linewidth = 1.1) +
    # 이름이 오른쪽 끝에서 잘리지 않도록, 오른쪽 1/4 구간에서는 선의 왼쪽에 쓴다
    annotate("text", x = qs, y = inv_supply(k, qs), label = "공급 (낙농가)",
             hjust = if (qs > xlim[1] + 0.75 * diff(xlim)) 1.1 else -0.12, vjust = 0.5,
             family = PLOT_FONT, size = TXT, color = CONCEPT$farm$ink, fontface = "bold") +
    annotate("text", x = qd, y = inv_demand(k, qd), label = "수요 (유업체)",
             hjust = if (qd > xlim[1] + 0.75 * diff(xlim)) 1.1 else -0.12, vjust = 0.5,
             family = PLOT_FONT, size = TXT, color = CONCEPT$proc$ink, fontface = "bold") +
    annotate("label", x = xlim[1] + 0.01 * diff(xlim), y = r$P0, label = paste0("도입 전 P* ", num(r$P0, 2)),
             hjust = 0, vjust = 1.25, size = TXT, family = PLOT_FONT, linewidth = 0, fill = "white",
             color = COL$muted)
  if (r$binding) {
    p <- p +
      geom_hline(yintercept = r$Ps, linetype = "dashed", color = COL$brand800, linewidth = 0.6) +
      annotate("label", x = xlim[1] + 0.01 * diff(xlim), y = r$Ps, label = paste0("연동제 가격 Ps ", num(r$Ps)),
               hjust = 0, vjust = -0.3, size = TXT, family = PLOT_FONT, linewidth = 0, fill = "white",
               color = COL$brand800, fontface = "bold") +
      geom_point(aes(x = c(r$Qd, r$Qs), y = c(r$Ps, r$Ps)), size = 2.6, color = COL$ink)
    # 면적 이름(A, B, D)을 면적 안에 쓴다 (너무 좁으면 생략)
    if (r$stock > 0.02 * r$Q0) {
      lab <- data.frame(
        x = c(xlim[1] + 0.06 * diff(xlim), (r$Qd + r$Qs + r$Q0) / 3, (r$Qd + r$Q0) / 2),
        y = c((r$P0 + r$Ps) / 2, (2 * r$Ps + r$P0) / 3, (ylim[1] + r$P0) / 2),
        t = c("A", "B", "D"))
      p <- p + geom_text(data = lab, aes(x, y, label = t), family = PLOT_FONT, fontface = "bold",
                         size = 4, color = COL$ink)
    }
  }
  if (nrow(areas) > 0) p <- p + scale_fill_manual(values = AREA_COL, drop = FALSE) +
    guides(fill = guide_legend(nrow = 1))
  p + geom_point(aes(x = r$Q0, y = r$P0), size = 2.6, color = COL$ink) +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    scale_x_continuous(labels = function(x) num(x)) +
    scale_y_continuous(labels = function(x) num(x)) +
    labs(x = "원유 수량 (천 톤)", y = "원유 가격 (원/L)") +
    theme_plot() +
    theme(panel.grid.major.x = element_line(color = "#EDF0EE", linewidth = 0.4))
}

plot_whatif <- function(df, cur, m) {
  cc <- CONCEPT[[METRIC_CONCEPT[[m]]]]
  rng <- range(c(df$y, 0)); pad <- diff(rng) * 0.18 + 1e-9
  cur$lab <- metric_fmt(cur$y, m)
  cur$hj <- ifelse(cur$xpos > 0.6, 1.1, -0.1)
  rising <- m %in% c("dPS", "stock")
  cur$vj <- ifelse(xor(cur$xpos > 0.6, rising), 1.5, -0.5)
  ggplot(df, aes(x, y)) +
    geom_hline(yintercept = 0, color = COL$line_strong, linewidth = 0.5) +
    geom_line(color = cc$line, linewidth = 1.2) +
    geom_point(data = cur, shape = 21, size = 3.4, stroke = 1.4, color = cc$ink, fill = "white") +
    # 값 글자는 선이 지나가지 않는 쪽에 둔다 (오른쪽에 쓸 때: 내려가는 선이면 위, 올라가는 선이면 아래)
    geom_label(data = cur, aes(label = lab, hjust = hj, vjust = vj), family = PLOT_FONT, fontface = "bold",
               size = TXT, color = cc$ink, fill = "white", linewidth = 0, label.padding = unit(2, "pt")) +
    facet_wrap(~panel, ncol = 1, scales = "free_x") +
    scale_y_continuous(labels = function(x) num(x), limits = c(rng[1] - pad, rng[2] + pad)) +
    scale_x_continuous(labels = function(x) num(x, if (max(abs(x), na.rm = TRUE) < 10) 1 else 0)) +
    labs(x = NULL, y = NULL) +
    theme_plot() +
    theme(panel.spacing.y = unit(18, "pt"))
}

# ---- 서버 ----------------------------------------------------------------------
server <- function(input, output, session) {

  # 설정 링크: 버튼을 누르면 현재 입력값을 URL로 저장하고 복사 창을 띄운다
  setBookmarkExclude(c("bookmark", "reset", "to_method", "to_sim", "tt_ps", "tt_cs", "tt_g", "tt_dw"))
  observeEvent(input$bookmark, session$doBookmark())
  onBookmarked(function(url) {
    updateQueryString(url)
    showModal(modalDialog(
      title = "이 설정 링크",
      p("아래 링크를 열면 지금의 시나리오, 원유가격, 탄력성, 잉여 원유 처리 설정, 탭이 그대로 재현됩니다. 발표 Q&A용으로 미리 저장해 두세요."),
      tags$textarea(id = "bm_url", class = "form-control", rows = 3, readonly = NA, `aria-label` = "설정 링크", url),
      footer = tagList(
        tags$button(type = "button", class = "btn btn-primary",
                    onclick = "navigator.clipboard.writeText(document.getElementById('bm_url').value); this.textContent='복사됨';",
                    "링크 복사"),
        modalButton("닫기")),
      easyClose = TRUE))
  })
  observeEvent(input$to_method, nav_select("nav", "method"))
  observeEvent(input$to_sim, nav_select("nav", "sim"))

  # 시나리오 적용
  observeEvent(input$scenario, {
    s <- SCENARIOS[SCENARIOS$id == input$scenario, ]
    if (!is.na(s$price)) updateSliderInput(session, "policy_price", value = s$price)
    if (!is.na(s$run))   updateRadioButtons(session, "run", selected = s$run)
  }, ignoreInit = TRUE)
  # 가격을 직접 바꿔 시나리오와 달라지면 '직접 설정'으로 표시한다 (화면의 상태와 실제 값이 어긋나지 않게)
  observeEvent(input$policy_price, {
    s <- SCENARIOS[SCENARIOS$id == input$scenario, ]
    if (!is.na(s$price) && input$policy_price != s$price) updateSelectInput(session, "scenario", selected = "custom")
  }, ignoreInit = TRUE)

  output$scenario_meta <- renderUI({
    s <- SCENARIOS[SCENARIOS$id == input$scenario, ]
    if (s$id == "custom") return(NULL)
    div(class = "scenario-meta", span("출처: ", s$source), HTML(status_chip(s$상태)))
  })

  # 단기·장기 기본값 적용
  observeEvent(input$run, {
    p <- ELAST_PRESET[[input$run]]
    updateSliderInput(session, "eta_r", value = p$eta_r)
    updateSliderInput(session, "eps_s", value = p$eps_s)
  }, ignoreInit = TRUE)

  # 슬라이더를 직접 움직였는지: 버튼의 '기본값'과 실제 값이 다르면 알린다
  preset_now <- reactive({
    p <- ELAST_PRESET[[input$run]]
    if (isTRUE(all.equal(c(input$eta_r, input$eps_s), c(p$eta_r, p$eps_s)))) input$run else "custom"
  })
  run_label <- reactive(switch(preset_now(), SR = "단기 기본값", LR = "장기 기본값", "직접 조정"))
  output$preset_state <- renderUI({
    if (preset_now() != "custom") return(NULL)
    p <- ELAST_PRESET[[input$run]]
    div(class = "preset-note", bs_icon("pencil"),
        paste0("직접 조정 중 · ", if (input$run == "SR") "단기" else "장기", " 기본값은 ε ", num(p$eps_s, 2),
               ", η ", num(p$eta_r, 2)))
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
  why <- reactive(explain(res()))
  chk <- reactive(identity_check(res()))
  payer_name <- reactive(if (res()$payer == "gov") "정부" else "유업체")

  output$price_big <- renderUI({
    r <- res()
    div(class = "price-readout",
        div(class = "price-big", span(class = "price-num", num(input$policy_price)), span(class = "price-unit", "원/L")),
        div(class = "price-delta",
            if (r$binding) paste0("도입 전 P* 대비 ", sgn(r$dp), "원/L (", sgn(r$farm_pct, 1), "%)")
            else "도입 전 P* 이하 · 가격하한 효력 없음"))
  })

  output$eta_farm_txt <- renderUI({
    HTML(paste0("→ 원유 수요탄력성 η<sub>a</sub> = <b>", num(inp()$eta_farm, 2), "</b>",
                "<span class='derived-sub'>Sa·η − Sb·σ로 계산 (Sa ", num(input$Sa, 2), ", σ ", num(input$sigma, 2), ")</span>"))
  })

  # ---- 머리글: 입력 조건 + 결과 한 문장 ----
  cond_ui <- function() {
    r <- res()
    item <- function(k, v) span(class = "cond", span(class = "cond-k", k), span(class = "cond-v", v))
    tagList(span(class = "cond-lead", "이 조건이면"),
            item("원유가격", paste0(num(r$Ps), "원/L")),
            item("공급탄력성 ε", num(input$eps_s, 2)),
            item("원유 수요탄력성 ηa", num(inp()$eta_farm, 2)),
            item("기간", run_label()))
  }
  output$hero_cond_sim     <- renderUI(cond_ui())
  output$hero_cond_welfare <- renderUI(cond_ui())

  output$hero_title <- renderUI({
    r <- res()
    if (!r$binding) return(HTML("가격하한 효력이 없어<br>시장은 도입 전 균형에서 거래됩니다"))
    HTML(paste0("원유 <span class='hl-gov'>", num(r$stock), "천 톤</span>이 남고,<br>",
                "사회 전체 후생은 <span class='hl-dwl'>", num(-r$dW), "억 원</span> 줄어듭니다"))
  })
  output$hero_sub <- renderUI({
    r <- res()
    if (!r$binding) "연동제 가격이 도입 전 균형가격 P*(980.29원/L) 이하입니다. 가격을 올려 보세요."
    else paste0("도입 전 P* 대비 ", sgn(r$dp), "원/L(", sgn(r$farm_pct, 1), "%) 인상 · 2013년 연간 원유 생산량 기준 · ",
                if (input$run == "SR") "1년 안의 반응" else "착유우 수 조정 후의 반응")
  })

  # ---- 지표 카드 (모두 '변화량'으로, 부호 포함) ----
  output$vb_ps  <- renderText(eok(res()$dPS))
  output$vb_cs  <- renderText(eok(res()$dCS))
  output$vb_g   <- renderText(eok(-res()$G))
  output$vb_dw  <- renderText(eok(res()$dW))
  output$vb_ps_title <- renderUI("낙농가")
  output$vb_cs_title <- renderUI("유업체")
  output$vb_g_title  <- renderUI(paste0("잉여 원유 처리 (", payer_name(), ")"))
  output$vb_dw_title <- renderUI("사회 전체")

  observe({
    w <- why()
    update_tooltip("tt_ps", w$dPS)
    update_tooltip("tt_cs", w$dCS)
    update_tooltip("tt_g",  paste0(w$G, " → 후생 변화는 −G"))
    update_tooltip("tt_dw", paste0(w$dW, " = −사중손실. ", w$DWL))
  })

  output$story <- renderUI({
    r <- res(); h <- r$household; c <- chk()
    if (!r$binding) return(p("연동제 가격이 도입 전 균형가격 P* 이하이므로 가격하한이 효력을 갖지 않습니다. 시장은 P*·Q*에서 거래되고 후생 변화는 없습니다."))
    tagList(
      p(HTML(paste0(
        "유업체는 원유를 리터당 <b>", sgn(r$dp), "원</b> 더 주고 사며 구매량을 <b>", num(r$Qd),
        "천 톤</b>으로 줄입니다. 낙농가는 <b>", num(r$Qs), "천 톤</b>을 생산해 <b>", num(r$stock),
        "천 톤</b>이 남고, ", payer_name(), "가 이를 사서 분유로 전환합니다. ",
        "부담이 소매가로 전가되면 흰 우유는 리터당 약 <b>", won(r$retail_won), "</b> 오르고, ",
        h$group[1], " 가구는 한 달에 약 <b>", won(h$monthly_won[1]), "</b>을 더 냅니다."))),
      p(class = "check-line", if (c$ok) tagList(bs_icon("check-circle"), " 검산: ΔCS + ΔPS − G = ΔW, 곡선 아래 적분값과 일치")
                              else tagList(bs_icon("x-circle"), paste0(" 검산 불일치: 차이 ", num(c$gap, 1), "억 원"))))
  })

  output$sd_title <- renderUI({
    r <- res()
    if (!r$binding) "연동제 가격이 P* 이하이므로 가격하한 효력이 없습니다"
    else paste0("원유 ", num(r$Ps), "원/L에서 낙농가 생산량 ", num(r$Qs), "천 톤이 유업체 구매량 ",
                num(r$Qd), "천 톤을 넘어 ", num(r$stock), "천 톤이 남습니다")
  })
  output$sd_plot <- renderPlot(plot_sd(res()), res = 96, alt = reactive({
    r <- res()
    paste0("수요·공급 그래프. 원유가격 ", num(r$Ps), "원/L에서 공급량 ", num(r$Qs), "천 톤, 수요량 ", num(r$Qd),
           "천 톤, 잉여 ", num(r$stock), "천 톤.")
  }))

  # ---- 가격·탄력성이 바뀌면 ----
  whatif <- reactive({
    m <- input$metric
    pg <- seq(980, 1300, by = 10)
    eg <- seq(0.05, 2, by = 0.05)
    lab_p <- "원유가격 (원/L)에 따라 · 탄력성은 현재 값"
    lab_e <- "원유 공급탄력성 ε에 따라 · 가격은 현재 값"
    df <- rbind(
      data.frame(panel = lab_p, x = pg,
                 y = vapply(pg, function(p) metric_val(simulate(make_inp(p, input$eta_r, input$eps_s)), m), 0)),
      data.frame(panel = lab_e, x = eg,
                 y = vapply(eg, function(e) metric_val(simulate(make_inp(input$policy_price, input$eta_r, e)), m), 0)))
    df$panel <- factor(df$panel, levels = c(lab_e, lab_p))
    v <- metric_val(res(), m)
    cur <- data.frame(panel = factor(c(lab_p, lab_e), levels = c(lab_e, lab_p)),
                      x = c(input$policy_price, input$eps_s), y = c(v, v),
                      xpos = c((input$policy_price - 980) / 320, (input$eps_s - 0.05) / 1.95))
    list(df = df, cur = cur, m = m)
  })
  output$whatif_plot <- renderPlot({
    w <- whatif(); plot_whatif(w$df, w$cur, w$m)
  }, res = 96, alt = reactive({
    w <- whatif()
    paste0(names(METRICS)[METRICS == w$m], "이 원유 공급탄력성과 원유가격에 따라 어떻게 바뀌는지 보여 주는 선 그래프. 현재 값 ",
           metric_fmt(w$cur$y[1], w$m), ".")
  }))

  output$market_tbl <- renderTable({
    r <- res()
    base  <- c(r$P0, r$Q0, r$Q0, 0, BASE$Pr0)
    after <- c(r$Ps, r$Qs, r$Qd, r$stock, BASE$Pr0 + r$retail_won)
    chg   <- after - base
    rate  <- ifelse(base > 0, paste0(" (", sgn(chg / base * 100, 1), "%)"), "")
    data.frame(
      항목 = c("원유가격 (원/L)", "낙농가 생산량 (천 톤)", "유업체 구매량 (천 톤)",
               "남는 원유 (천 톤)", "참고: 흰 우유 소매가 (원/L)"),
      기준 = num(base), 현재 = num(after), 변화 = paste0(sgn(chg), rate),
      check.names = FALSE)
  }, hover = TRUE, width = "100%", align = "lrrr")

  output$elast_now_tbl <- renderTable({
    rng <- ELAST_TBL$`타당 범위`
    data.frame(
      탄력성 = c("ε 원유 공급 (낙농가)", "η 흰 우유 소매 수요", "Sa 소매가 중 원유 몫", "σ 원유·기타 투입 대체",
                 "ηa 원유 수요 (계산)"),
      현재 = c(num(input$eps_s, 2), num(input$eta_r, 2), num(input$Sa, 2), num(input$sigma, 2),
               paste0("<b>", num(inp()$eta_farm, 2), "</b>")),
      `단기 기본` = c(num(ELAST_PRESET$SR$eps_s, 2), num(ELAST_PRESET$SR$eta_r, 2), num(ELAST_DEFAULT$Sa, 2),
                    num(ELAST_DEFAULT$sigma, 2), "계산"),
      `장기 기본` = c(num(ELAST_PRESET$LR$eps_s, 2), num(ELAST_PRESET$LR$eta_r, 2), num(ELAST_DEFAULT$Sa, 2),
                    num(ELAST_DEFAULT$sigma, 2), "계산"),
      `타당 범위` = c(rng[2], rng[3], rng[4], rng[5], "-"),
      상태 = vapply(ELAST_TBL$상태[c(2, 3, 4, 5, 1)], status_chip, ""),
      check.names = FALSE)
  }, sanitize.text.function = identity, hover = TRUE, width = "100%", align = "lrrrrl")

  # ---- 후생 결과 ----
  output$welfare_title <- renderUI({
    r <- res()
    if (!r$binding || is.na(r$TE) || r$TE <= 0) return("가격하한 효력이 없어 후생 변화가 없습니다")
    who <- if (r$payer == "gov") "유업체와 정부가" else "유업체가"
    HTML(paste0("낙농가가 1원을 얻을 때<br>", who, " <span class='hl-dwl'>", num(1 / r$TE, 2), "원</span>을 냅니다"))
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
          div(div(class = "srow-name", name, tip_icon(tip)), div(class = "srow-sub", sub)),
          div(class = "srow-track", div(class = "axis"), div(class = paste("dbar", cls), style = pos)),
          div(class = paste("srow-val", cls), eok(v)))
    }
    div(class = "srows",
      row("cow", "낙농가", "생산자잉여 변화 ΔPS", r$dPS, "farm", w$dPS),
      row("factory", "유업체", "구매자잉여 변화 ΔCS", r$dCS, "proc", w$dCS),
      row("sack", paste0("잉여 원유 처리 (", payer_name(), ")"), "처리 비용 G만큼 감소 (−G)", -r$G, "gov", w$G),
      row("scale", "사회 전체", "순후생 변화 ΔW = 위 세 줄의 합", r$dW, "dwl", w$dW, total = TRUE)
    )
  })

  output$metric_rows <- renderUI({
    r <- res(); w <- why()
    row <- function(icon, name, sub, val, tip = NULL)
      div(class = "mrow",
          span(class = "mrow-icon", art(icon, 36)),
          div(div(class = "mrow-name", name, if (!is.null(tip)) tip_icon(tip)), div(class = "mrow-sub", sub)),
          div(class = "mrow-val", val))
    div(class = "mbox",
        row("cycle", "이전효율", "낙농가가 얻은 몫 ÷ 다른 주체가 낸 몫 (1에 가까울수록 효율적)",
            if (is.na(r$TE)) "-" else num(r$TE, 2), w$TE),
        row("sack", "남는 원유", "낙농가 생산량 − 유업체 구매량", paste0(num(r$stock), "천 톤")),
        row("milk", "흰 우유 소매가", paste0("원유가 변화의 Sa(", num(input$Sa, 2), ")만큼 전가"),
            paste0(sgn(r$retail_won), "원/L")))
  })

  srlr <- reactive(list(SR = run_preset("SR"), LR = run_preset("LR")))

  output$srlr_plot <- renderPlot({
    s <- srlr()
    lv <- c("낙농가", "유업체", "잉여 처리", "사회 전체")
    df <- do.call(rbind, lapply(names(s), function(n) data.frame(
      기간 = ifelse(n == "SR", "단기 기본값", "장기 기본값"),
      항목 = factor(lv, levels = lv),
      값 = c(s[[n]]$dPS, s[[n]]$dCS, -s[[n]]$G, s[[n]]$dW))))
    df$항목 <- factor(df$항목, levels = rev(lv))
    df$기간 <- factor(df$기간, levels = c("장기 기본값", "단기 기본값"))
    lim <- max(abs(df$값)) * 1.35
    # 가로 막대: 값 글자가 막대 끝에 겹치지 않고 들어간다
    ggplot(df, aes(값, 항목, fill = 기간)) +
      geom_col(position = position_dodge(0.8), width = 0.74) +
      geom_vline(xintercept = 0, color = COL$ink, linewidth = 0.5) +
      geom_text(aes(label = num(값), hjust = ifelse(값 >= 0, -0.15, 1.15)), position = position_dodge(0.8),
                family = PLOT_FONT, size = 3.4, color = COL$sub) +
      scale_fill_manual(values = c("단기 기본값" = COL$period_sr, "장기 기본값" = COL$period_lr),
                        breaks = c("단기 기본값", "장기 기본값")) +
      scale_x_continuous(labels = function(x) num(x), limits = c(-lim, lim)) +
      labs(x = NULL, y = NULL) + theme_plot() +
      theme(legend.position = "top", legend.justification = "left",
            panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "#EDF0EE", linewidth = 0.4),
            axis.text.y = element_text(color = COL$ink, size = 12))
  }, res = 96, alt = "단기와 장기 기본 탄력성에서 낙농가, 유업체, 잉여 처리, 사회 전체의 후생 변화를 비교한 막대그래프.")

  output$reality_bar <- renderUI({
    q13 <- TREND$qty[TREND$year == 2013]; q14 <- TREND$qty[TREND$year == 2014]
    sr <- run_preset("SR", BASE$P_policy)$Qs; lr <- run_preset("LR", BASE$P_policy)$Qs
    f <- function(q) paste0(sgn((q / q13 - 1) * 100, 1), "%")
    div(class = "note-bar", HTML(paste0("실제와 비교: 2014년 원유 생산량 <b>", f(q14), "</b> · 모형 단기 ", f(sr),
                                        " · 장기 ", f(lr), " (2013년 실제 연동제 가격 기준)")))
  })

  output$hh_rows <- renderUI({
    r <- res(); h <- r$household
    burden <- h$income_pct
    mx <- max(burden, 1e-12)
    lab <- c("저소득 가구", "고소득 가구")
    rows <- lapply(seq_len(nrow(h)), function(i)
      div(class = "hrow",
          span(class = "hrow-icon", art("house", 36)),
          div(div(class = "hrow-name", lab[i]), div(class = "hrow-sub", h$group[i])),
          div(div(class = "hrow-val", paste0("월 ", won(h$monthly_won[i]))),
              div(class = "hrow-sub text-end", paste0("연 ", won(h$monthly_won[i] * 12)))),
          div(div(class = "hbar", div(style = sprintf("width:%.0f%%", burden[i] / mx * 100))),
              div(class = "hbar-cap", paste0("소비지출 대비 ", pct(burden[i], 3))))))
    ratio <- if (burden[2] > 0) burden[1] / burden[2] else NA
    tagList(div(class = "hrows", rows),
            div(class = "note-bar", HTML(paste0(
              "소비지출 대비 부담률은 저소득층이 고소득층의 ",
              if (is.na(ratio)) "-" else paste0("<b>", num(ratio, 1), "배</b>"), "입니다.")),
              div(class = "note-sub", paste0("소매가 ", sgn(r$retail_pct, 1), "% 전가 가정 · 전가율로만 계산해 단기와 장기가 같음 · 가계자료는 임시 값"))))
  })

  output$sens_tbl <- renderUI({
    eps_grid <- c(0.2, 0.5, 1.0)
    dp_grid  <- c(50, 100, 150)
    m <- outer(eps_grid, dp_grid, Vectorize(function(e, d)
      simulate(make_inp(BASE$P0 + d, input$eta_r, e))$dW))
    cur_i <- which.min(abs(eps_grid - input$eps_s))
    cur_j <- which.min(abs(dp_grid - (input$policy_price - BASE$P0)))
    mx <- max(abs(m), 1e-9)
    rlab <- c("낮음", "기준", "높음")
    clab <- c("낮은 인상폭", "기준 인상폭", "높은 인상폭")
    tags$table(class = "sens2",
      tags$caption(class = "visually-hidden", "공급탄력성과 인상폭에 따른 사회 전체 후생 변화"),
      tags$thead(tags$tr(tags$th(scope = "col", "공급탄력성 \\ 인상폭"),
                         lapply(seq_along(dp_grid), function(j)
                           tags$th(scope = "col", clab[j], span(class = "th-sub", paste0("+", dp_grid[j], "원/L")))))),
      tags$tbody(lapply(seq_along(eps_grid), function(i)
        tags$tr(tags$th(scope = "row", class = "rh", rlab[i], span(class = "th-sub", paste0("ε ", num(eps_grid[i], 1)))),
                lapply(seq_along(dp_grid), function(j)
                  tags$td(class = if (i == cur_i && j == cur_j) "cur",
                          style = sprintf("background: rgba(194,149,47,%.2f)", 0.08 + 0.42 * abs(m[i, j]) / mx),
                          eok(m[i, j])))))))
  })

  # ---- 정책·시장 ----
  output$structure_tbl <- renderTable(chipify(STRUCTURE_TBL), sanitize.text.function = identity,
                                      width = "100%", hover = TRUE)
  output$baseline_tbl  <- renderTable(chipify(BASELINE_TBL), sanitize.text.function = identity,
                                      width = "100%", hover = TRUE, align = "lrllll")

  output$m2013_plot <- renderPlot({
    m <- MONTHLY_2013
    m$기간 <- ifelse(m$month <= 7, "도입 전 (1-7월)", "도입 후 (8-12월)")
    ggplot(m, aes(month, price)) +
      annotate("rect", xmin = 7.5, xmax = 12.6, ymin = -Inf, ymax = Inf, fill = COL$brand100, alpha = 0.8) +
      annotate("segment", x = 0.6, xend = 7.4, y = BASE$P0, yend = BASE$P0, linetype = "dashed", color = COL$muted) +
      annotate("segment", x = 7.6, xend = 12.4, y = BASE$P_policy, yend = BASE$P_policy,
               linetype = "dashed", color = COL$brand800) +
      geom_line(color = COL$ink, linewidth = 0.8) +
      geom_point(aes(color = 기간), size = 2.8) +
      annotate("text", x = 4, y = BASE$P0 - 18, label = paste0("P* ", num(BASE$P0, 2), "원/L (1-7월 평균)"),
               family = PLOT_FONT, size = TXT, color = COL$muted) +
      annotate("text", x = 10, y = 1012, label = paste0("연동제 가격 ", num(BASE$P_policy, 2), "원/L\n(8-12월 평균)"),
               family = PLOT_FONT, size = TXT, color = COL$brand800, lineheight = 1.1) +
      annotate("text", x = 7.4, y = 1080, label = "8월 연동제 적용\n전월 대비 +96.45원/L",
               family = PLOT_FONT, size = TXT, color = COL$ink, hjust = 1, lineheight = 1.1) +
      scale_color_manual(values = c("도입 전 (1-7월)" = COL$period_sr, "도입 후 (8-12월)" = COL$brand800)) +
      scale_x_continuous(NULL, breaks = 1:12, labels = paste0(1:12, "월")) +
      scale_y_continuous(NULL, limits = c(940, 1120), labels = function(x) num(x)) +
      theme_plot()
  }, res = 96, alt = "2013년 월별 원유수취가격 선 그래프. 7월 965.10원에서 8월 1,061.55원으로 올랐다.")

  # 이중 축 대신 위(가격)·아래(생산량) 두 패널로 나눈다. 막대는 0에서 시작한다.
  output$trend_plot <- renderPlot({
    lp <- "원유수취가격 연평균 (원/L)"; lq <- "원유 생산량 (천 톤)"
    df <- rbind(data.frame(panel = lp, year = TREND$year, y = TREND$price),
                data.frame(panel = lq, year = TREND$year, y = TREND$qty))
    df$panel <- factor(df$panel, levels = c(lp, lq))
    ev <- TREND_EVENTS
    ev_lab <- data.frame(panel = factor(lp, levels = c(lp, lq)), year = ev$year, label = ev$label,
                         y = max(TREND$price) * 1.035, hj = c(-0.04, 1.04))
    ggplot(df, aes(year, y)) +
      geom_col(data = df[df$panel == lq, ], fill = CONCEPT$farm$light, width = 0.65) +
      geom_line(data = df[df$panel == lp, ], color = COL$ink, linewidth = 1) +
      geom_point(data = df[df$panel == lp, ], color = COL$ink, size = 1.8) +
      geom_vline(data = ev, aes(xintercept = year), linetype = "dashed", color = COL$muted) +
      geom_text(data = ev_lab, aes(x = year, y = y, label = label, hjust = hj),
                family = PLOT_FONT, size = 3.3, color = COL$sub) +
      facet_wrap(~panel, ncol = 1, scales = "free_y") +
      scale_x_continuous(NULL, breaks = TREND$year, labels = paste0("'", substr(TREND$year, 3, 4))) +
      scale_y_continuous(NULL, labels = function(x) num(x), expand = expansion(mult = c(0.02, 0.12))) +
      theme_plot() + theme(panel.spacing.y = unit(14, "pt"))
  }, res = 96, alt = "2013-2025년 원유수취가격 연평균(위)과 원유 생산량(아래). 가격은 1,022.92원에서 1,249.81원으로, 생산량은 2,093천 톤에서 1,951천 톤으로 바뀌었다.")
  output$trend_tbl <- renderTable({
    ev <- setNames(TREND_EVENTS$label, TREND_EVENTS$year)
    data.frame(연도 = as.character(TREND$year), `원유수취가격 (원/L)` = num(TREND$price, 2),
               `생산량 (천 톤)` = num(TREND$qty, 1),
               `정책 변경` = ifelse(as.character(TREND$year) %in% names(ev), ev[as.character(TREND$year)], ""),
               check.names = FALSE)
  }, hover = TRUE, width = "100%", align = "lrrl")

  # ---- 방법론 ----
  output$valid_tbl <- renderTable({
    v <- run_validation(VALIDATION_CASE)
    v$기대값 <- num(v$기대값); v$앱 <- num(v$앱)
    v
  }, width = "100%", hover = TRUE, align = "lrrc")
  output$check_tbl <- renderTable({
    r <- res(); a <- integrate_dW(r)
    data.frame(방법 = c("면적 공식 ΔCS + ΔPS − G", "곡선 아래 적분", "차이"),
               ΔW = c(num(r$dW, 1), num(a, 1), num(a - r$dW, 2)), check.names = FALSE)
  }, width = "100%", hover = TRUE, align = "lr")
  output$source_tbl <- renderTable(chipify(SOURCES_TBL), sanitize.text.function = identity,
                                   width = "100%", hover = TRUE)
  output$elast_tbl <- renderTable(chipify(ELAST_TBL), sanitize.text.function = identity,
                                  hover = TRUE, width = "100%", align = "lrrrlll")
}

shinyApp(ui, server, enableBookmarking = "url")
