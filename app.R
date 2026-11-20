# =====================================================================
# 원유가격 연동제 정책 시뮬레이터 (LESE412 기말 프로젝트)
# 실행: R에서 shiny::runApp("이 폴더 경로")
# 계산은 R/model.R 의 simulate() 하나만 사용한다.
# 기준연도 2013년, 자료는 낙농진흥회 낙농통계광장.
# 탭 구성은 과제 명세의 네 구성 요소 순서(① 정책과 시장 ② 시뮬레이터 ③ 후생 ④ 방법론)를 따른다.
# =====================================================================
library(shiny)
library(bslib)
library(bsicons)
library(ggplot2)

source("R/data.R",  encoding = "UTF-8")
source("R/model.R", encoding = "UTF-8")

# ---- 색과 글꼴 ------------------------------------------------------
COL <- list(primary = "#1F4E8C", gain = "#1E8A5A", loss = "#C0392B", cost = "#5B6B82",
            dwl = "#C98A00", supply = "#0F7B6C", ink = "#1B2433", muted = "#5B6676")
AREA_COL <- c(A = COL$loss, B = COL$gain, D = COL$dwl)
PLOT_FONT <- if (.Platform$OS.type == "windows") "Malgun Gothic" else "sans"
# 웹 배포판(shinylive/webR)에는 한글 폰트가 없으므로 동봉한 나눔고딕을 폰트 폴더에 복사해 쓴다
# (폰트 폴더가 없는 shinylive.io 링크 실행 시에는 jsDelivr에서 받아온다)
if (R.version$os == "emscripten") {
  font_dir <- "/home/web_user/fonts"
  dir.create(font_dir, recursive = TRUE, showWarnings = FALSE)
  if (dir.exists("fonts")) {
    file.copy(list.files("fonts", pattern = "\\.ttf$", full.names = TRUE), font_dir, overwrite = TRUE)
  } else {
    for (f in c("NanumGothic-Regular.ttf", "NanumGothic-Bold.ttf")) try(download.file(
      paste0("https://cdn.jsdelivr.net/gh/google/fonts@main/ofl/nanumgothic/", f),
      file.path(font_dir, f), mode = "wb", quiet = TRUE), silent = TRUE)
  }
  PLOT_FONT <- "NanumGothic"
}

theme_app <- bs_theme(
  version = 5, primary = COL$primary, success = COL$gain, danger = COL$loss,
  bg = "#FFFFFF", fg = COL$ink,
  base_font = font_collection("Pretendard", "Malgun Gothic", "Apple SD Gothic Neo", "sans-serif"),
  "border-radius" = "0.75rem", "font-size-base" = "0.95rem"
)

# ---- 아이콘 ---------------------------------------------------------
fa <- function(name, ...) shiny::icon(name, ...)
icon_chip <- function(name, cls = "") span(class = paste("icon-chip", cls), fa(name))

# ---- 숫자 표시 도우미 ---------------------------------------------------
num  <- function(x, d = 0) formatC(round(x, d), big.mark = ",", format = "f", digits = d)
eok  <- function(x) paste0(ifelse(round(x) > 0, "+", ""), num(x), "억 원")
won  <- function(x) paste0(num(x), "원")
pct  <- function(x, d = 1) paste0(num(x, d), "%")
sgn  <- function(x, d = 0) paste0(ifelse(round(x, d) > 0, "+", ""), num(x, d))
status_chip <- function(s) {
  cls <- switch(s, "확인 완료" = "ok", "확인 필요" = "check", "가상" = "virtual", "-" = "none", "fake")
  as.character(tags$span(class = paste("chip", cls), s))
}
chipify <- function(df) {
  if ("상태" %in% names(df)) df$상태 <- vapply(df$상태, status_chip, "")
  df
}
source_line <- function(...) div(class = "src-line", fa("database"), span(...))
info_tip <- function(id) tooltip(span(class = "tip-icon", bs_icon("info-circle")), "", id = id,
                                 placement = "bottom")
section_head <- function(icon, title) div(class = "side-sec", fa(icon), title)
page_head <- function(step, eyebrow, title, sub)
  div(class = "page-head",
      div(class = "eyebrow", span(class = "step", step), eyebrow),
      h2(title), p(sub))

# ---- 자료 상태 집계 (상단 배지) -------------------------------------------
DATA_STATUS <- local({
  s <- c(BASELINE_TBL$상태, ELAST_TBL$상태, STRUCTURE_TBL$상태, SOURCES_TBL$상태,
         vapply(POLICY_INFO, `[[`, "", "status"))
  list(ok = sum(s == "확인 완료"), check = sum(s == "확인 필요"), fake = sum(s == "가짜 값"),
       total = length(s))
})
page_foot <- function() div(class = "page-foot", fa("database"),
  "자료: 낙농진흥회 낙농통계광장 (원유생산통계·원유수취가격, 2013-2025, 2026-11-19 조회) · 기준연도 2013 · ",
  "후생 계산: 강의 가격지지 모형, 교과서형 예제·적분 검산 통과 · ",
  sprintf("자료 상태: 확인 완료 %d / 확인 필요 %d / 가짜 값 %d", DATA_STATUS$ok, DATA_STATUS$check, DATA_STATUS$fake))

# ---- 사이드바 조작부 ------------------------------------------------------
controls_ui <- function() tagList(
  section_head("layer-group", "시나리오"),
  selectInput("scenario", NULL, choices = setNames(SCENARIOS$id, SCENARIOS$name), selected = "base"),
  uiOutput("scenario_meta"),

  section_head("sliders", "정책 수준"),
  sliderInput("policy_price", "연동제 원유가격 Ps (원/L)", min = 980, max = 1300,
              value = BASE$policy_default, step = 1, ticks = FALSE),
  div(class = "slider-note", "도입 전 P* 980.29원 · 도입 후 실제 1,082.60원"),

  section_head("hourglass-half", "낙농가 조정 기간"),
  div(class = "seg", radioButtons("run", NULL, inline = TRUE,
                                  choices = c("단기 (1년)" = "SR", "장기 (5년+)" = "LR"))),

  section_head("boxes-stacked", "잉여 원유 처리"),
  div(class = "seg seg-sm", radioButtons("payer", NULL, inline = TRUE, choices = PAYER_CHOICES,
                                         selected = SURPLUS_DEFAULT$payer)),
  sliderInput("Pv", "잉여 원유 처분가치 Pv (원/L)", min = 0, max = 900,
              value = SURPLUS_DEFAULT$Pv, step = 10, ticks = FALSE),
  div(class = "slider-note", "0 = 매입 후 회수 가치 없음 (강의 모형). 분유로 팔아 회수하는 금액을 원유 1L 기준으로 넣으세요."),

  accordion(
    open = FALSE, class = "mt-3",
    accordion_panel(
      "탄력성 직접 조정", icon = fa("gear"),
      div(class = "derived", uiOutput("eta_farm_txt")),
      p(class = "foot mb-2", "원유 수요탄력성은 소매 탄력성, Sa, σ로 계산합니다 (Muth)."),
      sliderInput("eta_r", "흰 우유 소매 수요탄력성 η", min = -1.5, max = -0.05, value = ELAST_PRESET$SR$eta_r, step = 0.05, ticks = FALSE),
      sliderInput("eps_s", "원유 공급탄력성 ε", min = 0.05, max = 2, value = ELAST_PRESET$SR$eps_s, step = 0.05, ticks = FALSE),
      sliderInput("Sa", "소매가 중 원유 몫 Sa", min = 0.2, max = 0.8, value = ELAST_DEFAULT$Sa, step = 0.01, ticks = FALSE),
      sliderInput("sigma", "원유·기타 투입 대체탄력성 σ", min = 0, max = 1, value = ELAST_DEFAULT$sigma, step = 0.05, ticks = FALSE)
    )
  ),
  div(class = "btn-stack mt-3",
      actionButton("bookmark", "이 설정 링크 복사", icon = fa("link"),
                   class = "btn-primary btn-sm w-100"),
      actionButton("reset", "기준값으로 되돌리기", icon = fa("rotate-left"),
                   class = "btn-outline-secondary btn-sm w-100"))
)

side_info_ui <- tagList(
  section_head("user-tie", "사용자"),
  p(class = "side-note", "농림축산식품부 낙농 정책 담당 분석가"),
  section_head("circle-question", "분석 질문"),
  p(class = "side-note",
    "연동제로 원유 수매가가 오르면 낙농가 소득 보전(ΔPS)에 비해 유가공업체·소비자의 손실(ΔCS)과 잉여 원유 처리 비용은 얼마이며, 저소득층과 고소득층 가계의 실질 부담은 단기와 장기에 어느 수준인가?"),
  section_head("bullseye", "분석 시장"),
  p(class = "side-note", "농가 출하 단계의 원유 시장 (원/L, 천 톤). 가계 부담은 소매가 전가를 가정한 파급 효과로 봅니다."),
  section_head("calendar", "기준연도"),
  p(class = "side-note", "2013년. 연동제가 처음 적용된 해로, 도입 전(1-7월)과 도입 후(8-12월) 가격을 모두 관측할 수 있습니다.")
)

kpi_box <- function(title, out, tip, icon, cls, caption) {
  value_box(
    title = tagList(title, info_tip(tip)), value = textOutput(out),
    p(class = "vb-cap", caption),
    showcase = icon_chip(icon, cls), showcase_layout = "left center",
    class = paste("kpi", cls)
  )
}

# ---- 탭 ①: 정책과 시장 ------------------------------------------------------
market_ui <- tagList(
  page_head("1", "정책과 시장", "원유가격 연동제는 어떻게 작동하나",
            "정책의 목표·작동 방식·규모, 원유 시장의 구조, 2013년 기준선과 실제 시장의 움직임을 정리합니다."),
  layout_columns(
    col_widths = c(6, 6),
    card(card_header(fa("file-lines"), "정책 개요"),
         div(class = "policy-list", lapply(POLICY_INFO, function(x)
           div(class = "policy-item",
               icon_chip(x$icon, "soft"),
               div(div(class = "policy-title", x$title, HTML(status_chip(x$status))),
                   div(class = "policy-body", x$body))))),
         source_line(POLICY_SOURCE)),
    card(card_header(fa("diagram-project"), "원유 시장 구조와 가치사슬"),
         div(class = "chain", lapply(seq_len(nrow(VALUE_CHAIN)), function(i)
           div(class = paste("chain-step", VALUE_CHAIN$scope[i]),
               div(class = "chain-icon", fa(VALUE_CHAIN$icon[i])),
               div(class = "chain-name", VALUE_CHAIN$step[i]),
               div(class = "chain-note", VALUE_CHAIN$note[i])))),
         div(class = "chain-bracket", span("분석 대상: 원유 거래 (낙농가 → 유가공업체)")),
         p(class = "chain-legend",
           span(class = "dot policy"), "정책 작용 단계",
           span(class = "dot market"), "원유 구매자",
           span(class = "dot spill"), "파급 효과만 반영"),
         tableOutput("structure_tbl"))
  ),
  layout_columns(
    col_widths = c(6, 6),
    card(card_header(fa("calendar-days"), "2013년 월별 원유수취가격: 연동제 도입 전후"),
         plotOutput("m2013_plot", height = 290),
         source_line("자료: 낙농진흥회 원유수취가격 (전국, 원/L), 2013년 · 확인 완료 · 점선은 모형의 P*와 정책가격")),
    navset_card_underline(
      title = tagList(fa("chart-line"), "원유가격과 생산량 추이"),
      nav_panel("그래프", plotOutput("trend_plot", height = 290)),
      nav_panel("표", div(class = "tbl-nowrap", tableOutput("trend_tbl"))),
      footer = source_line("자료: 낙농진흥회 원유수취가격 연평균(원/L)·원유생산량(천 톤), 2013-2025 · 확인 완료")
    )
  ),
  layout_columns(
    col_widths = c(7, 5),
    card(card_header(fa("table"), "2013년 기준선"),
         div(class = "tbl-nowrap", tableOutput("baseline_tbl")),
         source_line("상태가 '확인 완료'인 값만 결론에 사용한다.")),
    card(card_header(fa("magnifying-glass-chart"), "실제 시장과 대조: 2014년 원유 생산량"),
         uiOutput("reality"),
         source_line("실제값: 낙농진흥회 원유생산통계 2013·2014 · 모형값: 2013년 8-12월 실제 연동제 가격(1,082.60원)에서 단기·장기 프리셋"))
  ),
  page_foot()
)

# ---- 탭 ②: 정책 시뮬레이터 -------------------------------------------------------
sim_ui <- tagList(
  div(class = "question-bar",
      div(class = "eyebrow", span(class = "step", "2"), "정책 시뮬레이터 · 분석 질문"),
      uiOutput("question")),
  uiOutput("stat_strip"),
  layout_column_wrap(
    width = "210px", fill = FALSE, class = "kpi-row",
    kpi_box("낙농가가 얻는 몫 ΔPS", "vb_ps", "tt_ps", "cow", "gain", "원유 판매자 잉여 = A + B"),
    kpi_box("유업체가 잃는 몫 ΔCS", "vb_cs", "tt_cs", "industry", "loss", "원유 구매자 잉여 = −A"),
    kpi_box("잉여 원유 처리 비용 G", "vb_g", "tt_g", "boxes-stacked", "cost", "매입액 − 처분가치 = B + D"),
    kpi_box("사회 전체 손실 DWL", "vb_dwl", "tt_dwl", "droplet-slash", "dwl", "누구에게도 가지 않는 몫 = D")
  ),
  div(class = "story",
      div(class = "story-head", fa("lightbulb"), "한눈에 보기"),
      uiOutput("story")),
  layout_columns(
    col_widths = c(8, 4),
    card(full_screen = TRUE,
         card_header(uiOutput("sd_title", inline = TRUE)),
         plotOutput("sd_plot", height = 430),
         card_footer(uiOutput("sd_source"))),
    card(card_header(fa("shapes"), "그래프 면적으로 읽기"),
         uiOutput("area_legend"))
  ),
  layout_columns(
    col_widths = c(6, 6),
    card(card_header(fa("clipboard-list"), "원유 시장 결과: 기준 → 정책 후 → 변화"),
         div(class = "tbl-nowrap", tableOutput("market_tbl")),
         source_line("기준 = 2013년 도입 전 P*·생산량. 변화 = 정책 후 − 기준 (괄호는 변화율)")),
    card(card_header(fa("sliders"), "사용한 탄력성"),
         div(class = "tbl-nowrap", tableOutput("elast_tbl")),
         uiOutput("elast_now"))
  ),
  accordion(
    open = FALSE, class = "assume-box",
    accordion_panel("이 결과의 가정", icon = fa("list-check"),
                    tags$ul(class = "assume", lapply(ASSUMPTIONS, tags$li)),
                    p(class = "foot", "수식과 출처는 ④ 방법론 탭에 있습니다."))
  ),
  page_foot()
)

# ---- 탭 ③: 후생 분석 ---------------------------------------------------------
welfare_ui <- tagList(
  page_head("3", "후생 분석", "누가 얻고, 누가 잃고, 누가 내는가",
            "후생 변화(ΔCS, ΔPS, G, ΔW, DWL), 비용 부담 주체, 소득계층별 가계 부담, 단기·장기 비교, 민감도 분석을 보여줍니다."),
  layout_columns(
    col_widths = c(7, 5),
    card(card_header(fa("chart-column"), "후생 변화의 흐름: 낙농가 이득에서 순후생까지",
                     uiOutput("identity_badge", inline = TRUE)),
         plotOutput("waterfall_plot", height = 300),
         source_line("단위: 억 원 (2013년 원유 1년 물량 기준). 막대는 ΔPS에서 시작해 ΔCS와 처리 비용을 빼 순후생 ΔW에 도달합니다.")),
    card(card_header(fa("calculator"), "후생 변화 요약"),
         uiOutput("welfare_tbl"),
         p(class = "foot", "ⓘ에 마우스를 올리면 산출 근거가 보입니다."))
  ),
  layout_columns(
    col_widths = c(6, 6),
    card(card_header(fa("people-group"), "이해관계자별 결과"), uiOutput("who_txt")),
    card(card_header(fa("house-chimney"), "파급 효과: 소득계층별 가계 부담"),
         tableOutput("hh_tbl"),
         uiOutput("hh_note"),
         source_line("자료: 가계동향조사 (원/월, 2013) · 상태: 가짜 값 (확인 필요)"))
  ),
  layout_columns(
    col_widths = c(5, 7),
    navset_card_underline(
      title = tagList(fa("right-left"), "단기 vs 장기"),
      nav_panel("그래프", plotOutput("srlr_plot", height = 260)),
      nav_panel("표", tableOutput("srlr_tbl")),
      footer = source_line("단기·장기 프리셋 탄력성 적용. 원유가격, Sa, σ, 잉여 처리 설정은 현재값.")
    ),
    card(card_header(fa("table-cells"), "민감도 분석: 공급탄력성 × 인상폭",
                     div(class = "ms-auto hdr-select",
                         selectInput("sens_metric", NULL, width = "170px",
                                     choices = c("순후생 ΔW" = "dW", "낙농가 ΔPS" = "dPS",
                                                 "유업체 ΔCS" = "dCS", "처리 비용 G" = "G",
                                                 "이전효율" = "TE")))),
         uiOutput("sens_tbl"),
         p(class = "foot", "인상폭은 도입 전 P* 대비. 수요탄력성·Sa·σ·잉여 처리는 현재 설정값. 굵은 테두리는 현재 설정과 가장 가까운 칸."))
  ),
  page_foot()
)

# ---- 탭 ④: 방법론 ------------------------------------------------------------
method_ui <- tagList(
  page_head("4", "방법론", "수식, 가정, 자료, 검증",
            "모형의 방정식과 가정, 자료 출처, 예제 검증 결과를 정리합니다."),
  withMathJax(),
  layout_columns(
    col_widths = c(7, 5),
    card(card_header(fa("square-root-variable"), "수식"),
         div(class = "eq",
             h6("1. 선형 수요·공급 (2013년 기준점에서 보정)"),
             p("$$Q^d = a - bP,\\quad Q^s = c + dP,\\quad b = |\\eta_a|\\,\\frac{Q^*}{P^*},\\quad d = \\varepsilon\\,\\frac{Q^*}{P^*}$$"),
             h6("2. 원유 수요탄력성 (유업체의 파생수요, Muth)"),
             p("$$\\eta_a = S_a\\,\\eta - S_b\\,\\sigma,\\quad S_b = 1 - S_a$$"),
             h6("3. 연동제 = 가격하한 (wedge)"),
             p("$$P_s > P^* \\Rightarrow Q^d(P_s) < Q^s(P_s),\\quad \\text{잉여} = Q^s(P_s) - Q^d(P_s)$$"),
             h6("4. 후생 면적 (그래프의 A, B, D)"),
             p("$$A = (P_s - P^*)\\frac{Q^* + Q^d}{2},\\quad B = (P_s - P^*)\\frac{Q^s - Q^d}{2}$$"),
             p("$$D = (Q^s - Q^d)\\left[\\frac{P_s + P^*}{2} - P_v\\right]$$"),
             p("$$\\Delta CS = -A,\\quad \\Delta PS = A + B,\\quad G = (P_s - P_v)(Q^s - Q^d) = B + D$$"),
             p("$$\\Delta W = \\Delta CS + \\Delta PS - G = -D,\\quad \\text{이전효율} = \\frac{\\Delta PS}{G - \\Delta CS}$$"),
             h6("5. 가계 파급 (원유가 → 소매가 전가)"),
             p("$$\\%\\Delta P_{retail} \\approx S_a \\times \\%\\Delta P_{farm}$$"),
             p("$$\\text{가계 부담} \\approx \\text{월 소비지출} \\times \\text{우유 지출 비중} \\times \\%\\Delta P_{retail}$$"),
             h6("6. 단위"),
             p(class = "foot", "원/L × 천 톤(≈ 백만 L) = 백만 원, ÷ 100 = 억 원")),
         p(class = "foot", "모든 수식은 강의 Supply Policy 슬라이드와 대조해 확인한다.")),
    card(card_header(fa("list-check"), "가정"),
         tags$ul(class = "assume", lapply(ASSUMPTIONS, tags$li)))
  ),
  layout_columns(
    col_widths = c(6, 6),
    card(card_header(fa("clipboard-check"), "모형 검증"),
         p(class = "valid-label", VALIDATION_CASE$label, HTML(status_chip("확인 필요"))),
         tableOutput("valid_tbl"),
         uiOutput("valid_live"),
         p(class = "foot", "기대값은 손으로 계산한 값, 앱 값은 simulate()를 같은 입력으로 실행한 결과입니다. ",
           "강의 슬라이드 예제를 받으면 R/data.R의 VALIDATION_CASE를 그 수치로 바꿔 다시 확인합니다.")),
    card(card_header(fa("link"), "자료 출처"), tableOutput("source_tbl"),
         source_line("낙농진흥회 낙농통계광장: dairy.or.kr › 낙농통계광장 › 국내통계 › 원유생산통계"))
  ),
  page_foot()
)

# ---- 전체 화면 (설정 링크 저장을 위해 함수 형태) ------------------------------------
ui <- function(request) {
  page_navbar(
    id = "nav", selected = "sim",
    window_title = "원유가격 시뮬레이터",
    title = tags$span(class = "brand",
                      span(class = "brand-mark", fa("cow")),
                      span(class = "brand-text",
                           span(class = "brand-name", "원유가격 연동제 시뮬레이터"),
                           span(class = "brand-sub", "2013 기준 · 낙농진흥회 통계")),
                      tags$span(class = "badge-status",
                                title = "기준선·탄력성·시장구조·출처 표의 상태 집계",
                                fa("circle-check"),
                                sprintf("자료 확인 %d/%d", DATA_STATUS$ok, DATA_STATUS$total))),
    theme = theme_app,
    fillable = FALSE,
    header = tags$head(
      tags$link(rel = "stylesheet",
                href = "https://cdn.jsdelivr.net/gh/orioncactus/pretendard@v1.3.9/dist/web/static/pretendard.min.css"),
      tags$link(rel = "stylesheet", href = "style.css")
    ),
    sidebar = sidebar(
      width = 300, open = list(desktop = "open", mobile = "closed"),
      conditionalPanel("input.nav == 'sim' || input.nav == 'welfare'", controls_ui()),
      conditionalPanel("input.nav == 'market' || input.nav == 'method'", side_info_ui)
    ),
    nav_panel(tagList(span(class = "tab-num", "1"), "정책과 시장"), value = "market", market_ui),
    nav_panel(tagList(span(class = "tab-num", "2"), "정책 시뮬레이터"), value = "sim", sim_ui),
    nav_panel(tagList(span(class = "tab-num", "3"), "후생 분석"), value = "welfare", welfare_ui),
    nav_panel(tagList(span(class = "tab-num", "4"), "방법론"), value = "method", method_ui),
    nav_spacer(),
    nav_item(tags$span(class = "nav-note", "LESE412 · 2026 가을"))
  )
}

# ---- 그래프 함수 ----------------------------------------------------------------
theme_plot <- function() {
  theme_minimal(base_family = PLOT_FONT, base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "#EDF0F4"),
          axis.title = element_text(color = COL$muted, size = 10.5),
          axis.text = element_text(color = COL$muted),
          legend.position = "bottom", legend.title = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(8, 12, 8, 8))
}

plot_sd <- function(r) {
  k <- r$curves
  xr <- c(r$Q0 * 0.8, r$Q0 * 1.2)
  ylo <- max(0, min(r$Pv, r$P0 * 0.7) - r$P0 * 0.04)
  yr <- c(ylo, max(r$Ps, r$P0) * 1.12)
  qx <- seq(xr[1], xr[2], length.out = 200)
  lines <- rbind(
    data.frame(curve = "원유 수요 (유업체)", Q = qx, P = inv_demand(k, qx)),
    data.frame(curve = "원유 공급 (낙농가)", Q = qx, P = inv_supply(k, qx))
  )
  g <- ggplot()
  if (nrow(r$areas) > 0) {
    lab <- data.frame(
      area = c("A", "B", "D"),
      x = c(xr[1] + (r$Qd - xr[1]) * 0.45, (r$Qd + r$Qs + r$Q0) / 3, (r$Qd + r$Qs) / 2),
      y = c((r$P0 + r$Ps) / 2, (2 * r$Ps + r$P0) / 3, (max(r$Pv, ylo) + r$P0) / 2))
    g <- g +
      geom_polygon(data = r$areas, aes(x, y, fill = area), alpha = 0.30, color = NA) +
      annotate("rect", xmin = r$Qd, xmax = r$Qs, ymin = max(r$Pv, ylo), ymax = r$Ps,
               fill = NA, color = COL$cost, linetype = "dashed", linewidth = 0.5) +
      geom_label(data = lab, aes(x, y, label = area), color = AREA_COL[lab$area], family = PLOT_FONT,
                 fontface = "bold", size = 4.2, fill = "white", linewidth = 0.3,
                 label.padding = unit(0.18, "lines"))
  }
  g +
    geom_line(data = lines, aes(Q, P, color = curve), linewidth = 1.1) +
    geom_hline(yintercept = r$P0, linetype = "dotted", color = COL$muted) +
    { if (r$binding) geom_hline(yintercept = r$Ps, linetype = "dashed", color = COL$primary) } +
    { if (r$Pv > ylo) geom_hline(yintercept = r$Pv, linetype = "dotted", color = COL$dwl) } +
    geom_point(aes(x = c(r$Qd, r$Qs, r$Q0), y = c(r$Ps, r$Ps, r$P0)), size = 2.6, color = COL$ink) +
    annotate("label", x = xr[2], y = r$Ps, label = paste0("연동제 가격 Ps ", num(r$Ps)),
             hjust = 1, vjust = -0.35, size = 3.4, family = PLOT_FONT, linewidth = 0, fill = "white",
             color = COL$primary) +
    annotate("label", x = xr[2], y = r$P0, label = paste0("도입 전 P* ", num(r$P0)),
             hjust = 1, vjust = 1.35, size = 3.4, family = PLOT_FONT, linewidth = 0, fill = "white",
             color = COL$muted) +
    { if (r$binding) annotate("text", x = c(r$Qd, r$Qs), y = ylo + (yr[2] - ylo) * 0.03,
                              label = c("Qd", "Qs"), family = PLOT_FONT, size = 3.4,
                              color = COL$ink, fontface = "bold") } +
    scale_fill_manual(values = AREA_COL,
                      labels = c(A = "A 유업체 → 낙농가", B = "B 처리비용 → 낙농가", D = "D 사중손실"),
                      drop = FALSE) +
    scale_color_manual(values = c("원유 수요 (유업체)" = COL$primary, "원유 공급 (낙농가)" = COL$supply)) +
    guides(color = guide_legend(order = 1, nrow = 1), fill = guide_legend(order = 2, nrow = 1)) +
    coord_cartesian(xlim = xr, ylim = yr, expand = FALSE) +
    labs(x = "원유 수량 (천 톤)", y = "원유 가격 (원/L)") +
    theme_plot() +
    theme(legend.box = "vertical", legend.spacing.y = unit(2, "pt"),
          panel.grid.major.x = element_line(color = "#EDF0F4"))
}

plot_waterfall <- function(r) {
  steps <- data.frame(
    lab = factor(c("낙농가 ΔPS", "유업체 ΔCS", "처리 비용 −G", "순후생 ΔW"),
                 levels = c("낙농가 ΔPS", "유업체 ΔCS", "처리 비용 −G", "순후생 ΔW")),
    val = c(r$dPS, r$dCS, -r$G, r$dW),
    kind = c("gain", "loss", "cost", "total"))
  ends <- cumsum(c(r$dPS, r$dCS, -r$G))
  steps$ymax <- c(r$dPS, ends[1], ends[2], 0)
  steps$ymin <- c(0, ends[2], ends[3], r$dW)
  steps$i <- seq_len(4)
  steps$txt <- vapply(steps$val, eok, "")
  ggplot(steps) +
    geom_rect(aes(xmin = i - 0.36, xmax = i + 0.36, ymin = pmin(ymin, ymax), ymax = pmax(ymin, ymax),
                  fill = kind)) +
    geom_segment(data = steps[1:3, ], aes(x = i + 0.36, xend = i + 0.64, y = c(ends[1], ends[2], ends[3]),
                                          yend = c(ends[1], ends[2], ends[3])),
                 color = COL$muted, linetype = "dotted") +
    geom_hline(yintercept = 0, color = COL$muted) +
    geom_text(aes(x = i, y = pmax(ymin, ymax), label = txt), vjust = -0.5, family = PLOT_FONT,
              size = 3.6, fontface = "bold", color = COL$ink) +
    scale_fill_manual(values = c(gain = COL$gain, loss = COL$loss, cost = COL$cost, total = COL$dwl),
                      guide = "none") +
    scale_x_continuous(NULL, breaks = 1:4, labels = levels(steps$lab)) +
    scale_y_continuous("억 원", labels = function(x) num(x), expand = expansion(mult = c(0.08, 0.14))) +
    theme_plot()
}

# ---- 서버 ----------------------------------------------------------------------
server <- function(input, output, session) {

  # 설정 링크
  setBookmarkExclude(c("bookmark", "reset", "tt_ps", "tt_cs", "tt_g", "tt_dwl"))
  observeEvent(input$bookmark, session$doBookmark())
  onBookmarked(function(url) {
    updateQueryString(url)
    showModal(modalDialog(
      title = "이 설정 링크",
      p("아래 링크를 열면 지금의 시나리오, 원유가격, 탄력성, 잉여 처리 설정, 탭이 그대로 재현됩니다. 발표 Q&A용으로 미리 저장해 두세요."),
      tags$textarea(id = "bm_url", class = "form-control", rows = 3, readonly = NA, url),
      footer = tagList(
        tags$button(type = "button", class = "btn btn-primary",
                    onclick = "navigator.clipboard.writeText(document.getElementById('bm_url').value); this.textContent='복사됨';",
                    "링크 복사"),
        modalButton("닫기")),
      easyClose = TRUE))
  })

  # 시나리오 적용
  observeEvent(input$scenario, {
    s <- SCENARIOS[SCENARIOS$id == input$scenario, ]
    if (!is.na(s$price)) updateSliderInput(session, "policy_price", value = s$price)
    if (!is.na(s$run))   updateRadioButtons(session, "run", selected = s$run)
  }, ignoreInit = TRUE)

  output$scenario_meta <- renderUI({
    s <- SCENARIOS[SCENARIOS$id == input$scenario, ]
    if (s$id == "custom") return(NULL)
    div(class = "scenario-meta", "출처: ", s$source, " ", HTML(status_chip(s$상태)))
  })

  # 단기·장기 프리셋 적용
  observeEvent(input$run, {
    p <- ELAST_PRESET[[input$run]]
    updateSliderInput(session, "eta_r", value = p$eta_r)
    updateSliderInput(session, "eps_s", value = p$eps_s)
  }, ignoreInit = TRUE)

  observeEvent(input$reset, {
    updateSelectInput(session, "scenario", selected = "base")
    updateSliderInput(session, "policy_price", value = BASE$policy_default)
    updateRadioButtons(session, "run", selected = "SR")
    updateRadioButtons(session, "payer", selected = SURPLUS_DEFAULT$payer)
    updateSliderInput(session, "Pv", value = SURPLUS_DEFAULT$Pv)
    updateSliderInput(session, "eta_r", value = ELAST_PRESET$SR$eta_r)
    updateSliderInput(session, "eps_s", value = ELAST_PRESET$SR$eps_s)
    updateSliderInput(session, "Sa", value = ELAST_DEFAULT$Sa)
    updateSliderInput(session, "sigma", value = ELAST_DEFAULT$sigma)
  })

  make_inp <- function(policy_price, eta_r, eps_s, Sa = input$Sa, sigma = input$sigma,
                       Pv = input$Pv, payer = input$payer) {
    list(policy_price = policy_price, P0 = BASE$P0, Q0 = BASE$Q0,
         eta_farm = derived_eta(eta_r, Sa, sigma), eps_s = eps_s, Sa = Sa,
         Pv = Pv, payer = payer, Pr0 = BASE$Pr0, household = HOUSEHOLD)
  }
  run_preset <- function(run, price = input$policy_price) {
    p <- ELAST_PRESET[[run]]
    simulate(make_inp(price, p$eta_r, p$eps_s))
  }

  inp <- reactive(make_inp(input$policy_price, input$eta_r, input$eps_s))
  res <- reactive(simulate(inp()))
  why <- reactive(explain(res()))
  chk <- reactive(identity_check(res()))

  output$eta_farm_txt <- renderUI({
    HTML(paste0("원유 수요탄력성 η<sub>a</sub> = ", num(input$Sa, 2), " × (", num(input$eta_r, 2), ") − ",
                num(1 - input$Sa, 2), " × ", num(input$sigma, 2), " = <b>", num(inp()$eta_farm, 2), "</b>"))
  })

  # ---- ② 시뮬레이터 ----
  output$question <- renderUI({
    r <- res()
    div(class = "q-text",
      HTML(paste0("연동제로 원유가격을 리터당 <span class='hl'>", num(r$Ps), "원</span>으로 정하면",
                  "<span class='q-sub'>(", if (!r$binding) "도입 전 수준 · 하한 효력 없음"
                  else paste0("도입 전 대비 +", num(r$dp), "원, +", num(r$farm_pct, 1), "%"), ")</span>, ",
                  if (input$run == "SR") "1년 안에" else "낙농가가 생산을 조정한 뒤",
                  " 누가 얼마나 얻고 잃을까?")))
  })

  output$stat_strip <- renderUI({
    r <- res()
    stat <- function(label, value, delta, cls = "") div(class = paste("stat", cls),
      div(class = "stat-label", label), div(class = "stat-value", value),
      div(class = "stat-delta", delta))
    payer_txt <- if (r$payer == "gov") "정부 부담" else "유업체 부담"
    div(class = "stat-strip",
      stat("낙농가 수취가격", won(r$Ps), paste0(sgn(r$dp), "원 (", sgn(r$farm_pct, 1), "%)")),
      stat("유업체 구매가격", won(r$Pd),
           if (r$payer == "proc") paste0("잉여 포함 실질 ", won(r$P_proc_eff)) else "= 연동제 가격"),
      stat("원유 생산량 Qs", paste0(num(r$Qs), "천 톤"), paste0(sgn(r$Qs - r$Q0), " (", sgn((r$Qs / r$Q0 - 1) * 100, 1), "%)")),
      stat("유업체 구매량 Qd", paste0(num(r$Qd), "천 톤"), paste0(sgn(r$Qd - r$Q0), " (", sgn((r$Qd / r$Q0 - 1) * 100, 1), "%)")),
      stat("잉여 원유 (분유 전환·재고)", paste0(num(r$stock, 1), "천 톤"),
           paste0("생산량의 ", num(r$stock / max(r$Qs, 1e-9) * 100, 1), "%"), "warn"),
      stat(paste0("처리 비용 (", payer_txt, ")"), paste0(num(r$G), "억 원"),
           paste0("매입 ", num(r$G_gross), " − 처분 ", num(r$salvage)), "cost"),
      stat("흰 우유 소매가 (참고)", won(BASE$Pr0 + r$retail_won), paste0(sgn(r$retail_won), "원 (", sgn(r$retail_pct, 1), "%)"))
    )
  })

  output$vb_ps  <- renderText(eok(res()$dPS))
  output$vb_cs  <- renderText(eok(res()$dCS))
  output$vb_g   <- renderText(eok(-res()$G))
  output$vb_dwl <- renderText(paste0(num(res()$DWL), "억 원"))

  observe({
    w <- why()
    update_tooltip("tt_ps",  w$dPS)
    update_tooltip("tt_cs",  w$dCS)
    update_tooltip("tt_g",   w$G)
    update_tooltip("tt_dwl", w$DWL)
  })

  check_badge <- function(c) {
    if (c$ok) span(class = "id-badge ok", fa("circle-check"), "면적 적분 검산 통과")
    else span(class = "id-badge bad", fa("circle-xmark"), paste0("검산 불일치: 차이 ", num(c$gap, 1), "억 원"))
  }

  output$story <- renderUI({
    r <- res(); h <- r$household
    if (!r$binding) return(HTML("연동제 가격이 도입 전 균형가격 P* 이하이므로 가격하한이 효력을 갖지 않습니다. 시장은 P*·Q*에서 거래되고 후생 변화는 0입니다."))
    tagList(
      HTML(paste0(
        "유업체는 원유를 리터당 <b>", won(r$dp), "</b> 더 주고 사며 구매량은 <b>", num(r$Qd), "천 톤</b>으로 줄고, ",
        "낙농가 생산은 <b>", num(r$Qs), "천 톤</b>으로 늘어 <b>", num(r$stock, 1), "천 톤</b>이 남습니다. ",
        "낙농가가 1원을 얻을 때 다른 쪽은 <b>", if (is.na(r$TE)) "-" else num(1 / r$TE, 2), "원</b>을 냅니다. ",
        "소매가로 전가되면 흰 우유가 리터당 약 <b>", won(r$retail_won), "</b> 오르고, ",
        h$group[1], " 가구는 한 달에 약 <b>", won(h$monthly_won[1]), "</b>을 더 냅니다. ")),
      check_badge(chk())
    )
  })

  output$sd_title <- renderUI({
    r <- res()
    tagList(fa("chart-area"),
            span(if (!r$binding) "연동제 가격이 P* 이하: 하한 효력 없음"
                 else paste0("원유 ", num(r$Ps), "원/L에서 생산 ", num(r$Qs), "천 톤이 구매 ",
                             num(r$Qd), "천 톤을 넘어 ", num(r$stock, 1), "천 톤이 남는다")))
  })
  output$sd_source <- renderUI({
    source_line(paste0("시장: 농가 출하 단계 원유 · 기준선: 2013년 낙농진흥회 (P* 980.29원/L, Q* 2,093천 톤) · ",
                       "탄력성: ηa ", num(res()$eta_farm, 2), ", ε ", num(res()$eps_s, 2),
                       " · 점선 사각형 = 잉여 처리 비용 G (B + D)"))
  })
  output$sd_plot <- renderPlot(plot_sd(res()), res = 96)

  output$area_legend <- renderUI({
    r <- res()
    row <- function(letter, title, value, formula, eq) div(class = paste("area-row", letter),
      span(class = "area-letter", letter),
      div(class = "area-body",
          div(class = "area-title", title, span(class = "area-val", value)),
          div(class = "area-formula", formula),
          div(class = "area-eq", eq)))
    tagList(
      row("A", "유업체 → 낙농가 이전", paste0(num(r$A), "억"),
          "(Ps − P*) × (Q* + Qd) ÷ 2", "유업체가 더 낸 원유 대금. ΔCS = −A"),
      row("B", "처리 비용 → 낙농가", paste0(num(r$B), "억"),
          "(Ps − P*) × (Qs − Qd) ÷ 2", "잉여 매입으로 낙농가에 간 몫. ΔPS = A + B"),
      row("D", "사중손실", paste0(num(r$D), "억"),
          "(Qs − Qd) × [(Ps + P*) ÷ 2 − Pv]", "누구에게도 가지 않는 몫. G = B + D"),
      div(class = "area-sum",
          div(span("ΔPS"), tags$b(eok(r$dPS))), div(span("ΔCS"), tags$b(eok(r$dCS))),
          div(span("G"), tags$b(eok(-r$G))), div(class = "tot", span("ΔW = −D"), tags$b(eok(r$dW)))),
      p(class = "foot", "A는 그래프 왼쪽 축(수량 0)까지 이어집니다. 점선 사각형 전체가 잉여 처리 비용 G입니다.")
    )
  })

  output$market_tbl <- renderTable({
    r <- res()
    base  <- c(r$P0, r$P0, r$Q0, r$Q0, 0, BASE$Pr0)
    after <- c(r$Ps, r$Pd, r$Qs, r$Qd, r$stock, BASE$Pr0 + r$retail_won)
    chg   <- after - base
    rate  <- ifelse(base > 0, paste0(" (", sgn(chg / base * 100, 1), "%)"), "")
    data.frame(
      항목 = c("낙농가 수취가격 (원/L)", "유업체 구매가격 (원/L)", "원유 생산량 (천 톤)",
               "유업체 구매량 (천 톤)", "잉여 원유·분유 전환 (천 톤)", "참고: 흰 우유 소매가 (원/L)"),
      기준 = num(base), `정책 후` = num(after), 변화 = paste0(sgn(chg), rate),
      check.names = FALSE)
  }, hover = TRUE, width = "100%", align = "lrrr")

  output$elast_tbl <- renderTable(chipify(ELAST_TBL), sanitize.text.function = identity,
                                  hover = TRUE, width = "100%")
  output$elast_now <- renderUI({
    p(class = "foot", HTML(paste0("현재 설정: η ", num(input$eta_r, 2), " · ε ", num(input$eps_s, 2),
                                  " · Sa ", num(input$Sa, 2), " · σ ", num(input$sigma, 2),
                                  " → 원유 수요탄력성 η<sub>a</sub> ", num(inp()$eta_farm, 2))))
  })

  # ---- ③ 후생 분석 ----
  output$identity_badge <- renderUI(check_badge(chk()))
  output$waterfall_plot <- renderPlot(plot_waterfall(res()), res = 96)

  output$welfare_tbl <- renderUI({
    r <- res(); w <- why()
    rows <- list(
      list("유업체(구매자)잉여 변화 ΔCS", eok(r$dCS), w$dCS),
      list("낙농가(생산자)잉여 변화 ΔPS", eok(r$dPS), w$dPS),
      list("잉여 원유 처리 비용 G", eok(-r$G), w$G),
      list("순후생 변화 ΔW", eok(r$dW), w$dW),
      list("사중손실 DWL", paste0(num(r$DWL), "억 원"), w$DWL),
      list("이전효율 ΔPS/(G − ΔCS)", if (is.na(r$TE)) "-" else num(r$TE, 2), w$TE)
    )
    tags$table(class = "table table-hover welfare",
      tags$thead(tags$tr(tags$th("항목"), tags$th(class = "text-end", "값"))),
      tags$tbody(lapply(rows, function(x)
        tags$tr(tags$td(x[[1]], tooltip(span(class = "tip-icon", bs_icon("info-circle")), x[[3]],
                                        placement = "right")),
                tags$td(class = "text-end", x[[2]])))))
  })

  output$who_txt <- renderUI({
    r <- res()
    row <- function(cls, icon, who, txt)
      div(class = paste("who", cls), icon_chip(icon, cls),
          div(div(class = "who-name", who), div(class = "who-txt", txt)))
    proc_txt <- if (r$payer == "proc")
      paste0("원유 대금으로 ", eok(r$dCS), ", 쿼터 의무 매입한 잉여 처리로 ", eok(-r$G), "를 더해 모두 ",
             eok(r$proc_loss), "를 잃습니다. 쓰는 원유 1L당 실질 원료비는 ", won(r$P_proc_eff), "입니다.")
    else paste0("원유를 비싸게 사서 ", eok(r$dCS), "의 구매자잉여를 잃습니다. 소매가로 전가하면 일부가 가계 부담이 됩니다.")
    tagList(
      row("gain", "cow", "얻는 쪽: 낙농가 (원유 판매자)",
          paste0("높은 원유가격과 잉여 매입으로 ", eok(r$dPS), "의 생산자잉여를 얻습니다.")),
      row("loss", "industry", "잃는 쪽: 유가공업체 (원유 구매자)", proc_txt),
      row("cost", if (r$payer == "gov") "building-columns" else "boxes-stacked",
          paste0("내는 쪽: ", if (r$payer == "gov") "정부 재정 (납세자)" else "유가공업체 (쿼터 의무 매입)"),
          paste0("남는 원유 ", num(r$stock, 1), "천 톤을 리터당 ", won(r$Ps), "에 매입하고 ", won(r$Pv),
                 "에 처분해 ", num(r$G), "억 원이 듭니다.")),
      row("dwl", "droplet-slash", "사라지는 몫",
          paste0("누구에게도 가지 않고 ", num(r$DWL), "억 원이 사라집니다. 낙농가에 1원을 옮기는 데 ",
                 if (is.na(r$TE) || r$TE <= 0) "-" else num(1 / r$TE, 2), "원이 듭니다 (이전효율 ",
                 if (is.na(r$TE)) "-" else num(r$TE, 2), ")."))
    )
  })

  output$hh_tbl <- renderTable({
    h <- res()$household
    data.frame(계층 = h$group, `우유 지출 비중` = pct(h$budget_share * 100),
               `소매가 변화` = pct(h$pct), `월 추가 부담` = won(h$monthly_won),
               `소비지출 대비` = pct(h$income_pct, 3),
               check.names = FALSE)
  }, hover = TRUE, width = "100%", align = "lrrrr")
  output$hh_note <- renderUI({
    h <- res()$household
    ratio <- if (h$income_pct[2] > 0) h$income_pct[1] / h$income_pct[2] else NA
    p(class = "foot", HTML(paste0(
      "소매가 변화율 ≈ Sa × 원유가 변화율 = ", num(input$Sa, 2), " × ", num(res()$farm_pct, 1), "%. ",
      "월 부담 금액은 고소득층이 더 크지만, 소비지출 대비 실질 부담은 ", h$group[1], "이 ",
      if (is.na(ratio)) "-" else paste0("<b>", num(ratio, 1), "배</b>"), "입니다 (역진적).")))
  })

  srlr <- reactive(list(SR = run_preset("SR"), LR = run_preset("LR")))

  output$srlr_plot <- renderPlot({
    s <- srlr()
    lv <- c("ΔPS", "ΔCS", "−G", "ΔW")
    df <- do.call(rbind, lapply(names(s), function(n) data.frame(
      기간 = ifelse(n == "SR", "단기", "장기"),
      항목 = factor(lv, levels = lv),
      값 = c(s[[n]]$dPS, s[[n]]$dCS, -s[[n]]$G, s[[n]]$dW))))
    ggplot(df, aes(항목, 값, fill = 기간)) +
      geom_col(position = position_dodge(0.7), width = 0.62) +
      geom_hline(yintercept = 0, color = COL$muted) +
      scale_fill_manual(values = c("단기" = "#9DB5D6", "장기" = COL$primary)) +
      scale_y_continuous(labels = function(x) num(x)) +
      labs(x = NULL, y = "억 원") + theme_plot()
  }, res = 96)

  output$srlr_tbl <- renderTable({
    s <- srlr()
    col <- function(r) c(num(r$Qs), num(r$stock, 1), eok(r$dCS), eok(r$dPS), eok(-r$G),
                         eok(r$dW), paste0(num(r$DWL), "억 원"), if (is.na(r$TE)) "-" else num(r$TE, 2))
    data.frame(
      항목 = c("원유 생산량 (천 톤)", "잉여 원유 (천 톤)", "ΔCS", "ΔPS", "비용 G", "순후생 ΔW", "사중손실", "이전효율"),
      단기 = col(s$SR), 장기 = col(s$LR), check.names = FALSE)
  }, hover = TRUE, width = "100%", align = "lrr")

  output$sens_tbl <- renderUI({
    metric <- input$sens_metric
    eps_grid <- c(0.2, 0.5, 0.8, 1.2)
    dp_grid  <- c(50, 100, 150, 200)
    m <- outer(eps_grid, dp_grid, Vectorize(function(e, d)
      simulate(make_inp(BASE$P0 + d, input$eta_r, e))[[metric]]))
    cur_i <- which.min(abs(eps_grid - input$eps_s))
    cur_j <- which.min(abs(dp_grid - (input$policy_price - BASE$P0)))
    good <- metric %in% c("dPS", "TE")
    fmt <- if (metric == "TE") function(x) num(x, 2) else if (metric == "G") function(x) eok(-x) else eok
    mx <- max(abs(m), na.rm = TRUE); mn <- min(abs(m), na.rm = TRUE)
    cell <- function(i, j) {
      a <- if (mx > mn) (abs(m[i, j]) - mn) / (mx - mn) else 0.5
      rgb <- if (good) "30,138,90" else "192,57,43"
      tags$td(class = if (i == cur_i && j == cur_j) "cur",
              style = sprintf("background: rgba(%s,%.2f)", rgb, 0.08 + 0.45 * a), fmt(m[i, j]))
    }
    tags$table(class = "sens",
      tags$thead(tags$tr(tags$th("ε \\ 인상폭"),
                         lapply(dp_grid, function(d) tags$th(paste0("+", d, "원/L"))))),
      tags$tbody(lapply(seq_along(eps_grid), function(i)
        tags$tr(tags$th(paste0("ε = ", num(eps_grid[i], 1))),
                lapply(seq_along(dp_grid), function(j) cell(i, j))))))
  })

  # ---- ① 정책과 시장 ----
  output$structure_tbl <- renderTable(chipify(STRUCTURE_TBL), sanitize.text.function = identity,
                                      width = "100%", hover = TRUE)
  output$baseline_tbl  <- renderTable(chipify(BASELINE_TBL), sanitize.text.function = identity,
                                      width = "100%", hover = TRUE)

  output$reality <- renderUI({
    q13 <- TREND$qty[TREND$year == 2013]; q14 <- TREND$qty[TREND$year == 2014]
    p14 <- TREND$price[TREND$year == 2014]
    sr <- run_preset("SR", BASE$P_policy); lr <- run_preset("LR", BASE$P_policy)
    bar <- function(label, q, cls) {
      w <- max(4, min(100, (q / q13 - 1) * 100 / 10 * 100))
      div(class = "rc-row",
          div(class = "rc-label", label),
          div(class = "rc-track", div(class = paste("rc-bar", cls), style = sprintf("width:%.0f%%", w))),
          div(class = "rc-val", paste0(num(q), "천 톤 (", sgn((q / q13 - 1) * 100, 1), "%)")))
    }
    tagList(
      p(class = "rc-lead", "연동제 가격(+10.4%)이 2014년 생산을 얼마나 늘렸을까? 모형의 생산 반응과 실제 생산량을 비교합니다."),
      bar("실제 2014년", q14, "actual"),
      bar("모형 단기 (ε 0.2)", sr$Qs, "sr"),
      bar("모형 장기 (ε 0.8)", lr$Qs, "lr"),
      p(class = "rc-note", HTML(paste0(
        "실제 증가율 <b>", sgn((q14 / q13 - 1) * 100, 1), "%</b>는 단기와 장기 예측 사이에 있습니다. ",
        "2014년 연평균 수취가격은 ", won(p14), "로 연동제 가격(", won(BASE$P_policy), ")이 유지되었습니다. ",
        "실제 생산에는 날씨·사료값·쿼터 조정도 작용하므로 이 비교는 방향과 크기의 점검용입니다.")))
    )
  })

  output$m2013_plot <- renderPlot({
    m <- MONTHLY_2013
    m$기간 <- ifelse(m$month <= 7, "도입 전 (1-7월)", "도입 후 (8-12월)")
    ggplot(m, aes(month, price)) +
      annotate("rect", xmin = 7.5, xmax = 12.6, ymin = -Inf, ymax = Inf, fill = "#EAF0F8", alpha = 0.7) +
      annotate("segment", x = 0.6, xend = 7.4, y = BASE$P0, yend = BASE$P0, linetype = "dashed", color = COL$muted) +
      annotate("segment", x = 7.6, xend = 12.4, y = BASE$P_policy, yend = BASE$P_policy,
               linetype = "dashed", color = COL$primary) +
      geom_line(color = COL$ink, linewidth = 0.8) +
      geom_point(aes(color = 기간), size = 2.6) +
      annotate("text", x = 3, y = BASE$P0 - 20, label = paste0("P* ", num(BASE$P0, 2), "원 (1-7월 평균)"),
               family = PLOT_FONT, size = 3.3, color = COL$muted) +
      annotate("text", x = 10.1, y = 1000, label = paste0("정책가격 ", num(BASE$P_policy, 2), "원\n(8-12월 평균)"),
               family = PLOT_FONT, size = 3.3, color = COL$primary) +
      annotate("text", x = 7.5, y = 1075, label = "8월 연동제 적용\n+96.45원",
               family = PLOT_FONT, size = 3.1, color = COL$loss, hjust = 1.05) +
      scale_color_manual(values = c("도입 전 (1-7월)" = "#8A94A6", "도입 후 (8-12월)" = COL$primary)) +
      scale_x_continuous(NULL, breaks = 1:12, labels = paste0(1:12, "월")) +
      scale_y_continuous("원유수취가격 (원/L)", limits = c(940, 1120)) +
      theme_plot()
  }, res = 96)

  output$trend_plot <- renderPlot({
    sc <- max(TREND$price) / max(TREND$qty)
    ggplot(TREND, aes(year)) +
      geom_col(aes(y = qty * sc), fill = "#DCE5F1", width = 0.65) +
      geom_line(aes(y = price), color = COL$primary, linewidth = 1.2) +
      geom_point(aes(y = price), color = COL$primary, size = 2) +
      geom_vline(data = TREND_EVENTS, aes(xintercept = year), linetype = "dashed", color = COL$loss) +
      geom_label(data = TREND_EVENTS, aes(x = year, y = max(TREND$price) * c(1.12, 1.03), label = label),
                 family = PLOT_FONT, size = 3.1, linewidth = 0, fill = "white", color = COL$loss,
                 hjust = c(-0.05, 1.05)) +
      scale_y_continuous("원유수취가격 연평균 (원/L)", sec.axis = sec_axis(~ . / sc, name = "원유 생산량 (천 톤)")) +
      scale_x_continuous(NULL, breaks = TREND$year, labels = paste0("'", substr(TREND$year, 3, 4))) +
      theme_plot()
  }, res = 96)
  output$trend_tbl <- renderTable({
    ev <- setNames(TREND_EVENTS$label, TREND_EVENTS$year)
    data.frame(연도 = as.character(TREND$year), `원유수취가격 (원/L)` = num(TREND$price, 2),
               `생산량 (천 톤)` = num(TREND$qty, 1),
               `정책 변경` = ifelse(as.character(TREND$year) %in% names(ev), ev[as.character(TREND$year)], ""),
               check.names = FALSE)
  }, hover = TRUE, width = "100%", align = "lrrl")

  # ---- ④ 방법론 ----
  output$valid_tbl <- renderTable({
    v <- run_validation(VALIDATION_CASE)
    v$기대값 <- num(v$기대값); v$앱 <- num(v$앱)
    v
  }, width = "100%", hover = TRUE, align = "lrrc")
  output$valid_live <- renderUI({
    c <- chk()
    div(class = "valid-live", fa(if (c$ok) "circle-check" else "circle-xmark"),
        HTML(paste0("현재 설정 검산: 면적 공식 ΔW ", eok(res()$dW), " vs 곡선 적분 ΔW ",
                    eok(integrate_dW(res())), " (차이 ", num(abs(c$gap), 3), "억 원)")))
  })
  output$source_tbl <- renderTable(chipify(SOURCES_TBL), sanitize.text.function = identity,
                                   width = "100%", hover = TRUE)
}

shinyApp(ui, server, enableBookmarking = "url")
