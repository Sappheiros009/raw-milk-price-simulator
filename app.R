# =====================================================================
# 원유가격 연동제 정책 시뮬레이터 (LESE412 기말 프로젝트)
# 실행: R에서 shiny::runApp("이 폴더 경로")
# 계산은 R/model.R 의 simulate() 하나만 사용한다.
# 기준연도 2013년, 자료는 낙농진흥회 낙농통계광장.
# =====================================================================
library(shiny)
library(bslib)
library(bsicons)
library(ggplot2)

source("R/data.R",  encoding = "UTF-8")
source("R/model.R", encoding = "UTF-8")

# ---- 색과 글꼴 ------------------------------------------------------
COL <- list(primary = "#1F4E8C", gain = "#1E8A5A", loss = "#C0392B",
            cost = "#8A94A6", supply = "#0F7B6C", ink = "#1B2433", cream = "#FFF8EC")
PLOT_FONT <- if (.Platform$OS.type == "windows") "Malgun Gothic" else "sans"

theme_app <- bs_theme(
  version = 5, primary = COL$primary, success = COL$gain, danger = COL$loss,
  bg = "#FFFFFF", fg = COL$ink,
  base_font = font_collection("Pretendard", "Malgun Gothic", "Apple SD Gothic Neo", "sans-serif"),
  "border-radius" = "0.75rem", "font-size-base" = "0.95rem"
)

# ---- 아이콘 (fontawesome: 젖소, 공장 등 낙농 주제 아이콘) --------------------------
fa <- function(name, ...) shiny::icon(name, ...)
icon_chip <- function(name, cls = "") span(class = paste("icon-chip", cls), fa(name))

# ---- 숫자 표시 도우미 ---------------------------------------------------
num  <- function(x, d = 0) formatC(round(x, d), big.mark = ",", format = "f", digits = d)
eok  <- function(x) paste0(ifelse(x > 0, "+", ""), num(x), "억 원")
won  <- function(x) paste0(num(x), "원")
pct  <- function(x, d = 1) paste0(num(x, d), "%")
sgn  <- function(x, d = 0) paste0(ifelse(x > 0, "+", ""), num(x, d))
status_chip <- function(s) {
  cls <- switch(s, "확인 완료" = "ok", "확인 필요" = "check", "가상" = "virtual", "-" = "none", "fake")
  as.character(tags$span(class = paste("chip", cls), s))
}
chipify <- function(df) {
  if ("상태" %in% names(df)) df$상태 <- vapply(df$상태, status_chip, "")
  df
}
source_line <- function(txt) div(class = "src-line", fa("database"), txt)
info_tip <- function(id) tooltip(span(class = "tip-icon", bs_icon("info-circle")), "", id = id,
                                 placement = "bottom")
page_foot <- function() div(class = "page-foot", fa("database"),
  "자료: 낙농진흥회 낙농통계광장 (원유생산통계·원유수취가격, 2013-2025, 2026-11-19 조회) · 기준연도 2013 · ",
  if (MODEL_IS_STUB) "후생 계산은 가짜 값 (프로토타입)" else "후생 계산은 강의 예제로 검증됨")
section_head <- function(icon, title) div(class = "side-sec", fa(icon), title)

# ---- 사이드바 조작부 ------------------------------------------------------
# 설정 링크로 값을 복원하려면 접속할 때마다 새로 만들어야 하므로 함수로 둔다
controls_ui <- function() tagList(
  section_head("layer-group", "시나리오"),
  selectInput("scenario", NULL, choices = setNames(SCENARIOS$id, SCENARIOS$name), selected = "base"),
  uiOutput("scenario_meta"),
  section_head("sliders", "정책 수준"),
  sliderInput("policy_price", "원유 지지가격 (원/L)", min = 980, max = 1300,
              value = BASE$policy_default, step = 1, ticks = FALSE),
  div(class = "slider-note", "도입 전 P* 980.29원 · 도입 후 1,082.60원"),
  section_head("hourglass-half", "낙농가 조정 기간"),
  div(class = "seg", radioButtons("run", NULL, inline = TRUE,
                                  choices = c("단기 (1년)" = "SR", "장기 (5년+)" = "LR"))),
  accordion(
    open = FALSE, class = "mt-3",
    accordion_panel(
      "탄력성 직접 조정", icon = fa("gear"),
      div(class = "derived", uiOutput("eta_farm_txt")),
      p(class = "foot mb-2", "원유 수요탄력성은 아래 소매 탄력성, Sa, σ로 계산합니다."),
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
  section_head("circle-question", "이 앱의 질문"),
  p(class = "side-note",
    "원유 기본가격을 올리면 원유를 파는 낙농가와 사는 유가공업체, 잉여 원유 처리 비용을 내는 쪽 중 누가 얼마나 얻고 잃는가, 그리고 낙농가가 생산을 조정한 뒤 답이 어떻게 바뀌는가."),
  section_head("bullseye", "분석 시장"),
  p(class = "side-note", "농가 출하 단계의 원유 시장 (원/L, 천 톤). 가계 부담은 소매가 전가를 가정한 파급 효과로만 봅니다."),
  section_head("calendar", "기준연도"),
  p(class = "side-note", "2013년. 연동제가 처음 적용된 해로, 도입 전(1-7월)과 도입 후(8-12월) 가격을 모두 관측할 수 있습니다."),
  section_head("user-tie", "사용자"),
  p(class = "side-note", "농림축산식품부 낙농 정책 담당 분석가")
)

kpi_box <- function(title, out, tip, icon, cls, caption) {
  value_box(
    title = tagList(title, info_tip(tip)), value = textOutput(out),
    p(class = "vb-cap", caption),
    showcase = icon_chip(icon, cls), showcase_layout = "left center",
    class = paste("kpi", cls)
  )
}

# ---- 탭 1: 정책·시장 -------------------------------------------------------
market_ui <- tagList(
  div(class = "page-head",
      div(class = "eyebrow", "정책과 원유 시장"),
      h2("원유가격 연동제는 어떻게 작동하나"),
      p("정책의 목표와 규모, 원유 시장의 구조, 2013년 기준선을 정리합니다.")),
  layout_columns(
    col_widths = c(6, 6),
    card(card_header(fa("file-lines"), "정책 개요"),
         div(class = "policy-list", lapply(POLICY_INFO, function(x)
           div(class = "policy-item",
               icon_chip(x$icon, "soft"),
               div(div(class = "policy-title", x$title), div(class = "policy-body", x$body)))))),
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
         source_line("자료: 낙농진흥회 원유수취가격 (전국, 원/L), 2013년 · 상태: 확인 완료 · 점선은 모형의 P*와 정책가격")),
    navset_card_underline(
      title = tagList(fa("chart-line"), "원유가격과 생산량 추이"),
      nav_panel("그래프", plotOutput("trend_plot", height = 290)),
      nav_panel("표", div(class = "tbl-nowrap", tableOutput("trend_tbl"))),
      footer = source_line("자료: 낙농진흥회 원유수취가격 연평균(원/L)·원유생산량(천 톤), 2013-2025 · 상태: 확인 완료")
    )
  ),
  card(card_header(fa("table"), "2013년 기준선"),
       div(class = "tbl-nowrap", tableOutput("baseline_tbl")),
       source_line("상태가 '확인 완료'인 값만 결론에 사용한다.")),
  page_foot()
)

# ---- 탭 2: 시뮬레이터 -------------------------------------------------------
sim_ui <- tagList(
  div(class = "question-bar",
      div(class = "eyebrow", "분석 질문"),
      uiOutput("question")),
  layout_column_wrap(
    width = "210px", fill = FALSE, class = "kpi-row",
    kpi_box("낙농가가 얻는 몫", "vb_ps", "tt_ps", "cow", "gain", "원유 판매자의 생산자잉여"),
    kpi_box("유업체가 잃는 몫", "vb_cs", "tt_cs", "industry", "loss", "원유 구매자의 잉여"),
    kpi_box("잉여 원유 처리 비용", "vb_g", "tt_g", "boxes-stacked", "cost", "분유 전환·재고 비용"),
    kpi_box("사회 전체에서 사라진 몫", "vb_dwl", "tt_dwl", "droplet-slash", "dwl", "사중손실 (DWL)")
  ),
  div(class = "story",
      div(class = "story-head", fa("lightbulb"), "한눈에 보기"),
      uiOutput("story")),
  layout_columns(
    col_widths = c(7, 5),
    card(full_screen = TRUE,
         card_header(uiOutput("sd_title", inline = TRUE)),
         plotOutput("sd_plot", height = 400),
         card_footer(uiOutput("sd_source"))),
    card(card_header(fa("clipboard-list"), "원유 시장 결과: 기준 → 정책 후 → 변화"),
         div(class = "tbl-nowrap", tableOutput("market_tbl")),
         source_line("기준 = 2013년 도입 전 P*·생산량. 변화 = 정책 후 − 기준 (괄호는 변화율)"))
  ),
  accordion(
    open = FALSE, class = "assume-box",
    accordion_panel("이 결과의 가정", icon = fa("list-check"),
                    tags$ul(class = "assume", lapply(ASSUMPTIONS, tags$li)),
                    p(class = "foot", "수식과 출처는 방법론 탭에 있습니다."))
  ),
  card(card_header(fa("sliders"), "사용한 탄력성"),
       div(class = "tbl-nowrap", tableOutput("elast_tbl"))),
  page_foot()
)

# ---- 탭 3: 후생 결과 ---------------------------------------------------------
welfare_ui <- tagList(
  div(class = "page-head",
      div(class = "eyebrow", "후생 분석"),
      h2("누가 얻고, 누가 잃고, 누가 내는가"),
      p("원유 시장의 후생 변화(낙농가, 유업체, 처리 비용), 단기와 장기 비교, 민감도 분석, 가계 파급 효과를 보여줍니다.")),
  layout_columns(
    col_widths = c(6, 6),
    card(card_header(fa("calculator"), "후생 변화 요약",
                     uiOutput("identity_badge", inline = TRUE)),
         uiOutput("welfare_tbl"),
         p(class = "foot", "ⓘ에 마우스를 올리면 산출 근거가 보입니다.")),
    card(card_header(fa("people-group"), "이해관계자별 결과"), uiOutput("who_txt"))
  ),
  layout_columns(
    col_widths = c(5, 7),
    card(card_header(fa("house-chimney"), "파급 효과: 가계 부담 (소득계층별)"),
         tableOutput("hh_tbl"),
         p(class = "foot", "원유가 상승분이 흰 우유 소매가로 전가된다고 가정. 소매가 변화율 ≈ Sa × 원유가 변화율, 가계 부담 ≈ 월 소비지출 × 우유 지출 비중 × 소매가 변화율"),
         source_line("자료: 가계동향조사 (원/월, 2013) · 상태: 가짜 값")),
    navset_card_underline(
      title = tagList(fa("right-left"), "단기 vs 장기"),
      nav_panel("그래프", plotOutput("srlr_plot", height = 260)),
      nav_panel("표", tableOutput("srlr_tbl")),
      footer = source_line("단기·장기 프리셋 탄력성을 적용. 지지가격과 Sa, σ는 현재 설정값.")
    )
  ),
  card(card_header(fa("table-cells"), "민감도 분석: 공급탄력성 × 인상폭(도입 전 P* 대비) → 순후생 변화"),
       uiOutput("sens_tbl"),
       p(class = "foot", "수요탄력성은 현재 설정값으로 고정. 굵은 테두리는 현재 설정과 가장 가까운 칸.")),
  page_foot()
)

# ---- 탭 4: 방법론 ------------------------------------------------------------
method_ui <- tagList(
  div(class = "page-head",
      div(class = "eyebrow", "방법론"),
      h2("수식, 가정, 자료, 검증"),
      p("모형의 수식, 가정, 자료 출처, 강의 예제 검증 결과를 정리합니다.")),
  withMathJax(),
  layout_columns(
    col_widths = c(7, 5),
    card(card_header(fa("square-root-variable"), "수식"),
         div(class = "eq",
             h6("1. 선형 수요·공급 (2013년 기준점에서 보정)"),
             p("$$Q^d = a - bP,\\quad Q^s = c + dP$$"),
             p("$$b = |\\eta_a|\\,\\frac{Q^*}{P^*},\\quad d = \\varepsilon\\,\\frac{Q^*}{P^*}$$"),
             h6("2. 원유 수요탄력성 (유업체의 유도수요)"),
             p("$$\\eta_a = S_a\\,\\eta - S_b\\,\\sigma,\\quad S_b = 1 - S_a$$"),
             h6("3. 지지가격과 잉여 매입 (wedge)"),
             p("$$Q^d(P_s) < Q^s(P_s),\\quad \\text{잉여} = Q^s(P_s) - Q^d(P_s)$$"),
             h6("4. 후생"),
             p("$$\\Delta W = \\Delta CS + \\Delta PS - G,\\quad DWL = -\\Delta W$$"),
             p("$$\\text{이전효율} = \\frac{\\Delta PS}{G - \\Delta CS}$$"),
             h6("5. 가계 파급 (원유가 → 소매가 전가)"),
             p("$$\\%\\Delta P_{retail} \\approx S_a \\times \\%\\Delta P_{farm}$$")),
         p(class = "foot", "모든 수식은 강의 Supply Policy 슬라이드와 대조해 확인한다.")),
    card(card_header(fa("list-check"), "가정"),
         tags$ul(class = "assume", lapply(ASSUMPTIONS, tags$li)))
  ),
  layout_columns(
    col_widths = c(6, 6),
    card(card_header(fa("clipboard-check"), "강의 예제 검증"), tableOutput("valid_tbl")),
    card(card_header(fa("link"), "자료 출처"), tableOutput("source_tbl"),
         source_line("낙농진흥회 낙농통계광장: dairy.or.kr › 낙농통계광장 › 국내통계 › 원유생산통계"))
  ),
  page_foot()
)

# ---- 전체 화면 (설정 링크 저장을 위해 함수 형태) ------------------------------------
ui <- function(request) {
  page_navbar(
    id = "nav",
    window_title = "원유가격 시뮬레이터",
    title = tags$span(class = "brand",
                      span(class = "brand-mark", fa("cow")),
                      span(class = "brand-text",
                           span(class = "brand-name", "원유가격 시뮬레이터"),
                           span(class = "brand-sub", "2013 기준 · 낙농진흥회 통계")),
                      if (MODEL_IS_STUB) tags$span(class = "badge-proto", "PROTOTYPE · 후생은 가짜 값")),
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
    nav_panel("시뮬레이터", value = "sim",     icon = fa("sliders"),               sim_ui),
    nav_panel("후생 결과",  value = "welfare", icon = fa("scale-unbalanced"),      welfare_ui),
    nav_panel("정책·시장",  value = "market",  icon = fa("truck-droplet"),         market_ui),
    nav_panel("방법론",     value = "method",  icon = fa("square-root-variable"),  method_ui),
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
          axis.title = element_text(color = "#5B6676", size = 10.5),
          axis.text = element_text(color = "#5B6676"),
          legend.position = "bottom", legend.title = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(8, 12, 8, 8))
}

plot_sd <- function(r) {
  k <- r$curves
  qx <- seq(r$Q0 * 0.6, r$Q0 * 1.4, length.out = 100)
  lines <- rbind(
    data.frame(curve = "원유 수요 (유업체)", Q = qx, P = (k$a - qx) / k$b),
    data.frame(curve = "원유 공급 (낙농가)", Q = qx, P = (qx - k$c) / k$d)
  )
  ylim <- c(r$P0 * 0.6, r$P0 * 1.4)
  ggplot() +
    geom_polygon(data = r$areas, aes(x, y, fill = area), alpha = 0.28, color = NA) +
    geom_line(data = lines, aes(Q, P, color = curve), linewidth = 1.1) +
    geom_hline(yintercept = r$P0, linetype = "dotted", color = "#5B6676") +
    geom_hline(yintercept = r$Ps, linetype = "dashed", color = COL$primary) +
    geom_point(aes(x = c(r$Qd, r$Qs, r$Q0), y = c(r$Ps, r$Ps, r$P0)),
               size = 2.6, color = COL$ink) +
    annotate("label", x = r$Q0 * 1.38, y = r$Ps, label = paste0("정책가격 ", num(r$Ps)),
             hjust = 1, vjust = -0.3, size = 3.4, family = PLOT_FONT, linewidth = 0, fill = "white") +
    annotate("label", x = r$Q0 * 1.38, y = r$P0, label = paste0("도입 전 P* ", num(r$P0)),
             hjust = 1, vjust = 1.3, size = 3.4, family = PLOT_FONT, linewidth = 0, fill = "white") +
    scale_color_manual(values = c("원유 수요 (유업체)" = COL$primary, "원유 공급 (낙농가)" = COL$supply)) +
    scale_fill_manual(values = c("유업체 손실" = COL$loss, "낙농가 이득" = COL$gain,
                                 "잉여 원유 처리 비용" = COL$cost)) +
    guides(color = guide_legend(order = 1, nrow = 1), fill = guide_legend(order = 2, nrow = 1)) +
    coord_cartesian(xlim = range(qx), ylim = ylim, expand = FALSE) +
    labs(x = "원유 수량 (천 톤)", y = "원유 가격 (원/L)") +
    theme_plot() +
    theme(legend.box = "vertical", legend.spacing.y = unit(2, "pt"),
          panel.grid.major.x = element_line(color = "#EDF0F4"))
}

# ---- 서버 ----------------------------------------------------------------------
server <- function(input, output, session) {

  # 설정 링크: 버튼을 누르면 현재 입력값을 URL로 저장하고 복사 창을 띄운다
  setBookmarkExclude(c("bookmark", "reset", "tt_ps", "tt_cs", "tt_g", "tt_dwl"))
  observeEvent(input$bookmark, session$doBookmark())
  onBookmarked(function(url) {
    updateQueryString(url)
    showModal(modalDialog(
      title = "이 설정 링크",
      p("아래 링크를 열면 지금의 시나리오, 지지가격, 탄력성, 탭이 그대로 재현됩니다. 발표 Q&A용으로 미리 저장해 두세요."),
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
    updateSliderInput(session, "eta_r", value = ELAST_PRESET$SR$eta_r)
    updateSliderInput(session, "eps_s", value = ELAST_PRESET$SR$eps_s)
    updateSliderInput(session, "Sa", value = ELAST_DEFAULT$Sa)
    updateSliderInput(session, "sigma", value = ELAST_DEFAULT$sigma)
  })

  make_inp <- function(policy_price, eta_r, eps_s, Sa, sigma) {
    list(policy_price = policy_price, P0 = BASE$P0, Q0 = BASE$Q0,
         eta_farm = derived_eta(eta_r, Sa, sigma), eps_s = eps_s, Sa = Sa,
         Pr0 = BASE$Pr0, household = HOUSEHOLD)
  }

  inp <- reactive(make_inp(input$policy_price, input$eta_r, input$eps_s, input$Sa, input$sigma))
  res <- reactive(simulate(inp()))
  why <- reactive(explain(res()))
  chk <- reactive(identity_check(res()))

  output$eta_farm_txt <- renderUI({
    HTML(paste0("원유 수요탄력성 η<sub>a</sub> = <b>", num(inp()$eta_farm, 2), "</b>"))
  })

  # 질문 문장과 KPI
  output$question <- renderUI({
    r <- res()
    div(class = "q-text",
      HTML(paste0("원유 기본가격을 리터당 <span class='hl'>", num(r$Ps), "원</span>으로 정하면",
                  "<span class='q-sub'>(", if (r$dp < 0.5) "도입 전 수준"
                  else paste0("도입 전 대비 +", num(r$dp), "원"), ")</span>, ",
                  if (input$run == "SR") "1년 안에" else "낙농가가 생산을 조정한 뒤",
                  " 누가 얼마나 부담할까?")))
  })
  output$vb_ps  <- renderText(eok(res()$dPS))
  output$vb_cs  <- renderText(eok(res()$dCS))
  output$vb_g   <- renderText(eok(-res()$G))
  output$vb_dwl <- renderText(paste0(num(res()$DWL), "억 원"))

  # 산출 근거 툴팁 갱신
  observe({
    w <- why()
    update_tooltip("tt_ps",  w$dPS)
    update_tooltip("tt_cs",  w$dCS)
    update_tooltip("tt_g",   w$G)
    update_tooltip("tt_dwl", w$DWL)
  })

  check_badge <- function(c) {
    if (c$ok) span(class = "id-badge ok", fa("circle-check"), "ΔCS + ΔPS − G = ΔW 검산 통과")
    else span(class = "id-badge bad", fa("circle-xmark"),
              paste0("검산 불일치: 차이 ", num(c$gap, 1), "억 원"))
  }

  output$story <- renderUI({
    r <- res(); h <- r$household
    tagList(
      HTML(paste0(
        "유업체는 원유를 리터당 <b>", won(r$dp), "</b> 더 주고 사며, ",
        "구매량은 <b>", num(r$Qd), "천 톤</b>으로 줄고 남는 원유 <b>", num(r$stock),
        "천 톤</b>은 분유로 전환됩니다. 이 부담이 소매가로 전가되면 흰 우유가 리터당 약 <b>",
        won(r$retail_won), "</b> 오르고, ", h$group[1], " 가구는 한 달에 약 <b>",
        won(h$monthly_won[1]), "</b>을 더 냅니다. ")),
      check_badge(chk())
    )
  })

  # 수급 그래프: 결론 문장 제목과 출처 줄
  output$sd_title <- renderUI({
    r <- res()
    tagList(fa("chart-area"),
            span(paste0("원유 ", num(r$Ps), "원/L에서 낙농가 생산 ", num(r$Qs), "천 톤이 유업체 구매 ",
                        num(r$Qd), "천 톤을 넘어 ", num(r$stock), "천 톤이 남는다")))
  })
  output$sd_source <- renderUI({
    source_line(paste0("시장: 농가 출하 단계 원유 · 기준선: 2013년 낙농진흥회 (P* 980.29원/L, Q* 2,093천 톤) · 모형: 선형 보정 + 지지가격·잉여 매입 · ",
                       if (MODEL_IS_STUB) "후생: 가짜 값" else "후생: 강의 예제 검증 완료"))
  })
  output$sd_plot <- renderPlot(plot_sd(res()), res = 96)

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

  # 후생 결과
  output$identity_badge <- renderUI(check_badge(chk()))

  output$welfare_tbl <- renderUI({
    r <- res(); w <- why()
    rows <- list(
      list("유업체(구매자)잉여 변화 ΔCS", eok(r$dCS), w$dCS),
      list("낙농가(생산자)잉여 변화 ΔPS", eok(r$dPS), w$dPS),
      list("잉여 원유 처리 비용 G", eok(-r$G), w$G),
      list("순후생 변화 ΔW", eok(r$dW), w$dW),
      list("사중손실 DWL", paste0(num(r$DWL), "억 원"), w$DWL),
      list("이전효율 ΔPS/(G−ΔCS)", if (is.na(r$TE)) "-" else num(r$TE, 2), w$TE)
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
    tagList(
      row("gain", "cow", "얻는 쪽: 낙농가 (원유 판매자)",
          paste0("높은 원유가격으로 ", eok(r$dPS), "의 생산자잉여를 얻습니다.")),
      row("loss", "industry", "잃는 쪽: 유가공업체 (원유 구매자)",
          paste0("원유를 비싸게 사서 ", eok(r$dCS), "의 구매자잉여를 잃습니다. 소매가로 전가하면 일부가 가계 부담이 됩니다.")),
      row("cost", "boxes-stacked", "내는 쪽: 잉여 원유 처리 (유업체·정부 지원)",
          paste0("남는 원유를 분유로 전환하는 데 ", num(r$G), "억 원이 듭니다. 분담 비율은 확인 필요.")),
      row("dwl", "droplet-slash", "사라지는 몫",
          paste0("누구에게도 가지 않고 ", num(r$DWL), "억 원이 사라집니다. 1원을 낙농가에 옮기는 데 ",
                 if (is.na(r$TE) || r$TE <= 0) "-" else num(1 / r$TE, 2), "원이 듭니다."))
    )
  })

  output$hh_tbl <- renderTable({
    h <- res()$household
    data.frame(계층 = h$group, `우유 지출 비중` = pct(h$budget_share * 100),
               `소매가 변화` = pct(h$pct), `월 추가 부담` = won(h$monthly_won),
               check.names = FALSE)
  }, hover = TRUE, width = "100%", align = "lrrr")

  srlr <- reactive({
    lapply(c(SR = "SR", LR = "LR"), function(run) {
      p <- ELAST_PRESET[[run]]
      simulate(make_inp(input$policy_price, p$eta_r, p$eps_s, input$Sa, input$sigma))
    })
  })

  output$srlr_plot <- renderPlot({
    s <- srlr()
    df <- do.call(rbind, lapply(names(s), function(n) data.frame(
      기간 = ifelse(n == "SR", "단기", "장기"),
      항목 = factor(c("ΔCS", "ΔPS", "−G", "ΔW"), levels = c("ΔCS", "ΔPS", "−G", "ΔW")),
      값 = c(s[[n]]$dCS, s[[n]]$dPS, -s[[n]]$G, s[[n]]$dW))))
    ggplot(df, aes(항목, 값, fill = 기간)) +
      geom_col(position = position_dodge(0.7), width = 0.62) +
      geom_hline(yintercept = 0, color = "#5B6676") +
      scale_fill_manual(values = c("단기" = "#9DB5D6", "장기" = COL$primary)) +
      labs(x = NULL, y = "억 원") + theme_plot()
  }, res = 96)

  output$srlr_tbl <- renderTable({
    s <- srlr()
    data.frame(
      항목 = c("원유 생산량 (천 톤)", "잉여 원유 (천 톤)", "ΔCS", "ΔPS", "비용 G", "순후생 ΔW", "사중손실"),
      단기 = c(num(s$SR$Qs), num(s$SR$stock), eok(s$SR$dCS), eok(s$SR$dPS), eok(-s$SR$G),
               eok(s$SR$dW), paste0(num(s$SR$DWL), "억 원")),
      장기 = c(num(s$LR$Qs), num(s$LR$stock), eok(s$LR$dCS), eok(s$LR$dPS), eok(-s$LR$G),
               eok(s$LR$dW), paste0(num(s$LR$DWL), "억 원")),
      check.names = FALSE)
  }, hover = TRUE, width = "100%", align = "lrr")

  output$sens_tbl <- renderUI({
    eps_grid <- c(0.2, 0.5, 1.0)
    dp_grid  <- c(50, 100, 150)
    m <- outer(eps_grid, dp_grid, Vectorize(function(e, d)
      simulate(make_inp(BASE$P0 + d, input$eta_r, e, input$Sa, input$sigma))$dW))
    cur_i <- which.min(abs(eps_grid - input$eps_s))
    cur_j <- which.min(abs(dp_grid - (input$policy_price - BASE$P0)))
    mx <- max(abs(m))
    cell <- function(i, j) {
      a <- if (mx > 0) abs(m[i, j]) / mx else 0
      tags$td(class = if (i == cur_i && j == cur_j) "cur",
              style = sprintf("background: rgba(192,57,43,%.2f)", 0.08 + 0.5 * a),
              eok(m[i, j]))
    }
    tags$table(class = "sens",
      tags$thead(tags$tr(tags$th("공급탄력성 ε \\ 인상폭"),
                         lapply(dp_grid, function(d) tags$th(paste0("+", d, "원/L"))))),
      tags$tbody(lapply(seq_along(eps_grid), function(i)
        tags$tr(tags$th(num(eps_grid[i], 1)), lapply(seq_along(dp_grid), function(j) cell(i, j))))))
  })

  # 정책·시장 탭
  output$structure_tbl <- renderTable(chipify(STRUCTURE_TBL), sanitize.text.function = identity,
                                      width = "100%", hover = TRUE)
  output$baseline_tbl  <- renderTable(chipify(BASELINE_TBL), sanitize.text.function = identity,
                                      width = "100%", hover = TRUE)

  output$m2013_plot <- renderPlot({
    m <- MONTHLY_2013
    m$기간 <- ifelse(m$month <= 7, "도입 전 (1-7월)", "도입 후 (8-12월)")
    ggplot(m, aes(month, price)) +
      annotate("rect", xmin = 7.5, xmax = 12.6, ymin = -Inf, ymax = Inf, fill = "#EAF0F8", alpha = 0.7) +
      geom_segment(aes(x = 0.6, xend = 7.4, y = BASE$P0, yend = BASE$P0), linetype = "dashed", color = "#5B6676") +
      geom_segment(aes(x = 7.6, xend = 12.4, y = BASE$P_policy, yend = BASE$P_policy),
                   linetype = "dashed", color = COL$primary) +
      geom_line(color = COL$ink, linewidth = 0.8) +
      geom_point(aes(color = 기간), size = 2.6) +
      annotate("text", x = 3, y = BASE$P0 - 20, label =paste0("P* ", num(BASE$P0, 2), "원 (1-7월 평균)"),
               family = PLOT_FONT, size = 3.3, color = "#5B6676") +
      annotate("text", x = 10.1, y = 1000, label =paste0("정책가격 ", num(BASE$P_policy, 2), "원\n(8-12월 평균)"),
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

  # 방법론 탭
  output$valid_tbl  <- renderTable(VALIDATION_TBL, width = "100%", hover = TRUE)
  output$source_tbl <- renderTable(chipify(SOURCES_TBL), sanitize.text.function = identity,
                                   width = "100%", hover = TRUE)
}

shinyApp(ui, server, enableBookmarking = "url")
