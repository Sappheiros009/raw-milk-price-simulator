# =====================================================================
# 경제 모형 파일
# 화면(app.R)은 이 파일의 simulate()만 호출한다.
# 실제 모형으로 바꾸는 방법:
#   1. simulate() 안의 '가짜 계산' 구간을 강의 수식으로 교체한다.
#   2. 반환 목록의 이름과 단위는 그대로 둔다. (아래 '출력 약속' 참고)
#   3. 강의 worked example을 넣어 VALIDATION_TBL(data.R)을 채운다.
#   4. MODEL_IS_STUB 를 FALSE로 바꾼다 -> 화면의 PROTOTYPE 표시가 사라진다.
# =====================================================================

MODEL_IS_STUB <- TRUE

# ---- 유도수요 탄력성: ηa = Sa·η − Sb·σ (과제 명세 Part 2) -----------------
derived_eta <- function(eta_r, Sa, sigma) {
  Sa * eta_r - (1 - Sa) * sigma
}

# ---- 선형 수요·공급 보정 (점탄력성 기준, 강의 수식과 대조 필요) ------------
# 수요 Qd = a − b·P,  공급 Qs = c + d·P
calibrate <- function(P0, Q0, eta, eps) {
  b <- abs(eta) * Q0 / P0
  d <- eps * Q0 / P0
  list(a = Q0 + b * P0, b = b, c = Q0 - d * P0, d = d)
}

# ---- 후생 면적 다각형 (그래프용) --------------------------------------
welfare_polygons <- function(P0, Q0, Ps, Qd, Qs, Pfloor) {
  rbind(
    data.frame(area = "유업체 손실", x = c(0, 0, Qd, Q0), y = c(P0, Ps, Ps, P0)),
    data.frame(area = "낙농가 이득", x = c(0, 0, Qs, Q0), y = c(P0, Ps, Ps, P0)),
    data.frame(area = "잉여 원유 처리 비용", x = c(Qd, Qd, Qs, Qs), y = c(Pfloor, Ps, Ps, Pfloor))
  )
}

# =====================================================================
# simulate(inp)
# 입력 inp (list):
#   policy_price  지지가격 (원/L)
#   P0, Q0        기준선 가격 (원/L), 수량 (천 톤)
#   eta_farm      농가 단계 수요탄력성 (음수)
#   eps_s         공급탄력성
#   Sa            농가 몫
#   Pr0           소매가격 (원/L)
#   household     data.frame(group, budget_share, monthly_exp)
# 출력 약속 (이름·단위 고정):
#   Ps, Pd        낙농가 수취가격, 유업체 구매가격 (원/L)
#   Qs, Qd, stock 원유 생산량, 유업체 구매량, 잉여 원유 (천 톤)
#   (원유 시장 기준: 수요자 = 유가공업체, ΔCS = 유업체 구매자잉여 변화)
#   dCS, dPS, G, dW, DWL   (억 원)    TE 이전효율 (비율)
#   retail_pct (%), retail_won (원/L)
#   household     data.frame(group, budget_share, pct, monthly_won)
#   curves        calibrate() 결과,  areas  welfare_polygons() 결과
# 금액 환산: 원/L × 천 톤(≈ 백만 L) = 백만 원 → /100 = 억 원
# =====================================================================
simulate <- function(inp) {
  k  <- calibrate(inp$P0, inp$Q0, inp$eta_farm, inp$eps_s)
  Ps <- max(inp$policy_price, inp$P0)
  dp <- Ps - inp$P0
  Qd <- max(k$a - k$b * Ps, 0)
  Qs <- k$c + k$d * Ps
  stock <- max(Qs - Qd, 0)

  # ------------------- 가짜 계산 (STUB) 시작 -------------------
  # 화면 배치 확인용 임시 숫자다. 실제 후생식으로 교체한다.
  fake <- 0.85
  dCS <- -dp * (inp$Q0 + Qd) / 2 / 100 * fake
  dPS <-  dp * (inp$Q0 + Qs) / 2 / 100 * fake
  G   <-  stock * Ps / 100 * 0.35
  # ------------------- 가짜 계산 (STUB) 끝 ---------------------

  dW  <- dCS + dPS - G
  DWL <- max(-dW, 0)
  TE  <- if ((G - dCS) > 0) dPS / (G - dCS) else NA_real_

  retail_pct <- inp$Sa * dp / inp$P0 * 100          # 소매가 변화 ≈ Sa × 농가가 변화율
  retail_won <- inp$Pr0 * retail_pct / 100

  hh <- inp$household
  hh$pct <- retail_pct
  hh$monthly_won <- hh$monthly_exp * hh$budget_share * retail_pct / 100

  Pfloor <- inp$P0 * 0.6   # 그래프 아래쪽 경계 (그래프 표시용)
  list(
    stub = MODEL_IS_STUB,
    P0 = inp$P0, Q0 = inp$Q0, Ps = Ps, Pd = Ps, dp = dp,
    Qs = Qs, Qd = Qd, stock = stock,
    dCS = dCS, dPS = dPS, G = G, dW = dW, DWL = DWL, TE = TE,
    retail_pct = retail_pct, retail_won = retail_won,
    household = hh,
    curves = k,
    areas = welfare_polygons(inp$P0, inp$Q0, Ps, Qd, Qs, Pfloor)
  )
}

# =====================================================================
# explain(r): 화면 툴팁에 띄울 산출 근거 문장
# simulate()의 계산식을 바꾸면 이 문장도 같은 식으로 고친다.
# =====================================================================
explain <- function(r) {
  f <- function(x, d = 0) formatC(round(x, d), big.mark = ",", format = "f", digits = d)
  stub_note <- if (isTRUE(r$stub)) " × 임시계수 0.85 (가짜 계산)" else ""
  list(
    dPS = paste0("ΔPS = (Ps − P*) × (Q* + Qs) ÷ 2 ÷ 100", stub_note,
                 " = (", f(r$Ps), " − ", f(r$P0), ") × (", f(r$Q0), " + ", f(r$Qs), ") ÷ 2 ÷ 100"),
    dCS = paste0("ΔCS = −(Ps − P*) × (Q* + Qd) ÷ 2 ÷ 100", stub_note,
                 " = −(", f(r$Ps), " − ", f(r$P0), ") × (", f(r$Q0), " + ", f(r$Qd), ") ÷ 2 ÷ 100"),
    G   = paste0("G = 재고 × Ps ÷ 100",
                 if (isTRUE(r$stub)) " × 임시계수 0.35 (가짜 계산)" else "",
                 " = ", f(r$stock), " × ", f(r$Ps), " ÷ 100"),
    dW  = paste0("ΔW = ΔCS + ΔPS − G = ", f(r$dCS), " + ", f(r$dPS), " − ", f(r$G)),
    DWL = paste0("DWL = −ΔW = ", f(r$DWL)),
    TE  = paste0("이전효율 = ΔPS ÷ (G − ΔCS) = ", f(r$dPS), " ÷ (", f(r$G), " + ", f(-r$dCS), ")")
  )
}

# ---- 후생 항등식 검산: ΔCS + ΔPS − G = ΔW, DWL = max(−ΔW, 0) -------------
identity_check <- function(r, tol = 1) {
  gap <- (r$dCS + r$dPS - r$G) - r$dW
  ok  <- abs(gap) < tol && abs(r$DWL - max(-r$dW, 0)) < tol
  list(ok = ok, gap = gap)
}
