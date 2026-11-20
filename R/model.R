# =====================================================================
# 경제 모형 파일
# 화면(app.R)은 이 파일의 simulate()만 호출한다.
#
# 모형: 원유(농가 출하 단계) 시장의 선형 수요·공급 + 가격지지(wedge)
#   - 기준점 (P*, Q*)에서 점탄력성으로 선형 곡선을 보정한다.
#   - 연동제 가격 Ps가 P*보다 높으면 가격하한으로 작동한다.
#     낙농가는 Qs(Ps)를 생산하고, 유업체는 Qd(Ps)만 쓰며,
#     남는 원유 Qs − Qd는 매입 후 분유 등으로 전환(처분가치 Pv)된다.
#   - 강의 공급정책(정부 매입을 수반하는 가격지지제)의 후생 계산을 따른다.
#
# 후생 면적 (그래프의 A, B, D):
#   A = 유업체 → 낙농가 이전 = (Ps − P*)(Q* + Qd)/2      ΔCS = −A
#   B = 잉여 매입비 → 낙농가  = (Ps − P*)(Qs − Qd)/2      ΔPS = A + B
#   D = 사중손실             = (Qs − Qd)[(Ps + P*)/2 − Pv] G   = B + D
#   ΔW = ΔCS + ΔPS − G = −D
#
# 금액 환산: 원/L × 천 톤(≈ 백만 L) = 백만 원 → ÷100 = 억 원 (unit = 100)
# =====================================================================

# ---- 유도수요 탄력성 (Muth): ηa = Sa·η − Sb·σ, Sb = 1 − Sa -------------
derived_eta <- function(eta_r, Sa, sigma) {
  Sa * eta_r - (1 - Sa) * sigma
}

# ---- 선형 수요·공급 보정 (기준점 점탄력성) ----------------------------
# 수요 Qd = a − b·P,  공급 Qs = c + d·P
calibrate <- function(P0, Q0, eta, eps) {
  b <- abs(eta) * Q0 / P0
  d <- eps * Q0 / P0
  list(a = Q0 + b * P0, b = b, c = Q0 - d * P0, d = d)
}
inv_demand <- function(k, q) (k$a - q) / k$b
inv_supply <- function(k, q) (q - k$c) / k$d

# ---- 후생 면적 다각형 (그래프용) --------------------------------------
welfare_polygons <- function(P0, Q0, Ps, Qd, Qs, Pv) {
  if (Ps - P0 < 1e-9) return(data.frame(area = character(), x = numeric(), y = numeric()))
  rbind(
    data.frame(area = "A", x = c(0, 0, Qd, Q0), y = c(P0, Ps, Ps, P0)),
    data.frame(area = "B", x = c(Qd, Qs, Q0), y = c(Ps, Ps, P0)),
    data.frame(area = "D", x = c(Qd, Qd, Q0, Qs, Qs), y = c(Pv, Ps, P0, Ps, Pv))
  )
}

# =====================================================================
# simulate(inp)
# 입력 inp (list):
#   policy_price  연동제 원유가격 (원/L)
#   P0, Q0        기준선 가격 (원/L), 수량 (천 톤)
#   eta_farm      원유 수요탄력성 (유업체, 음수)
#   eps_s         원유 공급탄력성
#   Pv            잉여 원유 처분가치 (원/L, 원유 환산)
#   payer         잉여 처리 비용 부담: "gov" 정부 / "proc" 유업체 의무 매입
#   Sa            소매가 중 원유 몫
#   Pr0           흰 우유 소매가격 (원/L)
#   household     data.frame(group, budget_share, monthly_exp)
#   unit          금액 나눗수 (기본 100 → 억 원). 검증 예제는 1.
# =====================================================================
simulate <- function(inp) {
  unit  <- if (is.null(inp$unit)) 100 else inp$unit
  Pv    <- if (is.null(inp$Pv)) 0 else inp$Pv
  payer <- if (is.null(inp$payer)) "gov" else inp$payer

  P0 <- inp$P0; Q0 <- inp$Q0
  k  <- calibrate(P0, Q0, inp$eta_farm, inp$eps_s)
  binding <- inp$policy_price > P0
  Ps <- if (binding) inp$policy_price else P0      # 하한이 P* 아래면 효력 없음
  dp <- Ps - P0

  Qd    <- max(k$a - k$b * Ps, 0)
  Qs    <- max(k$c + k$d * Ps, 0)
  stock <- max(Qs - Qd, 0)

  # ---- 후생 (강의 가격지지 모형) ----
  A   <- dp * (Q0 + Qd) / 2 / unit
  B   <- dp * stock / 2 / unit
  G_gross <- Ps * stock / unit                     # 잉여 매입액
  salvage <- Pv * stock / unit                     # 처분 수입
  G   <- G_gross - salvage                         # 순 처리 비용
  D   <- G - B
  dCS <- -A
  dPS <- A + B
  dW  <- dCS + dPS - G
  DWL <- -dW
  TE  <- if ((G - dCS) > 1e-9) dPS / (G - dCS) else NA_real_

  # 비용 부담 주체별 귀착
  gov_cost  <- if (payer == "gov") G else 0
  proc_loss <- dCS - (if (payer == "proc") G else 0)
  P_proc_eff <- if (payer == "proc" && Qd > 0) Ps + G * unit / Qd else Ps   # 유업체 실질 원료비 (원/L)

  # ---- 소매·가계 파급 (Muth: 소매가 변화율 ≈ Sa × 농가가 변화율) ----
  farm_pct   <- dp / P0 * 100
  retail_pct <- inp$Sa * farm_pct
  retail_won <- inp$Pr0 * retail_pct / 100
  hh <- inp$household
  hh$pct <- retail_pct
  hh$monthly_won <- hh$monthly_exp * hh$budget_share * retail_pct / 100
  hh$income_pct  <- hh$budget_share * retail_pct          # 소비지출 대비 실질부담 (%)

  list(
    P0 = P0, Q0 = Q0, Ps = Ps, Pd = Ps, dp = dp, binding = binding,
    Qs = Qs, Qd = Qd, stock = stock, Pv = Pv, payer = payer,
    A = A, B = B, D = D,
    dCS = dCS, dPS = dPS, G = G, G_gross = G_gross, salvage = salvage,
    dW = dW, DWL = DWL, TE = TE,
    gov_cost = gov_cost, proc_loss = proc_loss, P_proc_eff = P_proc_eff,
    farm_pct = farm_pct, retail_pct = retail_pct, retail_won = retail_won,
    household = hh, eta_farm = inp$eta_farm, eps_s = inp$eps_s,
    curves = k, unit = unit,
    areas = welfare_polygons(P0, Q0, Ps, Qd, Qs, Pv)
  )
}

# =====================================================================
# explain(r): 화면 툴팁에 띄울 산출 근거 문장 (simulate()와 같은 식)
# =====================================================================
explain <- function(r) {
  f <- function(x, d = 0) formatC(round(x, d), big.mark = ",", format = "f", digits = d)
  u <- if (r$unit == 100) " ÷ 100" else ""
  list(
    dPS = paste0("ΔPS = A + B = (Ps − P*) × (Q* + Qs) ÷ 2", u, " = (", f(r$Ps), " − ", f(r$P0),
                 ") × (", f(r$Q0), " + ", f(r$Qs), ") ÷ 2", u),
    dCS = paste0("ΔCS = −A = −(Ps − P*) × (Q* + Qd) ÷ 2", u, " = −(", f(r$Ps), " − ", f(r$P0),
                 ") × (", f(r$Q0), " + ", f(r$Qd), ") ÷ 2", u),
    G   = paste0("G = B + D = (Ps − Pv) × (Qs − Qd)", u, " = (", f(r$Ps), " − ", f(r$Pv),
                 ") × ", f(r$stock, 1), u),
    dW  = paste0("ΔW = ΔCS + ΔPS − G = ", f(r$dCS), " + ", f(r$dPS), " − ", f(r$G)),
    DWL = paste0("DWL = D = (Qs − Qd) × [(Ps + P*) ÷ 2 − Pv]", u, " = ", f(r$stock, 1),
                 " × (", f((r$Ps + r$P0) / 2), " − ", f(r$Pv), ")", u),
    TE  = paste0("이전효율 = ΔPS ÷ (G − ΔCS) = ", f(r$dPS), " ÷ (", f(r$G), " + ", f(-r$dCS), ")")
  )
}

# ---- 독립 검산: 곡선 아래 면적을 직접 적분해 ΔW를 다시 구한다 ------------
# ΔW = −∫[Qd,Q*] D(q)dq − ∫[Q*,Qs] S(q)dq + Pv(Qs − Qd)
#  (소비자가치 감소 − 추가 생산비 + 잉여 원유 처분가치)
integrate_dW <- function(r) {
  k <- r$curves
  area_lin <- function(f, lo, hi) (hi - lo) * (f(k, lo) + f(k, hi)) / 2   # 선형이므로 사다리꼴이 정확
  lost_value <- area_lin(inv_demand, r$Qd, r$Q0)
  extra_cost <- area_lin(inv_supply, r$Q0, r$Qs)
  (-lost_value - extra_cost + r$Pv * r$stock) / r$unit
}

identity_check <- function(r, tol = 0.5) {
  gap <- integrate_dW(r) - r$dW
  list(ok = abs(gap) < tol, gap = gap)
}

# ---- 교과서형 예제 검증: 손으로 계산한 기대값과 simulate() 결과 비교 --------
run_validation <- function(case) {
  r <- simulate(list(policy_price = case$Ps, P0 = case$P0, Q0 = case$Q0,
                     eta_farm = case$eta, eps_s = case$eps, Pv = case$Pv,
                     Sa = 1, Pr0 = 0, unit = 1,
                     household = data.frame(group = "-", budget_share = 0, monthly_exp = 0)))
  got <- c(r$Qd, r$Qs, r$stock, r$dCS, r$dPS, r$G, r$DWL)
  exp <- unlist(case$expected[c("Qd", "Qs", "stock", "dCS", "dPS", "G", "DWL")])
  data.frame(항목 = c("유업체 구매량 Qd", "낙농가 생산량 Qs", "잉여 매입량", "ΔCS", "ΔPS", "정부 비용 G", "DWL"),
             기대값 = exp, 앱 = round(got, 4), 일치 = ifelse(abs(got - exp) < 1e-6, "✓", "✗"),
             check.names = FALSE)
}
