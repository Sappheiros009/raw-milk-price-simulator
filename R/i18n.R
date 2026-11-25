# =====================================================================
# 영어 번역 (한/영 전환)
# - data.R의 한국어 자료와 같은 순서로 영어 문구를 둔다.
#   data.R의 값을 바꾸면 여기의 같은 위치도 함께 고친다.
# - 금액 단위: 억 원 → ₩ bn (1억 원 = ₩0.1 bn), 천 톤 → kt, 원/L → KRW/L
# =====================================================================

STATUS_EN <- c("확인 완료" = "Verified", "확인 필요" = "To verify", "임시 값" = "Placeholder",
               "가상" = "Hypothetical", "-" = "-")

POLICY_SOURCE_EN <- "Formula and background come from the government policy blog (Korea Policy Briefing, blog.naver.com/hellopolicy/220156198742); scale and change come from Korea Dairy Committee dairy statistics."

VALUE_CHAIN_EN <- data.frame(
  step = c("Dairy farms", "Collection", "Processors", "Retail & consumers"),
  note = c("Raw milk sellers (quota holders)", "Korea Dairy Committee, co-ops", "Raw milk buyers", "Pass-through only")
)

STRUCTURE_TBL_EN <- data.frame(
  Item = c("Market level", "Raw milk sellers", "Buyer concentration (processor CR4)",
           "Raw milk trade", "Binding border measures", "Storability", "Is perfect competition reasonable?"),
  Detail = c("Farm-gate raw milk (KRW/L, kt)",
             "Quota-holding farms (collection: Korea Dairy Committee, co-ops)",
             "To be entered",
             "Raw milk is hardly traded because it spoils quickly. It is imported as cheese, powder and other dairy products.",
             "The dairy FTA tariff elimination schedule needs to be checked.",
             "2-3 days refrigerated (long-term storage only as powder)",
             "Buyer-side oligopoly needs to be checked with CR4."),
  check.names = FALSE
)

BASELINE_TBL_EN <- data.frame(
  Item = c("Equilibrium price P* (pre-scheme, Jan-Jul avg.)", "Linked price Ps (post-scheme, Aug-Dec avg.)",
           "Raw milk production Q*", "Ref.: annual average farm-gate price",
           "Processor raw milk purchases (fluid and manufacturing)", "Surplus raw milk (converted to powder)",
           "Ref.: retail price of white milk"),
  Value = c("980.29", "1,082.60", "2,093.1", "1,022.92", "To be entered", "To be entered", "2,400"),
  Unit = c("KRW/L", "KRW/L", "kt", "KRW/L", "kt", "kt", "KRW/L"),
  Source = c("KDC farm-gate price", "KDC farm-gate price", "KDC raw milk production", "KDC farm-gate price",
             "KDC", "KDC", "KAMIS"),
  check.names = FALSE
)

ELAST_TBL_EN <- data.frame(
  Symbol = c("ηa (raw milk demand, processors)", "ε (raw milk supply, farms)", "η (retail white milk demand)",
             "Sa (raw milk share of retail price)", "σ (raw milk vs. other inputs)"),
  Source = c("Derived demand formula", "To be entered", "To be entered", "To be entered", "To be entered"),
  Level = c("Raw milk (farm-gate)", "Raw milk (farm-gate)", "Retail", "Raw milk / retail", "Processing"),
  check.names = FALSE
)

SOURCES_TBL_EN <- data.frame(
  Data = c("Raw milk production (monthly, annual)", "Farm-gate price (national, monthly)", "Milk production cost",
           "Retail price of white milk", "Dairy imports", "Elasticities", "Policy scale and budget"),
  Institution = c("Korea Dairy Committee raw milk production", "Korea Dairy Committee farm-gate price",
                  "KOSIS livestock production cost survey", "KAMIS", "Korea Customs Service trade statistics",
                  "KREI research reports", "MAFRA, Korea Dairy Committee"),
  check.names = FALSE
)

SCENARIOS_EN <- data.frame(
  name = c("Custom", "2013 scheme introduced (Aug-Dec avg.)", "Pre-scheme level (no policy)",
           "+50 KRW on top of 2013 level (hypothetical)", "+150 KRW, long run (hypothetical)"),
  source = c("-", "KDC farm-gate price", "KDC farm-gate price", "Hypothetical scenario", "Hypothetical scenario")
)

TREND_EVENTS_EN <- c("Scheme starts (Aug 2013)", "Pricing reform (to verify)")

HOUSEHOLD_GROUP_EN <- c("Income quintile 1", "Income quintile 5")

PAYER_EN <- c(gov = "Government", proc = "Processors")

VALIDATION_LABEL_EN <- "Lecture L11 p.6 price-support area formulas + textbook numbers (Qd = 100 − P, Qs = P, Ps = 60)"
VALIDATION_ITEMS_EN <- c("Processor purchases Qd", "Farm output Qs", "Surplus bought Qs − Qd",
                         "a + b (consumers → producers)", "c (new producer gain)",
                         "ΔCS = −(a + b)", "ΔPS = a + b + c", "G = b + c + d + e", "DWL = b + d + e",
                         "Resale v = 20: G", "Resale v = 20: DWL")
