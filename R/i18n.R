# =====================================================================
# 영어 번역 (한/영 전환)
# - data.R의 한국어 자료와 같은 순서로 영어 문구를 둔다.
#   data.R의 값을 바꾸면 여기의 같은 위치도 함께 고친다.
# - 금액 단위: 억 원 → ₩ bn (1억 원 = ₩0.1 bn), 천 톤 → kt, 원/L → KRW/L
# =====================================================================

STATUS_EN <- c("확인 완료" = "Verified", "확인 필요" = "To verify", "임시 값" = "Placeholder",
               "가상" = "Hypothetical", "-" = "-")

ASSUMPTIONS_EN <- c(
  "The market analysed is raw milk (unprocessed milk) at the farm-gate. Sellers are dairy farms; buyers are dairy processors.",
  "The base year is 2013. The average farm-gate price before the scheme (Jan-Jul) is the equilibrium price P*; the average after it (Aug-Dec) is the linked price. Quotas and negotiated prices existed before the scheme too, which is a limitation.",
  "Prices are the Korea Dairy Committee 'farm-gate price' (base price + component/hygiene incentives), which differs from the policy base price.",
  "Raw milk is highly perishable and hardly traded internationally, so the market is treated as closed.",
  "The linked price does not fall when demand falls, so it is modelled as a price floor (wedge). Farms receive and processors pay the same Ps, leaving a quantity wedge (Qs − Qd). Surplus milk is bought and turned into milk powder, recovering a salvage value Pv.",
  "The cost of buying the surplus is paid by government (as in the lecture model, default) or by processors (as under mandatory quota purchases). The payer changes only the distribution; net welfare is the same.",
  "Raw milk demand and supply are linear near the base point. 1 tonne of raw milk ≈ 1,000 L.",
  "Short and long run differ only in the supply elasticity of raw milk and the retail demand elasticity.",
  "The household burden is a pass-through approximation: the raw milk price increase passes into the retail price of white milk at the share Sa. Elasticities are not used, so the short- and long-run household burden is the same."
)

POLICY_INFO_EN <- list(
  list(title = "Goal", points = c(
    "The goal is to stabilise <b>dairy farm income</b> and the <b>raw milk supply</b>.",
    "Producers and processors used to negotiate the price <u>every 3 to 5 years</u>, and each round caused conflict.",
    "The scheme now <b>passes changes in production cost into the price automatically</b>.")),
  list(title = "How it works", points = c(
    "<b>Base raw milk price = base cost + variable cost</b>.",
    "The base cost follows <b>Statistics Korea milk production cost</b>; the variable cost follows <b>consumer price inflation</b>.",
    "Processors buy each farm's <b>quota volume</b> at this price.")),
  list(title = "Who runs it", points = c(
    "The <b>Korea Dairy Committee</b> sets the price and collects the milk.",
    "The <b>Ministry of Agriculture, Food and Rural Affairs</b> designs the scheme.",
    "<b>Dairy processors</b> buy the raw milk.")),
  list(title = "Scale and change", points = c(
    "At the first application in August 2013 the national farm-gate price rose from <u>965.10 to 1,061.55 KRW/L</u>, <b>up 96.45 KRW in one month</b>.",
    "The annual average rose from <u>1,022.92 KRW/L in 2013 to 1,249.81 KRW/L in 2025</u>.")),
  list(title = "Limitation: a de facto price floor", points = c(
    "When costs rise, <b>the price cannot fall</b> even if demand falls.",
    "Under the <b>quota system</b> introduced in <u>2002</u>, processors must buy fixed volumes.",
    "This app therefore models the scheme as a <b>price floor</b>."))
)
POLICY_SOURCE_EN <- "Formula and background come from the government policy blog (Korea Policy Briefing, blog.naver.com/hellopolicy/220156198742); scale and change come from Korea Dairy Committee dairy statistics."

VALUE_CHAIN_EN <- data.frame(
  step = c("Dairy farms", "Collection", "Processors", "Retail & consumers"),
  note = c("Raw milk sellers (quota holders)", "Korea Dairy Committee, co-ops", "Raw milk buyers", "Pass-through only")
)

STRUCTURE_TBL_EN <- data.frame(
  Item = c("Market level", "Raw milk sellers", "Buyer concentration (processor CR4)",
           "Raw milk trade", "Binding border measures", "Storability", "Is perfect competition reasonable?"),
  Detail = c("Farm-gate raw milk (KRW/L, kt)",
             "Quota-holding farms; collection by the Korea Dairy Committee and co-ops",
             "To be entered",
             "Raw milk itself is hardly traded because it is perishable; imported as cheese, powder and other dairy products",
             "Dairy FTA tariff elimination schedule to be checked",
             "Raw milk keeps 2-3 days refrigerated; surplus must be turned into powder to be stored",
             "Check buyer-side oligopoly with CR4"),
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
