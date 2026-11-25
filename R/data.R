# =====================================================================
# 자료 파일: 기준선, 탄력성, 정책 설명, 시장 구조
# - 기준연도: 2013년 (원유가격 연동제 첫 적용 연도)
# - 출처: 낙농진흥회 낙농통계광장 국내통계 (원유생산통계, 원유수취가격)
#   https://dairy.or.kr/kor/sub05/menu_01_3_1.php  (원유생산량, 톤)
#   https://dairy.or.kr/kor/sub05/menu_01_3_3.php  (원유수취가격, 원/L, 전국)
#   2026-11-19 조회. 연 확정치는 차년도 2월 공표.
# - 값을 바꿀 때는 반드시 '출처'와 '상태'를 함께 고친다.
#   상태: "확인 완료" / "확인 필요" / "임시 값"(출처 확인 전 자리표시 값) / "가상"
# =====================================================================

# ---- 기준선 (모형 보정점) -------------------------------------------
# 기준연도 설정 방식 A: 연동제 도입 전후 분리
#   P* = 2013년 1-7월 평균 수취가격 (도입 전), 연동제 가격 = 8-12월 평균 (도입 후)
BASE <- list(
  P0             = 980.29,    # 도입 전 원유수취가격 (원/L)         [확인 완료]
  P_policy       = 1082.60,   # 도입 후 원유수취가격 (원/L)         [확인 완료]
  Q0             = 2093.073,  # 2013년 원유생산량 (천 톤)           [확인 완료]
  policy_default = 1083,      # 슬라이더 기본값 (도입 후 평균 반올림)
  Pr0            = 2400,      # 참고: 흰 우유 소매가격 (원/L)       [임시 값]
  year           = 2013
)

# 분석 대상: 농가 출하 단계의 원유(가공 전 우유) 시장. 원유 구매자는 유가공업체다.
BASELINE_TBL <- data.frame(
  항목 = c("균형가격 P* (도입 전 1-7월 평균)", "연동제 가격 Ps (도입 후 8-12월 평균)",
           "원유 생산량 Q*", "참고: 연평균 원유수취가격",
           "유업체 원유 구매량 (음용유용과 가공유용)", "잉여 원유 (분유 전환량)",
           "참고: 흰 우유 소매가격"),
  값   = c("980.29", "1,082.60", "2,093.1", "1,022.92", "미입력", "미입력", "2,400"),
  단위 = c("원/L", "원/L", "천 톤", "원/L", "천 톤", "천 톤", "원/L"),
  연도 = rep("2013", 7),
  출처 = c("낙농진흥회 원유수취가격", "낙농진흥회 원유수취가격", "낙농진흥회 원유생산통계",
           "낙농진흥회 원유수취가격", "낙농진흥회", "낙농진흥회", "KAMIS"),
  상태 = c("확인 완료", "확인 완료", "확인 완료", "확인 완료", "임시 값", "임시 값", "임시 값"),
  check.names = FALSE
)

# ---- 탄력성 ---------------------------------------------------------
ELAST_PRESET <- list(
  SR = list(eta_r = -0.30, eps_s = 0.20),
  LR = list(eta_r = -0.50, eps_s = 0.80)
)
ELAST_DEFAULT <- list(Sa = 0.45, sigma = 0.30)

# 원유 수요탄력성 ηa는 문헌에 소매 탄력성만 있을 때 파생수요 공식으로 계산한다.
ELAST_TBL <- data.frame(
  기호       = c("ηa (원유 수요, 유업체)", "ε (원유 공급, 낙농가)", "η (흰 우유 소매 수요)",
                 "Sa (소매가 중 원유 몫)", "σ (원유와 기타 투입 대체)"),
  단기       = c("Sa·η − Sb·σ", "0.20", "−0.30", "0.45", "0.30"),
  장기       = c("Sa·η − Sb·σ", "0.80", "−0.50", "0.45", "0.30"),
  `타당 범위` = c("계산값", "0.1 ~ 1.2", "−0.8 ~ −0.2", "0.35 ~ 0.55", "0 ~ 0.5"),
  출처       = c("파생수요 공식", "미입력", "미입력", "미입력", "미입력"),
  `시장 수준` = c("원유 (농가 출하)", "원유 (농가 출하)", "소매", "원유/소매", "가공"),
  상태       = c("임시 값", "임시 값", "임시 값", "임시 값", "임시 값"),
  check.names = FALSE
)

# ---- 가계 영향 (소득계층별, 파급 효과) -----------------------------------
HOUSEHOLD <- data.frame(
  group        = c("소득 1분위", "소득 5분위"),
  budget_share = c(0.010, 0.004),      # 우유 지출 비중 (2013)       [임시 값]
  monthly_exp  = c(1200000, 5000000)   # 월 소비지출 (원, 2013)       [임시 값]
)

# ---- 정책 설명 (Part 1) ---------------------------------------------
# icon: fontawesome 아이콘 이름
# 글쓰기 기준
#   kv     = 항목: 값. 정의, 사실, 수치, 주체처럼 이름 하나에 값 하나가 대응할 때. 값은 명사구로 쓴다.
#   points = 문장. 이유, 목적, 한계, 해석처럼 설명이 필요할 때. 합니다체로 쓴다.
# 각 항목은 c(한국어, 영어). kv는 c(항목, 값, 영어 항목, 영어 값). <b>핵심어</b>, <u>숫자</u> 사용 가능
POLICY_INFO <- list(
  list(bs = "bullseye", title = c("목표", "Goal"),
       points = list(
         c("<b>낙농가 소득</b>과 <b>원유 수급</b>을 안정시키는 것이 목표입니다.",
           "The goal is to stabilise <b>dairy farm income</b> and the <b>raw milk supply</b>."),
         c("예전에는 생산자와 유업체가 <u>3~5년마다</u> 협상해 가격을 정했고, 협상 때마다 갈등이 생겼습니다.",
           "Producers and processors used to negotiate the price <u>every 3 to 5 years</u>, and each round caused conflict."),
         c("그래서 <b>생산비 변동을 가격에 자동으로 반영</b>하도록 바꿨습니다.",
           "The scheme now <b>passes changes in production cost into the price automatically</b>."))),
  list(bs = "gear", title = c("작동 방식", "How it works"),
       kv = list(
         c("산식", "원유기본가격 = 기준원가 + 변동원가", "Formula", "Base price = base cost + variable cost"),
         c("기준원가", "통계청 우유생산비 증감 반영", "Base cost", "Follows Statistics Korea milk production cost"),
         c("변동원가", "소비자물가 상승률 반영", "Variable cost", "Follows consumer price inflation"),
         c("매입 물량", "농가별 쿼터 물량", "Volume bought", "Each farm's quota volume"))),
  list(bs = "bank", title = c("운영 주체", "Who runs it"),
       kv = list(
         c("낙농진흥회", "가격 결정, 집유", "Korea Dairy Committee", "Sets the price, collects milk"),
         c("농림축산식품부", "제도 설계", "Ministry of Agriculture", "Designs the scheme"),
         c("유가공업체", "원유 매입", "Dairy processors", "Buy raw milk"))),
  list(bs = "rulers", title = c("규모와 변화", "Scale and change"),
       kv = list(
         c("첫 적용", "2013년 8월", "First applied", "August 2013"),
         c("한 달 인상", "965.10원 → 1,061.55원 (+96.45원/L)", "One-month rise", "965.10 → 1,061.55 KRW/L (+96.45)"),
         c("연평균 가격", "1,022.92원(2013) → 1,249.81원(2025)", "Annual average", "1,022.92 (2013) → 1,249.81 KRW/L (2025)"))),
  list(bs = "exclamation-triangle", title = c("한계: 사실상의 가격하한", "Limitation: a de facto price floor"),
       points = list(
         c("수요가 줄어도 생산비가 오르면 <b>가격을 낮출 수 없습니다</b>.",
           "When costs rise, <b>the price cannot fall</b> even if demand falls."),
         c("<u>2002년</u>에 도입된 <b>쿼터제</b> 때문에 유업체는 정해진 물량을 의무적으로 사야 합니다.",
           "Under the <b>quota system</b> introduced in <u>2002</u>, processors must buy fixed volumes."),
         c("그래서 이 앱은 연동제를 <b>가격하한</b>으로 모형화합니다.",
           "This app therefore models the scheme as a <b>price floor</b>.")))
)
POLICY_SOURCE <- "산출 공식과 도입 배경은 정부 정책 블로그(정책브리핑, blog.naver.com/hellopolicy/220156198742), 규모와 변화는 낙농진흥회 낙농통계광장 자료입니다."

# ---- 시장 구조 (Part 1) ---------------------------------------------
# scope: "policy" 정책 작용 단계, "market" 원유 구매자, "spill" 파급 효과만 반영
VALUE_CHAIN <- data.frame(
  step  = c("낙농가", "집유", "유가공업체", "유통과 소비자"),
  note  = c("원유 판매자 (쿼터 보유)", "낙농진흥회, 조합", "원유 구매자", "가계 파급만 반영"),
  icon  = c("cow", "truck-droplet", "industry", "cart-shopping"),
  li    = c("cow", "truck", "factory", "cart"),
  art   = c("chain-farm", "chain-collect", "chain-processor", "chain-consumer"),  # www/il-*.png
  scope = c("policy", "policy", "market", "spill")
)

STRUCTURE_TBL <- data.frame(
  항목 = c("분석 시장 수준", "원유 판매자", "원유 구매자 집중도 (유업체 CR4)",
           "원유 무역", "국경 조치 구속 여부", "저장성", "경쟁 가정의 타당성"),
  내용 = c("농가 출하 단계 원유 (원/L, 천 톤)",
           "쿼터를 가진 낙농가 (집유: 낙농진흥회, 조합)",
           "미입력",
           "원유는 쉽게 상해서 거의 교역되지 않습니다. 치즈, 분유 같은 유제품 형태로 수입됩니다.",
           "유제품 FTA 관세 철폐 일정을 확인해야 합니다.",
           "냉장 2~3일 (장기 저장은 분유로 전환해야 가능)",
           "구매 측 과점 여부를 CR4로 점검해야 합니다."),
  상태 = c("확인 필요", "확인 필요", "임시 값", "확인 필요", "확인 필요", "확인 필요", "임시 값"),
  check.names = FALSE
)

# ---- 시장 추이 (Part 1 차트) : 낙농진흥회, 확인 완료 ------------------------
TREND <- data.frame(
  year  = 2013:2025,
  price = c(1022.92, 1088.06, 1085.97, 1082.49, 1075.06, 1075.59, 1081.37,
            1082.66, 1093.85, 1115.62, 1180.97, 1245.64, 1249.81),   # 연평균 수취가격 (원/L)
  qty   = c(2093.1, 2214.0, 2168.2, 2069.6, 2058.2, 2040.8, 2049.4,
            2088.8, 2034.4, 1975.4, 1929.9, 1942.3, 1951.0)          # 원유 생산량 (천 톤)
)
TREND_EVENTS <- data.frame(
  year  = c(2013, 2023),
  label = c("연동제 첫 적용 (2013.8)", "차등가격제 개편 (확인 전)")
)

# ---- 2013년 월별 원유수취가격 (기준연도 근거 차트) : 낙농진흥회, 확인 완료 ----------
MONTHLY_2013 <- data.frame(
  month = 1:12,
  price = c(989.30, 988.29, 986.53, 984.02, 977.22, 971.58, 965.10,
            1061.55, 1071.35, 1087.08, 1095.73, 1097.30)
)

# ---- 시나리오 (사이드바 드롭다운) ----------------------------------------
SCENARIOS <- data.frame(
  id     = c("custom", "base", "pre", "up50", "up150"),
  name   = c("직접 설정", "2013년 연동제 도입 (8-12월 평균)", "도입 전 수준 (정책 없음)",
             "도입 후 +50원 추가 인상 (가상)", "도입 후 +150원, 장기 (가상)"),
  price  = c(NA, 1083, 980, 1133, 1233),
  run    = c(NA, "SR", "SR", "SR", "LR"),
  source = c("-", "낙농진흥회 원유수취가격", "낙농진흥회 원유수취가격", "가상 시나리오", "가상 시나리오"),
  상태   = c("-", "확인 완료", "확인 완료", "가상", "가상"),
  check.names = FALSE
)

# ---- 모형 가정 (주제별 묶음. 사실은 kv, 이유와 한계는 문장) ------------------------
ASSUMPTION_GROUPS <- list(
  list(title = c("시장과 곡선", "Market and curves"),
       kv = list(
         c("분석 대상", "농가 출하 단계의 원유 시장", "Market", "Farm-gate raw milk market"),
         c("판매자", "낙농가", "Sellers", "Dairy farms"),
         c("구매자", "유가공업체", "Buyers", "Dairy processors"),
         c("시장 형태", "국내 폐쇄 시장", "Market type", "Closed domestic market"),
         c("곡선 형태", "기준점 근방에서 선형", "Curves", "Linear near the base point"),
         c("수량 환산", "원유 1톤 ≈ 1,000L", "Unit conversion", "1 tonne of raw milk ≈ 1,000 L")),
       points = list(
         c("원유는 쉽게 상해서 국제 교역이 거의 없기 때문에 폐쇄 시장으로 봅니다.",
           "Raw milk spoils quickly and is hardly traded internationally, so the market is treated as closed."))),
  list(title = c("기준연도와 가격", "Base year and prices"),
       kv = list(
         c("기준연도", "2013년 (연동제 첫 적용)", "Base year", "2013 (first year of the scheme)"),
         c("균형가격 P*", "도입 전(1-7월) 평균 원유수취가격", "Equilibrium price P*", "Average farm-gate price before the scheme (Jan-Jul)"),
         c("연동제 가격 Ps", "도입 후(8-12월) 평균 원유수취가격", "Linked price Ps", "Average farm-gate price after the scheme (Aug-Dec)"),
         c("가격 자료", "낙농진흥회 원유수취가격 (기본가격 + 성분과 위생 인센티브)", "Price data", "KDC farm-gate price (base price + component and hygiene incentives)")),
       points = list(
         c("도입 전에도 쿼터와 협상가격이 있었으므로, 도입 전 가격이 완전한 시장균형은 아닙니다.",
           "Quotas and negotiated prices existed before the scheme, so the pre-scheme price is not a pure market equilibrium."),
         c("원유수취가격은 정책이 정하는 기본가격과 차이가 있습니다.",
           "The farm-gate price differs from the policy base price."))),
  list(title = c("가격하한과 비용", "Price floor and costs"),
       kv = list(
         c("가격", "낙농가 수취가격 = 유업체 구매가격 = Ps", "Price", "Farm price = processor price = Ps"),
         c("수량 쐐기", "남는 원유 Qs − Qd", "Quantity wedge", "Surplus Qs − Qd"),
         c("남는 원유 처리", "매입 후 분유로 전환, 처분가치 Pv 회수", "Surplus handling", "Bought and turned into powder, salvage value Pv"),
         c("비용 부담", "정부(기본, 강의 모형) 또는 유업체(쿼터 의무 매입)", "Who pays", "Government (default, lecture model) or processors (quota purchase)")),
       points = list(
         c("연동제 가격은 수요가 줄어도 내려가지 않으므로 가격하한(wedge)으로 모형화합니다.",
           "The linked price does not fall when demand falls, so it is modelled as a price floor (wedge)."),
         c("비용을 누가 내는지는 분배만 바꾸고, 사회 전체 후생 변화는 같습니다.",
           "Who pays changes only the distribution; the total welfare change is the same."))),
  list(title = c("기간과 가계", "Horizon and households"),
       kv = list(
         c("단기와 장기", "원유 공급탄력성 ε와 소매 수요탄력성 η의 차이로만 구분", "Short vs. long run", "Differ only in supply elasticity ε and retail demand elasticity η"),
         c("가계 부담", "원유가 상승분 × Sa만큼 흰 우유 소매가에 전가 (근사)", "Household burden", "Raw milk price rise × Sa passed into white milk retail price (approximation)")),
       points = list(
         c("가계 부담 계산에는 탄력성을 쓰지 않으므로 단기와 장기 결과가 같습니다.",
           "Elasticities are not used for the household burden, so short- and long-run results are the same.")))
)

# ---- 분석 개요 (방법론 > 분석 개요) ----------------------------------------
ABOUT_KV <- list(
  c("과목", "식품가격분석론 (LESE412)", "Course", "Food Price Analysis (LESE412)"),
  c("팀", "Group 4", "Team", "Group 4"),
  c("팀원", "정진욱, 최재령, 이수현, 배현민", "Members", "정진욱, 최재령, 이수현, 배현민"),
  c("사용자", "농림축산식품부 낙농 정책 담당 분석가", "User", "Dairy policy analyst, Ministry of Agriculture"),
  c("분석 시장", "농가 출하 단계의 원유 시장 (원/L, 천 톤)", "Market", "Farm-gate raw milk market (KRW/L, kt)"),
  c("기준연도", "2013년", "Base year", "2013")
)
ABOUT_POINTS <- list(
  c("연동제로 원유가격이 오르면 낙농가 소득 보전(ΔPS)에 비해 유가공업체와 소비자의 손실, 잉여 원유 처리 비용은 단기와 장기에 각각 얼마이며, 저소득층과 고소득층 가계의 실질 부담은 어느 수준인지 분석합니다.",
    "When the scheme raises the raw milk price, we analyse how large processor and consumer losses and surplus handling costs are relative to farm income support (ΔPS) in the short and long run, and the real burden on low- and high-income households."),
  c("2013년은 연동제가 처음 적용된 해라서 도입 전(1-7월)과 도입 후(8-12월) 가격을 모두 관측할 수 있습니다.",
    "2013 is the first year of the scheme, so prices both before (Jan-Jul) and after (Aug-Dec) are observed.")
)

# ---- 방법론 페이지 출처 목록 -------------------------------------------
SOURCES_TBL <- data.frame(
  자료 = c("원유 생산량 (월, 연)", "원유수취가격 (전국, 월)", "우유 생산비", "흰 우유 소매가격",
           "유제품 수입", "탄력성", "정책 규모와 예산"),
  기관 = c("낙농진흥회 원유생산통계", "낙농진흥회 원유수취가격", "KOSIS 축산물생산비조사", "KAMIS",
           "관세청 수출입무역통계", "KREI 연구보고서", "농림축산식품부, 낙농진흥회"),
  상태 = c("확인 완료", "확인 완료", "확인 필요", "확인 필요", "확인 필요", "확인 필요", "확인 필요"),
  check.names = FALSE
)

# ---- 모형 검증 예제 (방법론 페이지) ------------------------------------
# 강의 L11 Supply Policy 6쪽 "Support price with government purchases"의 면적 공식:
#   ΔCS = −(a + b),  ΔPS = a + b + c,  G = b + c + d + e,  Net = −(b + d + e)
#   재판매 가치 v가 있으면 G = (Ps − v)(Qs − Qd)
# 슬라이드에는 숫자 예제가 없으므로 교과서형 숫자를 넣어 면적을 손으로 계산하고 앱과 대조한다.
#   Qd = 100 − P, Qs = P → P* = 50, Q* = 50 (|η| = ε = 1), Ps = 60
#   Qd = 40, Qs = 60, 잉여 20
#   a + b = (60−50)(50+40)/2 = 450   (소비자 → 생산자 이전, 앱의 A)
#   c     = (60−50)(60−40)/2 = 100   (새 생산자 이득, 앱의 B)
#   ΔCS = −450, ΔPS = 550, G = 60 × 20 = 1200, b + d + e = 1200 − 100 = 1100 (DWL, 앱의 D)
#   재판매 v = 20: G = (60 − 20) × 20 = 800, DWL = 800 − 100 = 700
VALIDATION_CASE <- list(
  label = "강의 L11 6쪽 가격지지 면적 공식 + 교과서형 수치 (Qd = 100 − P, Qs = P, Ps = 60)",
  P0 = 50, Q0 = 50, eta = -1, eps = 1, Ps = 60, Pv = 0, Pv_resale = 20,
  expected = list(Qd = 40, Qs = 60, stock = 20, A = 450, B = 100, dCS = -450, dPS = 550,
                  G = 1200, DWL = 1100, G_resale = 800, DWL_resale = 700),
  status = "강의 공식 대조 완료 (슬라이드에 숫자 예제 없음)"
)

# ---- 잉여 원유 처리 설정 ---------------------------------------------
SURPLUS_DEFAULT <- list(Pv = 0, payer = "gov")
PAYER_CHOICES <- c("정부 매입" = "gov", "유업체 매입" = "proc")
