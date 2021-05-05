pacman:: p_load('dplyr', 'BBquantR', 'data.table', 'lubridate', 'TTR', 'ggplot2')

# 2021년 5월 2일 / 프로젝트 1일차 ----
# 볼린저밴드를 활용한 종목 예측
# 가격이 볼린저밴드에 닿으면, 가격상승/하락 신호로 이해한다
# 단, 신호가 오기 전까지는 계속 볼린저밴드 안에 있어야 한다.
# 또한 장기추세가 상승세로 돌아설 때 산다.

# 데이터 전처리 (data type : character -> numeric)
DT <- BBquantR::BBdata(DT = 'daily')
cols <- c('init.prc', 'high.prc', 'low.prc', 'end.prc', 'quant', 'year')
DT[, paste0(cols, "") := lapply(.SD, as.numeric), .SDcols = cols]
DT[, date := lubridate::ymd(date)]

DT[, y := log(end.prc) - log(init.prc), by = code]


# 100일 미만 종목은 테스트에서 제외
code_list1 <- DT[, .N, by = code][N >= 300, code]
code_list2 <- DT[, sum(quant > 0, na.rm = TRUE), by = code][, code]
code_list <- intersect(code_list1, code_list2)

DT <- DT[code %in% code_list, ]

DT[, ma120 := TTR::runMean(end.prc, n = 120), by = code]
DT[, ma60 := TTR::runMean(end.prc, n = 60), by = code]
DT[, ma20 := TTR::runMean(end.prc, n = 20), by = code]
DT[, ma5 := TTR::runMean(end.prc, n = 20), by = code]

DT[, sd120 := TTR::runSD(end.prc, n = 120), by = code]
DT[, sd60 := TTR::runSD(end.prc, n = 60), by = code]
DT[, sd20 := TTR::runSD(end.prc, n = 20), by = code]
DT[, sd5 := TTR::runSD(end.prc, n = 5), by = code]

DT[, ind1 := ifelse(ma20 > ma120, 1, 0), by = code]
DT[, ind2 := ifelse(ma20 > ma60, 1, 0), by = code]
DT[, ind3 := ifelse(ma60 > ma120, 1, 0), by = code]
DT[, ind4 := ifelse(lag(ind1) == 0 & ind1 == 1, 1, 0), by = code]

DT[, ind_ma := ind1 * ind2 * ind3 * ind4, by = code]

DT[, upper := ma20 + 2 * sd20, by = code]
DT[, ind_BB := ifelse(init.prc < upper & upper < end.prc, 1, 0), by = code]

DT[, ind_stg := ind_BB * ind_ma, by = code]

DT[, lead_y := lead(y, n = 1), by = code]
DT[ind_stg == 1, mean(lead_y)]

p_dt <- DT[ind_stg == 1, mean(lead_y), by = date]
p_dt %>% ggplot() + 
      geom_line(aes(x = date, y = V1))



# 2021년 5월 5일 / 프로젝트 2일차 ----
# 기본적인 분석으로 돌아가보자
# 당일 5% 이상 상승한 경우의 최근 추세가 어땠는지를 봐보자.

# 1. 데이터 불러오기 
DT <- BBquantR::BBdata(DT = 'daily')
cols <- c('init.prc', 'high.prc', 'low.prc', 'end.prc', 'quant', 'year')
DT[, paste0(cols, "") := lapply(.SD, as.numeric), .SDcols = cols]
DT[, date := lubridate::ymd(date)]
DT[, y := log(end.prc) - log(init.prc), by = code]
DT <- DT[quant != 0, ]

# 기간동안 거래일이 300일이 안되는 종목은 제외
code_list1 <- DT[, .N, by = code][N >= 300, code]
code_list2 <- DT[, sum(quant > 0, na.rm = TRUE), by = code][, code]
code_list <- intersect(code_list1, code_list2)

DT <- DT[code %in% code_list, ]

# 2. 당일 5% 이상 상승한 종목
DT[ , signal_3per := ifelse(y > 0.03, 1, 0), by = code]
DT_3per <- DT[, sum(signal_3per), by = code][order(V1, decreasing = TRUE), ] # 5% 이상 상승한 경우
DT_3per[V1 > 30, ] # 30회 이상 상승한 기업 : 1943개

# 3. 상승 이전의 신호들 찾기 : 추세선간의 비율, 추세선의 방향
# 장기추세선 : 120일 / 중기추세선 : 60일 / 단기추세선 : 20일 / 초단기 추세선 : 5일
DT[, ma120 := TTR::runMean(end.prc, n = 120), by = code]
DT[, ma60 := TTR::runMean(end.prc, n = 60), by = code]
DT[, ma20 := TTR::runMean(end.prc, n = 20), by = code]
DT[, ma5 := TTR::runMean(end.prc, n = 5), by = code]

# 추세선 방향 (상승 : 1, 하락/동일 : 0 )
DT[, ma120_dirc := c(NA, ifelse(diff(ma120) > 0, 1, 0)), by = code]
DT[, ma60_dirc := c(NA, ifelse(diff(ma60) > 0, 1, 0)), by = code]
DT[, ma20_dirc := c(NA, ifelse(diff(ma20) > 0, 1, 0)), by = code]
DT[, ma5_dirc := c(NA, ifelse(diff(ma5) > 0, 1, 0)), by = code]


# 추세선간 비율
DT[, ratio_120_60 := ma60/ma120, by = code]
DT[, ratio_60_20 := ma20/ma60, by = code]
DT[, ratio_20_5 := ma5/ma20, by = code]


# 최근 10일 방향
DT[, avg_ma120_dirc := TTR::runMean(ma120_dirc, n = 10), by = code]
DT[, avg_ma60_dirc := TTR::runMean(ma60_dirc, n = 10), by = code]
DT[, avg_ma20_dirc := TTR::runMean(ma20_dirc, n = 10), by = code]
DT[, avg_ma5_dirc := TTR::runMean(ma5_dirc, n = 10), by = code]

# 최근 10일 비율 / 소수점 둘째자리까지 표기
DT[, avg_ratio_120_60 := round(TTR::runMean(ratio_60_20, n = 10), 2), by = code]
DT[, avg_ratio_60_20 := round(TTR::runMean(ratio_60_20, n = 10), 2), by = code]
DT[, avg_ratio_20_5 := round(TTR::runMean(ratio_20_5, n = 10), 2), by = code]

# 상승 하루 전 확인
DT[, target := lead(signal_3per), by = code]

# CODE : 302550로 테스트
result <-  DT[code == '302550', ]

result[, .N, by = avg_ratio_20_5][order(N, decreasing = TRUE), ]
result[, .N, by = avg_ratio_60_20][order(N, decreasing = TRUE), ]
result[, .N, by = avg_ratio_120_60][order(N, decreasing = TRUE), ]

# 가장 자주 있던 값 선택
# 60 : 120 에서는 0.95 , 0.98
# 20 : 60 에서는 0.95 , 0.98
# 5 : 20 에서는 0.99, 1.00

DT[code == '302550' & 
         (avg_ratio_120_60 == 0.95 | avg_ratio_120_60 == 0.98) & 
         (avg_ratio_60_20 == 0.95 | avg_ratio_60_20 == 0.98) & 
         (avg_ratio_20_5 == 0.99 | avg_ratio_20_5 == 1.00), 
   mean(y)] # 0.03175362


# 가장 많은 빈도수를 골라보자!