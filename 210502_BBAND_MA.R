pacman:: p_load('dplyr', 'BBquantR', 'data.table', 'lubridate', 'TTR', 'ggplot2')

# 2021년 5월 2일 / 프로젝트 1일차
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
DT[, sd20 := TTR::runSD(end.prc, n = 20), by = code]
DT[, ma5 := TTR::runMean(end.prc, n = 20), by = code]

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
