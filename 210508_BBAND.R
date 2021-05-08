# 210508 
# 볼린저밴드를 사용
# 볼린저밴드는 MA(N) +/- k * SD(N)을 정함
# 여기서 K를, 시가총액에 연동시켜서 해보자
# 시가총액 또는 거래금액에 반비례하는 구조로 구성

pacman:: p_load('dplyr', 'BBquantR', 'data.table', 'lubridate', 'TTR', 'ggplot2')

DT <- BBquantR::BBdata(DT = 'daily')
cols <- c('init.prc', 'high.prc', 'low.prc', 'end.prc', 'quant', 'year')
DT[, paste0(cols, "") := lapply(.SD, as.numeric), .SDcols = cols]
DT[, date := lubridate::ymd(date)]
DT[, y := log(end.prc) - log(init.prc), by = code]

code_list <- unique(DT[, code])


DT_fs <- BBquantR::BBdata(DT = 'fs')
DT_fs <- DT_fs[year == 2019 , .(code,stock_quantity)]
DT_fs <- DT_fs[code %in% code_list, ]
DT_fs[, stock_quantity := as.numeric(stock_quantity)]

temp <- DT[DT_fs, on = .(code = code), allow.cartesian = TRUE]
temp[, sichong := end.prc * stock_quantity]
temp <- temp[quant != 0, ]
temp <- temp[order(date, code), ]
temp <- temp[, .SD[.N], by = code]


# 수식을 좀 정리해보고 재작업업
+