pacman:: p_load('dplyr', 'BBquantR', 'data.table', 'lubridate', 'TTR')

# project day 1 (210502)
# use bollinger band
# if the range of stock prices are in the bollinger band,
# it is interpreted as a signal of an increase 
# but, till the day when the signal occurs,
# the prices should be lower than the upper band
# 가격이 볼린저밴드에 닿으면, 가격상승/하락 신호로 이해한다
# if the price touch upper bollinger band,
# that is interpreted as the signal of sudden fluctuation of stock price
# 단, 신호가 오기 전까지는 계속 볼린저밴드 안에 있어야 한다.
# but, the price should be lower/higher than upper/lower bollinger band till the signal

DT <- BBquantR::BBdata(DT = 'daily')

cols <- c('init.prc', 'high.prc', 'low.prc', 'end.prc', 'quant', 'year')
DT[, paste0(cols, "") := lapply(.SD, as.numeric), .SDcols = cols]
DT[, date := lubridate::ymd(date)]
DT[, y := log(end.prc) - log(init.prc), by = code]


code_list <- DT[, .N, by = code][N >= 100, code]
DT <- DT[code %in% code_list, ]


DT[, ma := TTR::runMean(log(end.prc), n = n), by = code]
