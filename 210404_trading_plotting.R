pacman::p_load("data.table", "lubridate", "TTR", "BBquantR", "ggplot2", "dplyr")

DT <- fread("data/upjong_trading_DT.txt", colClasses = c("character", "numeric", "character"))
upjong <- BBquantR::BBdata(DT = "upjong")

DT[, date := ymd(date)]
DT[, ma5 := TTR::runMean(trd_vl, n = 5), by = upjong_code]
DT[, ma10 := TTR::runMean(trd_vl, n = 10), by = upjong_code]


upjong_list <- c(201, 205, 230, 221, 42)

# upjong[upjong_code %in% upjong_list, ]

bs_plot <- ggplot(DT) + 
      geom_line(aes(x = date, 
                    y = ma10, 
                    group = upjong_code, 
                    colour = upjong_code))

bs_plot %+% subset(DT, upjong_code %in% upjong_list)
