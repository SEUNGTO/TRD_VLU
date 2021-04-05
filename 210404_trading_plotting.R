pacman::p_load("data.table", "lubridate", "TTR", "BBquantR", "ggplot2", "dplyr")

DT <- fread("data/[TRD_VLU]upjong.txt", colClasses = c("character", "numeric", "character"))

DT[, date := ymd(date)]
DT[, ma5 := TTR::runMean(trd_vl, n = 5), by = upjong_code]
DT[, ma10 := TTR::runMean(trd_vl, n = 10), by = upjong_code]
DT[, ma20 := TTR::runMean(trd_vl, n = 20), by = upjong_code]


upjong <- BBdata(DT = "upjong")
upjong_list <- c(201, 205, 230, 221, 42)

# upjong[upjong_code %in% upjong_list, ]

bs_plot <- ggplot(DT, aes(x = date)) + 
      geom_line(aes(y = ma10, 
                    group = upjong_code, 
                    colour = upjong_code)) +
      geom_line(aes(y = ma20, 
                    group = upjong_code, 
                    colour = upjong_code)) +
      geom_vline(aes(xintercept = ymd("2020-07-01")), lty = "dashed", color = "Black") +
      geom_vline(aes(xintercept = ymd("2020-10-01")), lty = "dashed", color = "Black") +
      geom_vline(aes(xintercept = ymd("2021-01-01")), lty = "dashed", color = "Black") +
      geom_vline(aes(xintercept = ymd("2021-04-01")), lty = "dashed", color = "Black") 


bs_plot %+% subset(DT, upjong_code %in% upjong_list)
