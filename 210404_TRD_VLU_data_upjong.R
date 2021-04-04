pacman::p_load("data.table", "lubridate", "TTR", "BBquantR", "ggplot2")

path <- "D:/R/Data/stock_price_by_min"
setwd(path)

file_list <- dir()[-length(dir())]
upjong <- BBquantR::BBdata(DT = "upjong")
upjong_code <- upjong[, .(code, upjong_code)]

# first, analyze upjong
result <- matrix(nrow = 0, ncol = 3)
result <- data.table(result)
names(result) <- c("upjong_code", "trd_vl","date")


for(i in 1:length(file_list)){
      file_nm <- file_list[i]
      bs_DT <- fread(file_nm, colClasses = "character")
      
      if (dim(bs_DT)[2] == 5){
            cols = c("price", "quantity")
            bs_DT[, paste0(cols, "") := lapply(.SD, as.numeric), .SDcols = cols]
            
      }
      
      if (dim(bs_DT)[2] == 6){
            cols = c("price", "quantity", "cum_quantity")
            bs_DT[, paste0(cols, "") := lapply(.SD, as.numeric), .SDcols = cols]
            
      }
      
      date <- stringr::str_sub(file_nm, end = 10)
      bs_DT[, date := date]
      
      
      bs_DT <- bs_DT[upjong_code, on = .(code = code)]
      bs_DT <- na.omit(bs_DT)
      bs_DT[, "trd_vl" := price * quantity]
      temp <- bs_DT[, log(sum(trd_vl)), by = upjong_code]
      temp <- temp[, date := date]
      setnames(x = temp, old = "V1", new = "trd_vl" )
      result <- rbind(result, temp)
      cat("\r", date, "|", i, "                              ")
}

fwrite(x = result, 
       file = "C:/Users/Shin Seung Yeop/Documents/[R]TradeBot/data/upjong_trading_value.txt")
