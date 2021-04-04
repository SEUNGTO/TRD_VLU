pacman::p_load("data.table", "lubridate", "TTR", "BBquantR")
org_path <- "C:/Users/Shin Seung Yeop/Documents/TRD_VLU"
path <- "D:/R/Data/stock_price_by_min"
setwd(path)

file_list <- dir()[-length(dir())]
# first, analyze upjong
result <- matrix(nrow = 0, ncol = 3)
result <- data.table(result)
names(result) <- c("code", "trd_vl","date")

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
      
      bs_DT[, "trd_vl" := price * quantity]
      
      temp <- bs_DT[, log(sum(trd_vl)), by = code]
      temp <- temp[, date := date]
      setnames(x = temp, old = "V1", new = "trd_vl" )
      result <- rbind(result, temp)
      cat("\r", date, "|", i, "                              ")
}


setwd(org_path)

fwrite(x = result, 
       file = "./data/[TRD_VLU]firm.txt")

