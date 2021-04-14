a <- Sys.time()
library(data.table)
library(geosphere)

DT <- fread('rawdata2.csv', encoding = "UTF-8")
col_name <- c("id", "firms", "nm", "adrs", "tel", "org_blck_id", 
              "giro_c", "ltno_c", "bld_sqno", "ltno_adr", "rodnm_adr", 
              "xcdn", "ycdn", "mktn_blck_id", "addg_c", "nat_base_dtctc", "bas_ym")
names(DT) <- col_name

nh_DT <- DT[firms == "농협", .(id, firms, nm, xcdn, ycdn)]
nh_DT[, compt := 9999]
nh_DT[, compt2 := 9999]

oth_DT <- DT[firms != "농협", .(id, firms, nm, xcdn, ycdn)]
oth_DT2 <- oth_DT[firms != "NH농협은행", .(id, firms, nm, xcdn, ycdn)]


for (i in 1:nrow(nh_DT)){
      bs_x <- nh_DT[i, .(xcdn, ycdn)]
      oth_DT[, dist := distGeo(bs_x, oth_DT[, .(xcdn, ycdn)])]
      oth_DT[, ind := ifelse(dist <= 400, 1, 0)]
      nh_DT[i, 6] <- sum(oth_DT[, ind])
      cat("\r", i,  "|", nh_DT[i, id], "|", nh_DT[i, nm], "                  ")
      
}


for (i in 1:nrow(nh_DT)){
      bs_x <- nh_DT[i, .(xcdn, ycdn)]
      oth_DT2[, dist := distGeo(bs_x, oth_DT2[, .(xcdn, ycdn)])]
      oth_DT2[, ind := ifelse(dist <= 400, 1, 0)]
      nh_DT[i, 7] <- sum(oth_DT2[, ind])
      cat("\r", i,  "|", nh_DT[i, id], "|", nh_DT[i, nm], "                  ")

}


b <- Sys.time()
c <- difftime(b,a, units = "mins")
print(c)
