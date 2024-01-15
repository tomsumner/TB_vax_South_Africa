
rm(list = ls())

library(dplyr)
library(data.table)
library(fst)

df <- fread("C:/Users/ChristinahMukandavir/Documents/GitHub/tbvax/countries/ZAF/output/2021-06-28-1559_hivt1ntb_1_ZAFwave4.csv")
#vx <- fread("concat_files/vx_scenarios.csv")

# countrycode <- "ZAF"
# cc <- countrycode
# params <- dt5
# params_uid <- dt5$uid
# vx_chars <- vx[1,]
# vx_inc <- NULL
# 
# df1 <- df[]

#library(data.table)
dt <- data.table(df)
drop.cols <- grep(pattern = "HIV", x=colnames(dt))
const <- c("alphaH", "betaHA15", "betaHA0", "gammaH", "piH")


dt2 <- dt[, (drop.cols) := NULL]

reqcol <- c("lambdaH", "muH1", "muH2", "alphaHmul2010", "alphaHmul2019",
            "epsilonHmul2010",  "epsilonHmul2019", "phiHmul2010","phiHmul2019", "nhit", "uid")

dt3 <- dt2[,(const) := NULL]

dt4 <- dt3[order(dt3$nhits,decreasing=TRUE),]
n_points=5

dt5 <- dt4[1:n_points,]

write_fst(dt5,  "C:/Users/ChristinahMukandavir/Documents/GitHub/tbvax/concat_files/ZAF_paramsets.csv")

d <- write.csv(dt5, "concat_files/ZAF_paramsets.csv", sep = " ", row.names = FALSE)

