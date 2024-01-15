library(ggplot2)
library(reshape2)
library(fst)
library(data.table)
library(dplyr)

pathtofiles <- "C:/Users/ChristinahMukandavir/Documents/GitHub/tbvax/countries-examples/ZAF/logs/"

files <- list.files(path = pathtofiles, pattern = ".csv$")
#f1 <- fread("C:/Users/ChristinahMukandavir/Documents/GitHub/tbvax/countries-examples/ZAF/logs/[ZAF][2022-03-08_10h33m57s][f65c8e02d8e8b35d07cefda07eaaf768][3_2_7_2][XMLinput_epi_art_scaleup_2024][1][100][optim]_params.csv")
par_set <- list()
for(i in 1:length(files)){
  f1 <- fread(paste0(pathtofiles,files[i]))
  f2 <- f1 %>%
    filter(choose == "TRUE") %>%
    select(unique.name,mean)
  f3 <- data.table(t(f2[,2]))
  
  par_set[[i]] <- f3
}
cn <- (t(f2[,1]))
parset <- rbindlist(par_set)
colnames(parset) <- cn[1,]

parset[ , order(names(parset))]
test <- parset %>% 
  select(sort(names(.)))


write.csv(test, "output.csv",sep = " ",row.names = FALSE)