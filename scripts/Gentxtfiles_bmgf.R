rm(list=ls())
library(dplyr)
library(here)
library(data.table)
library(digest)
library(log4r)
library(fst)
library(git2r)
library(readr)

#BCG vac

#file names
fn <- c("BCG_scaleup_med_never_vac1.txt",
        "BCG_scaleup_med_vac1_vac2.txt",
        "BCG_scaleup_med_vac2_vac3.txt",
        "BCG_scaleup_med_never_prev1.txt",
        "BCG_scaleup_med_prev1_prev2.txt",
        "BCG_scaleup_med_prev2_prev3.txt",
        "BCG_scaleup_med_waned_prev1.txt",
        "BCG_scaleup_med_waned_vac1.txt"
        )

ca_80 <- c(0.1744,0.1443, 0.1198, 0.0996, 0.083)
year_scaleup <- 2025:2029
yr_vax <- 2025:2050
yr2 <- 2035
yr3 <- 2045

infec <- "Lf, Ls, L0, R, Ds"

#ca_ages <- c("11","12","13", "14", "15", "16", "17", "18")
ca_ages <- c(`11`,"12","13", "14", "15", "16", "17", "18")
colnum <- 19:26
  
  #routvac <- 0.8
  f <- fread("countries/ZAF/data/BCG_ado_vac_template.txt",header=TRUE)
  f <- data.table(f)
  
  #never to vax 1
  #for(i in 1:nrow(vx_char)){
 
  f1 <- f %>%
    filter(YEAR < 2025) 
  
  f2 <- f %>%
    filter(YEAR %in% yr_vax)
  
  #to generate scaleup vac
  
  nr <- nrow(f2)-4
  routvac <- c(-log(1-0.39), -log(1-0.5), -log(1-0.6), -log(1-0.7), rep(-log(1-0.8),nr))
  routvac <- round(routvac, 3)
  
  f2$`10` <- routvac
  
  
   for (i in 19:26) {
    
    if( i > 0 ) {
     #f2$paste0(ca_ages[i]) <- ifelse(f2$YEAR %in% year1,ca_80,0)
     f2[,i] <- ifelse(f2$YEAR %in% year_scaleup,ca_80,0)
     #f2[,i] <- ifelse(f2$YEAR %in% c(yr2,yr3),routvac[5],f2$YEAR)
   } 
   }
 
  f3 <- f2 %>%
    mutate(`11` = ifelse(f2$YEAR %in% c(yr2,yr3),routvac[5],`11`)) %>%
    mutate(`12` = ifelse(f2$YEAR %in% c(yr2,yr3),routvac[5],`12`)) %>%
    mutate(`13` = ifelse(f2$YEAR %in% c(yr2,yr3),routvac[5],`13`)) %>%
    mutate(`14` = ifelse(f2$YEAR %in% c(yr2,yr3),routvac[5],`14`)) %>%
    mutate(`15` = ifelse(f2$YEAR %in% c(yr2,yr3),routvac[5],`15`)) %>%
    mutate(`16` = ifelse(f2$YEAR %in% c(yr2,yr3),routvac[5],`16`)) %>%
    mutate(`17` = ifelse(f2$YEAR %in% c(yr2,yr3),routvac[5],`17`)) %>%
    mutate(`18` = ifelse(f2$YEAR %in% c(yr2,yr3),routvac[5],`18`)) 
    
  fnv1 <- rbind(f1,f3)
  
  write_tsv(fnv1,  paste0("countries/ZAF/data/BCGvx/",fn[1]))
  
  ########vac1 to vac2
  
  fv12 <- f %>%
    mutate(`11` = ifelse(YEAR %in% c(yr2,yr3),routvac[5],`11`)) %>%
    mutate(`12` = ifelse(YEAR %in% c(yr2,yr3),routvac[5],`12`)) %>%
    mutate(`13` = ifelse(YEAR %in% c(yr2,yr3),routvac[5],`13`)) %>%
    mutate(`14` = ifelse(YEAR %in% c(yr2,yr3),routvac[5],`14`)) %>%
    mutate(`15` = ifelse(YEAR %in% c(yr2,yr3),routvac[5],`15`)) %>%
    mutate(`16` = ifelse(YEAR %in% c(yr2,yr3),routvac[5],`16`)) %>%
    mutate(`17` = ifelse(YEAR %in% c(yr2,yr3),routvac[5],`17`)) %>%
    mutate(`18` = ifelse(YEAR %in% c(yr2,yr3),routvac[5],`18`)) 
  
  fv12$from <- "vac1"
  fv12$to <- "vac2"
  
  write_tsv(fv12,  paste0("countries/ZAF/data/BCGvx/",fn[2])) 
  
  
  ####vac2 to vac3
  fv23 <- f %>%
    mutate(`11` = ifelse(YEAR %in% c(yr3),routvac[5],`11`)) %>%
    mutate(`12` = ifelse(YEAR %in% c(yr3),routvac[5],`12`)) %>%
    mutate(`13` = ifelse(YEAR %in% c(yr3),routvac[5],`13`)) %>%
    mutate(`14` = ifelse(YEAR %in% c(yr3),routvac[5],`14`)) %>%
    mutate(`15` = ifelse(YEAR %in% c(yr3),routvac[5],`15`)) %>%
    mutate(`16` = ifelse(YEAR %in% c(yr3),routvac[5],`16`)) %>%
    mutate(`17` = ifelse(YEAR %in% c(yr3),routvac[5],`17`)) %>%
    mutate(`18` = ifelse(YEAR %in% c(yr3),routvac[5],`18`)) 
  
  fv23$from <- "vac2"
  fv23$to <- "vac3"
  
  write_tsv(fv23,  paste0("countries/ZAF/data/BCGvx/",fn[3])) 
  
  
 ###never to prev1
  
  fnp1 <- fnv1
  fnp1$to <- "prev1"
  fnp1$TB <- infec
  write_tsv(fnp1,  paste0("countries/ZAF/data/BCGvx/",fn[4])) 
  
  ###prev1 to prev2 Lf, Ls, L0, R, Ds
  
  fp12 <- fv12
  fp12$from <- "prev1"
  fp12$to <- "prev2"
  fp12$TB <- infec
  write_tsv(fp12,  paste0("countries/ZAF/data/BCGvx/",fn[5]))
  
  ###prev2 to prev3 Lf, Ls, L0, R, Ds
  
  fp23 <- fv23
  fp23$from <- "prev2"
  fp23$to <- "prev3"
  fp23$TB <- infec
  write_tsv(fp23,  paste0("countries/ZAF/data/BCGvx/",fn[6]))
  
  
  ###waved - prev1
  
  fwp1 <- fp12
  fwp1$from <- "waned"
  fwp1$to <- "prev1"
  write_tsv(fp23,  paste0("countries/ZAF/data/BCGvx/",fn[7]))
  
  
  ###waned - vac1
  fwv1 <- fv12
  fwv1$from <- "waned"
  fwv1$to <- "vac1"
  
  write_tsv(fp23,  paste0("countries/ZAF/data/BCGvx/",fn[8]))
  
  
######################high
  
  fn <- c("BCG_scaleup_high_never_vac1.txt",
          "BCG_scaleup_high_vac1_vac2.txt",
          "BCG_scaleup_high_vac2_vac3.txt",
          "BCG_scaleup_high_never_prev1.txt",
          "BCG_scaleup_high_prev1_prev2.txt",
          "BCG_scaleup_high_prev2_prev3.txt",
          "BCG_scaleup_high_waned_prev1.txt",
          "BCG_scaleup_high_waned_vac1.txt"
  ) 
  
  ca_90 <- c(0.198,0.16, 0.129, 0.105, 0.085)
  ro_90 <- c(0.58, 0.821, 1.109,1.561,rep(2.303,22))
  yr_scale <- 2024:2029
  
  fh_nv1 <- fnv1
  fh_nv1 <- fh_nv1 %>% 
    mutate(`10`=ifelse(YEAR >=2024, ro_90,`10`))
 f2 <- fh_nv1 
  
  f2 <- f2 %>%
    mutate(`11` = ifelse(f2$YEAR %in% yr_scale, ca_90, `11`)) %>%
    mutate(`12` = ifelse(f2$YEAR %in% yr_scale, ca_90, `12`)) %>%
    mutate(`13` = ifelse(f2$YEAR %in% yr_scale, ca_90, `13`)) %>%
    mutate(`14` = ifelse(f2$YEAR %in% yr_scale, ca_90, `14`)) %>%
    mutate(`15` = ifelse(f2$YEAR %in% yr_scale, ca_90, `15`)) %>%
    mutate(`16` = ifelse(f2$YEAR %in% yr_scale, ca_90, `16`)) %>%
    mutate(`17` = ifelse(f2$YEAR %in% yr_scale, ca_90, `17`)) %>%
    mutate(`18` = ifelse(f2$YEAR %in% yr_scale, ca_90, `18`)) %>%
    #years 2035,2045
    mutate(`11` = ifelse(f2$YEAR %in% c(yr2,yr3),ro_90[5],`11`)) %>%
    mutate(`12` = ifelse(f2$YEAR %in% c(yr2,yr3),ro_90[5],`12`)) %>%
    mutate(`13` = ifelse(f2$YEAR %in% c(yr2,yr3),ro_90[5],`13`)) %>%
    mutate(`14` = ifelse(f2$YEAR %in% c(yr2,yr3),ro_90[5],`14`)) %>%
    mutate(`15` = ifelse(f2$YEAR %in% c(yr2,yr3),ro_90[5],`15`)) %>%
    mutate(`16` = ifelse(f2$YEAR %in% c(yr2,yr3),ro_90[5],`16`)) %>%
    mutate(`17` = ifelse(f2$YEAR %in% c(yr2,yr3),ro_90[5],`17`)) %>%
    mutate(`18` = ifelse(f2$YEAR %in% c(yr2,yr3),ro_90[5],`18`)) 
  
  
  write_tsv(fnv1,  paste0("countries/ZAF/data/BCGvx/",fn[1]))
    
  