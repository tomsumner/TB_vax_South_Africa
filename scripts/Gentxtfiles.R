rm(list=ls())
library(dplyr)
library(here)
library(data.table)
library(digest)
library(log4r)
library(fst)
library(git2r)
library(readr)

###M72 vac

#instant vaccine
#routvac <- 0.8
#massvac <- 0.7


vx_char <- fread("scripts/M72_scenariosb.csv")

f <- fread("countries/ZAF/data/M72_adoadu_vac_template.txt",header=TRUE)
f <- data.table(f)

for(i in 1:nrow(vx_char)){
clt <- colnames(f)
ms <- colnames(f)[21:92]

f1 <- f %>%
  filter(YEAR < vx_char$Intro_Year[i]) 

f2 <- f %>%
  filter(YEAR %in% vx_char$Intro_Year[i]:2050)

nr <- nrow(f2)-4
routvac <- c(-log(1-0.39), -log(1-0.5), -log(1-0.6), -log(1-0.7), rep(-log(1-0.8),nr))
routvac <- round(routvac, 3)

f2$`9` <- routvac

massvac <- c(-log(1-0.14), -log(1-0.1208), -log(1-0.1035), -log(1-0.0890), -log(1-0.0766))
massvac <- round(massvac, 3)

f2[1:5 , 21:92 ] <- massvac

dat <- rbind(f1,f2)
 write_tsv(dat,  paste0("countries/ZAF/data/",vx_char$data_file_name1[i]))
  
}



#M72 prev

vx_char <- fread("scripts/M72_scenariosb.csv")

f <- fread("countries/ZAF/data/M72_adoadu_prev_template.txt",header=TRUE)
f <- data.table(f)

for(i in 1:nrow(vx_char)){
 
  clt <- colnames(f)
  ms <- colnames(f)[21:92]
  
  f1 <- f %>%
    filter(YEAR < vx_char$Intro_Year[i]) 
  
  f2 <- f %>%
    filter(YEAR %in% vx_char$Intro_Year[i]:2050)
  
  nr <- nrow(f2)-4
  routvac <- c(-log(1-0.39), -log(1-0.5), -log(1-0.6), -log(1-0.7), rep(-log(1-0.8),nr))
  routvac <- round(routvac, 3)
  
  f2$`9` <- routvac
  
  massvac <- c(-log(1-0.14), -log(1-0.1208), -log(1-0.1035), -log(1-0.0890), -log(1-0.0766))
  massvac <- round(massvac, 3)
  
  f2[1:5 , 21:92 ] <- massvac
  
  dat <- rbind(f1,f2)
  write_tsv(dat,  paste0("countries/ZAF/data/",vx_char$data_file_name2[i]))
  
}
  
#BCG vac
  
  vx_char <- fread("scripts/BCG_scenariosb.csv")
  
  #routvac <- 0.8
  f <- fread("countries/ZAF/data/BCG_ado_vac_template.txt",header=TRUE)
  f <- data.table(f)
  
  for(i in 1:nrow(vx_char)){
 
  f1 <- f %>%
    filter(YEAR < vx_char$Intro_Year[i]) 
  
  f2 <- f %>%
    filter(YEAR %in% vx_char$Intro_Year[i]:2050)
  
  #to generate scaleup vac
  
  nr <- nrow(f2)-4
  routvac <- c(-log(1-0.39), -log(1-0.5), -log(1-0.6), -log(1-0.7), rep(-log(1-0.8),nr))
  routvac <- round(routvac, 3)
  
  f2$`9` <- routvac
  
  dat <- rbind(f1,f2)
  write_tsv(dat,  paste0("countries/ZAF/data/",vx_char$data_file_name1[i]))
  
  }

  
  #BCG_prev  
  vx_char <- fread("scripts/BCG_scenariosb.csv")
  
  #routvac <- 0.8
  f <- fread("countries/ZAF/data/BCG_ado_prev_template.txt",header=TRUE)
  f <- data.table(f)
  
  for(i in 1:nrow(vx_char)){
    
    f1 <- f %>%
      filter(YEAR < vx_char$Intro_Year[i]) 
    
    f2 <- f %>%
      filter(YEAR %in% vx_char$Intro_Year[i]:2050)
    
    #to generate scaleup vac
    
    nr <- nrow(f2)-4
    routvac <- c(-log(1-0.39), -log(1-0.5), -log(1-0.6), -log(1-0.7), rep(-log(1-0.8),nr))
    routvac <- round(routvac, 3)
    
    f2$`9` <- routvac
    
    dat <- rbind(f1,f2)
    write_tsv(dat,  paste0("countries/ZAF/data/",vx_char$data_file_name2[i]))
    
  }  
 