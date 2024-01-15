
library(data.table)
library(dplyr)

#directory <- "C:/Users/ChristinahMukandavir/Documents/GitHub/tbvax/countries/ZAF/data/BCGvx/new/All/"
directory <- "C:/Users/ChristinahMukandavir/Documents/GitHub/tbvax/countries/ZAF/data/M72vx/"

files <- list.files(directory,pattern = ".txt")

for(i in seq_along(files)){
#for(i in 39:72){
  print(i)
 # i=38
 k<- read.table(paste0(directory,files[i]), header = TRUE) 
 
 val <- k$to[1]
 
 k$to <- paste0(val,",","recvcount")
 
 #colnames(k) <- c(0:79,80,90)
 
 colnames(k)[8:89] <- c(0:79,80,90)
 
 write.table(k,file = paste0("C:/Users/ChristinahMukandavir/Documents/GitHub/tbvax/countries/ZAF/data/M72vx_count/",files[i]),
             sep="\t", row.names = F)
}

