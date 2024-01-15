library(assertthat)
y = read.table(file="./countries-examples/ZAF/logs-area1-seeds/ZAF_2021-12-20_15h45m57s_3_2_3_XMLinput_2910_m.txt", 
               header = T)
df  = read.table(file="./countries-examples/ZAF/logs-area1-seeds/all_36_36_hits.txt")
names(df)=c("filename",names(y))
write.csv(df,file = "all_36_36_HITS_area1.csv",row.names = F)

priors = read.csv(file="./countries-examples/ZAF/parameters/Area1/input_0309.csv")
fitted = priors[priors$choose==T,]
fitted = fitted[order(fitted$unique.name),]
str(df)
# check
assert_that(all(fitted$unique.name == names(df)[8:40]),msg="something rotten")

minrow = df[nrow(df),] ; minrow[,1:7]=NA ; minrow[8:40]=fitted$min
maxrow = df[nrow(df),] ; maxrow[,1:7]=NA ; maxrow[8:40]=fitted$max
z = rbind(df,minrow)
z = rbind(z,maxrow)
tail(z)

my.points=function(x,y){
  n = length(x)
  xmin = x[n-1] ; xmax = x[n]
  ymin = y[n-1] ; ymax = y[n]
  x = x[-c(n-1,n)]
  y = y[-c(n-1,n)]
  points(x, y, type="p", pch=".", col="blue", xlim=c(xmin,xmax), ylim=c(ymin,ymax))
}
range = c(1:33)
pairs(z[,range+7],panel=my.points,gap=0)

