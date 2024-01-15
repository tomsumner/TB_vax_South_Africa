rm(list=ls())
model = new.env()
# for debugging set model to globalenv():
# model = globalenv()
source(here::here("R","include-v11.R"),model)
source(here::here("R","TBVx-run-v1.R"))
taskenvvar="taskID"
Sys.setenv(taskID=1089)
set.seed(99)

model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "IND", 
                              xml         = "XMLinput_plot4.xml") 

output1 = run(model, sample.parameters=F, write.to.file = T, combine.stocks.and.flows = F)



model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "IND", 
                              xml         = "XMLinputVx_infant_scaleup_med_LL.xml") 
output3 = run(model, sample.parameters=F, write.to.file = T, combine.stocks.and.flows = F, baseline = output1)

