rm(list=ls())
# model = new.env()
model = globalenv()
source(here::here("R","include-v11.R"),model)
source(here::here("R","TBVx-run-v1.R"),T)
taskenvvar="taskID"

model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "ZAF", 
                              xml         = "FAST/XMLinput_target_age_L0_aim_ABC_med_fast_minDs10d_38HITS_BIGRnozeros.xml",
                              #xml         = "FAST/XMLinput_target_age_L0_aim_ABC_med_fast_minDs10d_38HITS.xml",
                              #parameters  = "FAST/input_L0_red.csv",
                              targets     = "FAST/target_L0_LTBI_all2.csv",
                              lglevel     = "INFO")

Sys.time()
x=run(model,write.to.file = T,output.format = "fst")
Sys.time()





