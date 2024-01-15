rm(list=ls())
# model = new.env()
model = globalenv()
source(here::here("R","include-v11.R"),model)
source(here::here("R","TBVx-run-v1.R"),T)
taskenvvar="taskID"

model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "ZAF", 
                              xml         = "switch_files/XMLinput_target_age_L0_aim_ABC_med_switch2025_ode23_nochange_fast.xml",
                              parameters  = "switch_files/input_L0_red.csv",
                              targets     = "switch_files/target_L0_LTBI_all2.csv",
                              lglevel     = "INFO")


no_change=run(model,write.to.file = T)
model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "ZAF", 
                              xml         = "switch_files/XMLinput_target_age_L0_aim_ABC_med_switch2025_ode23_noswitch_fast.xml",
                              parameters  = "switch_files/input_L0_red.csv",
                              targets     = "switch_files/target_L0_LTBI_all2.csv",
                              lglevel     = "INFO")

no_switch=run(model,write.to.file = T)







