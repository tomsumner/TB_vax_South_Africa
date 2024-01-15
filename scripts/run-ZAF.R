rm(list=ls())
# model = new.env()
model = globalenv()
source(here::here("R","include-v11.R"),model)
source(here::here("R","TBVx-run-v1.R"),T)
taskenvvar="taskID"

model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "ZAF", 
                              xml         = "15JUN22/XMLinput_target_age_L0_aim_ABC_med_ode23_fast_Ds10d.xml",
                              parameters  = "15JUN22/input_L0_min0_exp.csv",
                              targets     = "15JUN22/target_L0_LTBI_all_fast.csv",
                              lglevel     = "ERROR")

model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "ZAF", 
                              xml         = "15JUN22/XMLinput_target_age_L0_aim_ABC_med_ode23_orig_Ds10d_switch.xml",
                              parameters  = "15JUN22/input_L0_min0_exp.csv",
                              targets     = "15JUN22/target_L0_LTBI_all.csv",
                              lglevel     = "ERROR")

model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "ZAF", 
                              xml         = "15JUN22/XMLinput_target_age_L0_aim_ABC_med_ode23_orig_Ds10d.xml",
                              parameters  = "15JUN22/input_L0_min0_exp.csv",
                              targets     = "15JUN22/target_L0_LTBI_all.csv",
                              lglevel     = "ERROR")

model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "ZAF", 
                              xml         = "CHRIS27JUN22/XMLinput_target_age_L0_aim_ABC_med_slow_switch_2025.xml",
                              parameters  = "CHRIS27JUN22/input_L0_red.csv",
                              targets     = "CHRIS27JUN22/target_L0_LTBI_all2.csv",
                              lglevel     = "INFO")
model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "ZAF", 
                              xml         = "CHRIS30JUN22/XMLinput_BCG_NCI_POI_45_10yr_med_2025.xml",
                              lglevel     = "INFO")

model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "ZAF", 
                              xml         = "switch_files/XMLinput_target_age_L0_aim_ABC_med_slow_switch2025_run_test_ode23.xml",
                              lglevel     = "INFO")


model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "ZAF", 
                              xml         = "switch_files/XMLinput_target_age_L0_aim_ABC_med_slow_noswitch_run_ode23.xml",
                              parameters  = "switch_files/input_L0_red.csv",
                              targets     = "switch_files/target_L0_LTBI_all2.csv",
                              lglevel     = "INFO")

model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "ZAF", 
                              xml         = "switch_files/XMLinput_target_age_L0_aim_ABC_med_slow_switch2025_run_test_ode23.xml",
                              parameters  = "switch_files/input_L0_red.csv",
                              targets     = "switch_files/target_L0_LTBI_all2.csv",
                              lglevel     = "INFO")


model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "ZAF", 
                              xml         = "switch_files/XMLinput_target_age_L0_aim_ABC_med_switch2025_ode23_nochange_fast.xml",
                              parameters  = "switch_files/input_L0_red.csv",
                              targets     = "switch_files/target_L0_LTBI_all2.csv",
                              lglevel     = "INFO")


model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "ZAF", 
                              xml         = "switch_files/XMLinput_target_age_L0_aim_ABC_med_slow_switch2025_run_test_ode23_noswitch.xml",
                              parameters  = "switch_files/input_L0_red.csv",
                              targets     = "switch_files/target_L0_LTBI_all2.csv",
                              lglevel     = "INFO")



model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "ZAF", 
                              xml         = "FAST/XMLinput_target_age_L0_aim_ABC_med_fast_minDs10d_38HITS.xml",
                              #parameters  = "switch_files/input_L0_red.csv",
                              targets     = "FAST/target_L0_LTBI_all2.csv",
                              lglevel     = "INFO")

model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "ZAF", 
                              xml         = "switch_files/XMLinput_target_age_L0_aim_ABC_med_slow_switch2025_run_test_ode45only.xml",
                              parameters  = "switch_files/input_L0_red.csv",
                              targets     = "switch_files/target_L0_LTBI_all2.csv",
                              lglevel     = "INFO")


system.time(run(model,write.to.file = F))









df = read.csv(file="./countries-examples/ZAF/parameters/CHRIS27JUN22/abc_EM6_13-06.csv")
i=1
#for (i in 1:nrow(df)){
  p = as.numeric(df[i,3:ncol(df)])
  names(p)=names(df[3:ncol(df)])
  cat("i=",i,"\n")
  b=run(model, new.parameter.values = p, write.to.file = T)
#}






