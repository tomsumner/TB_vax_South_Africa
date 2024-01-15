rm(list=ls())
# model = new.env()
model = globalenv()
source(here::here("R","include-v11.R"),model)
source(here::here("R","TBVx-run-v1.R"),T)
taskenvvar="taskID"

model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "ZAF", 
                              xml         = "examples-tbvax-3.2.8/XMLinput_target_age_L0_aim_ABC_med_fast_minDs10d_38HITS_count_E.xml",
                              #parameters  = "switch_files/input_L0_red.csv",
                              targets     = "examples-tbvax-3.2.8//target_L0_LTBI_all-count_E.csv",
                              lglevel     = "INFO")
system.time((z=run(model,write.to.file = T, output.flows=T)))
# user  system elapsed 
# 142.17    6.04  148.79  



model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "ZAF", 
                              xml         = "examples-tbvax-3.2.8/XMLinput_target_age_L0_aim_ABC_med_fast_minDs10d_38HITS_count_E.xml",
                              #parameters  = "switch_files/input_L0_red.csv",
                              targets     = "examples-tbvax-3.2.8/target_L0_LTBI_all-count_only_E.csv",
                              lglevel     = "INFO")
system.time((y=run(model,write.to.file = T, output.flows=F)))
# user  system elapsed 
# 45.02    1.62   46.62 

# The XML input file used below has 4 treatment matrices combined into one ; due to clever optimization algorithms that combine treatment matrices 
# if possible, performance should not be very different
model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "ZAF", 
                              xml         = "examples-tbvax-3.2.8/XMLinput_target_age_L0_aim_ABC_med_fast_minDs10d_38HITS_count_basic_E.xml",
                              #parameters  = "switch_files/input_L0_red.csv",
                              targets     = "examples-tbvax-3.2.8/target_L0_LTBI_all-count_only_E.csv",
                              lglevel     = "INFO")
system.time((x=run(model,write.to.file = T, output.flows=F)))
# user  system elapsed 
# 43.87    2.07   45.97 



rm(list=ls())
# model = new.env()
model = globalenv()
source(here::here("R","include-v11.R"),model)
source(here::here("R","TBVx-run-v1.R"),T)
taskenvvar="taskID"
param_sets <- "ZAF_params.csv"
parameters <- fread(here("./processing_files/paramsets", param_sets))
model$paths = model$set.paths(countries   = "countries",
                              countrycode = "ZAF",
                              xml         = "XMLinput_count_epi_baseline_bmgf_targetcheck_RB5_revA.xml",
                              parameters  = "input.csv",
                              # targets     = "target_L0_LTBI_all-count_E.csv",
                              targets     = "target_L0_LTBI_all-count_E_RB5_revA.csv",
                              lglevel     = "DEBUG")
for (i in 1:100){
  params = parameters[i,3:ncol(parameters)]
  output = run(model, new.parameter.values = params, write.to.file = T, output.flows = T)
  print(sum(output$hits$fit))
}
