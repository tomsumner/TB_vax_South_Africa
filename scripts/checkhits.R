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
