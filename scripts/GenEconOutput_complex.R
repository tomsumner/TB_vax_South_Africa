#-------------------
# Generate Econ Output - Complex Models
# Rebecca Clark
# Updated 3 February 2022
#-------------------

# Objectives: 
#  Run the model with the parameter sets from calibration. 
#  Generate the necessary econ output

# 1. Set-up: Load in the required packages
suppressPackageStartupMessages({
  rm(list=ls())
  model = globalenv()
  library(here)
  library(data.table)
  library(renv)
  library(digest)
  library(log4r)
  library(fst)
  library(git2r)
  library(arrow)
  if (!require("getopt")) install.packages("getopt")
  library(getopt)
  library(logger)
  source(here("R","include-v11.R"), model)
  source(here("R", "workflow", "run_param_set_econ_SA_count.R"))
  source(here::here("R","TBVx-run-v1.R"), model)

  
})

# 2. Load in the country code, parameters, and scenario information

if (F){ # on local machine
  cc <- "ZAF"
  vx_scenarios <- "econ_vx_scenarios_bmgf_all.csv"#econ_vx_scenarios_2501.csv
  econ = T
  
} else{ # on cluster
  opts = getopt(matrix(c('cc','c', 1, "character",
                         'scenario', 's', 1, "character"),
                       byrow=TRUE, ncol=4))
  
  #grid_task_int     <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  grid_task_int <- as.numeric(Sys.getenv("SGE_TASK_ID"))
  #task_ID <- as.numeric(Sys.getenv("SGE_TASK_ID"))
  
  cc <- opts$cc
  vx_scenarios <- opts$scenario
  
}


# Load in the fitted parameters for the country
parameters <- fread(here("./processing_files/paramsets/ZAF_params.csv"))
#parameters <- parameters[grid_task_int:grid_task_int, ]
#parameters <- parameters[(5*(grid_task_int-1)):(grid_task_int-4)*5, ]

# Read in the csv with the different vx_scenarios to run 

vx_scenarios <- fread(here("./processing_files", vx_scenarios))

rr_prop <- 0.034 ## ZAF specific



# 3. Make Directories

if(!dir.exists(here("econ_output"))) dir.create(here("econ_output"), showWarnings = F)
if(!dir.exists(here("econ_output/ZAF_TB"))) dir.create(here("econ_output/ZAF_TB"), showWarnings = F)
if(!dir.exists(here("econ_output/ZAF_TB_HIV"))) dir.create(here("econ_output/ZAF_TB_HIV"), showWarnings = F)
if(!dir.exists(here("econ_output/ZAF_alldeaths"))) dir.create(here("econ_output/ZAF_alldeaths"), showWarnings = F)



# j=1
# 4. Generate the output and write out 
for (j in 1:nrow(parameters)){
  
  print(paste0("start time = ", Sys.time()))
  
  print(paste0("starting parameter set number ", j))
  
  # select row j of parameters
  params <- parameters[j, ]
  
  # save the uid
  params_uid <- params[, uid]
  
  print(paste0("uid = ", params_uid))
  
  # get rid of everything except parameters
  params <- params[, !c("uid", "n_hit")]
  params <- unlist(params)
  
  
  # Run through all the vaccine scenarios for each parameter set
  
  cc_TB_param     <- list()
  cc_deaths_param <- list()
  cc_TB_HIV_param <- list()

  # i=2
  for (i in 1:nrow(vx_scenarios)){
    
  # subset to the i-th row of characteristics
    vx_chars <- vx_scenarios[i,]
    
    print(paste0("vaccine scenario number ", i))
    

    # run the model with the row of parameters
   # debugonce(run_param_set_econ)
    vx_scen_output <- run_param_set_econ(model, cc, params, params_uid, 
                                         vx_chars, rr_pct = rr_prop)
    
    cc_TB_param[[i]] <- vx_scen_output[["cc_TB"]]
    cc_TB_HIV_param[[i]] <- vx_scen_output[["cc_TB_HIV"]]
    cc_deaths_param[[i]] <- vx_scen_output[["cc_deaths"]]
    
  }
  
  
  cc_TB <- rbindlist(cc_TB_param)
  rm(cc_TB_param)
  
  cc_TB_HIV <- rbindlist(cc_TB_HIV_param)
  rm(cc_TB_HIV_param)
  
  cc_alldeaths <- rbindlist(cc_deaths_param)
  rm(cc_deaths_param)
  
  
  write_parquet(cc_TB, file.path("econ_output", paste0(cc, "_TB"),
                          paste0(params_uid, ".parquet")))
  rm(cc_TB)
  
  
  write_parquet(cc_TB_HIV, file.path("econ_output", paste0(cc, "_TB_HIV"),
                                 paste0(params_uid, ".parquet")))
  rm(cc_TB_HIV)
  
  
  write_parquet(cc_alldeaths, file.path("econ_output", paste0(cc, "_alldeaths"),
                                     paste0(params_uid, ".parquet")))
  rm(cc_alldeaths)
  
  
  print(paste0("end time for parameter set ", j, " = ", Sys.time()))
  
  }


#----end


