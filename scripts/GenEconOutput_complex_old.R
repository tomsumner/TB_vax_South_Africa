#-------------------
# Generate Econ Output - Complex Models
# Rebecca Clark
# 13 December 2021
#-------------------

# Objectives: 
#  Run the model with the parameter sets from calibration. 
#  Generate the necessary econ output

# 1. Set-up: Load in the required packages
suppressPackageStartupMessages({
  rm(list=ls())
  model = new.env()
  #model = globalenv()
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
  source(here("R", "workflow", "run_param_set_econ.R"))
  source(here::here("R","TBVx-run-v1.R"), model)

  
})

# 2. Load in the country code, parameters, and scenario information

if (F){ # on local machine
  cc <- "IND"
  vx_scenarios <- "econ_vx_scenarios_rate.csv"
  econ = T
  
} else{ # on cluster
  opts = getopt(matrix(c('cc','c', 1, "character",
                         'scenario', 's', 1, "character",
                         'econ', 'e', 1, "logical"),
                       byrow=TRUE, ncol=4))
  
  grid_task_int     <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  
  cc <- opts$cc
  vx_scenarios <- opts$scenario
  econ <- opts$econ
}


# Load in the fitted parameters for the country
parameters <- fread(here("./processing_files/param_sets/IND_fullfits_step_n20.csv"))

# Read in the csv with the different vx_scenarios to run 

vx_scenarios <- fread(here("./processing_files", vx_scenarios))

rr_prop <- 0.028 ## IND specific



# 3. Make Directories

if(!dir.exists(here("econ_output"))) dir.create(here("econ_output"), showWarnings = F)
if(!dir.exists(here("econ_output/IND_TB"))) dir.create(here("econ_output/IND_TB"), showWarnings = F)
if(!dir.exists(here("econ_output/IND_TB_HIV"))) dir.create(here("econ_output/IND_TB_HIV"), showWarnings = F)
if(!dir.exists(here("econ_output/IND_alldeaths"))) dir.create(here("econ_output/IND_alldeaths"), showWarnings = F)




# 4. Generate the output and write out 

for (j in 1:nrow(parameters)) {

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
  
for (i in 1:nrow(vx_scenarios)){
    
  # subset to the i-th row of characteristics
    vx_chars <- vx_scenarios[i,]
    
    print(paste0("vaccine scenario number ", i))
    
    # if it's an intervention run, set the vaccine incidence files:
    if (vx_chars$runtype != "baseline") {
      if (grepl("Neo", vx_chars$runtype) == TRUE) { # Infant vaccine = only one incidence file
        vx_inc <- c(vx_chars$vac)
      } else { # Ado/adult vaccine = two incidence files
        vx_inc <- c(vx_chars$prev, vx_chars$vac)
      }
    } else {
      vx_inc <- NA
    }
    
    
    # run the model with the row of parameters
    vx_scen_output <- run_param_set_econ(model, cc, params, params_uid,
                                          vx_chars, vx_inc, econ_output = econ, rr_pct = rr_prop)
    
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


econ_check <- open_dataset(sources = "./econ_output/IND_TB/") %>% dplyr::collect()
