#-------------------
# This script runs the epi scenarios for 
# "Modelling the health and economic impacts of M72/AS01E vaccination and BCG-revaccination: estimates for South Africa"
#-------------------

# 1. Set-up
suppressPackageStartupMessages({
  rm(list=ls())
  model = new.env()
  library(here)
  library(data.table)
  library(renv)
  library(digest)
  library(log4r)
  library(fst)
  library(arrow)
  library(logger)
  library(getopt)
  
  source(here("R","include-v11.R"), model)
  source(here::here("R","TBVx-run-v1.R"), model)
  source(here("R", "workflow", "run_param_set_epi_SA_count.R"))
  
})

# 2. Load in the country code and set the name of the csv with the vx_scenarios

if (Sys.getenv("TBVAX_CLUSTER_JOB") == "YES"){  # code for running on cluster
	opts <- getopt(matrix(c(
	"cc","c",1,"character",
	"scenario","s",1,"character",
	"params","p","1","character"	
	),byrow=TRUE,ncol=4))

	grid_task_int <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
	cc <- opts$cc
	vx_scenarios <- opts$scenario	
	param_sets <- opts$params
}else{                                       # if not on a cluster
	cc <- "ZAF"
	vx_scenarios <- "epi_vx_scenarios_bmgf_all.csv"
	param_sets <- "ZAF_params.csv"
	grid_task_int <- 1
}

# Load in the fitted parameters for the country
# n_per task will dtermine a) the number of parameter sets per node if on cluster b) the total number of parameter sets if running locally
parameters <- fread(here("processing_files/paramsets/",param_sets))
n_per_task <- 2
parameters <- parameters[(n_per_task*(grid_task_int-1)+1):(n_per_task*grid_task_int),]

# Read in the csv with the different vx_scenarios to run 
vx_scenarios <- fread(here("processing_files/", vx_scenarios))
rr_pct <- 0.034

# 3. Make Directories to store logs and output

local({
	log_dir <- paste0("countries/",cc,"/logs")
	op_dir <- paste0("countries/",cc,"/output")
	dir_list <- c("epi_output/n_epi",log_dir,op_dir)
	
	for (item in dir_list){
		iloc <- here(item)
		if (!dir.exists(iloc)) dir.create(iloc,showWarnings = F, recursive = T)
	}
})

# 4. Generate the output and write out 

for (j in 1:nrow(parameters)){  # for each parameter set
  
  print(paste0("start time = ", Sys.time()))
  
  print(paste0("starting parameter set number ", j))
  
  # select row j of parameters
  params <- parameters[j, ]
  
  # save the uid
  params_uid <- params[, uid]
  
  print(paste0("uid = ", params_uid))
  
  # Get rid of everything except parameters
  params <- params[, !c("uid", "n_hit")]
  params <- unlist(params)
  
  # Run through all the vaccine scenarios for each parameter set
  cc_n_epi_param <- list()
  
#  for (i in 1:nrow(vx_scenarios)){   # comment this out if you only want to run a subset of scenarios for testing
  for (i in c(1,2)){                  # define the subset here
    # subset to the i-th row of characteristics
    vx_chars <- vx_scenarios[i,]
    
    print(paste0("Running scenario number ", i, ": ", vx_chars$runtype))
    
    # run the model with the row of parameters
    vx_scen_output <- run_param_set_epi(model, cc, params, params_uid, vx_chars)
    
    cc_n_epi_param[[i]]  <- vx_scen_output[["n_epi"]]
    
  }
  
  # write the outputs for the parameter set
  write_parquet(rbindlist(cc_n_epi_param), here("epi_output", "n_epi", paste0(params_uid, ".parquet")))
  rm(cc_n_epi_param)
  gc(full= TRUE)
  
  print(paste0("end time for parameter set ", j, " = ", Sys.time()))
  
}

# ---- end















