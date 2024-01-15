#---------------------------
## Prepping the ABC wrapper
## Last updated 09.04.2021 RC
#---------------------------

rm(list=ls())

#--------
# Set-up
#--------
model = new.env()
#library(EasyABC)
library(fst)
library(here)
library(data.table)
library(logger)
library(digest)

paths = list()
paths$src = here("R")
main.workdir <- here()

#source(here(paths$src, "include-v11.R"), echo = F)
source(here::here("R","include-v11.R"),model)


#------------------------
# Load in Options
#------------------------

if (T){ # on local machine
  opts=list()
  opts$countrycode <- "IND"
  grid_task_int    <- 1
  
  } else{ # on harvard cluster
  require(getopt)
  
    opts              <- getopt(matrix(c('countrycode','c',1,"character"),
                                       byrow=TRUE, ncol=4))
    grid_task_int     <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  
  }

cc <- opts$countrycode

RUNID_TS = Sys.getenv("RUNID_TS")
# If the RUNIDTS environment variable is blank, this implies the script is running locally.
# Set RUNID_TS to local defaults
if (RUNID_TS == "") {
  LTS <- format.Date(Sys.time(), format = "%Y-%m-%d-%H%M", tz = "UTC")
  RUNID_TS <- sprintf("%s_%s_%s", LTS, "LOCAL", cc)
}


##### paths needs to be set before the model log
model$paths = model$set.paths(countries="countries-temp", countrycode="IND",xml="XMLinput.xml",parameters="input.csv",targets="target_Rgen.csv")

## Make sure output and log folders are created for writing into
dir.create(here(model$paths$country.dir, "logs"), showWarnings = F)
dir.create(here(model$paths$country.dir, "output"), showWarnings = F)

# Set up logging (needed for model running)
logr <- create.logger(logfile = here(model$paths$country.dir, "logs", 
                                     paste0(RUNID_TS, "_model.log")), level = "INFO")

logger <- function(level = "FATAL", msg = NULL) {
  levellog(logr, level = level, message = msg)
}

#-------------



#---------------------------
# Set up logging (for ABC)
#---------------------------

log_formatter(formatter_sprintf)
logger_abc <- layout_glue_generator(format = '[{time}]\t{level}\t{grid_task_int}\t{msg}')
log_layout(logger_abc)
log_appender(appender_tee(file = here(model$paths$country.dir,"logs",paste0(RUNID_TS, "_abc.log"))))


# Initial information
log_info("SESSION INFO: %s", sessionInfo()[["R.version"]]$version.string)
log_info("OUTPUT PATH: %s", model$paths$output.dir)
log_info("R-LIBPATH: %s", .libPaths())



#--------------------------------------------------------------------------------------------------------------
#           Method, Model, Priors

#---------------------------
# Targets
#---------------------------
targets <- read.targets(model$paths$targets) # load the actual targets for the calculated epi measures
log_debug("Epi targets: %s", targets)

target  <- 9   # the number of targets hit for the ABC to "accept" 
log_info("ABC target accept number: %s", target)

#---------------------------
# Parameter Priors
#---------------------------

param.data   <- subset(read.csv(model$paths$parameters, stringsAsFactors = F, header = T,
                                fileEncoding = "UTF-8-BOM"), choose == TRUE)  # the model input parameters
#param.data   <- setDT(param.data)
#param.data   <- param.data[order(unique.name)]

log_debug("Fitted parameter data: %s", param.data)


prior.ranges <- lapply(split(param.data, f = param.data$unique.name),
                       function(x) c("unif", x$min, x$max)) # put the param ranges into the correct list form
log_debug("PRIOR RANGE: %s", prior.ranges)


propfrac    <- 1/500
log_info("PROP_FRAC: %s", propfrac)

prop.ranges <- vapply(split(param.data , f = param.data$unique.name),
                      function(x) x$max-x$min, FUN.VALUE = numeric(1)) * propfrac
log_debug("PROP RANGE: %s", prop.ranges)




#--------------------------- 
## General Initializing
#---------------------------
n.samp      <- 100000   # number of sampled points along MCMC
log_info("CHAIN LENGTH: %s", n.samp)

wo_interval <- 1000   # write out interval: how many to go through before writing out
log_info("WO_INT: %s", wo_interval)

seed_search = FALSE
log_info("SEED SEARCH SETTING: %s", seed_search)

tss       <- c(1,0) # target summary statistic

gc        <- 1      # global counter

tc        <- 1      # try counter

hitarray  <- list() # initialize matrix to store hits

ahitarray <- list() # accepted hitarray

max_hits  <- 0      # initialize max hits counter



#---------------------------
# Initializing with seeds
#---------------------------

# Read in the file with seeds
seed_file <- paste0(cc, "_input_seeds.csv")
seed_path <- paste0(model$paths$country.dir, "/mcmc_seeds/", seed_file)
param.seeds <- fread(seed_path, header = TRUE)
# x <- as.data.frame(param.seeds)
# z = x[,1:2]
# y = x[,3:21][c(order(names(x)[3:21]))]
# param.seeds <- cbind(z,y)
log_info("SEED IMPORT: %s", seed_file)




# Determine which row to take
if (grid_task_int == 1 | nrow(param.seeds) == 1) {# if task_id =1 or n rows = 1 take the first row
  init.param <- param.seeds[1, ]
} else if (grid_task_int <= nrow(param.seeds)){ # if the task_id >1 but < n rows, take that row
  init.param <- param.seeds[grid_task_int, ]
} else if (grid_task_int > nrow(param.seeds) & grid_task_int %% nrow(param.seeds) == 0) {
  init.param <- param.seeds[nrow(param.seeds), ]
} else if (grid_task_int > nrow(param.seeds) & grid_task_int %% nrow(param.seeds) != 0) {
  init.param <- param.seeds[grid_task_int %% nrow(param.seeds), ]
}
## if nrows greater than grid int: and
#     grid int is an exact multiple of nrow, then take nrow
#     not exact multiple: take the mod



# some logging
log_debug("INIT PARAM: %s", init.param)
log_info("INIT PARAM UID: %s", init.param$uid)


# remove the UID, and save the names 
# init.param <- setDT(init.param)
# init.param <- init.param[, !c("uid","n_hit")]
# init.param <- as.list(init.param)
# init.param <- unlist(init.param)

init.param <- unlist(init.param[, !c("uid","n_hit")])
init.names <- names(init.param)   # need to save the names of the params  
log_debug("PARAM NAMES: %s", init.names)

#--------------------------------------------------------------------------------------------------------------

#--------------------------- 
# Source the core ABC needs
#--------------------------- 

# our core model wrapper
source(paste0(paths$src,"/R_ABC/ABC_core.R")) 

# ABC related things
source(paste0(paths$src,"/R_ABC/EasyABC-internal.R"))
source(paste0(paths$src,"/R_ABC/ABC_mcmc.R"))



#--------------------------- 
# Run the ABC MCMC
#--------------------------- 
# initialize params outside
params <- model$set.paths(countries="countries-temp", countrycode="IND",
                         xml="XMLinput.xml",parameters="input.csv",
                         targets = "target_Rgen.csv")


log_info("MAIN-START")
ABCmcmc <- ABC_mcmc(method              = "Marjoram_original",
                    model               = run.one.abc, # this part is the core
                    prior               = prior.ranges,
                    summary_stat_target = tss,
                    proposal_range      = prop.ranges,
                    n_rec               = n.samp,
                    verbose             = TRUE, 
                    n_between_sampling  = 1,
                    acceptance          = TRUE,
                    rejection           = TRUE,
                    init_param          = init.param
                    )

log_info("FINISH ABC_MCMC FUNCTION")
log_info("END OF SCRIPT >> GC: %s >> TC: %s >> G/T: %3.2f >> MAX HITS: %s", gc, tc, gc/tc, max_hits)



# ------end



