#---------------------------
## The ABC wrapper
## Last updated 25 November 2021
## Updated to work with model version 3.2.1
#---------------------------

rm(list=ls())


#--------
# Set-up
#--------

suppressPackageStartupMessages({
  library(fst)
  library(here)
  library(data.table)
  library(logger)
  library(digest)
  model = globalenv()
  
  paths = list()
  paths$src = here("R")
  main.workdir <- here()
  
  source(here(paths$src, "include-v11.R"), echo = F)
  source(here::here("R","TBVx-run-v1.R"), model)
  
})

#------------------------
# Load in Options
#------------------------

if (F){ # on local machine
  opts=list()
  opts$countrycode  <- "IND"               ## SET COUNTRY CODE
  opts$params       <- "input_step.csv"         ## SET PARAMETER FILE TO USE
  opts$xmlinput     <- "XMLinput_target_step.xml"      ## SET XMLINPUT TO USE
  opts$targetfile   <- "target_mod.csv"     ## SET TARGET FILE TO USE
  opts$accepttarget <- 13
  grid_task_int     <- 1

} else{ # on cluster
  require(getopt)
  opts              <- getopt(matrix(c('countrycode','c',1,"character",
                                       'params', 'p', 1, "character",
                                       'xmlinput', 'x', 1, "character",
                                       'targetfile', 't', 1, "character",
                                       'accepttarget', 'a',1, "integer",
                                       'seedfile', 's', 1, "character"
                                       ),
                                     byrow=TRUE, ncol=4))
  grid_task_int     <- as.numeric(Sys.getenv("SGE_TASK_ID"))

}


# Adaptive Settings

adaptive_settings <- list(
  toggle       = TRUE,
  cov_int      = 1,
  sample_width = 999,
  burn         = 1000,
  update       = FALSE,
  noise_factor = 0.0001
)




cc <- opts$countrycode
params <- opts$params
xmlinput <- opts$xmlinput
targetfile <- opts$targetfile


RUNID_TS = Sys.getenv("RUNID_TS")
# If the RUNIDTS environment variable is blank, this implies the script is running locally.
# Set RUNID_TS to local defaults
if (RUNID_TS == "") {
  LTS <- format.Date(Sys.time(), format = "%Y-%m-%d-%H%M", tz = "UTC")
  RUNID_TS <- sprintf("%s_%s_%s", LTS, "LOCAL", cc)
}


##### paths needs to be set before the model log
paths = set.paths(countrycode = cc,
                  xml = xmlinput,
                  parameters = params,
                  targets = targetfile)

## Make sure output and log folders are created for writing into
dir.create(here(paths$country.dir, "logs"), showWarnings = F)
dir.create(here(paths$country.dir, "output"), showWarnings = F)


# Set up logging (needed for model running)
logr <- create.logger(logfile = here(paths$country.dir,
                                     "logs", paste0(RUNID_TS, "_model.log")), level = "INFO")
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
log_appender(appender_tee(file = here(paths$country.dir,"logs",paste0(RUNID_TS, "_abc.log"))))

# Initial information
log_info("SESSION INFO: %s", sessionInfo()[["R.version"]]$version.string)
log_info("OUTPUT PATH: %s", paths$country.output.dir)
log_info("R-LIBPATH: %s", .libPaths())

#--------------------------------------------------------------------------------------------------------------
#           Method, Model, Priors
#---------------------------
# Targets
#---------------------------
targets <- read.targets(paths$targets) # load the actual targets for the calculated epi measures
log_debug("Epi targets: %s", targets)


# Set the number of targets hit to accept
target = opts$accepttarget

log_info("ABC target accept number: %s", target)


#---------------------------
# Parameter Priors
#---------------------------

param.data   <- subset(read.csv(paths$parameters, stringsAsFactors = F, header = T,
                                fileEncoding = "UTF-8-BOM"), choose == TRUE)  # the model input parameters
log_debug("Fitted parameter data: %s", param.data)


prior.ranges <- lapply(split(param.data, f = param.data$unique.name),
                       function(x) c("unif", x$min, x$max)) # put the param ranges into the correct list form
log_debug("PRIOR RANGE: %s", prior.ranges)

propfrac    <- 1/250
log_info("PROP_FRAC: %s", propfrac)

prop.ranges <- vapply(split(param.data , f = param.data$unique.name),
                      function(x) x$max-x$min, FUN.VALUE = numeric(1)) * propfrac
log_debug("PROP RANGE: %s", prop.ranges)



#---------------------------
## General Initializing
#---------------------------
n.samp      <- 1000000   # number of sampled points along MCMC
log_info("CHAIN LENGTH: %s", n.samp)

wo_interval <- 500   # write out interval: how many to go through before writing out
log_info("WO_INT: %s", wo_interval)

seed_search = FALSE
log_info("SEED SEARCH SETTING: %s", seed_search)
tss       <- 0.5    # target summary statistic
gc        <- 1      # global counter
tc        <- 1      # try counter
hitarray  <- list() # initialize matrix to store hits
ahitarray <- list() # accepted hitarray
max_hits  <- 0      # initialize max hits counter


#---------------------------
# Initializing with seeds
#---------------------------

# Read in the file with seeds
#seed_file <- opts$seedfile
seed_file <- paste0(cc, "_paramsets_nhit12_etamul.csv")
seed_path <- paste0(paths$country.dir, "/mcmc_seeds/", seed_file)
param.seeds <- fread(seed_path, header = TRUE)

log_info("SEED IMPORT: %s", seed_file)


# Determine which row to take as the input seeds
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
params <- read.model.parameters(paths = paths)
#debugonce(run.one.abc)
log_info("MAIN-START")
start_time <- Sys.time()

ABCmcmc <- ABC_mcmc(method              = "Marjoram_original",
                    model               = run.one.abc, # this part is the core
                    prior               = prior.ranges,
                    summary_stat_target = tss,
                    proposal_range      = prop.ranges,
                    n_rec               = n.samp,
                    verbose             = TRUE,
                    n_between_sampling  = 1,
                    init_param          = init.param,
                    dist_max            = 0.6,
                    adaptive            = adaptive_settings
)

end_time <- Sys.time()
log_info("FINISH ABC_MCMC FUNCTION")
log_info("Time taken: %f", end_time-start_time)
log_info("END OF SCRIPT >> GC: %s >> TC: %s >> G/T: %3.2f >> MAX HITS: %s", gc, tc, gc/tc, max_hits)

# ------end

