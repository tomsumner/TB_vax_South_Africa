rm(list=ls())
cc <- "ZAF" #set the country code

# 1. Set-up:
suppressPackageStartupMessages({
  model = globalenv()
  library(here)
  library(data.table)
  library(renv)
  library(digest)
  library(log4r)
  library(fst)
  library(arrow)
  library(logger)
  
  source(here("R","include-v11.R"), model)
  source(here::here("R","TBVx-run-v1.R"), model)
  
  # hivlist <- c("BWA", "CAF", "CIV", "CMR", "GAB", "GHA", "GMB", "GNB",
  #              "GNQ", "GUY", "KEN", "LSO", "MOZ", "MWI", "NAM", "RWA",
  #              "SWZ", "TGO", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
  # 
  #if(!dir.exists(here("countries", cc, "logs"))) dir.create(here("countries", cc, "logs"), showWarnings = F)
  #if(!dir.exists(here("countries", cc, "output"))) dir.create(here("countries", cc, "output"), showWarnings = F)
  
})

parameters <- fread(paste0("./processing_files/paramsets/", cc, "_params.csv"))
#ranks <- fread(paste0("./processing_files/rank_uids/", cc, "_ranks.csv"))

vx_scenarios <- fread("./processing_files/econ_vx_scenarios_counttest.csv")

#rr_pct_data <- fread("./processing_files/RRTB_proportions.csv")
#rr_prop <- rr_pct_data[country == cc]$rr_pct

rr_prop <- 0.034


j = 1 # j is the parameter set to select

params <- parameters[j, ]
params <- params[, !c("uid", "n_hit")]
params <- unlist(params)


# 2. Run the BASELINE
model_paths <- model$set.paths(countries   = "countries",
                               countrycode  = cc,
                               xml          = "XMLinput_econ_baseline_bmgf_counttest.xml",
                               VXa.incidence.files = NA,
                               parameters   = "input.csv")

#starttime <- Sys.time()
output_BL = model$run(model, new.parameter.values = params, baseline = NULL, output.flows = F)
#Sys.time() - starttime

model$baseline_output <- output_BL



#### 3. Run the INTERVENTION (adolescent routine only)
i = 2
vx_chars <- vx_scenarios[i,]

#if (grepl("Neo", vx_chars$runtype) == TRUE) { 
#  vx_inc <- c(vx_chars$vac)
#} else {
#  vx_inc <- c(vx_chars$prev, vx_chars$vac)
#}

model_paths <- model$set.paths(countries   = "countries",
                               countrycode  = cc,
                               xml          = paste0(vx_chars$xml),
                               VXa.incidence.files = NA,
                               parameters   = "input.csv")

# Run the model with the parameter set
if (vx_chars$runtype != "baseline"){
  scen_baseline = model$baseline_output 
} else if (vx_chars$runtype == "baseline"){
  scen_baseline = NULL
}

#starttime <- Sys.time()
output <- model$run(model, new.parameter.values = params, baseline = scen_baseline, output.flows = F)
#Sys.time() - starttime


bgdeaths_bl <- output_BL$dBGx[output_BL$dBGx$year %% 1 ==0.5,]
bgdeaths_vx <- output$dBGx[output$dBGx$year %% 1 ==0.5,]
tbhivdeaths_bl <- output_BL$dHIVTBx[output_BL$dHIVTBx$year %% 1 ==0.5,]
tbhivdeaths_vx <- output$dHIVTBx[output$dHIVTBx$year %% 1 ==0.5,]


bg_deaths_vx <- as.data.table(bgdeaths_vx)
bg_deaths_vx <- bg_deaths_vx[year >= 2024]
bg_deaths_vx <- melt(bg_deaths_vx, id.vars = c("year", "country"),
                      variable.name = "AgeGrp", value.name = "BGdeaths_vx")

bg_deaths_bl <- as.data.table(bgdeaths_bl)
bg_deaths_bl <- bg_deaths_bl[year >= 2024]
bg_deaths_bl <- melt(bg_deaths_bl, id.vars = c("year", "country"),
                     variable.name = "AgeGrp", value.name = "BGdeaths_bl")


tbhiv_deaths_vx <- as.data.table(tbhivdeaths_vx)
tbhiv_deaths_vx <- tbhiv_deaths_vx[year >= 2024]
tbhiv_deaths_vx <- melt(tbhiv_deaths_vx, id.vars = c("year", "country"),
                         variable.name = "AgeGrp", value.name = "tbhivdeaths_vx")

tbhiv_deaths_bl <- as.data.table(tbhivdeaths_bl)
tbhiv_deaths_bl <- tbhiv_deaths_bl[year >= 2024]
tbhiv_deaths_bl <- melt(tbhiv_deaths_bl, id.vars = c("year", "country"),
                        variable.name = "AgeGrp", value.name = "tbhivdeaths_bl")


bgdeaths <- merge(bg_deaths_bl, bg_deaths_vx)
tbhivdeaths <- merge(tbhiv_deaths_bl, tbhiv_deaths_vx)


# BL deaths - Intervention deaths
# positive means more baseline deaths
# negative means more intervention deaths
bgdeaths <- bgdeaths[, Diff_BG := BGdeaths_bl - BGdeaths_vx]

tbhivdeaths <- tbhivdeaths[, Diff_tbhiv := tbhivdeaths_bl - tbhivdeaths_vx]


deaths <- merge(bgdeaths, tbhivdeaths)
deaths <- deaths[, Total_BL := tbhivdeaths_bl + BGdeaths_bl]
deaths <- deaths[, Total_vx := tbhivdeaths_vx + BGdeaths_vx]
deaths <- deaths[, Total_diff := Total_BL - Total_vx]
deaths_check <- deaths[, .(Diff_comb = sum(Total_diff)), by = .(year, country)]

cc_output <- here("concat_files", "concat_output", cc)


fwrite(tbhivdeaths, file.path(cc_output, paste0("/tbhivdeaths.csv")))
fwrite(deaths, file.path(cc_output, paste0("/deaths.csv")))
fwrite(deaths_check, file.path(cc_output, paste0("/deaths_check.csv")))
fwrite(bgdeaths, file.path(cc_output, paste0("/bgdeaths.csv")))






