#-------------------
# GenEpiUncertainty_complex.R
# Version 1
# Last updated 19 November 2021
#-------------------


# 1. Set-up
suppressPackageStartupMessages({
  rm(list=ls())
  library(here)
  library(data.table)
  library(renv)
  library(git2r)
  library(arrow)
  library(magrittr)
  if (!require("getopt")) install.packages("getopt")
  library(getopt)
  
})

# Create a folder to save the output
if (!dir.exists(here("epi_output/grouped_output"))) dir.create(here("epi_output/grouped_output"))


# Load in data generated from GenEpiOutput_complex.R and correct the year variable
n_epi <- open_dataset(sources = "./epi_output/n_epi/") %>% dplyr::collect()
n_epi <- setDT(n_epi[, Year := Year - 0.5])


#-------------------------------------------------------------------------------------------

# 1. Data for plotting incidence and mortality rates over time
rate_plots <- n_epi
rate_plots <- rate_plots[, `:=`(inc_rate = (N_inc/Population)*(100000),
                                mort_rate = (N_mort/Population)*(100000)),
                         by = .(Year, AgeGrp, Runtype, UID)]

fwrite(rate_plots, "epi_output/grouped_output/rate_plots_28022022M72.csv")

#-------------------------------------------------------------------------------------------

# 2. Data for calculating incidence and mortality rate reductions (compared to baseline)
#       at specific years

# specific years
RR_years <- c(2035, 2050)

# subset to those years and calculate the incidence and mortality rates
rate_epi <- n_epi[Year %in% RR_years]
rate_epi <- rate_epi[, `:=`(inc_rate = (N_inc/Population)*(100000),
                            mort_rate = (N_mort/Population)*(100000)),
                     by = .(Year, AgeGrp, Runtype, UID)]


rate_epi <- as.data.table(rate_epi)
# Melt to long form (create another variable that is either inc_rate or mort_rate)
rate_epi_long <- melt(rate_epi, measure.vars = c("inc_rate", "mort_rate"), 
                     id.vars = c("Year", "AgeGrp", "Runtype", "UID"),
                     value.name = "Value", variable.name = "Indicator")


# Cast to wide form to calculate the rate reductions
rate_epi_wide <- dcast(rate_epi_long, Year + UID + AgeGrp + Indicator ~ Runtype,
                      value.var = "Value")


# Calculate the rate reductions (scenario compared to baseline)
vx_scenarios <- unique(rate_epi$Runtype)
vx_scenarios <- vx_scenarios[vx_scenarios != "baseline"]

rate_epi_wide <- setDT(rate_epi_wide)
for (scen in vx_scenarios) {
  set(x = rate_epi_wide, j = paste0(scen, "_diff"), value = rate_epi_wide[["baseline"]] - rate_epi_wide[[paste0(scen)]])
  set(x = rate_epi_wide, j = paste0("PER_", scen), value = rate_epi_wide[[paste0(scen, "_diff")]] / rate_epi_wide[["baseline"]])
  }


# Melt back to long form with the rate reduction variable
rate_reductions <- melt(data = rate_epi_wide, measure.vars = patterns("^PER.*"), 
                        id.vars = c("UID", "AgeGrp", "Year", "Indicator"),
                        value.name = "Value", variable.name = "Runtype")

# Calculate the median, upper, and lower bounds (Uncertainty from the UIDs)
rate_reductions <- rate_reductions[, .(medval = median(Value),
                                       lowval = quantile(Value, 0.025),
                                       highval = quantile(Value, 0.975)),
                                   by = .(Year, Runtype, AgeGrp, Indicator)]


rate_reductions <- rate_reductions[, combined := paste0(round(medval*100, 1),
                                                        "% (", round(lowval*100, 1),
                                                        ", ", round(highval*100, 1), ")")]

rate_reductions$Runtype <- gsub("PER_", "", as.character(rate_reductions$Runtype))


#-------------------------------------------------------------------------------------------

# 3. Data for plotting cumulative treatments, cases, and deaths over time (in 1000s)

### Have to start from where the intervention XML starts output from
sum_plots <- n_epi[Year >= 2024] 

sum_plots <- sum_plots[, `:=`(sum_tx = cumsum(N_tx),
                              sum_inc = cumsum(N_inc),
                              sum_mort = cumsum(N_mort)),
                       by = .(AgeGrp, Runtype, UID)]

fwrite(sum_plots, "./epi_output/grouped_output/sum_plots_28022022M72.csv")


#-------------------------------------------------------------------------------------------

# 4.  Data for calculating cumulative tx, cases, and deaths averted by specific years

# Cumulative number of tx/inc/mort between vaccine introduction and current year

### Have to start from where the intervention XML starts output from
sum_epi <- n_epi[Year >= 2024] 

sum_epi <- sum_epi[, `:=`(sum_tx = cumsum(N_tx),
                          sum_inc = cumsum(N_inc),
                          sum_mort = cumsum(N_mort)),
                   by = .(AgeGrp, Runtype, UID)]


# Subset to specific years (AFTER calculation above)
sum_years <- c(2035, 2050)
sum_epi <- sum_epi[Year %in% sum_years]

sum_epi_long <- melt(sum_epi, measure.vars = c("sum_tx", "sum_inc", "sum_mort"), 
                     id.vars = c("Year", "AgeGrp", "Runtype", "UID"),
                     value.name = "Value", variable.name = "Indicator")

sum_epi_wide <- dcast(sum_epi_long, Year + UID + AgeGrp + Indicator ~ Runtype,
                      value.var = "Value")

for (scen in vx_scenarios) {
  set(x = sum_epi_wide, j = paste0("Diff_", scen), value = sum_epi_wide[["baseline"]] - sum_epi_wide[[paste0(scen)]])
}


sum_averted <- melt(data = sum_epi_wide, measure.vars = patterns("^Diff.*"), 
                    id.vars = c("Year", "UID", "AgeGrp", "Indicator"),
                    value.name = "Value", variable.name = "Runtype")

sum_averted <- sum_averted[, .(medval = median(Value),
                               lowval = quantile(Value, 0.025),
                               highval = quantile(Value, 0.975)),
                           by = .(Year, Runtype, Indicator, AgeGrp)]

sum_averted <- sum_averted[, combined := paste0(round(medval, 1),
                                                 " (", round(lowval, 1),
                                                 ", ", round(highval, 1), ")")]

sum_averted$Runtype <- gsub("Diff_", "", as.character(sum_averted$Runtype))


#-------------------------------------------------------------------------------------------


epi_output <- rbind(rate_reductions, sum_averted)

fwrite(epi_output, "epi_output/grouped_output/epi_output_28022022M72.csv")



# ---- end

