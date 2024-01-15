#-------------------
# Epi numerical results
# Rebecca Clark
# Updated 26 June 2022
#-------------------

# Set-up

suppressPackageStartupMessages({
  rm(list=ls())
  
  library(data.table)
  
})


relative_output <- fread("./epi_output/grouped_output/relative_output_M72.csv")
relative_output <- relative_output[, .(Year, AgeGrp, Runtype, Indicator, medval, lowval, highval)]

relative_output <- relative_output[Year == 2050 & AgeGrp == "[0,99]"]

relative_output <- relative_output[grepl("*RR*", relative_output$Indicator),
                                   UR := paste0(format(round(medval*100, 2), nsmall = 1), "% (",
                                                format(round(lowval*100, 2), nsmall = 1), "–",
                                                format(round(highval*100, 2), nsmall = 1), ")")]

relative_output <- relative_output[grepl("*avert*", relative_output$Indicator),
                                   UR := paste0(format(round((medval*1000)/1e6, 2), nsmall = 1), "m (",
                                                format(round((lowval*1000)/1e6, 2), nsmall = 1), "–",
                                                format(round((highval*1000)/1e6, 2), nsmall = 1), ")")]





fwrite(relative_output, "C:/Users/ChristinahMukandavir/Documents/manuscript/results/relative_output_2050.csv")



relative_output_wide <- dcast(relative_output, Runtype ~ Indicator, value.var = "UR")

relative_output_wide <- relative_output_wide[, .(Runtype, inc_RR, mort_RR, inc_avert, tx_avert, mort_avert)]



relative_output_wide$Runtype <- 
  ordered(relative_output_wide$Runtype,
          levels = c("baseline", "M72_AI_POD_50_10yr_med_2030",
                     "M72_AI_POD_60_10yr_med_2030", "M72_AI_POD_70_10yr_med_2030",
                     "M72_AI_POD_50_5yr_med_2030",
                     "M72_AI_POD_50_15yr_med_2030", "M72_AI_POD_50_20yr_med_2030",
                     "M72_AI_POID_50_10yr_med_2030", "M72_CI_POD_50_10yr_med_2030",
                     "M72_AI_POD_50_10yr_med_2034",
                     "M72_AI_POD_50_10yr_med_2030_diffages", 
                     "M72_AI_POD_50_10yr_med_2030_young",
                     "M72_AI_POD_50_10yr_low_2030", "M72_AI_POD_50_10yr_high_2030",
                     
                     "M72_AI_POD_50_10yr_med_2030_hivsame",
                     "M72_AI_POD_50_10yr_med_2030_hivpess"
          ),
          labels = c("No-New-Vaccine", "Basecase",
                     "60% efficacy", "70% efficacy", 
                     "5 years duration of protection",
                     "15 years duration of protection", "20 years duration of protection",
                     "Prevention of infection & disease", "Efficacy with current infection at vaccination", 
                     "2034 introduction",
                     "Different Ages (campaign 18-55)", "Younger age groups",
                     "Low coverage", "High coverage",
                     
                     "Same efficacy in PLHIV", "Low efficacy in PLHIV"))







fwrite(relative_output_wide, "C:/Users/ChristinahMukandavir/Documents/manuscript/results/relative_output_wide_2050.csv")

#----end