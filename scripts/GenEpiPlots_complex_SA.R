#-------------------
# Epi single plots
# Rebecca Clark
# Updated 9 June 2022
#-------------------

# Set-up

suppressPackageStartupMessages({
  rm(list=ls())
  library(rlang)
  library(fs)
  library(data.table)
  library(ggplot2)
  library(cowplot)
  library(qs)
  library(uuid)
  library(gridExtra)
  library(ggpubr)
  library(digest)
  library(patchwork)
  library(dplyr, warn.conflicts = FALSE)
  options(dplyr.summarise.inform = FALSE)
  
  theme_set(theme_minimal_grid() + panel_border(color = "black"))
  
})


# Load in the rate_plots output from GenEpiUncertainty_complex.R

relative_output <- fread("./epi_output/grouped_output/relative_output_fin.csv")
relative_output <- relative_output[, .(Year, AgeGrp, Runtype, Indicator, medval, lowval, highval)]

raw_output <- fread("./epi_output/grouped_output/raw_output_fin.csv")
raw_output <- raw_output[, .(Year, AgeGrp, Runtype, Indicator, medval, lowval, highval)]

epi_estimates <- rbind(relative_output, raw_output)
library(tidyr)
#epi_estimates %>% drop_na

epi_estimates <- epi_estimates %>%
  # recode empty strings "" by NAs
  na_if("") %>%
  na.omit

epi_estimates <- epi_estimates[Indicator == "inc_RR", medval := medval*100]
epi_estimates <- epi_estimates[Indicator == "inc_RR", lowval := lowval*100]
epi_estimates <- epi_estimates[Indicator == "inc_RR", highval := highval*100]
epi_estimates <- epi_estimates[Indicator == "mort_RR", medval := medval*100]
epi_estimates <- epi_estimates[Indicator == "mort_RR", lowval := lowval*100]
epi_estimates <- epi_estimates[Indicator == "mort_RR", highval := highval*100]



# Subset to All ages and the key variables
epi_099 <- epi_estimates[AgeGrp == "[0,99]"]



# Relabel the values for Incidence and Mortality Rates
epi_099$Indicator  <- ordered(epi_099$Indicator,
                              levels = c("inc_rate", "mort_rate", "sum_inc", "sum_tx", "sum_mort",
                                         "inc_RR", "mort_RR", "inc_avert", "tx_avert", "mort_avert"),
                              labels = c("Incidence Rate", "Mortality Rate", "Cumulative Cases",
                                         "Cumulative Treatments", "Cumulative Deaths",
                                         "Incidence Rate Reduction", "Mortality Rate Reduction",
                                         "Cumulative Cases Averted", "Cumulative Treatments Averted",
                                         "Cumulative Deaths Averted"))

# source naming for M72 and BCG

epi_M72 <- epi_099[grepl("*M72*", epi_099$Runtype) | Runtype == "baseline"]
epi_BCG <- epi_099[grepl("*BCG*", epi_099$Runtype) | Runtype == "baseline"]


source("./EpiPlots_labels.R")

rate_reductions <- c("Incidence Rate Reduction", "Mortality Rate Reduction")
averted_numbers <- c("Cumulative Cases Averted", "Cumulative Treatments Averted",
                     "Cumulative Deaths Averted")



epi_M72_2050 <- epi_M72[Year == 2050]
epi_BCG_2050 <- epi_BCG[Year == 2050]



#---------------------------------------------------
# M72 scenario plots
#---------------------------------------------------

if (F){
  pdf("M72_rr_bc2.pdf",width = 12, height=6.5)
  ggplot(epi_M72_2050[Indicator %in% rate_reductions]) +
    labs(title=(expression(paste(M72/AS01[E],
                                 "-like vaccine scenario: Rate reductions in 2050")))) +
    geom_col(aes(x = Indicator, y = medval, fill = Runtype),
             position = position_dodge(), alpha = 0.5, width = 0.5) +
    geom_errorbar(aes(x = Indicator, ymin = lowval, ymax = highval, col = Runtype),
                  position = position_dodge(width = .5), size = 1, width = 0.25) +
    theme_minimal_grid() + guides(col = guide_legend(ncol=5)) +
    panel_border(color = "black") + 
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    theme(
      legend.text = element_text(size = 14),
      legend.title = element_blank(),
      legend.justification = "right",
      legend.position = "bottom",
      axis.title.x = element_blank(),
      axis.title.y = element_text(hjust = 1, size = 16),
      axis.text = element_text(size = 16)
    ) + ylab(label = "Rate Reduction (%)")  + 
    xlab(label = "Scenario") +
    scale_y_continuous(limits = c(0, 50), labels = scales::number_format(big.mark = ","))
  dev.off()
  
  
  pdf("M72_averted_bc.pdf",width = 12, height=6.5)
  ggplot(epi_M72_2050[Indicator %in% averted_numbers]) +
    labs(title=(expression(paste(M72/AS01[E],
                                 "-like vaccine scenario: Averted numbers by 2050")))) +
    geom_col(aes(x = Indicator, y = medval, fill = Runtype),
             position = position_dodge(), alpha = 0.5, width = 0.5) +
    geom_errorbar(aes(x = Indicator, ymin = lowval, ymax = highval, col = Runtype),
                  position = position_dodge(width = .5), size = 1, width = 0.25) +
    theme_minimal_grid() + guides(col = guide_legend(ncol=5)) +
    panel_border(color = "black") + 
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    theme(
      legend.text = element_text(size = 14),
      legend.title = element_blank(),
      legend.justification = "right",
      legend.position = "bottom",
      axis.title.x = element_blank(),
      axis.title.y = element_text(hjust = 1, size = 16),
      axis.text = element_text(size = 16)
    ) + ylab(label = "Numbers averted (1000s)")  + 
    scale_y_continuous(limits = c(0, 3000), labels = scales::number_format(big.mark = ","))
  dev.off()
  
}



#---------------------------------------------------
# BCG scenarios
#---------------------------------------------------

if (F){
  pdf("BCG_rr_bc.pdf",width = 12, height=6.5)
  ggplot(epi_BCG_2050[Indicator %in% rate_reductions]) +
    labs(title="BCG-revaccination-like vaccine scenario: Rate reductions in 2050") +
    geom_col(aes(x = Indicator, y = medval, fill = Runtype),
             position = position_dodge(), alpha = 0.5, width = 0.5) +
    geom_errorbar(aes(x = Indicator, ymin = lowval, ymax = highval, col = Runtype),
                  position = position_dodge(width = .5), size = 1, width = 0.25) +
    theme_minimal_grid() + guides(col = guide_legend(ncol=5)) +
    panel_border(color = "black") + 
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    theme(
      legend.text = element_text(size = 14),
      legend.title = element_blank(),
      legend.justification = "right",
      legend.position = "bottom",
      axis.title.x = element_blank(),
      axis.title.y = element_text(hjust = 1, size = 16),
      axis.text = element_text(size = 16)
    ) + ylab(label = "Rate Reduction (%)")  + 
    xlab(label = "Scenario") +
    scale_y_continuous(limits = c(0, 50), labels = scales::number_format(big.mark = ","))
  dev.off()
  
  pdf("BCG_averted_bc.pdf",width = 12, height=6.5)
  ggplot(epi_BCG_2050[Indicator %in% averted_numbers]) +
    labs(title="BCG-revaccination-like vaccine scenarios: Averted numbers by 2050") +
    geom_col(aes(x = Indicator, y = medval, fill = Runtype),
             position = position_dodge(), alpha = 0.5, width = 0.5) +
    geom_errorbar(aes(x = Indicator, ymin = lowval, ymax = highval, col = Runtype),
                  position = position_dodge(width = .5), size = 1, width = 0.25) +
    theme_minimal_grid() + guides(col = guide_legend(ncol=5)) +
    panel_border(color = "black") + 
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    theme(
      legend.text = element_text(size = 14),
      legend.title = element_blank(),
      legend.justification = "right",
      legend.position = "bottom",
      axis.title.x = element_blank(),
      axis.title.y = element_text(hjust = 1, size = 16),
      axis.text = element_text(size = 16)
    ) + ylab(label = "Numbers averted (1000s)")  + 
    scale_y_continuous(limits = c(0, 1500), labels = scales::number_format(big.mark = ","))
  dev.off()
  
}



#---------------------------------------------------
# EndTB scenarios
#---------------------------------------------------


endTB <- fread("./epi_output/grouped_output_endTB/relative_output_endTB.csv")
endTB <- endTB[Year == 2050 & AgeGrp == "[0,99]"][, .(Year, AgeGrp, Runtype, Indicator, medval, lowval, highval)]

endTB <- endTB[Indicator == "inc_RR", medval := medval*100]
endTB <- endTB[Indicator == "inc_RR", lowval := lowval*100]
endTB <- endTB[Indicator == "inc_RR", highval := highval*100]
endTB <- endTB[Indicator == "mort_RR", medval := medval*100]
endTB <- endTB[Indicator == "mort_RR", lowval := lowval*100]
endTB <- endTB[Indicator == "mort_RR", highval := highval*100]

endTB$Indicator  <- ordered(endTB$Indicator,
                            levels = c("inc_rate", "mort_rate", "sum_inc", "sum_tx", "sum_mort",
                                       "inc_RR", "mort_RR", "inc_avert", "tx_avert", "mort_avert"),
                            labels = c("Incidence Rate", "Mortality Rate", "Cumulative Cases",
                                       "Cumulative Treatments", "Cumulative Deaths",
                                       "Incidence Rate Reduction", "Mortality Rate Reduction",
                                       "Cumulative Cases Averted", "Cumulative Treatments Averted",
                                       "Cumulative Deaths Averted"))


# M72 plots
if (F){ 
  endTB_M72_2050 <- endTB[grepl("*M72*", endTB$Runtype)]
  epi_M72_endTB <- epi_M72_2050[Runtype == "Basecase"]
  epi_M72_endTB <- rbind(epi_M72_endTB, endTB_M72_2050)
  
  epi_M72_endTB$Runtype <- ordered(epi_M72_endTB$Runtype, 
                                   levels = c("Basecase", "M72_AI_POD_50_10yr_med_2030_endTB"),
                                   labels = c("Basecase - Status quo baseline",
                                              "Basecase - Existing intervention strengthening baseline"))
  
  ggplot(epi_M72_endTB[Indicator %in% rate_reductions]) +
    labs(title=(expression(paste(M72/AS01[E],
                                 "-like vaccine scenarios: Rate reductions in 2050")))) +
    geom_col(aes(x = Indicator, y = medval, fill = Runtype),
             position = position_dodge(), alpha = 0.5, width = 0.5) +
    geom_errorbar(aes(x = Indicator, ymin = lowval, ymax = highval, col = Runtype),
                  position = position_dodge(width = .5), size = 1, width = 0.25) +
    theme_minimal_grid() + guides(col = guide_legend(ncol=5)) +
    panel_border(color = "black") + 
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    theme(
      legend.text = element_text(size = 14),
      legend.title = element_blank(),
      legend.justification = "right",
      legend.position = "bottom",
      axis.title.x = element_blank(),
      axis.title.y = element_text(hjust = 1, size = 16),
      axis.text = element_text(size = 16)
    ) + ylab(label = "Rate Reduction (%)")  + 
    xlab(label = "Scenario") +
    scale_y_continuous(limits = c(0, 50), labels = scales::number_format(big.mark = ","))
  
  ggplot(epi_M72_endTB[Indicator %in% averted_numbers]) +
    labs(title=(expression(paste(M72/AS01[E],
                                 "-like vaccine scenarios: Averted numbers by 2050")))) +
    geom_col(aes(x = Indicator, y = medval, fill = Runtype),
             position = position_dodge(), alpha = 0.5, width = 0.5) +
    geom_errorbar(aes(x = Indicator, ymin = lowval, ymax = highval, col = Runtype),
                  position = position_dodge(width = .5), size = 1, width = 0.25) +
    theme_minimal_grid() + guides(col = guide_legend(ncol=5)) +
    panel_border(color = "black") + 
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    theme(
      legend.text = element_text(size = 14),
      legend.title = element_blank(),
      legend.justification = "right",
      legend.position = "bottom",
      axis.title.x = element_blank(),
      axis.title.y = element_text(hjust = 1, size = 16),
      axis.text = element_text(size = 16)
    ) + ylab(label = "Numbers averted (1000s)")  + 
    scale_y_continuous(limits = c(0, 15000), labels = scales::number_format(big.mark = ","))
  
  
}


# BCG plots
if (F){ 
  endTB_BCG_2050 <- endTB[grepl("*BCG*", endTB$Runtype)]
  epi_BCG_endTB <- epi_BCG_2050[Runtype == "Basecase"]
  epi_BCG_endTB <- rbind(epi_BCG_endTB, endTB_BCG_2050)
  
  epi_BCG_endTB$Runtype <- ordered(epi_BCG_endTB$Runtype, 
                                   levels = c("Basecase", "BCG_NCI_POI_45_10yr_med_2025_endTB"),
                                   labels = c("Basecase - Status quo baseline",
                                              "Basecase - Existing intervention strengthening baseline"))
  
  ggplot(epi_BCG_endTB[Indicator %in% rate_reductions]) +
    labs(title="BCG-revaccination-like vaccine scenarios: Rate reductions in 2050") +
    geom_col(aes(x = Indicator, y = medval, fill = Runtype),
             position = position_dodge(), alpha = 0.5, width = 0.5) +
    geom_errorbar(aes(x = Indicator, ymin = lowval, ymax = highval, col = Runtype),
                  position = position_dodge(width = .5), size = 1, width = 0.25) +
    theme_minimal_grid() + guides(col = guide_legend(ncol=5)) +
    panel_border(color = "black") + 
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    theme(
      legend.text = element_text(size = 14),
      legend.title = element_blank(),
      legend.justification = "right",
      legend.position = "bottom",
      axis.title.x = element_blank(),
      axis.title.y = element_text(hjust = 1, size = 16),
      axis.text = element_text(size = 16)
    ) + ylab(label = "Rate Reduction (%)")  + 
    xlab(label = "Scenario") +
    scale_y_continuous(limits = c(0, 50), labels = scales::number_format(big.mark = ","))
  
  ggplot(epi_BCG_endTB[Indicator %in% averted_numbers]) +
    labs(title="BCG-revaccination-like vaccine scenarios: Rate reductions in 2050") +
    geom_col(aes(x = Indicator, y = medval, fill = Runtype),
             position = position_dodge(), alpha = 0.5, width = 0.5) +
    geom_errorbar(aes(x = Indicator, ymin = lowval, ymax = highval, col = Runtype),
                  position = position_dodge(width = .5), size = 1, width = 0.25) +
    theme_minimal_grid() + guides(col = guide_legend(ncol=5)) +
    panel_border(color = "black") + 
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    theme(
      legend.text = element_text(size = 14),
      legend.title = element_blank(),
      legend.justification = "right",
      legend.position = "bottom",
      axis.title.x = element_blank(),
      axis.title.y = element_text(hjust = 1, size = 16),
      axis.text = element_text(size = 16)
    ) + ylab(label = "Numbers averted (1000s)")  + 
    scale_y_continuous(limits = c(0, 15000), labels = scales::number_format(big.mark = ","))
  
  
}



#--- end



# old / the plots where they're all on the same figure (pdf in slack)
if (F){
  
  
  
  
  
  pdf("M72_all_fin.pdf",width = 12, height=10.5)
  vr <- 2000
 
  M72plots <- ggplot(epi_M72_2050[Indicator %in% rate_reductions | Indicator %in% averted_numbers]) +
    ggtitle("M72/AS01E like vaccine scenarios") +
    geom_col(aes(x = Runtype, y = medval, fill = Runtype),
             position = position_dodge(), alpha = 0.5, width = 0.5) +
    geom_errorbar(aes(x = Runtype, ymin = lowval, ymax = highval, col = Runtype),
                  position = position_dodge(width = .5), size = 1, width = 0.25) +
    geom_blank(data=data.frame(Indicator = "Cumulative Cases Averted",
                               Runtype = "Basecase",
                               medval = vr), aes(x = Runtype, y = medval)) +
    geom_blank(data=data.frame(Indicator = "Cumulative Treatments Averted",
                               Runtype = "Basecase",
                               medval = vr), aes(x = Runtype, y = medval)) +
    geom_blank(data=data.frame(Indicator = "Cumulative Deaths Averted",
                               Runtype = "Basecase",
                               medval = vr), aes(x = Runtype, y = medval)) +
    geom_blank(data=data.frame(Indicator = "Incidence Rate Reduction",
                               Runtype = "Basecase",
                               medval = 50), aes(x = Runtype, y = medval)) +
    geom_blank(data=data.frame(Indicator = "Mortality Rate Reduction",
                               Runtype = "Basecase",
                               medval = 50), aes(x = Runtype, y = medval)) +
    
    theme_minimal_grid() + guides(col = guide_legend(ncol=3)) +
    facet_wrap(~ Indicator, nrow = 5, scales = "free_y") +
    panel_border(color = "black") + 
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    theme(
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      legend.justification = "right",
      legend.position = "none",
      axis.title.x = element_text(hjust = 1, size = 14),
      axis.title.y = element_text(hjust = 1, size = 14),
      axis.text = element_text(size = 12, hjust = 1, angle = 45)
    ) + ylab(label = " Rate Reduction (%)                                                                 Numbers averted (1000s)              ")  + 
    xlab(label = "Scenario") +
    scale_y_continuous(limits = c(0, NA), labels = scales::number_format(big.mark = ","))
  
  M72plots
  dev.off()
  
  
  
  #E hi
  pdf("BCG_all_fin.pdf",width = 15, height=15.5)
  vr <- 1500
  BCGplots <- ggplot(epi_BCG_2050[Indicator %in% rate_reductions | Indicator %in% averted_numbers]) +
    ggtitle("BCG-revaccination like vaccine scenarios") +
    geom_col(aes(x = Runtype, y = medval, fill = Runtype),
             position = position_dodge(), alpha = 0.5, width = 0.5) +
    geom_errorbar(aes(x = Runtype, ymin = lowval, ymax = highval, col = Runtype),
                  position = position_dodge(width = .5), size = 1, width = 0.25) +
    geom_blank(data=data.frame(Indicator = "Cumulative Cases Averted",
                               Runtype = "Basecase",
                               medval = vr), aes(x = Runtype, y = medval)) +
    geom_blank(data=data.frame(Indicator = "Cumulative Treatments Averted",
                               Runtype = "Basecase",
                               medval = vr), aes(x = Runtype, y = medval)) +
    geom_blank(data=data.frame(Indicator = "Cumulative Deaths Averted",
                               Runtype = "Basecase",
                               medval = vr), aes(x = Runtype, y = medval)) +
    geom_blank(data=data.frame(Indicator = "Incidence Rate Reduction",
                               Runtype = "Basecase",
                               medval = 50), aes(x = Runtype, y = medval)) +
    geom_blank(data=data.frame(Indicator = "Mortality Rate Reduction",
                               Runtype = "Basecase",
                               medval = 50), aes(x = Runtype, y = medval)) +
    theme_minimal_grid() + guides(col = guide_legend(ncol=3)) +
    facet_wrap(~ Indicator, nrow = 5, scales = "free_y") +
    panel_border(color = "black") + 
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    theme(
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      legend.justification = "right",
      legend.position = "none",
      axis.title.x = element_text(hjust = 1, size = 14),
      axis.title.y = element_text(hjust = 1, size = 14),
      axis.text = element_text(size = 12, hjust = 1, angle = 45)
    ) + ylab(label = " Rate Reduction (%)                                                                 Numbers averted (1000s)              ")  + 
    xlab(label = "Scenario") +
    scale_y_continuous(limits = c(0, NA), labels = scales::number_format(big.mark = ","))
  BCGplots
  dev.off()
  
  pdf(paste0("../manuscript/results/relative_output_plots.pdf"), height = 11.7, width = 8.3)
  print(M72plots)
  print(BCGplots)
  dev.off()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##### M72 PLOTS
  ggplot(epi_M72) + 
    geom_line(aes(x = Year, y = medval, col = Runtype), lwd = 1) + 
    ylim(0, NA) + xlim(2029, 2050) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval, fill = Runtype), alpha = 0.1) +
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    ylab("Numbers averted (1000s)") +
    facet_wrap(. ~ Indicator, nrow = 5, scales="free_y") +
    theme(
      legend.position = "bottom", legend.direction = "vertical",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      axis.title.x = element_text(hjust = 1, size = 14),
      axis.title.y = element_text(hjust = 1, size = 14),
      axis.text = element_text(angle = 0, hjust = 0.5, size = 14)) +
    guides(color = guide_legend(ncol = 6, byrow = TRUE))
  
  ##add
  
  
  ggplot(epi_M72[Runtype %in% coverage]) + 
    geom_line(aes(x = Year, y = medval, col = Runtype), lwd = 1) + ylim(0, NA) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval, fill = Runtype), alpha = 0.1) +
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    ylab("Numbers averted (1000s)") +
    facet_wrap(. ~ Indicator, scales="free_y", nrow = 5) +
    theme(
      legend.position = "bottom", legend.direction = "vertical",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      axis.title.x = element_text(hjust = 1, size = 14),
      axis.title.y = element_text(hjust = 1, size = 14),
      axis.text = element_text(angle = 0, hjust = 0.5, size = 14)) +
    guides(color = guide_legend(ncol = 4, byrow = TRUE))
  
  
  
  ggplot(epi_M72[Runtype %in% coverage & Indicator %in% rate_reductions]) + 
    geom_line(aes(x = Year, y = medval, col = Runtype), lwd = 1) + ylim(0, NA) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval, fill = Runtype), alpha = 0.1) +
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    ylab("Numbers averted (1000s)") +
    facet_wrap(. ~ Indicator, scales="free_y", nrow = 5) +
    theme(
      legend.position = "bottom", legend.direction = "vertical",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      axis.title.x = element_text(hjust = 1, size = 14),
      axis.title.y = element_text(hjust = 1, size = 14),
      axis.text = element_text(angle = 0, hjust = 0.5, size = 14)) +
    guides(color = guide_legend(ncol = 4, byrow = TRUE))
  
  
  ggplot(epi_M72[Runtype %in% duration & Indicator %in% rate_reductions]) + 
    geom_line(aes(x = Year, y = medval, col = Runtype), lwd = 1) + ylim(0, NA) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval, fill = Runtype), alpha = 0.1) +
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    ylab("Numbers averted (1000s)") +
    facet_wrap(. ~ Indicator, scales="free_y", nrow = 5) +
    theme(
      legend.position = "bottom", legend.direction = "vertical",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      axis.title.x = element_text(hjust = 1, size = 14),
      axis.title.y = element_text(hjust = 1, size = 14),
      axis.text = element_text(angle = 0, hjust = 0.5, size = 14)) +
    guides(color = guide_legend(ncol = 4, byrow = TRUE))
  
  
  ggplot(epi_M72[Runtype %in% mecheffect & Indicator %in% rate_reductions]) + 
    geom_line(aes(x = Year, y = medval, col = Runtype), lwd = 1) + ylim(0, NA) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval, fill = Runtype), alpha = 0.1) +
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    ylab("Numbers averted (1000s)") +
    facet_wrap(. ~ Indicator, scales="free_y", nrow = 5) +
    theme(
      legend.position = "bottom", legend.direction = "vertical",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      axis.title.x = element_text(hjust = 1, size = 14),
      axis.title.y = element_text(hjust = 1, size = 14),
      axis.text = element_text(angle = 0, hjust = 0.5, size = 14)) +
    guides(color = guide_legend(ncol = 4, byrow = TRUE))
  
  
  ggplot(epi_M72[Runtype %in% efficacy & Indicator %in% rate_reductions]) + 
    geom_line(aes(x = Year, y = medval, col = Runtype), lwd = 1) + ylim(0, NA) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval, fill = Runtype), alpha = 0.1) +
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    ylab("Numbers averted (1000s)") +
    facet_wrap(. ~ Indicator, scales="free_y", nrow = 5) +
    theme(
      legend.position = "bottom", legend.direction = "vertical",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      axis.title.x = element_text(hjust = 1, size = 14),
      axis.title.y = element_text(hjust = 1, size = 14),
      axis.text = element_text(angle = 0, hjust = 0.5, size = 14)) +
    guides(color = guide_legend(ncol = 4, byrow = TRUE))
  
  
  
  
  
  
  
  
  
  
  ##### BCG PLOTS
  
  ggplot(epi_BCG) + 
    geom_line(aes(x = Year, y = medval, col = Runtype), lwd = 1) + ylim(0, NA) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval, fill = Runtype), alpha = 0.1) +
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    ylab("Numbers averted (1000s)") +
    facet_wrap(. ~ Indicator, scales="free_y", nrow = 5) +
    theme(
      legend.position = "bottom", legend.direction = "vertical",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      axis.title.x = element_text(hjust = 1, size = 14),
      axis.title.y = element_text(hjust = 1, size = 14),
      axis.text = element_text(angle = 0, hjust = 0.5, size = 14)) +
    guides(color = guide_legend(ncol = 4, byrow = TRUE))
  
  
  ggplot(epi_BCG[Runtype %in% coverage]) + 
    geom_line(aes(x = Year, y = medval, col = Runtype), lwd = 1) + ylim(0, NA) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval, fill = Runtype), alpha = 0.1) +
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    ylab("Numbers averted (1000s)") +
    facet_wrap(. ~ Indicator, scales="free_y", nrow = 5) +
    theme(
      legend.position = "bottom", legend.direction = "vertical",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      axis.title.x = element_text(hjust = 1, size = 14),
      axis.title.y = element_text(hjust = 1, size = 14),
      axis.text = element_text(angle = 0, hjust = 0.5, size = 14)) +
    guides(color = guide_legend(ncol = 4, byrow = TRUE))
  
  
  
  ggplot(epi_BCG[Runtype %in% coverage & Indicator %in% rate_reductions]) + 
    geom_line(aes(x = Year, y = medval, col = Runtype), lwd = 1) + ylim(0, NA) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval, fill = Runtype), alpha = 0.1) +
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    ylab("Numbers averted (1000s)") +
    facet_wrap(. ~ Indicator, scales="free_y", nrow = 5) +
    theme(
      legend.position = "bottom", legend.direction = "vertical",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      axis.title.x = element_text(hjust = 1, size = 14),
      axis.title.y = element_text(hjust = 1, size = 14),
      axis.text = element_text(angle = 0, hjust = 0.5, size = 14)) +
    guides(color = guide_legend(ncol = 4, byrow = TRUE))
  
  
  ggplot(epi_BCG[Runtype %in% duration & Indicator %in% rate_reductions]) + 
    geom_line(aes(x = Year, y = medval, col = Runtype), lwd = 1) + ylim(0, NA) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval, fill = Runtype), alpha = 0.1) +
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    ylab("Numbers averted (1000s)") +
    facet_wrap(. ~ Indicator, scales="free_y", nrow = 5) +
    theme(
      legend.position = "bottom", legend.direction = "vertical",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      axis.title.x = element_text(hjust = 1, size = 14),
      axis.title.y = element_text(hjust = 1, size = 14),
      axis.text = element_text(angle = 0, hjust = 0.5, size = 14)) +
    guides(color = guide_legend(ncol = 4, byrow = TRUE))
  
  
  ggplot(epi_BCG[Runtype %in% mecheffect & Indicator %in% rate_reductions]) + 
    geom_line(aes(x = Year, y = medval, col = Runtype), lwd = 1) + ylim(0, NA) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval, fill = Runtype), alpha = 0.1) +
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    ylab("Numbers averted (1000s)") +
    facet_wrap(. ~ Indicator, scales="free_y", nrow = 5) +
    theme(
      legend.position = "bottom", legend.direction = "vertical",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      axis.title.x = element_text(hjust = 1, size = 14),
      axis.title.y = element_text(hjust = 1, size = 14),
      axis.text = element_text(angle = 0, hjust = 0.5, size = 14)) +
    guides(color = guide_legend(ncol = 4, byrow = TRUE))
  
  
  ggplot(epi_BCG[Runtype %in% efficacy & Indicator %in% rate_reductions]) + 
    geom_line(aes(x = Year, y = medval, col = Runtype), lwd = 1) + ylim(0, NA) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval, fill = Runtype), alpha = 0.1) +
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    ylab("Numbers averted (1000s)") +
    facet_wrap(. ~ Indicator, scales="free_y", nrow = 5) +
    theme(
      legend.position = "bottom", legend.direction = "vertical",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      axis.title.x = element_text(hjust = 1, size = 14),
      axis.title.y = element_text(hjust = 1, size = 14),
      axis.text = element_text(angle = 0, hjust = 0.5, size = 14)) +
    guides(color = guide_legend(ncol = 4, byrow = TRUE))
  
  
  
  #-----end
  
  
  mort <- raw_output[Indicator == "sum_mort"]
  sum_epi_wide <- dcast(mort, Year + AgeGrp + Indicator ~ Runtype,
                        value.var = "medval")
  
  vx_scenarios <- unique(mort$Runtype)
  vx_scenarios <- vx_scenarios[vx_scenarios  != "baseline"]
  
  for (scen in vx_scenarios) {
    set(x = sum_epi_wide, j = paste0("Diff_", scen), value = sum_epi_wide[["baseline"]] - sum_epi_wide[[paste0(scen)]])
  }
  
}
