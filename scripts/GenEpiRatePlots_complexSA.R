#-------------------
# Epi single plots
# Rebecca Clark
# 22 November 2021
#-------------------
rm(list=ls())
# Set-up
suppressPackageStartupMessages({
  library(rlang)
  library(fs)
  library(fst)
  library(data.table)
  library(drake)
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
})


# Load in the rate_plots output from GenEpiUncertainty_complex.R
epi_output <- fread("./epi_output/grouped_output/rate_plots_28022022BCG.csv")

# Subset to All ages and the key variables
epi_099 <- epi_output[AgeGrp == "[0,99]"]
epi_099 <- epi_099[, .(Year, UID, Runtype, inc_rate, mort_rate)]

# Melt and then calculate the median, low and high bounds 
#            for Incidence and Mortality rates
epi_099 <- melt(epi_099, id.vars = c("Year", "Runtype", "UID"),
                measure.vars = c("inc_rate", "mort_rate"),
                variable.name = "Indicator", value.name = "Value")

epi_099 <- epi_099[, .(med_rate = median(Value),
                       low_rate = quantile(Value, 0.025),
                       high_rate = quantile(Value, 0.975)),
                   .(Year, Runtype, Indicator)]

# Subset to data after 2010
epi_099 <- epi_099[Year >=2020]


# Relabel the values for Incidence and Mortality Rates
epi_099$Indicator  <- ordered(epi_099$Indicator ,
                     levels = c("inc_rate", "mort_rate"),
                     labels = c("Incidence Rate", "Mortality Rate"))

# Rename the vaccine scenarios


soi_eff <- c("M72_CI_POD_55_10yr_2030_adoadu","M72_CI_POD_60_10yr_2030_adoadu",
             "M72_CI_POD_75_10yr_2030_adoadu")

# soi_dur <- c("M72_CI_POD_55_3yr_2030_adoadu","M72_CI_POD_55_10yr_2030_adoadu",
#              "M72_CI_POD_55_15yr_2030_adoadu", "M72_CI_POD_55_LL_2030_adoadu",
#              "baseline")

soi_dur <- c("BCG_NCI_POI_45_3yr_2027_ado","BCG_NCI_POI_45_10yr_2027_ado",
             "BCG_NCI_POI_45_15_2027_ado", "BCG_NCI_POI_45_LL_2027_ado",
             "baseline")

epi_099 <- epi_099 %>%
  filter(Runtype %in% soi_dur)

epi_099$Runtype  <- ordered(epi_099$Runtype,
                            # levels = c("M72_CI_POD_55_3yr_2030_adoadu",
                            #            "M72_CI_POD_55_10yr_2030_adoadu",
                            #            "M72_CI_POD_55_15yr_2030_adoadu",
                            #            "M72_CI_POD_55_LL_2030_adoadu",
                            #            "baseline"),
                            levels = c("BCG_NCI_POI_45_3yr_2027_ado",
                                       "BCG_NCI_POI_45_10yr_2027_ado",
                                       "BCG_NCI_POI_45_15_2027_ado",
                                       "BCG_NCI_POI_45_LL_2027_ado",
                                       "baseline"),
                            labels = c("3 yr",
                                       "10 yr",
                                       "15 yr",
                                       "Life long",
                                       "No new vaccine"))
# Subset the epi_099 dataset

# Plot
#epi_M72 <- epi_099
epi_BCG <- epi_099
## M72 and baseline plot

pdf(file = "./SA_plot_BCG.pdf",   # The directory you want to save the file in
    width = 11.5, # The width of the plot in inches
    height = 4)


ggplot(epi_BCG) + 
  #ggtitle("South Africa") +
  geom_line(aes(x = Year, y = med_rate, col = Runtype),lwd=1.1) + ylim(0, NA) +
#  geom_ribbon(aes(x = Year, ymin = low_rate, ymax = high_rate, fill = Runtype), alpha = 0.4) +
  scale_colour_viridis_d(direction = -1, option = "plasma") +
  scale_fill_viridis_d(direction = -1, option = "plasma") +
  #ylab("Rate per 100,000 population") +
  labs(y = "Rate per 100,000 population", x = "Year")+
  #facet_grid(Indicator ~ ., scales="free") +
  facet_wrap(Indicator ~ ., scales="free") +
  theme_bw() +
  #theme(strip.background = element_blank()) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.title.x = element_text( size = 16),
    axis.title.y = element_text( size = 16),
    axis.text = element_text(angle = 0, hjust = 0.5, size = 16),
    strip.text = element_text(size = 20),
    strip.placement = "outside")


dev.off()


