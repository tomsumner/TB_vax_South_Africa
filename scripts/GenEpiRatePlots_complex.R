#-------------------
# Epi single plots
# Rebecca Clark
# 22 November 2021
#-------------------

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
epi_output <- fread("./epi_output/grouped_output/rate_plots_24112021.csv")

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
epi_099 <- epi_099[Year >=2010]


# Relabel the values for Incidence and Mortality Rates
epi_099$Indicator  <- ordered(epi_099$Indicator ,
                     levels = c("inc_rate", "mort_rate"),
                     labels = c("Incidence Rate", "Mortality Rate"))

# Rename the vaccine scenarios
epi_099$Runtype  <- ordered(epi_099$Runtype,
                              levels = c("M72_med_10y_PPI_AdoAdu",
                                         "BCG_med_10y_PRI_Ado",
                                         "baseline"),
                              labels = c("M72/AS01E-like Vaccine",
                                         "BCG-like Vaccine",
                                         "No New Vaccine"))

# Plot
# ggplot(epi_099) +
#   geom_line(aes(x = Year, y = med_rate, col = Runtype)) + ylim(0, NA) +
#   geom_ribbon(aes(x = Year, ymin = low_rate,ymax = high_rate, fill = Runtype), alpha = 0.4) +
#   scale_colour_viridis_d(direction = -1) +
#   scale_fill_viridis_d(direction = -1) +
#   ylab("Rate per 100,000 population") +
#   facet_grid(Indicator ~ ., scales="free") +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     legend.text = element_text(size = 10),
#     axis.title.x = element_text(hjust = 1, size = 14),
#     axis.title.y = element_text(hjust = 1, size = 14),
#     axis.text = element_text(angle = 0, hjust = 0.5, size = 14))




# ggplot(epi_099) + 
#   ggtitle("South Africa") +
#   geom_line(aes(x = Year, y = med_rate, col = Runtype)) + ylim(0, NA) +
#   geom_ribbon(aes(x = Year, ymin = low_rate,ymax = high_rate, fill = Runtype), alpha = 0.4) +
#   scale_colour_viridis_d(direction = -1, option = "plasma") +
#   scale_fill_viridis_d(direction = -1, option = "plasma") +
#   ylab("Rate per 100,000 population") +
#   facet_grid(Indicator ~ ., scales="free") +
#   theme_bw() +
#   theme(strip.background = element_blank()) +
#     theme(
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     legend.text = element_text(size = 12),
#     axis.title.x = element_text(hjust = 1, size = 14),
#     axis.title.y = element_text(hjust = 1, size = 14),
#     axis.text = element_text(angle = 0, hjust = 0.5, size = 14),
#   
#    )
# # --- end



# pdf(file = "./SA_plot3.pdf",   # The directory you want to save the file in
#     width = 8.5, # The width of the plot in inches
#     height = 9.0)
# 
# 
# ggplot(epi_099) + 
#   ggtitle("South Africa") +
#   geom_line(aes(x = Year, y = med_rate, col = Runtype)) + ylim(0, NA) +
#   geom_ribbon(aes(x = Year, ymin = low_rate, ymax = high_rate, fill = Runtype), alpha = 0.4) +
#   scale_colour_viridis_d(direction = -1, option = "plasma") +
#   scale_fill_viridis_d(direction = -1, option = "plasma") +
#   ylab("Rate per 100,000 population") +
#   facet_grid(Indicator ~ ., scales="free") +
#   theme_bw() +
#   theme(strip.background = element_blank()) +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     legend.text = element_text(size = 14),
#     axis.title.x = element_text(hjust = 1, size = 14),
#     axis.title.y = element_text(hjust = 1, size = 14),
#     axis.text = element_text(angle = 0, hjust = 0.5, size = 14))
# 
# 
# 
# dev.off()


# Subset the epi_099 dataset

epi_M72 <- epi_099[Runtype == "No New Vaccine" | Runtype == "M72/AS01E-like Vaccine"]
epi_BCG <- epi_099[Runtype == "No New Vaccine" | Runtype == "BCG-like Vaccine"]

# Plot

## M72 and baseline plot

pdf(file = "./SA_plot_M722.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 11)


ggplot(epi_M72) + 
  ggtitle("South Africa") +
  geom_line(aes(x = Year, y = med_rate, col = Runtype)) + ylim(0, NA) +
  geom_ribbon(aes(x = Year, ymin = low_rate, ymax = high_rate, fill = Runtype), alpha = 0.4) +
  scale_colour_viridis_d(direction = -1, option = "plasma") +
  scale_fill_viridis_d(direction = -1, option = "plasma") +
  ylab("Rate per 100,000 population") +
  facet_grid(Indicator ~ ., scales="free") +
  theme_bw() +
  theme(strip.background = element_blank()) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.title.x = element_text(hjust = 1, size = 14),
    axis.title.y = element_text(hjust = 1, size = 14),
    axis.text = element_text(angle = 0, hjust = 0.5, size = 14))


dev.off()


### BCG and Baseline Plot

pdf(file = "./SA_plot_BCG2.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 11)


ggplot(epi_BCG) + 
  ggtitle("South Africa") +
  geom_line(aes(x = Year, y = med_rate, col = Runtype)) + ylim(0, NA) +
  geom_ribbon(aes(x = Year, ymin = low_rate, ymax = high_rate, fill = Runtype), alpha = 0.4) +
  scale_colour_viridis_d(direction = -1, option = "plasma") +
  scale_fill_viridis_d(direction = -1, option = "plasma") +
  ylab("Rate per 100,000 population") +
  facet_grid(Indicator ~ ., scales="free") +
  theme_bw() +
  theme(strip.background = element_blank()) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.title.x = element_text(hjust = 1, size = 14),
    axis.title.y = element_text(hjust = 1, size = 14),
    axis.text = element_text(angle = 0, hjust = 0.5, size = 14))


dev.off()

