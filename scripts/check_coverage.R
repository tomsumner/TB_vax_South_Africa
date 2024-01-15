rm(list=ls())
model = new.env()
# for debugging set model to globalenv():
#model = globalenv()
suppressPackageStartupMessages({
  source(here::here("R","include-v11.R"),model)
  source(here::here("R","TBVx-run-v1.R"))
  taskenvvar="taskID"
  library(ggplot2)
  library(cowplot)
  theme_set(theme_minimal_grid() + panel_border(color = "black"))
})


model$paths = model$set.paths(countries   = "countries", 
                              countrycode = "ZAF", 
                              xml         = "BCGecon/XMLinput_BCG_NCI_POI_45_10yr_med_2025.xml", 
                              parameters  = "input.csv")

# If you want to use a fitted parameter set to test
parameters <- fread(paste0("./processing_files/paramsets/ZAF_params.csv"))
params <- parameters[2, ]
params <- params[, !c("uid", "nhits")]
params <- unlist(params)

output = run(model, new.parameter.values = params, sample.parameters=F, write.to.file = F,
             combine.stocks.and.flows = F, output.flows = F)



# Number currently in the VXa dimension
if (T){
  vxa_prev <- output$stocks[,!("RISK")]
  vxa_prev <- vxa_prev[TB != "Rdead" & TB!= "TBdead" & HIV != "HIVdead"]
  vxa_prev <- vxa_prev[!(VXa == "recvcount")]
  vxa_prev <- vxa_prev[year %% 0.5 == 0,]
  vxa_prev <- vxa_prev[!(age_from == 0 & age_thru == 99),]
  
  vxa_prev <- vxa_prev[age_from == 0  & age_thru == 0, AgeGrp := "[0,0]"]
  vxa_prev <- vxa_prev[age_from == 80  & age_thru == 89, AgeGrp := "(80,89]"]
  vxa_prev <- vxa_prev[age_from == 90  & age_thru == 99, AgeGrp := "(90,99]"]
  
  for (k in 1:79){
    vxa_prev[age_from == k  & age_thru == k, AgeGrp := as.character(paste0("(", k, ",", k, "]"))]
  }
  
  vxa_prev <- vxa_prev[, !c("age_from", "age_thru")]
  
  vxa_prev <- vxa_prev[, Number := sum(value), by = .(year, AgeGrp, VXa)]
  vxa_prev <- vxa_prev[, `:=`(Total = sum(Number)), by = .(year, AgeGrp)]
  
  vxa_prev <- vxa_prev[, Prop := Number/Total, by = .(year, AgeGrp)]
  
  
  vxa_prev$AgeGrp  <- ordered(vxa_prev$AgeGrp,
                              levels = c("(9,9]", "(10,10]", "(11,11]", "(12,12]", "(13,13]",
                                         "(14,14]", "(15,15]", "(16,16]", "(17,17]", "(18,18]", "(19,19]",
                                         "(20,20]","(21,21]", "(22,22]", "(23,23]", "(24,24]", "(25,25]",
                                         "(30,30]", "(40,40]","(50,50]"),
                              labels = c("(9,9]", "(10,10]", "(11,11]", "(12,12]", "(13,13]",
                                         "(14,14]", "(15,15]", "(16,16]", "(17,17]","(18,18]", "(19,19]",
                                         "(20,20]", "(21,21]", "(22,22]", "(23,23]", "(24,24]", "(25,25]",
                                         "(30,30]", "(40,40]","(50,50]"))
  
  vxa_prev <- vxa_prev[!(is.na(AgeGrp))]
  
  vxa_prev <- vxa_prev[grep("vac", VXa), Effective := "Vx Protected"]
  vxa_prev <- vxa_prev[grep("prev", VXa), Effective := "Vx NOT Protected"]
  vxa_prev <- vxa_prev[grep("never", VXa), Effective := "Never"]
  vxa_prev <- vxa_prev[grep("waned", VXa), Effective := "Waned"]
  
  
  ggplot(data = vxa_prev[!(is.na(AgeGrp))]) +
    geom_col(aes(x = year, y = (Number/Total)*100, fill = VXa), width = 1) +
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    geom_hline(yintercept = 80) +
    theme_bw() +  theme_minimal_grid() + panel_border(color = "black") + 
    facet_wrap(~ AgeGrp) + 
    theme(
      legend.text = element_text(size = 14),
      legend.title = element_blank(),
      legend.justification = "right",
      legend.position = "bottom",
      axis.title.x = element_text(hjust = 1, size = 16),
      axis.title.y = element_text(hjust = 1, size = 16),
      axis.text = element_text(size = 12)
    ) +
    labs(y = "Percent of population in each vaccine state")
  
  ggplot(data = vxa_prev[!(is.na(AgeGrp)) & (Effective == "Vx Protected" | Effective == "Vx NOT Protected")]) +
    geom_col(aes(x = year, y = (Number/Total)*100, fill = Effective), width = 1) +
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    geom_hline(yintercept = 80) + ylim(c(0,100)) +
    theme_bw() +  theme_minimal_grid() + panel_border(color = "black") + 
    facet_wrap(~ AgeGrp) + 
    theme(
      legend.text = element_text(size = 14),
      legend.title = element_blank(),
      legend.justification = "right",
      legend.position = "bottom",
      axis.title.x = element_text(hjust = 1, size = 16),
      axis.title.y = element_text(hjust = 1, size = 16),
      axis.text = element_text(size = 12)
    ) +
    labs(y = "Percent of population in each vaccine state")
}


# Number being vaccinated in each year
if (T){
  tb_vxa <- output$stocks[,!("RISK")]
  tb_vxa <- tb_vxa[grep("count", tb_vxa$VXa),]
  tb_vxa <- tb_vxa[!(year %% 0.5 == 0),]
  tb_vxa <- tb_vxa[!(age_from == 0 & age_thru == 99),]
  
  tb_vxa <- tb_vxa[age_from == 0  & age_thru == 0, AgeGrp := "[0,0]"]
  tb_vxa <- tb_vxa[age_from == 80  & age_thru == 89, AgeGrp := "(80,89]"]
  tb_vxa <- tb_vxa[age_from == 90  & age_thru == 99, AgeGrp := "(90,99]"]
  
  for (k in 1:79){
    tb_vxa[age_from == k  & age_thru == k, AgeGrp := as.character(paste0("(", k, ",", k, "]"))]
  }
  
  tb_vxa <- tb_vxa[, !c("age_from", "age_thru")]
  tb_vxa <- tb_vxa[year >= 2024]
  tb_vxa <- tb_vxa[TB != "Rdead" & TB!= "TBdead" & HIV != "HIVdead"]
  tb_vxa <- tb_vxa[,.(Number_VXa = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  tb_vxa <- tb_vxa[, Year := floor(Year)]
  
  
  population <- setDT(output$population)
  population <- population[year >= 2024]
  
  population <- population[(year %% 0.5 == 0),] # use the average of the year for the stocks
  population <- melt(population, id.vars = c("year", "country"),
                     variable.name = "AgeGrp", value.name = "Population")
  
  population <- population[AgeGrp == "0", AgeGrp := "[0,0]"]
  population <- population[AgeGrp == "80", AgeGrp := "(80,89]"]
  population <- population[AgeGrp == "90", AgeGrp := "(90,99]"]
  
  for (k in 1:79){
    population[AgeGrp == k, AgeGrp := as.character(paste0("(", k, ",", k, "]"))]
  }
  
  population <- population[, .(Country = country, Year = year, AgeGrp, Population)]
  population <- population[, Year := floor(Year)]
  
  
  tb_vxa <- merge(population, tb_vxa)
  tb_vxa <- tb_vxa[, Total := cumsum(Number_VXa), by = .(Country, AgeGrp)]
  tb_vxa <- tb_vxa[Number_VXa == 0, Total := 0]
  
  tb_vxa$AgeGrp <- ordered(tb_vxa$AgeGrp,
                           levels = c("(9,9]", "(10,10]", "(11,11]", "(12,12]", "(13,13]",
                                      "(14,14]", "(15,15]", "(16,16]", "(17,17]", "(18,18]", "(19,19]"),
                           labels = c("(9,9]", "(10,10]", "(11,11]", "(12,12]", "(13,13]",
                                      "(14,14]", "(15,15]", "(16,16]", "(17,17]","(18,18]", "(19,19]"))
  
  ggplot(data = tb_vxa[!(is.na(AgeGrp))]) +
    geom_col(aes(x = Year, y = (Number_VXa/Population)*100), width = 1) +
    scale_colour_viridis_d(direction = -1, option = "viridis") +
    scale_fill_viridis_d(direction = -1, option = "viridis") +
    theme_bw() +  theme_minimal_grid() + panel_border(color = "black") +
    geom_hline(yintercept = 80) +
    facet_wrap(~ AgeGrp) + ylim(c(0,100)) +
    theme(
      legend.text = element_text(size = 14),
      legend.title = element_blank(),
      legend.justification = "right",
      legend.position = "bottom",
      axis.title.x = element_text(hjust = 1, size = 16),
      axis.title.y = element_text(hjust = 1, size = 16),
      axis.text = element_text(size = 12)
    ) +
    labs(y = "Percent of age group receiving a vaccine that year")
  
}