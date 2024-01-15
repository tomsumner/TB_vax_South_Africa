run_param_set_econ <- function(model, cc, params, params_uid, vx_chars, rr_pct) {
  
  combined_ipj <- list()
  
  # set.paths = initialize the params
  model_paths <- model$set.paths(countrycode  = cc,
                                 xml          = vx_chars$xml,
                                 parameters   = vx_chars$input)
  
  # Run the model with the parameter set
  if (vx_chars$runtype != "baseline"){
    scen_baseline = model$baseline_output 
  } else if (vx_chars$runtype == "baseline"){
    scen_baseline = NULL
  }
  
  output = model$run(model, new.parameter.values = params, baseline = scen_baseline, output.flows = F)
  
  #### cc_TB
  if (T){
    counts <- setDT(output$stocks[,!("RISK")])
    counts <- counts[grep("count$", counts$TB),]
    counts <- counts[!(year %% 0.5 == 0),]
    counts <- counts[!(age_from == 0 & age_thru == 99),]
    counts$AgeGrp = ""
    counts <- counts[age_from == 80  & age_thru == 89, AgeGrp := "(80,89]"]
    counts <- counts[age_from == 90  & age_thru == 99, AgeGrp := "(90,99]"]
    sel = counts$age_from<80
    counts[sel,]$AgeGrp = as.character(paste0("(", counts[sel,]$age_from, ",", counts[sel,]$age_from, "]"))

    counts <- counts[, !c("age_from", "age_thru")]
    counts <- counts[, year := floor(year)]
    
    inc      <- counts[TB == "Dscount" ,]
    inc_low  <- inc[SES == "low"][, .(SES_Low_Inc = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
    inc_high <- inc[SES == "high"][, .(SES_High_Inc = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
    inc_ses  <- merge(inc_low, inc_high)
    
    rm(inc)
    rm(inc_low)
    rm(inc_high)
    gc(full= TRUE)
    
    inc_ses  <- inc_ses[, Total_Inc := SES_High_Inc + SES_Low_Inc]
    inc_ses  <- inc_ses[, SES_High_Inc_DS := (1-rr_pct)*SES_High_Inc][, SES_High_Inc_RR := (rr_pct)*SES_High_Inc]
    inc_ses  <- inc_ses[, SES_Low_Inc_DS  := (1-rr_pct)*SES_Low_Inc][, SES_Low_Inc_RR := (rr_pct)*SES_Low_Inc]
    inc_ses  <- inc_ses[, Total_Inc_DS    := (1-rr_pct)*Total_Inc][, Total_Inc_RR := (rr_pct)*Total_Inc]

    notif <- counts[TB == "DcTcount" ,]
    notif <- setDT(notif)
    notif_low  <- notif[SES == "low"][, .(SES_Low_Notif = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
    notif_high <- notif[SES == "high"][, .(SES_High_Notif = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
    notif_ses  <- merge(notif_low, notif_high)
    
    rm(notif)
    rm(notif_low)
    rm(notif_high)
    gc(full= TRUE)
    
    notif_ses <- notif_ses[, Total_Notif       := SES_High_Notif + SES_Low_Notif]
    notif_ses <- notif_ses[, SES_High_Notif_DS := (1-rr_pct)*SES_High_Notif][, SES_High_Notif_RR := (rr_pct)*SES_High_Notif]
    notif_ses <- notif_ses[, SES_Low_Notif_DS  := (1-rr_pct)*SES_Low_Notif][, SES_Low_Notif_RR := (rr_pct)*SES_Low_Notif]
    notif_ses <- notif_ses[, Total_Notif_DS    := (1-rr_pct)*Total_Notif][, Total_Notif_RR := (rr_pct)*Total_Notif]

    ####### LMIC Treatment Success
    
    trt_succ <- counts[(TB == "TDccount" | TB == "TRcount" | TB == "TTBdeadcount"),]
    
    
    # 1. TREATMENT COMPLETION: nodeadR = flows into R from T
    SES_High_nodeadR  <- trt_succ[SES == "high" & TB == "TRcount",
                                  .(SES_High_complete = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
    
    SES_Low_nodeadR  <- trt_succ[SES == "low" & TB == "TRcount",
                                 .(SES_Low_complete = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
    
    
    
    # 2. TREATMENT NON-COMPLETION: nodeadDc = flows into Dc from T
    SES_High_nodeadDc <- trt_succ[SES == "high" & TB == "TDccount",
                                  .(SES_High_nocomplete = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
    
    SES_Low_nodeadDc <- trt_succ[SES == "low" & TB == "TDccount",
                                 .(SES_Low_nocomplete = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
    
    
    # 3. ON-TREATMENT MORTALITY: deadT = flows into TBdead from T 
    SES_High_deadT <- trt_succ[SES == "high" & TB == "TTBdeadcount",
                               .(SES_High_dead = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
    
    SES_Low_deadT <- trt_succ[SES == "low" & TB == "TTBdeadcount",
                              .(SES_Low_dead = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
    
    
    trt_succ <- merge(SES_High_nodeadR, SES_Low_nodeadR)
    trt_succ <- merge(trt_succ, SES_High_deadT)
    trt_succ <- merge(trt_succ, SES_Low_deadT)
    trt_succ <- merge(trt_succ, SES_High_nodeadDc)
    trt_succ <- merge(trt_succ, SES_Low_nodeadDc)
    
    rm(SES_High_deadT)
    rm(SES_High_nodeadDc)
    rm(SES_High_nodeadR)
    rm(SES_Low_deadT)
    rm(SES_Low_nodeadDc)
    rm(SES_Low_nodeadR)    
    gc(full= TRUE)
    
    # Calculate the total number of treatment completions (high + low SES)
    trt_succ <- trt_succ[, Total_complete    := SES_High_complete + SES_Low_complete]
    
    # Calc the proportion completions of all outcomes (complete / (complete + no complete + dead)) for High and Low SES
    trt_succ <- trt_succ[, SES_High_compl_frac := SES_High_complete / (SES_High_complete + SES_High_nocomplete + SES_High_dead)]
    trt_succ <- trt_succ[, SES_Low_compl_frac  := SES_Low_complete / (SES_Low_complete + SES_Low_nocomplete + SES_Low_dead)]
    trt_succ <- trt_succ[, Total_compl_frac    := Total_complete / (SES_High_complete + SES_High_nocomplete + SES_High_dead + SES_Low_complete + SES_Low_nocomplete + SES_Low_dead)]
    
    # Calc the proportion completions of all outcomes (complete / (complete + no complete + dead)) for DS and RR tb separately
    trt_succ <- trt_succ[, SES_High_compl_frac_DS := (1-rr_pct)*SES_High_compl_frac][, SES_High_compl_frac_RR := (rr_pct)*SES_High_compl_frac]
    trt_succ <- trt_succ[, SES_Low_compl_frac_DS  := (1-rr_pct)*SES_Low_compl_frac][, SES_Low_compl_frac_RR := (rr_pct)*SES_Low_compl_frac]
    trt_succ <- trt_succ[, Total_compl_frac_DS    := (1-rr_pct)*Total_compl_frac][,Total_compl_frac_RR := (rr_pct)*Total_compl_frac]
    
    
    # subset to only get the required variables
    trt_succ <- trt_succ[, .(Country, Year, AgeGrp,
                             SES_High_compl_frac, SES_High_compl_frac_DS, SES_High_compl_frac_RR,
                             SES_Low_compl_frac, SES_Low_compl_frac_DS, SES_Low_compl_frac_RR,
                             Total_compl_frac_DS, Total_compl_frac_RR, Total_compl_frac)]
    
    # trt_succ <- trt_succ[, Year := floor(Year)]
    
    ### Population Size by Single ages
    population <- setDT(output$population)
    population <- population[year >= 2023,]
    population <- population[year %% 1 == 0.5,] # use the average of the year for the stocks
    population <- melt(population, id.vars = c("year", "country"),
                       variable.name = "age", value.name = "Population")
    population$age = as.integer(as.character(population$age))
    population$AgeGrp = "-1"
    sel = population$age<80
    population[sel,]$AgeGrp = as.character(paste0("(", population[sel,]$age, ",", population[sel]$age, "]"))
    sel = population$age==80
    population[sel,]$AgeGrp = "(80,89]"
    sel = population$age==90
    population[sel,]$AgeGrp = "(90,99]"
    
    population = population[,-c("age")]

    # Subset the variables and make sure the capitals are correct
    population <- population[, .(Country = country, Year = year, AgeGrp, Population)]
    
    population <- population[, Year := floor(Year)]
    population <- population[Year < 2051,]
    
    inc_ses   <- inc_ses[  Year >= 2019 & Year < 2051,]
    notif_ses <- notif_ses[Year >= 2019 & Year < 2051,]
    trt_succ  <- trt_succ[ Year >= 2019 & Year < 2051,]
    
    TB_inc_notif <- population[inc_ses, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
    TB_inc_notif <- TB_inc_notif[notif_ses, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
    TB_inc_notif <- TB_inc_notif[trt_succ, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
    
    rm(population)
    rm(inc_ses)
    rm(notif_ses)
    rm(trt_succ)
    gc(full= TRUE)
    
    
    # Number vaccinated per year
    # Note: vaccinated != vaccine protected
    # Number vaccinated = sum of vaccount and prevcount

    tb_vxa <- setDT(output$stocks[,!("RISK")])
    tb_vxa <- tb_vxa[grep("count$", tb_vxa$VXa),]
    tb_vxa <- tb_vxa[!(year %% 0.5 == 0),]
    tb_vxa <- tb_vxa[!(age_from == 0 & age_thru == 99),]
    tb_vxa$AgeGrp = ""
    tb_vxa <- tb_vxa[age_from == 80  & age_thru == 89, AgeGrp := "(80,89]"]
    tb_vxa <- tb_vxa[age_from == 90  & age_thru == 99, AgeGrp := "(90,99]"]
    sel = tb_vxa$age_from<80
    tb_vxa[sel,]$AgeGrp = as.character(paste0("(", tb_vxa[sel,]$age_from, ",", tb_vxa[sel,]$age_from, "]"))
    tb_vxa <- tb_vxa[, !c("age_from", "age_thru")]
    tb_vxa <- tb_vxa[(VXa == "recvcount")]
    tb_vxa <- tb_vxa[,.(Number_VXa = abs(sum(value))), by = .(Country = country, Year = year, AgeGrp)]
    tb_vxa <- tb_vxa[, Year := floor(Year)]
    
    tb_vxa   <- tb_vxa[  Year >= 2023 & Year < 2051,]
    TB_inc_notif <- merge(TB_inc_notif, tb_vxa, all = TRUE)
    
    rm(tb_vxa)
    gc(full = TRUE)
    combined_ipj[["cc_TB"]] <- TB_inc_notif[, `:=`(UID = params_uid,
                                                   Runtype = vx_chars$runtype,
                                                   Scenario = vx_chars$scenario)]
    rm(TB_inc_notif)
    gc(full= TRUE)
  }
  
  rm(counts)  
  gc(full= TRUE)
  
  #### cc_TB_HIV
  if(T){
    
    stocks_ipj <- setDT(output$stocks)
    stocks_ipj <- stocks_ipj[!(grep("count$", stocks_ipj$VXa)),]
    stocks_ipj <- stocks_ipj[!(grep("count$", stocks_ipj$TB)),]
    stocks_ipj <- stocks_ipj[!(year %% 0.5) == 0,]
    stocks_ipj <- stocks_ipj[!(age_from == 0 & age_thru == 99),]
    
    stocks_ipj <- stocks_ipj[age_from == 0  & age_thru == 0, AgeGrp := "(0,0]"]
    stocks_ipj <- stocks_ipj[age_from == 80  & age_thru == 89, AgeGrp := "(80,89]"]
    stocks_ipj <- stocks_ipj[age_from == 90  & age_thru == 99, AgeGrp := "(90,99]"]
    
    stocks_ipj <- stocks_ipj[age_from == age_thru,
                             AgeGrp := as.character(paste0("(", age_from, ",", age_from, "]"))]
   
    
    stocks_ipj <- stocks_ipj[, !c("RISK", "age_from", "age_thru")]
    stocks_ipj <- stocks_ipj[TB != "TBdead" & TB!= "Rdead", .(Raw_Value = sum(value)),
                             by = .(Country = country, Year = year, AgeGrp, TB, HIV)]
    
    stocks_ipj <- stocks_ipj[, Year := floor(Year)]
    
    combined_ipj[["cc_TB_HIV"]] <- stocks_ipj[, `:=`(UID = params_uid,
                                                     Runtype = vx_chars$runtype,
                                                     Scenario = vx_chars$scenario)]
    
  }
  
  rm(stocks_ipj)
  gc(full= TRUE)
  
  ##### cc_deaths
  if (T){
    
    tbhiv_deaths <- setDT(output$dHIVTBx)
    tbhiv_deaths <- tbhiv_deaths[!(year %% 0.5) == 0,]
    tbhiv_deaths <- tbhiv_deaths[year >= 2023, `:=`(UID = params_uid,
                                                      Runtype = vx_chars$runtype)]
    
    tbhiv_deaths <- melt(tbhiv_deaths, id.vars = c("year", "country", "UID", "Runtype"),
                         variable.name = "AgeGrp", value.name = "Deaths")
    
    setnames(tbhiv_deaths, "Deaths", "TBHIVdeaths")
    
    # Get the Background deaths (Econ output only)
    bg_deaths <- setDT(output$dBGx)
    bg_deaths <- bg_deaths[year >= 2023][!(year %% 0.5 == 0),][, `:=`(UID = params_uid,
                                                                   Runtype = vx_chars$runtype)]
    
    bg_deaths <- melt(bg_deaths, id.vars = c("year", "country","UID", "Runtype"),
                      variable.name = "AgeGrp", value.name = "Deaths")
    
    setnames(bg_deaths, "Deaths", "BGdeaths")
    
    cc_alldeaths <- merge(bg_deaths, tbhiv_deaths)
    cc_alldeaths <- cc_alldeaths[, ALLdeaths := BGdeaths + TBHIVdeaths]
    
    rm(bg_deaths)
    rm(tbhiv_deaths)
    
    cc_alldeaths$AgeGrp = as.character(cc_alldeaths$AgeGrp)
    sel = as.integer(cc_alldeaths$AgeGrp) < 80
    cc_alldeaths[sel,]$AgeGrp = as.character(paste0("(", cc_alldeaths[sel,]$AgeGrp, ",", cc_alldeaths[sel,]$AgeGrp, "]"))

    cc_alldeaths <- cc_alldeaths[AgeGrp == "80", AgeGrp := "(80,89]"]
    cc_alldeaths <- cc_alldeaths[AgeGrp == "90", AgeGrp := "(90,99]"]
    

    # Subset the variables and make sure the capitals are correct
    cc_alldeaths <- cc_alldeaths[, Scenario := vx_chars$scenario]
    cc_alldeaths <- cc_alldeaths[, year := floor(year)]
    cc_alldeaths <- cc_alldeaths[year < 2051, ]
    
    combined_ipj[["cc_deaths"]] <- cc_alldeaths[, .(Country = country, Year = year, UID, 
                                                    Runtype, Scenario, AgeGrp, BGdeaths, TBHIVdeaths, ALLdeaths)]
    rm(cc_alldeaths)
    gc(full=TRUE)  
  }
  
  if (vx_chars$runtype == "baseline"){
    model$baseline_output <- output
  }
  
  rm(output)
  gc(full= TRUE)
  
  combined_ipj
}
