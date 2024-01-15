run_param_set_epi <- function(model, cc, params, params_uid, vx_chars, vx_inc = NA) {
  
  combined_ipj <- list()
  
  # set.paths = initialize the params
  model_paths <- model$set.paths(countrycode  = cc,
                                 xml          = vx_chars$xml,
                                # VXa.incidence.files = vx_inc,
                                 parameters   = vx_chars$input)
  
  # Run the model with the parameter set
  
  if (vx_chars$runtype != "baseline"){
    scen_baseline = model$baseline_output 
  } else if (vx_chars$runtype == "baseline"){
    scen_baseline = NULL
  }
  
  output = model$run(model, new.parameter.values = params, baseline = scen_baseline)
  
  if (vx_chars$runtype == "baseline"){
    model$baseline_output <- output
  }
  
  
  # If need to plot:
  # Create the raw output format: merge stocks and flows
  #  stocks_and_flows_ipj <- model$merge.stocks.and.flows(output)
  
  
  # Add the vaccine characteristics
  # combined_ipj[["stocksflows"]] <- stocks_and_flows_ipj[, `:=`(UID = params_uid,
  #                                                             Runtype = vx_chars$runtype)]
  # rm(stocks_and_flows_ipj)
  
  
  flows_ipj <- output$flows
  flows_ipj <- flows_ipj[age_from == 0  & age_thru == 14, AgeGrp := "[0,14]"]
  flows_ipj <- flows_ipj[age_from == 15 & age_thru == 99, AgeGrp := "[15,99]"]
  flows_ipj <- flows_ipj[age_from == 0  & age_thru == 99, AgeGrp := "[0,99]"]
  flows_ipj <- flows_ipj[, !c("RISK", "age_from", "age_thru")]
  
  
  
  #### Incidence
  inc <- flows_ipj[dim == "TB" & TB == "Ds" & (HIV != "HIVdead") & flow == "in" 
                   & (subject != "TBp" & subject != "TBtr_init" & subject != "TBtr_nodeadreported" &
                        subject != "TBtr_nodeadnotreported" &
                        subject != "TBtr_died"),]
  inc <- inc[, .(N_inc = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  
  
  
  #### Mortality
  mort <- flows_ipj[dim == "TB" & TB == "TBdead" & (HIV != "HIVdead") & flow == "in"
                    & (subject == "TBtr_died" | subject == "TBp"),]  
  mort <- mort[, .(N_mort = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  
  
  
  #### Notifications
  notif <- flows_ipj[dim == "TB" & TB == "T" & (HIV != "HIVdead") & flow == "in" & subject == "TBtr_init",]
  notif <- notif[, .(N_tx = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  
  rm(flows_ipj)
  
  ### Population Size
  population <- setDT(output$population)
  population <- population[year >= 2010]
  population <- melt(population, id.vars = c("year", "country"),
                     variable.name = "Age", value.name = "Population")
  
  pop014 <- population[(Age == 0 | Age == 1 | Age == 2 | Age == 3 | Age == 4 | Age == 5 | 
                          Age == 6 | Age == 7 | Age == 8 | Age == 9 | Age == 10 | 
                          Age == 11 | Age == 12 | Age == 13 | Age == 14)]
  pop014 <- pop014[, .(Population = sum(Population)), by = .(Country = country, Year = year)]
  pop014 <- pop014[, AgeGrp := "[0,14]"]
  
  pop1599 <- population[!(Age == 0 | Age == 1 | Age == 2 | Age == 3 | Age == 4 | Age == 5 | 
                            Age == 6 | Age == 7 | Age == 8 | Age == 9 | Age == 10 | 
                            Age == 11 | Age == 12 | Age == 13 | Age == 14)]
  
  pop1599 <- pop1599[, .(Population = sum(Population)), by = .(Country = country, Year = year)]
  pop1599 <- pop1599[, AgeGrp := "[15,99]"]
  
  population <- population[, .(Population = sum(Population)), by = .(Country = country, Year = year)]
  population <- population[, AgeGrp := "[0,99]"]
  
  population <- rbind(population, pop1599)
  population <- rbind(population, pop014)
  
  
  # Combine everything into one dataset
  n_epi <- population[inc, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  n_epi <- n_epi[mort, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  n_epi <- n_epi[notif, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  
  
  rm(population)
  rm(inc)
  rm(mort)
  rm(notif)
  rm(output)
  
  combined_ipj[["n_epi"]] <- n_epi[, `:=`(UID = params_uid,
                                          Runtype = vx_chars$runtype,
                                          Scenario = vx_chars$scenario)]
  
  combined_ipj
}

