run_param_set_ltbi <- function(model, cc, params, params_uid, vx_chars, vx_inc = NA) {
  
  # set.paths = initialize the params
  model_paths <- model$set.paths(countrycode  = cc,
                                 xml          = vx_chars$xml,
                                 VXa.incidence.files = vx_inc,
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
  
  
  stocks <- output$stocks
  
  rm(output)
  
  stocks <- stocks[year %in% c(2020.5, 2030.5, 2040.5, 2050.5)]
  
  stocks <- stocks[!(age_from == 0 & age_thru == 99),]
  
  stocks <- stocks[, Age := age_from]
  
  latentprevalence <- stocks[(TB == "Lf" | TB == "Ls")]
  latentprevalence <- latentprevalence[, .(latentprevalence = sum(value)), by = .(country, year, Age)]
  
  latentprevalenceUc <- stocks[(TB == "Lf" | TB == "Ls"  | TB == "Uc")]
  latentprevalenceUc <- latentprevalenceUc[, .(latentprevalenceUc = sum(value)), by = .(country, year, Age)]
  
  
  latentprevalenceUcR <- stocks[(TB == "Lf" | TB == "Ls"  | TB == "Uc" | TB == "R")]
  latentprevalenceUcR <- latentprevalenceUcR[, .(latentprevalenceUcR = sum(value)), by = .(country, year, Age)]
  
  alive_pop <- stocks[!(TB == "TBdead" | TB == "Rdead")]
  alive_pop <- alive_pop[, .(alive_population = sum(value)), by = .(country, year, Age)]
  
  rm(stocks)
  
  latent <- merge(latentprevalence, latentprevalenceUc, by = c("country", "year", "Age"))
  latent <- merge(latent, latentprevalenceUcR, by = c("country", "year", "Age"))
  latent <- merge(latent, alive_pop, by = c("country", "year", "Age"))
  
  rm(latentprevalence)
  rm(latentprevalenceUc)
  rm(latentprevalenceUcR)
  rm(alive_pop)
  
  latent <- latent[, .(latentprev = latentprevalence/alive_population,
                       latentprevUc = latentprevalenceUc/alive_population,
                       latentprevUcR = latentprevalenceUcR/alive_population), by = .(country, year, Age)]
  
  # Add the vaccine characteristics
  combined_ipj <- latent[, uid := params_uid]
  combined_ipj
}
