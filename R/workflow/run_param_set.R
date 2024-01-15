run_param_set <- function(model, cc, params, params_uid, vx_chars, vx_inc = NA) {
  
  # set.paths = initialize the params
  model_paths <- model$set.paths(countrycode  = cc,
                                 xml          = vx_chars$xml,
                                 #VXa.incidence.files = vx_inc,
                                 parameters   = "input_L0_red.csv")
  
  # Run the model with the parameter set
  if (vx_chars$runtype != "baseline"){
    scen_baseline = model$baseline_output 
  } else if (vx_chars$runtype == "baseline"){
    scen_baseline = NULL
  }
  
  output = model$run(model, new.parameter.values = params, baseline = scen_baseline)
  
  if (length(output$stocks) == 0){
    print(paste0("Model runs failed for parameter set with uid = ", params_uid, " - check model log"))
  } 
  
  if (length(output$stocks) != 0){
    
    if (vx_chars$runtype == "baseline"){
      model$baseline_output <- output
    }
    
    # Create the output format: merge stocks and flows
    combined_ipj <- model$merge.stocks.and.flows(output)
    rm(output)
    
    # Add the vaccine characteristics
    combined_ipj <- combined_ipj[, `:=`(uid     = params_uid,
                                        version = model$modelversion(),
                                        runtype = vx_chars$runtype)]
    combined_ipj
  }
}
