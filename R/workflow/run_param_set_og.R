run_param_set <- function(model, cc, params, params_uid, vx_chars, vx_inc = NA) {
  
  # set.paths = initialize the params
  model_paths <- model$set.paths(countrycode  = cc,
                                 xml          = vx_chars$xml,
                                 VXa.incidence.files = vx_inc,
                                 parameters   = "input.csv")
  
  # Run the model with the parameter set
  # read_params        <- model$read.model.parameters(model_paths)
  # 
  # if (vx_chars$runtype != "baseline"){
  #   model$set.baseline.in.model.parameters(read_params, output = model$baseline_output)
  # }
  # 
  # updated_params     <- model$update.model.parameters(read_params, params)
  # initialized_params <- model$initialize.model.parameters(updated_params)
  # output             <- model$run.model(initialized_params)
  
  if (vx_chars$runtype != "baseline"){
    scen_baseline = model$baseline_output 
  } else if (vx_chars$runtype == "baseline"){
    scen_baseline = NULL
  }
  
  output = run(model, new.parameter.values = params, baseline = scen_baseline)
  
  
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
