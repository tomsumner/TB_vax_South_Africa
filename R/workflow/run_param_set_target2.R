run_param_set_target <- function(model, cc, params, params_uid, vx_chars, vx_inc = NA) {
  
  # set.paths = initialize the params
  model_paths <- model$set.paths(countrycode  = cc,
                                 xml          = vx_chars$xml,
                                 VXa.incidence.files = vx_inc,
                                 parameters   = "input_L0_red.csv",
                                 targets = "target_count2.csv")
  
  # Run the model with the parameter set
  output = model$run(model, new.parameter.values = params, output.flows=F)
  
  # Add the vaccine characteristics
  #combined_ipj1 <- data.frame(uid = params_uid, hits = sum(output[["hits"]]$fit))
  combined_ipj2 <- data.frame(uid = params_uid, varb = t(output[["hits"]]$fit))
  combined_ipj3 <- data.frame(uid = params_uid, mod = t(output[["hits"]]$model))
  combined_ipj <- cbind(combined_ipj2,combined_ipj3) 
  combined_ipj
}
