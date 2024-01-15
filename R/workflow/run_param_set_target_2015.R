run_param_set_target <- function(model, cc, params, params_uid, vx_chars, vx_inc = NA) {
  
  # set.paths = initialize the params
  model_paths <- model$set.paths(countrycode  = cc,
                                 xml          = vx_chars$xml,
                                 VXa.incidence.files = vx_inc,
                                 parameters   = "input.csv",
                                 targets = "target_w2015.csv")
  
  # Run the model with the parameter set
  output = model$run(model, new.parameter.values = params)
  
  # Add the vaccine characteristics
 # combined_ipj <- data.frame(uid = params_uid, inc = output[["hits"]]$model[34])
  combined_ipj <- data.frame(uid = params_uid,
                             inc = c(output[["hits"]]$model[35],output[["hits"]]$model[44],output[["hits"]]$model[36],
                                     output[["hits"]]$model[37],output[["hits"]]$model[45],output[["hits"]]$model[38],
                                     output[["hits"]]$model[39],output[["hits"]]$model[46],output[["hits"]]$model[40]
                                                       ))
  q <- combined_ipj
  q2 <- t(q[,2])
  #q3 <- cbind(q[1,1],q2)
  combined_ipj <- q2
  combined_ipj
}
