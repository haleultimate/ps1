#run_prediction
if (!(com.env$load_model | com.env$opt_model)) {
  print("Error: Must either load or optimize a new model")
  stop()
}

print("sample_vars")
if (com.env$opt_model)   source("sample_vars.R")  #set up rnd.env$vs.com and rnd_parms
print("define_vars")
if (!com.env$load_model) source("define_vars.R")  #if not loading model need to define predictor variable
if (com.env$load_model) load_model(com.env$model_filename)
print(paste("com.env$predict.ret",com.env$predict.ret))

print("opt model")
if (com.env$opt_model) opt_model(com.env$model_loops,com.env$add_var_levels,com.env$mod_var_loops)

if (com.env$save_model) save_model(com.env$model_filename)




