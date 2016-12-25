source("sample_vars.R")
source("define_vars.R")                         #define predictor var, setup initial v.com

for (l in 1:com.env$model_loops) {              #start model loop
  print(paste("model loop:",l))

  for (i in 1:com.env$add_vars) rnd_var()       #add new vars to v.com
  source("make_vars.R")                         #make vars in var.env
  source("collect_data.R")                      #get regression data frame
  print(paste("Data Collected:",Sys.time()))
  source("run_regression.R")                    #run regression
  #if (summary(model.stepwise)$adj.r.squared > 0.10) break
  source("clean_vcom.R")                        #delete useless variables

}                                               #end model loop
