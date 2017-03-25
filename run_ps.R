# run_ps.R
source("init_session.R")                        #clean environment, setup com parms

#source("rnd_parms.R")
source("rnd_lib.R")
source("make_lib.R")
source("calc_lib.R")   
source("model_select.R") 
source("reg_lib.R")
source("port_opt.R")                            #function libraries

source("run_prediction.R")                      #prediction model

if ((com.env$save_var_n > 0) | (com.env$run_sim)) {
  print(paste("Evaluating model,",Sys.time()))
  eval_adj_r2(oos_data=TRUE)
}

if (com.env$save_var_n > 0) save_vars(com.env$save_var_n) #get top vars

if (com.env$run_sim) {
  source("make_mu.R")                             #calc MU,VLTY,ADJRET for each var.env xts object
  print(paste("Total equity:",com.env$init_equity))
  for (alpha_wt in c(16000)) {
    com.env$alpha_wt <- alpha_wt
    print(paste("alpha_wt:",com.env$alpha_wt))
    source("blotter_sim.R")                         #run sim, plot daily profit
  }
  var.env$MU <- -var.env$MU
  print("reversing MU")
  source("blotter_sim.R")
}

print(paste("End time:",Sys.time()))
#source("close_session.R")
