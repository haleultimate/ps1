# run_ps.R
source("init_session.R")                        #clean environment, setup com parms

source("rnd_parms.R")
source("rnd_lib.R")
source("calc_lib.R")                            
source("model_select.R")                        
source("port_opt.R")                            #function libraries

source("run_prediction.R")                      #prediction model
source("make_mu.R")                             #calc MU for each var.env xts object 
source("blotter_sim.R")                         #run sim, plot daily profit

source("close_session.R")
