# run_ps.R
print(paste("Start time:",Sys.time()))
if (!exists("stx_list.loaded")) stx_list.loaded <- NULL 
rm(list = ls(all=TRUE)[!ls(all=TRUE) %in% c("data.env","stx_list.loaded")]) #clean workspace except for (data.env, stx.list.old) so we don't have to reload data

source("init_lib.R")            #library needed to load other libraries 

stx_list.loaded <- init_session(stx_list.loaded)    #load libraries, set com parms, load/clean data (if not loaded)

run_prediction()                #prediction model (reg_lib.R)

if (com.env$run_sim) run_sim()  #portfolio optimization (port_opt.R)

print(paste("End time:",Sys.time()))
