#run_prediction
if (!(com.env$load_model | com.env$opt_model)) {
  print("Error: Must either load or optimize a new model")
  stop()
}

if (com.env$opt_model)   source("sample_vars.R")  #set up rnd.env$vs.com and rnd_parms
if (!com.env$load_model) source("define_vars.R")  #if not loading model need to define predictor variable
if (com.env$load_model) load_model(com.env$model_filename)


if (com.env$opt_model) opt_model(com.env$model_loops,com.env$add_vars,com.env$mod_var_loops)
#opt_model <- function(model_loops,add_vars,mod_var_loops) {    
  #source("define_vars.R")                         #define predictor var, setup initial v.com
# if (com.env$opt_model) {
#   model_loops <- com.env$model_loops
#   add_vars <- com.env$add_vars
#   mod_var_loops <- com.env$mod_var_loops
#   print(paste("In opt_model, model_loops:",model_loops,"add_vars:",add_vars,"mod_var_loops:",mod_var_loops))
#   com.env$best_adj_r2 <- 0
#   for (l in 1:model_loops) {              #start model loop
#     print(paste("model loop:",l,"/",model_loops,Sys.time()))
#     for (i in 1:add_vars) rnd_var()       #add new vars to v.com
#     print("eval_adj_r2 add var loop")
#     print(names(com.env$v.com))
#     if (!check_dependencies()) {
#       print("Dependency problem when adding new vars, removing var env and reverting to old v.com")
#       print(names(com.env$v.com))
#       com.env$v.com <- com.env$best_vcom
#       var.env <- new.env()
#       next
#     }
#     ls(var.env)
#     orig_adj_r2 <- eval_adj_r2()
#     if (com.env$best_adj_r2 > orig_adj_r2) {
#       print(paste("Adj_r2 got worse when adding vars",orig_adj_r2,com.env$best_adj_r2))
#       print("Reverting to previous com.env$v.com********************************************")
#       #print(names(com.env$v.com))
#       #print(names(com.env$best_vcom))
#       com.env$v.com <- com.env$best_vcom
#       print("Removing var environment, add var loop")
#       var.env <- new.env()
#       next
#     } else {
#       com.env$best_adj_r2 <- orig_adj_r2
#       com.env$best_vcom <- com.env$v.com
#       #check_ids(com.env$ID_tried)
#     }
#     #print(paste("clean_vcom",Sys.time()))
#     clean_vcom()
#     if ((orig_adj_r2==0) | (com.env$mod_var_loops==0)) {
#       print("Removing var environment, add var loop")
#       var.env <- new.env()
#       next
#     }
#     
#     #mod vars
#     loops <- 0
#     check_adj_r2 <- orig_adj_r2
#     com.env$reg_names <- names(com.env$model.stepwise$coefficients)[-1]
#     com.env$reg_vcom_names <- get_reg_names(com.env$reg_names)
#     com.env$override_col <- NULL
#     #print(paste("reg_vars=",length(com.env$reg_names)))
#     #print(com.env$reg_vcom_names)
#     model_worse <- TRUE
#     #don't mod variables on last loop 
#     while ((model_worse) & (loops < mod_var_loops) & (l<com.env$model_loops)) {
#       #print(names(com.env$v.com))
#       loops <- loops + 1
#       #new_vd <- NULL
#       #new_vd$ID <- -1
#       new_vd <- rnd_mod(reg_names=com.env$reg_vcom_names)
#       #print(new_vd)
#       if (new_vd$ID != -1) {
#         #print(paste("save away orig_vd",new_vd$vcom_num))
#         orig_vd <- com.env$v.com[[new_vd$vcom_num]]       #store away original variable definition in vd_tmp
#         #print("eval_adj_r2 mod var loop")
#         if (!check_dependencies()) {
#           print("Dependency problem when modding var")
#           print(names(com.env$v.com))
#           model_worse <- FALSE
#           com.env$best_adj_r2 <- 0
#           new_adj_r2 <- 0
#           break
#         } else {
#           new_adj_r2 <- eval_adj_r2(vd=new_vd,orig_vd=orig_vd,old_adj_r2=orig_adj_r2)
#         }
#         if (new_adj_r2 > orig_adj_r2) {
#           model_worse <- FALSE
#           print(paste("model improved",new_adj_r2,orig_adj_r2,"loop#",loops,new_vd$var_name))
#           com.env$best_adj_r2 <- new_adj_r2
#           com.env$override_col <- com.env$mod_col
#           com.env$reg_names <- names(com.env$model.stepwise$coefficients)[-1]
#           new_vd <- optimize_mod(orig_vd,new_vd,orig_adj_r2,new_adj_r2)
#         } else {
#           #print(paste("model_worse",orig_adj_r2,new_adj_r2,"loop#",loops,"/",com.env$mod_var_loops,Sys.time()))
#         }
#       } #end if new_vd is valid
#     } #end mod while loop
#     print("Removing var environment, mod var loop")
#     rm(var.env)
#     var.env <- new.env()
#     print(names(com.env$v.com))  
#     ls(var.env)
#     if (com.env$best_adj_r2 < check_adj_r2) {
#       print(paste("Adj_r2 got worse when modding vars",check_adj_r2,com.env$best_adj_r2))
#       print("Reverting to previous com.env$v.com*************************************************")
#       com.env$v.com <- com.env$best_vcom
#       next
#     } else {
#       com.env$best_vcom <- com.env$v.com
#       #check_ids(com.env$ID_tried)
#     }
#   }                                               #end model loop
#   
#   #print(paste("model loop:",l+1,"/",com.env$model_loops,Sys.time()))
#   #for (i in 1:com.env$add_vars) rnd_var()       #add new vars to v.com
#   #print("eval_adj_r2, oos_data=TRUE")
#   #orig_adj_r2 <- eval_adj_r2(oos_data=TRUE)
#   #print("clean_vcom")
#   #clean_vcom()
#   
#   #get top vars
#   if (com.env$save_var_n > 0) save_vars(com.env$save_var_n)
#   
# } #end function opt_model




if (com.env$save_model) save_model(com.env$model_filename)




