source("sample_vars.R")
source("define_vars.R")                         #define predictor var, setup initial v.com

com.env$best_adj_r2 <- 0
for (l in 1:com.env$model_loops) {              #start model loop
  print(paste("model loop:",l,"/",com.env$model_loops,Sys.time()))
  for (i in 1:com.env$add_vars) rnd_var()       #add new vars to v.com
  #print("eval_adj_r2 add var loop")
  #print(names(com.env$v.com))
  orig_adj_r2 <- eval_adj_r2()
  if (com.env$best_adj_r2 > orig_adj_r2) {
    print(paste("Adj_r2 got worse when adding vars",orig_adj_r2,com.env$best_adj_r2))
    print("Reverting to previous com.env$v.com********************************************")
    #print(names(com.env$v.com))
    #print(names(com.env$best_vcom))
    com.env$v.com <- com.env$best_vcom
    print("Removing var environment, add var loop")
    var.env <- new.env()
    next
  } else {
    com.env$best_adj_r2 <- orig_adj_r2
    com.env$best_vcom <- com.env$v.com
    #check_ids(com.env$ID_tried)
  }
  print(paste("clean_vcom",Sys.time()))
  clean_vcom()
  if ((orig_adj_r2==0) | (com.env$mod_var_loops==0)) {
    print("Removing var environment, add var loop")
    var.env <- new.env()
    next
  }
  
  #mod vars
  loops <- 0
  check_adj_r2 <- orig_adj_r2
  com.env$reg_names <- names(com.env$model.stepwise$coefficients)[-1]
  com.env$reg_vcom_names <- get_reg_names(com.env$reg_names)
  com.env$override_col <- NULL
  print(paste("reg_vars=",length(com.env$reg_names)))
  #print(com.env$reg_names)
  model_worse <- TRUE
  while ((model_worse) & (loops < com.env$mod_var_loops)) {
    loops <- loops + 1
    new_vd <- NULL
    new_vd$ID <- -1
    while (new_vd$ID == -1) new_vd <- rnd_mod(reg_names=com.env$reg_vcom_names)
    orig_vd <- com.env$v.com[[new_vd$vcom_num]]                              #store away original variable definition in vd_tmp
    if (com.env$verbose) print("eval_adj_r2 mod var loop")
    new_adj_r2 <- eval_adj_r2(vd=new_vd,orig_vd=orig_vd,old_adj_r2=orig_adj_r2)
    if (new_adj_r2 > orig_adj_r2) {
      model_worse <- FALSE
      print(paste("model improved",new_adj_r2,orig_adj_r2))
      com.env$best_adj_r2 <- new_adj_r2
      com.env$override_col <- com.env$mod_col
      com.env$reg_names <- names(com.env$model.stepwise$coefficients)[-1]
      new_vd <- optimize_mod(orig_vd,new_vd,orig_adj_r2,new_adj_r2)
    } else {
      print(paste("model_worse",orig_adj_r2,new_adj_r2,"loop#",loops,"/",com.env$mod_var_loops,Sys.time()))
    }
  } #end mod while loop
  if (com.env$verbose) print("Removing var environment, mod var loop")
  rm(var.env)
  var.env <- new.env()
  if (com.env$verbose) print(names(com.env$v.com))  
  if (com.env$best_adj_r2 < check_adj_r2) {
    print(paste("Adj_r2 got worse when modding vars",check_adj_r2,com.env$best_adj_r2))
    print("Reverting to previous com.env$v.com*************************************************")
    com.env$v.com <- com.env$best_vcom
    next
  } else {
    com.env$best_vcom <- com.env$v.com
    #check_ids(com.env$ID_tried)
  }
}                                               #end model loop

print(paste("model loop:",l+1,"/",com.env$model_loops,Sys.time()))
for (i in 1:com.env$add_vars) rnd_var()       #add new vars to v.com
print("eval_adj_r2, oos_data=TRUE")
orig_adj_r2 <- eval_adj_r2(oos_data=TRUE)
print("clean_vcom")
clean_vcom()

#get top 5 vars
f1 <- as.formula(paste(com.env$predict.ret,"~.",sep=""))
if (length(colnames(var.env$reg_data.df)) > 50) {
  model.subsets <- regsubsets(f1,data=var.env$reg_data.df,nbest=com.env$save_var_n,nvmax=1,really.big=TRUE)
} else {
  model.subsets <- regsubsets(f1,data=var.env$reg_data.df,nbest=com.env$save_var_n,nvmax=1)
}
var_names <- NULL
for (i in 1:com.env$save_var_n) {
  var_names <- append(var_names,names(coef(model.subsets,i))[2])
}
print(var_names)
save_vcoms <- get_reg_names(var_names,return_num=TRUE)
print(save_vcoms)
save_vcom_vars(save_vcoms)



