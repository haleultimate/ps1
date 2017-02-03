source("sample_vars.R")
source("define_vars.R")                         #define predictor var, setup initial v.com

for (l in 1:com.env$model_loops) {              #start model loop
  print(paste("model loop:",l,"/",com.env$model_loops,Sys.time()))
  for (i in 1:com.env$add_vars) rnd_var()       #add new vars to v.com
  if (com.env$verbose) print("eval_adj_r2 add var loop")
  orig_adj_r2 <- eval_adj_r2()
  if (com.env$verbose) print(paste("clean_vcom",com.env$cmn_cols,com.env$cols))
  clean_vcom()
  if ((orig_adj_r2==0) | (com.env$mod_var_loops==0)) {
    if (com.env$verbose) print("Removing var environment, add var loop")
    var.env <- new.env()
    next
  }
  
  #mod vars
  loops <- 0
  reg_names <- get_reg_names(names(com.env$model.stepwise$coefficients)[-1])
  print(paste("reg_vars=",length(reg_names)))
  model_worse <- TRUE
  while ((model_worse) & (loops < com.env$mod_var_loops)) {
    loops <- loops + 1
    new_vd <- rnd_mod(reg_names=reg_names)
    orig_vd <- com.env$v.com[[new_vd$vcom_num]]                              #store away original variable definition in vd_tmp
    if (com.env$verbose) print("eval_adj_r2 mod var loop")
    new_adj_r2 <- eval_adj_r2(new_vd,orig_adj_r2)
    if (new_adj_r2 > orig_adj_r2) {
      model_worse <- FALSE
      print(paste("model improved",new_adj_r2,orig_adj_r2))
      new_vd <- optimize_mod(orig_vd,new_vd,orig_adj_r2,new_adj_r2)
    } else {
      print(paste("model_worse",orig_adj_r2,new_adj_r2,"loop#",loops,"/",com.env$mod_var_loops,Sys.time()))
    }
  } #end mod while loop
  if (com.env$verbose) print("Removing var environment, mod var loop")
  rm(var.env)
  var.env <- new.env()
  if (com.env$verbose) print(names(com.env$v.com))
}                                               #end model loop

print(paste("model loop:",l+1,"/",com.env$model_loops,Sys.time()))
for (i in 1:com.env$add_vars) rnd_var()       #add new vars to v.com
print("eval_adj_r2, oos_data=TRUE")
orig_adj_r2 <- eval_adj_r2(oos_data=TRUE)
print("clean_vcom")
clean_vcom()

