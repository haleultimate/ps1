
if (!(com.env$load_model)) {
  source("sample_vars.R")
  source("define_vars.R")                         #define predictor var, setup initial v.com

  com.env$best_adj_r2 <- 0
  for (l in 1:com.env$model_loops) {              #start model loop
    print(paste("model loop:",l,"/",com.env$model_loops,Sys.time()))
    for (i in 1:com.env$add_vars) rnd_var()       #add new vars to v.com
    #print("eval_adj_r2 add var loop")
    #print(names(com.env$v.com))
    if (!check_dependencies()) {
      print("Dependency problem when adding new vars, removing var env and reverting to old v.com")
      print(names(com.env$v.com))
      com.env$v.com <- com.env$best_vcom
      var.env <- new.env()
      next
    }
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
    #print(paste("clean_vcom",Sys.time()))
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
    #print(paste("reg_vars=",length(com.env$reg_names)))
    #print(com.env$reg_names)
    model_worse <- TRUE
    while ((model_worse) & (loops < com.env$mod_var_loops)) {
      loops <- loops + 1
      #new_vd <- NULL
      #new_vd$ID <- -1
      new_vd <- rnd_mod(reg_names=com.env$reg_vcom_names)
      #print(new_vd)
      if (new_vd$ID != -1) {
        orig_vd <- com.env$v.com[[new_vd$vcom_num]]                              #store away original variable definition in vd_tmp
        #print("eval_adj_r2 mod var loop")
        if (!check_dependencies()) {
          print("Dependency problem when modding var")
          print(names(com.env$v.com))
          model_worse <- FALSE
          com.env$best_adj_r2 <- 0
          new_adj_r2 <- 0
          break
        } else {
          new_adj_r2 <- eval_adj_r2(vd=new_vd,orig_vd=orig_vd,old_adj_r2=orig_adj_r2)
        }
        if (new_adj_r2 > orig_adj_r2) {
          model_worse <- FALSE
          print(paste("model improved",new_adj_r2,orig_adj_r2,"loop#",loops,new_vd$name))
          com.env$best_adj_r2 <- new_adj_r2
          com.env$override_col <- com.env$mod_col
          com.env$reg_names <- names(com.env$model.stepwise$coefficients)[-1]
          new_vd <- optimize_mod(orig_vd,new_vd,orig_adj_r2,new_adj_r2)
        } else {
          #print(paste("model_worse",orig_adj_r2,new_adj_r2,"loop#",loops,"/",com.env$mod_var_loops,Sys.time()))
        }
      } #end if new_vd is valid
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
  
  #get top vars
  if (com.env$save_var_n > 0) {
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
  }
  
  if (com.env$save_model) {
    saved_model_files <- list.files(path=com.env$modeldir)
    print(paste("Saved_model_files:",length(saved_model_files)))
    if (length(saved_model_files)>0) {
      loop <- 1
      while (com.env$model_filename %in% saved_model_files & loop<5) {
        print(paste("Warning:",com.env$model_filename,"already in model directory, appending _"))
        com.env$model_filename <- paste(com.env$model_filename,"_",loop,sep="")
        loop <- loop + 1
      }
    }
    modelfile <- paste(com.env$modeldir,"/",com.env$model_filename,sep="")
    print(paste("Saving:",modelfile))
    save(list=c("v.com"),file=modelfile,envir=com.env)
  }
  
} else { #load model from file
  modelfile <- paste(com.env$modeldir,"/",com.env$model_filename,sep="")
  print(paste("Loading:",modelfile))
  load(file=modelfile,envir=com.env)
  print(paste("Evaluating model,",Sys.time()))
  eval_adj_r2(oos_data=TRUE)
}
