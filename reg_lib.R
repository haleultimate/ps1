#run_regression.R

#remove all colinear dependent variables 
run_regression <- function(oos_data = FALSE) {
  print(paste("run full regression",Sys.time()))
  if (ncol(var.env$reg_data.df) > 2) {
    keep.dat <- vif_func(var.env$reg_data.df[,-1],thresh=10,trace=FALSE) #don't include predict.ret
    keep.dat <- append(colnames(var.env$reg_data.df)[1],keep.dat)
    if (com.env$verbose) print(keep.dat)
    var.env$reg_data.df <- var.env$reg_data.df[,keep.dat]
  }
  if (com.env$verbose) print(str(var.env$reg_data.df))
  
  #run_regression
  form1 <- as.formula(paste(colnames(var.env$reg_data.df)[1],"~ 1"))
  null <- lm(form1,data=var.env$reg_data.df)
  form2 <- as.formula(paste(colnames(var.env$reg_data.df)[1],"~ ."))
  reg.model <- lm(form2,data=var.env$reg_data.df)
  #print("model.select")
  com.env$model.stepwise <- model.select(reg.model,sig=0.001,verbose=FALSE)
  if (length(com.env$model.stepwise) == 0) {
    print("All variables left model")
    adj_r2 <- 0
  } else {
    adj_r2 <- summary(com.env$model.stepwise)$adj.r.squared
    #print(paste("In-sample adj-r2:",adj_r2))
    if (oos_data) {
      oos_stats <- oos.r2(com.env$model.stepwise,var.env$OOS_data.df)
      print(paste("r2",oos_stats$rsq,"r20",oos_stats$rsq0,"cor",oos_stats$cor,"mse",oos_stats$mse,"mean",oos_stats$mean,"wpct",oos_stats$winpct))
      print("reversing sign on regression model")
      oos_stats <- oos.r2(com.env$model.stepwise,var.env$OOS_data.df,reverse=TRUE)
      print(paste("r2",oos_stats$rsq,"r20",oos_stats$rsq0,"cor",oos_stats$cor,"mse",oos_stats$mse,"mean",oos_stats$mean,"wpct",oos_stats$winpct))
    }
  }
  print(paste("end regression",Sys.time()))
}

run_mod_regression <- function(reg_vars) {
  #print(paste("Run_mod_regression",Sys.time()))
  #print(reg_vars)
  f <- as.formula(paste(colnames(var.env$reg_data.df)[1],paste(reg_vars,collapse=" + "),sep=" ~ "))
  #print(f)
  com.env$model.stepwise <- lm(formula=f,data=var.env$reg_data.df)
  #print(summary(com.env$model.stepwise)$adj.r.squared)
  #print(Sys.time())
  return(summary(com.env$model.stepwise)$adj.r.squared)
}

collect_data <- function (oos_data = FALSE) {
  #determine columns used in regression
  vvars <- NULL
  com.env$name2vcomnum <- NULL
  allmodelvars <- NULL
  for (i in 1:length(com.env$v.com)) {
    vvars[i] <- (com.env$v.com[[i]]$use == "model")
    if (vvars[i]) {
      if (length(com.env$v.com[[i]]$name) == 1) {
        com.env$name2vcomnum <- c(com.env$name2vcomnum,i)
        names(com.env$name2vcomnum)[length(com.env$name2vcomnum)] <- com.env$v.com[[i]]$name 
      } else {
        for (nam in com.env$v.com[[i]]$name) {
          com.env$name2vcomnum <- c(com.env$name2vcomnum,i)
          names(com.env$name2vcomnum)[length(com.env$name2vcomnum)] <- nam 
        }
      }
    }
  }
  
  vvars <- which(vvars)
  for (i in vvars) {
    cn <- com.env$v.com[[i]]$col
    clist <- c(cn:(cn-1+length(com.env$v.com[[i]]$name)))
    allmodelvars <- append(allmodelvars,clist)
  }
  if (com.env$verbose) {
    print("collect data: allmodelvars")
    print(allmodelvars)
  }
  
  #get data for all stx into single data frame
  var.env$reg_data.df <- NULL
  var.env$OOS_data.df <- NULL
  for (i in 1:com.env$stx) {
    ticker <- com.env$stx.symbols[i]
    cmn_ticker <- com.env$cmn_lookup[ticker]
    cmd_string <- paste("cmn_start_date <- index(data.env$",cmn_ticker,"[",com.env$days2remove,",])",sep="")
    eval(parse(text=cmd_string))
    cmd_string <- paste("stk_start_date <- index(data.env$",ticker,"[",com.env$days2remove,",])",sep="")
    eval(parse(text=cmd_string))
    max_start_date <- max(c(cmn_start_date,stk_start_date,com.env$reg_start_date))
    cmd_string <- paste("start_idx <- which(max_start_date == index(var.env$",ticker,"))",sep="")
    eval(parse(text=cmd_string))
    cmd_string <- paste("end_idx <- which(com.env$reg_end_date == index(var.env$",ticker,"))",sep="")
    eval(parse(text=cmd_string))
    subset_string <- paste("var.env$",ticker,"[",start_idx,":",end_idx,",allmodelvars]",sep="")
    cmd_string <- paste("var.env$reg_data.df <- bind_rows(var.env$reg_data.df,as.data.frame(",subset_string,"))",sep="")
    #if (verbose) print(cmd_string)
    eval(parse(text=cmd_string))
    if (oos_data) {
      subset_string <- paste("var.env$",ticker,"[com.env$OOS_date_range,allmodelvars]",sep="")
      cmd_string <- paste("var.env$OOS_data.df <- bind_rows(var.env$OOS_data.df,as.data.frame(",subset_string,"))",sep="")
      #print(cmd_string)
      eval(parse(text=cmd_string))
    }
  }
  #str(reg_data.df)
  if (com.env$verbose) print(com.env$vcom_names)
  #com.env$ind_names
  #com.env$bin_names
  if (com.env$verbose) print(allmodelvars)
  
}

get_reg_names <- function(reg_model_vars,return_num=FALSE) { #take a list of variable names and return vcom names or vcom nums
  #print(paste(reg_model_vars,return_num))
  reg_names <- NULL
  for (v in 1:length(com.env$v.com)) {  #create list of model variables as candidates to modify
    vd <- com.env$v.com[[v]]
    #print(paste(v,vd$name))
    if (length(vd$name) == 1) {
      if ( (vd$name %in% reg_model_vars) ) {
        #print(v)
        if (return_num) {
          reg_names <- append(reg_names,v)
        } else {
          reg_names <- append(reg_names,vd$name)
        }
        #reg_names <- ifelse(return_num,append(reg_names,v),append(reg_names,vd$name))
        #print(reg_names)
      }
    } else {
      if ((vd$name[1] %in% reg_model_vars) | (vd$name[2] %in% reg_model_vars)) {
        #print(v)
        if (return_num) {
          reg_names <- append(reg_names,v)
        } else {
          reg_names <- append(reg_names,vd$var_name)
        }
        #reg_names <- ifelse(return_num,append(reg_names,v),)
        #print(reg_names)
      }
    }
  }  #end for v loop
  #return (rnd_mod(reg_names=reg_names))    #select vd from reg_names and modify it
  reg_names <- unique(reg_names)
  return(reg_names)
}


#clean_vcom.R
clean_vcom <- function() {
  keep_vcom_name <- names(com.env$model.stepwise$coefficients)[-1]
  keep_vcom_name <- append(colnames(var.env$reg_data.df)[1],keep_vcom_name)
  print(paste("#reg vars:",length(keep_vcom_name)))
  more_vcom_names <- NULL
  for (nam in keep_vcom_name) {
    vcom_num <- com.env$name2vcomnum[nam]
    more_vcom_names <- append(more_vcom_names,com.env$v.com[[vcom_num]]$requires)
  }
  keep_vcom_name <- append(keep_vcom_name,more_vcom_names)
  keep_vcom_name <- unique(keep_vcom_name)
  #print("keep_vcom_names")
  #print(keep_vcom_name)
  #print(length(com.env$v.com))
  delete_vcom_name <- NULL
  for (v in 1:length(com.env$v.com)) {
    vd <- com.env$v.com[[v]]
    if (length(vd$name) == 1) {
      if ( !(vd$name %in% keep_vcom_name) ) {
        delete_vcom_name <- append(delete_vcom_name,vd$name)
      }
    } else {
      if (!(vd$name[1] %in% keep_vcom_name) & !(vd$name[2] %in% keep_vcom_name)) {
        delete_vcom_name <- append(delete_vcom_name,substr(vd$name[1],1,(nchar(vd$name[1])-1)))
      }
    }
  }
  #print("v.com_names")
  #print(names(com.env$v.com))
  #print("delete_vcom_name")
  #print(delete_vcom_name)
  for (nam in delete_vcom_name) {
    cmd_string <- paste("com.env$vcom_names <- com.env$vcom_names[-(which(com.env$vcom_names %in% com.env$v.com$",nam,"$name))]",sep="")
    #print(cmd_string)
    eval(parse(text=cmd_string))
    cmd_string <- paste("com.env$v.com$",nam," <- NULL",sep="")
    #print(cmd_string)
    eval(parse(text=cmd_string))
  }
  #com.env$vcom_names <- com.env$vcom_names[-(which(com.env$vcom_names %in% delete_vcom_name))]
  print(paste("after vcom cleaning vars=",length(com.env$v.com)))
}

eval_adj_r2 <- function(vd=NULL,orig_vd=NULL,old_adj_r2=0,oos_data=FALSE) {
  #print(paste("make_vars",Sys.time()))
  vd <- make_vars(vd)                   #make modified variable & update columns in vd (for all stock in var.env)
  if (!is.null(vd)) { #insert modified vd into com.env$v.com
    old.v.com <- com.env$v.com
    #orig_vd <- com.env$v.com[[vd$vcom_num]]
    com.env$v.com[[vd$vcom_num]] <- vd                                  #replace modified vd in v.com
    names(com.env$v.com)[vd$vcom_num] <- vd$var_name
    #if (length(orig_vd$name) > length(vd$name)) {                        #if deleted bin change vd$name in names(v.com)
    #  names(com.env$v.com)[vd$vcom_num] <- vd$name
    #  print(paste("changing bin name",orig_vd$name,vd$name,vd$vcom_num))
    #}
  }
  #print(paste("collect_data",Sys.time()))
  collect_data(oos_data)
  #print(paste("run_regression",Sys.time()))
  if (is.null(vd)) {
    run_regression(oos_data)
  } else {                           #run_mod_regression        #only works for bin length <= 2, ticker BAC hardcoded
    if (!(length(orig_vd$name) == length(vd$name))) {
      print("bin var name lengths not the same, must run full regression")
      run_regression(oos_data)
    } else {
      #print("eval_adj_r2 with mod variable")
      #print(orig_vd$name)
      #print(vd$name)
      orig_col <- orig_vd$col
      if (!is.null(com.env$override_col)) orig_col <- com.env$override_col
      new_col <- vd$col
      com.env$mod_col <- new_col
      #print(paste(orig_col,new_col))
      reg_names <- com.env$reg_names
      #print(reg_names)
      if ((length(orig_vd$name) > 1) & (length(orig_vd$name)==length(vd$name))) {
        #print("dealing with bin_var")
        #print(paste(length(orig_vd$name),length(vd$name)))
        #orig_names_used <- (orig_vd$name %in% reg_names)
        #print(orig_names_used)
        new_name <- NULL
        orig_name <- NULL
        for (i in 1:length(orig_vd$name)) {
          name2remove <- names(var.env$BAC)[orig_col+i-1]
          #print(paste("check:",name2remove))
          if (name2remove %in% reg_names) {
            orig_name <- c(orig_name,name2remove)
            new_name <- c(new_name,names(var.env$BAC)[new_col + i - 1])
            #print(paste("add:",names(var.env$BAC)[new_col+i-1]))
          }
        }
        #print(orig_name)
        #print(new_name)
      } else {
        orig_name <- names(var.env$BAC)[orig_col]
        new_name <- names(var.env$BAC)[new_col]
        #print(paste("single name replacement:",orig_name,new_name))
      }
      new_reg_names <- reg_names[!(reg_names %in% orig_name)]
      new_reg_names <- c(new_reg_names,new_name)
      #print(new_reg_names)
      run_mod_regression(new_reg_names)
    }
  }
  new_adj_r2 <- 0
  if (length(com.env$model.stepwise) > 3) new_adj_r2 <- summary(com.env$model.stepwise)$adj.r.squared
  if (!is.null(vd) & new_adj_r2 <= old_adj_r2) { #revert to original com.env$v.com  #do we need to clean up var.env?
    com.env$v.com <- old.v.com
  }
  return(new_adj_r2)
  #print(paste("Done with regression,",Sys.time()))
}

opt_model <- function(model_loops,add_vars,mod_var_loops) {    
  #source("define_vars.R")                         #define predictor var, setup initial v.com
  print(paste("In opt_model, model_loops:",model_loops,"add_vars:",add_vars,"mod_var_loops:",mod_var_loops))
  com.env$best_adj_r2 <- 0
  for (l in 1:model_loops) {              #start model loop
    print(paste("model loop:",l,"/",model_loops,Sys.time()))
    for (i in 1:add_vars) rnd_var()       #add new vars to v.com
    #print("eval_adj_r2 add var loop")
    #print(names(com.env$v.com))
    if (!check_dependencies()) {
      print("Dependency problem when adding new vars, removing var env and reverting to old v.com")
      print(names(com.env$v.com))
      com.env$v.com <- com.env$best_vcom
      rm(var.env,envir=globalenv())
      var.env <<- new.env(parent = globalenv())
      next
    }
    ls(var.env)
    orig_adj_r2 <- eval_adj_r2()
    if (com.env$best_adj_r2 > orig_adj_r2) {
      print(paste("Adj_r2 got worse when adding vars",orig_adj_r2,com.env$best_adj_r2))
      print("Reverting to previous com.env$v.com********************************************")
      #print(names(com.env$v.com))
      #print(names(com.env$best_vcom))
      com.env$v.com <- com.env$best_vcom
      print("Removing var environment, add var loop")
      rm(var.env,envir=globalenv())
      var.env <<- new.env(parent = globalenv())
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
      rm(var.env,envir=globalenv())
      var.env <<- new.env(parent = globalenv())
      next
    }
    
    #mod vars
    loops <- 0
    check_adj_r2 <- orig_adj_r2
    com.env$reg_names <- names(com.env$model.stepwise$coefficients)[-1]
    com.env$reg_vcom_names <- get_reg_names(com.env$reg_names)
    com.env$override_col <- NULL
    #print(paste("reg_vars=",length(com.env$reg_names)))
    #print(com.env$reg_vcom_names)
    model_worse <- TRUE
    #don't mod variables on last loop 
    while ((model_worse) & (loops < mod_var_loops) & (l<com.env$model_loops)) {
      #print(names(com.env$v.com))
      loops <- loops + 1
      #new_vd <- NULL
      #new_vd$ID <- -1
      new_vd <- rnd_mod(reg_names=com.env$reg_vcom_names)
      #print(new_vd)
      if (new_vd$ID != -1) {
        #print(paste("save away orig_vd",new_vd$vcom_num))
        orig_vd <- com.env$v.com[[new_vd$vcom_num]]       #store away original variable definition in vd_tmp
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
          print(paste("model improved",new_adj_r2,orig_adj_r2,"loop#",loops,new_vd$var_name))
          com.env$best_adj_r2 <- new_adj_r2
          com.env$override_col <- com.env$mod_col
          com.env$reg_names <- names(com.env$model.stepwise$coefficients)[-1]
          new_vd <- optimize_mod(orig_vd,new_vd,orig_adj_r2,new_adj_r2)
        } else {
          #print(paste("model_worse",orig_adj_r2,new_adj_r2,"loop#",loops,"/",com.env$mod_var_loops,Sys.time()))
        }
      } #end if new_vd is valid
    } #end mod while loop
    #print("Removing var environment, mod var loop")
    rm(var.env,envir=globalenv())
    var.env <<- new.env(parent = globalenv())
    #print(names(com.env$v.com))  
    #ls(var.env)
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
  
  #print(paste("model loop:",l+1,"/",com.env$model_loops,Sys.time()))
  #for (i in 1:com.env$add_vars) rnd_var()       #add new vars to v.com
  #print("eval_adj_r2, oos_data=TRUE")
  #orig_adj_r2 <- eval_adj_r2(oos_data=TRUE)
  #print("clean_vcom")
  #clean_vcom()
  
  
} #end function opt_model

load_model <- function(filename) { #loads model and sets com.env$predict.ret
  print(paste("function load_model",filename))
  modelfile <- paste(com.env$modeldir,"/",filename,sep="")
  load(file=modelfile,envir=com.env)
  com.env$predict.ret <- com.env$v.com[[2]]$name  #hard coded, first define 'C' then lf var in define_vars.R
  print(paste("Loading:",modelfile,"predict:",com.env$predict.ret))
  #eval_adj_r2(oos_data=TRUE)
}

save_model <- function(filename) {
  print(paste("function save_model",filename))
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

save_vars <- function(save_var_n) {
  print(paste("function save_vars",save_var_n))
  if (save_var_n == 0) return()
  f1 <- as.formula(paste(com.env$predict.ret,"~.",sep=""))
  #print(f1)
  if (length(colnames(var.env$reg_data.df)) > 50) {
    print("model.subsets really.big")
    #print(colnames(var.env$reg_data.df))
    model.subsets <- regsubsets(f1,data=var.env$reg_data.df,nbest=save_var_n,nvmax=1,really.big=TRUE)
  } else {
    print("model.subsets small")
    #print(colnames(var.env$reg_data.df))
    model.subsets <- regsubsets(f1,data=var.env$reg_data.df,nbest=save_var_n,nvmax=1)
  }
  var_names <- NULL
  for (i in 1:save_var_n) {
    var_names <- append(var_names,names(coef(model.subsets,i))[2])
  }
  print(var_names)
  save_vcoms <- get_reg_names(var_names,return_num=TRUE)
  print(save_vcoms)
  save_vcom_vars(save_vcoms)
}

