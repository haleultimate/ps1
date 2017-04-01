#run_regression.R

#remove all colinear dependent variables 
run_regression <- function(oos_data = FALSE,verbose = FALSE) {
  print(paste("run full regression",ncol(var.env$reg_data.df),Sys.time()))
  if (ncol(var.env$reg_data.df) > 2) {
    keep.dat <- vif_func(var.env$reg_data.df[,-1],thresh=10,trace=FALSE) #don't include predict.ret
    keep.dat <- append(colnames(var.env$reg_data.df)[1],keep.dat)
    if (verbose) print(keep.dat)
    var.env$reg_data.df <- var.env$reg_data.df[,keep.dat]
  }
  if (verbose) print(str(var.env$reg_data.df))
  
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
  print(paste("end regression",adj_r2,Sys.time()))
  if (verbose) print(names(com.env$model.stepwise$coefficients)[-1])
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
clean_vcom <- function(verbose=FALSE) {
  print(paste("clean_vcom",Sys.time()))
  #reg_names <- names(com.env$model.stepwise$coefficients)[-1]  #get names from regression (not var_names)
  #print("names from regression")
  reg_names <- append(com.env$predict.ret,com.env$reg_names)
  keep_vcom_name <- NULL
  print(paste("reg_names from coefs + predictor:",length(reg_names)))
  for (nam in reg_names) { #convert binned variables and ID's to their var_name
    length_nam <- nchar(nam)
    if ((substr(nam,length_nam,length_nam) == 'h') | (substr(nam,length_nam,length_nam) == 'l')) {
      nam <- substr(nam,1,(length_nam-1))
    }
    if (substr(nam,1,1)=="v") { #find var_name of ID
      name_found <- FALSE
      nam <- substr(nam,2,nchar(nam)) #strip off "v"
      for (vd in com.env$v.com) {
        if (vd$ID == nam) {
          nam <- vd$var_name
          #print(nam)
          name_found <- TRUE
          break
        }
      }
      if (!name_found) {
        print("ERROR in clean_vcom, ID not converted to var_name")
        print(nam)
        for (vd in com.env$v.com) print(vd$ID)
        source("close_session.R")
      }
    }
    keep_vcom_name <- c(keep_vcom_name,nam)
  }
  #print("keep_vcom_names from regression")
  #print(keep_vcom_name)
  keep_vcom_name <- unique(keep_vcom_name)
  print(paste("#reg vars:",length(keep_vcom_name)))
  #print(keep_vcom_name)
  
  vdlist <- NULL
  for (i in 1:length(keep_vcom_name)) {
    #print(keep_vcom_name[[i]])
    V1 <- com.env$v.com[[keep_vcom_name[[i]]]]
    V1$requires <- get_requires_from_vd(V1)
    #print(V1$var_name)
    #print(V1$requires)
    vdlist[[i]] <- vcom2vdlist_req(V1)
    #print(paste("requires list:",length(V1$requires),"vdlist:",length(vdlist[[i]])))
  }
  #print(paste("keep_vcom_name",length(keep_vcom_name),"vdlist:",length(vdlist)))
  #stop()
  com.env$v.com <- NULL  
  for (i in 1:length(vdlist)) {
    vdlist2vcom(vdlist[[i]])
  }
  #print(names(com.env$v.com))
}

#if no mod list is given, function computes all vars in com.env$v.com and runs stepwise regression using each vd$name as given
#if mod list is given, com.env$v.com is copied and modified 
#    function computes only the new mod vars (appended to end of data frames in var.env) named with vd$longID_name [needed to keep small changes unique]
#    single regression is run replacing the orig_var with mod_var [com.env$opt determines whether name->longID_name or longID_name->longID_name conversion]
#    if adj_r2 improves keep com.env$v.com, otherwise revert to original com.env$v.com
#oos_data determines if OOS date range is computed, verbose controls printing (for debugging)  [both passed directly to collect_data and regression routines]
#Other parms modified: com.env$v.com, com.env$reg_names, com.env$best_adj_r2
#Other parms accessed: com.env$opt, colnames(var.env$BAC) [hardcoded]
eval_adj_r2 <- function(mod_pair=NULL,oos_data=FALSE,verbose=FALSE) {
  if (!is.null(mod_pair)) {
    #print(paste("make_vars with modvar_list",Sys.time()))
    V1 <- mod_pair[[1]]
    V2 <- make_vars(mod_pair[[2]])                   #make modified variable & update columns in vd (for all stock in var.env)
    old.v.com <- com.env$v.com
    com.env$v.com[[V2$vcom_num]] <- V2
    names(com.env$v.com)[V2$vcom_num] <- V2$var_name
    if (V2$use == "model") {        #only one var to calculate
      mod_list <- list(mod_pair)
    } else if (V2$use == "calc") {  #must get all model vars requiring calc var (fix com.env$v.com in clean_vcom)
      #not coded yet, create mod_list starting with calc var, then all model vars requiring calc_var 
      print("Support for calc vars not coded yet in eval_adj_r2")
      print(mod_pair[[1]])
      print(mod_pair[[2]])
      source("close_session.R")
    } else {
      print("Can't modify raw or data vars in eval_adj_r2")
      print(mod_pair[[1]])
      print(mod_pair[[2]])
      source("close_session.R")
    }
  } else {
    make_vars()  #eval_adj_r2 normally
  }
  #print(paste("collect_data",Sys.time()))
  collect_data(oos_data)
  #print(paste("run_regression",Sys.time()))
  if (is.null(mod_pair)) {
    run_regression(oos_data=oos_data,verbose=verbose)
  } else {                           #run_mod_regression        
    reg_names <- com.env$reg_names
    #print(paste("reg_names",length(reg_names)))
    #print(reg_names)
    new_reg_names <- reg_names
    cnBAC <- colnames(var.env$BAC)
    for (vd_pair in mod_list) {
      V1 <- vd_pair[[1]]
      V2 <- vd_pair[[2]]
      #print(V1$name)
      #print(V2$longID_name)
      #print(new_reg_names %in% V1$name)
      #print(com.env$opt)
      #print(new_reg_names %in% V1$longID_name)
      if (com.env$opt) {
        new_reg_names <- new_reg_names[!(new_reg_names %in% V1$longID_name)]
      } else {
        new_reg_names <- new_reg_names[!(new_reg_names %in% V1$name)]
      }
      #new_reg_names <- ifelse(com.env$opt,new_reg_names[!(new_reg_names %in% V1$longID_name)],new_reg_names[!(new_reg_names %in% V1$name)])
      #print(new_reg_names)
      #if (is.na(new_reg_names)) new_reg_names <- NULL
      name_found <- FALSE
      error_text <- NULL
      if ((length(V2$name)>1) & (length(V1$name)==length(V2$name))) {  #new var and old var are bin vars
        if (com.env$opt) {  #longID_name to longID_name
          for (i in 1:length(V2$longID_name)) {
            if (V1$longID_name[[i]] %in% reg_names) {
              new_reg_names <- c(new_reg_names,V2$longID_name[[i]])
              if (!(V1$longID_name[[i]] %in% cnBAC)) error_text <- paste(V1$longID_name[[i]],"found in reg_names, but not in colnames var.env$BAC")
              if (!(V2$longID_name[[i]] %in% cnBAC)) error_text <- paste(V2$longID_name[[i]],"found in new_reg_names, but not in colnames var.env$BAC")
              name_found <- TRUE
            }
          }
        } else {            #named var to longID_name
          for (i in 1:length(V2$name)) {
            if (V1$name[[i]] %in% reg_names) {
              new_reg_names <- c(new_reg_names,V2$longID_name[[i]])
              if (!(V1$name[[i]] %in% cnBAC)) error_text <- paste(V1$name[[i]],"found in reg_names, but not in colnames var.env$BAC")
              if (!(V2$longID_name[[i]] %in% cnBAC)) error_text <- paste(V2$longID_name[[i]],"found in new_reg_names, but not in colnames var.env$BAC")
              name_found <- TRUE
            }
          }
        }
      } else {                                                        #no binning, added bin, or deleted bin
        if (com.env$opt) {  #longID_name to longID_name
          new_reg_names <- c(new_reg_names,V2$longID_name)
          name_found <- ifelse((length(V1$longID_name) == 2),((V1$longID_name[[1]] %in% reg_names) | (V1$longID_name[[2]] %in% reg_names)),(V1$longID_name[[1]] %in% reg_names))
          V1_in_colnames <- ifelse((length(V1$longID_name) == 2),((V1$longID_name[[1]] %in% cnBAC) | (V1$longID_name[[2]] %in% cnBAC)),(V1$longID_name[[1]] %in% cnBAC))
          if (!(V1_in_colnames)) error_text <- paste(V1$name,"found in reg_names, but not in colnames var.env$BAC")
          if (!(V2$longID_name %in% cnBAC)) error_text <- paste(V2$longID_name,"found in new_reg_names, but not in colnames var.env$BAC")
        } else {            #named var to longID_name
          new_reg_names <- c(new_reg_names,V2$longID_name)
          name_found <- ifelse((length(V1$name) == 2),((V1$name[[1]] %in% reg_names) | (V1$name[[2]] %in% reg_names)),(V1$name[[1]] %in% reg_names))
          V1_in_colnames <- ifelse((length(V1$name) == 2),((V1$name[[1]] %in% cnBAC) | (V1$name[[2]] %in% cnBAC)),(V1$name[[1]] %in% cnBAC))
          if (!(V1_in_colnames)) error_text <- paste(V1$name,"found in reg_names, but not in colnames var.env$BAC")
          if (!(V2$longID_name %in% cnBAC)) error_text <- paste(V2$longID_name,"found in new_reg_names, but not in colnames var.env$BAC")
        }
      }
      if (!name_found) error_text <- paste(V1$name,"not found in reg_names",length(V1$name),length(V2$name),reg_names)
      if (!is.null(error_text)) {  #error checking to make sure names are available for regression
        print(error_text)
        source("close_session.R")
      }
    }
    #print(paste("new_reg_names:",length(new_reg_names),"reg_names:",length(reg_names)))
    #print(new_reg_names)
    run_mod_regression(new_reg_names)
  }
  new_adj_r2 <- 0
  if (length(com.env$model.stepwise) > 3) new_adj_r2 <- summary(com.env$model.stepwise)$adj.r.squared
  if (!is.null(mod_pair) & (new_adj_r2 <= com.env$best_adj_r2)) { #revert to original com.env$v.com  #do we need to clean up var.env?
    #print(paste("Not better in eval_adj_r2 mod loop, old_adj_r2=",old_adj_r2,"new_adj_r2=",new_adj_r2))
    com.env$v.com <- old.v.com
  } else {
    #reg_names <- names(com.env$model.stepwise$coefficients)[-1]  #update reg_names with new mod
    #print(paste("Better in eval_adj_r2 mod loop, old_adj_r2=",old_adj_r2,"new_adj_r2=",new_adj_r2))
  }
  return(new_adj_r2)
  #print(paste("Done with regression,",Sys.time()))
}

opt_model <- function(model_loops,add_vars,mod_var_loops) {    
  #source("define_vars.R")                         #define predictor var, setup initial v.com
  print(paste("In opt_model, model_loops:",model_loops,"add_vars:",add_vars,"mod_var_loops:",mod_var_loops))
  com.env$best_adj_r2 <- 0
  com.env$ID_tried <- NULL
  model_worse <- FALSE
  test_clean_vcom <- FALSE
  test_worse_vcom <- FALSE
  test_reverted_vcom <- TRUE
  for (l in 1:model_loops) {              #start model loop
    print(paste("model loop:",l,"/",model_loops,Sys.time()))
    com.env$opt <- FALSE
    if (!model_worse) {
      print("ADD VARS")
      for (i in 1:add_vars) make_new_model_var()    #add new vars to v.com
    }
    #print("eval_adj_r2 add var loop")
    #print(names(com.env$v.com))
    if (!check_dependencies()) {
      print("Dependency problem when adding new vars, removing var env and reverting to old v.com")
      print(names(com.env$v.com))
      source("close_session.R")
      com.env$v.com <- com.env$best_vcom
      rm(var.env,envir=globalenv())
      var.env <<- new.env(parent = globalenv())
      next
    }
    
    orig_adj_r2 <- eval_adj_r2()
      
    if (orig_adj_r2 < com.env$best_adj_r2)  {  #model worse
      print(paste("Adj_r2 got worse when adding vars",orig_adj_r2,com.env$best_adj_r2))
      print("Reverting to previous com.env$v.com********************************************")
      if (test_worse_vcom) {
        print("Model_worse, Removing var environment, checking regression run")
        rm(var.env,envir=globalenv())
        var.env <<- new.env(parent = globalenv())
        new_adj_r2 <- eval_adj_r2(verbose=FALSE)
        print(paste("best:",com.env$best_adj_r2,"1st try:",orig_adj_r2,"2nd try:",new_adj_r2))
      }
      com.env$v.com <- com.env$best_vcom
      print("Model_worse, Removing var environment, reverting to old v.com")
      rm(var.env,envir=globalenv())
      var.env <<- new.env(parent = globalenv())
      if (test_reverted_vcom) {
        orig_adj_r2 <- eval_adj_r2(verbose=FALSE)
        print(paste("Model reset to best_adj_r2:",orig_adj_r2," from:",com.env$best_adj_r2))
        if (orig_adj_r2 != com.env$best_adj_r2) {
          print("***********PROBLEM:changing best_adj_r2**********************************")
          com.env$best_adj_r2 <- orig_adj_r2
        }
        print("Removing var environment, reverted vcom and tested")
        rm(var.env,envir=globalenv())
        var.env <<- new.env(parent = globalenv())
      }
      next
    }
    
    com.env$best_adj_r2 <- orig_adj_r2
    com.env$reg_names <- names(com.env$model.stepwise$coefficients)[-1]
    com.env$reg_vcom_names <- get_reg_names(com.env$reg_names)
    if (test_clean_vcom) com.env$best_vcom <- com.env$v.com
    clean_vcom()

    if (test_clean_vcom & (orig_adj_r2>0)) {
      print("Removing var environment, testing cleaned vcom")
      rm(var.env,envir=globalenv())
      var.env <<- new.env(parent = globalenv())
      clean_adj_r2 <- eval_adj_r2()
      if (orig_adj_r2 != clean_adj_r2) {
        print(paste("orig_adj_r2:",orig_adj_r2," clean_adj_r2:",clean_adj_r2))
        print(summary(com.env$model.stepwise))
        com.env$v.com <- com.env$best_vcom
        print("Removing var environment, testing reverted vcom (with removed model vars added back in)")
        rm(var.env,envir=globalenv())
        var.env <<- new.env(parent = globalenv())
        revert_adj_r2 <- eval_adj_r2(verbose=FALSE)
        print(summary(com.env$model.stepwise))
        source("close_session.R")
      }
    }
    
    com.env$best_vcom <- com.env$v.com
    
    if ((orig_adj_r2==0) | (com.env$mod_var_loops==0)) {  #next loop if no model or no mod_var_loops
      print(paste("Removing var environment, add var loop, test_clean_vcom:",test_clean_vcom))
      rm(var.env,envir=globalenv())
      var.env <<- new.env(parent = globalenv())
      next
    }
    
    #mod vars
    loops <- 0
    check_adj_r2 <- com.env$best_adj_r2
    model_worse <- TRUE
    #com.env$ID_tried <- sort(com.env$ID_tried)
    #print(com.env$ID_tried)
    while ((model_worse) & (loops < mod_var_loops) & (l<com.env$model_loops)) {    #don't mod variables on last loop 
      loops <- loops + 1
      mod_pair <- mod_var("model")
      if (is.null(mod_pair)) next()  #mod_var already tried, loop again
      #print("mod_var model")
      new_adj_r2 <- eval_adj_r2(mod_pair=mod_pair)
      if (new_adj_r2 > com.env$best_adj_r2) {
        model_worse <- FALSE
        print(paste("model improved",new_adj_r2,orig_adj_r2,"loop#",loops,"/",com.env$mod_var_loops,Sys.time()))
        print(mod_pair[[1]]$math)
        print(mod_pair[[2]]$math)
        com.env$best_adj_r2 <- new_adj_r2
        com.env$reg_names <- names(com.env$model.stepwise$coefficients)[-1]
        #print("OPT VAR")
        com.env$opt <- TRUE
        optimize_mod_pair(mod_pair)  
        #if calc_name returned need to clean com.env$v.com [longID_name <- calc_name]
      } else {
        print(paste("model_worse",orig_adj_r2,new_adj_r2,"loop#",loops,"/",com.env$mod_var_loops,Sys.time()))
      }
    } #end mod while loop
    #print("Removing var environment, mod var loop")
    rm(var.env,envir=globalenv())
    var.env <<- new.env(parent = globalenv())
    #print(names(com.env$v.com))  
    if (com.env$best_adj_r2 < check_adj_r2) {
      print(paste("Adj_r2 got worse when modding vars, before:",check_adj_r2,"after mod:",com.env$best_adj_r2))
      print("Reverting to previous com.env$v.com*************************************************")
      com.env$v.com <- com.env$best_vcom
      com.env$best_adj_r2 <- check_adj_r2
      next
    } else if (length(com.env$v.com) != length(unique(names(com.env$v.com)))) { #check for duplicate var name
      print(paste("Found better adj_r2",com.env$best_adj_r2,"but requires duplicate var name, reverting v.com"))
      print(names(com.env$v.com))
      com.env$v.com <- com.env$best_vcom
      com.env$best_adj_r2 <- check_adj_r2
      print(paste("Reverting to adj_r2",check_adj_r2))
      print(names(com.env$v.com))
    } else {
      clean_vcom()
      com.env$best_vcom <- com.env$v.com
      #check_ids(com.env$ID_tried)
    }
  }                                               #end model loop

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
  if (!com.env$load_model) { #check if filename already exists, if loaded overwrite file
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

