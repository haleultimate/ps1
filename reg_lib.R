#run_regression.R

#run_prediction is main program for reg_lib.R
#com parms set in init_lib.R
#primary functions: opt_model -> calc_adj_r2 -> run_regression
#com.env$v.com contains all variable definitions
#var.env contains all calculated variables (by stock & reg_data.df)
#make_lib.R, rnd_lib.R controls variable creation process
run_prediction <- function() {
  if (com.env$opt_model)   sample_vars()            #source("sample_vars.R")  #set up rnd.env$vs.com and rnd_parms
  if (!com.env$load_model) define_predict_ret()     #source("define_vars.R")  #if not loading model need to define predictor variable
  if (com.env$load_model) load_model(com.env$model_filename)
  if (com.env$opt_model) opt_model(com.env$model_loops,com.env$mod_var_loops)
  if (com.env$save_model) save_model(com.env$model_filename)
  if (com.env$save_var_n > 0) {
    print(paste("Evaluating model to save top",com.env$save_var_n,"/",length(com.env$best_reg_names),"total vars",Sys.time()))
    eval_adj_r2(oos_data=TRUE)
    save_vars(com.env$save_var_n) #get top vars
  }
}

#optimize regression model based on adj_r2 to best calculate com.env$predict.ret [in-sample]
#Each loop try two things:
#(1) Add vars to com.env$v.com (variable definitions to base all calculations on)
#    Calculate all vars in var.env, Run stepwise regression
#    Creates com.env$best_reg_names giving list of regression vars (and corresponding com.env$best_adj_r2)
#(2) Modify vars in com.env$v.com
#    Change random definition slightly, recalculate that one var, Run single regression (using com.env$best_reg_names from step 1)
#    Repeat until first improvement is found
#Clean com.env$v.com (remove all non-significant variable definitions), delete var.env, repeat loop
#FINAL RESULT: com.env$v.com with variable definitions (how to calculate out-of-sample variables)
#Rerunning eval_adj_r2(oos=TRUE) with this com.env$v.com provides: 
#  var.env$ret_data.df containing calculations from final model (in-sample and out-of-sample)
#  com.env$stepwise.model containing coefficients for producing expected returns given out-of-sample variables
opt_model <- function(model_loops,mod_var_loops) {    
  #source("define_vars.R")                         #define predictor var, setup initial v.com
  print(paste("In opt_model, model_loops:",model_loops,"mod_var_loops:",mod_var_loops))
  com.env$best_adj_r2 <- 0
  com.env$best_reg_names <- NULL
  com.env$best_vcom <- NULL
  com.env$ID_tried <- NULL                    #don't repeat model variables
  com.env$add_var_worse <- FALSE
  start_time <- Sys.time()
  for (current_add_loop in 1:model_loops) {              #start model loop
    #add vars
    print(paste("model loop:",current_add_loop,"/",model_loops,"best_adj_r2:",com.env$best_adj_r2,Sys.time()))
    add_vars <- get_add_vars()
    if (add_vars>0) for (i in 1:add_vars) make_new_model_var()
    # if (!com.env$add_var_worse)  #if model_worse last loop don't add vars to v.com 
    #   for (i in 1:get_add_vars(add_var_levels,length(com.env$best_reg_names))) #add fewer vars as model develops 
    #     make_new_model_var()
    #run regression, if model improved update best vars; if model got worse revert and cycle add_var loop
    if (!try_mods(eval_adj_r2(),current_add_loop,model_loops,mod_var_loops)) next    
    
    #mod vars
    #cat("Entering mod_vars with",length(com.env$best_reg_names),"vars, best_adj_r2=",com.env$best_adj_r2,'\n')
    for (current_mod_loop in 1:mod_var_loops)       #end of loop controlled by finish_mod_loop below
      #run regression, if model improved optimize, update best vars and end loop; if model got worse revert and continue
      if (finish_mod_loop(mod_var("model"))) break  #try a random model var mod  
    print(paste("Mod Loops attempted:",current_mod_loop,"/",mod_var_loops,",",length(com.env$best_reg_names),com.env$best_adj_r2,Sys.time()))
    
    #print("Removing var environment, after mod var loop")
    rm(var.env,envir=globalenv())
    var.env <<- new.env(parent = globalenv())
  }                                               #end model loop
  print(paste("Start_time:",start_time,"End_time:",Sys.time(),"Elapsed:",round(Sys.time()-start_time),"minutes"))
  cat("Final adj R2:",com.env$best_adj_r2,'\n')
  cat("Reg vars:",length(com.env$best_reg_names),com.env$best_reg_names,'\n')
} #end function opt_model

#determine number of new vars to add to regression
#always provide at least the first level (in total), after the last level only add 1 at a time
#at intermediate levels add one more than the next level
get_add_vars <- function() {
  n.reg_vars <- length(com.env$best_reg_names)
  if (com.env$best_adj_r2 == 0) { #first loop
    add_vars <- ifelse(com.env$load_model,0,com.env$add_var_levels[1])     
  } else if (com.env$add_var_worse) {  #if last loop was bad don't add 
    add_vars <- 0
  } else {                             #normal operation
    avl_idx <- 1
    max_avl_idx <- length(com.env$add_var_levels)
    while(avl_idx <= max_avl_idx) {
      if (n.reg_vars < com.env$add_var_levels[avl_idx]) {
        add_vars <- max_avl_idx - avl_idx + 2
        if (avl_idx == 1) add_vars <- max(add_vars,(com.env$add_var_levels[1]-n.reg_vars))
        return(add_vars)
      }
      avl_idx <- avl_idx + 1
    }
    add_vars <- 1  #beyond last add_var_level
  }
  #cat("add_vars:",add_vars,"\n")
  return(add_vars)
}

#if model improved update com.env$best_reg_vars, com.env$best_adj_r2, com.env$best_vcom (return(TRUE))
#if model didn't improve delete var.env, revert to best previous com.env$v.com (return(FALSE))
try_mods <- function(adj_r2,current_loop,model_loops,mod_var_loops) {
  return_try_mods <- (mod_var_loops > 0)    #don't mod if no mod_var_loops
  if (adj_r2 > com.env$best_adj_r2) {
    com.env$best_reg_names <- com.env$reg_names
    cat("Model Improved in add_vars from:",com.env$best_adj_r2,"->",adj_r2,"with",length(com.env$best_reg_names),"variables.\n")
    com.env$best_adj_r2 <- adj_r2
    clean_vcom()                                      #remove var definitions not needed in regression
    com.env$best_vcom <- com.env$v.com
    com.env$add_var_worse <- FALSE
    return(return_try_mods)
  } else if (adj_r2 < com.env$best_adj_r2) {
    print(paste("Adj_r2 got worse when adding vars",adj_r2,com.env$best_adj_r2))
    if (com.env$add_var_worse) {                                          #Error trapping, clean run still got worse
      print("WARNING: *************Couldn't get better model from clean run of add_vars****************")
      #source("close_session.R")                                             #if this happens systematically we could reset the "best" vars
      cat("Resetting best vars, best_adj_r2",adj_r2,"best_reg_names (count):",length(com.env$reg_names),"v.com reset \n")
      com.env$best_adj_r2 <- adj_r2
      com.env$best_reg_names <- com.env$reg_names
      com.env$best_vcom <- com.env$v.com
    }
    com.env$add_var_worse <- TRUE             #try clean vcom without adding vars next loop
    com.env$v.com <- com.env$best_vcom        #revert vcom
    rm(var.env,envir=globalenv())             #delete var environment
    var.env <<- new.env(parent = globalenv())
    return(FALSE)
  } else if (adj_r2 == com.env$best_adj_r2) {
    if (com.env$add_var_worse) {
      cat("Model successfully reverted to previous best vcom\n")
    } else if (adj_r2 == 0) {
      cat("No Model found, trying add vars again\n")
      rm(var.env,envir=globalenv())             #delete var environment
      var.env <<- new.env(parent = globalenv())
      return(FALSE)
    } else {
      cat("Model didn't improve or get worse in add_vars, cleaning vcom and continuing to mod vars\n")
      clean_vcom()
    }
    com.env$add_var_worse <- FALSE
    return(return_try_mods)
  }
}

#runs mod_loop regression with mod_pair passed in
#if model improves try to optimize; when done update best vars (com.env$[best_reg_vars, best_adj_r2, best_vcom]) and clean var.env (return(TRUE))
#if worse (return(FALSE)) [try again]
finish_mod_loop <- function(mod_pair) {
  if (is.null(mod_pair)) return(FALSE)
  if (mod_pair[[2]]$use == "scale") com.env$ID_scale_opt <- mod_pair[[2]]$ID  #don't repeat scale var mods in same mod_var_loop
  mod_adj_r2 <- eval_adj_r2(mod_pair=mod_pair)  #run regression with modified variable
  if (mod_adj_r2 <= com.env$best_adj_r2) return(FALSE)
  print(paste("Model improved",com.env$best_adj_r2,"->",mod_adj_r2,'adjusting',mod_pair[[2]]$var_name,Sys.time()))
  #print(mod_pair[[1]]$math)
  #print(mod_pair[[2]]$math)
  com.env$best_adj_r2 <- mod_adj_r2
  com.env$best_reg_names <- com.env$reg_names
  optimize_mod_pair(mod_pair) #continually update best vars until no more optimization possible
  clean_vcom()
  com.env$best_vcom <- com.env$v.com 
  return(TRUE)
}

#if no mod list is given, function computes all vars in com.env$v.com, placing data in var.env, and runs stepwise regression using model vars
#if mod list is given, com.env$v.com is copied and modified (var.env assumed to be filled with all previous calcs)
#    function computes only the new mod vars (appended to end of data frames in var.env) 
#    named with vd$longID_name [needed to keep small changes unique]
#    single regression is run replacing the orig_var with mod_var
#    if adj_r2 improves keep com.env$v.com, changing all longID_name's back to var_name
#    otherwise revert to original com.env$v.com
#oos_data determines if OOS date range is computed, verbose controls printing (for debugging)  [both passed directly to collect_data and regression routines]
#Other vars modified: com.env$v.com, var.env$reg_data.df, com.env$reg_names, com.env$model.stepwise
#Other vars accessed: colnames(var.env$BAC) [hardcoded; only for error checking]
eval_adj_r2 <- function(mod_pair=NULL,oos_data=FALSE,verbose=FALSE) {
  if (!is.null(mod_pair)) {  #only calc modified vars and vars dependent on them
    #print(paste("make_vars with modvar_list",Sys.time()))
    V1 <- mod_pair[[1]]
    V2 <- make_vars(mod_pair[[2]])                   #make modified variable & update columns in vd (for all stock in var.env)
    old.v.com <- com.env$v.com
    com.env$v.com[[V2$vcom_num]] <- V2
    names(com.env$v.com)[V2$vcom_num] <- V2$var_name
    if (V2$use == "model") {        #only one var to calculate
      mod_list <- list(mod_pair)
    } else if (V2$use == "scale") {  #must get all model vars requiring scale var (fix com.env$v.com in clean_vcom)
      mod_list <- get_mod_list_req_mod_pair(mod_pair)
      for (mod_pair in mod_list) {  #check if any model variables have been tried before (if so return 0)
        if (mod_pair[[2]]$use == "model") {
          if (mod_pair[[2]]$ID %in% com.env$ID_tried) {
            print(paste("Warning:In eval_adj_r2",mod_pair[[2]]$var_name,"already tried, return adj_r2 = 0, revert com.env$v.com"))
            if (exists("reg_names",envir=com.env)) rm("reg_names",envir=com.env)
            com.env$v.com <- old.v.com
            return(0)
          } else {
            com.env$ID_tried <- c(com.env$ID_tried,mod_pair[[2]]$ID)
          }
        }
      }
    } else {
      print("Can't modify raw or data vars in eval_adj_r2")
      print(mod_pair[[1]])
      print(mod_pair[[2]])
      source("close_session.R")
    }
  } else {
    make_vars()  #eval_adj_r2 normally by calculating all vars in com.env$v.com
  }
  collect_data(oos_data)  #populate reg_data.df with model vars
  if (is.null(mod_pair)) {     #eval_adj_r2 normally
    run_regression(oos_data=oos_data,verbose=verbose)    #run full stepwise regression after first removing colinear vars
  } else {                     #run_mod_regression        
    new_reg_names <- reg_names_use_longID_name(mod_list)
    run_mod_regression(new_reg_names)  #run single regression with forced vars (using replaced longID_names where needed)
  }
  new_adj_r2 <- 0
  # if (!exists("best_adj_r2",envir=com.env)) {
  #   print("Warning: com.env$best_adj_r2 did not exist in eval_adj_r2")
  #   com.env$best_adj_r2 <- 0  #not defined if model is loaded so needs to be set here
  # }
  if (length(com.env$model.stepwise) > 3) new_adj_r2 <- summary(com.env$model.stepwise)$adj.r.squared
  if (!is.null(mod_pair)) 
    if (new_adj_r2 <= com.env$best_adj_r2) { #revert to original com.env$v.com [no longID_names in original]
      com.env$v.com <- old.v.com
      if (exists("reg_names",envir=com.env)) rm("reg_names",envir=com.env)  #try to catch improper setting of com.env$best_reg_names
    } else { #mod improved model, convert longID_names back to var_names (com.env$v.com, com.env$reg_names)                            
      clean_longID_names(mod_list)
      check_clean_longID_names()
    }
  return(new_adj_r2)
}

#given a mod_pair (scale) determine all vd's requiring it
#modify their math functions, calculate, and return as part of a mod_list(1(V1,V2),2(V1,V2),...)
#   need to make it recursive to work on raws (or if model vars can ever be used by other model vars)
get_mod_list_req_mod_pair <- function(mod_pair) {
  orig_var_name <- mod_pair[[1]]$var_name
  orig_longID_name <- mod_pair[[1]]$longID_name
  mod_var_name <- mod_pair[[2]]$var_name
  longID_name <- mod_pair[[2]]$longID_name
  #print(paste("orig_var_name",orig_var_name,"mod_var_name",mod_var_name,"longID_name",longID_name))
  scale_idx <- which(names(com.env$v.com) == mod_var_name)
  if (length(scale_idx)>1) {  #if duplicate names take current mod_var, place it in first scale_idx and delete other instance
    #print("WARNING:Two names in com.env$v.com == mod_var_name in get_mod_list_req_mod_pair")
    first_duplicate <- min(scale_idx)
    cat("Two vars with name:",mod_var_name,"Moving mod to vcom#:",first_duplicate,"deleting others\n")
    print(scale_idx)
    for (i in scale_idx) {
      if (i == first_duplicate) {
        com.env$v.com[[i]] <- mod_pair[[2]]
      } else {
        com.env$v.com <- com.env$v.com[-i]
      }
    }
    scale_idx <- first_duplicate
    check_scale_idx <- length(which(names(com.env$v.com)==mod_var_name))
    if (check_scale_idx > 1) {
      print(scale_idx)
      for (i in scale_idx) {
        print(com.env$v.com[[i]])
        print(paste("names(com.env$v.com([",i,"])",names(com.env$v.com)[i]))
      }
      for (vd in com.env$v.com) {
        if (mod_var_name %in% vd$requires) {
          print(vd$var_name)
          print(vd$requires)
          print(vd$math)
        }
      }
      print(names(com.env$v.com))
    } else {
      cat("scale_idx and vcom corrected successfully *******************************************\n")
    }
  }
  mod_list <- list(mod_pair) 
  for (vd_idx in (scale_idx + 1):length(com.env$v.com)) {
    if (any(com.env$v.com[[vd_idx]]$requires %in% orig_var_name)) {
      V1 <- com.env$v.com[[vd_idx]]
      V2 <- V1
      for (i in 1:length(V2$requires)) if (V2$requires[i] == orig_var_name) V2$requires[i] <- mod_var_name
      for (i in 1:length(V2$math)) V2$math[i] <- gsub(orig_var_name,mod_var_name,V2$math[i])
      for (i in 1:length(V2$math)) {   #should be fixed at end of calc_adj_r2
        if (grepl(orig_longID_name,V2$math[i])) {
          print("WARNING: longID_name sitting in original com.env$v.com in calc_adj_r2")
          print(paste("orig_var_name",orig_var_name,"mod_var_name",mod_var_name,"longID_name",longID_name))
          print(V2$math[i])
        }
        V2$math[i] <- gsub(orig_longID_name,mod_var_name,V2$math[i])
      }
      V2 <- set_name(V2)                        #no longID_name when calling set_name
      #set name with normal var_name fields
      #ISSUE:name issues with subsets            #adjust mod vd to use longID_name in math calcs #reverted in clean_vcom
      for (i in 1:length(V2$math)) V2$math[i] <- gsub(mod_var_name,longID_name,V2$math[i])
      V2 <- make_vars(V2)
      com.env$v.com[[V2$vcom_num]] <- V2
      names(com.env$v.com)[V2$vcom_num] <- V2$var_name
      mod_list[[length(mod_list)+1]] <- list(V1,V2)
    }
  }
  return(mod_list)
}

#function replaces longID_names from modified model variables into com.env$best_reg_names and returns them
reg_names_use_longID_name <- function(mod_list) {
  reg_names <- com.env$best_reg_names
  new_reg_names <- reg_names
  cnBAC <- colnames(var.env$BAC)  #for error checking
  for (vd_pair in mod_list) {
    V1 <- vd_pair[[1]]
    V2 <- vd_pair[[2]]
    if (V1$use == "model") {      #for all model vars use longID name in regression [check cnBAC to make sure data exists]
      new_reg_names <- new_reg_names[!(new_reg_names %in% V1$name)]
      name_found <- FALSE
      error_text <- NULL
      if ((length(V2$name)>1) & (length(V1$name)==length(V2$name))) {  #new var and old var are bin vars
        for (i in 1:length(V2$name)) {
          if (V1$name[[i]] %in% reg_names) {
            new_reg_names <- c(new_reg_names,V2$longID_name[[i]])
            if (!(V2$longID_name[[i]] %in% cnBAC)) error_text <- paste(V2$longID_name[[i]],"longID bin name found in new_reg_names, but not in colnames var.env$BAC")
            name_found <- TRUE
          }
        }
      } else {                                                        #no binning, added bin, or deleted bin
        new_reg_names <- c(new_reg_names,V2$longID_name)
        name_found <- ifelse((length(V1$name) == 2),((V1$name[[1]] %in% reg_names) | (V1$name[[2]] %in% reg_names)),(V1$name[[1]] %in% reg_names))
        V1_in_colnames <- ifelse((length(V1$name) == 2),((V1$name[[1]] %in% cnBAC) | (V1$name[[2]] %in% cnBAC)),(V1$name[[1]] %in% cnBAC))
        if (!(V2$longID_name %in% cnBAC)) error_text <- paste(V2$longID_name,"longID found in new_reg_names, but not in colnames var.env$BAC")
      }
      if (!is.null(error_text)) {  #error checking to make sure names are available for regression
        print(error_text)
        source("close_session.R")
      }
    } 
  } #end mod_list loop
  return(new_reg_names)
}

#called from clean_long_ID names (see comments below)
clean_vcom_longID2var_name <- function(mod_pair) {
  orig_var_name <- mod_pair[[1]][[1]]$var_name
  orig_longID_name <- mod_pair[[1]][[1]]$var_name
  mod_var_name <- mod_pair[[1]][[2]]$var_name
  mod_longID_name <- mod_pair[[1]][[2]]$longID_name
  #print(paste("orig_var_name",orig_var_name,"mod_var_name",mod_var_name,"longID_name",mod_longID_name))
  scale_idx <- which(names(com.env$v.com) == mod_var_name)
  #mod_list <- list(mod_pair) 
  if (length(scale_idx)>1) {
    print("WARNING:Two names in com.env$v.com == mod_var_name in clean_vcom_longID2var_name")
    print(scale_idx)
    for (i in scale_idx) {
      print(com.env$v.com[[i]])
      print(paste("names(com.env$v.com([",i,"])",names(com.env$v.com)[i]))
    }
    for (vd in com.env$v.com) {
      if (mod_var_name %in% vd$requires) {
        print(vd$var_name)
        print(vd$requires)
        print(vd$math)
      }
    }
    print(names(com.env$v.com))
  }
  if (scale_idx < length(com.env$v.com)) {
    for (vd_idx in (scale_idx + 1):length(com.env$v.com)) {
      if (any(com.env$v.com[[vd_idx]]$requires %in% mod_var_name)) {
        V2 <- com.env$v.com[[vd_idx]]
        #print(V2$requires)
        #print(V2$math)
        for (i in 1:length(V2$requires)) if (V2$requires[i] == orig_var_name) V2$requires[i] <- mod_var_name
        for (i in 1:length(V2$math)) V2$math[i] <- gsub(mod_longID_name,mod_var_name,V2$math[i])
        #print(V2$requires)
        #print(V2$math)
        V2 <- set_name(V2)                        #no longID_name when calling set_name
        #print(paste(V2$var_name,V2$ID))
        #set name with normal var_name fields
        #print(paste("In get_mod_list_req_mod_pair var_name and ID after set name",V2$var_name,V2$ID))
        com.env$v.com[[V2$vcom_num]] <- V2
        names(com.env$v.com)[V2$vcom_num] <- V2$var_name
      }
    }
  }
}

#called from clean_long_ID names (see comments below)
clean_reg_names_longID2var_name <- function() {
  for (i in 1:length(com.env$reg_names)) { #convert binned variables and ID's to their var_name
    reg_name <- com.env$reg_names[i]
    length_name <- nchar(reg_name)
    if (substr(reg_name,1,1)=="v") { #find var_name of ID
      longID_name <- reg_name
      if ((substr(reg_name,length_name,length_name) == 'h') | (substr(reg_name,length_name,length_name) == 'l')) {
        longID_name <- substr(reg_name,1,(length_name-1))
      }
      var_name <- NULL
      for (vd in com.env$v.com) {
        if (length(vd$longID_name)==1) if (longID_name == vd$longID_name[1]) var_name <- vd$var_name
        if (length(vd$longID_name)==2) if (paste0(longID_name,'l') == vd$longID_name[1]) var_name <- vd$var_name
        if (!is.null(var_name)) {
          #print(paste("in clean_reg_names_longID2var_name",longID_name,"->",var_name))
          break
        }
      }
      if (is.null(var_name)) {
        print("ERROR find var_name in v.com corresponding to longID_name in reg_vars")
        print(reg_vars)
        print(longID_name)
        print(names(com.env$v.com))
        source("close_session.R")
      } 
      #else {
      #  print(paste("converting",longID_name,"->",var_name))
      #}
      com.env$reg_names[i] <- gsub(longID_name,var_name,reg_name)
    }
  }
}

#function takes in mod_pair and cleans com.env$v.com, com.env$reg_names [not com.env$best_reg_names at this point]
#In com.env$v.com replaces longID_name -> var_name for mod_pair and every math function depending on pair
#function looks for longID_name(s) in com.env$reg_names and replaces them [leaving any binning indicators on]
#called at end of calc_adj_r2
clean_longID_names <- function(mod_pair) {
  #print("clean_vcom_longID2var_name")
  clean_vcom_longID2var_name(mod_pair)
  #print("clean_reg_names_longID2var_name")
  #print(com.env$reg_names)
  clean_reg_names_longID2var_name()
  #print(com.env$reg_names)
}  

check_clean_longID_names <- function() {
  #Error checking that longID names have been removed
  for (var_name in com.env$reg_names) {
    if (substr(var_name,1,1)=="v") {
      print("ERROR in clean_longID_names, let longID_name through in com.env$reg_names")
      print(var_name)
      print(com.env$reg_names)
      source("close_session.R")
    }
  }
  error_str <- NULL
  for (var_name in names(com.env$v.com)) if (substr(var_name,1,1)=="v") error_str <- paste(var_name,"not valid in names(com.env$v.com)")
  for (vd in com.env$v.com) {
    for (var_name in vd$requires) if (substr(var_name,1,1)=="v") error_str <- paste(var_name,"found in requires of vd",vd$vcom_num,vd$var_name)
    #from.var.env,calc_ia,calc_bin
    for (math in vd$math) {
      if (grepl('from.var.env',math) | grepl('calc_ia',math) | grepl('calc_bin',math)) {
        var_name <- get_scale_var_from_math(math)
        if (substr(var_name,1,1)=="v") error_str <- paste(var_name,"found in math of vd",vd$vcom_num,vd$var_name)
      }
    }
    if (substr(vd$var_name,1,1)=="v") error_str <- paste("var_name wrong in vd",vd$vcom_num,vd$var_name)
  }
  if (!is.null(error_str)) {
    print(error_str)
    source("close_session.R")
  }
}

#run_regression takes in var.env$reg_data.df [created in collect_data]
#removes all colinear reg variables [using vif_func]
#runs stepwise regression leaving only variables with significance > 0.001 [using model_select]
#called from get_adj_r2() [with no mod_pair]
#set com.env$adj_r2, com.env$reg_names, com.env$model.stepwise
#if oos_data == TRUE run stats on out-of-sampe data [contained in var.env$ret_data.df]
run_regression <- function(oos_data = FALSE,verbose = FALSE) {
  #print(paste("run full regression",ncol(var.env$reg_data.df),Sys.time()))
  if (!is.null(com.env$best_reg_names)) {
    for (var_name in com.env$best_reg_names) if (substr(var_name,1,1)=="v") {
      print("ERROR in run_regression com.env$best_reg_names contains long ID names")
    }
  } 
  if (ncol(var.env$reg_data.df) > 2) {
    if (is.null(com.env$best_reg_names)) {
      keep.dat <- vif_func(var.env$reg_data.df[,-1],thresh=10,trace=FALSE)
    } else {
      keep.dat <- vif_func(var.env$reg_data.df[,-1],keep=com.env$best_reg_names,thresh=10,trace=FALSE) #don't include predict.ret
    }
    remove_vars <- colnames(var.env$reg_data.df[,-1])[!(colnames(var.env$reg_data.df[,-1]) %in% keep.dat)]
    if (length(remove_vars)==0) remove_vars <- "none"
    if (any(remove_vars %in% com.env$best_reg_names)) {
      print("WARNING: in run_regression remove_vars contained in com.env$best_reg_vars")
      print(remove_vars)
      print(com.env$best_reg_names)
    }
    keep.dat <- append(colnames(var.env$reg_data.df)[1],keep.dat)
    var.env$reg_data.df <- var.env$reg_data.df[,keep.dat]
  }
  #run_regression
  form1 <- as.formula(paste(colnames(var.env$reg_data.df)[1],"~ 1"))
  null <- lm(form1,data=var.env$reg_data.df)
  form2 <- as.formula(paste(colnames(var.env$reg_data.df)[1],"~ ."))
  reg.model <- lm(form2,data=var.env$reg_data.df)
  com.env$model.stepwise <- model.select(reg.model,sig=0.001,verbose=FALSE)
  if (length(com.env$model.stepwise) == 0) {
    print("All variables left model")
    com.env$adj_r2 <- 0
  } else {
    com.env$adj_r2 <- summary(com.env$model.stepwise)$adj.r.squared
    com.env$reg_names <- names(com.env$model.stepwise$coefficients)[-1]
    if (oos_data) {
      oos_stats <- oos.r2(com.env$model.stepwise,var.env$OOS_data.df)
      print(paste("r2",oos_stats$rsq,"r20",oos_stats$rsq0,"cor",oos_stats$cor,"mse",oos_stats$mse,"mean",oos_stats$mean,"wpct",oos_stats$winpct))
      print("reversing sign on regression model")
      oos_stats <- oos.r2(com.env$model.stepwise,var.env$OOS_data.df,reverse=TRUE)
      print(paste("r2",oos_stats$rsq,"r20",oos_stats$rsq0,"cor",oos_stats$cor,"mse",oos_stats$mse,"mean",oos_stats$mean,"wpct",oos_stats$winpct))
    }
  }
  #print(paste("end regression",com.env$adj_r2,Sys.time()))
  if (verbose) print(names(com.env$model.stepwise$coefficients)[-1])
}

#run_mod_regression takes in data frame var.env$reg_data.df, a list of reg_vars, and runs regression
#called from get_adj_r2(mod_pair) [when mod_pair != NULL]
#set com.env$adj_r2, com.env$reg_names, com.env$model.stepwise
run_mod_regression <- function(reg_vars) {
  #print(paste("Run_mod_regression",Sys.time()))
  f <- as.formula(paste(colnames(var.env$reg_data.df)[1],paste(reg_vars,collapse=" + "),sep=" ~ "))
  com.env$model.stepwise <- lm(formula=f,data=var.env$reg_data.df)
  if (length(com.env$model.stepwise) == 0) {
    print("All variables left model")
    com.env$adj_r2 <- 0
  } else {
    com.env$adj_r2 <- summary(com.env$model.stepwise)$adj.r.squared
    com.env$reg_names <- names(com.env$model.stepwise$coefficients)[-1]
  }
  return(com.env$adj_r2)
}

#collect_data gathers data for regression
#takes data from each stock in var.env and places into a single data frame [var.env$reg_data.df]
#if oos_data == TRUE include out-of-sample data in var.env$reg_data.df
collect_data <- function (oos_data = FALSE) {
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
    eval(parse(text=cmd_string))
    if (oos_data) {
      subset_string <- paste("var.env$",ticker,"[com.env$OOS_date_range,allmodelvars]",sep="")
      cmd_string <- paste("var.env$OOS_data.df <- bind_rows(var.env$OOS_data.df,as.data.frame(",subset_string,"))",sep="")
      eval(parse(text=cmd_string))
    }
  }
}

#take a list of variable names and return vcom names or vcom nums
get_reg_names <- function(reg_model_vars,return_num=FALSE) { 
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

#clean_vcom.R based on com.env$best_reg_names (called from opt_model)
clean_vcom <- function(verbose=FALSE) {
  reg_names <- append(com.env$predict.ret,com.env$best_reg_names)
  keep_vcom_name <- NULL
  for (nam in reg_names) { #convert binned variables
    length_nam <- nchar(nam)
    if ((substr(nam,length_nam,length_nam) == 'h') | (substr(nam,length_nam,length_nam) == 'l')) {
      nam <- substr(nam,1,(length_nam-1))
    }
    if (substr(nam,1,1)=="v") { #find var_name of ID
      print("longID_name should not be in reg_names at this point")
      print(reg_names)
    }
    keep_vcom_name <- c(keep_vcom_name,nam)
  }
  keep_vcom_name <- unique(keep_vcom_name)
  vdlist <- NULL
  for (i in 1:length(keep_vcom_name)) {
    V1 <- com.env$v.com[[keep_vcom_name[[i]]]]
    if (is.null(V1)) print(keep_vcom_name[[i]])
    V1$requires <- get_requires_from_vd(V1)
    vdlist[[i]] <- vcom2vdlist_req(V1)
  }
  com.env$v.com <- NULL  
  for (i in 1:length(vdlist)) {
    vdlist2vcom(vdlist[[i]])
  }
}

load_model <- function(filename) { #loads model and sets com.env$predict.ret
  print(paste("function load_model",filename))
  modelfile <- paste(com.env$modeldir,"/",filename,sep="")
  load(file=modelfile,envir=com.env)
  com.env$predict.ret <- com.env$v.com[[1]]$name  #hard coded as first variable always
  com.env$best_adj_r2 <- 0
  print(paste("Loading:",modelfile,"predict:",com.env$predict.ret))
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
  com.env$model.subsets <- model.subsets
  for (i in 1:save_var_n) {
    var_names <- append(var_names,names(coef(model.subsets,i))[2])
  }
  print(var_names)
  save_vcoms <- get_reg_names(var_names,return_num=TRUE)
  print(save_vcoms)
  save_vcom_vars(save_vcoms)
}


