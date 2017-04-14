#run_regression.R

#run_regression takes in var.env$reg_data.df [created in collect_data]
#removes all colinear reg variables [using vif_func]
#runs stepwise regression leaving only variables with significance > 0.001 [using model_select]
#called from get_adj_r2() [with no mod_pair]
#set com.env$adj_r2, com.env$reg_names, com.env$model.stepwise
run_regression <- function(oos_data = FALSE,verbose = FALSE) {
  print(paste("run full regression",ncol(var.env$reg_data.df),Sys.time()))
  if (!is.null(com.env$best_reg_names)) {
    #cat('best reg names:',length(com.env$best_reg_names),com.env$best_reg_names,'\n')
    for (var_name in com.env$best_reg_names) if (substr(var_name,1,1)=="v") {
      print("ERROR in run_regression com.env$best_reg_names contains long ID names")
    }
  } else {
    #cat('best reg names: none\n')
  }
  #cat('var list:',colnames(var.env$reg_data.df[,-1]),'\n')
  if (ncol(var.env$reg_data.df) > 2) {
    if (is.null(com.env$best_reg_names)) {
      keep.dat <- vif_func(var.env$reg_data.df[,-1],thresh=10,trace=FALSE)
    } else {
      keep.dat <- vif_func(var.env$reg_data.df[,-1],keep=com.env$best_reg_names,thresh=10,trace=FALSE) #don't include predict.ret
    }
    #cat('vars to try after removing collinear vars:',keep.dat,'\n')
    remove_vars <- colnames(var.env$reg_data.df[,-1])[!(colnames(var.env$reg_data.df[,-1]) %in% keep.dat)]
    if (length(remove_vars)==0) remove_vars <- "none"
    #cat('colinear vars removed:',remove_vars,'\n')
    if (any(remove_vars %in% com.env$best_reg_names)) {
      print("ERROR in run_regression remove_vars contained in com.env$best_reg_vars")
      print(remove_vars)
      print(com.env$best_reg_names)
    }
    keep.dat <- append(colnames(var.env$reg_data.df)[1],keep.dat)
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
    com.env$adj_r2 <- 0
  } else {
    com.env$adj_r2 <- summary(com.env$model.stepwise)$adj.r.squared
    com.env$reg_names <- names(com.env$model.stepwise$coefficients)[-1]
    #print(paste("In-sample adj-r2:",adj_r2))
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
  #print(reg_vars)
  f <- as.formula(paste(colnames(var.env$reg_data.df)[1],paste(reg_vars,collapse=" + "),sep=" ~ "))
  #print(f)
  com.env$model.stepwise <- lm(formula=f,data=var.env$reg_data.df)
  if (length(com.env$model.stepwise) == 0) {
    print("All variables left model")
    com.env$adj_r2 <- 0
  } else {
    com.env$adj_r2 <- summary(com.env$model.stepwise)$adj.r.squared
    com.env$reg_names <- names(com.env$model.stepwise$coefficients)[-1]
  }
  #print(summary(com.env$model.stepwise)$adj.r.squared)
  #print(Sys.time())
  return(com.env$adj_r2)
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

get_var_name_from_longID_name <- function(longID_name) {
  nam <- substr(longID_name,2,nchar(longID_name)) #strip off "v"
  for (vd in com.env$v.com) {
    if (vd$ID == nam) {
      return(vd$var_name)
    }
  }
  return("none")  
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

#clean_vcom.R based on com.env$best_reg_names (called from opt_model)
clean_vcom <- function(verbose=FALSE) {
  #print(paste("clean_vcom",Sys.time()))
  #reg_names <- names(com.env$model.stepwise$coefficients)[-1]  #get names from regression (not var_names)
  #print("names from regression")
  reg_names <- append(com.env$predict.ret,com.env$best_reg_names)
  keep_vcom_name <- NULL
  #print(paste("reg_names from coefs + predictor:",length(reg_names)))
  for (nam in reg_names) { #convert binned variables
    length_nam <- nchar(nam)
    if ((substr(nam,length_nam,length_nam) == 'h') | (substr(nam,length_nam,length_nam) == 'l')) {
      nam <- substr(nam,1,(length_nam-1))
    }
    # moved to end of calc_adj_r2
    if (substr(nam,1,1)=="v") { #find var_name of ID
      print("longID_name should not be in reg_names at this point")
      print(reg_names)
    }
    keep_vcom_name <- c(keep_vcom_name,nam)
  }
  
  #print("keep_vcom_names from regression")
  #print(keep_vcom_name)
  keep_vcom_name <- unique(keep_vcom_name)
  #print(paste("#reg vars:",length(keep_vcom_name)))
  #print(keep_vcom_name)
  
  vdlist <- NULL
  for (i in 1:length(keep_vcom_name)) {
    #print(keep_vcom_name[[i]])
    V1 <- com.env$v.com[[keep_vcom_name[[i]]]]
    if (is.null(V1)) print(keep_vcom_name[[i]])
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
  if (length(scale_idx)>1) {
    print("WARNING:Two names in com.env$v.com == mod_var_name in get_mod_list_req_mod_pair")
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
  mod_list <- list(mod_pair) 
  for (vd_idx in (scale_idx + 1):length(com.env$v.com)) {
    if (any(com.env$v.com[[vd_idx]]$requires %in% orig_var_name)) {
      V1 <- com.env$v.com[[vd_idx]]
      #print(V1)
      V2 <- V1
      #print(V2$requires)
      #print(V2$math)
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
      #print(V2$requires)
      #print(V2$math)
      V2 <- set_name(V2)                        #no longID_name when calling set_name
      #print(paste(V2$var_name,V2$ID))
      #set name with normal var_name fields
      #print(paste("In get_mod_list_req_mod_pair var_name and ID after set name",V2$var_name,V2$ID))
      #ISSUE:name issues with subsets            #adjust mod vd to use longID_name in math calcs #reverted in clean_vcom
      for (i in 1:length(V2$math)) V2$math[i] <- gsub(mod_var_name,longID_name,V2$math[i])
      V2 <- make_vars(V2)
      #print(V2$math)
      #print(paste("New model var name:",V2$var_name,V2$ID,V2$vcom_num))
      com.env$v.com[[V2$vcom_num]] <- V2
      names(com.env$v.com)[V2$vcom_num] <- V2$var_name
      mod_list[[length(mod_list)+1]] <- list(V1,V2)
    }
  }
  return(mod_list)
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
    } else if (V2$use == "scale") {  #must get all model vars requiring scale var (fix com.env$v.com in clean_vcom)
      #print(paste(mod_pair[[1]]$var_name,mod_pair[[2]]$var_name,mod_pair[[2]]$longID_name))
      #for (vd in com.env$v.com) if (any(vd$requires %in% mod_pair[[1]]$var_name)) {
      #print(paste(vd$var_name))
      #print(vd$requires)
      #print(vd$math)
      #}
      mod_list <- get_mod_list_req_mod_pair(mod_pair)
      for (mod_pair in mod_list) {  #check if any model variables have been tried before (if so return 0)
        if (mod_pair[[2]]$use == "model") {
          if (mod_pair[[2]]$ID %in% com.env$ID_tried) {
            print(paste("Warning:In eval_adj_r2",mod_pair[[2]]$var_name,"already tried, return adj_r2 = 0, revert com.env$v.com"))
            rm("reg_names",envir=com.env)
            com.env$v.com <- old.v.com
            return(0)
          } else {
            com.env$ID_tried <- c(com.env$ID_tried,mod_pair[[2]]$ID)
          }
        }
      }
      #print(str(mod_list))
      #for (i in 1:length(mod_list)) {
      #  print(paste(mod_list[[i]][[1]]$var_name,mod_list[[i]][[1]]$ID,"->",mod_list[[i]][[2]]$var_name,mod_list[[i]][[2]]$ID))
      #}
      #print(mod_list[[2]][[2]]$math)
      #source("close_session.R")
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
    reg_names <- com.env$best_reg_names
    #print(paste("reg_names",length(reg_names)))
    #print(reg_names)
    new_reg_names <- reg_names
    cnBAC <- colnames(var.env$BAC)  #for error checking
    for (vd_pair in mod_list) {
      V1 <- vd_pair[[1]]
      V2 <- vd_pair[[2]]
      #print(V1$name)
      #print(V2$longID_name)
      #print(new_reg_names %in% V1$name)
      #print(com.env$opt)
      #print(new_reg_names %in% V1$longID_name)
      if (com.env$opt) {
        print("com.env$opt should never be true, reg_names should be cleaned of longID_name(s) after each run")
        source("close_session.R")
      }
      if (V1$use == "model") {
        new_reg_names <- new_reg_names[!(new_reg_names %in% V1$name)]
        name_found <- FALSE
        error_text <- NULL
        if ((length(V2$name)>1) & (length(V1$name)==length(V2$name))) {  #new var and old var are bin vars
          for (i in 1:length(V2$name)) {
            if (V1$name[[i]] %in% reg_names) {
              new_reg_names <- c(new_reg_names,V2$longID_name[[i]])
              #if (!(V1$name[[i]] %in% cnBAC)) error_text <- paste(V1$name[[i]]," bin name found in reg_names, but not in colnames var.env$BAC")
              if (!(V2$longID_name[[i]] %in% cnBAC)) error_text <- paste(V2$longID_name[[i]],"longID bin name found in new_reg_names, but not in colnames var.env$BAC")
              name_found <- TRUE
            }
          }
        } else {                                                        #no binning, added bin, or deleted bin
          new_reg_names <- c(new_reg_names,V2$longID_name)
          name_found <- ifelse((length(V1$name) == 2),((V1$name[[1]] %in% reg_names) | (V1$name[[2]] %in% reg_names)),(V1$name[[1]] %in% reg_names))
          V1_in_colnames <- ifelse((length(V1$name) == 2),((V1$name[[1]] %in% cnBAC) | (V1$name[[2]] %in% cnBAC)),(V1$name[[1]] %in% cnBAC))
          #if (!(V1_in_colnames)) error_text <- paste(V1$name,"name found in reg_names, but not in colnames var.env$BAC")
          if (!(V2$longID_name %in% cnBAC)) error_text <- paste(V2$longID_name,"longID found in new_reg_names, but not in colnames var.env$BAC")
        }
        #if (!name_found) error_text <- paste(V1$name,"not found in reg_names",length(V1$name),length(V2$name),reg_names)
        if (!is.null(error_text)) {  #error checking to make sure names are available for regression
          print(error_text)
          source("close_session.R")
        }
      } else { #var is scale var (FUTURE: raw vars)
        #print(paste("scale var mod_pair",V2$use,V2$var_name))
      }
    } #end mod_list loop
    #print(paste("new_reg_names:",length(new_reg_names),"reg_names:",length(reg_names)))
    #print(new_reg_names)
    run_mod_regression(new_reg_names)
  }
  new_adj_r2 <- 0
  if (length(com.env$model.stepwise) > 3) new_adj_r2 <- summary(com.env$model.stepwise)$adj.r.squared
  if (!is.null(mod_pair) & (new_adj_r2 <= com.env$best_adj_r2)) { #revert to original com.env$v.com  #do we need to clean up var.env?
    #print(paste("Not better in eval_adj_r2 mod loop, old_adj_r2=",old_adj_r2,"new_adj_r2=",new_adj_r2))
    com.env$v.com <- old.v.com
    rm("reg_names",envir=com.env)  #try to catch improper setting of com.env$best_reg_names
  } else if (!is.null(mod_pair)) {
    #clean com.env$v.com by replacing all longID_name with var_name
    #also clean com.env$reg_names by replacing all longID_name with var_name
    clean_longID_names(mod_list)
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
    #reg_names <- names(com.env$model.stepwise$coefficients)[-1]  #update reg_names with new mod
    #print(paste("Better in eval_adj_r2 mod loop, old_adj_r2=",old_adj_r2,"new_adj_r2=",new_adj_r2))
  }
  return(new_adj_r2)
  #print(paste("Done with regression,",Sys.time()))
}

#determine number of new vars to add to regression
#always provide at least the first level (in total), after the last level only add 1 at a time
#at intermediate levels add one more than the next level
get_add_vars <- function(add_var_levels,n.reg_vars) {
   avl_idx <- 1
   max_avl_idx <- length(add_var_levels)
   while(avl_idx <= max_avl_idx) {
     if (n.reg_vars < add_var_levels[avl_idx]) {
       add_vars <- max_avl_idx - avl_idx + 2
       if (avl_idx == 1) add_vars <- max(add_vars,(add_var_levels[1]-n.reg_vars))
       return(add_vars)
     }
     avl_idx <- avl_idx + 1
   }
   add_vars <- 1
   return(add_vars)
}

opt_model <- function(model_loops,add_var_levels,mod_var_loops) {    
  #source("define_vars.R")                         #define predictor var, setup initial v.com
  print(paste("In opt_model, model_loops:",model_loops,"mod_var_loops:",mod_var_loops))
  com.env$best_adj_r2 <- 0
  com.env$best_reg_names <- NULL
  com.env$ID_tried <- NULL
  model_worse <- FALSE
  test_clean_vcom <- FALSE
  test_worse_vcom <- FALSE
  test_reverted_vcom <- FALSE
  test_mod_vcom <- FALSE
  for (l in 1:model_loops) {              #start model loop
    print(paste("model loop:",l,"/",model_loops,Sys.time()))
    com.env$opt <- FALSE
    if (!model_worse) {
      if (com.env$best_adj_r2 == 0) {  #first loop
        add_vars <- add_var_levels[1]
      } else {                         #
        add_vars <- get_add_vars(add_var_levels,length(com.env$best_reg_names))
      }
      #print(paste("ADD VARS:",add_vars))
      for (i in 1:add_vars) make_new_model_var()    #add new vars to v.com
    }
    #print("eval_adj_r2 add var loop")
    #print(names(com.env$v.com))
    # if (!check_dependencies()) {
    #   print("Dependency problem when adding new vars, removing var env and reverting to old v.com")
    #   print(names(com.env$v.com))
    #   source("close_session.R")
    #   com.env$v.com <- com.env$best_vcom
    #   rm(var.env,envir=globalenv())
    #   var.env <<- new.env(parent = globalenv())
    #   next
    # }
    
    orig_adj_r2 <- eval_adj_r2()
    if (orig_adj_r2 < com.env$best_adj_r2)  {  #model worse
      print(paste("Adj_r2 got worse when adding vars",orig_adj_r2,com.env$best_adj_r2))
      print(names(com.env$model.stepwise$coefficients)[-1])
      print("Reverting to previous com.env$v.com********************************************")
      print(com.env$best_reg_names)
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
        print(names(com.env$model.stepwise$coefficients)[-1])
        if (orig_adj_r2 != com.env$best_adj_r2) {
          print("***********PROBLEM:changing best_adj_r2**********************************")
          com.env$best_adj_r2 <- orig_adj_r2
          print(com.env$best_reg_names)
          #com.env$reg_names <- names(com.env$model.stepwise$coefficients)[-1]
          com.env$best_reg_names <- com.env$reg_names
        }
        print("Removing var environment, reverted vcom and tested")
        rm(var.env,envir=globalenv())
        var.env <<- new.env(parent = globalenv())
      }
      next
    }
    
    com.env$best_adj_r2 <- orig_adj_r2
    #com.env$reg_names <- names(com.env$model.stepwise$coefficients)[-1]
    #print("best_reg_names set in add_var")
    com.env$best_reg_names <- com.env$reg_names
    com.env$reg_vcom_names <- get_reg_names(com.env$best_reg_names)
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
    check_reg_vars <- com.env$best_reg_names
    model_worse <- TRUE
    #com.env$ID_tried <- sort(com.env$ID_tried)
    #print(com.env$ID_tried)
    if (l < model_loops) { #don't mod variables on last loop 
      while ((model_worse) & (loops < mod_var_loops)) {    
        loops <- loops + 1
        mod_pair <- mod_var("model")
        if (is.null(mod_pair)) next()  #mod_var already tried, loop again
        #if (mod_pair[[2]]$use == "scale") print(paste("trying to mod scale var",mod_pair[[2]]$var_name))
        #print("mod_var model")
        new_adj_r2 <- eval_adj_r2(mod_pair=mod_pair)
        if (mod_pair[[2]]$use == "scale") com.env$ID_scale_opt <- mod_pair[[2]]$ID #don't repeat same scale var in optimization
        if (new_adj_r2 > com.env$best_adj_r2) {
          model_worse <- FALSE
          print(paste("model improved",new_adj_r2,orig_adj_r2,"loop#",loops,"/",com.env$mod_var_loops,Sys.time()))
          #print(mod_pair[[1]]$math)
          #print(mod_pair[[2]]$math)
          com.env$best_adj_r2 <- new_adj_r2
          #cat("best_reg_names set when first mod_var improves model\n")
          com.env$best_reg_names <- com.env$reg_names
          #print("OPT VAR")
          com.env$opt <- FALSE  #never set, reg_names should be clean of longID_names at this point
          optimize_mod_pair(mod_pair)  
          #if calc_name returned need to clean com.env$v.com [longID_name <- calc_name]
        } else {
          #print(paste("model_worse",orig_adj_r2,new_adj_r2,"loop#",loops,"/",com.env$mod_var_loops,Sys.time()))
        }
      } #end mod while loop
      #print("Removing var environment, mod var loop")
      rm(var.env,envir=globalenv())
      var.env <<- new.env(parent = globalenv())
      #print(names(com.env$v.com))  
      if (com.env$best_adj_r2 < check_adj_r2) {
        print(paste("Adj_r2 got worse when modding vars, before:",check_adj_r2,"after mod:",com.env$best_adj_r2))
        print("Reverting to previous com.env$v.com*************************************************")
        com.env$v.com <- com.env$best_vcom  #from add vars
        com.env$best_adj_r2 <- check_adj_r2
        com.env$best_reg_names <- check_reg_names
        next
      } else if (length(com.env$v.com) != length(unique(names(com.env$v.com)))) { #check for duplicate var name
        print(paste("Found better adj_r2",com.env$best_adj_r2,"but requires duplicate var name, reverting v.com"))
        print(names(com.env$v.com))
        com.env$v.com <- com.env$best_vcom  #from add vars
        com.env$best_adj_r2 <- check_adj_r2
        com.env$best_reg_names <- check_reg_names
        print(paste("Reverting to adj_r2",check_adj_r2))
        print(names(com.env$v.com))
      } else {
        clean_vcom()
        com.env$best_vcom <- com.env$v.com            #not set on mod or opt var, last set at end of add_vars
        #cat('best_reg_names set at end of mod vars') 
        #com.env$best_reg_names <- com.env$reg_names  #already set
        if (test_mod_vcom) {
          test_adj_r2 <- eval_adj_r2(verbose=FALSE)
          rm(var.env,envir=globalenv())
          var.env <<- new.env(parent = globalenv())
          print(paste("Testing vcom after mod, best_adj_r2=",com.env$best_adj_r2,"test_adj_r2=",test_adj_r2))
          if (test_adj_r2 != com.env$best_adj_r2) {
            print("*************************************************old reg vars:")
            print(com.env$best_reg_names)
            print("*************************************************new reg vars:")
            print(names(com.env$model.stepwise$coefficients)[-1])
            print(names(com.env$v.com))
          }
        }
        #check_ids(com.env$ID_tried)
      }
    } #end if last loop
  }                                               #end model loop
  cat("Final adj R2:",com.env$best_adj_r2,'\n')
  cat("Reg vars:",length(com.env$best_reg_names),com.env$best_reg_names,'\n')
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

