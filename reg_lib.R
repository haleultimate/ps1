#run_regression.R

#remove all colinear dependent variables 
run_regression <- function(oos_data = FALSE) {
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
  if (com.env$verbose) print("model.select")
  com.env$model.stepwise <- model.select(reg.model,sig=0.001,verbose=FALSE)
  if (length(com.env$model.stepwise) == 0) {
    print("All variables left model")
    adj_r2 <- 0
  } else {
    adj_r2 <- summary(com.env$model.stepwise)$adj.r.squared
    if (com.env$verbose) print(paste("In-sample adj-r2:",adj_r2))
    if (oos_data) {
      oos_stats <- oos.r2(com.env$model.stepwise,var.env$OOS_data.df)
      print(paste("r2",oos_stats$rsq,"r20",oos_stats$rsq0,"cor",oos_stats$cor,"mse",oos_stats$mse,"mean",oos_stats$mean,"wpct",oos_stats$winpct))
      print("reversing sign on regression model")
      oos_stats <- oos.r2(com.env$model.stepwise,var.env$OOS_data.df,reverse=TRUE)
      print(paste("r2",oos_stats$rsq,"r20",oos_stats$rsq0,"cor",oos_stats$cor,"mse",oos_stats$mse,"mean",oos_stats$mean,"wpct",oos_stats$winpct))
    }
  }
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
    print("allmodelvars")
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

get_reg_names <- function(reg_model_vars) {
  if (com.env$verbose) print(reg_model_vars)
  reg_names <- NULL
  for (v in 1:length(com.env$v.com)) {  #create list of model variables as candidates to modify
    vd <- com.env$v.com[[v]]
    if (length(vd$name) == 1) {
      if ( (vd$name %in% reg_model_vars) ) {
        reg_names <- append(reg_names,vd$name)
      }
    } else {
      if ((vd$name[1] %in% reg_model_vars) | (vd$name[2] %in% reg_model_vars)) {
        reg_names <- append(reg_names,substr(vd$name[1],1,(nchar(vd$name[1])-1)))
      }
    }
  }  #end for v loop
  #return (rnd_mod(reg_names=reg_names))    #select vd from reg_names and modify it
  return(reg_names)
}


#clean_vcom.R
clean_vcom <- function() {
  keep_vcom_name <- names(com.env$model.stepwise$coefficients)[-1]
  keep_vcom_name <- append(colnames(var.env$reg_data.df)[1],keep_vcom_name)
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
  if (com.env$verbose) print(paste("after vcom cleaning vars=",length(com.env$v.com)))
}

eval_adj_r2 <- function(vd=NULL,old_adj_r2=0,oos_data=FALSE) {
  vd <- make_vars(vd)                   #make modified variable & update columns in vd (for all stock in var.env)
  if (!is.null(vd)) { #insert modified vd into com.env$v.com
    old.v.com <- com.env$v.com
    #print(vd)
    vd_tmp <- com.env$v.com[[vd$vcom_num]]
    com.env$v.com[[vd$vcom_num]] <- vd                                  #replace modified vd in v.com
    if (length(vd_tmp$name) > length(vd$name)) {                        #if deleted bin change vd$name in names(v.com)
      names(com.env$v.com)[vd$vcom_num] <- vd$name
      print(paste("changing bin name",vd_tmp$name,vd$name,vd$vcom_num))
    }
  }
  if (com.env$verbose) print("collect_data")
  collect_data(oos_data)
  if (com.env$verbose) print("run_regression")
  run_regression(oos_data)
  new_adj_r2 <- 0
  if (length(com.env$model.stepwise) > 3) new_adj_r2 <- summary(com.env$model.stepwise)$adj.r.squared
  if (!is.null(vd) & new_adj_r2 <= old_adj_r2) { #revert to original com.env$v.com  #do we need to clean up var.env?
    com.env$v.com <- old.v.com
  }
  return(new_adj_r2)
}