source("stock_list.R")

rm.list <- ls(all=TRUE)
keep.list <- c("stx_list.old","data.env","com.env")
isNameinKeep <- rm.list %in% keep.list
rm.list <- c(rm.list[!isNameinKeep],"keep.list","isNameinKeep","rm.list")
rm(list = rm.list)  #clear environment except for loaded stock data and some com.env variables

#setup output to go to logfile
com.env$original_wd <- getwd()
com.env$logdir <- paste(com.env$original_wd,"/logs",sep="")
com.env$vardir <- paste(com.env$original_wd,"/vars",sep="")
com.env$modeldir <- paste(com.env$original_wd,"/models",sep="")
#setwd(logdir)
com.env$logfile <- paste(com.env$logdir,"/lf",gsub("[^0-9]","",Sys.time()),".txt",sep="")
#sink(file=com.env$logfile,type="output",split=TRUE)
#on.exit(sink())
print(paste("Start time:",Sys.time()))
#setwd(original_wd)

library(lpSolveAPI)
library(quantmod)
library(dplyr)
library(forecast)
library(leaps)
if (!exists("data.env")) data.env <- new.env(parent=globalenv())
var.env <- new.env(parent=globalenv())
rnd.env <- new.env(parent=globalenv())

#Init data_load vars
Sys.setenv(TZ = "UTC")
adjustment <- TRUE
start_date <- "2004-01-01" 
end_date <- "2013-03-31"
if (!exists("stx_list.old")) {         #only load if stx_list has changed
  getSymbols(Symbols = com.env$stx_list,
             env=data.env,
             src = "yahoo", 
             index.class = "POSIXct",
             from = start_date, 
             to = end_date, 
             adjust = adjustment)
  stx_list.old <- com.env$stx_list
} else if (!identical(com.env$stx_list,stx_list.old)) {
  isNameinStxold <- com.env$stx_list %in% stx_list.old
  stx_list.new <- com.env$stx_list[!isNameinStxold]
  getSymbols(Symbols = stx_list.new, 
             env=data.env,
             src = "yahoo", 
             index.class = "POSIXct",
             from = start_date, 
             to = end_date, 
             adjust = adjustment)
  rm(stx_list.new,isNameinStxold)
  stx_list.old <- com.env$stx_list
}

com.env$load_model <- TRUE
com.env$save_model <- FALSE
com.env$model_filename <- "lf5_feb28.vcom"
com.env$look_forward <- 5
com.env$save_var_n <- 0
com.env$opt_model <- FALSE
com.env$model_loops <- 2
com.env$add_vars <- 3
com.env$mod_var_loops <- 20
com.env$run_sim <- TRUE

com.env$load_multi_model <- TRUE
com.env$model_list <- c("lf1_feb28.vcom","lf5_feb28.vcom")
if (com.env$load_multi_model) {  #override com parameters for multi_model
  bad_model_list <- is.null(com.env$model_list)
  if (!bad_model_list) bad_model_list <- (length(com.env$model_list)<2)
  if (bad_model_list) {
    print("Must have at least two models to load to run multi_model")
    stop()
  }
  rm(bad_model_list)                    #only below settings are supported in multi-model
  com.env$load_model <- TRUE
  com.env$save_model <- FALSE
  com.env$model_filename <- com.env$model_list[1]
  com.env$opt_model <- FALSE
  com.env$run_sim <- TRUE
  com.env$save_var_n <- 0
}

com.env$days2remove <- 60
com.env$reg_start_date <- as.POSIXct("2004-07-01 UTC")
com.env$reg_end_date <- as.POSIXct("2011-12-30 UTC")
com.env$OOS_start_date <- "20120101"
com.env$OOS_end_date <- "20121231"
com.env$reg_date_range <- paste(com.env$reg_start_date,"/",com.env$reg_end_date,sep="")
com.env$OOS_date_range <- paste(com.env$OOS_start_date,"/",com.env$OOS_end_date,sep="")
com.env$sim_date_index <- index(data.env$XLF[com.env$OOS_date_range])

com.env$verbose <- FALSE
com.env$corr.threshold <- 0.6
com.env$var_files_tried <- NULL
#v.com <- NULL
#load(file="cores50.Rdata")
#if (verbose & exists("store.data")) print(store.data[length(store.data)])

#remove problematic stx
static.stx.symbols <- com.env$stx.symbols
for (i in 1:com.env$stx) {
  ticker <- static.stx.symbols[i]
  if (make.names(ticker) != ticker) {
    if (com.env$verbose) print(paste("remove",ticker,"from list, not valid name",make.names(ticker)))
    com.env$stx.symbols <- com.env$stx.symbols[-which(com.env$stx.symbols == ticker)] #remove from stx list
    com.env$stx_list <- com.env$stx_list[-which(com.env$stx_list == ticker)]
  }
  cmn <- com.env$cmn_lookup[ticker]
  cmd_string <- paste("corr.data <- cbind(data.env$",cmn,"[,'",cmn,".Adjusted'],data.env$",ticker,"[,'",ticker,".Adjusted'])",sep="")
  #if (verbose) print(cmd_string)
  eval(parse(text=cmd_string))
  corr.val <- cor(corr.data[com.env$reg_date_range],use="complete.obs")[1,2]
  cmd_string <- paste("not_enough_history <- nrow(data.env$",ticker,") < 320",sep="")
  eval(parse(text=cmd_string))
  if ( (not_enough_history) | (corr.val < com.env$corr.threshold) ) {
    #print(paste("remove",ticker,"from stx list, not correlated with cmn",corr.val))
    com.env$stx.symbols <- com.env$stx.symbols[-which(com.env$stx.symbols == ticker)] #remove from stx list
    com.env$stx_list <- com.env$stx_list[-which(com.env$stx_list == ticker)]
  } #else print(corr.val)
}
rm(static.stx.symbols,corr.val,corr.data)

com.env$stx <- length(com.env$stx.symbols)
com.env$port_size <- com.env$init_equity <- 10000*com.env$stx


