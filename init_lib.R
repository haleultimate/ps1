#init_lib.R
#parms that should be changed by user manually to control run_ps.R behavior
set_control_parms <- function() {
  com.env$model_loops <- 20
  
  com.env$add_var_levels <- c(10,15,20,30)
  com.env$opt_model <- TRUE
  com.env$load_vars <- FALSE
  com.env$load_model <- FALSE
  com.env$save_model <- FALSE
  com.env$save_var_n <- 0
  com.env$look_forward <- 5
  com.env$model_filename <- "lf5_soos616.vcom"
  com.env$mod_var_loops <- 20
  com.env$opt_type <- "single_oos"  #{adjr2_is,single_oos,rolling_oos}
  com.env$run_sim <- TRUE
  
  com.env$load_multi_model <- FALSE
  com.env$model_list <- c("lf1_mar3.vcom","lf2_mar3.vcom","lf3_mar3.vcom","lf5_mar3.vcom",
                          "lf8_mar3.vcom","lf13_mar3.vcom","lf21_mar3.vcom","lf34_mar3.vcom")
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
  if (((com.env$save_var_n >0) & !com.env$opt_model) |     #check logical load/save/opt model options
      (com.env$save_model & !com.env$opt_model) ) {
    cat("initial load/save/opt model options don't make sense\n")  
    cat("com.env$load_model",com.env$load_model,"com.env$opt_model",com.env$opt_model,"\n")
    cat("com.env$save_model",com.env$save_model,"com.env$save_var_n",com.env$save_var_n,"\n")
    source("close_session.R")
  }
  if (com.env$opt_model & com.env$load_vars) {
    com.env$saved_var_files <- list.files(path=com.env$vardir)
    print(paste("Available saved_var_files:",length(com.env$saved_var_files)))
    #print(com.env$saved_var_files)
  }
  com.env$verbose <- FALSE
  com.env$vlty_window <- 250
  #com.env$var_names <- NULL
}

  
#main progarm for init_lib
init_session <- function(stx_list.loaded) {
  load_custom_libraries()
  load_packages()
  set_up_environments()
  set_directories()    
  set_control_parms()                  #at top of this file, manually adjusted  
  #if (com.env$log_file) set_log_file()      #not working 
  stock_list()                         #setup stock symbols and com.env$cmn_lookup
  stx_list.loaded <- load_stock_history(stx_list.loaded)     #only needed after first run if stock list changes
  set_opt_type_settings()
  remove_problem_stocks()
  return(stx_list.loaded)
}

load_custom_libraries <- function() {
  source("rnd_lib.R")            
  source("make_lib.R")
  source("calc_lib.R")              
  source("model_select.R") 
  source("reg_lib.R")
  source("port_opt.R")            #function libraries
}

load_packages <- function() {
  library(lpSolveAPI)
  library(quantmod)
  library(dplyr)
  library(forecast)
  library(leaps)
  library(fmsb)
}

set_up_environments <- function() {
  if (!exists("data.env")) data.env <<- new.env(parent=globalenv())
  var.env <<- new.env(parent=globalenv())
  rnd.env <<- new.env(parent=globalenv())
  com.env <<- new.env(parent=globalenv())
}

set_directories <- function() {
  com.env$original_wd <- getwd()
  com.env$logdir <- paste(com.env$original_wd,"/logs",sep="")
  com.env$vardir <- paste(com.env$original_wd,"/vars",sep="")
  com.env$modeldir <- paste(com.env$original_wd,"/models",sep="")
  com.env$logfile <- paste(com.env$logdir,"/lf",gsub("[^0-9]","",Sys.time()),".txt",sep="")
}

set_opt_type_settings <- function() {  
  com.env$reg_start_date <- as.POSIXct("2004-07-01 UTC")
  com.env$reg_end_date <- as.POSIXct("2011-12-30 UTC")
  com.env$reg_date_range <- paste(com.env$reg_start_date,com.env$reg_end_date,sep="/")
  
  switch(com.env$opt_type,
         "adjr2_is" = {
           com.env$sig <- 0.001
         },
         "single_oos" = {
           com.env$sig <- 0.01
           com.env$reg_end_date <- as.POSIXct("2009-12-30 UTC")
           com.env$reg_date_range <- paste(com.env$reg_start_date,com.env$reg_end_date,sep="/")
           com.env$oos_start_date <- as.POSIXct("2010-01-01 UTC")
           com.env$oos_end_date <- as.POSIXct("2011-12-30 UTC")
           com.env$oos_date_range <- paste(com.env$oos_start_date,com.env$oos_end_date,sep="/")
           cmd_string <- paste0("com.env$oos_date_index <- index(data.env$",com.env$stx_list[1],"[com.env$oos_date_range])")   #hard coded to first stock
           eval(parse(text=cmd_string))
         },
         "rolling_oos" = {
           com.env$r2_wt <- 200
           com.env$rolling_best_score <- -99999999.  #-Inf
           com.env$sig <- 0.05
           com.env$rolling_start_date <- as.Date("2008-01-01 UTC")
           com.env$period <- 365
           oos_days <- as.Date(com.env$reg_end_date) - com.env$rolling_start_date
           com.env$rolling_periods <- as.numeric(round(oos_days/com.env$period))
           end_date <- as.Date(com.env$rolling_start_date - 1) 
           for (i in 1:com.env$rolling_periods) {
             start_date <- as.Date(end_date + 1)
             if (i < com.env$rolling_periods) {
               end_date <- as.Date(start_date+com.env$period)
             } else {
               end_date <- com.env$reg_end_date
             }
             #end_date <- ifelse(i == com.env$rolling_periods,as.Date(com.env$reg_end_date),as.Date(start_date + com.env$period))
             #print(paste(i,"start_date:",start_date,"end_date",end_date))
             oos_date_range <- paste(as.POSIXct(paste(start_date,"UTC")),as.POSIXct(paste(end_date,"UTC")),sep="/")
             #print(oos_date_range)
             cmd_string <- paste0("date_index <- index(data.env$",com.env$stx_list[1],"[oos_date_range])")
             #print(cmd_string)
             eval(parse(text=cmd_string))
             #print(date_index)
             com.env$oos_start_date[[i]] <- paste(start_date,"UTC")
             com.env$oos_date_index[[i]] <- date_index 
           }
           for(i in 1:com.env$rolling_periods) print(com.env$oos_start_date[[i]])
         },
         "rolling_sim" = {},
         {cat("Error: com.env$opt_type - ",com.env$opt_type," not supported\n")
          source("close_session.R")}
  )
  #if (com.env$run_sim) {
    com.env$sim_start_date <- as.POSIXct("2012-01-01 UTC")
    com.env$sim_end_date <- as.POSIXct("2012-12-31 UTC")
    com.env$sim_date_range <- paste(com.env$sim_start_date,com.env$sim_end_date,sep="/")
    #print(com.env$sim_date_range)
    cmd_string <- paste0("com.env$sim_date_index <- index(data.env$",com.env$stx_list[1],"[com.env$sim_date_range])")   #hard coded to first stock
    #print(cmd_string)
    eval(parse(text=cmd_string))
    #print(com.env$sim_date_index)
  #}
}

remove_problem_stocks <- function() {
  static.stx.symbols <- com.env$stx.symbols
  com.env$corr.threshold <- 0.6
  com.env$days2remove <- 60
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
    cmd_string <- paste("enough_history <- nrow(data.env$",ticker,"[com.env$reg_date_range]) > 320",sep="")
    eval(parse(text=cmd_string))
    if (enough_history) corr.val <- cor(corr.data[com.env$reg_date_range],use="complete.obs")[1,2]
    if ( (!enough_history) | (corr.val < com.env$corr.threshold) ) {
      #print(paste("remove",ticker,"from stx list, not correlated with cmn",corr.val))
      com.env$stx.symbols <- com.env$stx.symbols[-which(com.env$stx.symbols == ticker)] #stx.symbols - only stocks
      com.env$stx_list <- com.env$stx_list[-which(com.env$stx_list == ticker)]          #stx_list - contains etfs
    } #else print(corr.val)
  }
  rm(static.stx.symbols,corr.val,corr.data)
  com.env$stx <- length(com.env$stx.symbols)
  
  com.env$start_date <- NULL
  #com.env$start_date <- setNames(vector("list", length(com.env$stx.symbols)), com.env$stx.symbols)
  for (i in 1:com.env$stx) {
    ticker <- com.env$stx.symbols[i]
    cmn_ticker <- com.env$cmn_lookup[ticker]
    cmd_string <- paste("cmn_start_date <- index(data.env$",cmn_ticker,"[",com.env$days2remove,",])",sep="")
    eval(parse(text=cmd_string))
    cmd_string <- paste("stk_start_date <- index(data.env$",ticker,"[",com.env$days2remove,",])",sep="")
    #print(cmd_string)
    eval(parse(text=cmd_string))
    if (is.null(com.env$start_date)) {
      com.env$start_date <- max(c(cmn_start_date,stk_start_date))
    } else {
      com.env$start_date <- c(com.env$start_date,max(c(cmn_start_date,stk_start_date)))
    }
  }
  names(com.env$start_date) <- com.env$stx.symbols
  #print(com.env$start_date)
  com.env$port_size_mult <- 10000
  com.env$port_size <- com.env$init_equity <- com.env$port_size_mult*com.env$stx
  com.env$alpha_wt <- 16000
  com.env$retvlty_not_calced <- TRUE
  #print(com.env$stx_list)
  calc_adjusted_HLOJRlD(com.env$stx_list)
}

#loads all stock in com.env$stx_list not in stx_list.old (returns loaded list)
load_stock_history <- function(stx_list.old) {
  print("load_stock_history")
  Sys.setenv(TZ = "UTC")
  adjustment <- TRUE
  start_date <- "2004-01-01" 
  end_date <- "2013-03-31"
  if (is.null(stx_list.old)) {         #only load if stx_list has changed
    getSymbols(Symbols = com.env$stx_list,
               env=data.env,
               src = "yahoo", 
               index.class = "POSIXct",
               from = start_date, 
               to = end_date, 
               adjust = adjustment)
  } else if (!identical(com.env$stx_list,stx_list.old)) {
    isNameinStxold <- com.env$stx_list %in% stx_list.old
    stx_list.new <- com.env$stx_list[!isNameinStxold]
    #print(stx_list.new)
    getSymbols(Symbols = stx_list.new, 
               env=data.env,
               src = "yahoo", 
               index.class = "POSIXct",
               from = start_date, 
               to = end_date, 
               adjust = adjustment)
  }
  print("successfully loaded stx")
  print(com.env$stx_list)
  return(com.env$stx_list)
}

#function loads stx_list into com.env 
stock_list <- function() {
  symbols <- c(
    "XLF", # Financial sector ETF
    #"BRK-B",    not valid name
    "JPM",
    "WFC",
    "BAC",
    "C",
    "USB",
    "GS",
    "AIG",
    "CB",
    "AXP",
    "MET",
    "MS",
    "BLK",
    "PNC",
    "BK",
    "SCHW",
    "CME",
    "COF",
    "MMC",
    "PRU",
    "TRV",
    "SPGI",
    "ICE",
    "BBT",
    "AON",
    "AFL",
    "STT",
    "ALL",
    "DFS",
    "STI",
    "PGR",
    "MTB",
    "HIG",
    "TROW",
    "AMP",
    "FITB",
    "NTRS",
    "PFG",
    "KEY",
    "IVZ",
    "BEN",
    "RF",
    "CINF",
    "L",
    "HBAN",
    "LNC",
    "XL",
    "AJG",
    "UNM",
    "CMA",
    "NDAQ",
    "AMG",
    "ETFC",
    "TMK",
    "ZION",
    "LUK",
    "AIZ",
    "LM",
    "GDX", #gold miners ETF
    "ABX",
    "NEM",
    "GG",
    "FNV",
    "AEM",
    "SLW",
    "GOLD",
    "AU",
    "RGLD",
    "KGC",
    "BVN",
    "TAHO",
    "AUY",
    "GFI",
    "EGO",
    "PAAS",
    "BTG",
    "HL",
    "AGI",
    "NGD",
    "IAG",
    "CDE",
    #"SBGL",   #too recent
    "AG",
    "SSRI",
    #"OR",    #problem loading data from YHOO
    "HMY",
    "MUX",
    "KLDX",
    "AKG",
    "SAND",
    "XLE", #Energy ETF
    'CVX',
    'SLB',
    'COP',
    'EOG',
    'OXY',
    'HAL',
    'KMI',
    'PSX',
    'VLO',
    'MPC',
    'PXD',
    'APC',
    'WMB',
    'BHI',
    'CXO',
    'APA',
    'DVN',
    'TSO',
    'FTI',
    'NBL',
    'NOV',
    'HES',
    'COG',
    'MRO',
    'OKE',
    'EQT',
    'XEC',
    'NFX',
    'HP',
    'RRC',
    'MUR',
    'CHK',
    'RIG'
  )
  
  #create cmn lookup (stock contains etf used as cmn, cmn contains the word 'cmn')
  com.env$cmn.symbols <- c('XLF',
                           'GDX',
                           'XLE'
  )
  num_symbols <- length(symbols)
  com.env$cmns <- length(com.env$cmn.symbols)
  com.env$stx <- num_symbols - com.env$cmns
  cmn_num <- which(symbols %in% com.env$cmn.symbols)
  if (length(cmn_num) != com.env$cmns) stop()        #bug in ticker or cmn list
  
  com.env$cmn_lookup <- rep('cmn',num_symbols)
  for (i in 1:com.env$cmns) {
    start_idx <- cmn_num[i] + 1
    end_idx <- ifelse(i < com.env$cmns,cmn_num[i+1]-1,num_symbols)
    com.env$cmn_lookup[start_idx:end_idx] <- com.env$cmn.symbols[i]
  }
  names(com.env$cmn_lookup) <- symbols
  
  com.env$stx.symbols <- symbols[!(symbols %in% com.env$cmn.symbols)]
  com.env$stx_list <- symbols
  
  #print(ls(com.env))
}
