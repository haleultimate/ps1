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
  "LM"
)
stx_n <- c(2:length(symbols)) #c(2:length(symbols))  c(2:length(symbols))  #c(5:10) #
stx.symbols <- symbols[stx_n]      #list of stx to trade
stx <- length(stx_n)
#load mktdata
stx_list <- append(1,stx_n)
stx_list <- symbols[stx_list]      #cmn index + stx to trade

rm.list <- ls(all=TRUE)
keep.list <- c(stx_list,"stx","stx_list","stx.symbols","stx_n","stx_list.old","data.env")
isNameinKeep <- rm.list %in% keep.list
rm.list <- c(rm.list[!isNameinKeep],"keep.list","isNameinKeep","rm.list")
rm(list = rm.list)  #clear environment except for loaded stock data 

#setup output to go to logfile
original_wd <- getwd()
#logdir <- paste(original_wd,"/logs",sep="")
#setwd(logdir)
#logfile <- file("logfile.txt","w")
#sink(file=logfile,type="output")
print(paste("Start time:",Sys.time()))
#setwd(original_wd)

require(lpSolveAPI)
require(quantmod)
require(dplyr)
require(forecast)
if (!exists("data.env")) data.env <- new.env()
var.env <- new.env()
rnd.env <- new.env()
com.env <- new.env()

#Init data_load vars
Sys.setenv(TZ = "UTC")
adjustment <- TRUE
start_date <- "2004-01-01" 
end_date <- "2013-03-31"
if (!exists("stx_list.old")) {         #only load if stx_list has changed
  getSymbols(Symbols = stx_list,
             env=data.env,
             src = "yahoo", 
             index.class = "POSIXct",
             from = start_date, 
             to = end_date, 
             adjust = adjustment)
  stx_list.old <- stx_list
} else if (!identical(stx_list,stx_list.old)) {
  isNameinStxold <- stx_list %in% stx_list.old
  stx_list.new <- stx_list[!isNameinStxold]
  getSymbols(Symbols = stx_list.new, 
             env=data.env,
             src = "yahoo", 
             index.class = "POSIXct",
             from = start_date, 
             to = end_date, 
             adjust = adjustment)
  rm(stx_list.new,isNameinStxold)
  stx_list.old <- stx_list
}

#create cmn lookup (stock contains etf used as cmn, cmn contains the word 'cmn')
cmn_lookup <- rep('XLF',stx+1)
names(cmn_lookup) <- stx_list
cmn_lookup['XLF'] <- 'cmn'
cmns <- 1

verbose <- FALSE
com.env$model_loops <- 20
com.env$add_vars <- 20
#run_type <- "add_vars"
#insample.r2.threshold <- 0.02
predict.ret <- "C2Clf1p"    #should be set up as first model_var in v.com (define_vars.R)
com.env$reg_start_date <- "200407"
com.env$reg_end_date <- "20111231"
com.env$OOS_start_date <- "20120101"
com.env$OOS_end_date <- "20121231"
com.env$reg_date_range <- paste(com.env$reg_start_date,"/",com.env$reg_end_date,sep="")
com.env$OOS_date_range <- paste(com.env$OOS_start_date,"/",com.env$OOS_end_date,sep="")
com.env$sim_date_index <- index(data.env$XLF[com.env$OOS_date_range])

corr.threshold <- 0.6
#v.com <- NULL
#load(file="cores50.Rdata")
#if (verbose & exists("store.data")) print(store.data[length(store.data)])

#remove problematic stx
static.stx.symbols <- stx.symbols
for (i in 1:stx) {
  ticker <- static.stx.symbols[i]
  if (make.names(ticker) != ticker) {
    if (verbose) print(paste("remove",ticker,"from list, not valid name",make.names(ticker)))
    stx.symbols <- stx.symbols[-which(stx.symbols == ticker)] #remove from stx list
    stx_list <- stx_list[-which(stx_list == ticker)]
  }
  cmn <- cmn_lookup[ticker]
  cmd_string <- paste("corr.data <- cbind(data.env$",cmn,"[,'",cmn,".Adjusted'],data.env$",ticker,"[,'",ticker,".Adjusted'])",sep="")
  #if (verbose) print(cmd_string)
  eval(parse(text=cmd_string))
  corr.val <- cor(corr.data[com.env$reg_date_range],use="complete.obs")[1,2]
  cmd_string <- paste("not_enough_history <- nrow(data.env$",ticker,") < 320",sep="")
  eval(parse(text=cmd_string))
  if ( (not_enough_history) | (corr.val < corr.threshold) ) {
    #if (verbose) print(paste("remove",ticker,"from stx list, not correlated with cmn",corr.val))
    stx.symbols <- stx.symbols[-which(stx.symbols == ticker)] #remove from stx list
    stx_list <- stx_list[-which(stx_list == ticker)]
  } #else print(corr.val)
}
rm(static.stx.symbols,corr.val,corr.data,corr.threshold)

stx <- length(stx.symbols)
port_size <- init_equity <- 10000*stx


