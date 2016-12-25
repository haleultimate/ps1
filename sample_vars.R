#sample vars
V1 <- NULL
V1$col <- 1
V1$name <- "O"
V1$type <- "prc"
V1$tier <- 1
V1$requires <- NULL
V1$ID <- which(rnd.env$priceID==V1$name)
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_adj,'Open'"

cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$name <- "H"
V1$type <- "prc"
V1$tier <- 1
V1$requires <- NULL
V1$ID <- which(rnd.env$priceID==V1$name)
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_adj,'High'"

cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$name <- "L"
V1$type <- "prc"
V1$tier <- 1
V1$requires <- NULL
V1$ID <- which(rnd.env$priceID==V1$name)
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_adj,'Low'"

cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$name <- "C"
V1$type <- "prc"
V1$tier <- 1
V1$requires <- NULL
V1$ID <- which(rnd.env$priceID==V1$name)
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "from.data.env,'.Adjusted'"

cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$name <- "M"
V1$type <- "prc"
V1$tier <- 2
V1$requires <- c('H','L')
V1$ID <- which(rnd.env$priceID==V1$name)
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_math,c('H','L'),'XX0N <- sqrt(XX1*XX2)'"

cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$name <- "YC"
V1$type <- "prc"
V1$tier <- 2
V1$requires <- "C"
V1$ID <- 10 + which(rnd.env$priceID==V1$requires)
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "from.var.env,'C'"
V1$math[2] <- "calc_lag,1"

cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$name <- "YO"
V1$type <- "prc"
V1$tier <- 2
V1$requires <- "O"
V1$ID <- 10 + which(rnd.env$priceID==V1$requires)
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "from.var.env,'O'"
V1$math[2] <- "calc_lag,1"

cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$name <- "YH"
V1$type <- "prc"
V1$tier <- 2
V1$requires <- "H"
V1$ID <- 10 + which(rnd.env$priceID==V1$requires)
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "from.var.env,'H'"
V1$math[2] <- "calc_lag,1"

cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$name <- "YL"
V1$type <- "prc"
V1$tier <- 2
V1$requires <- "L"
V1$ID <- 10 + which(rnd.env$priceID==V1$requires)
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "from.var.env,'L'"
V1$math[2] <- "calc_lag,1"

cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$name <- "YM"
V1$type <- "prc"
V1$tier <- 3
V1$requires <- c('H','L','M')
V1$ID <- 10 + which(rnd.env$priceID==V1$requires[3])
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "from.var.env,'M'"
V1$math[2] <- "calc_lag,1"

cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$name <- "C2"
V1$type <- "prc"
V1$tier <- 2
V1$requires <- "C"
V1$ID <- 100 + 10 + which(rnd.env$priceID==V1$requires)
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "from.var.env,'C'"
V1$math[2] <- "calc_lag,2"

cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
eval(parse(text=cmd_string))

#data to calculate raw returns
rnd.env$prclu <- NULL
for (i in 1:length(rnd.env$vs.com)) {
  rnd.env$prclu[i] <- rnd.env$vs.com[[i]]$name
}

V1 <- NULL
V1$col <- 1
V1$use <- "calc"
V1$calc_cmn <- TRUE
for (i in 1:5) {
  end_price <- names(rnd.env$prcorder[i])
  ep_num <- which(rnd.env$prclu == end_price)
  vs_end <- rnd.env$vs.com[[ep_num]]
  for (j in (i+1):length(rnd.env$prcorder)) {
    start_price <- names(rnd.env$prcorder[j])
    sp_num <- which(rnd.env$prclu == start_price)
    vs_start <- rnd.env$vs.com[[sp_num]]
    V1$name <- ifelse( (start_price == 'YC'), paste('C',end_price,"raw",sep=""), paste(start_price,end_price,"raw",sep="")) 
    V1$type <- ifelse( (rnd.env$prcorder[[end_price]] == rnd.env$prcorder[[start_price]]),"rng","ret")
    V1$tier <- max(vs_end$tier,vs_start$tier) + 1
    V1$requires <- unique(c(vs_end$requires,vs_start$requires,start_price,end_price))
    V1$ID <- ifelse(start_price == 'Y2C', 2410 + vs_end$ID, 100*vs_start$ID + 10 + vs_end$ID)
    V1$math[1] <- paste("calc_ret,'",start_price,"','",end_price,"'",sep="")

    cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
    eval(parse(text=cmd_string))
    rnd.env$raw_list <- c(rnd.env$raw_list,length(rnd.env$vs.com))
  }
}
rm(end_price,ep_num,vs_end,start_price,sp_num,vs_start)

#calc rets from raws (currently add abscap:.05)
#ret_list <- NULL
#for (i in raw_list) {
#  V1 <- rnd.env$vs.com[[i]]
#  V1$use <- "model"
#  V1$requires <- c(V1$requires,V1$name)
#  V1$tier <- V1$tier + 1
#  V1$math[1] <- paste("from.var.env,'",V1$name,"'",sep="")
#  V1$math[2] <- "calc_cap,abscap=0.05"
#  V1$ID <- paste(V1$ID,get_id(V1$math[2]),sep="")
#  V1$name <- sub("aw","et",V1$name)
#  cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
#  eval(parse(text=cmd_string))
#  ret_list <- c(ret_list,length(rnd.env$vs.com))  
#}

#calc res's from raws (currently calc_res)
#res_list <- NULL
#for (i in raw_list) {
#  V1 <- rnd.env$vs.com[[i]]
#  V1$use <- "model"
#  V1$calc_cmn <- FALSE
#  V1$requires <- c(V1$requires,V1$name)
#  V1$tier <- V1$tier + 1
#  V1$math[1] <- paste("from.var.env,'",V1$name,"'",sep="")
#  V1$math[2] <- paste("calc_res,'",V1$name,"'",sep="")
#  V1$ID <- paste(V1$ID,get_id(V1$math[2]),sep="")
#  V1$name <- sub("aw","es",V1$name)
#  cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
#  eval(parse(text=cmd_string))
#  res_list <- c(res_list,length(rnd.env$vs.com))  
#}

V1 <- NULL
V1$col <- 1
V1$name <- "D"
V1$tier <- 1
V1$requires <- NULL
V1$ID <- which(rnd.env$priceID==V1$name)
V1$type <- "Vol"
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_dol,price='M'"

cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$name <- "lD"
V1$tier <- 2
V1$requires <- "D"
V1$ID <- 10*rnd.env$vs.com$D$ID + 1
V1$type <- "Vol"
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "from.var.env,field='D'"
V1$math[2] <- "calc_math,math_str='XX0 <- log(XX0) - 18.5'"

cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
eval(parse(text=cmd_string))
rnd.env$vol_raw <- length(rnd.env$vs.com)
#rnd.env$vol_list <- c(rnd.env$vol_list,length(rnd.env$vs.com))

#V1 <- NULL
#V1$col <- 1
#V1$name <- "clD"
#V1$tier <- 2
#V1$requires <- "D"
#V1$ID <- 10*rnd.env$vs.com$D$ID + 1
#V1$type <- "Vol"
#V1$use <- "calc"
#V1$calc_cmn <- TRUE
#V1$math[1] <- "from.var.env,field='D'"
#V1$math[2] <- "calc_cap,cap_pct=0.01"
#V1$math[3] <- "calc_math,math_str='XX0 <- log(XX0) - 18.5'"

#cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
#eval(parse(text=cmd_string))
#rnd.env$vol_list <- c(rnd.env$vol_list,length(rnd.env$vs.com))

#V1 <- NULL
#V1$col <- 1
#V1$name <- "aDd2"
#V1$tier <- 3
#V1$requires <- c("D","clD")
#V1$ID <- 10*rnd.env$vs.com$clD$ID + 2
#V1$type <- "Vol"
#V1$use <- "calc"
#V1$calc_cmn <- TRUE
#V1$math[1] <- "from.var.env,field='clD'"
#V1$math[2] <- "calc_decay,decay=0.02"

#cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
#eval(parse(text=cmd_string))
#rnd.env$vol_list <- c(rnd.env$vol_list,length(rnd.env$vs.com))

#V1 <- NULL
#V1$col <- 12
#V1$name <- "ADV"
#V1$tier <- 1
#V1$requires <- NULL
#V1$ID <- 93
#V1$type <- "Vol"
#V1$use <- "calc"
#V1$calc_cmn <- TRUE
#V1$math[1] <- "calc_adv,window=30"

#cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
#eval(parse(text=cmd_string))
#rnd.env$vol_list <- c(rnd.env$vol_list,length(rnd.env$vs.com))

rnd.env$namelu <- NULL
for (i in 1:length(rnd.env$vs.com)) {
  rnd.env$namelu[i] <- rnd.env$vs.com[[i]]$name
}
