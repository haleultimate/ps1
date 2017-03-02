source("rnd_parms.R")

#sample vars
V1 <- NULL
V1$col <- 1
V1$tier <- 1
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$type <- "Price"
V1$math[1] <- "from.data.env,'.Adjusted'"
V1 <- set_name(V1)
cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$type <- "prc"
V1$tier <- 1
V1$requires <- NULL
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_adj,'High'"
V1 <- set_name(V1)
cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$type <- "prc"
V1$tier <- 1
V1$requires <- NULL
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_adj,'Low'"
V1 <- set_name(V1)
cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$type <- "prc"
V1$tier <- 2
V1$requires <- c('H','L','C')
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_math,c('H','L','C'),'XX0N <- (XX1*XX2*XX3)^(1/3)'"
V1 <- set_name(V1)
cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$type <- "prc"
V1$tier <- 2
V1$requires <- c('H','L')
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_math,c('H','L'),'XX0N <- sqrt(XX1*XX2)'"
V1 <- set_name(V1)
cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$type <- "prc"
V1$tier <- 1
V1$requires <- NULL
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_adj,'Open'"
V1 <- set_name(V1)
cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$type <- "prc"
V1$tier <- 2
V1$requires <- "C"
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "from.var.env,'C'"
V1$math[2] <- "calc_lag,1"
V1 <- set_name(V1)
cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$type <- "prc"
V1$tier <- 2
V1$requires <- "H"
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "from.var.env,'H'"
V1$math[2] <- "calc_lag,1"
V1 <- set_name(V1)
cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$type <- "prc"
V1$tier <- 2
V1$requires <- "L"
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "from.var.env,'L'"
V1$math[2] <- "calc_lag,1"
V1 <- set_name(V1)
cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$type <- "prc"
V1$tier <- 3
V1$requires <- c('H','L','C','R')
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "from.var.env,'R'"
V1$math[2] <- "calc_lag,1"
V1 <- set_name(V1)
cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$type <- "prc"
V1$tier <- 3
V1$requires <- c('H','L','J')
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "from.var.env,'J'"
V1$math[2] <- "calc_lag,1"
V1 <- set_name(V1)
cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$type <- "prc"
V1$tier <- 2
V1$requires <- "O"
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "from.var.env,'O'"
V1$math[2] <- "calc_lag,1"
V1 <- set_name(V1)
cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$type <- "prc"
V1$tier <- 2
V1$requires <- "C"
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "from.var.env,'C'"
V1$math[2] <- "calc_lag,2"
V1 <- set_name(V1)
cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
eval(parse(text=cmd_string))

#data to calculate raw returns
#rnd.env$prclu <- NULL
#for (i in 1:length(rnd.env$vs.com)) {
#  rnd.env$prclu[i] <- rnd.env$vs.com[[i]]$name
#}

max_j <- length(rnd.env$vs.com)
for (i in 1:6) {
  #print(i)
  vd_end_price <- rnd.env$vs.com[[i]]
  for (j in (i+1):max_j) {
    V1 <- NULL
    V1$col <- 1
    V1$use <- "calc"
    V1$calc_cmn <- TRUE
    #print(paste(j,vd_end_price$name))
    vd_start_price <- rnd.env$vs.com[[j]]
    #print(vd_start_price$name)
    V1$type <- "ret"
    V1$tier <- 4
    V1$requires <- unique(c(vd_end_price$requires,vd_start_price$requires,vd_start_price$name,vd_end_price$name))
    V1$math[1] <- paste0("calc_ret,'",vd_start_price$name,"','",vd_end_price$name,"'")
    V1 <- set_name(V1)
    cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
    eval(parse(text=cmd_string))
    #print(V1$var_name)
    rnd.env$raw_list <- c(rnd.env$raw_list,length(rnd.env$vs.com))
  }
}
#rnd.env$ccraw_num <- which("CCraw" == names(rnd.env$vs.com))
rm(vd_end_price,vd_start_price)


V1 <- NULL
V1$col <- 1
V1$tier <- 1
V1$requires <- NULL
V1$type <- "Vol"
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_dol,price='R'"
V1 <- set_name(V1)
cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
eval(parse(text=cmd_string))

V1 <- NULL
V1$col <- 1
V1$tier <- 2
V1$requires <- "D"
V1$type <- "Vol"
V1$use <- "calc"
V1$calc_cmn <- TRUE
#V1$math[1] <- "from.var.env,field='D'"
V1$math[1] <- "calc_math,c('D'),math_str='XX0N <- log(XX1) - 18.5'"
V1 <- set_name(V1)
cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
eval(parse(text=cmd_string))
rnd.env$vol_raw <- length(rnd.env$vs.com)
#rnd.env$vol_list <- c(rnd.env$vol_list,length(rnd.env$vs.com))

V1 <- NULL
V1$col <- 1
V1$type <- "rng"
V1$tier <- 3
V1$requires <- c('H','L','C','B')
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_math,c('H','L','B'),'XX0N <- pmax(log(XX1/XX2),abs(log(XX1/XX3)),abs(log(XX2/XX3)))'"
V1 <- set_name(V1)
cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
eval(parse(text=cmd_string))
rnd.env$raw_list <- c(rnd.env$raw_list,length(rnd.env$vs.com))

V1 <- NULL
V1$col <- 1
#directional movement
V1$type <- "ti"
V1$tier <- 4
V1$requires <- c('H','G','GH','L','K','KL')
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_dm,'GH','KL'"
V1 <- set_name(V1)
cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
eval(parse(text=cmd_string))
#rnd.env$raw_list <- c(rnd.env$raw_list,length(rnd.env$vs.com))

V1 <- NULL
V1$col <- 1
#directional indicator
V1$type <- "ti"
V1$tier <- 4
V1$requires <- c('H','G','GH','L','K','KL','C','B')
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_math,c('DMd','TRd'),'XX0N <- XX1/XX2'" #DMd and TRd likely not correct. COME BACK
V1 <- set_name(V1)
cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
eval(parse(text=cmd_string))
#rnd.env$raw_list <- c(rnd.env$raw_list,length(rnd.env$vs.com))

V1 <- NULL
V1$col <- 1
#money flow
V1$type <- "ti"
V1$tier <- 4
V1$requires <- c('H','L','C','R','Q','QR','D')
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_math,c('QR','D'),math_str='XX0N <- ifelse(XX1>0,XX2,0)'"
V1 <- set_name(V1)
cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
eval(parse(text=cmd_string))
#rnd.env$raw_list <- c(rnd.env$raw_list,length(rnd.env$vs.com))

#V1 <- NULL
#V1$col <- 1
#V1$name <- "NMFraw"  #negative money flow
#V1$type <- "ti"
#V1$tier <- 4
#V1$requires <- c('T','YT','YTTraw','D')
#V1$ID <- rnd.env$nameID[[V1$name]]
#V1$use <- "calc"
#V1$calc_cmn <- TRUE
#V1$math[1] <- "calc_math,c('YTTraw','D'),math_str='XX0N <- ifelse(XX1<0,XX2,0)'"

#cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
#eval(parse(text=cmd_string))
#rnd.env$raw_list <- c(rnd.env$raw_list,length(rnd.env$vs.com))

V1 <- NULL
V1$col <- 1
#Force Index = return * Dollars
V1$type <- "ti"
V1$tier <- 4
V1$requires <- c('C','B','BC','D')
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_math,c('BC','D'),math_str='XX0N <- XX1*XX2'"
V1 <- set_name(V1)
cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
eval(parse(text=cmd_string))
#rnd.env$raw_list <- c(rnd.env$raw_list,length(rnd.env$vs.com))


#V1 <- NULL
#V1$col <- 1
#V1$name <- "ADXraw"  #positive directional movement
#V1$type <- "ti"
#V1$tier <- 5
#V1$requires <- c('H','YH','YHHraw','L','YL','YLLraw','C','YC','TRraw')
#V1$ID <- rnd.env$nameID[V1$name]
#V1$use <- "calc"
#V1$calc_cmn <- TRUE
#V1$math[1] <- paste("calc_adx,",V1$name,sep="")

#cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
#eval(parse(text=cmd_string))
#rnd.env$raw_list <- c(rnd.env$raw_list,length(rnd.env$vs.com))

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

#rnd.env$namelu <- NULL
#for (i in 1:length(rnd.env$vs.com)) {
#  rnd.env$namelu[i] <- rnd.env$vs.com[[i]]$name
#}

#Define MU,ADJRET,VLTY for use in simulation
# V1 <- NULL
# V1$col <- 1
# V1$name <- "MU"
# V1$tier <- 9999
# V1$requires <- NULL
# V1$ID <- 9999
# V1$type <- "Ret"
# V1$use <- "sim"
# V1$calc_cmn <- FALSE
# V1$math[1] <- "calc_prediction,'com.env$model.stepwise'"
# 
# rnd.env$vs.com$MU <- V1
# rm(V1)
# 
# V1 <- NULL
# V1$col <- 1
# V1$name <- "ADJRET"
# V1$tier <- 9999
# V1$requires <- NULL
# V1$ID <- 9999
# V1$type <- "Ret"
# V1$use <- "sim"
# V1$calc_cmn <- FALSE
# V1$math[1] <- "calc_adjret,'.Adjusted'"
# 
# com.env$v.com$ADJRET <- V1
# rm(V1)
# 
# V1 <- NULL
# V1$col <- 1
# V1$name <- "VLTY"
# V1$tier <- 9999
# V1$requires <- "ADJRET"
# V1$ID <- 9999
# V1$type <- "Vlt"
# V1$use <- "sim"
# V1$calc_cmn <- FALSE
# V1$math[1] <- "calc_vlty,'ADJRET',window=250"
# 
# com.env$v.com$VLTY <- V1

#print("End of Sample Vars")
