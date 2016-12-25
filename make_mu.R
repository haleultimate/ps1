#make_mu.R
#make xts columns in var.env for MU, VLTY, ADJRET
#accumulate into xts for MU, VLTY, ADJRET with stx as columns
V1 <- NULL
V1$col <- 1
V1$name <- "MU"
V1$tier <- 9999
V1$requires <- NULL
V1$ID <- 9999
V1$type <- "Ret"
V1$use <- "sim"
V1$calc_cmn <- FALSE
V1$math[1] <- "calc_prediction,'model.stepwise'"

com.env$v.com$MU <- V1
com.env$vcom_names <- c(com.env$vcom_names,V1$name)
com.env$mu_vcom_num <- length(com.env$v.com)
rm(V1)

V1 <- NULL
V1$col <- 1
V1$name <- "ADJRET"
V1$tier <- 9999
V1$requires <- NULL
V1$ID <- 9999
V1$type <- "Ret"
V1$use <- "sim"
V1$calc_cmn <- FALSE
V1$math[1] <- "calc_adjret,'.Adjusted'"

com.env$v.com$ADJRET <- V1
com.env$vcom_names <- c(com.env$vcom_names,V1$name)
com.env$adjret_vcom_num <- length(com.env$v.com)
rm(V1)

V1 <- NULL
V1$col <- 1
V1$name <- "VLTY"
V1$tier <- 9999
V1$requires <- "ADJRET"
V1$ID <- 9999
V1$type <- "Vlt"
V1$use <- "sim"
V1$calc_cmn <- FALSE
V1$math[1] <- "calc_vlty,'ADJRET',window=250"

com.env$v.com$VLTY <- V1
com.env$vcom_names <- c(com.env$vcom_names,V1$name)
com.env$vlty_vcom_num <- length(com.env$v.com)
rm(V1)


for (stk in 1:(stx+cmns)) {
  ticker <- stx_list[stk]
  #print(paste("Getting data for:",ticker))
  is.cmn <- (cmn_lookup[[ticker]] == 'cmn')
  if (is.cmn) next                           #nothing to compute in cmn
  ve.xts <- paste("var.env$",ticker,sep="")
  for (v in c(com.env$mu_vcom_num,com.env$adjret_vcom_num,com.env$vlty_vcom_num)) {
    
    cmd_string <- paste("c <- ncol(",ve.xts,") + 1",sep="")      
    eval(parse(text=cmd_string))                               #get column number
    com.env$v.com[[v]]$col <- c
    
    vd <- com.env$v.com[[v]]
    coln <- vd$col
    for (m in 1:length(vd$math)) {
      math <- strsplit(vd$math[m],split=",")[[1]]
      parms <- gsub("^[^,]*,","",vd$math[m])
      fun_call <- paste(math[1],"('",ve.xts,"',",coln,",",parms,")",sep="")
      #print(fun_call)
      eval(parse(text=fun_call))
    }
    name.var(ve.xts,(coln:(coln-1+length(vd$name))),vd$name)
  }
}

MU <- NULL
VLTY <- NULL
ADJRET <- NULL

for (stk in 1:(stx+cmns)) {
  ticker <- stx_list[stk]
  #print(paste("Getting data for:",ticker))
  is.cmn <- (cmn_lookup[[ticker]] == 'cmn')
  if (is.cmn) next                           #nothing to compute in cmn
  ve.xts <- paste("var.env$",ticker,sep="")
  cmd_string <- paste("MU <- cbind(MU,",ve.xts,"[,'MU'])",sep="")
  eval(parse(text=cmd_string))
  colnames(MU)[ncol(MU)] <- ticker
  cmd_string <- paste("VLTY <- cbind(VLTY,",ve.xts,"[,'VLTY'])",sep="")
  eval(parse(text=cmd_string))
  colnames(VLTY)[ncol(VLTY)] <- ticker
  cmd_string <- paste("ADJRET <- cbind(ADJRET,",ve.xts,"[,'ADJRET'])",sep="")
  eval(parse(text=cmd_string))
  colnames(ADJRET)[ncol(ADJRET)] <- ticker
}

