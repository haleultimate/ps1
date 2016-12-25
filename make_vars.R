#make_vars.R
col.calc <- NULL
col.cmn.calc <- NULL
col.calc[1:length(com.env$v.com)] <- FALSE
col.cmn.calc[1:length(com.env$v.com)] <- FALSE         #boolean array to indicate if column has been found for each var in v.com
for (stk in 1:(stx+cmns)) {
  ticker <- stx_list[stk]
  if (verbose) print(paste("Getting data for:",ticker))
  is.cmn <- (cmn_lookup[[ticker]] == 'cmn')
  ve.xts <- paste("var.env$",ticker,sep="")
  for (v in 1:length(com.env$v.com)) {
    if (is.cmn & !com.env$v.com[[v]]$calc_cmn) next          #nothing to compute in cmn

    if ((is.cmn & !col.cmn.calc[v]) | (!is.cmn & !col.calc[v])) {  #calculate new column num and insert it into v.com
      if (!exists(ticker,envir=var.env)) {
        c <- 1
      } else {
        cmd_string <- paste("c <- ncol(",ve.xts,") + 1",sep="")
        eval(parse(text=cmd_string))
      }
      if (is.cmn) {
        com.env$v.com[[v]]$cmn_col <- c 
        col.cmn.calc[v] <- TRUE
      } else {
        com.env$v.com[[v]]$col <- c
        col.calc[v] <- TRUE
      }
    } 

    vd <- com.env$v.com[[v]]
    coln <- ifelse(is.cmn,vd$cmn_col,vd$col)
    for (m in 1:length(vd$math)) {
      math <- strsplit(vd$math[m],split=",")[[1]]
      parms <- gsub("^[^,]*,","",vd$math[m])
      fun_call <- paste(math[1],"('",ve.xts,"',",coln,",",parms,")",sep="")
      if (verbose) print(fun_call)
      eval(parse(text=fun_call))
    }
    name.var(ve.xts,(coln:(coln-1+length(vd$name))),vd$name)
  }
}
