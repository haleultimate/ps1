#make_vars.R
make_vars <- function(vd = NULL) {
  #col.calc <- NULL
  #col.cmn.calc <- NULL
  #col.calc[1:length(com.env$v.com)] <- FALSE
  #col.cmn.calc[1:length(com.env$v.com)] <- FALSE         #boolean array to indicate if column has been found for each var in v.com
  for (stk in 1:(com.env$stx + com.env$cmns)) {
    ticker <- com.env$stx_list[stk]
    if (com.env$verbose) print(paste("Getting data for:",ticker))
    is.cmn <- (com.env$cmn_lookup[[ticker]] == 'cmn')
    ve.xts <- paste("var.env$",ticker,sep="")
    if (is.null(vd)) {
      for (v in 1:length(com.env$v.com)) {
        if (is.cmn & !com.env$v.com[[v]]$calc_cmn) next          #nothing to compute in cmn
        
        #if ((is.cmn & !col.cmn.calc[v]) | (!is.cmn & !col.calc[v])) {  #calculate new column num and insert it into v.com
        if (!exists(ticker,envir=var.env)) {
          c <- 1
        } else {
          cmd_string <- paste("c <- ncol(",ve.xts,") + 1",sep="")
          eval(parse(text=cmd_string))
        }
        if (is.cmn) {
          com.env$v.com[[v]]$cmn_col <- c 
          #col.cmn.calc[v] <- TRUE
        } else {
          com.env$v.com[[v]]$col <- c
          #col.calc[v] <- TRUE
        }
        #} 
        vd <- com.env$v.com[[v]]
        coln <- ifelse(is.cmn,vd$cmn_col,vd$col)
        for (m in 1:length(vd$math)) {
          math <- strsplit(vd$math[m],split=",")[[1]]
          parms <- gsub("^[^,]*,","",vd$math[m])
          fun_call <- paste(math[1],"('",ve.xts,"',",coln,",",parms,")",sep="")
          if (com.env$verbose) print(paste(fun_call,"m=",m,"v=",v))
          eval(parse(text=fun_call))
        }
        #if (vd$name[1] == "lD" & stk == 1) print(vd)
        name.var(ve.xts,(coln:(coln-1+length(vd$name))),vd$name)
      } #end make var loop
    } else { #vd defined, mod var
      cmd_string <- paste("coln <- ncol(",ve.xts,") + 1",sep="")
      eval(parse(text=cmd_string))
      for (m in 1:length(vd$math)) {
        math <- strsplit(vd$math[m],split=",")[[1]]
        parms <- gsub("^[^,]*,","",vd$math[m])
        fun_call <- paste(math[1],"('",ve.xts,"',",coln,",",parms,")",sep="")
        if (com.env$verbose) print(paste(fun_call,"m=",m,"v=",v))
        eval(parse(text=fun_call))
      }
      #if (vd$name[1] == "lD" & stk == 1) print(vd)
      for (n in 1:length(vd$name)) {
        cmd_string <- paste("colnames(",ve.xts,")[",coln,"] <- 'mod_val",n,"'",sep="")
        print(cmd_string)
        eval(parse(text=cmd_string))
      }
    } #end mod var
  } #end stock loop
}
