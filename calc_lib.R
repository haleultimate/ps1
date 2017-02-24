#calc_lib

name.var <- function(ve.xts,col_num,new_name,first_pass=FALSE) { #always name col_num, may have two names for bin vars
  if (first_pass & com.env$verbose) print(paste("name.var",ve.xts,col_num,new_name))
  for (n in 1:length(new_name)) {
    nn <- new_name[n]
    cn <- col_num[n]
    cmd_string <- paste("colnames(",ve.xts,")[",cn,"] <- '",nn,"'",sep="")
    if (first_pass & com.env$verbose) print(cmd_string)
    eval(parse(text=cmd_string))
  }
} 

calc_prediction <- function(ve.xts,coln,model,first_pass=FALSE) {
  #print(paste("ve.xts",ve.xts,"coln",coln,"model",model))
  #cmd_string <- paste("tmp.xts <- as.xts(predict.lm(",model,",newdata=data.frame(",ve.xts,")))",sep="") 
  #print(cmd_string)
  #eval(parse(text=cmd_string))
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",as.xts(predict.lm(",model,",newdata=data.frame(",ve.xts,"))))",sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
  #predict.lm(model,newdata=df.oos)
}

calc_adjret <- function(ve.xts,coln,field,first_pass=FALSE) {
  #print(paste("ve.xts",ve.xts,"coln",coln,"field",field))
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  de.xts <- paste("data.env$",ticker,"$",ticker,field,sep="")
  cmd_string <- paste("tmp.xts <- stats::lag(",de.xts,",-1)/",de.xts,sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
  tmp.xts[is.na(tmp.xts)] <- 0                  #replace NA's with 0
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",tmp.xts)",sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
}


from.data.env <- function(ve.xts,coln,field,first_pass=FALSE) {
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  if (first_pass & com.env$verbose) print(paste(ve.xts,coln,field,ticker))
  de.xts <- paste("data.env$",ticker,"$",ticker,field,sep="")
  if (exists(ve.xts)) {
    cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",",de.xts,")",sep="")
  } else {
    cmd_string <- paste(ve.xts," <- ",de.xts,sep="")
  }
  if (first_pass & com.env$verbose) print(cmd_string)
  eval(parse(text=cmd_string))
}

from.var.env <- function(ve.xts,coln,field,first_pass=FALSE) {
  #print(paste("from.var.env:","ve.xts=",ve.xts,"field=",field))
  ve.field <- paste(ve.xts,"$",field,sep="")
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",",ve.field,")",sep="")
  if (first_pass & com.env$verbose) print(cmd_string)
  eval(parse(text=cmd_string))
}

calc_look_forward <- function(ve.xts,coln,lf=-1,first_pass=FALSE) {
  cmd_string <- paste("tmp <- ",ve.xts,"[,'C']",sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
  tmp <- stats::lag(tmp,lf)
  cmd_string <- ifelse(lf<0,
                       paste(ve.xts," <- cbind(",ve.xts,",log(tmp/",ve.xts,"[,'C']))",sep=""),
                       paste(ve.xts," <- cbind(",ve.xts,",log(",ve.xts,"[,'C']/tmp))",sep=""))
  #print(cmd_string)
  eval(parse(text=cmd_string))
}

calc_lag <- function(ve.xts,coln,lag=1,first_pass=FALSE) {  #always lag coln
  #if (verbose) print(paste("calc_lag:","ve.xts=",ve.xts,"lag=",lag))
  cmd_string <- paste(ve.xts,"[,",coln,"] <- stats::lag(",ve.xts,"[,",coln,"],",lag,")",sep="")
  eval(parse(text=cmd_string))
}

calc_ret <- function(ve.xts,coln,start_price,end_price,first_pass=FALSE) {
  #if (verbose) print(paste("calc_ret:","ve.xts=",ve.xts,"start_price=",start_price,"end_price=",end_price))
  start_field <- paste(ve.xts,"$",start_price,sep="")
  end_field <- paste(ve.xts,"$",end_price,sep="")
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",log(",end_field,"/",start_field,"))",sep="")
  #if (verbose) print(cmd_string)
  eval(parse(text=cmd_string))
}

calc_cap <- function(ve.xts,coln,abscap=NULL,lcap=NULL,hcap=NULL,
                     cap_pct=NULL,lcp=NULL,hcp=NULL,zcap=NULL,lz=NULL,hz=NULL,first_pass=FALSE) {  #always cap coln
  if (com.env$verbose) print(paste("calc_cap:","ve.xts=",ve.xts,"coln=",coln,"abscap=",abscap,"cap_pct=",cap_pct,"zcap=",zcap))
  data_string <- paste(ve.xts,"[com.env$reg_date_range,",coln,"]",sep="")
  out_string <- paste(ve.xts,"[,",coln,"]",sep="")
  if (!is.null(abscap)) {
    lcap <- -abscap
    hcap <- abscap
  }
  if (!is.null(cap_pct)) {
    lcp <- cap_pct
    hcp <- 1-cap_pct
  }
  if (!is.null(zcap)) {
    lz <- -zcap
    hz <- zcap
  }
  if (!is.null(lcp)) {
    cmd_string <- paste("lcap <- quantile(",data_string,",lcp,na.rm=TRUE)",sep="")
    eval(parse(text=cmd_string))
    cmd_string <- paste("hcap <- quantile(",data_string,",hcp,na.rm=TRUE)",sep="")
    eval(parse(text=cmd_string))
  }
  if (!is.null(lz)) {
    cmd_string <- paste("sd_val <- sd(",data_string,",na.rm=TRUE)",sep="")
    eval(parse(text=cmd_string))
    cmd_string <- paste("mean_val <- mean(",data_string,",na.rm=TRUE)",sep="")
    eval(parse(text=cmd_string))
    lcap <- mean_val + lz*sd_val
    hcap <- mean_val + hz*sd_val
  }
  cmd_string <- paste(out_string,"[",out_string," < ",lcap,"] <- ",lcap,sep="")
  eval(parse(text=cmd_string))
  cmd_string <- paste(out_string,"[",out_string," > ",hcap,"] <- ",hcap,sep="")
  eval(parse(text=cmd_string))
}

#replace place holders XX0,XX1,..XXn with vars 
# XX0 represents last column, XX0N represents new column (can only be on right side)
# XX1..XXn existing vars
# math_str in the form 'XX0 <- f(XX0,XX1..XXn)' 
calc_math <- function(ve.xts,coln,XX_list=NULL,math_str,first_pass=FALSE) { #apply math to last column for XX0, create new last column XX0N
  #if (verbose) print(paste("ve.xts=",ve.xts,"XX_list=",XX_list,"math_str=",math_str))
  #ve.xts <- paste("var.env$",ticker,sep="")
  if (grepl("XX0N",math_str)) {
    place_holder_str <- "XX0N"
    data_string <- 'tmp.xts'
    new_var <- TRUE
  } else {
    place_holder_str <- "XX0"
    #cmd_string <- paste("col <- ncol(",ve.xts,")",sep="")
    #eval(parse(text=cmd_string))
    data_string <- paste(ve.xts,"[,",coln,"]",sep="")
    new_var <- FALSE
  }
  math_str <- gsub(place_holder_str,data_string,math_str)
  if (!is.null(XX_list)) {
    for (n in 1:length(XX_list)) {
      data_string <- paste(ve.xts,"[,'",XX_list[n],"']",sep="")
      place_holder_str <- paste("XX",n,sep="")
      math_str <- gsub(place_holder_str,data_string,math_str)
    }
  }
  if (first_pass & com.env$verbose) print(math_str)
  eval(parse(text=math_str))
  if (new_var) {
    cmd_str <- paste(ve.xts," <- cbind(",ve.xts,",tmp.xts)",sep="")
    if (first_pass & com.env$verbose) print(cmd_str)
    eval(parse(text=cmd_str))
  }
}

calc_z <- function(ve.xts,coln,ma=TRUE,first_pass=FALSE) { #compute zscore/zscale on coln
  #ve.xts <- paste("var.env$",ticker,sep="")
  #cmd_string <- paste("col <- ncol(",ve.xts,")",sep="")
  #eval(parse(text=cmd_string))
  data_string <- paste(ve.xts,"[com.env$reg_date_range,",coln,"]",sep="")
  out_string <- paste(ve.xts,"[,",coln,"]",sep="")
  cmd_string <- paste("sd_val <- sd(",data_string,",na.rm=TRUE)",sep="")
  eval(parse(text=cmd_string))
  if (sd_val > 0) {
    #print(cmd_string)
    mean_val <- 0
    if (ma) {
      cmd_string <- paste("mean_val <- mean(",data_string,",na.rm=TRUE)",sep="")
      #print(cmd_string)
      eval(parse(text=cmd_string))
    }
    #print(paste("mean_val=",mean_val,"sd_val=",sd_val))
    cmd_string <- paste(out_string," <- (",out_string," - mean_val)/sd_val",sep="")
    #print (cmd_string)
    eval(parse(text=cmd_string))
    #df.zscore <- scale(df,center=ma)
  } else {
    print(paste("WARNING:SD=0 in calc_z,coln=",coln))
    cmd_string <- paste(out_string," <- 0",sep="")
    eval(parse(text=cmd_string))
  }
}

calc_decay <- function(ve.xts,coln,decay,first_pass=FALSE) { #compute decay on coln
  #print(paste("ve.xts=",ve.xts,"decay=",decay))
  #ve.xts <- paste("var.env$",ticker,sep="")
  #cmd_string <- paste("col <- ncol(",ve.xts,")",sep="")
  #eval(parse(text=cmd_string))
  data_string <- paste(ve.xts,"[,",coln,"]",sep="")
  cmd_string <- paste("tmp.xts <-",data_string,sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
  tmp.xts[is.na(tmp.xts)] <- 0   #replace missing with 0
  tmp.ses <- ses(tmp.xts,alpha=decay)
  tmp.decay <- xts(fitted.values(tmp.ses),order.by = index(tmp.xts))
  cmd_string <- paste(data_string,"<- tmp.decay")
  #if (verbose) print(cmd_string)
  eval(parse(text=cmd_string))
}

calc_adj <- function(ve.xts,coln,field,first_pass=FALSE) { #take from data.env and append adjusted value to var.env
  #if (verbose) print(paste("ve.xts=",ve.xts,"field=",field))
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  de.xts <- paste("data.env$",ticker,"[,'",ticker,".",field,"']",sep="")
  de.adjc <- paste("data.env$",ticker,"[,'",ticker,".Adjusted']",sep="")
  de.c <- paste("data.env$",ticker,"[,'",ticker,".Close']",sep="")
  #ve.xts <- paste("var.env$",ticker,sep="")
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",",de.xts,"*",de.adjc,"/",de.c,")",sep="")
  #if (verbose) print(cmd_string)
  eval(parse(text=cmd_string))
  #adj_var <- df[,field] * df[,adjcnam] / df[,cnam]
}

#default window of 60 days
#future enhancements: price vlty (divide by close price), stdev (don't square)
calc_vlty <- function(ve.xts,coln,field,window=60,first_pass=FALSE) { #take vlty of field (in var.env) and append as last column
  #ve.xts <- paste("var.env$",ticker,"[,'",field,"']",sep="")
  f.xts <- paste(ve.xts,"[,'",field,"']",sep="")
  cmd_string <- paste("tmp.xts <- xts(apply(",f.xts,",2,runSD,n=window), index(",ve.xts,"))",sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
  #temp.xts <- stats::lag(temp.xts,1)   not needed if using day old returns
  tmp.xts <- tmp.xts*tmp.xts
  #df.vlt <- xts(apply(df,2,runSD,n=window), index(df))
  #df.vlt <- stats::lag(df.vlt,1) 
  #df.vlt <- df.vlt*df.vlt #vlt = sd*sd
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",tmp.xts)",sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
}

calc_adv <- function(ve.xts,coln,window=20,logv=TRUE,subtract=18.5,first_pass=FALSE)  {
  #if (verbose) print(paste("ve.xts=",ve.xts,"window=",window,"logv=",logv,"subtract=",subtract))
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  h.de <- paste("data.env$",ticker,"[,'",ticker,".High']",sep="")
  l.de <- paste("data.env$",ticker,"[,'",ticker,".Low']",sep="")
  v.de <- paste("data.env$",ticker,"[,'",ticker,".Volume']",sep="")
  cmd_string <- paste("tmp.xts <- sqrt(",h.de,"*",l.de,")*",v.de,sep="")
  #if (verbose) print(cmd_string)
  eval(parse(text=cmd_string))
  tmp.xts <- runMean(tmp.xts,n=window,cumulative=FALSE)
  if (logv) tmp.xts <- log(tmp.xts)
  if (subtract != 0) tmp.xts <- tmp.xts - subtract
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",tmp.xts)",sep="")
  #if (verbose) print(cmd_string)
  eval(parse(text=cmd_string))
  #logadv.z <- runMean(adjv[,"D"],n=20,cumulative=FALSE)
  #logadv.z <- log(logadv.z)
  #logadv.z <- logadv.z - 18.5 #create logadv.z, lb=20, assumed 18.5 as mean
}

calc_dol <- function(ve.xts,coln,price="T",first_pass=FALSE) {
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  v.de <- paste("data.env$",ticker,"[,'",ticker,".Volume']",sep="")
  cmd_string <- paste("tmp.xts <- data.env$",ticker,"[,'",ticker,".Volume']",sep="")
  eval(parse(text=cmd_string))
  tmp.xts[tmp.xts <= 0] <- 1
  if (price == "M") {
    h.de <- paste("data.env$",ticker,"[,'",ticker,".High']",sep="")
    l.de <- paste("data.env$",ticker,"[,'",ticker,".Low']",sep="")
    cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",sqrt(",h.de,"*",l.de,")*tmp.xts)",sep="")
  } else if (price == "T") {
    h.de <- paste("data.env$",ticker,"[,'",ticker,".High']",sep="")
    l.de <- paste("data.env$",ticker,"[,'",ticker,".Low']",sep="")
    c.de <- paste("data.env$",ticker,"[,'",ticker,".Close']",sep="")
    cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",tmp.xts*(",h.de,"*",l.de,"*",c.de,")^(1/3))",sep="")
  } else if (price == "C") {
    c.de <- paste("data.env$",ticker,"[,'",ticker,".Close']",sep="")
    cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",",c.de,"*tmp.xts)",sep="")
  }
  #if (verbose) print(cmd_string)
  eval(parse(text=cmd_string))
}

#looks up cmn from cmn_lookup
#subtracts cmn from raw variable held in each stock ve.xts
calc_res <- function(ve.xts,coln,field,first_pass=FALSE) {
  #if (verbose) print(paste("ve.xts=",ve.xts,"field=",field))
  #f.xts <- paste(ve.xts,"[,'",field,"']",sep="")
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  cmn <- com.env$cmn_lookup[ticker]
  cmd_string <- paste("cmn.xts <- merge(",ve.xts,"[,'",field,"'],var.env$",cmn,"[,'",field,"'],fill=0)",sep="")
  eval(parse(text=cmd_string))
  #cmn.xts[is.na(cmn.xts)] <- 0        #set cmn value to zero if missing (cmn not started yet)
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",(cmn.xts[,1]-cmn.xts[,2]))",sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
}

#looks up cmn from cmn_lookup
calc_cmn <- function(ve.xts,coln,field,first_pass=FALSE) {
  #if (verbose) print(paste("ve.xts=",ve.xts,"field=",field))
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  cmn <- com.env$cmn_lookup[ticker]
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",var.env$",cmn,"[,'",field,"'])",sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
}


#bin 'field' by 'bin_field' using bin_points b1 & b2
#append to ve.xts as last two columns
calc_bin <- function(ve.xts,coln,field=NULL,bin_field,b1=-2.,b2=2.,first_pass=FALSE) {
  if (first_pass & com.env$verbose) print(paste("ve.xts=",ve.xts,"coln"=coln,"field=",field,"bin_field=",bin_field,"Bins:",b1,b2,first_pass))
  #ve.xts <- paste("var.env$",ticker,sep="")
  if (is.null(field)) {
    f.xts <- paste(ve.xts,"[,",coln,"]",sep="")
    } else {
    f.xts <- paste(ve.xts,"[,'",field,"']",sep="")
    }
  bf.xts <- paste(ve.xts,"[,'",bin_field,"']",sep="")
  x <- c(b1,b2)
  y <- c(1,0)
  cmd_string <- paste("vl.xts <- ",f.xts,"*approx(x,y,",bf.xts,",yleft=1,yright=0)$y",sep="")
  #if (verbose) print (cmd_string)
  eval(parse(text=cmd_string))
  y <- c(0,1)
  cmd_string <- paste("vh.xts <- ",f.xts,"*approx(x,y,",bf.xts,",yleft=0,yright=1)$y",sep="")
  #if (verbose) print (cmd_string)
  eval(parse(text=cmd_string))
  if (is.null(field)) {
    cmd_string <- paste(ve.xts," <- cbind(",ve.xts,"[,-ncol(",ve.xts,")],vl.xts,vh.xts)",sep="") 
  } else {
    cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",vl.xts,vh.xts)",sep="")
  }
  if (first_pass & com.env$verbose) print (cmd_string)
  eval(parse(text=cmd_string))
    #calc_bin <- function(var,bin_var,b1=-2.,b2=2) {
    #v.temp <- na.exclude(merge(var,bin_var))
    #x <- c(b1,b2)
    #y <- c(1,0)
    #vl <- v.temp[,names(var)]*approx(x,y,v.temp[,names(bin_var)],yleft=1,yright=0)$y
    #y <- c(0,1)
    #vh <- v.temp[,names(var)]*approx(x,y,v.temp[,names(bin_var)],yleft=0,yright=1)$y
}

calc_dm <- function(ve.xts,coln,ret1,ret2,first_pass=FALSE) {
  #print(paste("calc_dm:","ve.xts=",ve.xts,"ret1=",ret1,"ret2=",ret2))
  if (ret1 == "YHHraw") {
    cmd_string <- paste("r1.xts <- ",ve.xts,"$",ret1,sep="")
    eval(parse(text=cmd_string))
    cmd_string <- paste("r2.xts <- -",ve.xts,"$",ret2,sep="")
    eval(parse(text=cmd_string))
  } else if (ret1 == "YLLraw") {
    cmd_string <- paste("r1.xts <- -",ve.xts,"$",ret1,sep="")
    eval(parse(text=cmd_string))
    cmd_string <- paste("r2.xts <- ",ve.xts,"$",ret2,sep="")
    eval(parse(text=cmd_string))
  } else {
    print(paste("Error in calc_dm, no valid return",ret1,ret2,sep=""))
    stop()
  }
  tmp.xts <- ifelse(r1.xts>r2.xts & r1.xts>0,r1.xts,0)
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",tmp.xts)",sep="")
  #if (verbose) print(cmd_string)
  eval(parse(text=cmd_string))
}

#make_vars.R
make_vars <- function(vd = NULL) {
  if (!is.null(vd)) if (vd$ID <= 0) vd <- NULL 
  make_vcom <- is.null(vd)
  first_pass <- TRUE
  #if (com.env$verbose) 
  if (com.env$verbose) print(paste("make_vars",make_vcom))
  if (make_vcom) {
    col.calc <- NULL
    col.cmn.calc <- NULL
    col.calc[1:length(com.env$v.com)] <- FALSE
    col.cmn.calc[1:length(com.env$v.com)] <- FALSE         #boolean array to indicate if column has been found for each var in v.com
  } 
  for (stk in 1:(com.env$stx + com.env$cmns)) {
    ticker <- com.env$stx_list[stk]
    if (com.env$verbose) print(paste("Getting data for:",ticker))
    is.cmn <- (com.env$cmn_lookup[[ticker]] == 'cmn')
    ve.xts <- paste("var.env$",ticker,sep="")
    if (make_vcom) {
      for (v in 1:length(com.env$v.com)) {
        if (is.cmn & !com.env$v.com[[v]]$calc_cmn) next          #nothing to compute in cmn
        if ((is.cmn & !col.cmn.calc[v]) | (!is.cmn & !col.calc[v])) {  #calculate new column num and insert it into v.com
          if (!exists(ticker,envir=var.env)) {
            coln <- 1
          } else {
            cmd_string <- paste("coln <- ncol(",ve.xts,") + 1",sep="")
            eval(parse(text=cmd_string))
          }
          if (is.cmn) {
            com.env$v.com[[v]]$cmn_col <- coln 
            col.cmn.calc[v] <- TRUE
          } else {
            com.env$v.com[[v]]$col <- coln
            col.calc[v] <- TRUE
          }
          if (first_pass & com.env$verbose) print(paste("finding col",coln," for v",v,is.cmn))
        } 
        vd <- com.env$v.com[[v]]
        coln <- ifelse(is.cmn,vd$cmn_col,vd$col)
        for (m in 1:length(vd$math)) {
          math <- strsplit(vd$math[m],split=",")[[1]]
          parms <- gsub("^[^,]*,","",vd$math[m])
          fun_call <- paste(math[1],"('",ve.xts,"',",coln,",",parms,",first_pass=first_pass)",sep="")
          if (first_pass & com.env$verbose) print(paste(fun_call,"m=",m,"v=",v,first_pass))
          eval(parse(text=fun_call))
        }
        if (first_pass & com.env$verbose) print(paste(coln,vd$name))
        name.var(ve.xts,(coln:(coln-1+length(vd$name))),vd$name,first_pass)
      } #end make var loop
    } else { #vd passed in, mod var
      if (is.cmn & !vd$calc_cmn) next          #nothing to compute in cmn
      cmd_string <- paste("coln <- ncol(",ve.xts,") + 1",sep="")
      eval(parse(text=cmd_string))
      if (first_pass & com.env$verbose) print(paste('mod_var',vd$vcom_num,coln))
      if (is.cmn) {
        vd$cmn_col <- coln
      } else {
        vd$col <- coln
      }
      for (m in 1:length(vd$math)) {
        math <- strsplit(vd$math[m],split=",")[[1]]
        parms <- gsub("^[^,]*,","",vd$math[m])
        fun_call <- paste(math[1],"('",ve.xts,"',",coln,",",parms,")",sep="")
        if (first_pass & com.env$verbose) print(paste(fun_call,"m=",m,"v=",vd$vcom_num))
        eval(parse(text=fun_call))
      }
      if (first_pass & com.env$verbose) {
        cmd_string <- paste("print(length(colnames(",ve.xts,")))",sep="")
        print(cmd_string)
        eval(parse(text=cmd_string))
      }
    } #end mod var
    if (stk>1) first_pass <- FALSE
  } #end stock loop
  if (!make_vcom) return(vd)
}

calc_vd <- function(vd) { #for use in computing MU,ADJRET,VLTY  #appended to each var.env$ticker xts object
  #print("calc VD")
  print(paste("calc_vd",vd$name,vd$math[1]))
  #print("starting stk loop")
  for (stk in 1:(com.env$stx+com.env$cmns)) {
    #print(stk)
    ticker <- com.env$stx_list[stk]
    #print(paste("Getting data for:",ticker))
    is.cmn <- (com.env$cmn_lookup[[ticker]] == 'cmn')
    if (is.cmn) next                           #nothing to compute in cmn
    ve.xts <- paste("var.env$",ticker,sep="")
    cmd_string <- paste("c <- ncol(",ve.xts,") + 1",sep="")      
    eval(parse(text=cmd_string))                               #get column number
    vd$col <- c
    
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

stk_matrix <- function(type,index=0) {
  print(paste("stk_oos_matrix",type,index))
  col_lu <- type
  if (index != 0) {type = paste0(type,index)}
  type <- paste0("var.env$",type)
  cmd_string <- paste0(type," <- NULL")
  #print(cmd_string)
  eval(parse(text=cmd_string))

  for (stk in 1:(com.env$stx+com.env$cmns)) {
    ticker <- com.env$stx_list[stk]
    #print(paste("Getting data for:",ticker))
    is.cmn <- (com.env$cmn_lookup[[ticker]] == 'cmn')
    if (is.cmn) next                           #nothing to compute in cmn
    ve.xts <- paste("var.env$",ticker,sep="")
    cmd_string <- paste0(type," <- cbind(",type,",",ve.xts,"[,'",col_lu,"'])")
    #print(cmd_string)
    eval(parse(text=cmd_string))
    cmd_string <- paste0("colnames(",type,")[ncol(",type,")] <- '",ticker,"'")
    #colnames(MU)[ncol(MU)] <- ticker
    #print(cmd_string)
    eval(parse(text=cmd_string))
  }
}