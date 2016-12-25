#calc_lib

name.var <- function(ve.xts,col_num,new_name) { #always name last column
  for (n in 1:length(new_name)) {
    nn <- new_name[n]
    cn <- col_num[n]
    cmd_string <- paste("colnames(",ve.xts,")[",cn,"] <- '",nn,"'",sep="")
    if (verbose) print(cmd_string)
    eval(parse(text=cmd_string))
  }
} 

calc_prediction <- function(ve.xts,coln,model) {
  #print(paste("ve.xts",ve.xts,"coln",coln,"model",model))
  #cmd_string <- paste("tmp.xts <- as.xts(predict.lm(",model,",newdata=data.frame(",ve.xts,")))",sep="") 
  #print(cmd_string)
  #eval(parse(text=cmd_string))
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",as.xts(predict.lm(",model,",newdata=data.frame(",ve.xts,"))))",sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
  #predict.lm(model,newdata=df.oos)
}

calc_adjret <- function(ve.xts,coln,field) {
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


from.data.env <- function(ve.xts,coln,field) {
  #if (verbose) print(paste("from.data.env:","ve.xts=",ve.xts,"field=",field))
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  if (verbose) print(paste(ve.xts,coln,field,ticker))
  de.xts <- paste("data.env$",ticker,"$",ticker,field,sep="")
  if (exists(ve.xts)) {
    cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",",de.xts,")",sep="")
  } else {
    cmd_string <- paste(ve.xts," <- ",de.xts,sep="")
  }
  eval(parse(text=cmd_string))
}

from.var.env <- function(ve.xts,coln,field) {
  #print(paste("from.var.env:","ve.xts=",ve.xts,"field=",field))
  ve.field <- paste(ve.xts,"$",field,sep="")
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",",ve.field,")",sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
}

calc_look_forward <- function(ve.xts,coln,lf=-1) {
  cmd_string <- paste("tmp <- ",ve.xts,"[,'C']",sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
  tmp <- stats::lag(tmp,lf)
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",log(tmp/",ve.xts,"[,'C']))",sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
}

calc_lag <- function(ve.xts,coln,lag=1) {  #always lag coln
  #if (verbose) print(paste("calc_lag:","ve.xts=",ve.xts,"lag=",lag))
  cmd_string <- paste(ve.xts,"[,",coln,"] <- stats::lag(",ve.xts,"[,",coln,"],",lag,")",sep="")
  eval(parse(text=cmd_string))
}

calc_ret <- function(ve.xts,coln,start_price,end_price) {
  #if (verbose) print(paste("calc_ret:","ve.xts=",ve.xts,"start_price=",start_price,"end_price=",end_price))
  start_field <- paste(ve.xts,"$",start_price,sep="")
  end_field <- paste(ve.xts,"$",end_price,sep="")
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",log(",end_field,"/",start_field,"))",sep="")
  #if (verbose) print(cmd_string)
  eval(parse(text=cmd_string))
}

calc_cap <- function(ve.xts,coln,abscap=NULL,lcap=NULL,hcap=NULL,
                     cap_pct=NULL,lcp=NULL,hcp=NULL,zcap=NULL,lz=NULL,hz=NULL) {  #always cap coln
  if (verbose) print(paste("calc_cap:","ve.xts=",ve.xts,"coln=",coln,"abscap=",abscap,"cap_pct=",cap_pct,"zcap=",zcap))
  data_string <- paste(ve.xts,"[,",coln,"]",sep="")
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
    cmd_string <- paste("lcap <- quantile(",ve.xts,"[com.env$reg_data_range,",coln,"],lcp,na.rm=TRUE)",sep="")
    eval(parse(text=cmd_string))
    cmd_string <- paste("hcap <- quantile(",ve.xts,"[com.env$reg_data_range,",coln,"],hcp,na.rm=TRUE)",sep="")
    eval(parse(text=cmd_string))
  }
  if (!is.null(lz)) {
    cmd_string <- paste("sd <- sd(",ve.xts,"[com.env$reg_data_range,",coln,"],na.rm=TRUE)",sep="")
    eval(parse(text=cmd_string))
    lcap <- lz*sd
    hcap <- hz*sd
  }
  cmd_string <- paste(data_string,"[",data_string," < ",lcap,"] <- ",lcap,sep="")
  eval(parse(text=cmd_string))
  cmd_string <- paste(data_string,"[",data_string," > ",hcap,"] <- ",hcap,sep="")
  eval(parse(text=cmd_string))
}

#replace place holders XX0,XX1,..XXn with vars 
# XX0 represents last column, XX0N represents new column (can only be on right side)
# XX1..XXn existing vars
# math_str in the form 'XX0 <- f(XX0,XX1..XXn)' 
calc_math <- function(ve.xts,coln,XX_list=NULL,math_str) { #apply math to last column for XX0, create new last column XX0N
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
  #print(math_str)
  eval(parse(text=math_str))
  if (new_var) {
    cmd_str <- paste(ve.xts," <- cbind(",ve.xts,",tmp.xts)",sep="")
    #if (verbose) print(cmd_str)
    eval(parse(text=cmd_str))
  }
}

calc_z <- function(ve.xts,coln,ma=TRUE) { #compute zscore/zscale on coln
  #ve.xts <- paste("var.env$",ticker,sep="")
  #cmd_string <- paste("col <- ncol(",ve.xts,")",sep="")
  #eval(parse(text=cmd_string))
  data_string <- paste(ve.xts,"[com.env$reg_date_range,",coln,"]",sep="")
  out_string <- paste(ve.xts,"[,",coln,"]",sep="")
  cmd_string <- paste("sd_val <- sd(",data_string,",na.rm=TRUE)",sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
  mean_val <- 1
  if (ma) {
    cmd_string <- paste("mean_val <- mean(",data_string,",na.rm=TRUE)",sep="")
    #print(cmd_string)
    eval(parse(text=cmd_string))
  }
  #print(paste("mean_val=",mean_val,"sd_val=",sd_val))
  cmd_string <- paste(out_string," <- (",out_string," - sd_val)/mean_val",sep="")
  #print (cmd_string)
  eval(parse(text=cmd_string))
  #df.zscore <- scale(df,center=ma)
}

calc_decay <- function(ve.xts,coln,decay) { #compute decay on coln
  #if (verbose) print(paste("ve.xts=",ve.xts,"decay=",decay))
  #ve.xts <- paste("var.env$",ticker,sep="")
  #cmd_string <- paste("col <- ncol(",ve.xts,")",sep="")
  #eval(parse(text=cmd_string))
  data_string <- paste(ve.xts,"[,",coln,"]",sep="")
  cmd_string <- paste("tmp.xts <-",data_string,sep="")
  #if (verbose) print(cmd_string)
  eval(parse(text=cmd_string))
  tmp.xts[is.na(tmp.xts)] <- 0   #replace missing with 0
  tmp.ses <- ses(tmp.xts,alpha=decay)
  tmp.decay <- xts(fitted.values(tmp.ses),order.by = index(tmp.xts))
  cmd_string <- paste(data_string,"<- tmp.decay")
  #if (verbose) print(cmd_string)
  eval(parse(text=cmd_string))
}

calc_adj <- function(ve.xts,coln,field) { #take from data.env and append adjusted value to var.env
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
calc_vlty <- function(ve.xts,coln,field,window=60) { #take vlty of field (in var.env) and append as last column
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

calc_adv <- function(ve.xts,coln,window=20,logv=TRUE,subtract=18.5)  {
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

calc_dol <- function(ve.xts,coln,price="M") {
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  v.de <- paste("data.env$",ticker,"[,'",ticker,".Volume']",sep="")
  if (price == "M") {
    h.de <- paste("data.env$",ticker,"[,'",ticker,".High']",sep="")
    l.de <- paste("data.env$",ticker,"[,'",ticker,".Low']",sep="")
    cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",sqrt(",h.de,"*",l.de,")*",v.de,")",sep="")
  } else if (price == "C") {
    c.de <- paste("data.env$",ticker,"[,'",ticker,".Close']",sep="")
    cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",",c.de,"*",v.de,")",sep="")
  }
  #if (verbose) print(cmd_string)
  eval(parse(text=cmd_string))
}

#looks up cmn from cmn_lookup
#subtracts cmn from raw variable held in each stock ve.xts
calc_res <- function(ve.xts,coln,field) {
  #if (verbose) print(paste("ve.xts=",ve.xts,"field=",field))
  #ve.xts <- paste("var.env$",ticker,sep="")
  f.xts <- paste(ve.xts,"[,'",field,"']",sep="")
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  cmn <- cmn_lookup[ticker]
  cmn.xts <- paste("var.env$",cmn,"[,'",field,"']",sep="")
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",(",f.xts,"-",cmn.xts,"))",sep="")
  #if (verbose) print(cmd_string)
  eval(parse(text=cmd_string))
}

#bin 'field' by 'bin_field' using bin_points b1 & b2
#append to ve.xts as last two columns
calc_bin <- function(ve.xts,coln,field=NULL,bin_field,b1=-2.,b2=2.) {
  #if (verbose) print(paste("ve.xts=",ve.xts,"field=",field,"bin_field=",bin_field,"Bins:",b1,b2))
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
  #if (verbose) print (cmd_string)
  eval(parse(text=cmd_string))
    #calc_bin <- function(var,bin_var,b1=-2.,b2=2) {
    #v.temp <- na.exclude(merge(var,bin_var))
    #x <- c(b1,b2)
    #y <- c(1,0)
    #vl <- v.temp[,names(var)]*approx(x,y,v.temp[,names(bin_var)],yleft=1,yright=0)$y
    #y <- c(0,1)
    #vh <- v.temp[,names(var)]*approx(x,y,v.temp[,names(bin_var)],yleft=0,yright=1)$y
}