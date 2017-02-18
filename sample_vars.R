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
V1$name <- "T"
V1$type <- "prc"
V1$tier <- 2
V1$requires <- c('H','L','C')
V1$ID <- which(rnd.env$priceID==V1$name)
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_math,c('H','L','C'),'XX0N <- (XX1*XX2*XX3)^(1/3)'"

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
V1$name <- "YT"
V1$type <- "prc"
V1$tier <- 3
V1$requires <- c('H','L','C','T')
V1$ID <- 10 + which(rnd.env$priceID==V1$requires[4])
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "from.var.env,'T'"
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
#rnd.env$ccraw_num <- which("CCraw" == names(rnd.env$vs.com))
rm(end_price,ep_num,vs_end,start_price,sp_num,vs_start)


V1 <- NULL
V1$col <- 1
V1$name <- "D"
V1$tier <- 1
V1$requires <- NULL
V1$ID <- which(rnd.env$priceID==V1$name)
V1$type <- "Vol"
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_dol,price='T'"

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
#V1$math[1] <- "from.var.env,field='D'"
V1$math[1] <- "calc_math,c('D'),math_str='XX0N <- log(XX1) - 18.5'"

cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
eval(parse(text=cmd_string))
rnd.env$vol_raw <- length(rnd.env$vs.com)
#rnd.env$vol_list <- c(rnd.env$vol_list,length(rnd.env$vs.com))

V1 <- NULL
V1$col <- 1
V1$name <- "TRraw"  #true range
V1$type <- "rng"
V1$tier <- 3
V1$requires <- c('H','L','C','YC')
V1$ID <- rnd.env$nameID[V1$name]
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_math,c('H','L','YC'),'XX0N <- pmax(log(XX1/XX2),abs(log(XX1/XX3)),abs(log(XX2/XX3)))'"

cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
eval(parse(text=cmd_string))
rnd.env$raw_list <- c(rnd.env$raw_list,length(rnd.env$vs.com))

V1 <- NULL
V1$col <- 1
V1$name <- "DM"  #directional movement
V1$type <- "ti"
V1$tier <- 4
V1$requires <- c('H','YH','YHHraw','L','YL','YLLraw')
V1$ID <- rnd.env$nameID[V1$name]
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_dm,'YHHraw','YLLraw'"

cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
eval(parse(text=cmd_string))
#rnd.env$raw_list <- c(rnd.env$raw_list,length(rnd.env$vs.com))

V1 <- NULL
V1$col <- 1
V1$name <- "DI"  #directional indicator
V1$type <- "ti"
V1$tier <- 4
V1$requires <- c('H','YH','YHHraw','L','YL','YLLraw','C','YC')
V1$ID <- rnd.env$nameID[V1$name]
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_math,c('DMd','TRd'),'XX0N <- XX1/XX2'"

cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
eval(parse(text=cmd_string))
#rnd.env$raw_list <- c(rnd.env$raw_list,length(rnd.env$vs.com))

V1 <- NULL
V1$col <- 1
V1$name <- "MF"  #money flow
V1$type <- "ti"
V1$tier <- 4
V1$requires <- c('H','L','C','T','YT','YTTraw','D')
V1$ID <- rnd.env$nameID[V1$name]
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_math,c('YTTraw','D'),math_str='XX0N <- ifelse(XX1>0,XX2,0)'"

cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
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
V1$name <- "FI"  #Force Index = return * Dollars
V1$type <- "ti"
V1$tier <- 4
V1$requires <- c('C','YC','CCraw','D')
V1$ID <- rnd.env$nameID[V1$name]
V1$use <- "calc"
V1$calc_cmn <- TRUE
V1$math[1] <- "calc_math,c('CCraw','D'),math_str='XX0N <- XX1*XX2'"

cmd_string <- paste("rnd.env$vs.com$",V1$name," <- V1",sep="")
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

save_vcom_vars <- function(var_num_list) {  #take var_num and create com.env$VCOM which defines it (including its dependencies)
  #print(paste("function vcom_var, var_num=",var_num))
  saved_var_files <- list.files(path=com.env$vardir)
  for (var_num in var_num_list) {
    vd <- com.env$v.com[[var_num]]
    if (is.null(vd$vcom_name)) {
      if (length(vd$name) == 1) {
        vd$vcom_name <- vd$name
      } else {
        vd$vcom_name <- substr(vd$name[1],1,(nchar(vd$name[1])-1))
      }
    }
    com.env$VCOM <- NULL
    for (v in vd$requires) {
      vcom_not_found <- TRUE
      i <- 0
      while ((vcom_not_found) & (i+1 < var_num)) {
        i <- i + 1
        if (length(com.env$v.com[[i]]$name) == 1) {
          if (v == com.env$v.com[[i]]$name) {
            vcom_not_found <- FALSE
          }
        } else if (!is.null(com.env$v.com$vcom_name)) {
          if (v == com.env$v.com[[i]]$name) {
            vcom_not_found <- FALSE
          }
        }
      }
      if (i == var_num) {
        print(paste("Error:All required vars for",vcom_name,"not defined in v.com, i=",i))
      }
      V1 <- com.env$v.com[[i]]
      cmd_string <- paste("com.env$VCOM$",V1$name," <- V1",sep="")
      #print(cmd_string)
      eval(parse(text=cmd_string))
    }
    cmd_string <- paste("com.env$VCOM$",vd$vcom_name," <- vd",sep="")
    print(cmd_string)
    eval(parse(text=cmd_string))
    varfile_name <- paste(vd$vcom_name,".vcom",sep="")
    varfile <- paste(com.env$vardir,"/",varfile_name,sep="")
    j <- 1
    save_file <- TRUE
    while (varfile_name %in% saved_var_files & save_file) {
      print(paste("Duplicate name",varfile_name))
      load(file=varfile,envir=rnd.env)
      saved_vd <- rnd.env$VCOM[[length(rnd.env$VCOM)]]
      if (vd$ID == saved_vd$ID) {
        print("Same ID, no need to save")
        save_file <- FALSE
      } else {
        j <- j + 1
        varfile_name <- paste(vd$vcom_name,"_",j,".vcom",sep="")
        varfile <- paste(com.env$vardir,"/",varfile_name,sep="")
      }
    }
    print(varfile)
    if (save_file) save(list=c("VCOM"),file=varfile,envir=com.env)
  }
}

load_rnd_var <- function() {
  #print("load_rnd_var")
  saved_var_files <- list.files(path=com.env$vardir)
  #print(saved_var_files)
  #print(com.env$var_files_tried)
  saved_var_files <- saved_var_files[!(saved_var_files %in% com.env$var_files_tried)]
  #print(saved_var_files)
  if (length(saved_var_files) > 0) {
    varfile_name <- sample(saved_var_files,size=1)
    print(paste(varfile_name,",",length(saved_var_files),"left"))
    com.env$var_files_tried <- c(com.env$var_files_tried,varfile_name)
    varfile <- paste(com.env$vardir,"/",varfile_name,sep="")
    load(file=varfile,envir=rnd.env)
    #print(names(rnd.env$VCOM))
    for (i in 1:length(names(rnd.env$VCOM))) {
      vname <- names(rnd.env$VCOM)[i]
      #print(vname)
      match <- FALSE
      if (vname %in% names(com.env$v.com)) {
        match <- length(com.env$v.com[[which(vname == names(com.env$v.com))]]$math) == length(rnd.env$VCOM[[vname]]$math)
        if (match) match <- 
            all(com.env$v.com[[which(vname == names(com.env$v.com))]]$math == rnd.env$VCOM[[vname]]$math)
        if (!match) {
          print("Can't load sample var, same name in requires list, but different math")
          return(-1)
        }
      }
      #if ((!match) & (vname!=orig_vname)) { #must update all "requires" fields
      #  for (j in (i+1):length(names(rnd.env$VCOM))) {
      #    vd <- rnd.env$VCOM[[j]]
      #    for (k in 1:length(vd$requires)) {
      #      if (vd$requires[k]==orig_vname) {
      #        vd$requires[k] <- vname
      #        print(paste("updating requires",vd$requires,vd$ID))
      #      }
      #    }
      #  }
      #}
      if (!match) {
        cmd_string <- paste("com.env$v.com$",vname," <- rnd.env$VCOM[[i]]",sep="")
        #print(cmd_string)
        eval(parse(text=cmd_string))
      } else {
        #print(paste(vname,"already in v.com"))
      }
    }
    return(0)
  } else {
    rnd.env$prob$type.wts[length(rnd.env$prob$type.wts)] <- 0. #prob of selecting var from file set to zero
    rnd.env$prob$type.bv.wts[length(rnd.env$prob$type.bv.wts)] <- 0. #prob of selecting var from file set to zero
    return(-1)                                                  #file always last entry in type.wts
  }
}


