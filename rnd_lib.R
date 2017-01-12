#rnd_lib.R
get_id <- function(math.list) {
  id <- NULL
  for (math_str in math.list) {
    math <- strsplit(math_str,split=",")[[1]][1] #get element to first comma (function call)
    fun_id <- which(math==names(rnd.env$fun_id))
    if (length(fun_id) > 0 & length(id) > 0) id <- paste(id,fun_id,sep="")
    if (length(fun_id) > 0 & length(id) == 0) id <- fun_id
    parms <- gsub("^[^,]*,","",math_str) #get everything after first comma (parameters)
    parms <- gsub("[^0-9]","",parms)     #remove all non-numeric characters
    if (length(parms) > 0 & length(id) > 0) id <- paste(id,parms,sep="")
    if (length(parms) > 0 & length(id) == 0) id <- parms
  }
  return(id)
}

#insert sample var into v.com given its name
scom2vcom <- function(V1) {
  #print(paste("scom2vcom",V1$name,V1$requires))
  if (!(V1$name %in% com.env$vcom_names)) {
    if (length(V1$requires) > 0) {
      for (i in 1:length(V1$requires)) {
        if (V1$requires[i] %in% rnd.env$namelu) {
          n <- which(rnd.env$namelu == V1$requires[i])
          V2 <- rnd.env$vs.com[[n]]
          scom2vcom(V2)                 #recursive call
        } else {
          if (!(V1$requires[i] %in% names(com.env$v.com))) {
            print(paste("Error",V1$requires[i],"not found in sample_vars or v.com, needed for",V1$name))
            stop()
          }
        }
        #print(paste("inner loop scom2vcom:",n,V1$name,V2$name))
      }
    }
    cmd_string <- paste("com.env$v.com$",V1$name," <- V1",sep="")
    #print(cmd_string)
    eval(parse(text=cmd_string))
    com.env$vcom_names <- c(com.env$vcom_names,V1$name)
  }
} 

rnd_val <- function(choice,type="model") {
  if (type == "model") {
    cmd_string <- paste("return(sample(rnd.env$prob$",choice,",size=1,prob=rnd.env$prob$",choice,".wts))",sep="")
  } else if (type == "bin") {
    cmd_string <- paste("return(sample(rnd.env$prob$",choice,".bv,size=1,prob=rnd.env$prob$",choice,".bv.wts))",sep="")
  }
  eval(parse(text=cmd_string))
}

unique_name <- function(name,id,first=TRUE) {
  #print(paste("name=",name,"id=",id,"first=",first))
  if (name %in% com.env$vcom_names) {
    for (i in 1:length(com.env$v.com)) {
      if (name %in% com.env$v.com[[i]]$name) {
        old_id <- com.env$v.com[[i]]$ID
        break
      }
    }  
    #print(paste("id",id,"old_id",old_id,which(name==com.env$vcom_names),com.env$v.com[[11]]$ID))
    if (id == old_id) return(-1)
    if (first) {
      new_name <- paste(name,"2",sep="")
    } else {
      num <- as.integer(substr(name,nchar(name),nchar(name)))
      num <- as.character(num + 1)
      new_name <- gsub(".$",num,name)
    }
    return(unique_name(new_name,id,first=FALSE)) 
  } else {
    return(name)
  }
}

select_rnd_raw <- function() {
  choice <- sample(rnd.env$prob$raw_var,size=1,prob=rnd.env$prob$raw_var.wts)
  switch(choice,
         "retrange" = {
           V1 <- rnd.env$vs.com[[sample(rnd.env$raw_list,1)]]
         },
         "ccd" = {
           V1 <- rnd.env$vs.com[[which("CCraw" == names(rnd.env$vs.com))]]
           d <- rnd_val("decay")
           if (d>=1) d <- 0.10
           V1$math[2] <- paste('calc_decay,decay=',d,sep="")
           V1$name <- paste('CCd',100*d,"raw",sep="")
           V1$ID <- 100*(V1$ID + d) 
         },
         "c2c" = {
           lag <- sample(rnd.env$prob$raw_var.c2c.lags,1)
           V1 <- NULL
           V1$col <- 1
           V1$name <- paste("C2C",lag,"raw",sep="")
           V1$tier <- 2
           V1$requires <- "C"
           V1$ID <- 10*(100 + lag) + 1
           V1$type <- "Ret"
           V1$use <- "calc"
           V1$calc_cmn <- TRUE
           V1$math[1] <- paste("calc_look_forward,",lag,sep="")
         },
         "ti" = {
           d <- rnd_val("decay")
           if (d>=1) d <- 0.10
           type <- sample(rnd.env$prob$raw_var.ti.type,size=1,prob=rnd.env$prob$raw_var.ti.wts)
           print(paste("ti raw,type=",type,"d=",d))
           switch(type,
                  "adx" = {
                    for (raw in c('PDM','NDM','TR')) {
                      print(paste("adx,raw=",raw))
                      nam <- paste(raw,'d',100*d,sep="")
                      if (!(nam %in% names(com.env$v.com))) {
                        vs.nam <- ifelse(raw=='TR','TRraw','DM') 
                        V1 <- rnd.env$vs.com[[which(vs.nam == names(rnd.env$vs.com))]]
                        V1$name <- nam
                        V1$ID <- 100*(V1$ID+d)
                        if (raw == 'NDM') {
                          V1$ID <- V1$ID + 100
                          V1$math[1] <- "calc_dm,'YLLraw','YHHraw'"
                        }
                        V1$math[2] <- paste("calc_decay,decay=",d,sep="")
                        scom2vcom(V1)
                      }
                    }
                    pdm.nam <- paste('PDMd',100*d,sep="")
                    ndm.nam <- paste('NDMd',100*d,sep="")
                    tr.nam <- paste('TRd',100*d,sep="")
                    raw <- sample(c('PDI','NDI'),size=1)
                    #               for (raw in c('PDI','NDI')) {
                    nam <- paste(raw,'d',100*d,'raw',sep="")
                    #if (!(nam %in% names(com.env$v.com))) {
                    V1 <- rnd.env$vs.com[[which('DI' == names(rnd.env$vs.com))]]
                    V1$name <- nam
                    V1$ID <- 100*(V1$ID+d)
                    if (raw == 'PDI') {
                      V1$requires <- c(V1$requires,pdm.nam,tr.nam)
                      V1$math[1] <- paste("calc_math,c('",pdm.nam,"','",tr.nam,"'),'XX0N <- XX1/XX2'",sep="")
                      V1$ID <- 10*V1$ID + 1                     
                    } else { #NDI
                      V1$requires <- c(V1$requires,ndm.nam,tr.nam)
                      V1$math[1] <- paste("calc_math,c('",ndm.nam,"','",tr.nam,"'),'XX0N <- XX1/XX2'",sep="")
                      V1$ID <- 10*V1$ID + 2                     
                    }
                    #               }
                    #}
                  },
                  "mf" = {
                    raw <- sample(c('PMF','NMF'),size=1)
                    print(paste("mf,raw=",raw))
                    mf.nam <- paste(raw,'d',100*d,sep="")
                    if (!(mf.nam %in% names(com.env$v.com))) {
                      V1 <- rnd.env$vs.com[[which('MF' == names(rnd.env$vs.com))]]
                      V1$name <- mf.nam
                      V1$ID <- 100*(V1$ID+d)
                      if (raw == 'PMF') {
                        V1$ID <- 10*V1$ID + 100                     
                      } else { #NMF
                        V1$math[1] <- "calc_math,c('YTTraw','D'),math_str='XX0N <- ifelse(XX1<0,XX2,0)'"
                        V1$ID <- 10*V1$ID + 200                     
                      }
                      V1$math[2] <- paste("calc_decay,decay=",d,sep="")
                      scom2vcom(V1)
                    }
                    d.nam <- paste('Dd',100*d,sep="")
                    if (!(d.nam %in% names(com.env$v.com))) {
                      V1 <- rnd.env$vs.com[[which('D' == names(rnd.env$vs.com))]]
                      V1$name <- d.nam
                      V1$ID <- 100*(V1$ID+d)
                      V1$math[2] <- paste("calc_decay,decay=",d,sep="")
                      scom2vcom(V1)
                    }
                    nam <- paste(mf.nam,'raw',sep="")
                    V1 <- rnd.env$vs.com[[which('MF' == names(rnd.env$vs.com))]]
                    V1$name <- nam
                    V1$requires <- c(V1$requires,mf.nam,d.nam)
                    V1$tier <- V1$tier + 1
                    V1$ID <- 100*(V1$ID+d)
                    V1$ID <- ifelse(raw=='PMF',V1$ID+800,V1$ID+900)
                    V1$math[1] <- paste("calc_math,c('",mf.nam,"','",d.nam,"'),math_str='XX0N <- XX1/XX2'",sep="")
                  },
                  "fi" = {
                    fi.nam <- paste('FId',100*d,sep="")
                    print(paste("fi,fi.nam=",fi.nam))
                    if (!(fi.nam %in% names(com.env$v.com))) {
                      V1 <- rnd.env$vs.com[[which('MF' == names(rnd.env$vs.com))]]
                      V1$name <- fi.nam
                      V1$ID <- 100*(V1$ID+d)
                      V1$math[2] <- paste("calc_decay,decay=",d,sep="")
                      scom2vcom(V1)
                    }
                    d.nam <- paste('Dd',100*d,sep="")
                    if (!(d.nam %in% names(com.env$v.com))) {
                      V1 <- rnd.env$vs.com[[which('D' == names(rnd.env$vs.com))]]
                      V1$name <- d.nam
                      V1$ID <- 100*(V1$ID+d)
                      V1$math[2] <- paste("calc_decay,decay=",d,sep="")
                      scom2vcom(V1)
                    }
                    nam <- paste(fi.nam,'raw',sep="")
                    V1 <- rnd.env$vs.com[[which('FI' == names(rnd.env$vs.com))]]
                    V1$name <- nam
                    V1$requires <- c(V1$requires,fi.nam,d.nam)
                    V1$tier <- V1$tier + 1
                    V1$ID <- 100*(V1$ID+d+9)
                    V1$math[1] <- paste("calc_math,c('",fi.nam,"','",d.nam,"'),math_str='XX0N <- XX1/XX2'",sep="")
                  }
           )
         }
  )
  print(paste("select_rnd_raw",V1$name))
  #print(V1)
  scom2vcom(V1)                        #insert selected raw into vcom (if not already there)
  return(V1)
  #rnd.env$prob$raw_var <- c('retrange','ccd','c2c')
  #rnd.env$prob$raw_var.wts <- c(0.4,0.3,0.3)
  #rnd.env$prob$raw_var.c2c.lags <- c(3,5,8,13,21,34,55,89,144)
}

#place a variable and all of its required calcs in v.com
rnd_var <- function(var_type='model') {
  #print(paste("var_type=",var_type))
  if (var_type == 'model') {
    loop_list <- rnd.env$prob$choices
  } else if (var_type == 'bin') {
    loop_list <- rnd.env$prob$choices.bv
  }
  for (choice in loop_list) {
    switch(choice,
      "type" = {
        t <- rnd_val(choice,var_type)
        switch(t,
          "ret" = {
            #raw <- sample(rnd.env$raw_list,1)          #select a random raw
            V1 <- select_rnd_raw()
            V1$use <- var_type
            V1$requires <- c(V1$requires,V1$name)
            V1$tier <- V1$tier + 1
            V1$math[1] <- paste("from.var.env,'",V1$name,"'",sep="")
            V1$name <- sub("raw","ret",V1$name)
            V1$calc_cmn <- TRUE
          },
          "res" = {
            #raw <- sample(rnd.env$raw_list,1)          #select a random raw
            V1 <- select_rnd_raw()
            #scom2vcom(V1$name)                        #insert selected raw into vcom (if not already there)
            V1$use <- var_type
            V1$requires <- c(V1$requires,V1$name)
            V1$tier <- V1$tier + 1
            V1$math[1] <- paste("calc_res,'",V1$name,"'",sep="")
            V1$name <- sub("raw","res",V1$name)
            V1$calc_cmn <- FALSE 
          },
          "vlt" = {
            #raw <- sample(rnd.env$raw_list,1)          #select a random raw
            V1 <- select_rnd_raw()
            #scom2vcom(V1$name)                        #insert selected raw into vcom (if not already there)
            V1$use <- var_type
            V1$requires <- c(V1$requires,V1$name)
            V1$tier <- V1$tier + 1
            V1$math[1] <- paste("calc_vlty,'",V1$name,"'",sep="")
            V1$name <- sub("raw","vlt",V1$name)
            V1$calc_cmn <- TRUE
          },
          "vol" = {
            V1 <- rnd.env$vs.com[[rnd.env$vol_raw]]
            scom2vcom(V1)                        #insert selected raw into vcom (if not already there)
            V1$use <- var_type
            V1$requires <- c(V1$requires,V1$name)
            V1$tier <- V1$tier + 1
            V1$calc_cmn <- TRUE
            V1$name <- "Dol"
          },
          "vrs" = { #volume residualized 
            V1 <- rnd.env$vs.com[[rnd.env$vol_raw]]
            scom2vcom(V1)                        #insert selected raw into vcom (if not already there)
            V1$use <- var_type
            V1$requires <- c(V1$requires,V1$name)
            V1$math[2] <- paste("calc_res,'",V1$name,"'",sep="")
            V1$tier <- V1$tier + 1
            V1$calc_cmn <- FALSE
            V1$ID <- V1$ID + 1
            V1$name <- "Drs"
          }
          )
      },
      "cap" = {
        c <- rnd_val(choice)
        switch(c,
          "abscap" = {
            p <- rnd_val(c)
            V1$math[length(V1$math)+1] <- paste("calc_cap,abscap=",p,sep="")
            },
          "cap_pct" = {
            p <- rnd_val(c)
            V1$math[length(V1$math)+1] <- paste("calc_cap,cap_pct=",p,sep="")
            },
          "zcap" = {
            p <- rnd_val(c)
            V1$math[length(V1$math)+1] <- paste("calc_cap,zcap=",p,sep="")
          })
      },
      "scale" = {
        if (t == "vol" | t == "vrs") next
        if (var_type == "model") {
          s <- rnd_val(choice)
        } else if (var_type == "bin") {
          s <- "zscale"
        }
        switch(s,
          "zscale" = {
            V1$math[length(V1$math)+1] <- "calc_z,ma=TRUE"     
          },
          "zscore" = {
            V1$math[length(V1$math)+1] <- "calc_z,ma=FALSE"     
          })
      },
      "decay" = {
        d <- rnd_val(choice)
        if (d >= 1) {  #lag
          V1$math[length(V1$math)+1] <- paste("calc_lag,lag=",d,sep="")
          if (d == 2) V1$name <- paste(V1$name,"L2",sep="")
        } else {       #decay
          V1$math[length(V1$math)+1] <- paste("calc_decay,decay=",d,sep="")
          V1$name <- paste(V1$name,"d",as.character(trunc(100*d)),sep="")
        }
      },
      "bin" = {
        b <- rnd_val(choice)
        b1 <- rnd_val("bin_pt1")
        b2 <- rnd_val("bin_pt2")
        if (b2 <= b1) next
        switch(b,
          "new_var" = {
            status <- rnd_var(var_type="bin")
            if (status == -1) return(status)
            V2 <- com.env$v.com[[length(com.env$v.com)]]
            V1$requires <- unique(c(V1$requires,V2$requires,V2$name))
            V1$tier <- max( V1$tier,(V2$tier+1) )
            V1$calc_cmn <- (V1$calc_cmn & V2$calc_cmn)
            V1$math[length(V1$math)+1] <- paste("calc_bin,bin_field='",V2$name,"',b1=",b1,",b2=",b2,sep="")
          },
          "existing_var" = {
            print("not coded yet")
            return(-1)
          })
      }
      )
  }
  binning <- FALSE
  if (choice == "bin") {
    if ((b == "new_var") & (b1 < b2)) {
      binning <- TRUE
      b12 <- paste(gsub("[^0-9]","",b1),gsub("[^0-9]","",b2),sep="")
      V1$ID <- paste(V1$ID,b12,sep="")
      n1 <- paste(V1$name,"b",b12,"l",sep="")
      n2 <- paste(V1$name,"b",b12,"h",sep="")
      n1 <- unique_name(n1,V1$ID)
      n2 <- unique_name(n2,V1$ID)
      if (n1 == -1 | n2 == -1) {
        V1$name <- -1
      } else {
        V1$name <- c(n1,n2)
      }
      vcom.name <- substr(n1,1,(nchar(n1)-1))
    }
  } 
  if (!binning) {
    V1$ID <- paste(V1$ID,get_id(V1$math),sep="")
    V1$name <- unique_name(V1$name,V1$ID)
    vcom.name <- V1$name
  }
  newvar.error <- FALSE
  if (length(V1$name) == 1) if (V1$name == -1) newvar.error <- TRUE
  if (newvar.error) {
    print(paste("warning:created identical variable",V1$name,V1$ID))
    return(-1)
  } else {
    cmd_string <- paste("com.env$v.com$",vcom.name," <- V1",sep="")
    if (var_type == "model" & verbose) print(cmd_string)
    #if (vcom.name == "CCret") print(V1)
    eval(parse(text=cmd_string))
    com.env$vcom_names <- c(com.env$vcom_names,V1$name)
    #if (var_type == "model") com.env$ind_names <- c(com.env$ind_names,V1$name)
    #if (var_type == "bin") com.env$bin_names <- c(com.env$bin_names,V1$name)
    return(0)
  }
}

# take vnames from regression and delete any com.env$v.com not used 
delete_vcom <- function(keep_list) {
  vcom.keep <- NULL  #list of v.com names to keep
  
  com.env$namelu
}