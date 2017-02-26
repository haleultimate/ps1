#rnd_lib.R
fdeList <- c("C")
names(fdeList) <- c("'.Adjusted'")
adjList <- c("H", "L","O","V")
names(adjList) <- c("'High'", "'Low'","'Open'","'Volume'")
lagList <- c()

#returns vd$name and vd$ID given a variable's math.list
set_name = function(V1) {
  if(V1$type == "ti"){
    print("In set_name:")
    print(V1)
  }
  V1$var_name <- NULL
  V1$ID <- NULL
  namePart <- NULL
  IDPart <- NULL
  binning <- FALSE
  for (math_str in V1$math) {
    namePart <- NULL
    math <- strsplit(math_str,split=",")[[1]][1] #get element to first comma (function call)
    parms <- gsub("^[^,]*,","",math_str) #get everything after first comma (parameters) 
    switch(math, 
           "from.data.env" = {
             V1$var_name <- fdeList[which(parms == names(fdeList))]
             V1$ID <- which(V1$var_name == LETTERS)
             V1$ID <- formatC(V1$ID, width = 2, format = "d", flag = "0")
             break
           },
           "calc_look_forward" = {
             if(parms < 0){
               V1$var_name <- paste0("C2Clf", -as.numeric(parms), "p")
               V1$ID <- 9999
               break
             } else {
               namePart <- paste0("C2C", parms)
             }
           },
           "calc_dol" = {
             namePart <- "D"
             IDPart <- which(V1$var_name == LETTERS)
             IDPart <- formatC(IDPart, width = 2, format = "d", flag = "0")
           },
           "calc_math" = {
             switch(parms,
                    "c('H','L'),'XX0N <- sqrt(XX1*XX2)'" = {
                      namePart <- "J"
                      IDPart <- which(namePart == LETTERS)
                      IDPart <- formatC(IDPart, width = 2, format = "d", flag = "0")
                    }, 
                    "c('H','L','C'),'XX0N <- (XX1*XX2*XX3)^(1/3)'" = {
                      namePart <- "R"
                      IDPart <- which(namePart == LETTERS)
                      IDPart <- formatC(IDPart, width = 2, format = "d", flag = "0")
                    },
                    "c('D'),math_str='XX0N <- log(XX1) - 18.5'" = {
                      namePart <- "ld"
                      IDPart <- rnd.env$nameId[which(namePart == names(rnd.env$nameID))]
                    },
                    "c('H','L','B'),'XX0N <- pmax(log(XX1/XX2),abs(log(XX1/XX3)),abs(log(XX2/XX3)))'" = {
                      namePart <- "tr"
                      IDPart <- rnd.env$nameId[which(namePart == names(rnd.env$nameID))]
                    },
                    "c('DMd','TRd'),'XX0N <- XX1/XX2'" = {
                      namePart <- "di"
                      IDPart <- rnd.env$nameId[which(namePart == names(rnd.env$nameID))]
                    },
                    "c('QR','D'),math_str='XX0N <- ifelse(XX1>0,XX2,0)'" = {
                      namePart <- "dd"
                      IDPart <- rnd.env$nameId[which(namePart == names(rnd.env$nameID))]
                    },
                    "c('BC','D'),math_str='XX0N <- XX1*XX2'" = {
                      namePart <- "fi"
                      IDPart <- rnd.env$nameId[which(namePart == names(rnd.env$nameID))]
                    },
                    {
                    print(parms)
                    print(strsplit(parms,split = "'"))
                    namePart <- "ti"
                    IDPart <- rnd.env$nameId[which(namePart == names(rnd.env$nameID))]
                    }
             )
           },
           "calc_res" = {
             namePart <- "S"
             IDPart <- "19"
           },
           "calc_adj" = {
             namePart <- adjList[which(parms == names(adjList))]
             IDPart <- which(namePart == LETTERS)
           },
           "from.var.env" = {
             namePart <- gsub("'","",parms)
             if(nchar(namePart) > 1) IDPart <- sumID(com.env$v.com[parms]$ID)
           },
           "calc_ret" = {
             namePart <-  substr(parms,2,2)
             namePart <- paste0(namePart,substr(parms,6,6))
             IDPart <- which(substr(parms,2,2) == LETTERS)
             IDPart <- formatC(IDPart, width = 2, format = "d", flag = "0")
             IDPart <- paste0(IDPart, formatC(which(substr(parms,6,6) == LETTERS), width = 2, format = "d", flag = "0"))
           },
           "calc_lag" = {
             if(V1$var_name %in% LETTERS){
               index <- which(V1$var_name == LETTERS)
               V1$var_name <- LETTERS[index-as.numeric(parms)]
               V1$ID <- which(V1$var_name == LETTERS)
               V1$ID <- formatC(V1$ID, width = 2, format = "d", flag = "0")
               break
             } else {
               namePart <- "l"
               namePart <- paste0(namePart,parms)
               IDPart <- "38"
             }
           },
           "calc_cmn" = {
             namePart <- gsub("'","",parms)
             namePart <- paste0(namePart,"E")
             IDPart <- "05"
           },
           "calc_decay" = {
             namePart <- 100*as.numeric(strsplit(parms,split="0")[[1]][2])
             IDPart <- paste0("04",namePart)
             namePart <- paste0("d",namePart)
           },
           "calc_bin" = {
             binning <- TRUE
             if(grepl("E",parms)){
               namePart <- "be"
               IDPart <- "2831"
             } else if(grepl("T",parms)){
               namePart <- "bt"
               IDPart <- "2846"
             } else if(grepl("D",parms)){
               namePart <- "bd"
               IDPart <- "2830"
             } else if(grepl("S",parms)){
               namePart <- "bs"
               IDpart <- "2845"
             } else{
               namePart <- "bw"
               IDPart <- "2849"
             }
           },
           "calc_vlty" = {
             namePart <- gsub("'","",parms)
             namePart <- paste0(namePart,"T")
             IDPart <- "20"
           },
           "calc_dm" = {
             namePart <- ifelse(substr(parms,2,2)=="G","pdm","ndm")
             IDPart <- rnd.env$nameId[which(namePart == names(rnd.env$nameID))]
           },
           print("calc_function not found in set_name")
    )
    if(V1$type == "ti") {
      print(paste(namePart,IDPart,V1$var_name,V1$ID))
    }
    V1$var_name <- paste0(V1$var_name,namePart)
    V1$ID <- paste0(V1$ID,IDPart)
  }
  if (!binning) {
    V1$name <- V1$var_name
  } else {
    V1$name[1] <- paste0(V1$var_name,"l")
    V1$name[2] <- paste0(V1$var_name,"h")
  }
  if (V1$type == "ti") {
    print("return V1 from set_name")
    print(V1)
  }
  return(V1)
}

sumID <- function(ID){
  total <- 0
  for(i in 1:nchar(ID)) (total <- total + as.numeric(substring(ID,i,i)))
  return(total)
}

get_id <- function(math.list) {
  id <- NULL
  for (math_str in math.list) {
    math <- strsplit(math_str,split=",")[[1]][1] #get element to first comma (function call)
    fun_id <- which(math==names(rnd.env$fun_id))
    if (length(fun_id) > 0 & length(id) > 0) id <- paste(id,fun_id,sep="")
    if (length(fun_id) > 0 & length(id) == 0) id <- fun_id
    parms <- gsub("^[^,]*,","",math_str) #get everything after first comma (parameters)
    parms <- sub("TRUE","8",parms)
    parms <- sub("FALSE","9",parms)
    parms <- gsub("[^0-9]","",parms)     #remove all non-numeric characters
    if (length(parms) > 0 & length(id) > 0) id <- paste(id,parms,sep="")
    if (length(parms) > 0 & length(id) == 0) id <- parms
  }
  return(id)
}

check_ids <- function(id_list) {
  for (i in 1:length(id_list)) {
    if (gsub("[^0-9]","",id_list[i]) != id_list[i]) {
      print(paste("non-numeric character in id:",i,id_list[i]))
      return(-1)
      print(names(com.env$v.com))
      for (i in 1:length(com.env$v.com)) {
        print(paste(i,com.env$v.com[[i]]$ID))
        if (gsub("[^0-9]","",com.env$v.com[[i]]$ID) != com.env$v.com[[i]]$ID) {
          print(com.env$v.com[[i]])
        }
      }
    }
  }
  return(0)
}

#insert sample var into v.com given its name
scom2vcom <- function(V1) {
  #print(paste("scom2vcom",V1$name,V1$requires))
  if (!(V1$name %in% names(com.env$v.com))) {
    if (length(V1$requires) > 0) {
      for (i in 1:length(V1$requires)) {
        if (V1$requires[i] %in% names(rnd.env$vs.com)) {
          n <- which(names(rnd.env$vs.com) == V1$requires[i])
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
    } else {
      #print(paste("no requirements in",V1$name,"requires:",V1$requires))
    }
    cmd_string <- paste("com.env$v.com$",V1$name," <- V1",sep="")
    #print(paste("scom2vcom",cmd_string))
    eval(parse(text=cmd_string))
    com.env$vcom_names <- c(com.env$vcom_names,V1$name)
    if (!all(V1$requires %in% names(com.env$v.com))) {
      print(paste("ERROR",V1$name,"Missing required var in com.env$v.com in scom2vcom"))
      print(V1$requires)
      print(names(com.env$v.com))
    }
  } else {
    #print(paste("V1$name",V1$name," already in v.com"))
  }
} 

rnd_val <- function(choice,type="model") {
  #print(paste("type",type,"choice",choice))
  if (type == "model") {
    cmd_string <- paste("return(sample(rnd.env$prob$",choice,",size=1,prob=rnd.env$prob$",choice,".wts))",sep="")
  } else if (type == "bin") {
    cmd_string <- paste("return(sample(rnd.env$prob$",choice,".bv,size=1,prob=rnd.env$prob$",choice,".bv.wts))",sep="")
  }
  eval(parse(text=cmd_string))
}

unique_name <- function(name,id,first=TRUE) {
  #print(paste("name=",name,"id=",id,"first=",first))
  old_id <-  NULL
  if (name %in% com.env$vcom_names) {
    for (i in 1:length(com.env$v.com)) {
      if (name %in% com.env$v.com[[i]]$name) {
        old_id <- com.env$v.com[[i]]$ID
        break
      }
    }  
    #print(paste("id",id,"old_id",old_id,which(name==com.env$vcom_names),com.env$v.com[[11]]$ID))
    if (is.null(old_id)) return(-1)
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
           #print(paste("retrange",V1$name,"requires",V1$requires))
         },
         "ccd" = {
           V1 <- rnd.env$vs.com[[which("BC" == names(rnd.env$vs.com))]]
           d <- rnd_val("decay")
           if (d>=1) d <- 0.10
           V1$math[2] <- paste('calc_decay,decay=',d,sep="")
           #V1$name <- paste('CCd',100*d,"raw",sep="")
           #V1$ID <- 100*(V1$ID + d) 
         },
         "c2c" = {
           lag <- sample(rnd.env$prob$raw_var.c2c.lags,1)
           V1 <- NULL
           V1$col <- 1
           #V1$name <- paste("C2C",lag,"raw",sep="")
           V1$tier <- 2
           V1$requires <- "C"
           #V1$ID <- 10*(100 + lag) + 1
           V1$type <- "Ret"
           V1$use <- "calc"
           V1$calc_cmn <- TRUE
           V1$math[1] <- paste("calc_look_forward,",lag,sep="")
         },
         "ti" = {
           d <- rnd_val("decay")
           if (d>=1) d <- 0.10
           type <- sample(rnd.env$prob$raw_var.ti.type,size=1,prob=rnd.env$prob$raw_var.ti.wts)
           #print(paste("ti raw,type=",type,"d=",d))
           switch(type,
                  "adx" = {
                    for (raw in c('PDM','NDM','TR')) {
                      print(paste("adx,raw=",raw))
                      nam <- paste(raw,'d',100*d,sep="")
                      if (!(nam %in% names(com.env$v.com))) {
                        vs.nam <- ifelse(raw=='TR','tr','dm') 
                        V1 <- rnd.env$vs.com[[which(vs.nam == names(rnd.env$vs.com))]]
                        #V1$name <- nam
                        #V1$ID <- 100*(V1$ID+d)
                        if (raw == 'NDM') {
                         # V1$ID <- V1$ID + 100
                          V1$math[1] <- "calc_dm,'KL','GH'"
                        }
                        V1$math[2] <- paste("calc_decay,decay=",d,sep="")
                        V1 <- set_name(V1)
                        scom2vcom(V1)
                        switch(raw,
                               'PDM' = {
                                 pdm.nam <- V1$name
                               },
                               'NDM' = {
                                 ndm.nam <- V1$name
                               },
                               'TR' = {
                                 tr.nam <- V1$name
                               })
                      }
                    }
                    #pdm.nam <- paste('PDMd',100*d,sep="")
                    #ndm.nam <- paste('NDMd',100*d,sep="")
                    #tr.nam <- paste('TRd',100*d,sep="")
                    raw <- sample(c('PDI','NDI'),size=1)
                    #               for (raw in c('PDI','NDI')) {
                    #nam <- paste(raw,'d',100*d,'raw',sep="")
                    #if (!(nam %in% names(com.env$v.com))) {
                    V1 <- rnd.env$vs.com[[which('di' == names(rnd.env$vs.com))]]
                    #V1$name <- nam
                    #V1$ID <- 100*(V1$ID+d)
                    if (raw == 'PDI') {
                      V1$requires <- c(V1$requires,pdm.nam,tr.nam)
                      V1$math[1] <- paste("calc_math,c('",pdm.nam,"','",tr.nam,"'),'XX0N <- XX1/XX2'",sep="")
                      #V1$ID <- 10*V1$ID + 1                     
                    } else { #NDI
                      V1$requires <- c(V1$requires,ndm.nam,tr.nam)
                      V1$math[1] <- paste("calc_math,c('",ndm.nam,"','",tr.nam,"'),'XX0N <- XX1/XX2'",sep="")
                      #V1$ID <- 10*V1$ID + 2                     
                    }
                    #               }
                    #}
                  },
                  "mf" = {
                    raw <- sample(c('PMF','NMF'),size=1)
                    #print(paste("mf,raw=",raw))
                    mf.nam <- paste(raw,'d',100*d,sep="")
                    if (!(mf.nam %in% names(com.env$v.com))) {
                      V1 <- rnd.env$vs.com[[which('mf' == names(rnd.env$vs.com))]]
                      #V1$name <- mf.nam
                      #V1$ID <- 100*(V1$ID+d)
                      if (raw == 'PMF') {
                        #V1$ID <- 10*V1$ID + 100                     
                      } else { #NMF
                        V1$math[1] <- "calc_math,c('QR','D'),math_str='XX0N <- ifelse(XX1<0,XX2,0)'"
                        #V1$ID <- 10*V1$ID + 200                     
                      }
                      V1$math[2] <- paste("calc_decay,decay=",d,sep="")
                      #print(paste("mf",V1$name,"requires",V1$requires))
                      V1 <- set_name(V1)
                      mf.nam <- V1$name
                      scom2vcom(V1)
                    }
                    d.nam <- paste('Dd',100*d,sep="")
                    if (!(d.nam %in% names(com.env$v.com))) {
                      V1 <- rnd.env$vs.com[[which('D' == names(rnd.env$vs.com))]]
                     # V1$name <- d.nam
                      #V1$ID <- 100*(V1$ID+d)
                      V1$math[2] <- paste("calc_decay,decay=",d,sep="")
                      V1 <- set_name(V1)
                      d.nam <- V1$name
                      scom2vcom(V1)
                    }
                    nam <- paste(mf.nam,'raw',sep="")
                    V1 <- rnd.env$vs.com[[which('mf' == names(rnd.env$vs.com))]]
                    #V1$name <- nam
                    V1$requires <- c(V1$requires,mf.nam,d.nam)
                    V1$tier <- V1$tier + 1
                    #V1$ID <- 100*(V1$ID+d)
                    #V1$ID <- ifelse(raw=='PMF',V1$ID+800,V1$ID+900)
                    V1$math[1] <- paste("calc_math,c('",mf.nam,"','",d.nam,"'),math_str='XX0N <- XX1/XX2'",sep="")
                  },
                  "fi" = {
                    fi.nam <- paste('FId',100*d,sep="")
                    #print(paste("fi,fi.nam=",fi.nam))
                    if (!(fi.nam %in% names(com.env$v.com))) {
                      V1 <- rnd.env$vs.com[[which('fi' == names(rnd.env$vs.com))]]
                      #V1$name <- fi.nam
                      #V1$ID <- 100*(V1$ID+d)
                      V1$math[2] <- paste("calc_decay,decay=",d,sep="")
                      V1 <- set_name(V1)
                      fi.nam <- V1$name
                      scom2vcom(V1)
                    }
                    d.nam <- paste('Dd',100*d,sep="")
                    if (!(d.nam %in% names(com.env$v.com))) {
                      V1 <- rnd.env$vs.com[[which('D' == names(rnd.env$vs.com))]]
                      #V1$name <- d.nam
                      #V1$ID <- 100*(V1$ID+d)
                      V1$math[2] <- paste("calc_decay,decay=",d,sep="")
                      V1 <- set_name(V1)
                      d.nam <- V1$name
                      scom2vcom(V1)
                    }
                    nam <- paste(fi.nam,'raw',sep="")
                    V1 <- rnd.env$vs.com[[which('fi' == names(rnd.env$vs.com))]]
                    #V1$name <- nam
                    V1$requires <- c(V1$requires,fi.nam,d.nam)
                    V1$tier <- V1$tier + 1
                    #V1$ID <- 100*(V1$ID+d+9)
                    V1$math[1] <- paste("calc_math,c('",fi.nam,"','",d.nam,"'),math_str='XX0N <- XX1/XX2'",sep="")
                  }
           )
         }
  )
  #print(paste("select_rnd_raw",V1$name))
  #print(V1)
  V1 <- set_name(V1)
  scom2vcom(V1)                        #insert selected raw into vcom (if not already there)
  #if (!all(V1$requires %in% names(com.env$v.com))) {
  #  print(paste("ERROR",V1$name,"Missing required var in com.env$v.com in select_rnd_raw"))
  #  print(V1$requires)
  #  print(names(com.env$v.com))
  #}
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
            V1$use <- var_type
            V1$requires <- c(V1$requires,V1$name)
            V1$tier <- V1$tier + 1
            V1$math[1] <- paste("calc_res,'",V1$name,"'",sep="")
            V1$name <- sub("raw","res",V1$name)
            V1$calc_cmn <- FALSE 
          },
          "cmn" = {
            #raw <- sample(rnd.env$raw_list,1)          #select a random raw
            V1 <- select_rnd_raw()
            if (!all(V1$requires %in% names(com.env$v.com))) {
              print(paste("ERROR",V1$name,"Missing required var in com.env$v.com"))
              print(V1$requires)
              print(com.env$v.com)
            }
            V1$use <- var_type
            V1$requires <- c(V1$requires,V1$name)
            V1$tier <- V1$tier + 1
            V1$math[1] <- paste("calc_cmn,'",V1$name,"'",sep="")
            V1$name <- sub("raw","cmn",V1$name)
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
            #V1$requires <- c(V1$requires,V1$name)
            V1$tier <- V1$tier
            V1$calc_cmn <- TRUE
            V1$name <- "Dol"
          },
          "vrs" = { #volume residualized 
            V1 <- rnd.env$vs.com[[rnd.env$vol_raw]]
            scom2vcom(V1)                        #insert selected raw into vcom (if not already there)
            V1$use <- var_type
            V1$requires <- c(V1$requires,V1$name)
            V1$math[1] <- paste("calc_res,'",V1$name,"'",sep="")
            V1$tier <- V1$tier + 1
            V1$calc_cmn <- FALSE
            V1$ID <- V1$ID + 1
            V1$name <- "Drs"
          },
          "file" = {
            status <- load_rnd_var()
            if (status==0) {
              return(0)
            } else {
              print("problem loading var from file")
              return(-1)
            }
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
            V1$math[length(V1$math)+1] <- "calc_z,ma=FALSE"     
          },
          "zscore" = {
            V1$math[length(V1$math)+1] <- "calc_z,ma=TRUE"     
          })
      },
      "decay" = {
        d <- rnd_val(choice)
        if (d >= 1) {  #lag
          V1$math[length(V1$math)+1] <- paste("calc_lag,",d,sep="")
          if (d == 2) V1$name <- paste(V1$name,"L2",sep="")
        } else {       #decay
          V1$math[length(V1$math)+1] <- paste("calc_decay,decay=",d,sep="")
          V1$name <- paste(V1$name,"d",as.character(trunc(100*d)),sep="")
        }
      },
      "bin" = {
        if (var_type=='bin') {
          print("Warning: Can't use bin var to bin yet **********************")
          print(var_type)
          print(choice)
          print(loop_list)
          stop()
        }
        b <- rnd_val(choice)
        b1 <- rnd_val("bin_pt1")
        b2 <- rnd_val("bin_pt2")
        if (b2 <= b1) next
        switch(b,
          "new_var" = {
            status <- rnd_var(var_type="bin")
            if (status == -1) return(status)
            V2 <- com.env$v.com[[length(com.env$v.com)]]
            if (length(V2$name)>1) {
              print("Warning: Can't use bin var to bin yet **********************")
              return(-1)
            }
            V1$requires <- unique(c(V1$requires,V2$requires,V2$name))
            V1$tier <- max( V1$tier,(V2$tier+1) )
            V1$calc_cmn <- (V1$calc_cmn & V2$calc_cmn)
            if (length(V1$math) <= 0) print(paste("V1$math length:",length(V1$math)))
            V1$math[length(V1$math)+1] <- paste("calc_bin,bin_field='",V2$name,"',b1=",b1,",b2=",b2,sep="")
            #print(paste("calc_bin,bin_field='",V2$name,"',b1=",b1,",b2=",b2,sep=""))
            #print(V1$math[length(V1$math)])
          },
          "existing_var" = {
            print("not coded yet")
            return(-1)
          })
      }
      )  #end choice switch
  } #end loop over choice list
  binning <- FALSE
  if (choice == "bin") {  #assumes last choice is binning
    if ((b == "new_var") & (b1 < b2)) {
      binning <- TRUE
      b12 <- paste(gsub("[^0-9]","",b1),gsub("[^0-9]","",b2),sep="")
      V1$ID <- paste(V1$ID,get_id(V1$math),sep="")      
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
  V1 <- set_name(V1)
  # if (!binning) {
  #   V1$ID <- paste(V1$ID,get_id(V1$math),sep="")  #loop over all math functions to create id
  #   V1$name <- unique_name(V1$name,V1$ID)
  #   vcom.name <- V1$name
  # }
  newvar.error <- FALSE
  if (length(V1$name) == 1) if (V1$name == -1) newvar.error <- TRUE
  if (newvar.error) {
    print(paste("warning:created identical variable",V1$name,V1$ID))
    return(-1)
  } else if (V1$ID %in% com.env$ID_tried) {
    print(paste("warning:trying to create variable already tried",V1$name,V1$ID))
    if (com.env$verbose) print(com.env$ID_tried)
    return(-2)
  } else {
    com.env$ID_tried <- c(com.env$ID_tried,V1$ID)
    cmd_string <- paste("com.env$v.com$",V1$var_name," <- V1",sep="")
    #if (var_type == "model") 
    #print(paste("rnd_var",cmd_string))
    #if (vcom.name == "CCret") print(V1)
    eval(parse(text=cmd_string))
    #com.env$vcom_names <- c(com.env$vcom_names,V1$name)
    #if (var_type == "model") com.env$ind_names <- c(com.env$ind_names,V1$name)
    #if (var_type == "bin") com.env$bin_names <- c(com.env$bin_names,V1$name)
    #if (!all(V1$requires %in% names(com.env$v.com))) {
    #  print(paste("ERROR",V1$name,"Missing required var in com.env$v.com in rnd_var"))
    #  print(V1$requires)
    #  print(names(com.env$v.com))
    #}
    print(paste("Adding variable:",V1$var_name))
    return(0)
  }
}

#mod_var returns a variable definition structure (vd) to try in place of an existing variable
#calling routine needs to trap for a return of -1 (unable to mod any variable)
#LEFT TO DO: need to figure out how to change names of variables, currently names are left as is
rnd_mod <- function(reg_names=NULL,vcom_num=0) {
  #print(paste("mod_var",vcom_num))
  vd <- NULL
  if (vcom_num==0 & is.null(reg_names)) {
    print("Error: mod_var called without vcom_num and without list of potential vars to mod")
    vd$ID <- -1
    return(vd)
  }
  math.list <- NULL
  loop_num <- 0
  while ((is.null(math.list) & loop_num < 10) | (loop_num < 1 & is.null(reg_names))) {
    loop_num <- loop_num + 1
    if (!is.null(reg_names)) {
      vcom_name <- sample(reg_names,size=1)
      vcom_name <- paste("^",vcom_name,"$",sep="")
      vcom_num <- grep(vcom_name,names(com.env$v.com))
      #print(paste("vcom_num=",vcom_num,vcom_name))
    }
    vd <- com.env$v.com[[vcom_num]]
    vd$vcom_num <- vcom_num
    for (m in 1:length(vd$math)) {
      math <- strsplit(vd$math[m],split=",")[[1]]
      if (math[1] %in% names(rnd.env$known_mod_fun)) math.list <- c(math.list,m)
    }
  }
  if (is.null(math.list)) {
    if (loop_num == 1) {
      print(paste("Error: given vcom_num,",vcom_num,", has no modifiable functions"))
    } else {
      print("Error: No modifiable regression variable found")
    }
    vd$ID <- -1
    return(vd)
  }
  current_math_id <- 0
  orig_math_id <- 0
  loop_num <- 0
  while ((current_math_id == orig_math_id) & (loop_num < 10)) {
    loop_num <- loop_num + 1
    math_num <- ifelse(length(math.list)==1,math.list,sample(math.list,size=1))
    orig_math <- vd$math[math_num]
    length_orig_math <- length(vd$math)
    math <- strsplit(vd$math[math_num],split=",")[[1]]
    switch(math[1],
           'calc_cap' = {
             c <- rnd_val("cap")
             switch(c,
                    "abscap" = {
                      p <- rnd_val(c)
                      vd$math[math_num] <- paste("calc_cap,abscap=",p,sep="")
                    },
                    "cap_pct" = {
                      p <- rnd_val(c)
                      vd$math[math_num] <- paste("calc_cap,cap_pct=",p,sep="")
                    },
                    "zcap" = {
                      p <- rnd_val(c)
                      vd$math[math_num] <- paste("calc_cap,zcap=",p,sep="")
                    },
                    "none" = {
                      vd$math <- vd$math[-math_num]
                    })
           },
           'calc_z' = {
             p <- rnd_val("scale")
             if (p == "none") {
               vd$math <- vd$math[-math_num]
             } else {
               vd$math[math_num] <- ifelse(grepl("TRUE",vd$math[math_num]),
                                           sub("TRUE","FALSE",vd$math[math_num]),
                                           sub("FALSE","TRUE",vd$math[math_num])) 
             }
           },
           'calc_decay' = {
             d <- rnd_val("decay")
             if (d >= 1) {  #lag
               vd$math[math_num] <- paste("calc_lag,",d,sep="")
               #if (d == 2) V1$name <- paste(V1$name,"L2",sep="")
             } else {       #decay
               vd$math[math_num] <- paste("calc_decay,decay=",d,sep="")
               #V1$name <- paste(V1$name,"d",as.character(trunc(100*d)),sep="")
             }
           },
           'calc_lag' = {
             d <- rnd_val("decay")
             if (d >= 1) {  #lag
               vd$math[math_num] <- paste("calc_lag,",d,sep="")
               #if (d == 2) V1$name <- paste(V1$name,"L2",sep="")
             } else {       #decay
               vd$math[math_num] <- paste("calc_decay,decay=",d,sep="")
               #V1$name <- paste(V1$name,"d",as.character(trunc(100*d)),sep="")
             }
             if (com.env$verbose) print(paste("calc_lag, decay:",d,vd$math[math_num]))
             },
            'calc_bin' = { 
               b <- rnd_val("mod_bin")
               if (b == "none") {
                 vd$math <- vd$math[-math_num]
                 vd$name <- sub("b.*$","",vd$name[1])
               } else {
                 bin_name <- gsub(".*=","",math[2])
                 if (substr(bin_name,nchar(bin_name),nchar(bin_name)) == "l") {
                   print("Can't use bin as binning variable")
                   print(bin_name)
                   print(orig_math)
                   stop()
                 }
                 b1 <- gsub(".*=","",math[3])
                 b2 <- gsub(".*=","",math[4])
                 b1_n <- which(b1==rnd.env$mod$bins)
                 b2_n <- which(b2==rnd.env$mod$bins)
                 new_b1_n <- b1_n
                 new_b2_n <- b2_n
                 while((new_b1_n==b1_n)&(new_b2_n==b2_n)) {
                   new_b1_n <- sample(1:(b2_n-1),size=1)
                   new_b2_n <- sample((new_b1_n+1):length(rnd.env$mod$bins),size=1)
                 }
                 new_b1 <- rnd.env$mod$bins[new_b1_n]
                 new_b2 <- rnd.env$mod$bins[new_b2_n]
                 vd$math[math_num] <- paste("calc_bin,bin_field=",bin_name,",b1=",new_b1,",b2=",new_b2,sep="")
                 if (com.env$verbose) print(vd$math[math_num])
               }
           }
    )
    orig_math_id <- get_id(com.env$v.com[[vcom_num]]$math)
    current_math_id <- get_id(vd$math)
  }
  vd$ID <- sub(orig_math_id,current_math_id,vd$ID)
  if (vd$ID %in% com.env$ID_tried) {
    print(paste("warning:trying to mod a var already tried",vd$name,vd$ID))
    if (com.env$verbose) print(com.env$ID_tried)
    #vd$ID <- -1
    vd$ID <- -1
    return(vd)
  } else {
    com.env$ID_tried <- c(com.env$ID_tried,vd$ID)
  }
  if (length_orig_math == length(vd$math)) {
    #print(paste("orig:",orig_math,"try:",vd$math[math_num]))
  } else {
    #print(paste("orig:",orig_math,"try: delete math"))
  }
  vd <- set_name(vd)
  return(vd)
}

get_sample_idx <- function(orig_idx,new_idx,idx_size,direction) {
  sample_idx <- min(max(new_idx+direction,1),idx_size)
  if (com.env$verbose) print(paste("get_sample_idx",orig_idx,new_idx,idx_size,sample_idx,direction))
  return(sample_idx)
}

create_vd_list <- function(orig_vd,new_vd,try_num) {
  vd_list <- NULL
  if (length(orig_vd$math) != length(new_vd$math)) { #mod was a deletion or addition, nothing to optimize
    print(paste("mod was deletion, nothing to optimize",length(orig_vd$math),length(new_vd$math)))
    vd_list$ID <- -1
    return(vd_list)
  }
  i <- 1
  while ((orig_vd$math[i] == new_vd$math[i]) & (i <= length(orig_vd$math))) {
    i <- i + 1
  }
  if (i > length(orig_vd$math)) {
    print("Warning: All math the same in orig_vd and new_vd")
    print(orig_vd)
    print(new_vd)
    vd_list$ID <- -1
    return(vd_list)
  }
  if (com.env$verbose) print(paste(orig_vd$math[i],new_vd$math[i]))
  new_math_id <- get_id(new_vd$math)
  if (com.env$verbose) print(paste("new math id:",new_math_id,new_vd$ID))
  orig_math <- strsplit(orig_vd$math[i],split=",")[[1]]
  new_math <- strsplit(new_vd$math[i],split=",")[[1]]
  orig_parm <- strsplit(orig_math[2],split="=")[[1]]
  new_parm <- strsplit(new_math[2],split="=")[[1]]
  if (com.env$verbose) print(paste(orig_parm,new_parm,orig_math[1],"switch----------------"))
  direction <- 1
  switch(new_math[1],
         'calc_cap' = {
           if (try_num > 2) {
             vd_list$ID <- -1
             return(vd_list)
           }
           cmd_string <- paste("orig_idx <- which(orig_parm[2]==rnd.env$mod$",orig_parm[1],")",sep="")
           eval(parse(text=cmd_string))
           cmd_string <- paste("new_idx <- which(new_parm[2]==rnd.env$mod$",new_parm[1],")",sep="")
           eval(parse(text=cmd_string))
           cmd_string <- paste("idx_size <- length(rnd.env$mod$",new_parm[1],")",sep="")
           eval(parse(text=cmd_string))
           if ((orig_parm[2] < new_parm[2]) & (try_num == 1)) direction <- (-1) 
           sample_idx <- get_sample_idx(orig_idx,new_idx,idx_size,direction)
           vd_list <- new_vd
           cmd_string <- paste("parm <- rnd.env$mod$",new_parm[1],"[",sample_idx,"]",sep="")
           eval(parse(text=cmd_string))
           cmd_string <- paste("vd_list$math[",i,"] <- \"calc_cap,",new_parm[1],"=",parm,"\"",sep="")
           if (com.env$verbose) print(cmd_string)
           eval(parse(text=cmd_string))
           if ((vd_list$math[i] == orig_vd$math[i]) | (vd_list$math[i] == new_vd$math[i])) vd_list <- NULL
         },
         'calc_z' = {
           print("nothing to optimize in calc_z")
           vd_list$ID <- -1
           return(vd_list)
         },
         'calc_decay' = {
           if ((try_num > 2) | (orig_math[1]=='calc_lag')) {
             vd_list$ID <- -1
             return(vd_list)
           }
           orig_idx <- which(orig_parm[2]==rnd.env$mod$decay)
           new_idx <- which(new_parm[2]==rnd.env$mod$decay)
           idx_size <- length(rnd.env$mod$decay)
           if ((orig_parm[2] < new_parm[2]) & (try_num == 1)) direction <- (-1) 
           sample_idx <- get_sample_idx(orig_idx,new_idx,idx_size,direction)
           vd_list <- new_vd
           cmd_string <- paste("parm <- rnd.env$mod$decay[",sample_idx,"]",sep="")
           eval(parse(text=cmd_string))
           cmd_string <- paste("vd_list$math[",i,"] <- \"calc_decay,decay=",parm,"\"",sep="")
           if (com.env$verbose) print(cmd_string)
           eval(parse(text=cmd_string))
           if ((vd_list$math[i] == orig_vd$math[i]) | (vd_list$math[i] == new_vd$math[i])) vd_list <- NULL
         },
         'calc_lag' = {
           print("nothing to optimize in calc_lag")
           vd_list$ID <- -1
           return(vd_list)
         },
         'calc_bin' = { 
           if (try_num > 2) {
             vd_list$ID <- -1
             return(vd_list)
           }
           orig_b1 <- gsub(".*=","",orig_math[3])
           orig_b2 <- gsub(".*=","",orig_math[4])
           new_b1 <- gsub(".*=","",new_math[3])
           new_b2 <- gsub(".*=","",new_math[4])
           orig_idx_b1 <- which(orig_b1==rnd.env$mod$bins)
           orig_idx_b2 <- which(orig_b2==rnd.env$mod$bins)
           new_idx_b1 <- which(new_b1==rnd.env$mod$bins)
           new_idx_b2 <- which(new_b2==rnd.env$mod$bins)
           idx_size <- length(rnd.env$mod$bins)
           switch(try_num,
                  '1' = {
                    direction <- 1
                  },
                  '2' = {
                    direction <- (-1)
                  }
           )
           sample_idx_b1 <- get_sample_idx(orig_idx_b1,new_idx_b1,idx_size,direction)
           sample_idx_b2 <- get_sample_idx(orig_idx_b1,new_idx_b2,idx_size,direction)
           if (sample_idx_b1>=sample_idx_b2) {
             vd_list <- NULL
             return(vd_list)
           }
           vd_list <- new_vd
           cmd_string <- paste("parmb1 <- rnd.env$mod$bins[",sample_idx_b1,"]",sep="")
           eval(parse(text=cmd_string))
           cmd_string <- paste("parmb2 <- rnd.env$mod$bins[",sample_idx_b2,"]",sep="")
           eval(parse(text=cmd_string))
           cmd_string <- paste("vd_list$math[",i,"] <- \"calc_bin,",new_math[2],",b1=",parmb1,",b2=",parmb2,"\"",sep="")
           if (com.env$verbose) print(cmd_string)
           eval(parse(text=cmd_string))
           if ((vd_list$math[i] == orig_vd$math[i]) | (vd_list$math[i] == new_vd$math[i])) vd_list <- NULL
         }
  )
  current_math_id <- get_id(vd_list$math)
  if (com.env$verbose) print(paste("opt var",current_math_id))
  if (is.null(current_math_id)) {
    print(paste("current_math_id missing",vd_list))
    vd_list$ID <- -1
    return(vd_list)
  }
  vd_list$ID <- sub(new_math_id,current_math_id,new_vd$ID)
  if (vd_list$ID %in% com.env$ID_tried) {
    print(paste("warning:trying to optimize a var already tried",vd_list$name,vd_list$ID))
    if (com.env$verbose) print(com.env$ID_tried)
    vd_list$ID <- -1
    return(vd_list)
  } else {
    com.env$ID_tried <- c(com.env$ID_tried,vd_list$ID)
  }
  print(paste("orig:",orig_vd$math[i],"new:",new_vd$math[i],"try:",vd_list$math[i]))
  return(vd_list)
}

optimize_mod <- function(orig_vd,new_vd,orig_adj_r2,new_adj_r2,try_num=NULL,improve=FALSE) {
  if (new_adj_r2 <= orig_adj_r2) {
    print("error: new_vd not better than original vd")
    return(orig_vd)
  }
  if (is.null(try_num)) try_num <- 1
  vd_list <- create_vd_list(orig_vd,new_vd,try_num)
  if (com.env$verbose) print(paste("try_num,ID,improve:",try_num,vd_list$ID,improve))
  adj_r2 <- 0
  if (!is.null(vd_list)) { 
    if (vd_list$ID == -1) return(new_vd) 
    #print("evaluating new vd")
    #print(paste(vd_list$name,orig_vd$name))
    com.env$override_col <- com.env$mod_col
    com.env$reg_names <- names(com.env$model.stepwise$coefficients)[-1]
    adj_r2 <- eval_adj_r2(vd=vd_list,orig_vd=orig_vd)
  }
  if (adj_r2 > new_adj_r2) {
    print(paste("optimize model improved",adj_r2,new_adj_r2))
    best_vd <- vd_list
    com.env$best_adj_r2 <- adj_r2
    com.env$override_col <- com.env$mod_col
    com.env$reg_names <- names(com.env$model.stepwise$coefficients)[-1]
    return(optimize_mod(new_vd,best_vd,new_adj_r2,com.env$best_adj_r2,try_num,improve=TRUE))
  } else {
    print(paste("optimize model did not improve",adj_r2,new_adj_r2,improve,try_num))
    if (improve) return(new_vd)
    try_num <- try_num + 1
    return(optimize_mod(orig_vd,new_vd,orig_adj_r2,new_adj_r2,try_num))
  }
}


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

check_dependencies <- function() {
  for (i in 1:length(com.env$v.com)) {
    if (length(com.env$v.com[[i]]$requires) > 0) {
      for (var_name in com.env$v.com[[i]]$requires) {
        if (!(var_name %in% names(com.env$v.com)[1:(i-1)])) {
          print(paste("WARNING:",var_name,"does not come before",com.env$v.com[[i]]$name))
          return(FALSE)
        }
      }
    } 
  }
  return(TRUE)
}
