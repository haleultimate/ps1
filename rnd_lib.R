add_vd <- function(V1,vcom_num=NULL) {  #if var_num is null append V1 to v.com list, otherwise place it at var_num
  if (is.null(vcom_num)) {
    V1 <- set_name(V1)
    if (V1$var_name %in% names(com.env$v.com)) {
      return(com.env$v.com[[V1$var_name]])
      #print(paste("ERROR in add_vd,",V1$var_name," already in v.com"))
      #print(names(com.env$v.com))
      #print(V1)
      #source("close_session.R")
    }
    V1$vcom_num <- length(com.env$v.com) + 1
    #print(V1$var_name)
    cmd_string <- paste0("com.env$v.com$",V1$var_name," <- V1")
    #print(cmd_string)
    eval(parse(text=cmd_string))
  } else {
    V1$vcom_num <- vcom_num
    V1 <- set_name(V1,orig_name=V1$var_name)
    com.env$v.com[[vcom_num]] <- V1
    names(com.env$v.com)[vcom_num] <- V1$var_name
  }
  #print(paste("add_vd",V1$var_name,vcom_num))
  return(V1)
}

#returns vd$name, vd$var_name, and vd$ID given a variable's math.list
set_name = function(V1,orig_name=NULL,vdlist=NULL) {
  #print(paste("in set_name:",V1$math))
  #if (V1$use == "model") {
  #  if (!is.null(vdlist)) {
  #    print(names(vdlist))
  #    print(vdlist[[length(vdlist)]])
  #  }
  #  print(V1$math)
  #}
  if (is.null(V1)) {
    print("Warning:V1 is null, no name/ID given")
    return(V1)
  }
  if (is.null(vdlist)) {
    vdlist <- com.env$v.com
  } else {
    #print(names(vdlist))
    #print(paste("last var in vdlist:",vdlist[[length(vdlist)]]$ID,vdlist[[length(vdlist)]]$var_name))
    if (!is.null(orig_name)) print ("Warning: Orig_name affects com.env$v.com not vdlist")
  }
  #orig_name <- ifelse(mod,V1$var_name,NULL) 
  V1$var_name <- NULL
  V1$ID <- NULL
  #print(V1$ID)
  binning <- FALSE
  #i <- 0
  for (math_str in V1$math) {
    #i <- i + 1 
    #print(paste(i,"math:",math_str,"V1$var_name:",V1$var_name,"V1$ID:",V1$ID))
    namePart <- NULL
    IDPart <- NULL
    math <- strsplit(math_str,split=",")[[1]][1] #get element to first comma (function call)
    parms <- gsub("^[^,]*,","",math_str) #get everything after first comma (parameters) 
    switch(math, 
           "from.data.env" = {
             if (parms=="'Adjusted'") {
               V1$var_name <- "C"
             } else {
               V1$var_name <- gsub("'","",parms)
             }
             V1$ID <- which(V1$var_name == LETTERS)
             V1$ID <- formatC(V1$ID, width = 2, format = "d", flag = "0")
            },
           "calc_constant" = {
             #parms <- gsub(parms,".","")  #remove decimal if necessary
             namePart <- "i"
             IDPart <- paste0(which(namePart == letters) + 26,parms)
             #namePart <- paste0(namePart,parms)  #if we ever start using constants other than 1
           },
           "calc_look_forward" = {
             if(parms < 0){
               V1$var_name <- paste0("CClf", -as.numeric(parms), "p")
               V1$ID <- "9999"
               break
             } else {
               namePart <- paste0("C", parms)
               IDPart <- paste0("03",parms)
             }
           },
           "calc_dol" = {
             namePart <- "D"
             IDPart <- which(namePart == LETTERS)
             IDPart <- formatC(IDPart, width = 2, format = "d", flag = "0")
           },
           "calc_calc" =,
           "calc_ia" = {
             parm_list <- strsplit(parms,split=",")[[1]]
             #print(parm_list)
             switch(parm_list[1],
                    "'log'" = {
                      IDPart <- "95"
                    },
                    "'exp'" = {
                      IDPart <- "96"
                    },
                    "'pow'" = {
                      IDPart <- "97"
                    },
                    "'abs'" = {
                      IDPart <- "98"
                    },
                    "'mul'" = {
                      namePart <- "x"
                      IDPart <- which(namePart == letters) + 26
                    },
                    "'div'" = {
                      namePart <- "y"
                      IDPart <- which(namePart == letters) + 26
                    },
                    "'add'" = {
                      namePart <- "p"
                      IDPart <- which(namePart == letters) + 26
                    },
                    "'sub'" = {
                      namePart <- "q"
                      IDPart <- which(namePart == letters) + 26
                    },
                    "'rsh'" = {
                      namePart <- "e"
                      IDPart <- "31"
                    },
                    "'fth'" = {
                      namePart <- "f"
                      IDPart <- "32"
                    }
             )
             if (length(parm_list)==3) {
               sign <- as.numeric(strsplit(parm_list[3],split="=")[[1]][2])
               IDPart <- paste0(IDPart,(sign+1)) #-1 option for sign in pow,rsh,fth 
             }
             if (length(parm_list)>=2) {
               if (grepl("[[:alpha:]]", parm_list[2])) {   #get variable name and sum its ID
                 #print(paste("Variable name found in calc_calc",parm_list[2]))
                 newname <- gsub("'", "", parm_list[2])
                 #print(paste("calc_calc naming1:",namePart,newname))
                 #namePart <- paste0(namePart,newname)  #don't add in ia var name
                 if(grepl("E",newname)){
                   namePart <- paste0(namePart,"E")
                   IDPart <- paste0(IDPart,"05")
                 } else if(grepl("T",newname)){
                   namePart <- paste0(namePart,"T")
                   IDPart <- paste0(IDPart,"20")
                 } else if(grepl("D",parms)){
                   namePart <- paste0(namePart,"D")
                   IDPart <- paste0(IDPart,"04")
                 } else if(grepl("S",parms)){
                   namePart <- paste0(namePart,"S")
                   IDPart <- paste0(IDPart,"19")
                 } else{
                   namePart <- paste0(namePart,"W")
                   IDPart <- paste0(IDPart,"23")
                 }  
                 if (is.null(vdlist[[newname]]$ID)) {
                   print(paste("calc_ia,calc_calc,newname:",newname))
                   print(vdlist[[newname]])
                   print(V1)
                 }
                 IDPart <- paste0(IDPart, sumID(vdlist[[newname]]$ID,vdlist[[newname]]))
               } else if (parm_list[1] %in% c("log", "mul", "div", "add", "sub")) {
                 parm_list[2] <- gsub("[^0-9]", "", parm_list[2])
                 IDPart <- paste0(IDPart, parm_list[2])
               } else if (parm_list[1] == "pow") {
                 pow <- strsplit(parm_list[2],"=")[[1]][2]
                 IDPart <- paste0(IDPart, which(rnd.env$pow_list == pow))
               }
             }
             #print(paste("calc_calc naming:",namePart,IDPart,parms))
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
                      namePart <- "lD"
                      #IDPart <- rnd.env$nameId[[which(namePart == names(rnd.env$nameID))]]
                      IDPart <- "53"
                    },
                    "c('H','L','B'),'XX0N <- pmax(log(XX1/XX2),abs(log(XX1/XX3)),abs(log(XX2/XX3)))'" = {
                      namePart <- "tr"
                      #IDPart <- rnd.env$nameId[[which(namePart == names(rnd.env$nameID))]]
                      IDPart <- "54"
                    },
                    "c('DMd','TRd'),'XX0N <- XX1/XX2'" = {
                      namePart <- "di"
                      #IDPart <- rnd.env$nameId[[which(namePart == names(rnd.env$nameID))]]
                      IDPart <- "55"
                    },
                    "c('QR','D'),math_str='XX0N <- ifelse(XX1>0,XX2,0)'" = {
                      namePart <- "pmf"
                      #IDPart <- rnd.env$nameId[[which(namePart == names(rnd.env$nameID))]]
                      IDPart <- "56"
                    },
                    "c('QR','D'),math_str='XX0N <- ifelse(XX1<0,XX2,0)'" = {
                      namePart <- "nmf"
                      #IDPart <- rnd.env$nameId[[which(namePart == names(rnd.env$nameID))]]
                      IDPart <- "57"
                    },
                    "c('BC','D'),math_str='XX0N <- XX1*XX2'" = {
                      namePart <- "fi"
                      #IDPart <- rnd.env$nameId[[which(namePart == names(rnd.env$nameID))]]
                      IDPart <- "58"
                    },
                    {
                    #print(paste("ti",parms))
                    namelist <- strsplit(parms,split="'")[[1]]
                    namePart <- paste0(namelist[2],"X")
                    #print(com.env$v.com[[namelist[2]]]$ID)
                    if (is.null(com.env$v.com[[namelist[2]]]$ID)) print(V1)
                    IDPart <- paste0("24",sumID(vdlist[[namelist[2]]]$ID,vdlist[[namelist[2]]]))
                    }
             )
           },
           "calc_cap" = {
             parm_split <- strsplit(parms,"=")[[1]]
             switch(parm_split[1],
                    "cap_pct" = { IDPart <- paste0("1",which(as.numeric(rnd.env$cap_pct_list)==as.numeric(parm_split[2]))) },
                    "zcap" = { IDPart <- paste0("2",which(as.numeric(rnd.env$zcap_list)==as.numeric(parm_split[2]))) },
                    "abscap" = { IDPart <- paste0("3",gsub("[^0-9]", "", parm_split[2]))})
             if (nchar(IDPart)<2) print(paste(parm_split,IDPart))
             IDPart <- paste0("29",IDPart)
           },
           "calc_stk" = {
             namePart <- gsub("'","",parms)
             if (is.null(vdlist[[namePart]]$ID)) {
               print(paste("calc_stk:",namePart))
               print(vdlist[[namePart]]$ID)
               print(V1)
             }
             IDPart <- paste0("19",sumID(vdlist[[namePart]]$ID,vdlist[[namePart]]))
             namePart <- paste0(namePart,"S")
           },
           "calc_adj" = {
             namePart <- substr(parms,2,2) #adjList[which(parms == names(adjList))]
             IDPart <- which(namePart == LETTERS)
             IDPart <- formatC(IDPart, width = 2, format = "d", flag = "0")
           },
           "from.var.env" = {
             namePart <- gsub("'","",parms)
             #if (is.null(vdlist[[namePart]]$ID)) {
             #   print(V1)
             #   print(names(vdlist))
             #}
             if (nchar(namePart) > 1) {
               if (is.null(vdlist[[namePart]]$ID)) {
                 print(paste("fve, sumID:",vdlist[[namePart]]$ID,namePart))
                 print(vdlist[[namePart]])
                 print(V1)
               }
               IDPart <- sumID(vdlist[[namePart]]$ID,vdlist[[namePart]])
             }
           },
           "calc_ret" = {
             namePart <-  substr(parms,2,2)
             namePart <- paste0(namePart,substr(parms,6,6))
             IDPart <- which(substr(parms,2,2) == LETTERS)
             IDPart <- formatC(IDPart, width = 2, format = "d", flag = "0")
             IDPart <- paste0(IDPart, formatC(which(substr(parms,6,6) == LETTERS), width = 2, format = "d", flag = "0"))
           },
           "calc_lag" = {  #reserved for "def" vars, shouldn't lag 'D' [only 'V', log dollars, should be lagged] 
             #print(paste(V1$var_name,math_str))
             if(V1$var_name %in% LETTERS){
               if (V1$var_name == 'D') {
                 print("Dollar lagging not supported in set_name, should use decay,1")
                 source("close_session.R")
               }
               index <- which(V1$var_name == LETTERS)
               V1$var_name <- LETTERS[index-as.numeric(parms)]
               V1$ID <- which(V1$var_name == LETTERS)
               V1$ID <- formatC(V1$ID, width = 2, format = "d", flag = "0")
               break
             } else {
               namePart <- "l"
               namePart <- paste0(namePart,parms)
               IDPart <- paste0("38",parms)
             }
           },
           "calc_etf" = {
             namePart <- gsub("'","",parms)
             if (is.null(vdlist[[namePart]]$ID)) {
               print(paste("calc_etf,namePart",namePart))
               print(vdlist[[namePart]])
               print(V1)
             }
             IDPart <- paste0("05",sumID(vdlist[[namePart]]$ID,vdlist[[namePart]]))
             namePart <- paste0(namePart,"E")
           },
           "calc_decay" = {
             #numPart <- as.numeric(unlist(regmatches(parms,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",parms))))
             #print(paste("calc_decay naming:",parms,numPart))
             numPart <- get_decay_from_math(math_str)
             if (numPart < 1) {
               #namePart <- substr(gsub("[^0-9]","",numPart),2,3)  #remove decimal
               namePart <- which(as.numeric(numPart)==as.numeric(rnd.env$decay_list))
               IDPart <- paste0("30",namePart)
               namePart <- paste0("d",namePart)
             } else if (numPart > 2) {
               IDPart <- paste0("27",numPart)
               namePart <- paste0("a",numPart)
             } else  {
               IDPart <- paste0("38",numPart)
               namePart <- paste0("l",numPart)
             }
             if (nchar(IDPart) < 3) print(paste("calc_decay [more than 2 digits]:",IDPart))
           },
           "calc_bin" = {
             binning <- TRUE
             # if(grepl("E",parms)){
             #   namePart <- "bE"
             #   IDPart <- "2805"
             # } else if(grepl("T",parms)){
             #   namePart <- "bT"
             #   IDPart <- "2820"
             # } else if(grepl("D",parms)){
             #   namePart <- "bD"
             #   IDPart <- "2804"
             # } else if(grepl("S",parms)){
             #   namePart <- "bS"
             #   IDpart <- "2819"
             # } else{
             #   namePart <- "bW"
             #   IDPart <- "2823"
             # }  
             parm_list <- strsplit(parms,split=",")[[1]]
             name <- gsub(".*=","",parm_list[1])
             name <- gsub("'","",name)
             b1 <- as.numeric(gsub(".*=","",parm_list[2]))
             b2 <- as.numeric(gsub(".*=","",parm_list[3]))
             scale_type <- substr(V1$scale_type,1,1)
             if (scale_type == "z") {
               st <- 1
               b11 <- which(as.numeric(rnd.env$bin_point.zlist)==as.numeric(b1))
               b12 <- which(as.numeric(rnd.env$bin_point.zlist)==as.numeric(b2))
             } else { #scale_type == "r"
               st <- 2
               b11 <- which(as.numeric(rnd.env$bin_point.rlist)==as.numeric(b1))
               b12 <- which(as.numeric(rnd.env$bin_point.rlist)==as.numeric(b2))
             }
             if (is.null(vdlist[[name]]$ID)) {
                   print(paste("calc_bin",name))
                   print(vdlist[[name]])
                   print(V1)
             }
             if (is.null(b11) | is.null(b12)) print(paste("b1=",b1,"b2=",b2,"st=",st,"b11=",b11,"b12=",b12))
             IDPart <- paste0(IDPart,"28",sumID(vdlist[[name]]$ID,vdlist[[name]]),st,b11,b12)
             namePart <- paste0("b",name)
           },
           "calc_vlty" = {  #doesn't support field and window
             parm_list <- strsplit(parms,",")[[1]]
             if (grepl("window",parms[1])) {
               vlty_idx <- which(paste0("v",strsplit(parms[1],"=")[[1]][2])==rnd.env$vlty_list)
               IDPart <- paste0("20",vlty_idx)
               namePart <- paste0("T",vlty_idx)
             } else {
               namePart <- gsub("'","",parm_list[1])
               #print(paste("calc_vlty",namePart))
               if (is.null(vdlist[[namePart]]$ID)) {
                 print(paste("calc_vlty",namePart))
                 print(vdlist[[namePart]])
                 print(V1)
               }
               IDPart <- paste0("20",sumID(vdlist[[namePart]]$ID,vdlist[[namePart]]))
               namePart <- paste0(namePart,"T")
             }
           },
           "calc_dm" = {
             namePart <- ifelse(substr(parms,2,2)=="G","pdm","ndm")
             #IDPart <- rnd.env$nameId[[which(namePart == names(rnd.env$nameID))]]
             IDPart <- ifelse(substr(parms,2,2)=="G","59","60")
           },
           "calc_z" = {
             if (grepl("TRUE",parms)) {  #Zscore "Z"
               namePart <- "Z"
               IDPart <- "52"
             } else {                    #Zscale "z"
               namePart <- "z"
               IDPart <- "26"
             }
           },
           "calc_rank" = {
             namePart <- "r"
             IDPart <- "18"              #rank "r"
           },
           print(paste("calc_function not found in set_name",math_str,math,parms))
    )
    if(V1$type == "ti") {
      #print(paste(namePart,IDPart,V1$var_name,V1$ID))
    }
    #print(paste(namePart,IDPart))
    V1$var_name <- paste0(V1$var_name,namePart)
    V1$ID <- paste0(V1$ID,IDPart)
    #print(paste(V1$var_name,V1$ID))
  }
  if (V1$use %in% c("raw","model","scale")) {
    #print(paste("model var, appending ID",V1$var_name))
    if (is.null(V1$ID)) print(V1)
    switch(V1$use,
           "model" = {V1$ID <- paste0(V1$ID,"39")},  # 'm'
           "scale" = {V1$ID <- paste0(V1$ID,"45")},  # 's'
           "raw"   = {V1$ID <- paste0(V1$ID,"49")})  # 'w'
    if (is.null(V1$ID)) {
      print("set_name (raw, model, or calc var)")
      print(V1)
    }
    letter_id <- substr(V1$use,1,1)
    if (letter_id=="r") letter_id <- "w"
    var_name <- paste0(V1$var_name,letter_id,sumID(V1$ID,V1))
    if (V1$use == "scale") {
      var_name <- gsub("w[[:digit:]]+","",var_name)     #shorten raw names by removing "w[sumID]"
      var_name <- gsub("l1","",var_name)                #remove l1 on raw vars
    }
    if (V1$use == "model") {
      var_name <- gsub("w[[:digit:]]+","",var_name)     #shorten raw names by removing "w[sumID]"
      var_name <- gsub("s[[:digit:]]+","",var_name)     #shorten scale names by removing "s[sumID]"
      var_name <- gsub("l1","",var_name)
      var_name <- gsub("Z","",var_name)
      var_name <- gsub("z","",var_name)
      var_name <- gsub("r","",var_name)                 #shorten model names by removing scaling and lag1
    }
    if (nchar(var_name) > 25) {
      print("*************************************************************")
      print(V1$math)
      print(var_name)
    }
    V1$var_name <- var_name
  }
  if (!binning) {
    V1$name <- V1$var_name
    V1$longID_name <- paste0("v",V1$ID)
  } else {
    V1$name[1] <- paste0(V1$var_name,"l")
    V1$name[2] <- paste0(V1$var_name,"h")
    V1$longID_name[1] <- paste0("v",V1$ID,"l")
    V1$longID_name[2] <- paste0("v",V1$ID,"h")
  }
  if (!is.null(orig_name)) {
    if (V1$var_name != orig_name) {
      if (is.null(V1$vcom_num)) {
        print(paste("WARNING:",orig_name,"changed to",V1$var_name,"with no vcom_num, replace_name not called"))
      } else {
        replace_name(V1$var_name,orig_name,V1$vcom_num)
      }
    }
  }
  #if (V1$var_name %in% names(com.env$v.com) & (V1$use %in% c("model","scale"))) {
  #  print(paste("*****************Warning in set_name, var_name already in com.env$v.com",V1$var_name))
    #print(names(com.env$v.com))
    #source("close_session.R")
  #}
  #print(paste("set_name return:",V1$var_name,V1$ID))
  return(V1)
}

replace_name <- function(name,orig_name,vcom_num) {
  if (is.null(vcom_num)) {
    print(paste("Warning: Can't replace name without vcom_num",name,orig_name))
    return()
  }
  if (name == orig_name) return()
  if (vcom_num == length(com.env$v.com)) return()
  exact_orig_name <- paste0("^",orig_name,"$")
  quoted_orig_name <- paste0("'",orig_name,"'")
  quoted_name <- paste0("'",name,"'")
  for (i in (vcom_num+1):length(com.env$v.com)) {
    orig_requires <- com.env$v.com[[i]]$requires
    orig_math <- com.env$v.com[[i]]$math
    com.env$v.com[[i]]$requires <- gsub(exact_orig_name,name,com.env$v.com[[i]]$requires)
    com.env$v.com[[i]]$math <- gsub(quoted_orig_name,quoted_name,com.env$v.com[[i]]$math)
    if (!identical(orig_requires,com.env$v.com[[i]]$requires)) {
      print(paste("replace_name:",name,orig_name,vcom_num))
      print(orig_requires)
      print(orig_math)
      print(com.env$v.com[[i]]$requires)
      print(com.env$v.com[[i]]$math)
    }
  }
}

sumID <- function(ID,V1=NULL){  #sum digits two at a time (so index 10 and 1 aren't identical)
  if (is.null(ID)) {
    print ("Error: V1 has no ID")
    print (V1)
    source("close_session.R")
  }
  total <- 0
  for (i in 1:nchar(ID)) total <- total + as.numeric(substring(ID,i,i))
#  if (nchar(ID) < 3) {
#    total <- as.numeric(ID) 
#  } else if (nchar(ID)%%3 == 0) {
#    for (i in 1:(nchar(ID)/3)) total <- total + as.numeric(substring(ID,(3*i-2),(3*i)))
#  } else if (nchar(ID)%%3 == 1) {
#    for (i in 1:trunc(nchar(ID)/3)) total <- total + as.numeric(substring(ID,(3*i-2),(3*i)))
#    total <- total + as.numeric(substring(ID,nchar(ID),nchar(ID)))                                                            
#  } else if (nchar(ID)%%3 == 2) {
#    for (i in 1:trunc(nchar(ID)/3)) total <- total + as.numeric(substring(ID,(3*i-2),(3*i)))
#    total <- total + as.numeric(substring(ID,nchar(ID)-1,nchar(ID)))                                                            
#  }
  return(total)
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
  #print(paste("scom2vcom",V1$var_name,V1$requires))
  if (!(V1$var_name %in% names(com.env$v.com))) {
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
    V1 <- add_vd(V1) #append V1 to vcom list
    #print(paste("scom2vcom, added",V1$var_name))
    com.env$vcom_names <- c(com.env$vcom_names,V1$var_name)
    if (!all(V1$requires %in% names(com.env$v.com))) {
      print(paste("ERROR",V1$var_name,"Missing required var in com.env$v.com in scom2vcom"))
      print(V1$requires)
      print(names(com.env$v.com))
    }
  } else {
    #print(paste("V1$name",V1$name," already in v.com"))
  }
  return(V1)
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

rnd_choice <- function(choice) {
  #print(paste('rnd_choice',choice))
  #cmd_string <- paste0("print(rnd.env$p$",choice,")")
  #eval(parse(text=cmd_string))
  cmd_string <- paste0("return(sample(names(rnd.env$p$",choice,"),size=1,prob=rnd.env$p$",choice,"))")
  #print(cmd_string)
  eval(parse(text=cmd_string))
}

#types of new_vars: {"raw","calc","bin","model"}
#new_var creates variable definition and appends it and all needed dependencies onto com.env$v.com list
new_var <- function(type,raw_type=NULL) {
  #print(paste("new_var",type,raw_type))
  switch(type,
         "raw" = {
           if (is.null(raw_type)) raw_type <- rnd_choice("raw")
           #print(paste("Create new raw var, raw_type",raw_type)) #FUTURE: allow existing raw to be chosen
           switch(raw_type,
                  "ret" = {
                    V1 <- rnd.env$vs.com[[sample(rnd.env$ret_list,1)]]
                  },
                  "BC" = {
                    V1 <- rnd.env$vs.com[[which("BC" == names(rnd.env$vs.com))]]
                  },
                  "V" = {
                    V1 <- rnd.env$vs.com[[which("lD" == names(rnd.env$vs.com))]]
                  },
                  "C2C" = {
                    V1 <- rnd.env$vs.com[[which("CC3" == names(rnd.env$vs.com))]]
                    lag <- rnd_choice("C2Clag")
                    V1$math <- sub("3",lag,V1$math)
                  },
                  "TI" =, #not supported yet
                  {
                  print(paste("Raw type not supported",raw_type))
                  source("close_session.R")
                  }
                  )
           V1$use <- "raw"
           V1 <- set_name(V1)
           #print(V1$var_name)
           V1 <- scom2vcom(V1)                        #insert selected raw and dependencies into vcom (if not already there)
           return(V1)
         },
         "calc" = { 
           #FUTURE: allow existing calc to be chosen
           #select raw
           raw_type <- rnd_choice("raw")
           V1 <- new_var("raw",raw_type=raw_type)
           #print(paste("Create new calc var",V1$var_name)) 
           V1$requires <- c(V1$requires,V1$var_name)
           V1$use <- "calc"
           calc_type <- rnd_choice(paste0(raw_type,".type"))
           if (calc_type == "T") { #if volatility calc_vlty, otherwise calc_decay
             V1$math <- paste0("calc_vlty,'",V1$var_name,"'")
             next_math <- 2
           } else { #select decay
             decay <- rnd_choice(paste0(raw_type,".d"))
             V1$math <- paste0("from.var.env,'",V1$var_name,"'")
             V1$math[2] <- paste0("calc_decay,",decay)
             next_math <- 3
             if (calc_type == "S") { #add V1 to v.com list (so ETF can be calculated), restart new V1 with calc_res
               V1 <- add_vd(V1)
               V1$requires <- c(V1$requires,V1$var_name)
               V1$calc_cmn <- FALSE
               V1$math <- paste0("calc_stk,'",V1$var_name,"'")
               if (V1$type == "ret") V1$type <- "stk"
               next_math <- 2
             }
           }
           #print(V1$math)
           #select cap
           cap_type <- rnd_choice("cap_type")
           if (cap_type != "none") {
             cap <- rnd_choice(cap_type)
             V1$math[next_math] <- paste0("calc_cap,",cap_type,"=",cap)
             next_math <- next_math + 1
           } 
           #select calc
           calc <- rnd_choice("calc")
           if (calc != "none") {
             if (calc == "pow") {
               pow <- rnd_choice("pow")
               V1$math[next_math] <- paste0("calc_calc,'pow',",pow)
             } else {
               V1$math[next_math] <- paste0("calc_calc,'",calc,"'")
             }
             next_math <- next_math + 1
           }
           #select scale
           scale <- rnd_choice("scale")
           #print(paste(scale,next_math))
           if (scale == "z") {
             V1$math[next_math] <- "calc_z,ma=FALSE"
             V1$scale_type <- "zscale"
           } else if (scale == "Z") {
             V1$math[next_math] <- "calc_z,ma=TRUE"
             V1$scale_type <- "zscore"
           } else if (scale == "r") {
             V1$math[next_math] <- "calc_rank,10"  #use deciles to approx rank
             V1$scale_type <- "rank"
           }
           V1 <- add_vd(V1)  #add calc var to v.com list
           if (calc_type == "E") {
             V1$requires <- c(V1$requires,V1$var_name)
             V1$calc_cmn <- FALSE
             V1$math <- paste0("calc_etf,'",V1$var_name,"'")
             if (V1$type == "ret") V1$type <- "etf"
             V1 <- add_vd(V1)
           }
           #print(V1$name)
           #print(V1$math)
           return(V1)
         },
         "bin" = {
           #only code if needed
         },
         "model" = {
           #get new calc_var   
           V1 <- new_var("calc")
           #print(paste("Create new model var",V1$var_name))
           V1$requires <- c(V1$requires,V1$var_name)
           V1$math <- paste0("from.var.env,'",V1$var_name,"'")
           #get interaction term
           interact <- rnd_choice("interaction")
           if (interact != "none") {  #FUTURE: add in sign choices, add in rnd weightings for add/sub
             V2 <- new_var("calc")
             V1$requires <- unique(c(V1$requires,V2$requires,V2$var_name))
             V1$math[2] <- paste0("calc_calc,'",interact,"','",V2$var_name,"'")
             V1$calc_cmn <- (V1$calc_cmn & V2$calc_cmn)
             next_math <- 3
           } else {
             next_math <- 2
           }
           #get bin term
           binning <- rnd_choice("bin")
           if (binning != "none") {  #FUTURE: add chance to reuse interaction var
             V2 <- new_var("calc") 
             V1$requires <- unique(c(V1$requires,V2$requires,V2$var_name))
             scale_type <- substr(V2$scale_type,1,1)
             #print(paste(V2$var_name,V2$math,scale_type))
             b1 <- rnd_choice(paste0("bin_points.",scale_type))
             b2 <- b1                  
             while(b1==b2) b2 <- rnd_choice(paste0("bin_points.",scale_type))
             bp1 <- min(b1,b2)
             bp2 <- max(b1,b2)
             V1$math[next_math] <- paste0("calc_bin,bin_field='",V2$var_name,"',b1=",bp1,",b2=",bp2)
             V1$calc_cmn <- (V1$calc_cmn & V2$calc_cmn)
             next_math <- next_math + 1
             #print(V1$math)
           }
           #FUTURE: add in decays (calc_bin_decay if binned, calc_decay if no binning)
           V1$use <- "model"
           V1 <- add_vd(V1)
           #print(paste("Final model variable added:",V1$var_name))
           print(V1$math)
           return(V1)
         }
         )
}

#select a raw var, add it and its dependencies to v.com (using scom2vcom)
select_rnd_raw <- function() {
  #print("select_rnd_raw")
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
                      #print(paste("adx,raw=",raw))
                      nam <- paste(raw,'d',100*d,sep="")
                      if (!(nam %in% names(com.env$v.com))) {
                        vs.nam <- ifelse(raw=='TR','tr','pdm') 
                        V1 <- rnd.env$vs.com[[which(vs.nam == names(rnd.env$vs.com))]]
                        #V1$name <- nam
                        #V1$ID <- 100*(V1$ID+d)
                        if (raw == 'NDM') {
                         # V1$ID <- V1$ID + 100
                          V1$math[1] <- "calc_dm,'KL','GH'"
                        }
                        V1$math[2] <- paste("calc_decay,decay=",d,sep="")
                        #V1$var_name <- NULL
                        V1 <- set_name(V1)
                        V1 <- scom2vcom(V1)
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
                      V1 <- rnd.env$vs.com[[which('pmf' == names(rnd.env$vs.com))]]
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
                      #V1$var_name <- NULL
                      V1 <- set_name(V1)
                      V1 <- scom2vcom(V1)
                      mf.nam <- V1$name
                    }
                    d.nam <- paste('Dd',100*d,sep="")
                    if (!(d.nam %in% names(com.env$v.com))) {
                      V1 <- rnd.env$vs.com[[which('D' == names(rnd.env$vs.com))]]
                     # V1$name <- d.nam
                      #V1$ID <- 100*(V1$ID+d)
                      V1$math[2] <- paste("calc_decay,decay=",d,sep="")
                      #V1$var_name <- NULL
                      V1 <- set_name(V1)
                      V1 <- scom2vcom(V1)
                      d.nam <- V1$name
                    }
                    nam <- paste(mf.nam,'raw',sep="")
                    V1 <- rnd.env$vs.com[[which('pmf' == names(rnd.env$vs.com))]]
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
                      V1 <- scom2vcom(V1)
                      fi.nam <- V1$name
                    }
                    d.nam <- paste('Dd',100*d,sep="")
                    if (!(d.nam %in% names(com.env$v.com))) {
                      V1 <- rnd.env$vs.com[[which('D' == names(rnd.env$vs.com))]]
                      #V1$name <- d.nam
                      #V1$ID <- 100*(V1$ID+d)
                      V1$math[2] <- paste("calc_decay,decay=",d,sep="")
                      #V1$var_name <- NULL
                      V1 <- set_name(V1)
                      V1 <- scom2vcom(V1)
                      d.nam <- V1$name
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
  V1 <- set_name(V1)
  V1 <- scom2vcom(V1)                        #insert selected raw into vcom (if not already there)
  if (is.null(V1$name) | is.null(V1$var_name) | is.null(V1$ID)) print(V1)
  return(V1)
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
            #V1$use <- var_type
            V1$requires <- c(V1$requires,V1$name)
            V1$tier <- V1$tier + 1
            V1$math[1] <- paste("from.var.env,'",V1$name,"'",sep="")
            V1$name <- sub("raw","ret",V1$name)
            V1$calc_cmn <- TRUE
          },
          "res" = {
            #raw <- sample(rnd.env$raw_list,1)          #select a random raw
            V1 <- select_rnd_raw()
            #V1$use <- var_type
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
            #V1$use <- var_type
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
            #V1$use <- var_type
            V1$requires <- c(V1$requires,V1$name)
            V1$tier <- V1$tier + 1
            V1$math[1] <- paste("calc_vlty,'",V1$name,"'",sep="")
            V1$name <- sub("raw","vlt",V1$name)
            V1$calc_cmn <- TRUE
          },
          "vol" = {
            V1 <- rnd.env$vs.com[[rnd.env$vol_raw]]
            V1 <- scom2vcom(V1)                        #insert selected raw into vcom (if not already there)
            #V1$use <- var_type
            #V1$requires <- c(V1$requires,V1$name)
            V1$tier <- V1$tier
            V1$calc_cmn <- TRUE
            V1$name <- "Dol"
          },
          "vrs" = { #volume residualized 
            V1 <- rnd.env$vs.com[[rnd.env$vol_raw]]
            V1 <- scom2vcom(V1)                        #insert selected raw into vcom (if not already there)
            #V1$use <- var_type
            V1$requires <- c(V1$requires,V1$name)
            V1$math[1] <- paste("calc_res,'",V1$name,"'",sep="")
            V1$tier <- V1$tier + 1
            V1$calc_cmn <- FALSE
            #V1$ID <- V1$ID + 1
            #V1$name <- "Drs"
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
          print("Warning: Can't use bin var to bin yet ******[trying to create]*****")
          print(var_type)
          print(choice)
          print(loop_list)
          print(V1$use)
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
              print("Warning: Can't use bin var to bin yet ********[new var created]*******")
              print(paste(V2$var_name,V2$use))
              return(-1)
            } else if (V2$use != "bin") {
              print(paste("Warning: Created bin var not of use 'bin'",V2$var_name,V2$use))
            }
            V1$requires <- unique(c(V1$requires,V2$requires,V2$var_name))
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
  V1 <- set_name(V1)  #needed to check if ID is unique
  if (V1$ID %in% com.env$ID_tried) {
    print(paste("warning:trying to create variable already tried",V1$name,V1$ID))
    if (com.env$verbose) print(com.env$ID_tried)
    return(-2)
  } else {
    com.env$ID_tried <- c(com.env$ID_tried,V1$ID)
    V1$use <- var_type
    V1 <- add_vd(V1)
    print(paste("Adding variable:",com.env$v.com[[length(com.env$v.com)]]$var_name))
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
    #print(vd$math)
    vd$vcom_num <- vcom_num
    for (m in 1:length(vd$math)) {
      math <- strsplit(vd$math[m],split=",")[[1]]
      if (math[1] %in% names(rnd.env$known_mod_fun)) math.list <- c(math.list,m)
    }
  }
  #print(math.list)
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
  orig_math_id <- vd$ID
  loop_num <- 0
  while ((vd$ID == orig_math_id) & (loop_num < 10)) {
    loop_num <- loop_num + 1
    math_num <- ifelse(length(math.list)==1,math.list,sample(math.list,size=1))
    orig_math <- vd$math[math_num]
    #print(paste(loop_num,math_num,orig_math))
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
                   print("Can't use bin as binning variable [in rnd_mod]")
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
    #orig_math_id <- get_id(com.env$v.com[[vcom_num]]$math)
    #current_math_id <- get_id(vd$math)
    vd <- set_name(vd)  #needed to check ID
  }
  #vd$ID <- sub(orig_math_id,current_math_id,vd$ID)
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
    print(paste(vd$var_name,"orig:",orig_math,"try:",vd$math[math_num]))
  } else {
    print(paste(vd$var_name,"orig:",orig_math,"try: delete math"))
  }
  #print(vd$math)
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
             vd_list$ID <- -1
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
  #current_math_id <- get_id(vd_list$math)
  if (com.env$verbose) print(paste("opt var",current_math_id))
  if (is.null(vd_list)) {
    print(paste("vd_list missing, orig:",orig_vd$math[i]))
    vd_list$ID <- -1
    return(vd_list)
  }
  #vd_list$ID <- sub(new_math_id,current_math_id,new_vd$ID)
  vd_list <- set_name(vd_list)   #needed to check ID
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
    #com.env$override_col <- com.env$mod_col
    #com.env$reg_names <- names(com.env$model.stepwise$coefficients)[-1]
    adj_r2 <- eval_adj_r2(vd=vd_list,orig_vd=orig_vd)
  }
  if (adj_r2 > new_adj_r2) {
    print(paste("optimize model improved",adj_r2,new_adj_r2))
    best_vd <- vd_list
    com.env$best_adj_r2 <- adj_r2
    #com.env$override_col <- com.env$mod_col
    #com.env$reg_names <- names(com.env$model.stepwise$coefficients)[-1]
    return(optimize_mod(new_vd,best_vd,new_adj_r2,com.env$best_adj_r2,try_num,improve=TRUE))
  } else {
    #print(paste("optimize model did not improve",adj_r2,new_adj_r2,improve,try_num))
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
      V1 <- set_name(rnd.env$VCOM[[i]])
      vname <- V1$var_name
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
      if (!match) {
        print(paste("Loading variable:",V1$var_name))
        add_vd(V1)
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
