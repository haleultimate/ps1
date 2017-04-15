#returns vd$name, vd$var_name, and vd$ID given a variable's math.list
set_name = function(V1,orig_name=NULL,vdlist=NULL) {
  if (is.null(V1)) {
    print("Warning:V1 is null, no name/ID given")
    return(V1)
  }
  if (is.null(vdlist)) {
    vdlist <- com.env$v.com
  } else {
    if (!is.null(orig_name)) print ("Warning: Orig_name affects com.env$v.com not vdlist")
  }
  V1$var_name <- NULL
  V1$ID <- NULL
  binning <- FALSE
  #i <- 0
  for (math_str in V1$math) {
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
                    "abscap" = { IDPart <- "308"})
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
             if (grepl("TRUE",parms)) {  #Zscore "Z"  ID's for Z,z,r chosen so they don't sum to same thing
               namePart <- "Z"
               IDPart <- "1"
             } else {                    #Zscale "z"
               namePart <- "z"
               IDPart <- "5"
             }
           },
           "calc_rank" = {
             namePart <- "r"
             IDPart <- "9"               #rank "r"
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
    if (V1$use == "raw") letter_id <- "w"
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
        #add_vd(V1)
            #MUST CHANGE THIS TO ADD variable and all of its dependencies
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
# add_vd <- function(V1,vcom_num=NULL) {  #if var_num is null append V1 to v.com list, otherwise place it at var_num
#   if (is.null(vcom_num)) {
#     V1 <- set_name(V1)
#     if (V1$var_name %in% names(com.env$v.com)) {
#       return(com.env$v.com[[V1$var_name]])
#       #print(paste("ERROR in add_vd,",V1$var_name," already in v.com"))
#       #print(names(com.env$v.com))
#       #print(V1)
#       #source("close_session.R")
#     }
#     V1$vcom_num <- length(com.env$v.com) + 1
#     #print(V1$var_name)
#     cmd_string <- paste0("com.env$v.com$",V1$var_name," <- V1")
#     #print(cmd_string)
#     eval(parse(text=cmd_string))
#   } else {
#     V1$vcom_num <- vcom_num
#     V1 <- set_name(V1,orig_name=V1$var_name)
#     com.env$v.com[[vcom_num]] <- V1
#     names(com.env$v.com)[vcom_num] <- V1$var_name
#   }
#   #print(paste("add_vd",V1$var_name,vcom_num))
#   return(V1)
# }


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

#Needed if creating model from scratch, first variable is dependent variable to predict
define_predict_ret <- function() {
  print("In define_vars")
  com.env$v.com <- NULL
  V1 <- NULL
  V1$requires <- NULL
  V1$type <- "ret"
  V1$use <- "model"
  V1$calc_cmn <- TRUE
  V1$math[1] <- paste0("calc_look_forward,-",com.env$look_forward)  #only calc function that can handle empty data.env
  V1$math[2] <- "calc_cap,abscap=0.05"
  V1 <- set_name(V1)
  V1$vcom_num <- 1
  cmd_string <- paste0("com.env$v.com$",V1$var_name," <- V1")
  eval(parse(text=cmd_string))
  #V1 <- add_vd(V1)
  com.env$predict.ret <- V1$var_name #always first variable [hard coded when loading model]
}

#set up rnd_parms.R [parms controlling how to randomly create and modify vars]
#set up sample vars in rnd.env$vs.com for import into com.env$v.com as needed
sample_vars <- function() {
  print("sample_vars.R")
  source("rnd_parms.R")          #set rnd_parms.R in rnd.env
  
  #set up sample vars, prices, returns, volumes
  #FUTURE: TI
  V1 <- NULL
  V1$use <- "def"
  V1$calc_cmn <- TRUE
  V1$type <- "prc"
  V1$math[1] <- "from.data.env,'Adjusted'"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "from.data.env,'H'"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "from.data.env,'L'"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "from.data.env,'R'"
  #V1$math[1] <- "calc_math,c('H','L','C'),'XX0N <- (XX1*XX2*XX3)^(1/3)'"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "from.data.env,'J'"
  #V1$math[1] <- "calc_math,c('H','L'),'XX0N <- sqrt(XX1*XX2)'"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "from.data.env,'O'"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "from.data.env,'Adjusted'"
  V1$math[2] <- "calc_lag,1"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "from.data.env,'H'"
  V1$math[2] <- "calc_lag,1"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "from.data.env,'L'"
  V1$math[2] <- "calc_lag,1"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "from.data.env,'R'"
  V1$math[2] <- "calc_lag,1"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "from.data.env,'J'"
  V1$math[2] <- "calc_lag,1"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "from.data.env,'O'"
  V1$math[2] <- "calc_lag,1"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "from.data.env,'Adjusted'"
  V1$math[2] <- "calc_lag,2"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  #set up returns from prices
  max_j <- length(rnd.env$vs.com)
  for (i in 1:6) {
    vd_end_price <- rnd.env$vs.com[[i]]
    for (j in (i+1):max_j) {
      V1 <- NULL
      V1$use <- "def"
      V1$calc_cmn <- TRUE
      vd_start_price <- rnd.env$vs.com[[j]]
      V1$type <- "ret"
      V1$requires <- unique(c(vd_end_price$requires,vd_start_price$requires,vd_start_price$name,vd_end_price$name))
      V1$math[1] <- paste0("calc_ret,'",vd_start_price$name,"','",vd_end_price$name,"'")
      V1$math[2] <- "calc_cap,abscap=0.08"     #hardcoded, may wish to revisit
      V1 <- set_name(V1)
      cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
      eval(parse(text=cmd_string))
    }
  }
  rm(vd_end_price,vd_start_price)
  rnd.env$ret_list <- (max_j+1):length(rnd.env$vs.com)
  
  #set up Dollar (D) and log Dollar (V) volume
  V1 <- NULL
  V1$requires <- NULL
  V1$type <- "vol"
  V1$use <- "def"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "from.data.env,'D'"
  V1$math[2] <- "calc_cap,cap_pct=0.005"       #hardcoded, may wish to revisit
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$requires <- NULL
  V1$type <- "vol"
  V1$use <- "def"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "from.data.env,'V'"
  #V1$math[1] <- "calc_math,c('D'),math_str='XX0N <- log(XX1) - 18.5'"
  V1$math[2] <- "calc_cap,cap_pct=0.005"       #hardcoded, may wish to revisit
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))

  #set up CC3 for use as template for CC# raws
  V1 <- NULL
  V1$col <- 1
  V1$requires <- NULL
  V1$type <- "ret"
  V1$use <- "def"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "calc_look_forward,3"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  #calculate true range for TI calcs
  V1 <- NULL
  V1$type <- "rng"
  V1$requires <- c('H','L','C','B')
  V1$use <- "raw"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "calc_math,c('H','L','B'),'XX0N <- pmax(log(XX1/XX2),abs(log(XX1/XX3)),abs(log(XX2/XX3)))'"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  rnd.env$raw_list <- c(rnd.env$raw_list,length(rnd.env$vs.com))
  
  V1 <- NULL
  #directional movement
  V1$type <- "ti"
  V1$requires <- c('H','G','GH','L','K','KL')
  V1$use <- "raw"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "calc_dm,'GH','KL'"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))

  V1 <- NULL
  #directional indicator
  V1$type <- "ti"
  V1$requires <- c('H','G','GH','L','K','KL','C','B')
  V1$use <- "raw"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "calc_math,c('DMd','TRd'),'XX0N <- XX1/XX2'" #DMd and TRd likely not correct. COME BACK
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))

  V1 <- NULL
  #money flow
  V1$type <- "ti"
  V1$requires <- c('H','L','C','R','Q','QR','D')
  V1$use <- "raw"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "calc_math,c('QR','D'),math_str='XX0N <- ifelse(XX1>0,XX2,0)'"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))

  V1 <- NULL
  V1$type <- "constant"
  V1$use <- "data"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "calc_constant,1"
  V1$scale_type <- "constant"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  #Force Index = return * Dollars
  V1$type <- "ti"
  V1$requires <- c('C','B','BC','D')
  V1$use <- "scale"
  V1$calc_cmn <- TRUE
  V1$math[1] <- "calc_math,c('BC','D'),math_str='XX0N <- XX1*XX2'"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
}

