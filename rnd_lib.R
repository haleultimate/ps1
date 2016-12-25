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
scom2vcom <- function(vname) {
  #print(paste("vname=",vname))
  if (!(vname %in% com.env$vcom_names)) {
    #insert requirements in v.com
    n <- which(rnd.env$namelu == vname)
    #print(paste("n=",n))
    if (length(rnd.env$vs.com[[n]]$requires) > 0) {
      for (i in 1:length(rnd.env$vs.com[[n]]$requires)) {
        scom2vcom(rnd.env$vs.com[[n]]$requires[i])                 #recursive call
      }
    }
    cmd_string <- paste("com.env$v.com$",vname," <- rnd.env$vs.com[[",n,"]]",sep="")
    #print(paste("inserting",vname,"into v.com"))
    #print(cmd_string)
    eval(parse(text=cmd_string))
    com.env$vcom_names <- c(com.env$vcom_names,vname)
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
            raw <- sample(rnd.env$raw_list,1)          #select a random raw
            V1 <- rnd.env$vs.com[[raw]]
            scom2vcom(V1$name)                        #insert selected raw into vcom (if not already there)
            V1$use <- var_type
            V1$requires <- c(V1$requires,V1$name)
            V1$tier <- V1$tier + 1
            V1$math[1] <- paste("from.var.env,'",V1$name,"'",sep="")
            V1$name <- sub("raw","ret",V1$name)
            V1$calc_cmn <- TRUE
          },
          "res" = {
            raw <- sample(rnd.env$raw_list,1)          #select a random raw
            V1 <- rnd.env$vs.com[[raw]]
            scom2vcom(V1$name)                        #insert selected raw into vcom (if not already there)
            V1$use <- var_type
            V1$requires <- c(V1$requires,V1$name)
            V1$tier <- V1$tier + 1
            V1$math[1] <- paste("calc_res,'",V1$name,"'",sep="")
            V1$name <- sub("raw","res",V1$name)
            V1$calc_cmn <- FALSE 
          },
          "vlt" = {
            raw <- sample(rnd.env$raw_list,1)          #select a random raw
            V1 <- rnd.env$vs.com[[raw]]
            scom2vcom(V1$name)                        #insert selected raw into vcom (if not already there)
            V1$use <- var_type
            V1$requires <- c(V1$requires,V1$name)
            V1$tier <- V1$tier + 1
            V1$math[1] <- paste("calc_vlty,'",V1$name,"'",sep="")
            V1$name <- sub("raw","vlt",V1$name)
            V1$calc_cmn <- TRUE
          },
          "vol" = {
            V1 <- rnd.env$vs.com[[rnd.env$vol_raw]]
            scom2vcom(V1$name)                        #insert selected raw into vcom (if not already there)
            V1$use <- var_type
            V1$requires <- c(V1$requires,V1$name)
            V1$tier <- V1$tier + 1
            V1$calc_cmn <- TRUE
            V1$name <- "Dol"
          },
          "vrs" = {
            V1 <- rnd.env$vs.com[[rnd.env$vol_raw]]
            scom2vcom(V1$name)                        #insert selected raw into vcom (if not already there)
            V1$use <- var_type
            V1$requires <- c(V1$requires,V1$name)
            V1$math[3] <- paste("calc_res,'",V1$name,"'",sep="")
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
    if (var_type == "model") print(cmd_string)
    if (vcom.name == "CCret") print(V1)
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