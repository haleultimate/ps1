#make vdlist starting with first requires var and ending in V1
vcom2vdlist_req <- function(V1,env_lookup="com.env") {  #look in com.env$v.com, otherwise rnd.env$vs.com (for raws)
  #print(paste("vcom2vdlist_req",V1$var_name))
  vdlist <- NULL
  for (v in V1$requires) {
    if (env_lookup == "com.env") {
      cmd_string <- paste0("vdlist$",com.env$v.com[[v]]$var_name," <- com.env$v.com[['",v,"']]")
    } else if (env_lookup == "rnd.env") {
      cmd_string <- paste0("vdlist$",rnd.env$vs.com[[v]]$var_name," <- rnd.env$vs.com[['",v,"']]")
    } else {
      print(paste("ERROR: Wrong env_lookup in vcom2vdlist_req:",env_lookup))
    }
    #print(cmd_string)
    eval(parse(text=cmd_string))
  }
  cmd_string <- paste0("vdlist$",V1$var_name," <- V1")
  #print(cmd_string)
  eval(parse(text=cmd_string))
  #print(names(vdlist))
  return(vdlist)
}

#create list of all v.com vars of either "model" or "calc" use
vcom2vdlist_use <- function(use) {
  vdlist <- NULL
  for (vd in com.env$v.com) {
    if (vd$use == use) {
      cmd_string <- paste0("vdlist$",vd$var_name," <- vd")
      #print(cmd_string)
      eval(parse(text=cmd_string))
    }
  }
  return(vdlist)
}

#insert vdlist into com.env$v.com, do not duplicate vd already in v.com
vdlist2vcom <- function(vdlist,vcom_num=0) {  #append to end if no vcom_num given
  #print("vdlist2vcom, vcom_num=",vcom_num)
  #print(names(vdlist))
  if (is.null(com.env$v.com)) {
    vd <- vdlist[[1]]
    vd$vcom_num <- 1
    cmd_string <- paste0("com.env$v.com$",vd$var_name," <- vd")
    #print(cmd_string)
    eval(parse(text=cmd_string))
    vdlist <- vdlist[-1]
  }
  if (vcom_num == 0) vcom_num = length(com.env$v.com) + 1
  for (vd in vdlist) {
    if (vd$var_name %in% names(com.env$v.com)) next()
    vd$vcom_num <- vcom_num
    cmd_string <- paste0("com.env$v.com$",vd$var_name," <- vd")
    #print(cmd_string)
    eval(parse(text=cmd_string))
    vcom_num <- vcom_num + 1
  }
}

#return decay from calc_decay math string
#Assumes math form: "none", "calc_decay, decay" or "calc_decay, decay, var_cnt=cnt"
get_decay_from_math <- function(math) {
  print(paste("get_decay_from_math",math))
  if (math == "none") {
    return(1)
  } else {
    if (strsplit(math,split=",")[[1]][1] != "calc_decay") {
      print(paste("ERROR in get_decay_from_math, passed",math))
      source("close_session.R")
    }
    return(strsplit(math,split=",")[[1]][2])
  }
}

#produce math string from decay, default "calc_decay, decay"
# if math provided insert decay into first parameter (leaving var_cnt unchanged)
# if var_cnt (!= 1) provided produce "calc_decay, decay, var_cnt=cnt"
# if decay = 1 return "none"
get_math_from_decay <- function(decay,math=NULL,var_cnt=NULL) {
  print(paste("get_math_from_decay, decay=",decay,math,var_cnt))
  if (decay == 1) {
    math <- "none"
  } else if (is.null(math)) {
    if (is.null(var_cnt)) {
      math <- paste0("calc_decay,",decay)
    } else if (var_cnt == 1) {
      math <- paste0("calc_decay,",decay)
    } else {
      math <- paste0("calc_decay,",decay,",var_cnt=",var_cnt)
    }
  } else {
    if (strsplit(math,split=",")[[1]][1] != "calc_decay") {
      print(paste("ERROR in get_math_from_decay, passed",math))
      source("close_session.R")
    }
    math <- paste0("calc_decay,",decay,",",strsplit(math,split=",")[[1]][3])
  }
}

#return vdlist with last vd="raw" var [types generally derived from set rnd.env$vs.com sample vars]
get_raw_list <- function(raw_type = NULL) {
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
           V1 <- rnd.env$vs.com[[which("C3" == names(rnd.env$vs.com))]]
           lag <- rnd_choice("C2Clag")
           V1$math <- sub("3",lag,V1$math)
         },
         "TI" =, #not supported yet
         "existing" =, #not supported yet
         {
           print(paste("Raw type not supported",raw_type))
           source("close_session.R")
         }
  )
  V1$use <- "raw"
  #print(paste("call raw set_name",V1$math))
  V1 <- set_name(V1)
  #print(V1$math)
  return(vcom2vdlist_req(V1,env_lookup="rnd.env"))
}

#return vdlist with last vd="calc" var
#calc math in the form of "decay:calc:cap:scale","decay;get(stk/ETF):calc:cap:scale","vlty:calc:cap:scale"
get_calc_list <- function() {
  #FUTURE: allow existing calc to be chosen
  #select raw
  raw_type <- rnd_choice("raw")
  vdlist <- get_raw_list(raw_type=raw_type)
  V1 <- vdlist[[length(vdlist)]]
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
    V1$math[2] <- get_math_from_decay(decay)  #paste0("calc_decay,",decay)
    next_math <- 3
    if (calc_type %in% c("S","E")) { #add V1 to vdlist (so ETF can be calculated), restart new V1 with calc_stk or calc_etf
      #print(paste("call S/E set_name",V1$math))
      V1 <- set_name(V1,vdlist=vdlist)
      cmd_string <- paste0("vdlist$",V1$var_name," <- V1")
      #print(cmd_string)
      eval(parse(text=cmd_string))
      #vdlist <- append(vdlist,V1)
      #names(vdlist)[length(vdlist)] <- V1$var_name
      V1$requires <- c(V1$requires,V1$var_name)
      V1$calc_cmn <- FALSE
      V1$math <- ifelse(calc_type=="S",paste0("calc_stk,'",V1$var_name,"'"),paste0("calc_etf,'",V1$var_name,"'"))
      if (V1$type == "ret") V1$type <- ifelse(calc_type=="S","stk","etf")
      next_math <- 2
    }
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
  #select cap
  cap_type <- rnd_choice("cap_type")
  if (cap_type != "none") {
    cap <- rnd_choice(cap_type)
    V1$math[next_math] <- paste0("calc_cap,",cap_type,"=",cap)
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
  #print(paste("call calc set_name",V1$math))
  #for (vd in vdlist) if (is.null(vd$ID)) print(vd)
  V1 <- set_name(V1,vdlist=vdlist)
  cmd_string <- paste0("vdlist$",V1$var_name," <- V1")
  #print(cmd_string)
  eval(parse(text=cmd_string))
  #vdlist <- append(vdlist,V1)
  #names(vdlist)[length(vdlist)] <- V1$var_name
  #print(names(vdlist))
  #print(V1$math)
  return(vdlist)
}

#modify decay of model variable
mod_model_decay <- function(V1) {
  i <- which(grepl('calc_decay',V1$math))
  if (length(i) == 0) i = length(V1$math) + 1  #no decay in model var, add one randomly 
  d <- rnd_choice("model.d")
  if (d == "none") {
    V1$math <- V1$math[-i]
  } else {
    V1$math[i] = get_math_from_decay(d,var_cnt=length(V1$names))
    # paste0("calc_decay,",d)
    # if (length(V1$names) > 1) V1[i] <- paste0(V1$math[i],",var_cnt=",length(V1$names))  #decay multiple columns
  }
  return(V1)
}

#modify bin points of model variable [FUTURE: if no binning add binning]
mod_model_bin <- function(V1) {
  i <- which(grepl('calc_bin',V1$math))
  if (length(i) == 0) {
    print("ERROR: ADDING bin not supported yet")
    source("close_session.R")
  }
  math_list <- strsplit(V1$math[[i]],split=",")[[1]]  #bin points are items 3 and 4 
  if ((length(math_list) > 2) & (V1$scale_type != "constant")) { #either new var or binning constant, binning not to be deleted
    if (rnd_choice("delete_bin") == "delete") {
      V1$math <- V1$math[-i]
      return(V1)
    }
  }
  scale_type <- substr(V1$scale_type,1,1)
  b1 <- rnd_choice(paste0("bin_points.",scale_type))
  b2 <- b1                  
  while(b1==b2) b2 <- rnd_choice(paste0("bin_points.",scale_type)) #make sure bin_point parms have at least 2 choices (otherwise infinite loop)
  bp1 <- min(b1,b2)
  bp2 <- max(b1,b2)
  
  
  V1$math[[i]] <- paste(math_list[1],math_list[2],paste0("b1=",bp1),paste0("b2=",bp2),sep=",")
  
  return(V1)
}

#return a vdlist with last vd="model" var
#math in the form of "get_calc_var,interact,bin,bin_decay"
#interact in c("mul","div","add","sub","rsh","fth") with a chosen calc_var
#bin points chosen based on scaling of bin var (zscore or rank) [bin var = calc_var]
#FUTURE: Allow model vars to interact / bin
get_model_list <- function() {
  #print("In get_model_list")
  #get new calc_var
  vdlist <- NULL
  model_start <- rnd_choice("model_start")
  if (model_start == "calc_var") {
    calc_list <- vcom2vdlist_use("calc")
    if (runif(1,0,1)<length(calc_list)/(length(calc_list)+length(rnd.env$vs.com))) { #"existing_calc_var"
      V1 <- sample(calc_list,size=1)[[1]]
      vdlist <- vcom2vdlist_req(V1)
    } else {                                                                         #"new_calc_var"
      vdlist <- get_calc_list()
      V1 <- vdlist[[length(vdlist)]]
    }
    V1$requires <- c(V1$requires,V1$var_name)
    V1$math <- paste0("from.var.env,'",V1$var_name,"'")
  } else if (model_start == "constant") {                    #start with constant=1 (binning required)
    V1 <- rnd.env$vs.com[["i"]]
  } 
  #get interaction term
  interact <- ifelse(model_start=="constant","none",rnd_choice("interaction"))
  #print(interact)
  if (interact != "none") {  #FUTURE: add in sign choices, add in rnd weightings for add/sub
    vdlist2 <- get_calc_list()
    vdlist <- c(vdlist,vdlist2)     #may have replicated var definitions, will be managed in vdlist2vcom
    V2 <- vdlist[[length(vdlist)]]
    V1$requires <- unique(c(V1$requires,V2$requires,V2$var_name))
    V1$math[2] <- paste0("calc_ia,'",interact,"','",V2$var_name,"'")
    V1$calc_cmn <- (V1$calc_cmn & V2$calc_cmn)
    if ((V1$scale_type == "rank") & (V2$scale_type != "rank")) V1$scale_type <- "zscale"  #best guess for bin points
    next_math <- 3
  } else {
    next_math <- 2
  }
  #get bin term, if var is constant binning is mandatory
  binning <- ifelse(model_start=="constant","bin",rnd_choice("bin"))
  #print(paste("binning =",binning,"next_math=",next_math))
  if (binning != "none") {  #FUTURE: add chance to reuse interaction var
    vdlist2 <- get_calc_list()
    vdlist <- c(vdlist,vdlist2)
    V2 <- vdlist[[length(vdlist)]]
    V1$requires <- unique(c(V1$requires,V2$requires,V2$var_name))
    V1$scale_type <- V2$scale_type
    V1$calc_cmn <- (V1$calc_cmn & V2$calc_cmn)
    V1$math[next_math] <- paste0("calc_bin,bin_field='",V2$var_name,"'")
    V1 <- mod_model_bin(V1)  #append random bin points 
    #print(paste(V2$var_name,V2$math,scale_type))
    #b1 <- rnd_choice(paste0("bin_points.",scale_type))
    #b2 <- b1                  
    #while(b1==b2) b2 <- rnd_choice(paste0("bin_points.",scale_type)) #make sure bin_point parms have at least 2 choices (otherwise infinite loop)
    #bp1 <- min(b1,b2)
    #bp2 <- max(b1,b2)
    next_math <- next_math + 1
    #print(V1$math)
  }
  V1 <- mod_model_decay(V1)  #add in decay
  V1$use <- "model"
  V1 <- set_name(V1,vdlist=vdlist)
  cmd_string <- paste0("vdlist$",V1$var_name," <- V1")
  #print(cmd_string)
  eval(parse(text=cmd_string))
  #vdlist <- append(vdlist,V1)
  #names(vdlist)[length(vdlist)] <- V1$var_name
  #print(names(vdlist))
  #print(V1$math)
  return(vdlist)
}

make_new_model_var <- function() {
  #print("make_new_model_var")
  vdlist <- get_model_list()
  vdlist2vcom(vdlist)
  print(paste("make_new_model_var:",vdlist[[length(vdlist)]]$var_name))
  #print(names(com.env$v.com))
}

#takes in a calc vd and returns a mod_list [orig_vd, mod_vd]
#if V1 not provided a random one is selected from vcom list
mod_var_calc <- function(V1=NULL) {
  print(paste("mod_var_calc",V1$var_name,V1$use))
  if (is.null(V1)) {
    vdlist <- vcom2vdlist_use("calc")
    V1 <- sample(vdlist,size=1)
  }
  if (V1$use != "calc") {
    print(paste("ERROR: wrong use type in mod_var_model",V1$var_name,V1$use))
    soure("close_session.R")
  }
  calc <- (grepl("calc_calc",V1$math))
  vlty <- (grepl("calc_vlty",V1$math))
  decay_only <- (length(V1$math) == 1)
  
  
}

#takes in a model vd and returns a mod_list [orig_vd, mod_vd]
#if V1 is not provided a random one is selected from vcom list
mod_var_model <- function(V1=NULL) {
  #print(paste("mod_var_model",V1$var_name,V1$use))
  new_mod <- FALSE
  loops <- 0
  while ((!new_mod) & (loops<10)) {
    loops <- loops + 1
    if (is.null(V1)) {
      vdlist <- vcom2vdlist_use("model")
      vdlist <- vdlist[-1]  #remove predictor var
      V1 <- sample(vdlist,size=1)[[1]]
    }
    if (V1$use != "model") {
      print(paste("ERROR: wrong use type in mod_var_model",V1$var_name,V1$use))
      source("close_session.R")
    }
    
    interact <- any(grepl("calc_calc",V1$math))
    bin <- any(grepl("calc_bin",V1$math))
    if (interact & bin) {
      choice <- rnd_choice("int_bin")
    } else if (interact) {
      choice <- rnd_choice("int")
    } else if (bin) {
      choice <- rnd_choice("bin")
    } else {
      choice <- rnd_choice("fve")
    }
    switch(choice,
           "decay" = {
             V2 <- mod_model_decay(V1)
           },
           "bin" = {
             V2 <- mod_model_bin(V1)
           },
           "int" =,
           "fve" =,
           {
             print(paste("Error: choice not supported in mod_var_model",choice))
             source("close_session.R")
           }
    )
    V2 <- set_name(V2)
    new_mod <- (V1$ID != V2$ID)
  }
  
  if (!new_mod) {
    print(paste("Error: Could not modify model variable",V1$var_name))
    source("close_session.R")
  }
  print(V1$math)
  print(V2$math)
  modlist <- NULL
  cmd_string <- paste0("modlist$",V1$var_name," <- V1")
  eval(parse(text=cmd_string))
  cmd_string <- paste0("modlist$",V2$var_name," <- V2")
  eval(parse(text=cmd_string))
  return(modlist)
}

#randomly selects a vd from vcom list and modifies it
#returns a mod_list [orig_vd,mod_vd]
mod_var <- function(mod_use=NULL) {
  #print(paste("mod_var",mod_use))
  if (is.null(mod_use)) {
    var_use <- rnd_choice("mod_use")
  }
  if (mod_use == "model") {
    mod_list <- mod_var_model()
  } else if (mod_use == "calc") {
    mod_list <- mod_var_calc()    
  }
  return(mod_list)
}

#find opt_type from math_list[orig_math,mod_math]
#"none" is used to show addition/deletion in a math list
find_opt_type <- function(math_list) {
  print(paste("find_opt_type",math_list[1],math_list[2]))
  orig_math <- math_list[[1]]
  mod_math <- math_list[[2]]
  opt_type <- strsplit(orig_math,split=",")[[1]][1]  
  if (opt_type == "none") opt_type <- strsplit(mod_math,split=",")[[1]][1]  
  return(opt_type)
}

#move out 0.5X difference between i1 and i2
#must fall between 1 and max_i and not be equal to i1 or i2 [if not possible return 0]
move_out_index <- function(i1,i2,max_i,movement=0.5) {
  print(paste("move_out_index",i1,i2,max_i,movement))
  if (i1 == i2 | i2 == 1 | i2 == max_i | max_i < 3) return(0)
  if (i1 > i2) { #reducing index
    i3 <- i2 - ceiling(movement*(i1-i2))
  } else { #increasing index
    i3 <- i2 + ceiling(movement*(i2-i1))  
  }
  i3 <- min(max(i3,1),max_i)
  print(i3)
  return(i3)
}

#move in 0.5X difference between i1 and i2
#must not be equal to i1 or i2 [if not possible return 0]
move_in_index <- function(i1,i2,movement=0.5) {
  print(paste("move_out_index",i1,i2,movement))
  if (i1 == i2) return(0)
  if (i1 > i2) { #reducing index
    i3 <- i1 - ceiling(movement*(i1-i2))
  } else {       #increasing index
    i3 <- i1 + ceiling(movement*(i2-i1))  
  }
  print(i3)
  return(i3)
}


#opt_decay
#takes in math_list [orig_math,mod_math] and returns opt_math_list [mod_math,new_math]
#if no move, and more tries possible, new_math="try again", if no move and no tries left, new_math="opt"
#orig_math is calc_decay (or lag or ma)
#IF decay (parm in the range (0,1) or math=="none"):
#  decays are indexed by rnd.env$decay_list, with "none" representing decay=1 [added to decay list] 
#  try1: 1.5X as far from orig_decay as mod_decay
#  try2: midway from orig_decay and mod_decay
#IF lag (parm 1 or 2), no opt since only two options, return [mod_math,mod_math]
#IF ma (parm > 2):                                                               #not coded yet, ma doesn't work
#  ma windows indexed by rnd.env$ma_window_list, try_num same as if decay 
#Assumes math form: "calc_decay, parm" or "calc_decay, parm, var_cnt=cnt"
opt_decay <- function(math_list,try_num) {
  print(paste("opt_decay",math_list[1],math_list[2],try_num))
  orig_math <- math_list[[1]]
  mod_math <- math_list[[2]]
  orig_decay <- get_decay_from_math(orig_math)
  mod_decay <- get_decay_from_math(mod_math)
  
  # orig_type <- strsplit(orig_math,split=",")[[1]][1]  
  # if (orig_type == "none") {
  #   decay <- 1
  # } else {
  #   decay <- gsub("^[^,]*,","",orig_math)         #get everything after first comma (parameters), assumes no "decay=" 
  # }

  if (decay < 1) {
    decay_index_list <- c(1,rnd.env$decay_list)
    orig_index <- which(orig_decay==decay_index_list)
  #  mod_decay <- gsub("^[^,]*,","",mod_math)      #get everything after first comma (parameters), assumes no "decay="
    mod_index <- which(mod_decay==decay_index_list)
    if (try_num == 1) {  #  try1: 1.5X as far from orig_decay as mod_decay
      new_index <- move_out_index(orig_index,mod_index,length(decay_index_list))  #move_out_index should make 0.5 -> 1, -0.5 -> -1
      if (new_index == 0) {
        new_math <- "try again"
      } else {
        new_decay <- decay_index_list[new_index]
        new_math <- get_math_from_decay(new_decay,math=mod_math)
      }
    } else if (try_num == 2) { #  try2: midway from orig_decay and mod_decay (last try)
      new_index <- move_in_index(orig_index,mod_index)
      if (new_index == 0) {
        new_math <- "opt"
      } else {
        new_decay <- decay_index_list[new_index]
        new_math <- get_math_from_decay(new_decay,math=mod_math)
      }
    }
  } else { #lag or ma
    new_math <- "opt"  #no opt left
  }
  
  opt_math_list <- c(mod_math,new_math)
  print(paste("new_math:",new_math))
  return(opt_math_list)
}

#opt_cap
#takes in math_list [orig_math,mod_math] and returns opt_math_list [mod_math,new_math]
#if no move, and more tries possible, new_math="try again", if no move and no tries left, new_math="opt"
#orig_math is calc_cap, (cap_pct, zcap, abscap)
#IF cap_pct:
#  decays are indexed by rnd.env$cap_pct_list, with "none" representing cap_pct=0 [added to decay list] 
#  try1: 1.5X as far from orig_cap as mod_cap
#  try2: midway from orig_cap and mod_cap
#IF zcap:
#  decays are indexed by rnd.env$zcap_list, with "none" representing zcap=0 [added to decay list]
#  tries as given in cap_pct
#IF abscap:
#  return "opt"
#Assumes math form: "calc_cap,CAP_TYPE=parm"  where CAP_TYPE in (cap_pct,zcap,abscap)
opt_cap <- function(math_list,try_num) {
  print(paste("opt_cap",math_list[1],math_list[2],try_num))
  orig_math <- math_list[[1]]
  mod_math <- math_list[[2]]
  
  if (orig_math == "none") {
    cap_type <- strsplit(strsplit(mod_math,split=",")[[1]][2],split="=")[[1]][1] #inherit from mod_math
    orig_parm <- 0
  } else {
    cap_type <- strsplit(strsplit(orig_math,split=",")[[1]][2],split="=")[[1]][1] 
    orig_parm <- strsplit(strsplit(orig_math,split=",")[[1]][2],split="=")[[1]][2]
  }
  if (mod_math == "none") {
    mod_parm <- 0
  } else {
    mod_parm <- strsplit(strsplit(mod_math,split=",")[[1]][2],split="=")[[1]][2]
  }
  
  if (cap_type == "abscap") {
    return(c(mod_math,"opt"))
  }
  
  cap_index_list <- ifelse(cap_type=="cap_pct",c(0,rnd.env$cap_pct_list),c(0,rnd.env$zcap_list)) #else "zcap"
  orig_index <- which(mod_parm == cap_index_list)
  mod_index <- which(mod_parm == cap_index_list)
  if (try_num == 1) {
    new_index <- move_out_index(orig_index,mod_index,length(decay_index_list))
    if (new_index==0) return(c(mod_math,"try again"))
  } else if (try_num == 2) {
    new_index <- move_in_index(orig_index,mod_index)
    if (new_index==0) return(c(mod_math,"opt"))
  } else {
    return(c(mod_math,"opt"))
  }
  new_cap <- cap_index_list[new_index]
  new_math <- paste0("calc_cap,",cap_type,"=",new_cap)
  print(new_math)
  return(c(mod_math,new_math))
}
                  
#opt_bin - need to code


#compare_math takes in math pair (math1,math2) and determines difference type
#if identical returns "same"
#if sharing calc call returns single calc call (one of "calc_decay", "calc_cap", "calc_ia", "calc_bin", etc.)
#if either is "none" the other calc call is returned
#if different calc calls returns list of ["different",calc_call1,calc_call2]
compare_math_calc <- function(math_pair) {
  print("compare_math_calc",math_pair)
  if (math_pair[[1]] == math_pair[[2]]) return("same")
  math_type1 <- strsplit(math_pair[[1]],split=",")[[1]][1]
  math_type2 <- strsplit(math_pair[[2]],split=",")[[1]][1]
  if (math_type1 == "none") return(math_type2)
  if ((math_type1 == "none") | (math_type1 == math_type2)) return(math_type1)
  return(c("different",math_type1,math_type2))
}

#takes a mod_list [orig_vd,mod_vd] and returns a list of pairs of math differences [(orig_math1,mod_math1),(orig_math2,mod_math2),...]
#will insert "none" into the math list to show deleted ("none" in mod_math) or added ("none" in orig_math) functions
#if identical will return ("same","same")
get_vd_diff <- function(mod_list) {
  print(paste("get_vd_diff"))
  print(mod_list[[1]]$math)
  print(mod_list[[2]]$math)
  long_math_list <- NULL
  lml_idx <- 1
  orig_vd <- mod_list[[1]]
  mod_vd <- mod_list[[2]]
  orig_math_list <- orig_vd$math
  mod_math_list <- mod_vd$math
  
  if (orig_vd$use != "model" | mod_vd != "model") {
    print("Error: Non-model vd types not supported in get_vd_diff yet")
    print(orig_vd)
    print(mod_vd)
    source("close_session.R")
  }

  #model vars assumed to be in the form of ("from.var.env","calc_ia","calc_bin","calc_decay") any of the last three can be missing  
  orig_idx <- 1
  mod_idx <- 1
  loops <- 0
  while ((orig_idx <= length(orig_math_list)) | (mod_idx <= length(mod_math_list))) {
    loops <- loops + 1
    if (loops > 1000) {
      print("infinite loop in get vd_diff")
      print(mod_list)
      source("close_session.R")
    }
    if (mod_idx > length(mod_math_list)) {              #mod math ran out 
      math_pair <- c(orig_math_list[orig_idx],"none")
      orig_idx <- orig_idx + 1
    } else if (orig_idx > length(orig_math_list)) {     #orig math ran out
      math_pair <- c("none",mod_math_list[mod_idx])
      mod_idx <- mod_idx + 1
    } else {
      math_pair <- c(orig_math_list[orig_idx],mod_math_list[mod_idx])
      cmc <- compare_math_calc(math_pair)
      if (cmc[1] == "same") {
        math_pair <- NULL
        mod_idx <- mod_idx + 1
        orig_idx <- orig_idx + 1
      } else if (cmc[1] == "different") {
        if (((cmc[2]=="calc_ia") & ((cmc[3]=="calc_bin") | (cmc[3]=="calc_decay"))) |
            ((cmc[2]=="calc_bin") & (cmc[3]=="calc_decay"))) {                #no match in mod_math
          math_pair <- c(orig_math,"none")
          orig_idx <- orig_idx + 1
        } else if ((((cmc[2]=="calc_bin") | (cmc[2]=="calc_decay")) & (cmc[3]=="calc_ia")) | 
                   ((cmc[2]=="calc_decay") & (cmc[3]=="calc_bin"))) {         #no match in orig_math
          math_pair <- c("none",mod_math[mod_idx])
          mod_idx <- mod_idx + 1
        }
      } else {
        mod_idx <- mod_idx + 1
        orig_idx <- mod_idx + 1
      }
    }
    if (!is.null(math_pair)) {
      long_math_list[[lml_idx]] <- math_pair
      lml_idx <- lml_idx + 1
    }
  }
  if (is.null(long_math_list)) long_math_list[[1]] <- c("same","same")
  return(long_math_list)
}

#takes a mod_list [orig_vd,mod_vd] with the assumption that mod_vd performed better
#and returns an new_mod_list [mod_vd,new_vd] to attempt to improve further
#try_num gives the number of previous attempts, 
#if opt_math routine returns "opt" or no new math can be found, new_mod_list[mod_vd,"opt"] is returned
#if opt_math routine returns "try again" then new_mod_list[mod_vd,"try again"] is returned (opt_math called again with try_num incremented)
opt_math <- function(mod_list,try_num) {
  print(paste("opt_math, try:",try_num,mod_list))
  orig_vd <- mod_list[[1]]
  mod_vd <- mod_list[[2]]

  math_list <- get_vd_diff(mod_list)
  if (length(math_list) != 1) {
    print("more than one diff not supported yet")
    return(list(mod_vd,"opt"))
  }
  cmc <- compare_math_calc(math_list)
  
  switch(cmc,
         "calc_decay" = {
           opt_math_list <- opt_decay(math_list,try_num)
         },
         "calc_cap" = {
           opt_math_list <- opt_cap(math_list,try_num)
         },
         "calc_bin" =, 
#           {
#           opt_math_list <- opt_bin(math_list,try_num)
#         },
         "calc_bin_decay" =,
         "calc_scale" = ,
         "calc_calc" = ,
         "calc_interact" = ,
         "from.var.env" =,
         "calc_etf" =,
         "calc_stk" =,
         "calc_vlty" =,
         {
          print(paste("Warning: Opt_type:",opt_type," not supported (in function opt_var)"))
          return(list(mod_vd,"opt"))
         }
  )
  
  new_vd <- mod_vd
  if ((math_list[2] == "opt") | (math_list[2] == "try again")) {
    return(list(mod_vd,math_list[2]))
  } else if (math_list[2] == "none") {
    new_vd$math <- new_vd$math[-which(new_vd$math[grepl(cdc,new_vd$math)])] 
  } else {
    new_vd$math[which(new_vd$math[grepl(cdc,new_vd$math)])] <- math_list[2]
  }
  return(list(mod_vd,new_vd))
}

#function takes mod_list[orig_vd,mod_vd] and tests new vd's trying to find improvement
#opt_math creates new vd based on try_num
#eval_adj_r2 evaluates new_vd and if better places it into current com.env$v.com
#continue until "opt" is returned from opt_math
optimize_mod_list <- function(mod_list, best_adj_r2) {
  print("optimize_code")
  try_num <- 1
  opt_not_found <- TRUE
  loop <- 0
  while (opt_not_found) {
    loop <- loop + 1
    if (loop > 100) {
      print("infinite loop in optimize_mod_list")
      print(mod_list)
      source("close_session.R")
    }
    new_mod_list <- opt_math(mod_list,try_num)
    if (length(new_mod_list[[2]]) == 1) {
      if (new_mod_list[[2]] == "try again") {
        try_num <- try_num + 1
      } else if (new_mod_list[[2]] == "opt") {
        opt_not_found <- FALSE
      }
    } else {
      new_adj_r2 <- eval_adj_r2(new_mod_list,old_adj_r2=best_adj_r2)
      if (new_adj_r2 > best_adj_r2) {
        print(new_mod_list[[2]]$math)
        print(paste("opt model improved from: best_adj_r2:",best_adj_r2,"to:",new_adj_r2,"try:",try_num))
        mod_list <- new_mod_list
        best_adj_r2 <- new_adj_r2
      } else {
        print(new_mod_list[[2]]$math)
        print(paste("opt model did not improve: best_adj_r2:",best_adj_r2,"to:",new_adj_r2,"try:",try_num))
        try_num <- try_num + 1
      }
    }
  }
}
