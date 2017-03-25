#define_vars.R
if (com.env$verbose) print("calling define_vars.R")

com.env$v.com <- NULL
com.env$vcom_names <- NULL
#com.env$ind_names <- NULL
#com.env$bin_names <- NULL

V1 <- NULL
V1$col <- 1
V1$name <- "C"
V1$tier <- 1
V1$use <- "data"
V1$calc_cmn <- TRUE
V1$type <- "Price"
V1$math[1] <- "from.data.env,'.Adjusted'"
#print(V1$name)
add_vd(V1)
#V1 <- set_name(V1)
#cmd_string <- paste0("com.env$v.com$",V1$var_name," <- V1")
#eval(parse(text=cmd_string))
rm(V1)

V2 <- NULL
V2$col <- 2
V2$tier <- 2
V2$requires <- "C"
V2$type <- "Ret"
V2$use <- "model"
V2$calc_cmn <- TRUE
V2$math[1] <- paste0("calc_look_forward,-",com.env$look_forward)
V2$math[2] <- "calc_cap,abscap=0.05"
V2 <- add_vd(V2)
#V2 <- set_name(V2)
#cmd_string <- paste0("com.env$v.com$",V2$var_name," <- V2")
#eval(parse(text=cmd_string))
com.env$predict.ret <- V2$var_name #always second variable [hard coded when loading model]
rm(V2)

# V2 <- NULL
# V2$col <- 2
# V2$name <- "YC"
# V2$tier <- 2
# V2$requires <- "C"
# V2$ID <- 11
# V2$type <- "Price"
# V2$use <- "calc"
# V2$calc_cmn <- TRUE
# V2$math[1] <- "from.var.env,'C'"
# V2$math[2] <- "calc_lag,1"
# 
# com.env$v.com$YC <- V2
# com.env$vcom_names <- c(com.env$vcom_names,V2$name)
# rm(V2)
# 
# V3 <- NULL
# V3$col <- 3
# V3$name <- "CCraw"
# V3$tier <- 3
# V3$requires <- c("C","YC")
# V3$ID <- 1101
# V3$type <- "Ret"
# V3$use <- "calc"
# V3$calc_cmn <- TRUE
# V3$math[1] <- "calc_ret,'YC','C'"
# 
# com.env$v.com$CCret_raw <- V3
# com.env$vcom_names <- c(com.env$vcom_names,V3$name)
# rm(V3)
# 
# V4 <- NULL
# V4$col <- 4
# V4$name <- "CCretp"
# V4$tier <- 4
# V4$requires <- c("C","YC","CCraw")
# V4$ID <- 1101005
# V4$type <- "Ret"
# V4$use <- "model"
# V4$calc_cmn <- TRUE
# V4$math[1] <- "from.var.env,'CCraw'"
# V4$math[2] <- "calc_cap,abscap=0.05"
# 
# com.env$v.com$CCretp <- V4
# com.env$vcom_names <- c(com.env$vcom_names,V4$name)
# rm(V4)




#V5 <- NULL
#V5$col <- 5
#V5$name <- "YCCret"
#V5$tier <- 5
#V5$requires <- c("C","YC","CCret_raw","CCret")
#V5$ID <- 0.1101051
#V5$type <- "Ret"
#V5$use <- "model"
#V5$calc_cmn <- TRUE
#V5$math[1] <- "from.var.env,'CCret'"
#V5$math[2] <- "calc_lag,1"

#v.com$YCCret <- V5
#rm(V5)

#V6 <- NULL
#V6$col <- 6
#V6$name <- "YCCret_z"
#V6$tier <- 3
#V6$requires <- c("C","YC")
#V6$ID <- 0.11011059
#V6$type <- "Ret"
#V6$use <- "calc"
#V6$calc_cmn <- TRUE
#V6$math[1] <- "calc_math,c('C','YC'),'XX0N <- log(XX1/XX2)'"
#V6$math[2] <- "calc_lag,1"
#V6$math[3] <- "calc_cap,abscap=0.05"
#V6$math[4] <- "calc_z,ma=TRUE"

#v.com$CCret_raw2 <- V6
#rm(V6)

#V7 <- NULL
#V7$col <- 7
#V7$name <- "CCret_d3"
#V7$tier <- 3
#V7$requires <- c("C","YC")
#V7$ID <- 0.110130
#V7$type <- "Ret"
#V7$use <- "calc"
#V7$calc_cmn <- TRUE
#V7$math[1] <- "from.var.env,'CCret'"
#V7$math[2] <- "calc_decay,0.3"

#v.com$CCret_d3 <- V7
#rm(V7)

#V8 <- NULL
#V8$col <- 8
#V8$name <- "O"
#V8$tier <- 1
#V8$requires <- NULL
#V8$ID <- 0.2
#V8$type <- "Price"
#V8$use <- "calc"
#V8$calc_cmn <- TRUE
#V8$math[1] <- "calc_adj,'Open'"

#v.com$O <- V8
#rm(V8)

#V9 <- NULL
#V9$col <- 9
#V9$name <- "L"
#V9$tier <- 1
#V9$requires <- NULL
#V9$ID <- 0.3
#V9$type <- "Price"
#V9$use <- "calc"
#V9$calc_cmn <- TRUE
#V9$math[1] <- "calc_adj,'Low'"

#v.com$L <- V9
#rm(V9)

#V10 <- NULL
#V10$col <- 10
#V10$name <- "H"
#V10$tier <- 1
#V10$requires <- NULL
#V10$ID <- 0.4
#V10$type <- "Price"
#V10$use <- "calc"
#V10$calc_cmn <- TRUE
#V10$math[1] <- "calc_adj,'High'"

#v.com$H <- V10
#rm(V10)

#V11 <- NULL
#V11$col <- 11
#V11$name <- "CCret_vlt"
#V11$tier <- 6
#V11$requires <- c('C','YC','CCret_raw','CCret','YCCret')
#V11$ID <- 0.11010518
#V11$type <- "#Vlt"
#V11$use <- "model"
#V11$calc_cmn <- TRUE
#V11$math[1] <- "calc_vlty,'YCCret',window=120"

#v.com$CCret_vlt <- V11
#rm(V11)

#V12 <- NULL
#V12$col <- 12
#V12$name <- "AD#V"
#V12$tier <- 1
#V12$requires <- NULL
#V12$ID <- 0.930
#V12$type <- "#Vol"
#V12$use <- "calc"
#V12$calc_cmn <- TRUE
#V12$math[1] <- "calc_adv,window=30"

#v.com$ADV <- V12
#rm(V12)

#V13 <- NULL
#V13$col <- 13
#V13$name <- "YCCres"
#V13$tier <- 6
#V13$requires <- c('C','YC','CCret_raw','CCret','YCCret')
#V13$ID <- 0.101117
#V13$type <- "Ret"
#V13$use <- "model"
#V13$calc_cmn <- FALSE
#V13$math[1] <- "calc_res,'YCCret'"

#v.com$YCCres <- V13
#rm(V13)

#V14 <- NULL
#V14$col <- c(14,15)
#V14$name <- c("CCresbadvl","CCresbadvh")
#V14$tier <- 7
#V14$requires <- c('AD#V','C','YC','CCret_raw','CCret','YCCret')
#V14$ID <- 0.101117
#V14$type <- "Ret"
#V14$use <- "model"
#V14$calc_cmn <- FALSE
#V14$math[1] <- "calc_bin,'YCCres','ADV',-1,3"

#v.com$CCresbadv <- V14
#rm(V14)

#print("End of Define Vars")