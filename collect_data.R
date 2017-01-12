#collect_data.R
# Run through xts objects in var.env extracting data frame for each ticker and then bind them into one data frame for regression

#determine columns used in regression
vvars <- NULL
name2vcomnum <- NULL
allmodelvars <- NULL
for (i in 1:length(com.env$v.com)) {
  vvars[i] <- (com.env$v.com[[i]]$use == "model")
  if (vvars[i]) {
    if (length(com.env$v.com[[i]]$name) == 1) {
      name2vcomnum <- c(name2vcomnum,i)
      names(name2vcomnum)[length(name2vcomnum)] <- com.env$v.com[[i]]$name 
    } else {
      for (nam in com.env$v.com[[i]]$name) {
        name2vcomnum <- c(name2vcomnum,i)
        names(name2vcomnum)[length(name2vcomnum)] <- nam 
      }
    }
  }
}

vvars <- which(vvars)
for (i in vvars) {
  cn <- com.env$v.com[[i]]$col
  clist <- c(cn:(cn-1+length(com.env$v.com[[i]]$name)))
  allmodelvars <- append(allmodelvars,clist)
}

#get data for all stx into single data frame
reg_data.df <- NULL
OOS_data.df <- NULL
for (i in 1:stx) {
  ticker <- stx.symbols[i]
  cmn_ticker <- cmn_lookup[ticker]
  cmd_string <- paste("cmn_start_date <- index(data.env$",cmn_ticker,"[",com.env$days2remove,",])",sep="")
  eval(parse(text=cmd_string))
  cmd_string <- paste("stk_start_date <- index(data.env$",ticker,"[",com.env$days2remove,",])",sep="")
  eval(parse(text=cmd_string))
  max_start_date <- max(c(cmn_start_date,stk_start_date,com.env$reg_start_date))
  cmd_string <- paste("start_idx <- which(max_start_date == index(var.env$",ticker,"))",sep="")
  eval(parse(text=cmd_string))
  cmd_string <- paste("end_idx <- which(com.env$reg_end_date == index(var.env$",ticker,"))",sep="")
  eval(parse(text=cmd_string))
  subset_string <- paste("var.env$",ticker,"[",start_idx,":",end_idx,",allmodelvars]",sep="")
  cmd_string <- paste("reg_data.df <- bind_rows(reg_data.df,as.data.frame(",subset_string,"))",sep="")
  #if (verbose) print(cmd_string)
  eval(parse(text=cmd_string))
  subset_string <- paste("var.env$",ticker,"[com.env$OOS_date_range,allmodelvars]",sep="")
  cmd_string <- paste("OOS_data.df <- bind_rows(OOS_data.df,as.data.frame(",subset_string,"))",sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
}
#str(reg_data.df)
if (verbose) print(com.env$vcom_names)
#com.env$ind_names
#com.env$bin_names
if (verbose) print(allmodelvars)


