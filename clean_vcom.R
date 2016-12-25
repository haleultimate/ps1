#clean_vcom.R

keep_vcom_name <- names(model.stepwise$coefficients)[-1]
keep_vcom_name <- append(colnames(reg_data.df)[1],keep_vcom_name)
more_vcom_names <- NULL
for (nam in keep_vcom_name) {
  vcom_num <- name2vcomnum[nam]
  more_vcom_names <- append(more_vcom_names,com.env$v.com[[vcom_num]]$requires)
}
keep_vcom_name <- append(keep_vcom_name,more_vcom_names)
keep_vcom_name <- unique(keep_vcom_name)
print(length(com.env$v.com))
delete_vcom_name <- NULL
for (v in 1:length(com.env$v.com)) {
  vd <- com.env$v.com[[v]]
  if (length(vd$name) == 1) {
    if ( !(vd$name %in% keep_vcom_name) ) {
      delete_vcom_name <- append(delete_vcom_name,vd$name)
    }
  } else {
    if (!(vd$name[1] %in% keep_vcom_name) & !(vd$name[2] %in% keep_vcom_name)) {
      delete_vcom_name <- append(delete_vcom_name,substr(vd$name[1],1,(nchar(vd$name[1])-1)))
    }
  }
}
#print(com.env$vcom_names)
#print(delete_vcom_name)
for (nam in delete_vcom_name) {
  cmd_string <- paste("com.env$vcom_names <- com.env$vcom_names[-(which(com.env$vcom_names %in% com.env$v.com$",nam,"$name))]",sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
  cmd_string <- paste("com.env$v.com$",nam," <- NULL",sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
}
#com.env$vcom_names <- com.env$vcom_names[-(which(com.env$vcom_names %in% delete_vcom_name))]

print(length(com.env$v.com))

if (com.env$model_loops != l) {
  print("Removing var environment")
  rm(var.env)
  var.env <- new.env()
}


