fdeList <- c("C")
names(fdeList) <- c("'.Adjusted'")
adjList <- c("H", "L","O","V")
names(adjList) <- c("'High'", "'Low'","'Open'","'Volume'")
lagList <- c()

set_name = function(math.list) {
  name <- NULL
  namePart <- NULL
  for (math_str in math.list) {
    namePart <- NULL
    math <- strsplit(math_str,split=",")[[1]][1] #get element to first comma (function call)
    parms <- gsub("^[^,]*,","",math_str) #get everything after first comma (parameters) 
    switch(math, 
           "from.data.env" = {
             name <- fdeList[which(parms == names(fdeList))]
             return(name)
           },
           "calc_look_forward" = {
             if(parms < 0){
               name <- paste0("C2Clf", -as.numeric(parms), "p")
               return(name)
             } else {
               namePart <- paste0("C2C", parms)
             }
           },
           "calc_dol" = {
             namePart <- "D"
           },
           "calc_math" = {
             switch(parms,
               "c('H','L','XX0N <- sqrt(XX1*XX2)'" = {
                 namePart <- "J"
               }, 
               "c('H','L','C'),'XX0N <- (XX1*XX2*XX3)^(1/3)'" = {
                 namePart <- "R"
               },
               "c('D'),math_str='XX0N <- log(XX1) - 18.5'" = {
                 namePart <- "ld"
               },
               "c('H','L','YC'),'XX0N <- pmax(log(XX1/XX2),abs(log(XX1/XX3)),abs(log(XX2/XX3)))'" = {
                 namePart <- "tr"
               },
               "c('DMd','TRd'),'XX0N <- XX1/XX2'" = {
                 namePart <- "di"
               },
               "c('YTTraw','D'),math_str='XX0N <- ifelse(XX1>0,XX2,0)'" = {
                 namePart <- "dd"
               },
               "calc_math,c('CCraw','D'),math_str='XX0N <- XX1*XX2'" = {
                 namePart <- "fi"
               },
                namePart <- "ti"
             )
           },
           "calc_res" = {
             namePart <- "S"
           },
           "calc_adj" = {
             namePart <- adjList[which(parms == names(adjList))]
           },
           "from.var.env" = {
             namePart <- gsub("'","",parms)
           },
           "calc_ret" = {
             namePart <-  substr(parms,2,2)
             namePart <- paste0(namePart,substr(parms,6,6))
           },
           "calc_lag" = {
             if(name %in% LETTERS){
               index <- which(name == LETTERS)
               name <- LETTERS[index-as.numeric(parms)]
             } else {
               namePart <- "l"
               namePart <- paste0(namePart,parms)
             }
           },
           "calc_cmn" = {
             namePart <- gsub("'","",parms)
             namePart <- paste0(namePart,"E")
           },
           "calc_decay" = {
             namePart <- 100*as.numeric(strsplit(parms,split="0")[[1]][2])
             namePart <- paste0("d",namePart)
           },
           "calc_bin" = {
             if(grepl("E",parms)){
               namePart <- "be"
             } else if(grepl("T",parms)){
               namePart <- "bt"
             } else if(grepl("D",parms)){
               namePart <- "bd"
             } else if(grepl("S",parms)){
               namePart <- "bs"
             } else{
               namePart <- "bw"
             }
           },
           "calc_vlty" = {
             namePart <- gsub("'","",parms)
             namePart <- paste0(namePart,"T")
           }
           
           
           
    )
    name <- paste0(name,namePart)
  }
  return(name)
}

