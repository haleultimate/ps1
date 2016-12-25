#####################################
# Automated model selection
# Author      : Joris Meys
# version     : 0.2
# date        : 12/01/09
#####################################
#CHANGE LOG
# 0.2   : check for empty scopevar vector
#####################################

# Function has.interaction checks whether x is part of a term in terms
# terms is a vector with names of terms from a model
has.interaction <- function(x,terms){
  out <- sapply(terms,function(i){
    sum(1-(strsplit(x,":")[[1]] %in% strsplit(i,":")[[1]]))==0
  })
  return(sum(out)>0)
}

# Function Model.select
# model is the lm object of the full model
# keep is a list of model terms to keep in the model at all times
# sig gives the significance for removal of a variable. Can be 0.1 too (see SPSS)
# verbose=T gives the F-tests, dropped var and resulting model after 
model.select <- function(model,keep,sig=0.05,verbose=F){
  counter=1
  # check input
  if(!is(model,"lm")) stop(paste(deparse(substitute(model)),"is not an lm object\n"))
  # calculate scope for drop1 function
  terms <- attr(model$terms,"term.labels")
  if(missing(keep)){ # set scopevars to all terms
    scopevars <- terms
  } else{            # select the scopevars if keep is used
    index <- match(keep,terms)
    # check if all is specified correctly
    if(sum(is.na(index))>0){
      novar <- keep[is.na(index)]
      warning(paste(
        c(novar,"cannot be found in the model",
          "\nThese terms are ignored in the model selection."),
        collapse=" "))
      index <- as.vector(na.omit(index))
    }
    scopevars <- terms[-index]
  }
  
  # Backward model selection : 
  
  while(T){
    # extract the test statistics from drop.
    test <- drop1(model, scope=scopevars,test="F")
    
    if(verbose){
      cat("-------------STEP ",counter,"-------------\n",
          "The drop statistics : \n")
      print(test)
    }
    
    pval <- test[,dim(test)[2]]
    
    names(pval) <- rownames(test)
    pval <- sort(pval,decreasing=T)
    
    if(sum(is.na(pval))>0) stop(paste("Model",
                                      deparse(substitute(model)),"is invalid. Check if all coefficients are estimated."))
    
    # check if all significant
    if(pval[1]<sig) break # stops the loop if all remaining vars are sign.
    
    # select var to drop
    i=1
    while(T){
      dropvar <- names(pval)[i]
      check.terms <- terms[-match(dropvar,terms)]
      if (length(check.terms)==0) {
        #print("**********************error in model_selection*************************")
        #print(dropvar)
        #print(check.terms)
        #print(names(pval))
        #print(terms)
        break
      }
      x <- has.interaction(dropvar,check.terms)
      if(x){i=i+1;next} else {break}              
    } # end while(T) drop var
    
    if(pval[i]<sig) break # stops the loop if var to remove is significant
    
    if(verbose){
      cat("\n--------\nTerm dropped in step",counter,":",dropvar,"\n--------\n\n")              
    }
    
    #update terms, scopevars and model
    scopevars <- scopevars[-match(dropvar,scopevars)]
    terms <- terms[-match(dropvar,terms)]
    
    formul <- as.formula(paste(".~.-",dropvar))
    model <- update(model,formul)
    
    if(length(scopevars)==0) {
      warning("All variables are thrown out of the model.\n",
              "No model could be specified.")
      return()
    }
    counter=counter+1
  } # end while(T) main loop
  return(model)
}

#out of sample r2 calculation, returns test (contains $rsq,$cor)
oos.r2 <- function(model,df.oos) {
  predicted.model <- predict.lm(model,newdata=df.oos)
  test.y <- df.oos[,predict.ret]
  mean.test.y <- mean(test.y)
  SS.total      <- sum((test.y - mean.test.y)^2)
  SS.residual   <- sum((test.y - predicted.model)^2)
  #SS.regression <- sum((predicted.model - mean(test.y))^2)
  #SS.total - (SS.regression+SS.residual)
  test <- NULL
  test$rsq <- 1 - (SS.residual/SS.total)  
  results <- cbind(predict.lm(model,newdata=df.oos),df.oos[,predict.ret])
  test$cor <- cor(results,use="complete.obs")[1,2]
  test$mse <- mean((test.y-predicted.model)^2)
  cnt <- length(predicted.model)
  wincnt <- 0
  for (i in 1:length(predicted.model)) if (abs(test.y[i]-predicted.model[i]) < abs(test.y[i])) wincnt <- wincnt + 1
  test$winpct <- wincnt/cnt
  #print(test)
  return(test)
}

#VIF function removes colinear variables from data frame
#From: https://beckmw.wordpress.com/2013/02/05/collinearity-and-stepwise-vif-selection/

#for debugging...
#in_frame <- stx.data[,2:ncol(stx.data)]
#thresh <- 10
#trace <- T

#vif_func<-function(in_frame,thresh=10,trace=T,...){  #... removed by RHB
vif_func<-function(in_frame,thresh=10,trace=T){
    
  require(fmsb)
  #detach(package:dplyr) #RHB
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  if(trace==T) print(var_names)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    #vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...)))) #RHB
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame))))
  }
  keep.dat <- !(vif_init[,2] %in% c("NaN","Inf")) #RHB
  vif_init <- vif_init[keep.dat,]                 #RHB
  in_frame <- in_frame[,keep.dat]                 #RHB
  var_names <- names(in_frame)                    #RHB
  vif_max  <- max(as.numeric(vif_init[,2]))
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        #vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_add<-VIF(lm(form_in, data = in_dat))     #RHB
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2])))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}