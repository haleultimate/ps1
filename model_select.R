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
oos.r2 <- function(model,df.oos,reverse=FALSE) {
  if (reverse) {
    predicted.model <- -predict.lm(model,newdata=df.oos)
  } else {
    predicted.model <- predict.lm(model,newdata=df.oos)
  }
  test.y <- df.oos[,com.env$predict.ret]
  mean.test.y <- mean(test.y)
  SS.total      <- sum((test.y - mean.test.y)^2)
  SS.residual   <- sum((test.y - predicted.model)^2)
  #SS.regression <- sum((predicted.model - mean(test.y))^2)
  #SS.total - (SS.regression+SS.residual)
  test <- NULL
  test$rsq <- 1 - (SS.residual/SS.total)  
  SS.total0 <- sum(test.y^2)
  test$rsq0 <- 1 - (SS.residual/SS.total0)
  results <- cbind(predicted.model,df.oos[,com.env$predict.ret])
  test$cor <- cor(results,use="complete.obs")[1,2]
  test$mse <- sqrt(mean((test.y-predicted.model)^2))
  test$mean <- mean(abs(test.y))
  cnt <- 0
  #cnt <- length(predicted.model)
  wincnt <- 0
  for (i in 1:length(predicted.model)) {
    if ((sign(test.y[i]) == sign(predicted.model[i])) & 
       ((test.y[i]!=0) & (predicted.model[i]!=0))) wincnt <- wincnt + 1
    if ((test.y[i] != 0) & (predicted.model[i]!=0)) cnt <- cnt + 1
  }
  test$winpct <- wincnt/cnt
  #print(test)
  return(test)
}

#VIF function removes colinear variables from data frame
#From: https://beckmw.wordpress.com/2013/02/05/collinearity-and-stepwise-vif-selection/

#for debugging...
#in_frame <- stx.data[,2:ncol(stx.data)]
#keep <- com.env$best_reg_vars [added by RHB] [variables to keep in model]
#thresh <- 10
#trace <- T

#vif_func<-function(in_frame,thresh=10,trace=T,...){  #... removed by RHB
vif_func<-function(in_frame,keep=NULL,thresh=10,trace=T){
  if (trace==T) {
    cat('In vif_func, trying to determine which vars are colinear and need to be removed\n')
    if (!is.null(keep)) {
      cat('Cannot remove:',keep,'\n')
    } else {
      cat('keep == NULL\n')
    }
  }
  #require(fmsb)
  #detach(package:dplyr) #RHB
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  #if(trace==T) print(var_names)
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
      #prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      #cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    removed_vals <- NULL
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
      
      if (!is.null(keep)) { #RHB, sort by vif (values and var_names)
        sort_vif <- sort(as.numeric(vif_vals[,2]),decreasing=TRUE)
        sort_var_names <- vif_vals[order(as.numeric(vif_vals[,2]),decreasing=TRUE),1]
        if (trace==T) print(rbind(sort_vif,sort_var_names))
      }
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',max_row,vif_vals[max_row,1],vif_max,'\n')
        if (!is.null(removed_vals)) cat('Previously removed vals:',removed_vals,'\n')
        #if (!is.null(keep)) { #RHB, print sorted vif (values and var_names)
        #  cat('sorted vif values and var_names\n')          
        #  print(sort_vif)
        #  print(sort_var_names)
        #}
        flush.console()
      }
      
      if (is.null(keep)) {
        if (trace==T) cat('removing:',vif_vals[max_row,1],'\n')
        in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]  #original code (without keep functionality
        removed_vals <- c(removed_vals,vif_vals[max_row,1])
      } else {
        max_vif <- sort_var_names[1]
        if (trace==T) cat('max_viv (regardless of keep vars):',sort_var_names[1],'\n')
        removable_var_names <- sort_var_names[!(sort_var_names %in% keep)]  #keep passed in from com.env$best_reg_names, vars not to remove
        if (trace==T) {
          print("sort_var_names with keep names removed")
          print(removable_var_names)
        }
        if (is.null(removable_var_names)) {
          print("Can't keep vars requested AND get vif < thresh")
          print(keep)
          cat('thresh:',thresh)
          return(keep)
        } else {
          if (trace==T) cat('max_viv (given keep vars):',removable_var_names[1],'\n')
          if(max_vif != removable_var_names[1]) 
            cat('Because of keep list',max_vif,vif_vals[max_row,2],'kept,',
                'removed',removable_var_names[1],sort_vif[which(removable_var_names[1]==sort_var_names)],'\n')
          in_dat <- in_dat[,!(names(in_dat) %in% removable_var_names[1])]
          removed_vals <- c(removed_vals,removable_var_names[1])
        }
      }
      
      if (any(names(in_dat) %in% removed_vals)) {
        print("WARNING: In VIF function, data remains in_dat after being removed")
        cat('removed_vals:',removed_vals,'\n')
        cat('names(id_dat):',names(in_dat),'\n')
      }
      if (any(keep %in% removed_vals)) {
        print("WARNING: In VIF function, removing vals asked to be kept")
        cat('removed_vals:',removed_vals,'\n')
        cat('keep:',keep,'\n')
      }
    }
    
    return(names(in_dat))
    
  }
  
}