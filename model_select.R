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
  # if (trace==T) {
  #   cat('In vif_func, trying to determine which vars are colinear and need to be removed\n')
  #   if (!is.null(keep)) {
  #     cat('Cannot remove:',keep,'\n')
  #     cat('Able to remove:',names(in_frame)[!names(in_frame) %in% keep],'\n')
  #     if (length(keep[!keep %in% names(in_frame)])>0) {
  #       print("ERROR: Names in keep not in in_frame")
  #       print(keep[!keep %in% names(in_frame)])
  #     }
  #   } else {
  #     cat('keep == NULL\n')
  #   }
  # }
  #require(fmsb)
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  problem_vals_last_time <-FALSE
  removed_vals <- NULL         #vars to remove from in_frame
  vif_calcs <- NULL            #list containing vif values for all var_names
  check_vals <- names(in_frame)
  vif_max <- thresh+1          #forces loop 
  while (vif_max > thresh) {   #compute VIF for each var; construct vif_calcs list
    # if(trace==T) {
    #   cat("removed_vals:",removed_vals,'\n')
    #   cat("check_vals:",check_vals,'\n')
    # }
    in_dat <- in_frame[,!(names(in_frame) %in% removed_vals)]
    var_names <- names(in_dat)
    #vif_vals <- NULL             #array containing var_names, vif_calcs (vif_vals[,1],vif_vals[,2])
    vif_calcs <- vif_calcs[!names(vif_calcs) %in% removed_vals]
    for (val in check_vals) {
      regressors <- var_names[-which(var_names == val)]
      form <- paste(regressors, collapse = '+')
      form_in <- formula(paste(val, '~', form))
      if (val %in% names(vif_calcs)) {
        vif_calcs[val] <- VIF(lm(form_in,data=in_dat))
      } else {
        vif_calcs <- c(vif_calcs,VIF(lm(form_in,data=in_dat)))
        names(vif_calcs)[length(vif_calcs)] <- val               
      }
    }
    problem_vals <- names(vif_calcs[vif_calcs %in% c("NaN","Inf")]) 
    if (length(problem_vals)>0) {                     
      # if(trace==T) {
      #   cat("problem_vals:",problem_vals,'\n')
      #   print(vif_calcs)
      # }
      if (problem_vals_last_time) {
        print("ERROR in vif_func: problem with data in keep list")
        print(keep)
        print(problem_vals)
        print(vif_calcs[vif_calcs %in% c("NaN","Inf")])
        print(var_names)
        source("close_session.R")
      }
      problem_vals_last_time <- TRUE
      removed_vals <- c(removed_vals,problem_vals[!problem_vals %in% keep])
      var_names <- var_names[!var_names %in% removed_vals]
      check_vals <- names(in_frame[,!(names(in_frame) %in% removed_vals)])
      vif_init <- NULL
      next
    } else {
      problem_vals_last_time <- FALSE
    }
    sort_vif_calcs <- sort(vif_calcs,decreasing=TRUE)
    #if(trace==T) print(sort_vif_calcs)
    vif_max <- sort_vif_calcs[1]
    if (vif_max <= thresh) break
    check_vals <- NULL
    starting_removed_count <- length(removed_vals)
    for (vif_max_name in names(sort_vif_calcs)) {
      if ((!vif_max_name %in% keep) & (length(removed_vals)==starting_removed_count)) {
        removed_vals <- c(removed_vals,vif_max_name)
      } else {
        if (vif_calcs[vif_max_name] > thresh) check_vals <- c(check_vals,vif_max_name)
      }
      if (vif_calcs[vif_max_name] <= thresh) break
    }
    if (length(removed_vals) == starting_removed_count) {
      print("Can't keep vars requested AND get vif < thresh, keep vars:")
      print(keep)
      cat('thresh:',thresh,'max_vif:',sort_vif_calcs[1],'\n')
      print(sort_vif_calcs)
      cat('Calling vif_func without trying to keep com.env$best_reg_names, resetting best_vars\n')
      com.env$best_adj_r2 <- 0
      com.env$best_reg_names <- 0
      com.env$best_vcom <- NULL
      return(vif_func(in_frame,keep=NULL,thresh=5,trace=T))  #reduce threshold to make model less colinear
    }
  }
  # DEBUGGING
  if (trace==T) {
    if (any(var_names %in% removed_vals)) {
      print("WARNING: In VIF function, data remains in_dat after being removed")
      cat('removed_vals:',removed_vals,'\n')
      cat('names(id_dat):',names(in_dat),'\n')
    }
    if (any(keep %in% removed_vals)) {
      print("WARNING: In VIF function, removing vals asked to be kept")
      cat('removed_vals:',removed_vals,'\n')
      cat('keep:',keep,'\n')
    }
    print(sort_vif_calcs)
    cat("removing names from reg_data.df:",removed_vals,'\n')
  }
  return(var_names)
}

