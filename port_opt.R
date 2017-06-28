#run portfolio optimizatoin [lp_sim; below] with expectations as is
#then reverses expectations and reruns portfolio optimization
run_sim <- function() {
  print("run_sim")
  make_mu()       #calc MU,VLTY,ADJRET for each var.env xts object
  print(paste("Total equity:",com.env$init_equity))
  for (alpha_wt in c(16000)) {
    com.env$alpha_wt <- alpha_wt
    print(paste("alpha_wt:",com.env$alpha_wt))
    lp_sim("MU",com.env$stx.symbols,com.env$sim_date_index,com.env$port_size_mult*length(com.env$stx.symbols),plot_profit=TRUE)      #source("blotter_sim.R") #run sim, plot daily profit
  }
  #var.env$MU <- -var.env$MU
  #print("reversing MU")
  #lp_sim("MU",com.env$stx.symbols,com.env$sim_date_index,plot_profit=TRUE)  #source("blotter_sim.R")
}

#lp_sim sim calls port_opt_lp [optimizes portfolio position daily; below]
#calculates profit by multiplying daily (cc_return)*(position in dollars [aka shares])
#add in ordering logic
#return profit
lp_sim <- function(mu_col_name,stx,sim_date_index,equity,plot_profit=FALSE) {
  print (paste("Running Sim",Sys.time()))
  #shares <- matrix(nrow=length(sim_date_index),ncol=length(com.env$stx.symbols))
  shares <- matrix(nrow=length(sim_date_index),ncol=length(stx))
  first_pass <- FALSE
  
  for (SimDate_i in 1:(length(sim_date_index))) {
    SimDate <- sim_date_index[SimDate_i]
    #equity <- com.env$init_equity 
    #print(paste("DATE:",SimDate,"Equity:",equity))
    if (first_pass) {
      cmd_string <- paste0("port.pos <- port_opt_lp(as.vector(var.env$",mu_col_name,"[SimDate,stx]),as.vector(var.env$VLTY[SimDate,stx]),equity,first_pass)")
      first_pass <- FALSE
    } else {
      cmd_string <- paste0("port.pos <- port_opt_lp(as.vector(var.env$",mu_col_name,"[SimDate,stx]),as.vector(var.env$VLTY[SimDate,stx]),equity)")
    }
    eval(parse(text=cmd_string))
    #port.pos <- port_opt_lp(as.vector(var.env$MU[SimDate]),as.vector(var.env$VLTY[SimDate]),equity)
    shares[SimDate_i,] <- port.pos
    #print(port.pos)
  }
  
  ADJRET.matrix <- as.matrix(var.env$ADJRET[sim_date_index,stx])
  ADJRET_shares <- ADJRET.matrix*shares
  ADJRET_shares[is.na(ADJRET_shares)] <- 0     #should we repair or crash when NA's are found?
  dayprofit <- rowSums(ADJRET_shares)
  stockprofit <- colSums(ADJRET_shares)
  totalprofit <- cumsum(dayprofit)
  print(paste("total profit:",totalprofit[length(totalprofit)]))
  if (plot_profit) {
    plot(sim_date_index,totalprofit)
    points(sim_date_index,dayprofit,col="red")
  }
  return(totalprofit[length(totalprofit)])
}

#pass in mu, vlty, port_size
#future: add in order vars (take into account order costs)
port_opt_lp <- function (lp.mu, lp.vlty, lp.port_size, first_pass = FALSE) {
  
  if (!exists("lp.first_run")) {
    lp.first_run <- FALSE
    lp.old_port_size <- (-1)
    lp.stx <- length(lp.mu)
    if (lp.stx != length(lp.vlty)) print (paste("ERROR: MU and VLTY must have same number of stocks, mu_n:",lp.stx,"vlty_n:",length(lp.vlty)))
    
    lp.vlty_bounds <- c(0,15000,30000,45000)
    vb_n <- length(lp.vlty_bounds)    #number of vlty bounds
    lp.vbs <- vb_n - 2                #number of vlty bound segments needing variables
    max_vb <- lp.vlty_bounds[vb_n]    #upper positions size constraint
    
    #create random u and v vectors
    #mu <- rnorm(lp.stx,0,.0050)
    #vlty <- rnorm(lp.stx,0.015,.0025)
    #vlty <- vlty*vlty
    
    lp.alpha_wt <- com.env$alpha_wt
    lp.vlty_wt <- rep(-1.,vb_n-1)
    
    pos_vars <- lp.stx                #position (only var allowed to go negative)
    long_short_vars <- 2*lp.stx       #long, short (abs of position)
    vb_vars <- lp.vbs*lp.stx             #volatility bound segments (2..vb_n-1)  #first segment uses long/short vars
    
    # 1:lp.stx                        # pos_vars
    # (lp.stx+1):(2*lp.stx)              # long_vars
    # (2*lp.stx+1):(3*lp.stx)            # short_vars
    # (3*lp.stx+1):(3*lp.stx+vbi*lp.stx)    # vb_vars (one for every lp.stx for each lp.vlty_bounds segment)
    lp.vars <- pos_vars + long_short_vars + vb_vars
    
    unique_cons <- 3               #port_size (1), long_short balance (2)
    long_short_cons <- long_short_vars
    vb_cons <- 2*vb_vars           #each variable defined long/short
    
    cons <- unique_cons + long_short_cons + vb_cons
    
    #init lp.port.model
    lp.port.model <- make.lp(nrow=0,ncol=lp.vars)
    lp.control(lp.port.model,sense="max")
    
    
    set.bounds(lp.port.model,lower=c(rep(-max_vb,lp.stx),rep(0,(lp.vars-lp.stx))),columns=(1:lp.vars))
    set.bounds(lp.port.model,upper=rep(max_vb,(3*lp.stx)),columns=(1:(3*lp.stx)))
    for (vb_i in 1:lp.vbs) {
      max_vbs <- max_vb - lp.vlty_bounds[vb_i+1]
      set.bounds(lp.port.model,upper=rep(max_vbs,lp.stx),columns=((3*lp.stx+(vb_i-1)*lp.stx+1):(3*lp.stx+vb_i*lp.stx)))
    }
    add.constraint(lp.port.model,c(rep(0,lp.stx),rep(1,2*lp.stx),rep(0,(lp.vars-3*lp.stx))),"=",lp.port_size)
    add.constraint(lp.port.model,c(rep(1,lp.stx),rep(0,(lp.vars-lp.stx))),"=",0)
    add.constraint(lp.port.model,c(rep(0,lp.stx),rep(1,lp.stx),rep(-1,lp.stx),rep(0,lp.vars-3*lp.stx)),"=",0)
    #L[1:lp.stx]
    for (var_i in 1:lp.stx) {
      add.constraint(lp.port.model,xt=c(1,-1),type="<=",rhs=0,indices=c(var_i,lp.stx+var_i))
    }
    #S[lp.stx]
    for (var_i in 1:lp.stx) {
      add.constraint(lp.port.model,xt=c(1,1),type=">=",rhs=0,indices=c(var_i,2*lp.stx+var_i))
    }
    #VB[1:lp.vbs]L[1:lp.stx]
    #VB[1:lp.vbs]S[1:lp.stx]
    for (vb_i in 1:lp.vbs) {
      for (var_i in 1:lp.stx) {                                                              #long var
        add.constraint(lp.port.model,xt=c(1,-1),type="<=",rhs=lp.vlty_bounds[vb_i+1],indices=c((lp.stx+var_i),((3+vb_i-1)*lp.stx+var_i)))
      }
      for (var_i in 1:lp.stx) {                                                              #short var
        add.constraint(lp.port.model,xt=c(1,-1),type="<=",rhs=lp.vlty_bounds[vb_i+1],indices=c((2*lp.stx+var_i),((3+vb_i-1)*lp.stx+var_i)))
      }
    }
    
    nums <- 1:lp.stx
    col_chars <- c("P","L","S")
    for (vb_i in 1:lp.vbs) {
      col_chars <- c(col_chars,paste("VB",vb_i+1,"_",sep=""))
    }
    var_names <- NULL
    for (char in 1:length(col_chars)) {
      var_names <- c(var_names,paste(col_chars[char],nums,sep=""))
    }
    
    con_chars <- c("L","S")
    for (vb_i in 1:lp.vbs) {
      con_chars <- c(con_chars,paste("VB",vb_i+1,"L",sep=""))
      con_chars <- c(con_chars,paste("VB",vb_i+1,"S",sep=""))
    }
    con_names <- c("port_size","ls_balance1","ls_balance2")
    for (char in 1:length(con_chars)) {
      con_names <- c(con_names,paste(con_chars[char],nums,sep=""))
    }  
    dimnames(lp.port.model) <- list(con_names,var_names)
  }

  #set objective function, needed for every new MU, VLTY
  lp.obj.fun <- rep(0,lp.vars)
  #mu component
  lp.obj.fun[1:lp.stx] <- lp.alpha_wt*lp.mu
  #first vlty segment
  lp.obj.fun[(lp.stx+1):(2*lp.stx)]   <- lp.vlty_wt[1]*lp.vlty_bounds[2]*lp.vlty
  lp.obj.fun[(2*lp.stx+1):(3*lp.stx)] <- lp.obj.fun[(lp.stx+1):(2*lp.stx)]
  #subsequent vlty segments
  for (vb_i in 1:lp.vbs) { #piecwise linear component for segment (portion added to all previous segments)
    lp.obj.fun[(3*lp.stx+(vb_i-1)*lp.stx+1):(3*lp.stx+vb_i*lp.stx)] <- lp.vlty_wt[vb_i+1]*lp.vlty_bounds[vb_i+2]*lp.vlty
  }
  set.objfn(lp.port.model,lp.obj.fun)
  #set port_size constraint, needed for every new port_size
  if (lp.port_size != lp.port_size) {
    #add.constraint(port.model,c(rep(0,lp.stx),rep(1,2*lp.stx),rep(0,(lp.vars-3*lp.stx))),"=",lp.port_size)
    set.rhs(lp.port.model,b=lp.port_size,constraints=1) #can I use the constraint name = "port_size" here?
  }
  
  if (first_pass) write.lp(lp.port.model,filename="test_lp",type="lp")
  
  solve(lp.port.model)
  lp.positions <- get.variables(lp.port.model)[1:lp.stx]

  if(first_pass) {
    pos_sqr <- lp.positions*lp.positions
  
    calc_mu <- lp.positions*lp.mu*lp.alpha_wt
    calc_vlty <- pos_sqr*lp.vlty
  
    total_mu <- sum(calc_mu)
    total_vlty <- sum(calc_vlty)
  
    print(lp.positions)
    print(lp.mu)
    print(lp.vlty)
  
    print(paste("total mu=",total_mu,"total vlty=",total_vlty))
    print(total_mu-total_vlty)
    print(get.objective(lp.port.model))
  }  
  return(lp.positions)
}  