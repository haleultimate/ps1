#run portfolio optimizatoin [lp_sim; below] with expectations as is
#then reverses expectations and reruns portfolio optimization
run_sim <- function() {
  print("run_sim")
  print(com.env$clu_names)
  make_mu()       #calc MU,VLTY,ADJRET for each var.env xts object
  print(paste("Total equity:",com.env$init_equity))
  for (alpha_wt in c(16000)) {
    com.env$alpha_wt <- alpha_wt
    com.env$pca_wt <- 1.              #adjust when relative magnitudes are known
    print(paste("alpha_wt:",com.env$alpha_wt))
    lp_sim("MU",com.env$stx.symbols,com.env$sim_date_index,com.env$port_size_mult*length(com.env$stx.symbols),plot_profit=TRUE)      #run sim, plot daily profit
  }
  if (com.env$liqx) {
    make_mu_liqx()
    lp_sim("MU_liqx",com.env$stx.symbols,com.env$sim_date_index,com.env$port_size_mult*length(com.env$stx.symbols),plot_profit=TRUE)      #run sim, plot daily profit
    sim_stats <- calc.r2(com.env$model.current,var.env$sim_data.df)
    cat("r2",sim_stats$rsq,"r20",sim_stats$rsq0,"cor",sim_stats$cor,"\n")
    cat("mse",sim_stats$mse,"mean",sim_stats$mean,"wpct",sim_stats$winpct,"\n")
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
  share_min <- 100
  #shares <- matrix(nrow=length(sim_date_index),ncol=length(com.env$stx.symbols))
  sim.env$shares <- matrix(nrow=length(sim_date_index),ncol=length(stx))
  first_pass <- TRUE
  sim.env$tc$oc <- 10      #transaction cost per order
  sim.env$tc$bp <- 0.0007  #transaction cost per dollar
  
  for (i in 1:(length(sim_date_index))) {
    SimDate <- sim_date_index[i]
    if (i == 1) {
      port_str <- "rep(0,length(stx))"
    } else {
      #yesterday_SimDate <- sim_date_index[SimDate_i-1]
      #print(paste(SimDate,yesterday_SimDate))
      port_str <- "as.vector(sim.env$shares[i-1,1:length(stx)])"
    }
    
    #equity <- com.env$init_equity 
    #print(paste("DATE:",SimDate,"Equity:",equity))
    if (first_pass) {
      cmd_string <- paste0("port.pos <- port_opt_lp(as.vector(var.env$",mu_col_name,
                           "[SimDate,stx]),as.vector(var.env$VLTY[SimDate,stx]),equity,",
                           port_str,",sim.env$tc,sim.env$pca,first_pass)")
      #first_pass <- FALSE
    } else {
      cmd_string <- paste0("port.pos <- port_opt_lp(as.vector(var.env$",mu_col_name,
                           "[SimDate,stx]),as.vector(var.env$VLTY[SimDate,stx]),equity,",
                           port_str,",sim.env$tc,sim.env$pca)")
    }
    eval(parse(text=cmd_string))
    if (first_pass) {
      print(cmd_string)
      first_pass <- FALSE
    }
    #port.pos <- port_opt_lp(as.vector(var.env$MU[SimDate]),as.vector(var.env$VLTY[SimDate]),equity)
    sim.env$shares[i,] <- port.pos
    #print(port.pos)
  }
  
  sim.env$shares[abs(sim.env$shares)<share_min] <- 0
  ADJRET.matrix <- as.matrix(var.env$ADJRET[sim_date_index,stx])
  MU.matrix <- as.matrix(var.env$MU[sim_date_index,stx])
  VLTY.matrix <- as.matrix(var.env$VLTY[sim_date_index,stx])

  sim.env$MU_shares <- MU.matrix*sim.env$shares
  sim.env$day_MU <- rowSums(sim.env$MU_shares)
  sim.env$VLTY_shares <- VLTY.matrix*sim.env$shares*sim.env$shares
  sim.env$day_VLTY <- rowSums(sim.env$VLTY_shares)

  sim.env$ADJRET_shares <- ADJRET.matrix*sim.env$shares
  sim.env$ADJRET_shares[is.na(sim.env$ADJRET_shares)] <- 0     #should we repair or crash when NA's are found?
  sim.env$dayprofit <- rowSums(sim.env$ADJRET_shares)
  sim.env$stockprofit <- colSums(sim.env$ADJRET_shares)
  sim.env$totalprofit <- cumsum(sim.env$dayprofit)
  print(paste("total profit:",sim.env$totalprofit[length(sim.env$totalprofit)]))
  if (plot_profit) {
    plot(sim_date_index,sim.env$totalprofit)
    points(sim_date_index,sim.env$dayprofit,col="red")
  }
  return(sim.env$totalprofit[length(sim.env$totalprofit)])
}

#pass in mu, vlty, port_size
#pass in current portfolio, transaction cost per order (tc$oc), transaction cost per dollar (tc$bp)
#pass in pca array
#future: add in order vars (take into account order costs)
#        add in Principal components (sim.env$pca)
port_opt_lp <- function (lp.mu, lp.vlty, lp.port_size, lp.port, lp.tc, lp.pca, first_pass = FALSE) {
  if (first_pass) print(paste(length(lp.mu),length(lp.vlty),lp.port_size,length(lp.port),lp.tc$oc,lp.tc$bp,length(lp.pca),first_pass))
  if (!exists("lp.first_run")) {
    lp.first_run <- FALSE
    lp.old_port_size <- (-1)
    lp.stx <- length(lp.mu)
    if (lp.stx != length(lp.vlty)) print (paste("ERROR: MU and VLTY must have same number of stocks, mu_n:",lp.stx,"vlty_n:",length(lp.vlty)))
    
    lp.vlty_bounds <- c(0,15000,30000,45000)
    vb_n <- length(lp.vlty_bounds)    #number of vlty bounds
    pc_n <- ncol(lp.pca)              #number of principal components to maintain in balance
    lp.vbs <- vb_n - 2                #number of vlty bound segments needing variables
    max_pos <- lp.vlty_bounds[vb_n]    #upper positions size constraint
    
    #create random u and v vectors
    #mu <- rnorm(lp.stx,0,.0050)
    #vlty <- rnorm(lp.stx,0.015,.0025)
    #vlty <- vlty*vlty
    
    lp.alpha_wt <- com.env$alpha_wt
    lp.pca_wt <- com.env$pca_wt
    lp.vlty_wt <- rep(-1.,vb_n-1)
    
    pos_vars <- lp.stx                #position (only var allowed to go negative)
    long_short_vars <- 2*lp.stx       #long, short (abs of position)
    order_vars <- 3*lp.stx            #shares bought (sb), shares sold (ss) vars, one [0,1] order (O) var for each stock
    vb_vars <- lp.vbs*lp.stx          #volatility bound segments (2..vb_n-1)  #first segment uses long/short vars
    pop_pon_vars <- 2*pc_n            #pca out of balance positive (pop), pca out of balance negative (pon)
    
    
    # 1:lp.stx                                         # position_vars
    # (lp.stx+1):(3*lp.stx)                            # long_vars + short_vars
    # (3*lp.stx+1):(6*lp.stx)                          # shares_bought + shares_sold vars + order vars
    # (6*lp.stx+1):((6+lp.vbs)*lp.stx)                 # vb_vars (one for every lp.stx for each lp.vlty_bounds segment)
    # ((6+vbi)*lp.stx+1):((6+lp.vbs)*lp.stx + 2*pc_n)  # pca out of balance (positive [pop] and negative [pon])
    
    lp.vars <- pos_vars + long_short_vars + order_vars + vb_vars + pop_pon_vars
    
    unique_cons <- 3                    #port_size (1), long_short balance (2)
    long_short_cons <- long_short_vars  #definition constraint for each long and short variable
    order_cons <- 3*lp.stx              #order constraints for each stock [pp+sb-ss=pos,sb<O*2*max_pos,ss<O*2*max_pos]
    vb_cons <- 2*vb_vars                #volatility cost for each variable defined long/short
    pca_cons <- pc_n                    #pca out of balance constraint
    
    cons <- unique_cons + long_short_cons + order_cons + vb_cons + pca_cons
    
    #init lp.port.model
    lp.port.model <- make.lp(nrow=0,ncol=lp.vars)
    lp.control(lp.port.model,sense="max")

    set.type(lp.port.model, columns=1:(5*lp.stx), type = "real")              #set all vars to real
    set.type(lp.port.model, columns=(5*lp.stx+1):(6*lp.stx), type = "binary") #Order vars to binary
    set.type(lp.port.model, columns=(6*lp.stx+1):lp.vars, type = "real")      #set all vars to real
    
    #all vars except pos vars must be positive
    set.bounds(lp.port.model,lower=c(rep(-max_pos,lp.stx),rep(0,(lp.vars-lp.stx))),columns=(1:lp.vars))  
    #all position vars bounded by max_pos, shares bought/sold bounded by 2*max_pos, binary order vars bounded by 1
    set.bounds(lp.port.model,upper=c(rep(max_pos,(3*lp.stx)),rep((2*max_pos),(2*lp.stx)),rep(1,lp.stx)),columns=(1:(6*lp.stx)))
    #variability bound vars range from boundary point to max_pos
    for (vb_i in 1:lp.vbs) {
      max_vbs <- max_pos - lp.vlty_bounds[vb_i+1]
      set.bounds(lp.port.model,upper=rep(max_vbs,lp.stx),columns=((6*lp.stx+(vb_i-1)*lp.stx+1):(6*lp.stx+vb_i*lp.stx)))
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
    #buy long, sell short constraints; 
    #new_Pos - shares_bought + shares_sold = current_pos
    #shares_bought - order[0,1] * (2*max_pos) <= 0
    #shares_sold - order[0,1] * (2*max_pos) <= 0
    for (var_i in 1:lp.stx) {
      add.constraint(lp.port.model,xt=c(1,-1,1),type="=",rhs=lp.port[var_i],indices=c(var_i,3*lp.stx+var_i,4*lp.stx+var_i))
      add.constraint(lp.port.model,xt=c(1,-2*max_pos),type="<=",rhs=0,indices=c(3*lp.stx+var_i,5*lp.stx+var_i))
      add.constraint(lp.port.model,xt=c(1,-2*max_pos),type="<=",rhs=0,indices=c(4*lp.stx+var_i,5*lp.stx+var_i))
    }
    #VB[1:lp.vbs]L[1:lp.stx]; long_shares - vbi_var <= vb_bounds
    #VB[1:lp.vbs]S[1:lp.stx]; short_shares - vbi_var <= vb_bounds
    for (vb_i in 1:lp.vbs) {
      for (var_i in 1:lp.stx) {                                                              #long var
        add.constraint(lp.port.model,xt=c(1,-1),type="<=",rhs=lp.vlty_bounds[vb_i+1],
                       indices=c((lp.stx+var_i),((6+vb_i-1)*lp.stx+var_i)))
      }
      for (var_i in 1:lp.stx) {                                                              #short var
        add.constraint(lp.port.model,xt=c(1,-1),type="<=",rhs=lp.vlty_bounds[vb_i+1],
                       indices=c((2*lp.stx+var_i),((6+vb_i-1)*lp.stx+var_i)))
      }
    }
    #PCA balance constraint
    #for each pc_i in pca vector
    #(pos1*pca(pc_i,1) + pos2*pca(pc_i,2) + ... posn*pca(pc_i,n)) - pop(pc_i) + pon(pc_i) = 0
    for (pc_i in 1:pc_n) {
      add.constraint(lp.port.model,xt=c(as.vector(lp.pca[,pc_i]),-1,1),type="=",rhs=0,
                     indices=c(1:lp.stx,(6+lp.vbs)*lp.stx+2*(pc_i-1)+1,(6+lp.vbs)*lp.stx+2*(pc_i-1)+2))
    }
    #print("end adding constraints")
    
    #col chars = 6 var types + volatility bounds
    #Each char represents lp.stx vars
    nums <- 1:lp.stx
    col_chars <- c("P","L","S","SB","SS","O")
    for (vb_i in 1:lp.vbs) {
      col_chars <- c(col_chars,paste0("VB",vb_i+1,"_"))
    }
    var_names <- NULL
    for (char in 1:length(col_chars)) {
      var_names <- c(var_names,paste0(col_chars[char],nums))
    }
    #Add in a POP and PON var for each principal component index
    for (pc_i in 1:pc_n) {
      var_names <- c(var_names,paste0("POP",pc_i),paste0("PON",pc_i))
    }
    
    #name first three constraints
    con_names <- c("port_size","ls_balance1","ls_balance2")
    #name long/short var constraints
    con_chars <- c("L","S")
    for (char in 1:length(con_chars)) {
      con_names <- c(con_names,paste0(con_chars[char],nums))
    } 
    #name constraints for BUY-SELL constraint (BS), BUY-Order constraint (BO), Sell-Order Constraint (SO)
    for (ci in 1:lp.stx) {
      con_names <- c(con_names,paste0("BS",ci),paste0("BO",ci),paste0("SO",ci))
    }
    #name constraints for volatility bounds
    con_chars <- NULL
    for (vb_i in 1:lp.vbs) {
      con_chars <- c(con_chars,paste0("VB",vb_i+1,"L"),paste0("VB",vb_i+1,"S"))
    }
    for (char in 1:length(con_chars)) {
      con_names <- c(con_names,paste0(con_chars[char],nums))
    } 
    #name principal component constraints
    for (pc_i in 1:pc_n) {
      con_names <- c(con_names,paste0("PC",pc_i))
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
  lp.obj.fun[(3*lp.stx+1):(5*lp.stx)] <- rep(-lp.tc$bp,2*lp.stx)  #basis point costs per dollar ordered, SB/SS vars
  lp.obj.fun[(5*lp.stx+1):(6*lp.stx)] <- rep(-lp.tc$oc,lp.stx)    #order cost per order, O vars
  #subsequent vlty segments
  for (vb_i in 1:lp.vbs) { #piecwise linear component for segment (portion added to all previous segments)
    lp.obj.fun[(6*lp.stx+(vb_i-1)*lp.stx+1):(6*lp.stx+vb_i*lp.stx)] <- lp.vlty_wt[vb_i+1]*lp.vlty_bounds[vb_i+2]*lp.vlty
  }
  lp.obj.fun[((6+vb_i)*lp.stx+1):((6+vb_i)*lp.stx + 2*pc_n)] <- rep(-lp.pca_wt,2*pc_n)    #out of balance pca vector penalty
  set.objfn(lp.port.model,lp.obj.fun)
  #set port_size constraint, needed for every new port_size
  if (lp.port_size != lp.port_size) {
    #add.constraint(port.model,c(rep(0,lp.stx),rep(1,2*lp.stx),rep(0,(lp.vars-3*lp.stx))),"=",lp.port_size)
    set.rhs(lp.port.model,b=lp.port_size,constraints=1) #can I use the constraint name = "port_size" here?
  }
  
  if (first_pass) {
    print("Writing out lp to file 'test_lp'")
    write.lp(lp.port.model,filename="test_lp",type="lp")
  }
  
  solve(lp.port.model)
  lp.positions <- get.variables(lp.port.model)[1:lp.stx]

  if(first_pass) {
    pos_sqr <- lp.positions*lp.positions
  
    calc_mu <- lp.positions*lp.mu
    calc_vlty <- pos_sqr*lp.vlty*(1/lp.alpha_wt)
    
    order <- 0
    shares_BS <- 0
    for (i in 1:lp.stx) {
      if (lp.port[i] != lp.positions[i]) {
        order <- order + 1
        shares_BS <- shares_BS + abs(lp.port[i]-lp.positions[i])
      }
    }
    order_costs <- order*lp.tc$oc
    bp_costs <- shares_BS*lp.tc$bp
    
    pca_oob <- NULL
    for (i in 1:pc_n) {
      pca_oob[i] <- sum(lp.positions * as.vector(lp.pca[,i]))
    }
  
    total_mu <- sum(calc_mu)
    total_vlty <- sum(calc_vlty)
  
    print(lp.positions)
    print(lp.mu)
    print(lp.vlty)
    print(paste("order_costs:",order_costs," bp_costs:",bp_costs))
    print("PCA out of balance:")
    print(pca_oob)
  
    print(paste("total mu=",total_mu,"total vlty=",total_vlty))
    print(total_mu-total_vlty)
    print(get.objective(lp.port.model))
  }  
  return(lp.positions)
}  