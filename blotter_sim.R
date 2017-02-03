#make into function
#add in ordering logic
#pass in sim_start_date, sim_end_date


#sim Blotter
print (paste("Running Sim",Sys.time()))
shares <- matrix(nrow=length(com.env$sim_date_index),ncol=length(com.env$stx.symbols))
for (SimDate_i in 1:(length(com.env$sim_date_index))) {
  SimDate <- com.env$sim_date_index[SimDate_i]
  equity <- com.env$init_equity 
  #print(paste("DATE:",SimDate,"Equity:",equity))
  port.pos <- port_opt_lp(as.vector(MU[SimDate]),as.vector(VLTY[SimDate]),equity)
  shares[SimDate_i,] <- port.pos
  #print(port.pos)
}

ADJRET.matrix <- as.matrix(ADJRET[com.env$sim_date_index])
ADJRET_shares <- ADJRET.matrix*shares
dayprofit <- rowSums(ADJRET_shares)
stockprofit <- colSums(ADJRET_shares)
totalprofit <- cumsum(dayprofit)
print(paste("total profit:",totalprofit[length(totalprofit)]))

plot(com.env$sim_date_index,totalprofit)
points(com.env$sim_date_index,dayprofit,col="red")