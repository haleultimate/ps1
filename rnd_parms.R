#rnd_parms.R
# list of settings for constructing and modifying variables
rnd.env <- new.env()
rnd.env$vs.com <- NULL
rnd.env$raw_list <- NULL

rnd.env$priceID <- c('H','L','O','C','M','V','D')
rnd.env$prcorder <- c(1,2,2,2,3,11,12,12,12,13,111)
names(rnd.env$prcorder) <- c('C','M','L','H','O','YC','YM','YL','YH','YO','C2') #c('YO','YL','YH','YM','YC','O','L','H','M','C')

rnd.env$fun_id <- c(1:5)  #any undefined function will be mapped to zero
names(rnd.env$fun_id) <- c('calc_cap','calc_z','calc_res','calc_decay','calc_vlty')

rnd.env$prob$choices <- c('type','cap','math','scale','decay','bin')
rnd.env$prob$choices.wts <- c(0.1,0.2,0.0,0.3,0.3,0.4) 
rnd.env$prob$choices.bv <- c('type','cap','scale','decay')
rnd.env$prob$choices.bv.wts <- c(0.1,0.2,0.,0.2)
rnd.env$prob$type <- c("ret","res","vlt","vol","vrs")
rnd.env$prob$type.wts <- c(0.4,0.5,0.1,0.1,0.1)
rnd.env$prob$type.bv <- rnd.env$prob$type
rnd.env$prob$type.bv.wts <- c(0.1,0.1,0.02,0.1,0.1)
rnd.env$prob$cap <- c("abscap","cap_pct","zcap","none")
rnd.env$prob$cap.wts <- c(0.1,0.4,0.2,0.3)
rnd.env$prob$abscap <- c(.13,.08,.05,.03,.02,.01,.005,.003)
rnd.env$prob$abscap.wts <- rep(0.1,times=length(rnd.env$prob$abscap))
rnd.env$prob$cap_pct <- c(.001,.005,.01,.02,.03,.05,.08,.13)
rnd.env$prob$cap_pct.wts <- rep(0.1,times=length(rnd.env$prob$cap_pct))
rnd.env$prob$zcap <- c(.5,1.5,2,2.5,3,3.5,4,5)
rnd.env$prob$zcap.wts <- rep(0.1,times=length(rnd.env$prob$zcap))
rnd.env$prob$math <- c('power','log','exp','none')
rnd.env$prob$math.wts <- c(.03,.02,.01,.94)
rnd.env$prob$scale <- c('zscore','zscale','none')
rnd.env$prob$scale.wts <- c(0.1,0.5,0.2)
rnd.env$prob$decay <- c(2,1,.89,.55,.34,.21,.13,.08,.05,.03,.02,.01)
rnd.env$prob$decay.wts <- c(0.05,0.5,rep(0.05,times=(length(rnd.env$prob$decay)-2)))
rnd.env$prob$bin <- c('new_var','none')
rnd.env$prob$bin.wts <- c(0.5,0.5)
rnd.env$prob$bin_pt1 <- c(-3,-2,-1,0,1) 
rnd.env$prob$bin_pt1.wts <- c(0.1,0.1,0.1,0.1,0.1)
rnd.env$prob$bin_pt2 <- c(-1,0,1,2,3) 
rnd.env$prob$bin_pt2.wts <- c(0.1,0.1,0.1,0.1,0.1)

#currently being coded
rnd.env$prob$choices <- c('type','cap','scale','decay','bin')


                            