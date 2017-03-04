#rnd_parms.R
# list of settings for constructing and modifying variables
rnd.env <- new.env(parent=globalenv())
rnd.env$vs.com <- NULL
rnd.env$raw_list <- NULL

rnd.env$nameID <- c(53,54,55,56,57,58,59,60,61)
names(rnd.env$nameID) <- c('ld','tr','di','pmf','nmf','fi','ti','pdm','ndm')

rnd.env$fun_id <- c(1:6)  #any undefined function will be mapped to zero
names(rnd.env$fun_id) <- c('calc_cap','calc_z','calc_res','calc_decay','calc_vlty','calc_bin')
rnd.env$known_mod_fun <- c(1:5)
names(rnd.env$known_mod_fun) <- c('calc_cap','calc_z','calc_decay','calc_lag','calc_bin')

rnd.env$prob$choices <- c('type','cap','math','scale','decay','bin')
rnd.env$prob$choices.wts <- c(0.1,0.3,0.0,0.3,0.2,0.4) 
rnd.env$prob$choices.bv <- c('type','cap','scale','decay')
rnd.env$prob$choices.bv.wts <- c(0.1,0.2,0.,0.2)
rnd.env$prob$raw_var <- c('retrange','ccd','c2c','ti')
rnd.env$prob$raw_var.wts <- c(0.4,0.2,0.2,0.2)
#rnd.env$prob$raw_var.wts <- c(0.4,0.3,0.3,0)
rnd.env$prob$raw_var.c2c.lags <- c(3,5,8,13,21,34,55,89,144)
rnd.env$prob$raw_var.ti.type <- c('adx','mf','fi')
rnd.env$prob$raw_var.ti.wts <- c(0.3,0.5,0.2)
rnd.env$prob$type <- c('ret','res','cmn','vlt','vol','vrs','file') #keep 'file' hard coded as last entry
rnd.env$prob$type.wts <- c(0.9,0.9,0.1,0.3,0.3,0.1,0.9)
rnd.env$prob$type.bv <- rnd.env$prob$type
rnd.env$prob$type.bv.wts <- c(0.1,0.1,0.1,0.1,0.1,0.1,0)
rnd.env$prob$cap <- c('cap_pct','zcap','abscap','none')
rnd.env$prob$cap.wts <- c(0.4,0.2,0.,0.3)
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
rnd.env$prob$bin.wts <- c(0.75,0.25)
rnd.env$prob$bin_pt1 <- c(-3,-2,-1,0,1) 
rnd.env$prob$bin_pt1.wts <- c(0.1,0.1,0.1,0.1,0.1)
rnd.env$prob$bin_pt2 <- c(-1,0,1,2,3) 
rnd.env$prob$bin_pt2.wts <- c(0.1,0.1,0.1,0.1,0.1)
rnd.env$prob$mod_bin <- c('new_var','none')
rnd.env$prob$mod_bin.wts <- c(0.8,0.2)

rnd.env$mod$bins <- c(-4,-3.5,-3,-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3,3.5,4)
rnd.env$mod$abscap <- c(.13,.08,.05,.03,.02,.01,.005,.003) #assumed to be returns
rnd.env$mod$cap_pct <- c(.001,.005,.01,.02,.03,.05,.08,.13,.21,.34)
rnd.env$mod$zcap <- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5.)
rnd.env$mod$decay <- c(.89,.55,.34,.21,.13,.08,.05,.03,.02,.01)


#currently being coded
rnd.env$prob$choices <- c('type','cap','scale','decay','bin')


                            