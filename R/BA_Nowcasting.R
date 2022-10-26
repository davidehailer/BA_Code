# max D ist noch nen rießen Problem (cutoff, priors usw.)
Nowcasting = function(data, now, model = "Skellam", unit = "1 week", ageGroup = "00+", moving_window=NULL, max_D= NULL, quiet = TRUE, conf = 0.95,
                      specs_mcmc = list(nAdapt=1000,nChains=1,nBurnin=1000,nThin=1,nSamp=10000),
                      specs_prior = list("alpha1.mean.prior" = NULL, "alpha1.prec.prior" = NULL, "alphat.rate.prior" = NULL, "alphat.shape.prior" = NULL,
                                         "beta.priors" = NULL, "psi.shape.prior" = NULL, "psi.rate.prior" = NULL, "prec.shape.prior" = NULL,
                                         "prec.rate.prior" = NULL)){
  check_params(data, now)
  unit.num = ifelse(unit == "1 week",7,1)
  w.days = (moving_window-1)*unit.num
  if(!is.null(moving_window)){from_ = now - (moving_window * unit.num)}
  data = preprocess_rep_tri(data, ageGroup = ageGroup, from = from_, to = now)
  moving_window = length(data$date)
  message(paste("Computing a nowcast for ",now))
  if(!is.null(max_D)) {data =  data[1:(max_D + 2)]} 
  rt = reconstruct_rt(data, now)
  dataList = list(Today = moving_window, D = max_D, n = rt[-1])
  if(model == "Skellam"){
    file = file.path("JAGS","nowcastSkellam.txt")
    dataList["alpha1.mean.prior"] = ifelse(is.null(specs_prior$"alpha1.mean.prior"),0,specs_prior$"alpha1.mean.prior") 
    dataList["alpha1.prec.prior"] = ifelse(is.null(specs_prior$"alpha1.prec.prior"),0.001,specs_prior$"alpha1.prec.prior")
    dataList["alphat.rate.prior"] = ifelse(is.null(specs_prior$"alphat.rate.prior"),0.001,specs_prior$"alphat.rate.prior")
    dataList["alphat.shape.prior"] = ifelse(is.null(specs_prior$"alphat.shape.prior"),0.001,specs_prior$"alphat.shape.prior")
    dataList["psi.shape.prior"] = ifelse(is.null(specs_prior$"psi.shape.prior"),0.001,specs_prior$"psi.shape.prior")
    dataList["psi.rate.prior"] = ifelse(is.null(specs_prior$"psi.rate.prior"),0.001,specs_prior$"psi.rate.prior")
    dataList$beta.priors[1:(max_D+1)] = ifelse(is.null(specs_prior$"beta.priors"),rep(0.1, times= (max_D) +1),specs_prior$"beta.priors")
    dataList$p_pos.prior[1:(max_D+1)] = ifelse(is.null(specs_prior$"p_pos.prior"),rep(0.5, times=(max_D) +1),specs_prior$"p_pos.prior")
    progress.bar = ifelse(quiet == TRUE, "none","text")
    param_names = c("my","alpha","beta.logged","tau2.alpha","sum.n")
  }
  if(model == "discreteNormal"){
    file = file.path("JAGS","nowcast_discr_norm.txt")
    dataList["alpha1.mean.prior"] = ifelse(is.null(specs_prior$"alpha1.mean.prior"),0,specs_prior$"alpha1.mean.prior") 
    dataList["alpha1.prec.prior"] = ifelse(is.null(specs_prior$"alpha1.prec.prior"),0.001,specs_prior$"alpha1.prec.prior")
    dataList["alphat.rate.prior"] = ifelse(is.null(specs_prior$"alphat.rate.prior"),0.001,specs_prior$"alphat.rate.prior")
    dataList["alphat.shape.prior"] = ifelse(is.null(specs_prior$"alphat.shape.prior"),0.001,specs_prior$"alphat.shape.prior")
    dataList["prec.shape.prior"] = ifelse(is.null(specs_prior$"psi.shape.prior"),0.001,specs_prior$"psi.shape.prior")
    dataList["prec.rate.prior"] = ifelse(is.null(specs_prior$"psi.rate.prior"),0.001,specs_prior$"psi.rate.prior")
    dataList$beta.priors[1:(max_D+1)] = ifelse(is.null(specs_prior$"beta.priors"),rep(0.1, times= (max_D) +1),specs_prior$"beta.priors")
    progress.bar = ifelse(quiet == TRUE, "none","text")
    param_names = c("my","alpha","beta.logged","tau2.alpha","sum.n")
  }
  nowcastmodel = jags.model(
    file = file,
    data = dataList,
    n.chains = specs_mcmc$nChains,
    n.adapt = specs_mcmc$nAdapt,
    inits=list(.RNG.seed=1,.RNG.name="base::Super-Duper"), #Ok wo kommt das her??
    quiet=quiet)
  
  update( object = nowcastmodel, n.iter = specs_mcmc$nBurnin , progress.bar = progress.bar)
  
  lambda.output = coda.samples(
    model = nowcastmodel,
    variable.names =  param_names,
    n.iter = specs_mcmc$nSamp * specs_mcmc$nThin,
    thin = specs_mcmc$nThin,
    quiet=quiet,
    progress.bar=progress.bar)
  
  mymod.mcmc <- as.mcmc(lambda.output) 
  mymod.dat <- as.data.frame(as.matrix(mymod.mcmc))
  
  t.extract <- (1):(moving_window) # nowcast all weeks up through the present
  
  estimates <- matrix(NA, ncol=3, nrow=moving_window,dimnames=list(NULL,c("estimate","lower","upper")))
  for(v in t.extract){
    estimates[v,1] <- median(mymod.dat[, grep(paste("sum.n[",v,"]",sep=""), colnames(mymod.dat), fixed=TRUE)])
    estimates[v,2] <- quantile((mymod.dat[, grep(paste("sum.n[",v,"]",sep=""), colnames(mymod.dat), fixed=TRUE)]),probs = c((1-conf)/2,1-((1-conf)/2)))[1]
    estimates[v,3] <- quantile((mymod.dat[, grep(paste("sum.n[",v,"]",sep=""), colnames(mymod.dat), fixed=TRUE)]),probs = c((1-conf)/2,1-((1-conf)/2)))[2]
  }
  rt$sum = rowSums(rt[-1], na.rm = TRUE)
  reported = rt[c("date","sum")]
  colnames(reported) <- c("onset_date","reported_at_now")
  reported$reported_overall = rowSums(data[-1],na.rm = TRUE)
  reported$onset_date = as.Date(reported$onset_date)
  estimates <- data.frame(estimates, onset_date= reported$onset_date) %>%
    left_join(reported,by="onset_date")
  
  parameter_extract <- matrix(NA, nrow=10000)
  if(model == "Skellam"){
    parameter_extract = cbind(parameter_extract,mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("my[",moving_window,",",sep="")))))
    betas.logged = matrix(NA,nrow=10000,ncol=(max_D+1))
    dimnames(betas.logged) = list(NULL,c(paste("Beta",c(0:max_D))))
    for(d in 0:max_D){
      betas.logged[,(d+1)] <- (mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("beta.logged[",(d+1),"]",sep="")))))[,1]
    }
    parameter_extract = cbind(parameter_extract,betas.logged)
    parameter_extract = cbind(parameter_extract,mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("alpha[",moving_window,sep="")))))
    parameter_extract = cbind(parameter_extract,mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with("tau2.alpha"))))
  }
  if(model == "discreteNormal"){
    parameter_extract = cbind(parameter_extract,mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("my[",moving_window,",",sep="")))))
    betas.logged = matrix(NA,nrow=10000,ncol=(max_D+1))
    dimnames(betas.logged) = list(NULL,c(paste("Beta",c(0:max_D))))
    for(d in 0:max_D){
      betas.logged[,(d+1)] <- (mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("beta.logged[",(d+1),"]",sep="")))))[,1]
    }
    parameter_extract = cbind(parameter_extract,betas.logged)
    parameter_extract = cbind(parameter_extract,mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("alpha[",moving_window,sep="")))))
    parameter_extract = cbind(parameter_extract,mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with("tau2.alpha"))))
  }
  
  nowcast.post.samps = (mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("sum.n[",moving_window,sep="")))))[,1]
  
  list(estimates=estimates, nowcast.post.samps=nowcast.post.samps,params.post=parameter_extract[,2:ncol(parameter_extract)])
  
}


