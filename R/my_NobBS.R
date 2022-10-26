# Problem 1 = Datum muss Sonntag sein
# Problem 2 kann bisher nur maxdelays von 11 handeln
# Units undso






NobBS_for_respinow = function(data, now,mein_erster_Versuch = FALSE, units = "1 week", ageGroup = "00+",moving_window=NULL, max_D=11, cutoff_D=NULL, proportion_reported=1, quiet=TRUE,cutoff_D = TRUE,
                              specs=list(
                                dist=c("Poisson","NB"),
                                alpha1.mean.prior=0,
                                alpha1.prec.prior=0.001,
                                alphat.shape.prior=0.001,
                                alphat.rate.prior=0.001,
                                beta.priors=NULL,
                                beta_gain.priors = NULL,
                                beta_streich.priors = NULL,
                                param_names=NULL,
                                conf=0.95,
                                dispersion.prior=NULL,
                                nAdapt=1000,
                                nChains=1,
                                nBurnin=1000,
                                nThin=1,
                                nSamp=10000)) {
  # Check that "now" is entered as a Date
  if(inherits(now, "Date")==FALSE){
    stop("'Now' argument must be of datatype Date (as.Date)")
  }
  data$date = as.Date(data$date)
  # Check that "now" is possible in the sequence of reporting data
  if(!(now %in% data$date)){
    stop("The date `now` is not possible to estimate")
  }
  
  # Print date
  message(paste("Computing a nowcast for ",now))
  # Define "T", the length of dates between the first date of data and "now", making sure that "T" is unaffected by skipped-over dates in the time series
  # If the moving window is specified, "T" considers only the dates within the moving window; otherwise considers all historical data
  now.T <- ifelse(is.null(moving_window),length(seq(min(data$date),as.Date(now),by=units)),
                  moving_window)
  
  # Check the default arguments
  if (is.null(moving_window)) {
    moving_window <- now.T
  }
  if (is.null(max_D)) {
    max_D <- now.T-1 # ifelse(is.null(moving_window),now.T-1,moving_window-1)
  }
  if (is.null(cutoff_D)) {
    cutoff_D <- TRUE
  }
  if(quiet==TRUE){
    progress.bar <- "none"
  }
  if(quiet==FALSE){
    progress.bar <- "text"
  }
  
  # Check that proportion_reported is between 0,1
  if (proportion_reported > 1 | proportion_reported<=0){
    stop("The proportion_reported must be a number between (0,1].")
  }
  
  
  # Manipulate the control arguments -- NOCH GAR KEIN PLAN
  if ("Poisson"%in%(specs[["dist",exact=TRUE]])) { # if no distribution specified, take Poisson as default
    specs$dist <- "Poisson"
  }
  if (is.null(specs[["dist",exact=TRUE]])) {
    specs$dist <- "Poisson"
  }
  if (is.null(specs[["alpha1.mean.prior",exact=TRUE]])) {
    specs$alpha1.mean.prior <- 0
  }
  if (is.null(specs[["alpha1.prec.prior",exact=TRUE]])) {
    specs$alpha1.prec.prior <- 0.001
  }
  if (is.null(specs[["alphat.shape.prior",exact=TRUE]])) {
    specs$alphat.shape.prior <- 0.001
  }
  if (is.null(specs[["alphat.rate.prior",exact=TRUE]])) {
    specs$alphat.rate.prior <- 0.001
  }
  if (is.null(specs[["beta.priors",exact=TRUE]])) {
    specs$beta.priors <- rep(0.1, times=(max_D)+1)
  }
  if (is.null(specs[["param_names",exact=TRUE]])&(specs[["dist"]]=="Poisson")&(mein_erster_Versuch == FALSE)) {
    specs$param_names <- c( "lambda","alpha","beta.logged","tau2.alpha","sum.n")
  }
  
  # EINGEFÜGT FÜR SKELLAM
  if (is.null(specs[["param_names",exact=TRUE]])&(mein_erster_Versuch == TRUE)) {
    specs$param_names <- c("lambda_gain","lambda_streich","alpha","beta_gain.logged","beta_streich.logged","tau2.alpha","sum.n")
  }
  
  if (is.null(specs[["param_names",exact=TRUE]])&(specs[["dist"]]=="NB")) {
    specs$param_names <- c( "lambda","alpha","beta.logged","tau2.alpha","sum.n","r")
  }
  if (is.null(specs[["conf",exact=TRUE]])) {
    specs$conf <- 0.95
  }
  if (is.null(specs[["dispersion.prior",exact=TRUE]])&(specs[["dist"]]=="NB")) {
    specs$dispersion.prior <- c(0.001,0.001)
  }
  if (is.null(specs[["nAdapt",exact=TRUE]])) {
    specs$nAdapt <- 1000
  }
  if (is.null(specs[["nChains",exact=TRUE]])) {
    specs$nChains <- 1
  }
  if (is.null(specs[["nBurnin",exact=TRUE]])) {
    specs$nBurnin <- 1000
  }
  if (is.null(specs[["nThin",exact=TRUE]])) {
    specs$nThin <- 1
  }
  if (is.null(specs[["nSamp",exact=TRUE]])) {
    specs$nSamp <- 10000
  }
  # EINGEFÜGT FÜR SKELLAM
  if ((is.null(specs[["beta_gain.priors",exact=TRUE]])) & (mein_erster_Versuch == TRUE)) {
    specs$beta_gain.priors <- c(5,0.7,rep(0.08,max_D -1))
  }
  if ((is.null(specs[["beta_streich.priors",exact=TRUE]]))  & (mein_erster_Versuch == TRUE)) {
    specs$beta_streich.priors <- c(0.001,rep(0.1,max_D))
  }
  # Warnings
  if(max_D>(moving_window-1) | max_D > 11){
    stop("Maximum delay cannot be greater than the length of the moving window minus 1 time unit and/or cant be greater than the Respinow Data")
  }
  # ist hier einfacher zu finden
  unit.num <- switch(units, "1 day"=1,"1 week"=7)
  w.days <- max((moving_window-1)*unit.num,(now.T-1)*unit.num)
  #make the reporting triangle to how it was observed at now
  data_total_at_now = subset(data, age_group == ageGroup)
  lw = subset(data_total_at_now, (date == now))$week - moving_window 
  if(is.null(moving_window)){ 
    lw = 0
    }
  data_total_at_now = subset(data_total_at_now, date <= now & week > lw)
  
  # building the retrospective triangle
  observed_at_now = data_total_at_now[,6:17]
  l = 2
  for(row in seq(moving_window, moving_window - max_D +1 )){
    observed_at_now[row, l: (max_D + 1)] = NA
    l = l + 1
  }
  reporting.triangle = as.matrix(observed_at_now)
  if(mein_erster_Versuch == TRUE){
    rt_gain = reporting.triangle
    rt_gain[rt_gain < 0] = 0
    rt_streich = reporting.triangle
    rt_streich[rt_streich > 0] = 0
    rt_streich = abs(rt_streich)
  }
  
  if(mein_erster_Versuch == FALSE){
    reporting.triangle = abs(reporting.triangle)
    }
 
  # Run the JAGS model
  #WAS MACHT DAS???
  if(specs[["dist"]]=="Poisson"){
    params=c( "lambda","alpha","beta.logged","tau2.alpha","n","sum.n","sum.lambda")
  }
  if(specs[["dist"]]=="NB"){
    params=c( "lambda","alpha","beta.logged","tau2.alpha","n","sum.n","sum.lambda","r")
  }
  nAdapt = specs[["nAdapt"]] #default = 1000
  nChains = specs[["nChains"]] # default=1
  nBurnin = specs[["nBurnin"]] # default=1000
  nThin = specs[["nThin"]] # default=1
  nKeep = specs[["nSamp"]] # default=10,000
  nIter = nKeep * nThin
  
  if(specs[["dist"]]=="Poisson"){
    dataList = list(Today = now.T,
                    D = max_D,
                    n = reporting.triangle,
                    alpha1.mean.prior=specs$alpha1.mean.prior,
                    alpha1.prec.prior=specs$alpha1.prec.prior,
                    alphat.rate.prior=specs$alphat.rate.prior,
                    alphat.shape.prior=specs$alphat.shape.prior,
                    beta.priors=specs$beta.priors)
  }
  if(specs[["dist"]]=="Poisson" & (mein_erster_Versuch == TRUE)){
    dataList = list(Today = now.T,
                    D = max_D,
                    n_gain = rt_gain,
                    n_streich = rt_streich,
                    alpha1.mean.prior=specs$alpha1.mean.prior,
                    alpha1.prec.prior=specs$alpha1.prec.prior,
                    alphat.rate.prior=specs$alphat.rate.prior,
                    alphat.shape.prior=specs$alphat.shape.prior,
                    beta_gain.priors=specs$beta_gain.priors,
                    beta_streich.priors=specs$beta_streich.priors)
  }
  
  if(specs[["dist"]]=="NB"){
    dataList = list(Today = now.T,
                    D = max_D,
                    n = reporting.triangle,
                    alpha1.mean.prior=specs$alpha1.mean.prior,
                    alpha1.prec.prior=specs$alpha1.prec.prior,
                    alphat.rate.prior=specs$alphat.rate.prior,
                    alphat.shape.prior=specs$alphat.shape.prior,
                    beta.priors=specs$beta.priors,
                    dispersion.prior.shape=specs$dispersion.prior[1],
                    dispersion.prior.rate=specs$dispersion.prior[2])
  }
  print(specs$param_names)
  
  JAGSmodPois <- (system.file("JAGS", "nowcastPois.txt", package="NobBS")) # file.path(path.package('NobBS'),"nowcastPois.txt")
  JAGSmodNB <- (system.file("JAGS", "nowcastNB.txt", package="NobBS")) #file.path(path.package('NobBS'),"nowcastNB.txt")
  #JAGSmodMEINS <- (system.file("JAGS", "nowcastIrgendwas.txt", package="NobBS")) #file.path(path.package('NobBS'),"nowcastNB.txt")
  JAGSmodMEINS <- file.path("NobBS","inst","JAGS","nowcastIrgendwas.txt")
  file_ = if(specs[["dist"]]=="Poisson" & mein_erster_Versuch == FALSE) JAGSmodPois else if(specs[["dist"]]=="Poisson" & mein_erster_Versuch == TRUE) JAGSmodMEINS else JAGSmodNB
  nowcastmodel = jags.model(
    file = file_,
    data = dataList,
    n.chains = nChains,
    n.adapt = nAdapt,
    inits=list(.RNG.seed=1,.RNG.name="base::Super-Duper"), #Ok wo kommt das her??
    quiet=quiet)
  
  update( object = nowcastmodel, n.iter = nBurnin , progress.bar = progress.bar)
  
  lambda.output = coda.samples(
    model = nowcastmodel,
    variable.names =  if("sum.n"%in%specs$param_names) c(specs$param_names) else c(specs$param_names,"sum.n"),
    n.iter = nIter,
    thin = nThin,
    quiet=quiet,
    progress.bar=progress.bar)
  
  mymod.mcmc <- as.mcmc(lambda.output) # warum das??
  mymod.dat <- as.data.frame(as.matrix(mymod.mcmc))
  
  # Extract all hindcasts and 95% credible intervals
  t.extract <- (now.T-(now.T-1)):(now.T) # nowcast all weeks up through the present
  
  estimates <- matrix(NA, ncol=3, nrow=now.T,dimnames=list(NULL,c("estimate","lower","upper")))
  for(v in t.extract){
    estimates[v,1] <- median(mymod.dat[, grep(paste("sum.n[",v,"]",sep=""), colnames(mymod.dat), fixed=TRUE)])
    estimates[v,2] <- quantile((mymod.dat[, grep(paste("sum.n[",v,"]",sep=""), colnames(mymod.dat), fixed=TRUE)]),probs = c((1-specs$conf)/2,1-((1-specs$conf)/2)))[1]
    estimates[v,3] <- quantile((mymod.dat[, grep(paste("sum.n[",v,"]",sep=""), colnames(mymod.dat), fixed=TRUE)]),probs = c((1-specs$conf)/2,1-((1-specs$conf)/2)))[2]
  }
  
  # Estimates inflated by proportion reported
  estimates.inflated <- matrix(NA, ncol=3, nrow=now.T,dimnames=list(NULL,c("estimate_inflated","lower","upper")))
  for(v in t.extract){
    estimates.inflated[v,1] <- median(mymod.dat[, grep(paste("sum.n[",v,"]",sep=""), colnames(mymod.dat), fixed=TRUE)])/proportion_reported
    estimates.inflated[v,2] <- quantile((mymod.dat[, grep(paste("sum.n[",v,"]",sep=""), colnames(mymod.dat), fixed=TRUE)]),probs = c((1-specs$conf)/2,1-((1-specs$conf)/2)))[1]/proportion_reported
    estimates.inflated[v,3] <- quantile((mymod.dat[, grep(paste("sum.n[",v,"]",sep=""), colnames(mymod.dat), fixed=TRUE)]),probs = c((1-specs$conf)/2,1-((1-specs$conf)/2)))[2]/proportion_reported
  }
  
  # Combine nowcast estimates with: dates, number of cases reported at each date
  #build another reporting triangle because I´m dumb and forgot to include date in the first one
  data_reported_at_now = data_total_at_now
  l = 7
  for(row in seq(moving_window, moving_window - max_D +1 )){
    data_reported_at_now[row, l: 17] = NA
    l = l + 1
  }
  data_reported_at_now[is.na(data_reported_at_now)] <- 0
  if(mein_erster_Versuch == FALSE){
    data_reported_at_now$n.reported <- rowSums(abs(data_reported_at_now[6:17])) #hier auch abs muss man auch ändern
  } else {
    data_reported_at_now$n.reported <- rowSums(data_reported_at_now[6:17])
  }
  data_reported_at_now = select(data_reported_at_now, c(date,n.reported))
  colnames(data_reported_at_now) <- c("onset_date","n.reported")
  reported = data_reported_at_now
  

  
  
  # # # # #
  estimates <- data.frame(estimates, onset_date=(seq(as.Date(now)-w.days,as.Date(now),by=units))) %>%
    left_join(reported,by="onset_date")
  
  estimates.inflated <- data.frame(estimates.inflated, onset_date=(seq(as.Date(now)-w.days,as.Date(now),by=units))) %>%
    left_join(reported,by="onset_date")
  
  t <- now.T
  
  parameter_extract <- matrix(NA, nrow=10000)
  ### HIER FÜR SKELLAM MODELL
  if("lambda_gain"%in%specs$param_names){
    parameter_extract <- cbind(parameter_extract,mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("lambda_gain[",t,",",sep="")))))
    parameter_extract <- cbind(parameter_extract,mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("lambda_streich[",t,",",sep="")))))
  }
  if("beta_gain.logged"%in%specs$param_names){
    betas_gain.logged<- matrix(NA,nrow=10000,ncol=(max_D+1))
    betas_streich.logged<- matrix(NA,nrow=10000,ncol=(max_D+1))
    dimnames(betas_gain.logged) = list(NULL,c(paste("Beta_gain",c(0:max_D))))
    dimnames(betas_streich.logged) = list(NULL,c(paste("Beta_streich",c(0:max_D))))
    for(d in 0:max_D){
      betas_gain.logged[,(d+1)] <- (mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("beta_gain.logged[",(d+1),"]",sep="")))))[,1]
      betas_streich.logged[,(d+1)] <- (mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("beta_streich.logged[",(d+1),"]",sep="")))))[,1]
    }
    parameter_extract <- cbind(parameter_extract,betas_gain.logged)
    parameter_extract <- cbind(parameter_extract,betas_streich.logged)
  }
  
  if("lambda"%in%specs$param_names){
    parameter_extract <- cbind(parameter_extract,mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("lambda[",t,",",sep="")))))
  }
  if("beta.logged"%in%specs$param_names){
    betas.logged<- matrix(NA,nrow=10000,ncol=(max_D+1))
    dimnames(betas.logged) = list(NULL,c(paste("Beta",c(0:max_D))))
    for(d in 0:max_D){
      betas.logged[,(d+1)] <- (mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("beta.logged[",(d+1),"]",sep="")))))[,1]
    }
    parameter_extract <- cbind(parameter_extract,betas.logged)
  }
  if("alpha"%in%specs$param_names){
    parameter_extract <- cbind(parameter_extract,mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("alpha[",t,sep="")))))
  }
  if("tau2.alpha"%in%specs$param_names){
    parameter_extract <- cbind(parameter_extract,mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with("tau2.alpha"))))
  }
  
  #log.beta.td1 <- mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("beta.logged[1]",sep=""))))
  #log.beta.td2 <- mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("beta.logged[2]",sep=""))))
  #log.beta.td3 <- mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("beta.logged[3]",sep=""))))
  #alpha.last <- mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("alpha[",t,sep=""))))
  #tau2.alpha <- mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with("tau2.alpha")))
  
  #parameter_extract <- cbind(pi.logged.td1,pi.logged.td2,pi.logged.td3,
  #                          alpha.last,tau2.alpha)
  
  nowcast.post.samps <- (mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("sum.n[",t,sep="")))))[,1]
  
  # nowcast_results <<- UPDATE: do not save to global environment; user will have to do this
  
  list(estimates=estimates,estimates.inflated=estimates.inflated, nowcast.post.samps=nowcast.post.samps,params.post=parameter_extract[,2:ncol(parameter_extract)])
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}
