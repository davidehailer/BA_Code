#specifying the reporting triangle and returning it in the form of [date, values]
preprocess_rep_tri = function(data, ageGroup = "00+", from = NULL, to = NULL){
  if(!is.null(from) ){
    data = subset(data,date >= from)
  }
  if(!is.null(to) ){
    data = subset(data,date <= to)
  }
  data = subset(data,age_group == ageGroup)
  data = data[,5:length(colnames(data))]
  data
}
#reconstructing the reporting traingle at time t
reconstruct_rt = function(rt, t){
  rt = subset(rt, date <= t)
  lenrow = length(rt$date)
  width_of_triangle = length(colnames(rt)) - 1
  l = 2
  for(row in seq(lenrow,lenrow - (width_of_triangle-2) )){
    rt[row, l : width_of_triangle + 1] = NA
    l = l + 1
  }
  rt
}
#
check_params = function(data, now, model, unit, ageGroup, moving_window, max_D, quiet, conf,specs_mcmc,specs_prior){
  if(inherits(now, "Date")==FALSE){
    stop("'Now' argument must be of datatype Date (as.Date)")
  }
  if(!(now %in% seq(as.Date(dplyr::first(data$date)),as.Date(dplyr::last(data$date)), by = "1 day"))){
    stop("The date `now` is not possible to estimate")
  }

}

