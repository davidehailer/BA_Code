#this needs the estimates list
evaluate_one_nowcast = function(nc_data, show_reported_at_now = FALSE, title = ""){
  nc = nc_data$estimates
  ggplot(nc) + geom_line(aes(onset_date,estimate,col="Nowcast estimate"),linetype="longdash") +
    geom_line(aes(onset_date,reported_overall,col="current values"),linetype="solid") +
    scale_colour_manual(name="",values=c("black","indianred3","blue"))+
    theme_classic()+
    geom_ribbon(fill="indianred3",aes(x = onset_date,ymin=lower, 
                                      ymax=upper),alpha=0.3)+
    xlab("Date") + ylab("Estimated cases") +
    ggtitle(title) + 
    if(show_reported_at_now){geom_line(aes(onset_date,reported_at_now,col="Reported"),linetype="solid")} 
}
# TODO mutiple nowcasts 
  
  
  
  
  

library(dplyr)
plot_diff_between_real_and_reported = function(data, maxD = 10){
  data = preprocess_rep_tri(data, ageGroup  = "00+")
  date_seq = data$date
  value_at_now = rowSums(data[,2:length(colnames(data))],na.rm = TRUE)
  under_or_over = c(data[,2]) - value_at_now
  under_or_over[under_or_over <= 0 ] = "red"
  under_or_over[under_or_over > 0 ] = "blue"
  plot(as.Date(date_seq),c(value_at_now), type = "l", col = under_or_over, xlab = "Date", ylab = "SARI Cases",main = "Visualization of reporting delay")
  for(d in date_seq){
    rt_at_d = reconstruct_rt(data, as.Date(d))
    dates = seq(as.Date(d) - (maxD *7), as.Date(d), by = "1 week")
    rt_at_d = subset(rt_at_d, as.Date(date) %in% as.Date(dates))
    value_at_d = rowSums(rt_at_d[,2:length(colnames(rt_at_d))],na.rm = TRUE)
    lines(as.Date(rt_at_d$date),c(value_at_d), col = "red")
  }
  #for visbility reasons
  lines(as.Date(date_seq),c(value_at_now), type = "l", col = under_or_over,lwd=2.0)
  #legend(as.Date("2022-01-01"),1000, legend = c("True Timeseries of cases", "Timeseries as reported at time t"), col = c("blue","red"))
}

