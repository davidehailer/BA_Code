library(NobBS)
library(rjags)
library(coda)
library(dplyr)
library(magrittr)
library(stats)
library(ggplot2)
data("denguedat")
denguedat
sari_rt = read.csv("https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Data/main/data/SARI/SARI_reporting_triangle.csv")

rp = NobBS_for_respinow(data = sari_rt,mein_erster_Versuch = TRUE, now = as.Date("2022-06-26"), quiet = FALSE)
a = NobBS_d(denguedat, as.Date("1990-10-01"),units="1 week",onset_date="onset_week",report_date="report_week",quiet=FALSE)
l = NobBS::NobBS(denguedat, as.Date("1990-10-01"),units="1 week",onset_date="onset_week",report_date="report_week")
plot_diff_between_real_and_reported(sari_rt)
d = preprocess_rep_tri(sari_rt, ageGroup = "00+")
reconstruct_rt(d,as.Date("2022-07-31"))

a = Nowcasting(sari_rt,now = as.Date("2022-09-01"), model = "discreteNormal",max_D = 11, quiet = FALSE, moving_window = 23)
a
a$estimates
subset(sari_rt, age_group == "00+")

evaluate_one_nowcast(a)






as.Date("2022-06-25") %in% seq(as.Date(dplyr::first(sari_rt$date)),as.Date(dplyr::last(sari_rt$date)), by = "1 day")







evd = read.csv("NobBS/data/euro_vs_dollar.csv")
evd$rv = 1/as.numeric(evd$value)
plot(as.Date(evd$date),evd$rv, type = "l", xlab = "Time", ylab = "Exchange Rate", main = "US-Dollar vs Euro, Daily", width = 14, height = 8)
library(quantmod)
SPX <- getSymbols("^GSPC", from = "1970-01-01")
d = rownames(as.data.frame(GSPC))
plot(as.Date(d), GSPC$GSPC.Volume, type = "l", xlab = "Time", ylab = "Trading Volume", main = "S&P 500 Trading Volume, Daily")
ddd = read.csv("NobBS/data/3month_yield.csv")
plot(as.Date(ddd$DATE), ddd$IR3TIB01DEM156N, type = "l", xlab = "Time", ylab = "Interest Rate", main = "German Three-month Market Rate, Monthly")

library(skellam)
a = Nowcasting(sari_rt, now = as.Date("2022-07-10"), moving_window = 20, max_D = 5)
a
for(t in seq(1,20)){
  for(d in seq(0,10)){
    print(a[t,(d+1)])
  }
}
rep(0.1, times= (5) +1)
dataList = list(c="c")
x = 1
dataList$beta.priors[1:max_D + 1] = ifelse(x > 0,rep(0.1, times= (max_D) +1),100)
dataList



dat_for_eval = subset(sari_rt, age_group == "00+")
dat_for_eval = subset(dat_for_eval, date <= as.Date("2022-07-24") & date >= as.Date("2022-04-17"))
dat_1_day_hindcast = subset(sari_rt, date <= as.Date("2022-07-17") & date >= as.Date("2022-04-10") & age_group == "00+" )
estimates = NULL
upper = NULL
lower = NULL
for(now in as.list(date_seq)){
  rp = NobBS_for_respinow(data = sari_rt,mein_erster_Versuch = TRUE, now = as.Date(now), quiet = FALSE)
  estimates = c(estimates,dplyr::nth(rp$estimates$estimate, -2))
  upper = c(upper,dplyr::nth(rp$estimates$upper, -2))
  lower = c(lower,dplyr::nth(rp$estimates$lower, -2))
}
dat_for_eval$res = rowSums(dat_for_eval[6:17])

dat_1_day_hindcast$res = rowSums(dat_1_day_hindcast[6:17])

full_evaluation_frame = data.frame(date = date_seq, result = dat_for_eval$res, estimate = estimates, upper = upper, lower = lower)

full_evaluation_h1 = data.frame(date = date_seq, result = dat_1_day_hindcast$res, estimate = estimates, upper = upper, lower = lower)

full_evaluation_frame = subset(full_evaluation_frame, estimate > 300 )

full_evaluation_h1 = subset(full_evaluation_h1, estimate > 300 )


ggplot(full_evaluation_h1) + geom_line(aes(date,estimate,col="Nowcast estimate"),linetype="longdash") +
  geom_line(aes(date,result,col="Reported"),linetype="solid") +
  scale_colour_manual(name="",values=c("indianred3","black"))+
  theme_classic()+
  geom_ribbon(fill="indianred3",aes(x = date,ymin=lower, 
                                    ymax=upper),alpha=0.3)+
  xlab("Date") + ylab("Estimated cases") +
  ggtitle("erster naiver Versuch mit Annahme (gain oder streich = 0) - Hindcast 1 Woche")


d = subset(sari_rt, age_group == "00+")
d = subset(d, date <= as.Date("2022-07-24"))
sum = rowSums(d[6:16])
first2 = rowSums(d[6:7])

(sum(abs(sum - first2))/length(sum))/mean(sum)






