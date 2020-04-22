job1 = read.csv("~/jobs.csv")

library("tseries")
library(igraph)

#creating time series object
ts_jobs = ts(job1$Total.Filled.Jobs,start = c(2004,1),frequency = 4)
plot(ts_jobs)

#plotting monthly mean
plot(aggregate(ts_jobs,FUN = mean))

#plotting boxplot
boxplot(ts_jobs~cycle(ts_jobs))

#decomposing using stl()
stl_jobs = stl(ts_jobs,s.window = "periodic")
plot(stl_jobs)
library("forecast")

#finding trend
trend_jobs = ma(ts_jobs,order = 4,centre = T)
plot(trend_jobs)

#finding seasonality
ts.comp = stl_jobs$time.series[,1]
plot(ts.comp)

#detrending 
detrend_ts = ts_jobs/trend_jobs
plot(as.ts(detrend_ts))
m_jobs = t(matrix(data = detrend_ts, nrow = 4)) #finding out the seasonality(1)
seasonal_jobs = colMeans(m_jobs, na.rm = T) #finding out the seasonality(2)
plot(as.ts(rep(seasonal_jobs,4)))
random_jobs = ts_jobs / (trend_jobs * seasonal_jobs) #finding out the randomness in the data
plot(random_jobs)

#first 75% values
job2<-job1[1:69,]

job_75 = ts(job2$Total.Filled.Jobs,start = c(2004,1),frequency = 12)

#Holt Winters
holt_job = hw(job_75,h = 23,seasonal = "multiplicative")
summary(holt_job)

calculated_job = holt_job$mean

#remaining 25% values
job3 = job1[70:92,]
actual_data = ts(job3$Total.Filled.Jobs,start = c(2010,1),frequency = 12)

#comparing actual and predicted data
plot(actual_data,col = "blue",ylab = "Jobs")
lines(calculated_job,col = "red")

error <- (calculated_job - actual_data)
#finding root mean square
rmse = function(error)
{ sqrt(mean((error)^2))}

rmse(error)

plot(job_75)

plot(diff(job_75))
#stationarity of data
adf.test(diff(job_75), alternative="stationary", k=0)

#finding autocorrelation
acf(diff(job_75))
pacf(diff(job_75))

plot(diff(job_75),col = "blue",ylab = "jobs")

fit <- arima(diff(job_75), c(1, 1, 1),seasonal = list(order = c(1, 1, 1), period = 12))
pred2 <- predict(fit,n.ahead = 3*4,se.fit = "FALSE")
lines(pred2,col="red")

error_job<- (pred2-(diff(actual_data)))
rmse(error_job)

#for p=0,q=0,d=0 the model will give thw minimum rmse
fit <- arima(diff(job_75), c(1, 1, 1),seasonal = list(order = c(0, 0, 0), period = 12))
pred2 <- predict(fit,n.ahead = 3*4,se.fit = "FALSE")
error_job<- (pred2-(diff(actual_data)))
rmse(error_job)




