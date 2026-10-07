library(dlnm)
library(zoo)#插值的包
library(mgcv)
dayas_of_lag=1#之后天数，后边可以改成2或者3


data <- read.table("data_withair.txt",
                   header = FALSE,        # 第一行是否为列名
                   sep = "\t",           # 分隔符，如 "\t"、","、" "
                   stringsAsFactors = FALSE,  # 字符串不转因子
                   encoding = "UTF-8",   # 编码
                   na.strings = c("NA", ""),  # 缺失值标记
                   skip = 0)          # 读取行数

data_flip <- data[nrow(data):1, ]#数据上下翻转（行倒序调整）


#插值
data_flip[] <- lapply(data_flip, function(x) na.approx(x, na.rm = FALSE))

#最大最小归一化
data_flip[] <- lapply(data_flip, function(x) {
  if (is.numeric(x)) {
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  } else {
    x  # 非数值列原样返回
  }
})
names(data_flip) <- c('PM25','PM10','SO2','CO','NO2','O3','Temperature','Atmosphericpressure','Humidity','Windspeed','Minimumtemperature','Maximumtemperature','Visibility','Dewpoint','Precipitation','Totalcases')



cb.pm25 <- crossbasis(data_flip$PM25, lag = dayas_of_lag, 
                      argvar = list(fun = "poly", degree = dayas_of_lag),
                      arglag = list(fun = "integer"))
cb.pm10 <- crossbasis(data_flip$PM10, lag = dayas_of_lag, 
                      argvar = list(fun = "poly", degree = dayas_of_lag),
                      arglag = list(fun = "integer"))
cb.so2 <- crossbasis(data_flip$SO2, lag = dayas_of_lag, 
                     argvar = list(fun = "poly", degree = dayas_of_lag),
                     arglag = list(fun = "integer"))
cb.co <- crossbasis(data_flip$CO, lag = dayas_of_lag, 
                    argvar = list(fun = "poly", degree = dayas_of_lag),
                    arglag = list(fun = "integer"))

cb.no2 <- crossbasis(data_flip$NO2, lag = dayas_of_lag, 
                     argvar = list(fun = "poly", degree = dayas_of_lag),
                     arglag = list(fun = "integer"))

cb.o3 <- crossbasis(data_flip$O3, lag = dayas_of_lag, 
                    argvar = list(fun = "poly", degree = dayas_of_lag),
                    arglag = list(fun = "integer"))

cb.temperature <- crossbasis(data_flip$Temperature, lag = dayas_of_lag, 
                             argvar = list(fun = "poly", degree = dayas_of_lag),
                             arglag = list(fun = "integer"))
cb.atmosphericpressure <- crossbasis(data_flip$Atmosphericpressure, lag = dayas_of_lag, 
                                     argvar = list(fun = "poly", degree = dayas_of_lag),
                                     arglag = list(fun = "integer"))
cb.humidity <- crossbasis(data_flip$Humidity, lag = dayas_of_lag, 
                          argvar = list(fun = "poly", degree = dayas_of_lag),
                          arglag = list(fun = "integer"))
cb.windspeed <- crossbasis(data_flip$Windspeed, lag = dayas_of_lag, 
                           argvar = list(fun = "poly", degree = dayas_of_lag),
                           arglag = list(fun = "integer"))
cb.minimumtemperature <- crossbasis(data_flip$Minimumtemperature, lag = dayas_of_lag, 
                                    argvar = list(fun = "poly", degree = dayas_of_lag),
                                    arglag = list(fun = "integer"))
cb.maximumtemperature <- crossbasis(data_flip$Maximumtemperature, lag = dayas_of_lag, 
                                    argvar = list(fun = "poly", degree = dayas_of_lag),
                                    arglag = list(fun = "integer"))
cb.visibility <- crossbasis(data_flip$Visibility, lag = dayas_of_lag, 
                            argvar = list(fun = "poly", degree = dayas_of_lag),
                            arglag = list(fun = "integer"))
cb.dewpoint <- crossbasis(data_flip$Dewpoint, lag = dayas_of_lag, 
                          argvar = list(fun = "poly", degree = dayas_of_lag),
                          arglag = list(fun = "integer"))
cb.precipitation <- crossbasis(data_flip$Precipitation, lag = dayas_of_lag, 
                               argvar = list(fun = "poly", degree = dayas_of_lag),
                               arglag = list(fun = "integer"))
cb.totalcases <- crossbasis(data_flip$Totalcases, lag = dayas_of_lag, 
                            argvar = list(fun = "poly", degree = dayas_of_lag),
                            arglag = list(fun = "integer"))

data_flip$cb.pm25 <- cb.pm25
data_flip$cb.pm10 <- cb.pm10
data_flip$cb.so2  <- cb.so2
data_flip$cb.co   <- cb.co
data_flip$cb.no2  <- cb.no2
data_flip$cb.o3   <- cb.o3

data_flip$cb.temperature <- cb.temperature
data_flip$cb.atmosphericpressure <- cb.atmosphericpressure
data_flip$cb.humidity  <- cb.humidity
data_flip$cb.windspeed   <- cb.windspeed
data_flip$cb.minimumtemperature  <- cb.minimumtemperature
data_flip$cb.maximumtemperature   <- cb.maximumtemperature
data_flip$cb.visibility   <-cb.visibility
data_flip$cb.dewpoint   <- cb.dewpoint
data_flip$cb.precipitation   <-cb.precipitation
data_flip$cb.totalcases   <-cb.totalcases

# allcb <- cbind(cb.pm25, cb.pm10, cb.so2, cb.co, cb.no2, cb.o3,cb.temperature,cb.atmosphericpressure,cb.humidity,cb.windspeed,cb.minimumtemperature,cb.maximumtemperature,cb.visibility,cb.dewpoint,cb.precipitation,cb.totalcases)
# data_flip <- cbind(data_flip, allcb)

n <- nrow(data_flip)
cut <- floor(n * 0.8)
train <- data_flip[1:cut, ]          # 前80%
test  <- data_flip[(cut + 1):n, ]    # 后20%

model <- gam(train$PM25 ~ cb.pm25 + cb.pm10++cb.so2+cb.co+cb.no2+cb.o3+cb.temperature+cb.atmosphericpressure+cb.humidity+cb.windspeed+cb.minimumtemperature+cb.maximumtemperature+cb.visibility+cb.dewpoint+cb.precipitation+cb.totalcases,
             data = train, family = quasipoisson, method = "REML")
# cb_name <- "cb"
# cb_cols <- grep(cb_name, names(coef(model)))

pred_base <- predict(model, newdata = test, type = "response", se.fit = TRUE)
# pred_base$fit就是预测结果
result <- data.frame(
  真实值 = test$Totalcases,
  预测值 = pred_base$fit,
  下限   = pred_base$fit - 1.96 * pred_base$se.fit,
  上限   = pred_base$fit + 1.96 * pred_base$se.fit
)
write.table(result, file = "results_1.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
