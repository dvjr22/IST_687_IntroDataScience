rm(list=ls()) # clear work space
dev.off(dev.list()["RStudioGD"]) # clear plots

library(kernlab)
library(ggplot2)
library(e1071)
library(gridExtra)
library(grid)

getwd()
setwd("C:/Users/dvjr2/Documents")

dataAQ = airquality
summary(dataAQ)
str(dataAQ)

dataAQ = na.omit(dataAQ)
sum(is.na(dataAQ) == TRUE)

trngData = dataAQ[1:77, ]
testData = dataAQ[78:111, ]

plot(airquality$Solar.R, airquality$Ozone)
plot(airquality$Wind, airquality$Ozone)
plot(airquality$Temp, airquality$Ozone)
plot(airquality$Month, airquality$Ozone)

#dataAQ$Month = as.character(dataAQ$Month)
#tapply(airquality$Wind, airquality$Month, sum)

model01 = ksvm(Ozone ~ Solar.R + Wind + Temp + Month, data = trngData, kernel = "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
model01
model02 = ksvm(Ozone ~ Wind + Temp + Month, data = trngData, kernel = "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
model02

results = predict(model02, testData[ , 3:5])

P.Ozone. = testData$Ozone - results
P.Ozone = scale(P.Ozone)
P.Ozone = sqrt(P.Ozone*P.Ozone) + 1
resultsdf = cbind(testData$Wind, testData$Temp, P.Ozone)

resultsdf = as.data.frame(resultsdf)

colnames(resultsdf) = c("Wind", "Temp", "P.Ozone")

k = ggplot(resultsdf, aes(Temp, Wind)) + geom_point(size = P.Ozone)

hist(alpha(model02))[[1]]

model03 = svm(Ozone ~ Wind + Temp + Month, data = trngData)
model04 = lm(Ozone ~ Wind + Temp + Month, data = trngData)

svmResult = predict(model03, testData[ , 3:5])
lmResult = predict(model04, testData[ , 3:5])

P.Ozone.svm = testData$Ozone - svmResult
P.Ozone.svm = scale(P.Ozone.svm)
P.Ozone.svm = sqrt(P.Ozone.svm*P.Ozone.svm) + 1
resultsdf$P.Ozone.svm = P.Ozone.svm

s = ggplot(resultsdf, aes(Temp, Wind)) + geom_point(size = P.Ozone.svm)

P.Ozone.lm = testData$Ozone - lmresults
P.Ozone.lm = scale(P.Ozone.lm)
P.Ozone.lm = sqrt(P.Ozone.lm*P.Ozone.lm) + 1
resultsdf$P.Ozone.lm = P.Ozone.lm

l = ggplot(resultsdf, aes(Temp, Wind)) + geom_point(size = P.Ozone.lm)

grid.arrange(k, s, l, ncol = 2)

goodOzone = mean(trngData$Ozone)
trngData$GoodOzone = ifelse(trngData$Ozone >= goodOzone, 1, 0)

goodOzone = mean(testData$Ozone)
testData$GoodOzone = ifelse(testData$Ozone >= goodOzone, 1, 0)

gOzoneM = ksvm(GoodOzone ~ Wind + Temp + Month, data = trngData, kernel = "rbfdot", kpar = "automatic", C = 50, cross = 3, prob.model = TRUE, type = 'C-svc')
gOzoneM

gOzoneResult = predict(gOzoneM, testData[ , 3:5], type = 'response')
gOzoneResult

testData$gOzoneResult = gOzoneResult
ggplot(testData, aes(Temp, Wind)) + geom_point(size = GoodOzone, shape = gOzoneResult)


