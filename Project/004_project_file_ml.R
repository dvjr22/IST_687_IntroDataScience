# 003_project_file_ml.R
# @version: 1
# @author:  Diego Valdes
# @date:  Feb 14, 2019
# IST 687
# Clean openpowerlifting.csv - https://www.kaggle.com/open-powerlifting/powerlifting-database


library(randomForest)
#library(e1071)
#library(cluster)

rm(list=ls()) # clear work space
dev.off(dev.list()["RStudioGD"]) # clear plots

getwd()
setwd("C:/Users/dvjr2/Google Drive/Documents/Syracuse/IST_687/Project/")
filePath = "clean_power_lifting.csv"

# read data and take a look
powerLiftingOG <- read.csv(file = filePath, header=TRUE, sep=",", stringsAsFactors = FALSE) 
str(powerLiftingOG)
summary(powerLiftingOG)

powerLiftingOG$Sex = as.factor(powerLiftingOG$Sex)

powerLifting = powerLiftingOG[ , c(-1,-2,-3, -5,-6, -8, -12, -13)] # get rid of unwanted cols
powerLifting = na.omit(powerLifting) # clear NAs to run model

#divide data by sex
f_powerLifting = powerLifting[powerLifting$Sex == 'F', ]
f_powerLifting$Sex = 1
m_powerLifting = powerLifting[powerLifting$Sex == 'M', ]
m_powerLifting$Sex = 0

# use 70% of data to train model
trngData = rbind(f_powerLifting[1:ceiling(nrow(f_powerLifting)*.7) , ], m_powerLifting[1:ceiling(nrow(m_powerLifting)*.7) , ] )
prediction = rbind(f_powerLifting[(ceiling(nrow(f_powerLifting)*.7)+1):nrow(f_powerLifting) , ], m_powerLifting[(ceiling(nrow(m_powerLifting)*.7)+1):nrow(m_powerLifting) , ] )


model = glm(Sex ~ BodyweightKg + BestSquatKg + BestBenchKg + BestDeadliftKg, data = trngData, family = "binomial")
summary(model) # all variables are significant


prediction = prediction[sample(nrow(prediction)), ] # randomize

accuracy = .5
result = predict(model, prediction[1:500, 2:5], type = "response")
result = ifelse(result > accuracy, 1, 0)
Error = mean(result != prediction[1:500, 1])
print(paste('Accuracy', 1  - Error))
