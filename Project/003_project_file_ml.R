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

powerLifting = powerLifting[sample(nrow(powerLifting)), ] # randomize
trngData = powerLifting[1:ceiling(nrow(powerLifting)*.7) , ]
testData = powerLifting[(ceiling(nrow(powerLifting)*.7)+1):nrow(powerLifting), ]

#divide data by sex
#f_powerLifting = powerLifting[powerLifting$Sex == 'F', ]
#m_powerLifting = powerLifting[powerLifting$Sex == 'M', ]

# use 70% of data to train model
#trngData = rbind(f_powerLifting[1:ceiling(nrow(f_powerLifting)*.7) , ], m_powerLifting[1:ceiling(nrow(m_powerLifting)*.7) , ] )

# sep data to be trained
#predict = trngData[ , 1]
#summary = trngData[ , -1] # remove gender col

# train the model
#model = randomForest(summary, predict)
modelRF = randomForest(trngData[ , -1], trngData[ , 1]) # independent, dependent
summary(modelRF)

round(importance(modelRF), 2) # importance of variables, high is better

# data to compare prediction
#prediction = rbind(f_powerLifting[(ceiling(nrow(f_powerLifting)*.7)+1):nrow(f_powerLifting) , ], m_powerLifting[(ceiling(nrow(m_powerLifting)*.7)+1):nrow(m_powerLifting) , ] )

# make predictions
#results = predict(model, prediction[ , -1])
results = predict(modelRF, testData[ ,  -1])
#x = as.factor(predict(model, prediction[ , -1]))

table(results)
#table(prediction$Sex)
table(testData$Sex)
#matrix = table(results, prediction$Sex) # confusion matrix
matrix = table(results, testData$Sex)
matrix

# male vs female accuracy
#print(paste('Accuracy:', matrix[1,1]/sum(prediction$Sex == "F")))  
print(paste('Female Predict Accuracy:', matrix[1,1]/sum(testData$Sex == "F"))) 

#print(paste('Accuracy:', matrix[2,2]/sum(prediction$Sex == "M")))  
print(paste('Male Predict Accuracy:', matrix[2,2]/sum(testData$Sex == "M"))) 

# Overall accuracy
#(matrix[1,1] + matrix[2,2])/length(prediction$Sex ) # overall 92.1% accurate at gender prediction
print(paste('Overall Accuracy: ', (matrix[1,1] + matrix[2,2])/length(testData$Sex )))

############################################################################################################


# change F/M to 1/0
#powerLifting = ifelse(powerLifting$Sex == 'F', 1, 0)
trngData$Sex = ifelse(trngData$Sex == 'F', 1, 0)
testData$Sex = ifelse(testData$Sex == 'F', 1, 0)

# use 70% of data to train model
#trngData = rbind(f_powerLifting[1:ceiling(nrow(f_powerLifting)*.7) , ], m_powerLifting[1:ceiling(nrow(m_powerLifting)*.7) , ] )
#prediction = rbind(f_powerLifting[(ceiling(nrow(f_powerLifting)*.7)+1):nrow(f_powerLifting) , ], m_powerLifting[(ceiling(nrow(m_powerLifting)*.7)+1):nrow(m_powerLifting) , ] )

modelGLM = glm(Sex ~ BodyweightKg + BestSquatKg + BestBenchKg + BestDeadliftKg, data = trngData, family = "binomial")
summary(modelGLM) # all variables are significant

#prediction = prediction[sample(nrow(prediction)), ] # randomize

accuracy = .5
result = predict(modelGLM, testData[ , 2:5], type = "response")
result = ifelse(result > accuracy, 1, 0)
Error = mean(result != testData[ , 1])
print(paste('Overall Accuracy:', 1  - Error))


