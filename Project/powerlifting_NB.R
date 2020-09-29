# powerlifting_NB.R
# Clean openpowerlifting.csv - https://www.kaggle.com/open-powerlifting/powerlifting-database

library(randomForest)
library(e1071)
library(cluster)

rm(list=ls()) # clear work space
dev.off(dev.list()["RStudioGD"]) # clear plots

# =============================================================================================================
# Import Data
# =============================================================================================================

filepath <- "~/Documents/R/diego/clean_power_lifting.csv"
dat <- read.csv(file = filePath, header=TRUE, sep=",", stringsAsFactors = FALSE) 
str(dat)
summary(dat)
dat$Sex <- as.factor(dat$Sex)
dat$WeightClassKg <- as.factor(dat$WeightClassKg)
# Note:
# read.csv() set argument stringsAsFactors = FALSE.  This way you don't import character vectors as factors.
# You can then change the vectors you want as factors implicitly 

# =============================================================================================================
# Clean and Prepare Data
# =============================================================================================================
smalldat <- dat[ , c(-1,-2,-3, -5,-6,-8,-13)] # get rid of unwanted cols
# removed the factor variable WeightClassKg as well
str(smalldat)

# How many NAs or NaNs
(sum(is.na(smalldat))/2112532)*100
(sum(is.na(smalldat)))*100
# 5% of the data is NAs - is this acceptable?
# at the very least its worth mentioning when you report accuracy of your models
smalldat <- na.omit(smalldat)
str(smalldat)
# it may be worth exploring changing NAs to values instead of just removing them
# we've reduced our data set by almost 20%

# Normalize numerical vectors using Min/Max normalization
# You could plot some histograms to see if this is really necessary but this is typical of most data this
# large
max <- apply(smalldat[,2:6] , 2 , max)
min <- apply(smalldat[,2:6], 2 , min)
scaleddat <- as.data.frame(cbind(smalldat$Sex, scale(smalldat[,2:6], center = min, scale = max - min)))

# Split data
samplesize = 0.70 * nrow(smalldat)
set.seed(80) ## for reproducability
index = sample(seq_len(nrow(smalldat)), size = samplesize ) ## we want to sample random observations

traindat <- smalldat[index, ]
testdat <- smalldat[-index, ]

# =============================================================================================================
# Build Random Forest Model
# ?randomForest
# =============================================================================================================
rfModel <- randomForest(Sex ~ ., data = traindat, proximity = TRUE)
print(rfModel)

## Look at variable importance: Higher values mean more important
round(importance(rfModel), 2)

predResults <- predict(rfModel, testdat)
table(predResults) ## view what observations were predicted male and female
table(testdat$Sex) ## view how many actual male and female observations
t <- table(predResults, testdat$Sex) ## view a confusion matrix
t
t[1,1]/sum(testdat$Sex == "F") ## model was ~82% accurate at predicting Females
t[2,2]/sum(testdat$Sex == "M") ## model was ~96% accurate at predicting Males

(t[1,1]+t[2,2])/length(testdat$Sex) ## Overall model was ~92% accurate at predicting Sex  
# (not bad, can probably do some tweaking to improve)





