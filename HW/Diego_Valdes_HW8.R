rm(list=ls()) # clear work space
dev.off(dev.list()["RStudioGD"]) # clear plots

suppressWarnings(require(openxlsx))
suppressWarnings(require(ggplot2))

# Downloaded from: 
# http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/frames/frame.html
# File saved as .xlsx 

setwd("C:/Users/dvjr2/Google Drive/Documents/Syracuse/IST_687/HW/")
fileName = "mlr01.xlsx"

fawnData = read.xlsx(fileName)
colnames(fawnData) = c("FawnCount", "AntelopeAdultPop", "Percipitation", "WinterSev")
summary(fawnData)
str(fawnData)

# fawn vs adult antelopes
ggplot(fawnData, aes(AntelopeAdultPop, FawnCount)) + geom_jitter() +
  xlab("Adult Antelope") + ylab("Fawns")

# fawn vs precipitation
ggplot(fawnData, aes(Percipitation, FawnCount)) + geom_jitter() +
  xlab("Percipitation") + ylab("Fawns")

# fawn vs winter
ggplot(fawnData, aes(WinterSev, FawnCount)) + geom_jitter() +
  xlab("Winter Severity") + ylab("Fawns")

# linear models

# fawns ~ winter
model_001 = lm(FawnCount ~ WinterSev, data = fawnData)
summary(model_001)
#plot(fawnData$WinterSev, fawnData$FawnCount)
#abline(model_001)

# fawns ~ winter + percipitation
model_002 = lm(FawnCount ~ WinterSev + Percipitation, data = fawnData)
summary(model_002)

# fawns ~ winter + percipitation + adults
model_003 = lm(FawnCount ~ WinterSev + Percipitation + AntelopeAdultPop, data = fawnData)
summary(model_003)

# Which	model	works	best?	Model_003 works best Both R^2 values are the highest at .97 and .955
# Which	of	the	predictors	are	statistically	significant	in	each	model? 
# Percipitation is the most significant in models 2 and 3
# If	you	wanted	to	create	the	most	parsimonious	model,	what	would	it	contain?
# I would create a model with Percipitation and Antelope Population and see how well that performs on it's own
# b/c in model 3, those were the most significant variables.  Based on that result, I'd make a choice between 
# model 3 and the new model.



