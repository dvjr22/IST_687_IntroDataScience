# Diego Valdes
# IST 687
# HW 2
# date due
# sub date

install.packages("survey")

myCars = mtcars

#step 1
# 1
max(myCars$hp)
# 2 
myCars[which.max(myCars$hp), ] # returns row
rownames(myCars[which.max(myCars$hp), ]) # extract row name

#step 2
# 3
max(myCars$mpg)
# 4
myCars[which.max(myCars$mpg), ]
rownames(myCars[which.max(myCars$mpg), ])
# 5 
myCarsMpg = myCars[order(myCars$mpg), ]

# step 3
#6
myCarsMpg[order(myCarsMpg$hp), ]
bestMPG = mean(myCarsMpg$mpg)
bestHP = mean(myCarsMpg$hp)

bestCars = myCars[myCars$mpg > bestMPG,]
bestComboCar = bestCars[bestCars$hp == max(bestCars$hp), ]

# step 4
# This is just me exploring for this step.
library(survey)
myCars.weighted = svydesign(ids = ~1, data = myCars, weights = myCars$mpg)
summary(myCars.weighted)
myCars.weighted
prop.table(table(myCars$mpg))
