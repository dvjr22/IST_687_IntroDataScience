rm(list=ls()) # clear work space
dev.off(dev.list()["RStudioGD"]) # clear plots

getwd()
setwd("C:/Users/dvjr2/Google Drive/Documents/Syracuse/IST_687/Project/")

filePath = "C:/Users/dvjr2/Google Drive/Documents/Syracuse/IST_687/Project/powerlifting_database/openpowerlifting.csv"


##function for importing and cleaning table, table must be on the desktop.
cleanPowerLiftData <- function(filePath) {
  
  powerLifting <- read.csv(file = filePath, header=TRUE, sep=",")
  
  ##create variable to work with
  pl <- powerLifting
  
  ##rounding Age variable to a whole number
  pl$Age<-ceiling(pl$Age)
  
  ##removing period from Name variable
  pl$Name<-gsub("\\.","",pl$Name)
  
  ##removing Squat4Kg, Bench4Kg, Deadlift4Kg and Wilks variables
  pl<-pl[, -c(6,9,11,13, 17)]
  
  ##removing duplicate rows
  pl = pl[!duplicated(pl) , ]
  
  
}


liftData = cleanPowerLiftData(filePath) # load data
namesMeets = liftData[, c(1,2) ] # col 1 and 2
duplicateValues = namesMeets[duplicated(namesMeets), ] # values that are duplicated


###################################################### Remove duplicate value pairs MeetID | Name ######################################################

liftData.2 = liftData[1:500, ] # taking a slice of data for testing purposes
namesMeets = liftData.2[, c(1,2) ] # col 1 and 2 the names and meets data
duplicateValues.2 = namesMeets[duplicated(namesMeets), ] # values pairs that are duplicated
duplicateValues.2 = duplicateValues.2[!duplicated(duplicateValues.2), ]
toBeCleaned = liftData.2[0, ] # empty dataframe to store the duplicate values that will need to be cleaned


# Brute Force that mother!
# for loop to itterate through liftData
for (row in 1:nrow(liftData.2)){
  
  # print(row) # this was a check during testing
  
  # for loop to itterate through duplicate value pairs
  for (row_1 in 1:nrow(duplicateValues.2)){
    # print(row_1) # testing check
    
    # check if the pair in liftData matches the value in duplicateValues
    if(liftData.2[row , 1 ] == duplicateValues.2[row_1, 1] && liftData.2[row , 2] == duplicateValues.2[row_1, 2] ){
      # print("test") # testing check
      toBeCleaned =rbind(toBeCleaned, liftData.2[row, ]) # add duplicate row to new df
      liftData.2[row, 1] = NA # mark the meetid NA.  This is so we can remove them later
      
      break # once a match is found, no need to continue search, break out of the loop
    }
  }
}

liftData.2 = liftData.2[!is.na(liftData.2$MeetID), ] # remove the NA in meet, all duplicate value pairs removed

################################################ Remove duplicate value pairs MeetID | Name | Equipment ################################################

liftData.3 = liftData[1:500, ]
namesMeets = liftData.3[, c(1,2,4) ] # col 1 and 2 and 4, Adding equipment
duplicateValues.3 = namesMeets[duplicated(namesMeets), ] # values that are duplicated
toBeCleaned.3 = liftData.3[0, ] # to store values with named cols

# same loop as above, just added equipment in the conditional
for (row in 1:nrow(liftData.3)){
  
  for (row_1 in 1:nrow(duplicateValues.3)){
    
    
    if(liftData.3[row , 1 ] == duplicateValues.3[row_1, 1] && liftData.3[row , 2] == duplicateValues.3[row_1, 2] && liftData.3[row , 4] == duplicateValues.3[row_1, 3] ){
      toBeCleaned.3 = rbind(toBeCleaned.3, liftData.3[row, ])
      liftData.3[row, 1] = NA
      
      break
    }
  }
}

test = liftData.2[!is.na(liftData.2$MeetID), ] # remove the NA in meet

  
library('sqldf')
library(stringi)

liftData_2 = liftData[1:500, ]
namesMeets = liftData_2[, c(1,2) ] # col 1 and 2
duplicateValues_2 = namesMeets[duplicated(namesMeets), ] # values that are duplicated
duplicateValues_2 = duplicateValues_2[!duplicated(duplicateValues_2), ]
toBeCleaned_2 = liftData_2[0, ] 

for (row in 1:nrow(duplicateValues_2)){
  
  result = sqldf(test = stri_paste("select  * from liftData_2 
                            where liftData_2.MeetID = ", duplicateValues_2[row, 1],
                            " and liftData_2.Name = '", duplicateValues_2[row, 2], "'"))
  toBeCleaned_2 = rbind(toBeCleaned_2, result) #  

  liftData_2[(liftData_2$MeetID == duplicateValues_2[row, 1]) & (liftData_2$Name == duplicateValues_2[row, 2]), 1] = 'DELETE'
  }

liftData_2 = liftData_2[!liftData_2$MeetID == 'DELETE', ]
##############################################################################################################


new_lifting_df = cleanPowerLiftData(dire)
summary(new_lifting_df)
str(new_lifting_df)
colnames(new_lifting_df)
View(new_lifting_df)

lifting_ds = read.csv("powerlifting_database/openpowerlifting.csv")
summary(lifting_ds)
str(lifting_ds)
View(lifting_ds)
colnames(lifting_ds)

summary(lifting_ds$BodyweightKg)
lifting_ds$BodyweightKg[8]

summary(lifting_ds$Name)
lifting_ds$Name[500]

meets_ds = read.csv("powerlifting_database/meets.csv")
summary(meets_ds)
str(meets_ds)
colnames(meets_ds)



length(unique(meets_ds$MeetPath))
length(meets_ds$MeetPath)

sum(is.na(meets_ds$Date))

length(unique(meets_ds$MeetName))
length(meets_ds$MeetName)

meets_ds$MeetName

states = states[complete.cases(states),] # remove na rows

x = complete.cases(lifting_ds[ , lifting_ds$Age])
test_d = lifting_ds[complete.cases(lifting_ds[ , lifting_ds$Age]), ]
test_d = na.omit(lifting_ds, cols = lifting_ds$Age)
test_d = lifting_ds[!is.na(lifting_ds$Age),]
?na.omit
summary(test_d)
summary(lifting_ds)
str(test_d)
test_d$Age
