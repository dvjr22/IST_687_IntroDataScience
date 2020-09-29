# 002_project_file.R
# @version: 2
# @author:  Katie Poole
# @author:  Diego Valdes
# @date:  Feb 13, 2019
# IST 687
# Clean openpowerlifting.csv - https://www.kaggle.com/open-powerlifting/powerlifting-database


rm(list=ls()) # clear work space
dev.off(dev.list()["RStudioGD"]) # clear plots

#libraries
# suppressWarnings(library('sqldf'))
suppressWarnings(require(stringi))
suppressWarnings(require(stats))

getwd() # get the current working directory
setwd("C:/Place/Your/File/Path/Here/") # set working directory
filePath = "C:/Place/Location/Of/The/File/Here/openpowerlifting.csv"

#############################################################################################################
# function for importing and cleaning table
# @filePath : directory path of openpowerlifting.csv to be cleaned
# Cleans file

cleanPowerLiftData <- function(pl) {
  
  start = Sys.time() # time algorithim start
  print(stri_paste("Start Cleaning: ", start))
  
  # Code to do initial cleaning
  pl <- powerLifting  ##create variable to work with
  pl$Age<-ceiling(pl$Age)  ##rounding Age variable to a whole number
  pl$Name<-gsub("\\.","",pl$Name)  ##removing period from Name variable
  pl<-pl[, -c(6,9,11,13, 17)]  ##removing Squat4Kg, Bench4Kg, Deadlift4Kg and Wilks variables
  pl = pl[!duplicated(pl) , ] ##removing duplicate rows
  
  # Code to remove duplicate key values meetid | name | equipment
  namesMeets = pl[, c(1,2,4) ] # col meetid, names, equipment
  duplicateValues = namesMeets[duplicated(namesMeets), ] # values that are duplicated
  duplicateValues = duplicateValues[!duplicated(duplicateValues), ] # remaining duplicates
  #toBeCleaned = liftData[0, ] # empty dataframe for removed data
  
  for (row in 1:nrow(duplicateValues)){
    
    #print(row)
    
    # mark duplicate for delete
    pl[(pl$MeetID == duplicateValues[row, 1]) & (pl$Name == duplicateValues[row, 2]) & (pl$Equipment == duplicateValues[row, 3]), 1] = 'DELETE'
    # Just to make sure R isn't freezing
    print(stri_paste("Processing: ", duplicateValues[row, 1], " | ", duplicateValues[row, 2], " | ", duplicateValues[row, 3] ))
  }
  
  pl = pl[!pl$MeetID == 'DELETE', ] # remove duplicates from data set
  pl$Place = gsub("[^0-9.]", "DELETE", pl$Place) # Mark alphas for deletion
  pl = pl[!pl$Place == 'DELETE', ] # delete marked data
  pl = pl[!pl$Place == 'DELETEDELETE', ]
  pl = pl[!pl$Place == '', ]
  
  end = Sys.time() # time algortihm end
  print(stri_paste("Cleaning Complete: ", end))
  print(stri_paste("Total run time: ", end - start, " mins")) # total time
  
  return(pl)
}

#############################################################################################################
# Execute
#############################################################################################################
powerLifting <- read.csv(file = filePath, header=TRUE, sep=",") # load data
liftDataOrig = cleanPowerLiftData(powerLifting) # clean data
write.csv(liftDataOrig, file = "clean_power_lifting.csv") # write data to file in setwd() location

unique(liftDataOrig$Place)


