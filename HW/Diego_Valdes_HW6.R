rm(list=ls()) # clear work space
dev.off(dev.list()["RStudioGD"]) # clear plots

suppressWarnings(require(ggplot2))
suppressWarnings(require(lubridate))

#suppressWarnings(require(plyr))
#suppressWarnings(require(scales))
#suppressWarnings(require(zoo))
#theme_set(theme_bw())
#theme_set(theme_classic())

# get data and clean it up
dataAQ = airquality
str(dataAQ)
summary(dataAQ)

dataAQ = na.omit(dataAQ)
str(dataAQ)
summary(dataAQ)


# add year
dataAQ$Year = c(rep(1973, nrow(dataAQ)))
#dataAQ$Time = with(dataAQ, ISOdate(dataAQ$Year, dataAQ$Month, dataAQ$Day))
dataAQ$Time = ISOdate(dataAQ$Year, dataAQ$Month, dataAQ$Day)

# histogram for each variable
ggplot(dataAQ, aes(Ozone) ) +  geom_histogram(binwidth = 20) 
ggplot(dataAQ, aes(Temp) ) +  geom_histogram(binwidth = 5) 
ggplot(dataAQ, aes(Solar.R) ) +  geom_histogram(binwidth = 20) 
ggplot(dataAQ, aes(Wind) ) +  geom_histogram(binwidth = 1) 

# box plots for ozone
ggplot(dataAQ, aes(format(Time, "%m"), Ozone)) + 
  geom_boxplot(fill = "plum") +
  xlab("Month")

# box plot for wind
ggplot(dataAQ, aes(format(Time, "%m"), ceiling(Wind))) + 
  geom_boxplot(fill = "plum") + 
  xlab("Month") + ylab("Wind")

# line charts for variables
ggplot(dataAQ, aes(x = Time)) + geom_line(aes(y = Ozone))
ggplot(dataAQ, aes(x = Time)) + geom_line(aes(y = Temp))
ggplot(dataAQ, aes(x = Time)) + geom_line(aes(y = Wind))
ggplot(dataAQ, aes(x = Time)) + geom_line(aes(y = Solar.R))

# all of them and with colors
ggplot(dataAQ, aes(x = Time)) + 
  geom_line(aes(y = Ozone), color = 2) + 
  geom_line(aes(y = Temp), color = 4) + 
  geom_line(aes(y = Wind), color = 6) + 
  geom_line(aes(y = Solar.R), color = 8) +
  ylab("Values") + xlab("Time") 

# mutating data to fit heat map
# filtering data so that the values Ozone, Temp, Wind, and Solar are categories
# Going to make the data fit the graph I need to create
OzoneDf = dataAQ[ , c(1,5,6,7,8)]
OzoneDf$Category = c(rep("Ozone", 111))
newColNames = colnames(OzoneDf) # col names
newColNames[1] = "Value"
colnames(OzoneDf) = newColNames # Standardize columns across dfs
OzoneDf$Value = OzoneDf$Value/max(dataAQ$Ozone) # convert to percent
OzoneDf$Zscore = scale(OzoneDf$Value)[ , 1] # z scores
  
# repeat process for each value
SolarDf = dataAQ[ , c(2,5,6,7,8)]
SolarDf$Category = c(rep("Solar.R", 111))
colnames(SolarDf) = newColNames
SolarDf$Value = SolarDf$Value/max(dataAQ$Solar.R)
SolarDf$Zscore = scale(SolarDf$Value)[ , 1]

WindDf = dataAQ[ , c(3,5,6,7,8)]
WindDf$Category = c(rep("Wind", 111))
colnames(WindDf) = newColNames
WindDf$Value = WindDf$Value/max(dataAQ$Wind)
WindDf$Zscore = scale(WindDf$Value)[ , 1]

TempDf = dataAQ[ , c(4,5,6,7,8)]
TempDf$Category = c(rep("Temp", 111))
colnames(TempDf) = newColNames
TempDf$Value = TempDf$Value/max(dataAQ$Temp)
TempDf$Zscore = scale(TempDf$Value)[ , 1]

newDataAQ = rbind(OzoneDf, SolarDf, WindDf, TempDf) # combine all into one df

# plot heat map w/ percents
ggplot(newDataAQ, aes(Day, Category)) + geom_tile(aes(fill = Value)) +
  facet_grid(Year ~ Month) +
  #scale_fill_gradientn(colours = heat.colors(500))  
  scale_fill_gradient(low = "yellow", high = "red")

# plot heat map 2 z scores
ggplot(newDataAQ, aes(Day, Category)) + geom_tile(aes(fill = Zscore)) +
  facet_grid(Year ~ Month) +
  #scale_fill_gradientn(colours = heat.colors(500))  
  scale_fill_gradient(low = "yellow", high = "red")

# scatter plot
ggplot(dataAQ ,aes(Wind, Temp)) + 
  geom_point(aes(color = Solar.R, size = Ozone)) +
  xlab("Wind") + ylab("Temperature") 

##################################################################################################
# FINAL ANALYSIS
# Some of the patterns I see are that higher ozone values seem to be correlated with higher temperature.The 
# month of July had some of the highest values overall amoung all 4 variables.
# 
# I found the scatter plot to be the most useful because it had all the data on one graph and the representation
# for each value was different and easy to understand.  I also found the box plots and heat map to be useful
# for different reasons.  The box plots where a good visual to see where and when most of the data was collected,
# while the heat map also had all the values on one chart for correlation, however, I didn't find it as easily to read
# as the scatter plot.