suppressWarnings(require(ggplot2))
suppressWarnings(require(lubridate))

suppressWarnings(require(plyr))
suppressWarnings(require(scales))
suppressWarnings(require(zoo))
theme_set(theme_bw())
theme_set(theme_classic())

dataAQ = airquality
dataAQ = na.omit(dataAQ)

# Add time variables
dataAQ$Year = c(rep(1973, nrow(dataAQ)))
#dataAQ$Time = with(dataAQ, ISOdate(dataAQ$Year, dataAQ$Month, dataAQ$Day))
dataAQ$Time = ISOdate(dataAQ$Year, dataAQ$Month, dataAQ$Day)

# Standardize columns across dfs
newColNames = colnames(OzoneDf)
newColNames[1] = "Value"

# filtering data so that the values Ozone, Temp, Wind, and Solar are categories
# Going to make the data fit the graph I need to create
OzoneDf = dataAQ[ , c(1,5,6,7,8)]
OzoneDf$Category = c(rep("Ozone", 111))
colnames(OzoneDf) = newColNames # Standardize columns across dfs
OzoneDf$Value = OzoneDf$Value/max(dataAQ$Ozone) # convert to percent

# repeat process for each value
SolarDf = dataAQ[ , c(2,5,6,7,8)]
SolarDf$Category = c(rep("Solar.R", 111))
colnames(SolarDf) = newColNames
SolarDf$Value = SolarDf$Value/max(dataAQ$Solar.R)

WindDf = dataAQ[ , c(3,5,6,7,8)]
WindDf$Category = c(rep("Wind", 111))
colnames(WindDf) = newColNames
WindDf$Value = WindDf$Value/max(dataAQ$Wind)

TempDf = dataAQ[ , c(4,5,6,7,8)]
TempDf$Category = c(rep("Temp", 111))
colnames(TempDf) = newColNames
TempDf$Value = TempDf$Value/max(dataAQ$Temp)

# combine all into one df
newDataAQ = rbind(OzoneDf, SolarDf, WindDf, TempDf)

# plot that mother fucker v1
ggplot(newDataAQ, aes(Day, Category)) + geom_tile(aes(fill = Value)) +
scale_fill_gradientn(colours = heat.colors(500))  

# v2
ggplot(newDataAQ, aes(Day, Category)) + geom_tile(aes(fill = Value)) +
  scale_fill_gradientn(colours = heat.colors(500))  + 
  facet_grid(Year ~ Month)

# v3
ggplot(newDataAQ, aes(Day, Category)) + geom_tile(aes(fill = Value)) +
  scale_fill_gradientn(colours = heat.colors(500))  + 
  facet_grid(Year ~ Month)

