library(shiny)
library(vroom)
library(tidyverse)
library(zoo)
library(lubridate) #for the strptime() function


#I want to import a sample datasheet that came from the MBT EXO...I will upload a .csv that I compiled from all the data I downloaded from the ORCA google drive
MBT <- read.csv(file = 'AllMBT.csv', check.names = F) 


#Convert the combined date and time column to date-time that R will understand
MBT$Combined <- strptime(MBT$Combined, format = "%m/%d/%y %H:%M", tz="US/Pacific")
class(MBT$Combined)

#MBT$Combined <- as.POSIXct(strptime(MBT$Combined, format = "%m/%d/%y %H:%M"))

#change the date from character to date class
MBT$Date <- as.Date(MBT$Date, format = "%m/%d/%y")
class(MBT$Date)


#Need to change data frame to a time series for easy plotting...
MBT.zoo <- zoo(MBT, order.by = MBT$Combined)
class(MBT.zoo)

#create a date range from the time series (these will be reactives based on user inputs in my app)
MBT.subset <- window(MBT.zoo, start = as.Date("2020-09-01"), end = as.Date("2020-09-30"))

#fortify.zoo returns a data.frame either in long format (melt = TRUE) or in wide format (melt = FALSE). The long format has three columns: the time Index, a factor indicating the Series, and the corresponding Value. The wide format simply has the time Index plus all columns of coredata(model).
MBT.subset <- fortify.zoo(MBT.subset)

#Now, I can easily plot the MBT data frame with ggplot(), while still keeping the time series (from the .zoo object)

MBT.subset %>% 
  ggplot(aes(Index, Temperature, group=1)) + 
  geom_line(na.rm = TRUE) + #remove all NA values
  labs(y = "Temperature (°C)", x= "Date")


#Helpful EXTRA information for dates/times:
#"Excel stores dates as sequential serial numbers so that they can be used in calculations. By default, January 1, 1900 is serial number 1, and January 1, 2008 is serial number 39448 because it is 39,447 days after January 1, 1900."


#convert the date to a date class (from a character class)
#MBT$Date <- as.Date(MBT$Date, format = "%m/%d/%y")
#class(MBT$Date)

#convert the time to a time class (from a character class)
#MBT$Time <-as.POSIXlt(MBT$Time, format = "%H/%M/%S")

