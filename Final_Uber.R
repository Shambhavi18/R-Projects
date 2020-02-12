#installing the package
#lubridate, stringr, tidyr,dplyr, ggplot2 etc 

# import  the cSV file
uber<- read.csv("Uber Request Data.csv")

# Check for duplicates in file
sum(duplicated(uber$Request.id))

#Checking for NA
colSums(is.na(uber))

#Converting the formating to time columns to date time

uber$Request.timestamp <- parse_date_time(uber$Request.timestamp, c("%d/%m/%y %H:%M", "%d-%m-%y %H:%M:%S"))
uber$Drop.timestamp <- parse_date_time(uber$Drop.timestamp, c("%d/%m/%y %H:%M", "%d-%m-%y %H:%M:%S"))

Str(uber)

# Creating saperate date and time column in uber data frame
uber$Reqhour <- format(uber$Request.timestamp, "%H")
uber$Reqday <- format(uber$Request.timestamp, "%d-%m-%y")

# creating different date_time_slot
uber$date_time_slot = ifelse(as.numeric (uber$Reqhour) < 5, "Early_Morning", ifelse( as.numeric (uber$Reqhour) < 10,"Morning_Rush",ifelse(as.numeric(uber$Reqhour) < 17,"Afternoon",ifelse(as.numeric(uber$Reqhour) < 22,"Evening_Rush","Late_Night"))))

# Finding number of trips made in each slot
nrow(subset(uber, uber$date_time_slot == "Early_Morning"))
nrow(subset(uber, uber$date_time_slot == "Morning_Rush"))
nrow(subset(uber, uber$date_time_slot == "Afternoon"))
nrow(subset(uber, uber$date_time_slot == "Evening_Rush"))
nrow(subset(uber, uber$date_time_slot == "Late_Night"))

# visualising most problematic time slot in which uber is getting most cancelation request
ggplot(uber, aes(x = as.factor(date_time_slot), fill= as.factor(uber$Status))) + geom_bar()+labs(x = "Time Slot", y = "Number of Requests", fill = "Status" )

# From above visualisation we found that in  morning_Rush drivers are cancelling more requests

#Visualising reason of cancelation (pickup point)
uber_MorningRush <- subset(uber,date_time_slot=="Morning_Rush")
ggplot(uber_MorningRush, aes(x = as.factor(Pickup.point), fill= as.factor(uber_MorningRush$Status))) + geom_bar() +labs(x = "Pickup Point", y = "Number of Requests", fill = "Status" )

# number of cancelation with respect to pickup point

nrow(subset(uber_MorningRush, uber_MorningRush$Pickup.point == "Airport" & uber_MorningRush$Status == "Cancelled"))
nrow(subset(uber_MorningRush, uber_MorningRush$Pickup.point == "City" & uber_MorningRush$Status == "Cancelled"))


# percentage of Most pressing problem of uber
uber_MorningRush_City <- subset(uber_MorningRush, Pickup.point %in% "City")
ggplot(uber_MorningRush_City, aes(x = uber_MorningRush_City$Pickup.point, fill= as.factor(uber_MorningRush_City$Status))) + geom_bar()+ coord_polar(theta = "y", start=0)+ labs( y = "Number of Requests", x = "", fill = "Status")

# Supply and demand GAP
nrow(subset(uber_MorningRush_City, uber_MorningRush_City$Pickup.point == "City" & uber_MorningRush_City$Status == "Trip Completed"))
#[1] 472
nrow(subset(uber_MorningRush_City, uber_MorningRush_City$Pickup.point == "City"))
  #[1] 1677

####Problem 2 (No cars available) 

uber_eveningrush<- subset(uber, date_time_slot =="Evening_Rush")
View(uber_eveningrush)
ggplot(uber_eveningrush, aes(x = as.factor(Pickup.point), fill= as.factor(uber_eveningrush$Status))) + geom_bar() +labs(x = "Pickup Point", y = "Number of Requests", fill = "Status" )

## By this we found out that the cancelation is highest at the airport
# visualizing the requests in Airport
uber_eveningrush_airport <- subset(uber_eveningrush, pickup.point %in% "Airport")

ggplot(uber_eveningrush_airport, aes(x = uber_eveningrush_airport$Pickup.point, fill= as.factor(uber_eveningrush_airport$Status))) + geom_bar()+ coord_polar(theta = "y", start=0)+ labs( y = "Number of Record", x = "", fill = "Status")

## Supply and Demand
nrow(subset(uber_eveningrush_airport, uber_eveningrush_airport$Pickup.point == "Airport" & uber_eveningrush_airport$Status == "Trip Completed"))
#[1] 373
nrow(subset(uber_eveningrush_airport, uber_eveningrush_airport$Pickup.point == "Airport"))
  #[1] 1800

