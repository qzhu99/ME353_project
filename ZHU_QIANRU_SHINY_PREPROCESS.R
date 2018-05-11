#df$Date <- as.Date(df$Start) #already got this one from the answers above
#df$Time <- format(as.POSIXct(df$Start) ,format = "%H:%M:%S") 

###split the date and time using the pickup datetime
###put date in a new column
###only pick up one day and plot that date on shiny app

#make a copy of the taxi dataset
t1<-read.csv("ZHU_QIANRU_FINAL_COMPLETE_TRIP_DATA.csv")
t1<-subset(t1, Fare_amount >0& Tip_amount >=0 & Trip_distance>0&Total_amount >= 2.5& Passenger_count >0)
t1$tip_percentage<- t1$Tip_amount/t1$Fare_amount *100
t1$date<-as.Date(t1$lpep_pickup_datetime)

t2<-t1[t1$date == "2015-09-16",]

t3<- t2[c("RateCodeID","Pickup_longitude","Pickup_latitude" ,"Passenger_count","Trip_distance","Tip_amount","Total_amount","Payment_type","Trip_type","tip_percentage","Fare_amount")]
#,"Trip_distance","Tip_amount","Total_amount","Payment_type"," Trip_type","tip_percentage")]
names(t3)<- c("ratecodeid","longitude",'latitude',"passengercount","tripdistance","tipamount","totalamount",'payment',"triptype","tippercentage","fareamount")
t3<-t3[t3$latitude != 0 & t3$longitude !=0,]
t3<-t3[abs(t3$latitude-mean(t3$latitude))<=3*sd(t3$latitude),]
t3<-t3[abs(t3$longitude-mean(t3$longitude))<=3*sd(t3$latitude),]

#t3<-t3[is.na(t3$Pickup_longitude) == FALSE,]
#t2<-t2[is.na(t2$Pickup_longitude) == FALSE,]
#t2<-t2[is.na(t2$Dropoff_latitude) == FALSE,]
#t2<-t2[is.na(t2$Dropoff_longitude) == FALSE,]


write.csv(t3, "ZHU_QIANRU_FINAL_SHINY_091615.csv",row.names = F)
