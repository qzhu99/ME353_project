ZHU_QIANRU_FINAL_Rfunction <- function(){
  t1<-read.csv("ZHU_QIANRU_FINAL_RData.csv")
  t1<-subset(t1, Fare_amount >0& Tip_amount >=0 & Trip_distance>0&Total_amount >= 2.5& Passenger_count >0)

  #t1$date<-as.Date(t1$lpep_pickup_datetime)
  #t1$time <- format(as.POSIXct(t1$lpep_pickup_datetime),"%H:%M:%S")
  t1$hour <- format(as.POSIXct(t1$lpep_pickup_datetime),"%H")
  t1$trip_duration<-as.numeric((as.POSIXct(t1$Lpep_dropoff_datetime)-as.POSIXct(t1$lpep_pickup_datetime))/60)
  #t11$day <- weekdays(t11$date)
  t1$tip_percentage <- t1$Tip_amount/t1$Fare_amount*100

  tip_by_hour<-aggregate(t1$tip_percentage, by = list(t1$hour),FUN = mean)
  names(tip_by_hour)<- c('Hour_of_the_Day', "Tip_Percentage")
  
  pdf(file = "ZHU_QIANRU_FINAL_TIP_PLOT.pdf", height = 6, width = 9)
  plot(tip_by_hour$Hour_of_the_Day, tip_by_hour$Tip_Percentage, pch = 20, col = "blue", xlab = "Hour of the Day", 
      ylab = "Tip Percentage (%)", xaxt = "n",main = "Mean of Tip Percentage \n by Hour of the Day")
  lines(tip_by_hour$Hour_of_the_Day, tip_by_hour$Tip_Percentage, col = "blue")
  axis(1, at=0:24, labels=c(0:24))
  dev.off()
  
  #hist(t1$trip_duration, breaks = 30, xlim = c(1,100))
  #tip_per<- t1$tip_percentage[t1$tip_percentage != 0]
  
  #let t2 be the trips paied only by credit card
  t2<-subset(t1,  select = c(Passenger_count,Trip_distance,Fare_amount,Extra,MTA_tax,Payment_type,tip_percentage,Payment_type,Trip_type,improvement_surcharge))
  #Payment_type == 1,
  t2<- t2[is.na(t2$tip_percentage) == FALSE,]
  #t2$tip_percentage[]
  
  
  summary(t2$tip_percentage)
  
  ####look at data to tell the portion that no tip was given #0.1357896
  ####the portion that generous tip (> 28%) was given #0.1375258
  #dim(t2[t2$tip_percentage ==0,])[1]/dim(t2)[1]
  #dim(t2[t2$tip_percentage >28,])[1]/dim(t2)[1]
  
  ###generate subsets for no_tip trip and generous_tip trip separately
  no_tip<-t2[t2$tip_percentage == 0,]
  no_tip$tip_type = 0
  gen_tip<-t2[t2$tip_percentage >28,]
  gen_tip$tip_type = 1
  
  set.seed(1234)
  no_tip<-no_tip[sample(nrow(no_tip)),]
  no_tip<-no_tip[1:dim(gen_tip)[1],]
  
  trips<- rbind(no_tip,gen_tip)
  
  

}