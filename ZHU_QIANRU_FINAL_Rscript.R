  #install.packages("leaflet")  
#install.packages("dbscan") 
#install.packages("ROCR") 
#install.packages("pscl") 
#install.packages("MASS") 
#install.packages("gplots") 
  library(leaflet)
  library(dbscan)
  library(ROCR)
  library(pscl)
  library(MASS)
  library(gplots)
  
  ######DELIVERABLE 3: Tip Type Classifier######
  
  trips<-read.csv("ZHU_QIANRU_FINAL_CLASSIFIER_DATA.csv")
  #cor(subset(x= trips, select = c(Passenger_count,Trip_distance,Fare_amount,Extra,MTA_tax,tip_type)))
  
  d2<-dim(trips)[1]/2
  d3<-d2+1
  d4<-d2*2
  trips_train<-trips[1:d2,]
  trips_test<-trips[d3:d4,]

  lgm<- glm(tip_type ~ Passenger_count+Trip_distance+Fare_amount+Extra+MTA_tax+Payment_type,family = binomial(link = "logit"),data = trips_train)
  #summary(lgm)

  anova(lgm, test="Chisq")

  pR2(lgm)
  fitted.results <- predict(lgm, newdata=subset(trips_test,select=c(Passenger_count,Trip_distance,Fare_amount,Extra,MTA_tax,Payment_type)),type='response')
  fitted.results <- ifelse(fitted.results > 0.5,1,0)
  misClasificError <- mean(fitted.results != trips_test$tip_type)

  #####DELIVERABLE 4: Location Clustering #######
  t1<-read.csv("ZHU_QIANRU_FINAL_RData.csv")
  locations<-t1[c("Pickup_latitude","Pickup_longitude")]
  names(locations)<-c("lat","long")
  locations<-locations[locations$lat != 0 & locations$long != 0,]
  EPS <- 0.0015
  # clusters <- dbscan(select(locations, lat,long), eps = EPS)
  clusters<- dbscan(locations, eps = EPS)
  locations$cluster <- clusters$cluster

  Nclu<-length(levels(as.factor(locations$cluster)))
  #pal <- colorNumeric(c("red", "green", "blue"), 1:length(levels(as.factor(locations$cluster))))

  factpal <- colorFactor(topo.colors(Nclu), locations$cluster)
  leaflet(locations) %>% addTiles() %>%
    addCircleMarkers(
      radius = 3,
     stroke = FALSE, fillOpacity = 0.4,color = ~factpal(cluster)
    )
  
  print(paste('The Classifier has accuracy of ',round(1-misClasificError, 4)*100, '%', sep = ''))
  
  print(paste("The clustering tool by location shows that there will be ",Nclu," clusters", sep = ""))
