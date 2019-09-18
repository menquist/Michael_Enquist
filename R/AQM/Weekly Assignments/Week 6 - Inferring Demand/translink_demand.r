
setwd("~/Downloads/Translink/")

t=readRDS("trips_2011.Rds") 


#col.names = colnames(read.table(file ="headers_28_subset.txt", header = T))

#names(t)=col.names

head(t)
str(t)


keywords <- t$Pattern[grep("*041", t$Pattern)]
streetname <- t$StopName[grep("*EB W 41 AV FS GRANVILLE ST", t$StopName)]




#         BikeLoaded,BikeUnloaded,VehicleDescription,VehicleClass,VehicleAge,VehicleYear,  select(StopName,StopSeqNo,ArriveLoadCompensated,OnsLoadCompensated,OffsLoadCompensated,
#LeaveLoadCompensated,Pattern,OperationDate, 
#WCLiftActivated,BikeLoaded ,BikeUnloaded ,Conditions,
#Wind.Speed,Visibility,Humidity,Temp,DwellTime,DepartureDelay,
#OnAndOffsCompensated,ArriveDelay) %>%  
#VehicleCapacity,VehicleSittingCapacity,VehicleFuelTyle,VehicleLength,ArriveLoad,LeaveLoad, Ons,Offs
filter.integers.numerics <- t %>% 
 filter(StopName %in% streetname, Pattern %in% keywords  ) 
str(filter.integers.numerics)
summary(filter.integers.numerics)


range.street <- filter.integers.numerics %>% 
  filter(ArriveDelay > mean(ArriveDelay))

# add a column for time (the hour)
df <- data.frame(range.street, Time = substr(range.street$Hour, start = 12, stop = 13))
levels(df$Time) <- rep(0:24, each = 1) 


df <- df %>%
  mutate(Time = as.integer(Time)) %>%
  mutate(Time.of.Day = ifelse(Time >= 0 & Time < 12, "8am - 11am",
                              ifelse(Time >= 12 & Time < 16, "12pm - 5pm",
                                     ifelse(Time >= 16 & Time < 20, "6pm - 8pm",
                                            "9pm - 12am"
                                     )))) %>%
  mutate(Time = as.factor(Time)) 

df <- transform(df, 
                Time.of.Day = factor(Time.of.Day, 
                                     levels = c("8am - 11am", "12pm - 5pm", "6pm - 8pm", "9pm - 12am"), ordered = TRUE)) 

Arriveload_bus010 <- ggplot(data = df, aes(ArriveDelay, fill = Time.of.Day)) + 
  geom_histogram(bins = 50) + ylab("Number of observations") +
  theme_bw() +
  facet_wrap(~Time.of.Day, scales="free") + geom_vline(xintercept = 10, linetype = "longdash") +
  theme(legend.position="none")
Arriveload_bus010
#scale_fill_discrete(name="Time of Day")


############# Using Forecasting: Holt-Winters Method

# add a column for time (the hour)
df.1 <- data.frame(filter.integers.numerics, Time = substr(filter.integers.numerics$Hour, start = 12, stop = 13))
levels(df$Time) <- rep(0:24, each = 1) 

df.1$Time <- as.numeric(df.1$Time)

### Looking at morning bus rides
select_Time <- df.1 %>% 
  filter(Time >= 5 , Time <=12)

qplot(data =df.1,Time, ArriveDelay)

hist(df.1$ArriveDelay, breaks = 50 )
summary(df.1$ArriveDelay)
demand <- ts(df.1$ArriveDelay, start = c(0, 1), frequency = 25)
#plot(demand)

hw <- HoltWinters(demand)
#plot(hw)

forecast <- predict(hw, n.ahead = 25, prediction.interval = T, level = 0.95)
hw
forecast
str(forecast)
plot(hw, forecast)
plot(fitted(hw))
# prediction
plot( hw, forecast, xlim = c(275, 300)) 

