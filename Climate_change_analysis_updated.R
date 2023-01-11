# loading R library
library(tidyverse) # plotting and data manipulating, 
library(lubridate)  # works with data, convert number to data, and vice versa, works well with data
library(caret)   # ML library, random forest, and other ML functionality
library(MLmetrics) # If we build a model, we need to measure the validity, is it good or bad
library(stargazer)  # It provides interactive, and other statistical tables, summary stats
library(ggstatsplot) # statistical plots with hypothesis testing
library(prophet)   #editing regression models library, and provide forecasting capability (weather forecast)
library(neuralnet) # function for neural modelling ( how people think ,complex network of network)- nodes are connected, based on maths model
library(ggpubr)
library(fpp2)           # working with time series data
library(zoo)            # working with time series data            
library(table1) # provides stat tables
library(patchwork) # arraning plot

# Read csv file 
df <- read_csv('F:/Fall 2021/Big Data/big data project/USA_city_landTemp.csv')
# set working directory
setwd("F:/Fall 2021/Big Data/big data project")

# find unique values in column and sort
sort(unique(df$City)) # dataset is big, sorted the unique cities, sort is alphebetic arrangement
# select some cities from city column
df<- df %>%    # df- name of dataframe. 
                #filtering by the cities that we want to study.
  filter(City %in% c("Atlanta","New York","Chicago","Los Angeles"))
                # filtering more than 1 value, in=function in a city colomn
# date column to as date format
df$Date <- as.Date(df$dt,format='%Y-%m-%D')
# add new column for year

df$Year <- format(df$Date, "%Y")
# add new column for month of short form
df$Month <- factor(format(df$Date, "%b"),levels = month.abb)


# group by month and year and find mean of temperature

df %>% 
  group_by(City) %>% 
  summarise(mean_temp = mean(AverageTemperature,na.rm=TRUE), # (na.rm) ignoring the data that are missing
            max_temp=max(AverageTemperature,na.rm=TRUE),
            min_temp=min(AverageTemperature,na.rm=TRUE),
            var_temp=var(AverageTemperature,na.rm=TRUE)) %>% 
  write.csv("City_Summary_USCities.csv",row.names = FALSE)

# monthly summaryfor 4 cites from about 300 years ( approx), R will summarize in 4 colums

Monthly_city<- df %>% 
  group_by(City) %>% 
  summarise(mean_temp = mean(AverageTemperature,na.rm=TRUE),
            max_temp=max(AverageTemperature,na.rm=TRUE),
            min_temp=min(AverageTemperature,na.rm=TRUE),
            var_temp=var(AverageTemperature,na.rm=TRUE))

# group by month and year and find mean of temperature

Monthly_Temp<-df %>% # pipe operator #
  group_by(Month,Year) %>% 
  summarise(mean_temp = mean(AverageTemperature)) %>%
  drop_na() # we are dropping na / blanks temperature value 
# reassuring by checking 
str(Monthly_Temp$Year)
# converting character to number
Monthly_Temp$Year<- as.numeric(Monthly_Temp$Year)

# 
str(Monthly_Temp$Year)
#Monthly_Temp$Year<-year(as.Date(Monthly_Temp$Year,format='%Y'))
  
ggplot(Monthly_Temp) +
  aes(x = Year, y = mean_temp, colour = Month) +
  geom_line(size = 1.0) +
  ylab("Temperature(ï¿½C)")+
  ggtitle(' Monthly Temperature line Chart')+ 
  scale_color_hue(direction = 1) +
  ggthemes::theme_wsj() +
  theme(legend.position = "bottom")+
  guides(colour = guide_legend(nrow = 1))+
  theme(axis.title=element_text(size=8)) 
  

# city wise box for preparation
cities<- df %>% 
   mutate(Year=df$Year <- format(df$Date, "%Y")) %>% 
  group_by(Year,City) %>% 
  summarise(mean_temp = mean(AverageTemperature,rm.na=TRUE),.groups = 'drop')
# city wise boxplot
ggplot(cities) +
  aes(x = City, y = mean_temp, fill = City) +
  geom_boxplot(shape = "circle") +
  scale_fill_hue(direction = 1) +
  labs(y = "Temperature (C)") +
  ggthemes::theme_economist() +
  theme(legend.position = "none")


# Plot for all 4 cities in 1 visual
# cite wise plot
ggplot(df) +
 aes(x = Date, y = AverageTemperature, colour = AverageTemperature) +
 geom_line(size = 0.001) +
  geom_smooth(color = "blue",size = 1)+
 scale_color_distiller(palette = "YlOrRd", direction = 1) +
 labs(x = "Year", y = "Average Temperature (C)", 
 color = "Temperature (C)") +
 ggthemes::theme_economist() +
 theme(axis.title.y = element_text(size = 15L, 
 face = "bold"), axis.title.x = element_text(size = 18L, face = "bold")) +
 facet_wrap(vars(City), 
 scales = "free")




df$Year<-  as.numeric(df$Year)

# Plot for LA
df %>% 
  filter(City == "Los Angeles") %>% 
    filter( Year >=  1950) %>% 
  
  # cite wise plot
  
  ggplot() +
  aes(x = Date, y = AverageTemperature, colour = AverageTemperature) +
  geom_line(size = 0.001) +
  geom_smooth(color = "blue",size = 1)+
  scale_color_distiller(palette = "YlOrRd", direction = 1) +
  labs(x = "Year", y = "Average Temperature (C)", 
       color = "Temperature (C)") +
  ggthemes::theme_economist() +
  theme(axis.title.y = element_text(size = 15L, 
                                    face = "bold"), axis.title.x = element_text(size = 18L, face = "bold")) 
  
# plot for new york [center title ]
df %>% 
  filter(City == "New York") %>% 
  filter( Year >=  1950) %>% 
  
  # cite wise plot , new york
  ggplot() +
  aes(x = Date, y = AverageTemperature, colour = AverageTemperature) +
  geom_line(size = 0.001) +
  geom_smooth(color = "blue",size = 1.5)+
  scale_color_distiller(palette = "YlOrRd", direction = 1) +
  labs(x = "Year", y = "Average Temperature (C)",title = 'New York', 
       color = "Temperature (C)") +
  theme(plot.title = element_text(size=15,hjust=0.3))+
  geom_text(x=as.Date(' 2014-06-30'),y=12.2,label='12.2',size=3)+
  geom_text(x=as.Date(' 1949-01-01'),y=11,label='10.1',size=3)


df %>% 
  filter(City == "Chicago") %>% 
  filter( Year >=  1950) %>% 
  
  # cite wise plot , Chicago
  ggplot() +
  aes(x = Date, y = AverageTemperature, colour = AverageTemperature) +
  geom_line(size = 0.001) +
  geom_smooth(color = "blue",size = 1.5)+
  scale_color_distiller(palette = "YlOrRd", direction = 1) +
  labs(x = "Year", y = "Average Temperature (C)",title = 'Chicago', 
       color = "Temperature (C)") +
  theme(plot.title = element_text(size=15,hjust=0.3))+
  geom_text(x=as.Date(' 2015-06-01'),y=12.2,label='11.2')+
  geom_text(x=as.Date(' 1949-01-01'),y=11,label='9.43')

df %>% 
  filter(City == "Chicago") %>% 
  filter( Year >=  1950) %>% 
  group_by(Year) %>% 
  summarise(meanc=mean(AverageTemperature)) %>% 
  tail(3)

# Atlanta calculation

df %>% 
  filter(City == "Atlanta") %>% 
  group_by(Year) %>% 
  summarise(meanc=mean(AverageTemperature,na.rm=TRUE))

##need to add graph for 2 other cities

# filtering for the cities and 2013 and then we found average temperature for 2013 is 12.2
df %>% 
  filter(City == "Chicago") %>% 
  filter( Year ==  2013) %>% 
  group_by(Year) %>%
  summarize(mean(AverageTemperature),
            max(AverageTemperature)) # add max min 

df %>% 
  filter(City == "Chicago") %>% 
  #group_by(Year) %>
  summarize(mean(AverageTemperature,na.rm=TRUE),
            max(AverageTemperature,na.rm=TRUE),
            min(AverageTemperature,na.rm=TRUE)) 

# need to create plots for each city , and find the value for 1950, 2013, and the lowest point
# plot

#Violin Shift box plot
ggbetweenstats(
  data = cities,
  x = City,
  y = mean_temp,
  title = "Distribution of Temperature across the major cities",
  caption =" Berkeley Earth data source",
  ylab = "Temperature (C)")

# make linear regression model
lm_model <- lm(mean_temp ~ Year,data=Monthly_Temp)
# find summary of linear regression model
summary(lm_model)
# find residuals
residuals(lm_model)
stargazer(lm_model,type = 'html')
# find residual plot
par(mfrow=c(1,2))
plot(lm_model)


# find model accuracy
summary(lm_model)$coefficients
# find prediction
prediction <- as.data.frame(predict(lm_model, newdata=list(Year= Monthly_Temp$Year)))
colnames(prediction)= 'Prediction'

head(prediction)

# make accuracy matrix and r square value
summary(lm_model)$coefficients
summary(lm_model)$r.squared # r square values 
# 0.01  means 1% of the data 
# Compute the mean squared error regression loss.
MSE(Monthly_Temp$mean_temp, prediction$Prediction,na.rm=TRUE)  # I will check later

RMSE(Monthly_Temp$mean_temp, prediction$Prediction)






#library(lubridate)

#data$Year <- year(data$dt)



#Aggregating the average temperatures year wise
Avg_temp_agg <- aggregate(AverageTemperature ~ Year, FUN=mean, data = df)



#Plotting the increase in temperature in last 50 years
temp_50 <- subset(Avg_temp_agg, Year > 1963)
temp_50$Year<- as.numeric(temp_50$Year)
# year wise  temprature and regression line 
ggplot(temp_50 , aes(Year, AverageTemperature)) + 
  geom_line(size = 2, aes(colour = AverageTemperature)) +
  scale_colour_gradient(low="orange", high="red") +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Surface Temperature for four cities combined in the last 50 Years")+
  labs( y = "Average Temperature (C)")+
  theme(panel.grid.major = element_line(size=1))+
  theme_minimal()

 


#Plotting the increase in temperature in last 10 years
temp_10 <- subset(Avg_temp_agg, Year > 2003)
# checking data frame data types
str(temp_10)
# converting character to number 
temp_10$Year<- as.numeric(temp_10$Year)
ggplot(temp_10, aes(Year, AverageTemperature)) +
  geom_line(size = 2, aes(colour = AverageTemperature)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("land Surface Temperature of  in the last 10 Years")



# making r style sumamry table 
table1::table1(~AverageTemperature|City,data = df)
# other way
table1::table1(~AverageTemperature|Month,data = df)

# Make neural network model
data<- na.omit(df[c(9,2)]) 
data$Year<- as.numeric(data$Year)

index <- sample(1:nrow(data),round(0.75*nrow(data)))
# split train and test
                
train <- data[index,]
test <- data[-index,]
n <- names(train)
f <- as.formula(paste("AverageTemperature~", paste(n[!n %in% "AverageTemperature"], collapse = " + ")))
nn <- neuralnet(f,data=train,hidden=c(5,3),linear.output=T)
nn$result.matrix

summary(nn)
# visualize model
plot(nn)
prediction_nn <- predict(nn,test)
MSE.lm <- sum((prediction_nn- test$AverageTemperature)^2)/nrow(test)
MSE.lm
nn$result.matrix

RMSE(test$AverageTemperature,prediction_nn)
?RMSE
prediction_nn<- as.data.frame(prediction_nn)
colnames(prediction_nn) <- "prediction"
nn$response

new_df<- cbind(test,prediction_nn)


# make a plot

ggplot(new_df,aes(x=Year)) + 
   geom_line(aes(y =AverageTemperature, colour = "Real")) + 
   geom_line(aes(y =prediction, colour="Predicted"))+
 scale_colour_manual("",breaks = c("Real", "Predicted"),
                        values = c("gray","steelblue"))


# evaluate the model
RMSE(test$AverageTemperature, prediction_nn$prediction)


#pr.nn <- compute(nn,test[,1:2])
#pr.nn_ <- pr.nn$net.result*data$AverageTemperature
#test.r <- (test$AverageTemperature)*(max(test$AverageTemperature))-min(test$AverageTemperature)+min(test$AverageTemperature)
#MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test)

# caret regression
lm1 <- train(f, data = train, method = "lm")
lm1$results
summary(lm1)
# forecast using prophet
# forecast for Atlanta 
df1<- df %>% 
  filter(City %in% c("Atlanta")) %>% 
  select(Date,AverageTemperature)
colnames(df1)<- c('ds',	'y')
m <- prophet(df1,weekly.seasonality=FALSE,daily.seasonality = FALSE)
future <- make_future_dataframe(m, periods =   7300) # for 20 years
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

tail(future)
#plot(m, forecast,main="Forecasting for Atlanta",xlab='Year',y='Temperature and forecasted') 

library(tidyquant)
#prophet_plot_components(m,forecast)


plot(forecast$ds,forecast$trend)
prophet_plot_components(m,forecast,plot_cap=FALSE)+
  theme_tq()

s <- forecast %>% 
  inner_join(df1, by="ds") %>% 
  mutate(
    delta = (y-yhat), 
    isPositive= delta >= 0) %>% 
  select(ds,y,yhat)




# Atlanta

# rolling mean and save for atlanta as two_atlanta
two_atlanta<- s %>%
  select(ds, y,yhat) %>%
  mutate(observed = rollmean(y, k =300, fill = NA),
         predicted=rollmean(yhat,k=300,fill=NA)) %>% 
  # plot two lines with observed and predicted values
  ggplot() +
  geom_line(aes(x=ds,y=predicted,color="predicted"),size=1) +
  geom_line(aes(x=ds,y=observed,color="observed"),size=1) +
  ggthemes::theme_igray()+
  labs(x="Date",title='Atlanta',color='Temperature',
       y=expression(paste("Temperature( ",'°C' ,")"))) +
  theme(
    plot.title = element_text(size = 14L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(size = 15L,
                                face = "bold",
                                hjust = 0.5),
    axis.title.x = element_text(size = 13L,
                                face = "bold"),
    strip.text.x = element_text(face = "bold"),
    legend.position=c(.2,.75))
   
         
     
    
# loss angeles regreeession and correlation plot
Atlanta<-ggplot(data = s, aes(x =  y , y = yhat)) +
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) +
  geom_point()+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 16.2,color = "blue")+ #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(label.y = 19,color = "blue",output.type = "expression")+ #this means at 30th unit regresion line equation will be shown
  ggthemes::theme_igray()+
  labs(x='Observed Temperature (C)',y='Predicted Temperature(°C)',
       title = 'Observed versus predicted Temperature, Atlanta')+
  theme(
    plot.title = element_text(size = 14L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(size = 15L,
                                face = "bold",
                                hjust = 0),
    axis.title.x = element_text(size = 13L,
                                face = "bold"))



Altan<- qplot(s$y, s$yhat) +
  geom_abline(color='red') +
  ggtitle("Comparing Observed Temperature versus predicted Temperature, Atlanta") +
  theme_minimal()+
  labs(x='Observed Temperature (C)',y='Predicted Temperature(C)')


# Calculating RMSE using rmse()         

cat("final answer Root mean squared error: ",sqrt(mean(s$y -s$yhat,na.rm = TRUE)^2))

# we can say that our model is working at best level

#"New York"
df1<- df %>% 
  filter(City %in% c("New York")) %>% 
  select(Date,AverageTemperature)
colnames(df1)<- c('ds',	'y')
m <- prophet(df1)
future <- make_future_dataframe(m, periods =   7300) # for 20 years
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

tail(future)
#plot(m, forecast,main="Forecasting for Atlanta",xlab='Year',y='Temperature and forecasted')

#prophet_plot_components(m,forecast)

s <- forecast %>% 
  inner_join(df1, by="ds") %>% 
  mutate(
    delta = (y-yhat), 
    isPositive= delta >= 0) %>% 
  select(ds,y,yhat)


# new york
Newyork<-ggplot(data = s, aes(x =  y , y = yhat)) +
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) +
  geom_point()+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 16.2,color = "blue")+ #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(label.y = 19,color = "blue",output.type = "expression")+ #this means at 30th unit regresion line equation will be shown
  ggthemes::theme_igray()+
  labs(x='Observed Temperature (C)',y='Predicted Temperature(C)',
       title = 'Observed versus predicted Temperature, New York')+
  theme(
    plot.title = element_text(size = 14L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(size = 15L,
                                face = "bold",
                                hjust = 0),
    axis.title.x = element_text(size = 13L,
                                face = "bold"))

# rolling mean and save for atlanta as two_newyork
two_newyork<- s %>%
  select(ds, y,yhat) %>%
  mutate(observed = rollmean(y, k =300, fill = NA),
         predicted=rollmean(yhat,k=300,fill=NA)) %>% 
  # plot two lines with observed and predicted values
  ggplot() +
  geom_line(aes(x=ds,y=predicted,color="predicted"),size=1) +
  geom_line(aes(x=ds,y=observed,color="observed"),size=1) +
  ggthemes::theme_igray()+
  labs(x="Date",title='New York',color='Temperature',
       y=expression(paste("Temperature( ","°C" ,")"))) +
  theme(
    plot.title = element_text(size = 14L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(size = 15L,
                                face = "bold",
                                hjust = 0.5),
    axis.title.x = element_text(size = 13L,
                                face = "bold"),
    strip.text.x = element_text(face = "bold"),
    legend.position=c(.2,.75))
NEW<- qplot(s$y, s$yhat) +
  geom_abline(color='red') +
  ggtitle("Comparing Observed Temperature versus predicted Temperature, New York") +
  theme_minimal()+
  labs(x='Observed Temperature (C)',y='Predicted Temperature(C)')


# Calculating RMSE using rmse()         

cat("final answer Root mean squared error: ",sqrt(mean(s$y -s$yhat,na.rm = TRUE)^2))

#"Chicago"
df1<- df %>% 
  filter(City %in% c("Chicago")) %>% 
  select(Date,AverageTemperature)
colnames(df1)<- c('ds',	'y')
m <- prophet(df1,yearly.seasonality = FALSE,daily.seasonality = FALSE)
future <- make_future_dataframe(m, periods =   7300) # for 20 years
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

tail(future)
plot(m, forecast,main="Forecasting for Atlanta",xlab='Year',y='Temperature and forecasted')
s <- forecast %>% 
  inner_join(df1, by="ds") %>% 
  mutate(
    delta = (y-yhat), 
    isPositive= delta >= 0) %>% 
  select(ds,y,yhat)

# chicago
Chicago<-ggplot(data = s, aes(x =  y , y = yhat)) +
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) +
  geom_point()+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 10.1,color = "blue")+ #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(label.y = 9.9,color = "blue",output.type = "expression")+ #this means at 30th unit regresion line equation will be shown
  ggthemes::theme_igray()+
  labs(x='Observed Temperature (C)',y='Predicted Temperature(°C)',
       title = 'Observed versus predicted Temperature, Chicago')+
  theme(
    plot.title = element_text(size = 14L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(size = 15L,
                                face = "bold",
                                hjust = 0),
    axis.title.x = element_text(size = 13L,
                                face = "bold"))
# rolling mean and save for atlanta as two_chicago
two_chicago<- s %>%
  select(ds, y,yhat) %>%
  mutate(observed = rollmean(y, k =300, fill = NA),
         predicted=rollmean(yhat,k=300,fill=NA)) %>% 
  # plot two lines with observed and predicted values
  ggplot() +
  geom_line(aes(x=ds,y=predicted,color="predicted"),size=1) +
  geom_line(aes(x=ds,y=observed,color="observed"),size=1) +
  ggthemes::theme_igray()+
  labs(x="Date",title='Chicago',color='Temperature',
       y=expression(paste("Temperature( ","°C" ,")"))) +
  theme(
    plot.title = element_text(size = 14L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(size = 15L,
                                face = "bold",
                                hjust = 0.5),
    axis.title.x = element_text(size = 13L,
                                face = "bold"),
    strip.text.x = element_text(face = "bold"),
    legend.position=c(.2,.75))
chicago<- qplot(s$y, s$yhat) +
  geom_abline(color='red') +
  ggtitle("Comparing Observed Temperature versus predicted Temperature, Chicago") +
  theme_minimal()+
  labs(x='Observed Temperature (C)',y='Predicted Temperature(C)')


# Calculating RMSE using rmse()         

cat("final answer Root mean squared error: ",sqrt(mean(s$y -s$yhat,na.rm = TRUE)^2))

#"Los Angeles"
df1<- df %>% 
  filter(City %in% c("Los Angeles")) %>% 
  select(Date,AverageTemperature)
colnames(df1)<- c('ds',	'y')
m <- prophet(df1,yearly.seasonality = FALSE,weekly.seasonality = FALSE)
future <- make_future_dataframe(m, periods =   7300) # for 20 years
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

tail(future)
plot(m, forecast,main="Forecasting for Atlanta",xlab='Year',y='Temperature and forecasted')

s <- forecast %>% 
  inner_join(df1, by="ds") %>% 
  mutate(
    delta = (y-yhat), 
    isPositive= delta >= 0) %>% 
  select(ds,y,yhat)
# Los Angeles
Los_Angeles<-ggplot(data = s, aes(x =  y , y = yhat)) +
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) +
  geom_point()+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 16.1,color = "blue")+ #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(label.y = 16.2,color = "blue",output.type = "expression")+ #this means at 30th unit regresion line equation will be shown
  ggthemes::theme_igray()+
  labs(x='Observed Temperature (C)',y='Predicted Temperature(°C)',
       title = ' Observed versus predicted Temperature Los Angeles')+
  theme(
    plot.title = element_text(size = 14L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(size = 15L,
                                face = "bold",
                                hjust = 0),
    axis.title.x = element_text(size = 13L,
                                face = "bold"))
# rolling mean and save for atlanta as two_los_angeles
two_los_angeles<-  s %>%
  select(ds, y,yhat) %>%
  mutate(observed = rollmean(y, k =300, fill = NA),
         predicted=rollmean(yhat,k=300,fill=NA)) %>% 
  # plot two lines with observed and predicted values
  ggplot() +
  geom_line(aes(x=ds,y=predicted,color="predicted"),size=1) +
  geom_line(aes(x=ds,y=observed,color="observed"),size=1) +
  ggthemes::theme_igray()+
  labs(x="Date",title='Los Angeles',color='Temperature',
       y=expression(paste("Temperature( ","°C" ,")"))) +
  theme(
    plot.title = element_text(size = 14L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(size = 15L,
                                face = "bold",
                                hjust = 0.5),
    axis.title.x = element_text(size = 13L,
                                face = "bold"),
    strip.text.x = element_text(face = "bold"),
    legend.position=c(.2,.75))


angles<- qplot(s$y, s$yhat) +
  geom_abline(color='red') +
  ggtitle("Comparing Observed Temperature versus predicted Temperature, Los Angeles") +
  theme_minimal()+
  labs(x='Observed Temperature (C)',y='Predicted Temperature(C)')


# arrange plot


Atlanta +Los_Angeles+Chicago+Newyork
# arrange two_atlanta, two_los_angeles, two_chicago, two_newyork
two_atlanta + two_los_angeles + two_chicago + two_newyork
# Calculating RMSE using rmse()         

cat("final answer Root mean squared error: ",sqrt(mean(s$y -s$yhat,na.rm = TRUE)^2))

NEW+chicago/angles+Altan

data<- na.omit(df[c(1,2)]) 
model = Prophet(yearly_seasonality=True)
model.fit(train)
future = model.make_future_dataframe(periods=10)
forecast = model.predict(future)
# plot
model.plot(forecast)
# evaluate the model
RMSE(test$AverageTemperature, forecast[, 'yhat'])
