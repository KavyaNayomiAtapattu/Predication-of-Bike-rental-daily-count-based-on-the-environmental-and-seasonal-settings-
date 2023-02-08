#installing packages

#install.packages("corrplot")
#install.packages("magrittr")
#install.packages("anytime")
#install.packages("anydate")
#install.packages("qqplotr")
#install.packages("corrgram")
#install.packages("rsample")
#install.packages("randomForest")
#install.packages("ranger")
#install.packages("caret")
#install.packages("ggthemes")
#install.packages("scales")
#install.packages("wesanderson")
#install.packages("styler")
#install.packages("faraway")
#install.packages("lmtest")
#install.packages("ggpubr") 
#install.packages("itsadug")
#install.packages("hrbrthemes")
#install.packages("lindia")




#Loading packages

library(ggpubr) 
library(tidyverse)
library(corrplot)
library(ggplot2)
library(stats)
library(dplyr)
library(lmtest)
library(car)
library(hrbrthemes)
library(magrittr)
library(lubridate)
library(faraway)
library(itsadug)
library(GGally)
library(corrgram)
library(ggplot2)
library(qqplotr)
library(tidyverse)
library(rsample) # data splitting
library(randomForest) # basic implementation
library(ranger) # a faster implementation of randomForest
library(caret) # an aggregator package for performing many machine learning models
library(ggthemes)
library(scales)
library(wesanderson)
library(styler)
library(tidyverse)
library(skimr)
library(lattice)
library(car)
library(broom)
library(olsrr)
library(lindia)
library(lmtest)



#Rename the columns

names(bike_train)<-c('rec_id','datetime','season','year','month','holiday','weekday','workingday','weather_condition','temp','atemp','humidity','windspeed','total_count')
names(bike_test)<-c('rec_id','datetime','season','year','month','holiday','weekday','workingday','weather_condition','temp','atemp','humidity','windspeed','total_count')


#Read the data
view(bike_train)
view(bike_test)
summary(bike_train)
cor(bike_train)

#Typecasting the datetime and numerical attributes to category


bike_train$month<-as.factor(bike_train$month)
bike_train$season <- as.factor(bike_train$season)
bike_train$holiday<- as.factor(bike_train$holiday)
bike_train$weekday<- as.factor(bike_train$weekday)
bike_train$workingday<- as.factor(bike_train$workingday)
bike_train$weather_condition<- as.factor(bike_train$weather_condition)



bike_test$datetime<- as.Date(bike_test$datetime)
bike_test$year<-as.factor(bike_test$year)
bike_test$month<-as.factor(bike_test$month)
bike_test$season <- as.factor(bike_test$season)
bike_test$holiday<- as.factor(bike_test$holiday)
bike_test$weekday<- as.factor(bike_test$weekday)
bike_test$workingday<- as.factor(bike_test$workingday)
bike_test$weather_condition<- as.factor(bike_test$weather_condition)




#Missing values in dataset
missing_val<-data.frame(apply(bike_train,2,function(x){sum(is.na(x))}))
names(missing_val)[1]='missing_val'
missing_val


#Removing datetime field

bike_train <- bike_train[,-1]
bike_test <- bike_test[,-1]


#Removing rec id

bike_train <- bike_train[,-1]
bike_test <- bike_test[,-1]


#remove year

bike_train <- bike_train[,-2]
bike_test <- bike_test[,-2]

#viewing train and test data sets

view(bike_train)
view(bike_test)
str(bike_train)
str(bike_test)


#boxplot for total_count_outliers
par(mfrow=c(1, 1))#divide graph area in 1 columns and 1 rows
boxplot(bike_train$total_count,main='Total_count',sub=paste(boxplot.stats(bike_train$total_count)$out))


#box plots for outliers
par(mfrow=c(2,2))
#Box plot for temp outliers
boxplot(bike_train$temp, main="Temp",sub=paste(boxplot.stats(bike_train$temp)$out))
#Box plot for atemp outliers
boxplot(bike_train$atemp, main="ATemp",sub=paste(boxplot.stats(bike_train$atemp)$out))
#Box plot for humidity outliers
boxplot(bike_train$humidity,main="Humidity",sub=paste(boxplot.stats(bike_train$humidity)$out))
#Box plot for windspeed outliers
boxplot(bike_train$windspeed,main="Windspeed",sub=paste(boxplot.stats(bike_train$windspeed)$out))



ggplot(bike_train, aes(y = total_count, x = season, fill = season)) + geom_boxplot()  


boxplot(bike_train$cnt ~ bike_train$workingday,
        data = bike_train,
        main = "Total Bike Rentals Vs Working Day",
        xlab = "Non-working Day/Working Day",
        ylab = "Total Bike Rentals",
        col = c("pink", "pink1", "pink2", "pink3")) 

boxplot(bike_train$total_count ~ bike_train$season,
        data = bike_train,
        main = "Total Bike Rentals Vs Season",
        xlab = "Season",
        ylab = "Total Bike Rentals",
        col = c("coral", "coral1", "coral2", "coral3")) 


boxplot(bike_train$total_count ~ bike_train$weather_condition,
        data = bike_train,
        xlim = 4,
        ylim = 6043,
        main = "Total Bike Rentals Vs Weather Situation",
        xlab = "Weather Situation",
        ylab = "Total Bike Rentals",
        col = c("#3399FF", "#0066CC", "#003366", "#006699")) 


plot(data$temp, data$cnt,
     xlim=c(0,0.84917) , ylim=c(0, 6043), 
     pch=18, 
     cex=2, 
     col="#69b3a2",
     xlab = "Temperature in Celsius", ylab="Count of rental bikes",
     main="The scatterplot of Count of rental bikes Vs. Temperature  "
)



plot(bike_train$humidity,bike_train$total_count ,
     xlim=c(0,0.9725) , ylim=c(0,6043), 
     pch=18, 
     cex=2, 
     col="#663333",
     xlab="Normalized humidity", ylab="Count of rental bikes",
     main="The scatterplot of Count of rental bikes Vs. Humdity" 
)
cor(bike_train$humidity,bike_train$total_count)
cor(bike_train$temp,bike_train$total_count)
cor(bike_train$windspeed,bike_train$total_count)


plot(bike_train$windspeed,bike_train$total_count ,
     xlim=c(0,0.50746) , ylim=c(0,6043), 
     pch=18, 
     cex=2, 
     col="#FF6699",
     xlab="Normalized wind speed", ylab="Count of rental bikes",
     main="The scatterplot of Count of rental bikes Vs. Wind Speed" 
)


h <- hist(bike_train$total_count, breaks = 25, ylab = 'Frequency of Rental Bikes', xlab = 'Total Bike Rental Count', main = 'Distribution of Total Bike Rental Count', col = "#99CC66" )

xfit <- seq(min(bike_train$total_count),max(bike_train$total_count), length = 50)
yfit <- dnorm(xfit, mean =mean(bike_train$total_count),sd=sd(bike_train$total_count))
yfit <- yfit*diff(h$mids[1:2])*length(bike_train$total_count)
lines(xfit,yfit, col='red', lwd= 3)



shapiro.test(day_data$cnt)
qqnorm(day_data$cnt)
qqline(day_data$cnt, col='red')




bike_train %>%
  ggplot( aes(x=temp)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Distribution of Normalized Temperature") +
  theme_ipsum()


bike_train %>%
  ggplot( aes(x=atemp)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Distribution of Normalized Feeling Temperature") +
  theme_ipsum()



bike_train %>%
  ggplot( aes(x=windspeed)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Distribution of Normalized Wind Speed") +
  theme_ipsum()



bike_train %>%
  ggplot( aes(x=humidity)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Distribution of Normalized Humidity") +
  theme_ipsum()



#seasons
bike_test$season <- factor(format(bike_test$season, format="%A"),
                           levels = c("1", "2","3","4") , labels = c("Spring","Summer","Fall","Winter")) 


bike_train$season <- factor(format(bike_train$season, format="%A"),
                            levels = c("1", "2","3","4") , labels = c("Spring","Summer","Fall","Winter")) 

#holiday

bike_train$holiday <- factor(format(bike_train$holiday, format="%A"),
                             levels = c("0", "1") , labels = c("Holiday","Not a holiday")) 



view(bike_train)
view(bike_test)



#years

bike_test$year <- factor(format(bike_test$year, format="%A"),
                         levels = c("0", "1") , labels = c("2011","2012")) 

bike_train$year <- factor(format(bike_train$year, format="%A"),
                          levels = c("0", "1") , labels = c("2011","2012")) 


#months

bike_train$month <- factor(format(bike_train$month, format="%A"),
                           levels = c("1","2","3","4","5","6","7","8","9","10","11","12") , labels = c("Jan","Feb","March","April","May","June","July","Aug","Sep","Oct","Nov","Dec")) 


bike_test$month <- factor(format(bike_test$month, format="%A"),
                          levels = c(1,2,3,4,5,6,7,8,9,10,11,12) , labels = c("Jan","Feb","March","April","May","June","July","Aug","Sep","Oct","Nov","Dec")) 



#weather

bike_train$weather_condition <- factor(format(bike_train$weather_condition, format="%A"),
                                       levels = c("1","2","3") , labels = c("Clear+Few Clouds+Partly cloudly","Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist","Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds")) 




#Correlation plot

df1 <- bike_train[,-11]
view(df1)

variables <- c('season','month','holiday','weekday','workingday','weather_condition','temp','atemp','humidity','windspeed')

corrgram(bike_train[,variables],order=F,upper.panel=panel.pie,text.panel=panel.txt,main='Correlation Plot')

ggpairs(df1, title="Pearson correlation and Variable distribution ") 

pairs(bike_train)
cor(bike_train)


#-------------------------------------------------------------------------------------------------------


bike_model$X4 <- factor(format(bike_model$X4, format="%A"),
                        levels = c("1", "2","3","4") , labels = c("Spring","Summer","Fall","Winter"))

bike_model$X5<- factor(format(bike_model$X5, format="%A"),
                       levels = c("0", "1") , labels = c("Holiday","Not a holiday")) 


bike_model$X7 <- factor(format(bike_model$X7, format="%A"),
                        levels = c("1","2","3") , labels = c("Clear+Few Clouds+Partly cloudly","Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist","Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds"))


bike_model$X6<- factor(format(bike_model$X6, format="%A"),
                       levels = c("0", "1","2","3","4","5","6") , labels = c("Mon","Tue","Wed","Thu","Fri","","")) 


view(bike_model)






bike <- bike_model
model1 <- lm(Y~ X1,data= bike)
model1
summary(model1)
anova(model1)

model2 <- lm(Y~ X2,data= bike)
model2
summary(model2)
anova(model2)

model3 <- lm(Y~ X3,data= bike)
model3
summary(model3)
anova(model3)

model4 <- lm(Y~ X4,data= bike)
model4
summary(model4)
anova(model4)

model5 <- lm(Y~ X5,data= bike)
model5
summary(model5)
anova(model5)

model6 <- lm(Y~ X6,data= bike)
model6
summary(model6)
anova(model6)

model7 <- lm(Y~ X7,data= bike)
model7
summary(model7)
anova(model7)

#------------------------------------------------
model8 <- lm(Y~ X1+X2,data= bike)
summary(model8)
anova(model8)

model9 <- lm(Y~ X1+X3,data= bike)
summary(model9)
anova(model9)

model10 <- lm(Y~ X1+X4,data= bike)
summary(model10)
anova(model10)

model11 <- lm(Y~ X1+X5,data= bike)
summary(model11)
anova(model11)

model12 <- lm(Y~ X1+X6,data= bike)
summary(model12)
anova(model12)

model13 <- lm(Y~ X1+X7,data= bike)
summary(model13)
anova(model13)

#-----------------------------------------------

#model14 missing 

model15 <- lm(Y~ X1+X2+X3+X4+X5+X6+X7,data= bike)
summary(model15)


model16 <- lm(Y~ X1+X2+X3+X4+X5+X7,data= bike)
summary(model16)


model17 <- lm(Y~ X1+X2+X3+X4+X7,data= bike)
summary(model17)
res <-residuals(model17)
plot(res)
hist(model17$residuals)
qqnorm(model17$residuals);qqline(model17$residuals)

shapiro.test(model17$residuals)
plot(model17)
abline(model17)

plot(Y~ X1+X2+X3+X4+X7,data= bike)

resid(model17)
model17



bike_model$X4 <- factor(format(bike_model$X4, format="%A"),
                        levels = c("1", "2","3","4") , labels = c("Spring","Summer","Fall","Winter"))

bike_model$X5<- factor(format(bike_model$X5, format="%A"),
                       levels = c("0", "1") , labels = c("Not a holiday","Holiday")) 

bike_model$X6<- factor(format(bike_model$X6, format="%A"),
                       levels = c("0", "1","2","3","4","5","6") , labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")) 
view(bike_model)

bike <- bike_model

model1 <- lm(Y~ X1+X2+X3+X4+X5+X6,data= bike)
summary(model1)


model2 <- lm(Y~ X1+X2+X3+X4+X5,data= bike)
summary(model2)
shapiro.test(model2$residuals)
anova(model2)

par(mfrow=c(2, 2))

qqnorm(model2$residuals);qqline(model2$residuals)

plot(model2$residuals,main = "Residuals Vs. Observation order",xlab= "Observation order", ylab = "Residuals")

hist(model2$residuals,main = "Histogram of Residuals",xlab= "Residuals", ylab = "Frequency")
?hist

plot(model2$fitted.values,model2$residuals,main = "Fitted values Vs. Residuals",xlab= "Fitted values", ylab = "Residuals")
abline()

dwtest(model2)

durbinWatsonTest(model2)


model1$model

res.model = rstandard(model2)
par(mfrow = c(2,2))
plot(model1,which = c(1,1))
hist(res.model,main="Histogram of Standardised Residual")
qqnorm(res.model, manin = "QQ plot of standardised Residuals")
qqline(res.model,col=2,lty=2)
acf(res.model,main="ACF of Standardised Residuals")
print(shapiro.test(res.model))
print(durbinWatsonTest(model2))
print(ncvTest(model2))




residual.analysis(model2)

?rstandard
bgtest(model2, order=5, data=bike)
cooks.distance(model2) 
plot(cooks.distance(model2))


acf(model3$residuals,main="ACF of Standardised Residuals")






#Method 2

----------------------------------------------------------------------
  
  data <- day_data

data$season <- as.factor(data$season)
data$workingday <- as.factor(data$workingday)
data$weathersit <- as.factor(data$weathersit)


training_set <- data %>% filter(yr == 0)
testing_set <- data %>% filter(yr == 1)

#both holiday and weekday information is given by working day variable.
#month variable is removed since it has 12 levels and the model becomes complicated.
#


training_set <- training_set %>% select(-c(instant, dteday, yr, mnth, holiday, weekday))

qplot(data = training_set, hum, cnt,ylab="Total count") #humidity removed since no effect on cnt
qplot(data = training_set, atemp)
qplot(data = training_set, temp, ylab="Total count")
qplot(data = training_set, windspeed, ylab="Total count")


view(day_data)
quantitative_data <- training_set %>% select(c(temp, atemp, hum,windspeed,cnt))
cor(quantitative_data, use = "complete.obs")
cor.test(training_set$hum, training_set$cnt) #put into data analysis part

#Now plot 2 quantitative variables with one catergorical variables and check for interactions.
#Get interaction plot for two catergorical variables and tell that there is interaction between catergorical varibales
#There is a possiblity that 3 way interactions are also there.
#Limitaion 01 - No interactions are considered

#humidity removed, atemp removed

model1 <- lm(data = training_set, cnt ~ temp + windspeed + season + workingday + weathersit)
summary(model1)
#working day removed by backward elimination

model2 <- lm(data = training_set, cnt ~ temp + windspeed + season + weathersit)
summary(model2)


#--------------------------------------

bike_model$X4 <- factor(format(bike_model$X4, format="%A"),
                        levels = c("1", "2","3","4") , labels = c("Spring","Summer","Fall","Winter"))


bike_model$X5 <- factor(format(bike_model$X5, format="%A"),
                        levels = c("0", "1") , labels = c("Either weekend or a holiday ","Neither weekend nor holiday"))


bike_model$X6 <- factor(format(bike_model$X6, format="%A"),
                        levels = c("1","2","3") , labels = c("Clear+Few Clouds+Partly cloudly","Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist","Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds"))




bike <- bike_model
view(bike_model)

model1 <- lm(Y~ X1+X2+X3+X4+X5+X6,data= bike)
summary(model1)


model2 <- lm(Y~ X1+X2+X3+X4+X6,data= bike)
summary(model2)




model2_fitresid <- augment(model2)
shapiro.test(model2_fitresid$.resid)
plot(model2, which = 2)
#not normal
plot(model2, which = 1)
#not constant

#checking outliers
ols_plot_resid_stand(model2)
#influential points
ols_plot_cooksd_bar(model2)

#Y outliers are also there but removing them leads to non normality.


#Step 01
#removinf influential points
model2_fitresid <- model2_fitresid %>% filter(model2_fitresid$.cooksd <= 0.011)


#refitting without influentials
model3 <- lm(data = model2_fitresid, Y ~ X1 + X2 + X3 + X4 + X6)
summary(model3)
anova(model3)
model3_fitresid <- augment(model3)

shapiro.test(model3_fitresid$.resid)
#normality is good

#Graphical check for constant error variance
sq_resid <- model3_fitresid$.resid^2

model3_fitresid <- data.frame(model3_fitresid, sq_resid)

ggplot(model3_fitresid, aes(x = '.fitted', y = 'sq_resid')) +
  geom_point()

ggplot(model3_fitresid, aes(x = .fitted, y = .resid)) +
  geom_point()


?ggplot
#BP Test
bptest(model3)
#H0 - error variance is constant
#error variance is not constant hence its heterscedastic

#Now we have to check for the independency.

#Checking for multicollinearity
vif(model3)
corvif(model3)
#Note that multicollinearity of qualitative predictors are not interpretable

#Now we have to check for autocorrelation
autoplot()
?vif


qqnorm(model3$residuals);qqline(model3$residuals)


par(mfrow = c(2, 2))
plot(model3)



faraway::vif(model3)

stepVIF(model3)



ts <- ts(model3$residuals)
autoplot(model3$residuals)
?autoplot

acf()
bgtest(model3, order=4, data=bike)


view(test)



test$X6 <- factor(format(test$X6, format="%A"),
                  levels = c("1","2","3") , labels = c("Clear+Few Clouds+Partly cloudly","Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist","Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds"))

test$X4 <- factor(format(test$X4, format="%A"),
                  levels = c("1", "2","3","4") , labels = c("Spring","Summer","Fall","Winter"))





testing <- test

ypred = predict(model3,newdata = testing)
MSE = mean((ypred-testing$Y)^2)
MSE

ypred2 = predict(model2,newdata = testing)
MSE2 = mean((ypred2-testing$Y)^2)
MSE2



predictions <- model3 %>% predict(testing)
data.frame( R2 = R2(predictions, testing$Y),
            RMSE = RMSE(predictions, testing$Y),
            MAE = MAE(predictions, testing$Y))


predictions1 <- model2 %>% predict(testing)
data.frame( R2 = R2(predictions1, testing$Y),
            RMSE = RMSE(predictions1, testing$Y),
            MAE = MAE(predictions1, testing$Y))


