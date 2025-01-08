library(readxl)
library(tidyverse)
library(ggcorrplot)
library(Hmisc)

#Loading Data
barca <- read_excel('BarcelonaRE_Data.xlsx',sheet = 2)
bardos <- select(barca,-1,-3)
#Add how many amenities does the place has. The more the best?
bardos$Amenities <- bardos$Elevator + bardos$`"Atico"`+bardos$Terrasse + bardos$Parking + bardos$Kitchen + bardos$Yard
#Correlation of the different variables
ggcorrplot(cor(bardos))
names(bardos)[2]<- paste('m2')
colnames(bardos)[1] <- "Price"

#First model considering the Price in function of the number of rooms, bathrooms ans amenities
model1 <- lm(Price ~ m2 + Rooms + Bathrooms + Amenities, data = bardos)
summary(model1)
hist(residuals(model1), breaks = 200)
plot(fitted.values(model1), residuals(model1))

#We noticed an exponential tendency
plot(bardos$m2, bardos$Price)
plot(bardos$m2, log(bardos$Price))
model2 <- lm(log(Price) ~ m2 + Rooms + Bathrooms + Amenities, data = bardos)
summary(model2)
hist(residuals(model2), breaks = 20)
plot(fitted.values(model2), residuals(model2))
plot(bardos$Amenities, log(bardos$Price))

#We noticed the number of rooms have a big P value so we tried a model without it
model3 <- lm(log(Price) ~ m2 + Bathrooms + Amenities, data = bardos)
summary(model3)
hist(residuals(model3), breaks = 20)
plot(fitted.values(model3), residuals(model3))
plot(bardos$m2, residuals(model3))
plot(bardos$Bathrooms, residuals(model3))
plot(bardos$Amenities, residuals(model3))


bardos$BathPerRoom <- bardos$Bathrooms*bardos$Rooms
#Now we include the city zones as dummy variables
dummies <- model.matrix(~`City Zone` - 1, data = barca)
bartres <- data.frame(log(bardos$Price),bardos,dummies)
ggcorrplot(cor(bartres))
barmodel4 <- data.frame(bartres$Price,bartres$m2,bartres$Bathrooms,bartres$Amenities,bartres[,15:24])
model4 <- lm(log(bartres.Price) ~ ., data= barmodel4) 
summary(model4)
hist(residuals(model4), breaks = 20)
plot(fitted.values(model4), residuals(model4))
plot(bardos$m2, residuals(model4))
plot(bardos$Bathrooms, residuals(model4))
plot(bardos$Amenities, residuals(model4))



dummies <- model.matrix(~`City Zone` - 1, data = barca)
bartres <- data.frame(log(bardos$Price),bardos,dummies)
ggcorrplot(cor(bartres))
barmodel4 <- data.frame(bartres$Price,bartres$m2,bartres$Bathrooms,bartres$Amenities,bartres[,15:23])
model4 <- lm(log(bartres.Price) ~ ., data= barmodel4) 
summary(model4)
hist(residuals(model4), breaks = 20)
plot(fitted.values(model4), residuals(model4))
plot(bardos$m2, residuals(model4))
plot(bardos$Bathrooms, residuals(model4))
plot(bardos$Amenities, residuals(model4))
sum(barmodel4[-1])
boxplot(bardos$Price)
max(bardos$Price)


#We used an interaction between the size and the number of amenities
barmodel5 <- data.frame(bartres$Price,(bartres$m2*bartres$Amenities),bartres$Bathrooms,bartres[,15:23])
colnamesbest = c("Price","M2 x Amenities","Bathrooms","Ciutat Vella","Eixample","Gràcia","Horta Guinardó","Les Corts","Nou Barris","Sant Andreu","Sant Marti","Sants Montjuïc" ) 
barmodel52<-lapply(barmodel5, setNames, colnamesbest)
colnames(barmodel5) <- colnamesbest
model5 <- lm(log(Price) ~ ., data= barmodel5) 
summary(model5)
hist(residuals(model5), breaks = 50)
plot(fitted.values(model5), residuals(model5))
plot(barmodel5$`M2 x Amenities`, residuals(model5))
plot(barmodel5$Bathrooms, residuals(model5))
plot(barmodel5$`Ciutat Vella`, residuals(model5))
plot(barmodel5$Eixample, residuals(model5))
plot(barmodel5$Gràcia, residuals(model5))
plot(barmodel5$`Horta Guinardó`, residuals(model5))
plot(barmodel5$`Les Corts`, residuals(model5))
plot(barmodel5$`Nou Barris`, residuals(model5))
plot(barmodel5$`Sant Andreu`, residuals(model5))
plot(barmodel5$`Sant Marti`, residuals(model5))
plot(barmodel5$`Sants Montjuïc`, residuals(model5))


#Loading the data for forecasting
newdata <- read_excel('rcosas/PriceSubmission.xlsx')
dummiespred <- model.matrix(~`City Zone` - 1, data = newdata)
newdata$Amenities <- newdata$Elevator + newdata$`"Atico"`+newdata$Terrasse + newdata$Parking + newdata$Kitchen + newdata$Yard
predframe <- data.frame((newdata$`m^2`*newdata$Amenities),newdata$Bathrooms,dummiespred)
colnamespred = c("M2 x Amenities","Bathrooms","Ciutat Vella","Eixample","Gràcia","Horta Guinardó","Les Corts","Nou Barris","Sant Andreu","Sant Marti","Sants Montjuïc")
colnames(predframe) <- colnamespred
forecastprice <- predict(model5, newdata = predframe, interval = "prediction")
forecastprice <- exp(forecastprice)
comparacion <- data.frame(forecastprice,predframe)

df.forecast <- data.frame("X.bartres.m2...bartres.Amenities." = 110, "bartres.Bathrooms" = 2, "X.City.Zone.Nou.Barris" = 0, "X.City.Zone.Ciutat.Vella" = 0,"X.City.Zone.Eixample" = 0,"X.City.Zone.Gràcia" = 1,"X.City.Zone.Horta...Guinardó"=0,   "X.City.Zone.Les.Corts"=0, "X.City.Zone.Sant.Andreu"=0, "X.City.Zone.Sant.Marti"=0, "X.City.Zone.Sants...Montjuïc"=0)
forecast <- predict(model5, newdata = df.forecast, interval = "prediction")
df.forecast <- data.frame(df.forecast,forecast)
exp(df.forecast["fit"])
