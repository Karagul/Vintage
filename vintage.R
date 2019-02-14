#Pull in the Wine data
library(dplyr)
library(data.table)
library(stringr)
wine <- fread('D:/WINE DATA/winemag-data-130k-v2.csv')
wine <- wine[,-1]
summary(wine)

#california wines 
ca_wines <- subset(wine, wine$province == 'California')
table(ca_wines$variety)

#CA, pinot 
ca_pinot <- subset(ca_wines, ca_wines$variety == "Pinot Noir")

table(ca_pinot$region_1)

#feature engineer the vintage
vintage <- str_extract(ca_pinot$title, '[0-9]{4,}') #gets any 4 digits next to each other
table(vintage) #make sure this looks reasonable

ca_pinot$vintage <- vintage

#time to find our other datasets....
#temp data for windsor b/c most central to russian river valley
#using santa rosa airport scans 
STS <- fread('D:/WINE DATA/NOAA_climate_data_STS_yearly.csv')
#manually put in 2013 and 2015 avg STS temps 


ca_pinot$temps <- c(0)
ca_pinot$precip <- c(0)
#subset the CA_pinot set for the years we have data on 1999 - 2017
ca_pinot <- subset(ca_pinot, ca_pinot$vintage >= 1999)
#loop 
for(i in 1:nrow(ca_pinot)){
  yr <- ca_pinot$vintage[i]
  ca_pinot$temps[i] <- STS[STS$DATE == yr,5]
  ca_pinot$precip[i] <- STS[STS$DATE == yr,4]
}
ca_pinot$temps <- as.numeric(ca_pinot$temps)
ca_pinot$precip <- as.numeric(ca_pinot$precip)

#now that we have complete predictors, get rid of the data we don't need 
clean <- ca_pinot[,c("price", "points","temps", "precip")]
index <- sample(1:nrow(clean),round(0.75*nrow(clean)))
train <- clean[index,]
test <- clean[-index,]
#make a general lineal model
lm.fit <- glm(price~points+temps+precip, data=clean)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$price)^2, na.rm = T)/nrow(test)
#the MSE is AWFUL, try a neural network? 
#begin normalization
maxs <- apply(clean, 2, max) 
mins <- apply(clean, 2, min)
scaled <- as.data.frame(scale(clean, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)
