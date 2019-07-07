#Pull in the Wine data
library(dplyr)
library(data.table)
library(stringr)
wine <- fread('D:/WINE DATA/winemag-data-130k-v2.csv')
wine <- wine[,-1] #drop index column
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
#get rid of wines without price 
ca_pinot <- subset(ca_pinot, !is.na(ca_pinot$price))
#loop and append climate data
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
lm.fit.simple <- lm(price~points+temps+precip, data=clean)
summary(lm.fit.simple)
#log regression 
logfit <- lm(log(price)~points+temps+precip, data=clean)
summary(logfit)

###Let's try to rephrase / reestablish the research question at hand. 
### Why would it be useful to predict the price of a wine? Why would I want to know the price given the region and the grape?
### This might be useful for farmers to know if they have a good Vintage or not (If the weather conditions are ideal for their 
### specific grape)

### On the consumer side, it might be more insightful to derive the Score of a wine. Given the temperature data and the type of wine,
### how "good" will this wine be? Price is not always indicative of quality so a score might be a better indicator. 

### Text analytics would be good for a recommendation system. Input the kind of wines that you like and receive recommendations a la Trade coffeee?
### we would be mining the text fields for descriptors of wine: fruity, bold, strong, light, acidity etc. This would be fun but highly subjective. 
