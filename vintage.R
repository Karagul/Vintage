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

