setwd("G:/My Drive/Study/2018d_Autumn/Citadel DataOpen")

install.packages("sqldf")
library(sqldf)

listings <- read.csv("data/listings.csv")
calendar <- read.csv("data/calendar.csv")
real_estate <- read.csv("data/real_estate.csv")

calendar$yymm <- substr(as.character(calendar$date),1,7)

calendar$price_num <- as.numeric(gsub("[^0-9.]", "", as.character(calendar$price)))

query <- "SELECT listing_id, yymm, MEDIAN(price_num) AS med_price
            FROM calendar
           WHERE price_num IS NOT NULL
           GROUP BY listing_id, yymm"

data_ts1 <- sqldf(query)

listings$zipcode <- as.integer(substr(as.character(listings$zipcode),1,5))

query <- "SELECT a.*, b.*
            FROM data_ts1 AS a JOIN listings AS b
           WHERE a.listing_id = b.id AND b.zipcode IS NOT NULL"

data_ts <- sqldf(query)

data_ts$ZHVI <- data_ts$ZRI <- NA

for(i in 1:nrow(data_ts)){
  ind <- real_estate$zipcode==data_ts$zipcode[i] & real_estate$type=="ZHVI"
  if(data_ts$yymm[i]<="2017-06"& sum(ind)==1){
    data_ts$ZHVI[i] <- real_estate[ind, paste("X",gsub("-",".",data_ts$yymm[i]),sep="")]
  }
  
  ind <- real_estate$zipcode==data_ts$zipcode[i] & real_estate$type=="ZRI"
  if(data_ts$yymm[i]<="2017-06"& sum(ind)==1){
    data_ts$ZRI[i] <- real_estate[ind, paste("X",gsub("-",".",data_ts$yymm[i]),sep="")]
  }
}

data_ts$year <- as.integer(substr(data_ts$yymm,1,4))
data_ts$month <- as.factor(substr(data_ts$yymm,6,7))

table(data_ts$year)

hist(data_ts$med_price, breaks=7000, xlim=c(0,1000))
hist(log(data_ts$med_price), breaks=100, xlim=c(0,10))

m_test <- lm(log(med_price)~year+month+metropolitan+year*metropolitan, data=data_ts)
summary(m_test)
anova(m_test)

m_test <- lm(log(med_price)~year+month+metropolitan, data=data_ts)
summary(m_test)
anova(m_test)

m_test <- lm(log(med_price)~year+month+metropolitan+ZHVI+ZRI, data=data_ts)
summary(m_test)
anova(m_test)

write.csv(data_ts, "data/data_ts.csv", row.names=F)

data_sample <- data.frame(zipcode = substr(as.character(listings$zipcode), 1, 5), price = as.numeric(gsub("[^0-9.]", "", as.character(listings$price))))

query <- "SELECT zipcode, MEDIAN(price) AS price
            FROM data_sample
           GROUP BY zipcode"

price_zip <- sqldf(query)

write.csv(price_zip, "data/price_zip.csv", row.names = F)

library(ggplot2)

theme.citadel <- theme(text = element_text(size=20, family="serif"))


tiff("dist1.tiff", width = 7, height = 8, units = 'in', res = 300)
ggplot(data_ts, aes(x=med_price)) +
  geom_histogram(aes(y=..density..),color="white",binwidth=10, fill="cornflowerblue", alpha=.8) +
  xlim(c(0,1000)) +
  xlab("Price ($)") + ylab("Density") + theme.citadel
dev.off()

tiff("dist2.tiff", width = 7, height = 8, units = 'in', res = 300)
ggplot(data_ts, aes(x=log(med_price))) +
  geom_histogram(aes(y=..density..),color="white",binwidth=0.1, fill="cornflowerblue", alpha=.8) +
  xlim(c(2,10)) +
  xlab("log(Price)") + ylab("Density") + theme.citadel
dev.off()

ggplot(data_ts, aes(sample=med_price)) + stat_qq(color="darkorchid1",fill="white",alpha=.3) +
  xlab("Theoretical Quantile") + ylab("Sample Quantile") + theme.citadel



library(randomForest)

select <- !(is.na(data_ts$ZHVI)|is.na(data_ts$ZRI))

rf.fit = randomForest(med_price~year+month+metropolitan+ZHVI+ZRI, data[select,], ntree=200, mtry=20)



rf.pred = predict(rf.fit, data.reg.nn[-train,])
mean(abs(exp(rf.pred)-obs[-train])/obs[-train])
median(abs(exp(rf.pred)-obs[-train])/obs[-train])










exp(7.5e-2)

paste("X",gsub("-",".",data_ts_2$yymm[i]),sep="")

mean(is.na(data_timeseries$med_price))

table(listings$metropolitan)
table(listings$city)
length(unique(listings$city))
length(unique(listings$zipcode))

calendar <- read.csv("data/calendar.csv")
min(as.character(calendar$date))

length(unique(calendar$listing_id))

sum(is.na(listings$property_type))

real_estate <- read.csv("data/real_estate.csv")
