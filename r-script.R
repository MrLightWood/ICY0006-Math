library(readr)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(dplyr)
library(ggpubr)
library(car)
library(psych)
library(MLmetrics)
library(e1071)

main <- read_csv("C:/Users/saile/Desktop/math/house/train.csv")

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

#Declaring variables
dPrice <- main$"price"
dArea <- main$"sqft_living"
dBedrooms <- main$"bedrooms"
dBathrooms <- main$"bathrooms"
dFloors <- main$"floors"
dCondition <- main$"condition"
dGrade <- main$"grade"
dYear <- main$"yr_built"

#Check for NULL values
sum(is.na(dPrice))
sum(is.na(dArea))
sum(is.na(dBedrooms))
sum(is.na(dBathrooms))
sum(is.na(dFloors))
sum(is.na(dCondition))
sum(is.na(dGrade))

#Histograms
hist(dPrice)
hist(dArea)
hist(dBedrooms)
hist(dBathrooms)
hist(dFloors)
hist(dCondition)
hist(dGrade)
hist(dYear)

#Central tendency
mean(dPrice, trim = 0, na.rm = FALSE)
mean(dArea, trim = 0, na.rm = FALSE)
mean(dBedrooms, trim = 0, na.rm = FALSE)
mean(dBathrooms, trim = 0, na.rm = FALSE)
mean(dFloors, trim = 0, na.rm = FALSE)
mean(dCondition, trim = 0, na.rm = FALSE)
mean(dGrade, trim = 0, na.rm = FALSE)
mean(dYear, trim = 0, na.rm = FALSE)

gm_mean(dPrice)
gm_mean(dArea)
gm_mean(dBedrooms)
gm_mean(dBathrooms)
gm_mean(dFloors)
gm_mean(dCondition)
gm_mean(dGrade)
gm_mean(dYear)

median(dPrice, trim = 0, na.rm = FALSE)
median(dArea, trim = 0, na.rm = FALSE)
median(dBedrooms, trim = 0, na.rm = FALSE)
median(dBathrooms, trim = 0, na.rm = FALSE)
median(dFloors, trim = 0, na.rm = FALSE)
median(dCondition, trim = 0, na.rm = FALSE)
median(dGrade, trim = 0, na.rm = FALSE)
median(dYear, trim = 0, na.rm = FALSE)

getmode(dPrice)
getmode(dArea)
getmode(dBedrooms)
getmode(dBathrooms)
getmode(dFloors)
getmode(dCondition)
getmode(dGrade)
getmode(dYear)

#Variability measures
max(dPrice)
max(dArea)
max(dBedrooms)
max(dBathrooms)
max(dFloors)
max(dCondition)
max(dGrade)
max(dYear)

min(dPrice)
min(dArea)
min(dBedrooms)
min(dBathrooms)
min(dFloors)
min(dCondition)
min(dGrade)
min(dYear)

range(dPrice, na.rm = FALSE)
range(dArea, na.rm = FALSE)
range(dBedrooms, na.rm = FALSE)
range(dBathrooms, na.rm = FALSE)
range(dFloors, na.rm = FALSE)
range(dCondition, na.rm = FALSE)
range(dGrade, na.rm = FALSE)
range(dYear, na.rm = FALSE)

IQR(dPrice)
IQR(dArea)
IQR(dBedrooms)
IQR(dBathrooms)
IQR(dFloors)
IQR(dCondition)
IQR(dGrade)
IQR(dYear)

var(dPrice)
var(dArea)
var(dBedrooms)
var(dBathrooms)
var(dFloors)
var(dCondition)
var(dGrade)
var(dYear)

sd(dPrice)
sd(dArea)
sd(dBedrooms)
sd(dBathrooms)
sd(dFloors)
sd(dCondition)
sd(dGrade)
sd(dYear)

#Or just most measures in one function
describe(main)

#Box plot
boxplot(dPrice, main = "Price")
boxplot(dArea, main = "Area")
boxplot(dBedrooms, main = "Bedrooms")
boxplot(dBathrooms, main = "Bathrooms")
boxplot(dFloors, main = "Floors")
boxplot(dCondition, main = "Condition")
boxplot(dGrade, main = "Grade")
boxplot(dYear, main = "Year")

#Correlation matrix
myCor <- main %>% select("sqft_living", "price", "bedrooms", "bathrooms", "floors",
                         "condition", "grade", "yr_built")
cor(myCor)
ggcorrplot(cor(myCor))

#Scatter plots
ggscatter(main, x = "sqft_living", y = "price", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Area", ylab = "Price")

ggscatter(main, x = "sqft_living", y = "bathrooms", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Area", ylab = "Bathrooms")

ggscatter(main, x = "sqft_living", y = "bedrooms", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Area", ylab = "Bedrooms")

ggscatter(main, x = "sqft_living", y = "grade", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Area", ylab = "Grade")

ggscatter(main, x = "sqft_living", y = "condition", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Area", ylab = "Condition")

ggscatter(main, x = "price", y = "condition", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Price", ylab = "Condition")

ggscatter(main, x = "price", y = "grade", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Price", ylab = "Grade")

ggscatter(main, x = "floors", y = "price", 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Floors", ylab = "Price")

#Less outliers
main <- read_csv("C:/Users/saile/Desktop/math/house/train-no-outliers.csv")

myCor <- main %>% select("sqft_living", "price", "bedrooms", "bathrooms", "floors",
                         "condition", "grade", "yr_built")

cormat <- round(cor(myCor),2)
p.mat <- cor_pmat(myCor)
ggcorrplot(cormat,outline.col = "white",  hc.order = TRUE,
           type = "full",lab = TRUE,p.mat = p.mat)

ggscatter(main, x = "sqft_living", y = "price", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Area", ylab = "Price")

ggscatter(main, x = "sqft_living", y = "bathrooms", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Area", ylab = "Bathrooms")

ggscatter(main, x = "sqft_living", y = "bedrooms", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Area", ylab = "Bedrooms")

ggscatter(main, x = "sqft_living", y = "grade", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Area", ylab = "Grade")

ggscatter(main, x = "price", y = "grade", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Price", ylab = "Grade")

ggscatter(main, x = "grade", y = "price", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Grade", ylab = "Price")

#STAGE 5
test <- read_csv("C:/Users/saile/Desktop/math/house/test-no-outliers.csv")

#Declaring variables
tPrice <- test$"price"
tArea <- test$"sqft_living"
tBedrooms <- test$"bedrooms"
tBathrooms <- test$"bathrooms"
tGrade <- test$"grade"

#Declaring linear models
areaPrice.lm<-lm(tPrice~tArea)
areaBathrooms.lm<-lm(tBathrooms~tArea)
areaBedrooms.lm<-lm(tBedrooms~tArea)
areaGrade.lm<-lm(tGrade~tArea)
priceGrade.lm<-lm(tGrade~tPrice)

#LM Prediction
areaPrice.prediction.lm <- predict (areaPrice.lm, test)
paste0("areaPrice linear model: ",
       MAPE(areaPrice.prediction.lm,tPrice))

areaBathrooms.prediction.lm <- predict (areaBathrooms.lm, test)
paste0("areaBathrooms linear model: ",
       MAPE(areaBathrooms.prediction.lm,tBathrooms))

areaBedrooms.prediction.lm <- predict (areaBedrooms.lm, test)
paste0("areaBedrooms linear model: ",
       MAPE(areaBedrooms.prediction.lm,tBedrooms))

areaGrade.prediction.lm <- predict (areaGrade.lm, test)
paste0("areaGrade linear model: ",
       MAPE(areaGrade.prediction.lm,tGrade))

priceGrade.prediction.lm <- predict (priceGrade.lm, test)
paste0("priceGrade linear model: ",
       MAPE(priceGrade.prediction.lm,tGrade))

#SVM Prediction
areaPrice.svm<-svm(tPrice ~ tArea, data = test)
areaPrice.prediction.svm <- predict(areaPrice.svm, tArea)
paste0("areaPrice svm model: ",
       MAPE(areaPrice.prediction.svm,tPrice))

areaBathrooms.svm<-svm(tBathrooms ~ tArea, data = test)
areaBathrooms.prediction.svm <- predict(areaBathrooms.svm, tArea)
paste0("areaBathrooms svm model: ",
       MAPE(areaBathrooms.prediction.svm,tBathrooms))

areaBedrooms.svm<-svm(tBedrooms ~ tArea, data = test)
areaBedrooms.prediction.svm <- predict(areaBedrooms.svm, tArea)
paste0("areaBedrooms svm model: ",
       MAPE(areaBedrooms.prediction.svm,tBedrooms))

areaGrade.svm<-svm(tGrade ~ tArea, data = test)
areaGrade.prediction.svm <- predict(areaGrade.svm, tArea)
paste0("areaGrade svm model: ",
       MAPE(areaGrade.prediction.svm,tGrade))

priceGrade.svm<-svm(tGrade ~ tPrice, data = test)
priceGrade.prediction.svm <- predict(priceGrade.svm, tPrice)
paste0("areaGrade svm model: ",
       MAPE(priceGrade.prediction.svm,tGrade))

#Area & Price Prediction Plot
plot( tArea,tPrice, xlab="Area", ylab="Price", pch=21)
points (tArea,
        areaPrice.prediction.lm,
        col = "blue",
        pch = 16)
points(tArea,
       areaPrice.prediction.svm,
       col = "red",
       pch = 16)
abline(lm(dPrice ~ dArea, data = main), col = "green")

#Area & Bathrooms Prediction Plot
plot( tArea,tBathrooms, xlab="Area", ylab="Bathrooms", pch=21)
points (tArea,
        areaBathrooms.prediction.lm,
        col = "blue",
        pch = 16)
points(tArea,
       areaBathrooms.prediction.svm,
       col = "red",
       pch = 16)
abline(lm(dBathrooms ~ dArea, data = main), col = "green")

#Area & Bedrooms Prediction Plot
plot( tArea,tBedrooms, xlab="Area", ylab="Bedrooms", pch=21)
points (tArea,
        areaBedrooms.prediction.lm,
        col = "blue",
        pch = 16)
points(tArea,
       areaBedrooms.prediction.svm,
       col = "red",
       pch = 16)
abline(lm(dBedrooms ~ dArea, data = main), col = "green")

#Area & Grade Prediction Plot
plot( tArea,tGrade, xlab="Area", ylab="Grade", pch=21)
points (tArea,
        areaGrade.prediction.lm,
        col = "blue",
        pch = 16)
points(tArea,
       areaGrade.prediction.svm,
       col = "red",
       pch = 16)
abline(lm(dGrade ~ dArea, data = main), col = "green")

#Price & Grade Prediction Plot
plot( tPrice,tGrade, xlab="Price", ylab="Grade", pch=21)
points (tPrice,
        priceGrade.prediction.lm,
        col = "blue",
        pch = 16)
points(tPrice,
       priceGrade.prediction.svm,
       col = "red",
       pch = 16)
abline(lm(dGrade ~ dPrice, data = main), col = "green")
