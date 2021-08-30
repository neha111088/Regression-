attach(ToyotaCorolla)
corola <- ToyotaCorolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
attach(corola)

View(corola)

library("moments")
summary(corola)

cor(corola)

install.packages("GGally")
install.packages("strings")
library(GGally)
windows()
ggpairs(corola)


install.packages("corpcor")
library(corpcor)
cor2pcor(cor(corola))


model.corola <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data = corola)
summary(model.corola)

model.corola_age <- lm(Price~Age_08_04)
summary(model.corola_age)


model.corola_KM <- lm(Price~KM)
summary(model.corola_KM)


model.corola_HP <- lm(Price~HP)
summary(model.corola_HP)


model.corola_cc <- lm(Price~cc)
summary(model.corola_cc)


model.corola_Doors <- lm(Price~Doors)
summary(model.corola_Doors)


model.corola_Gears <- lm(Price~Gears)
summary(model.corola_Gears)


model.corola_Quarterly_Tax <- lm(Price~Quarterly_Tax)
summary(model.corola_Quarterly_Tax)


model.corola_Weight <- lm(Price~Weight)
summary(model.corola_Weight)



install.packages("car")
library(car)          
vif(model.corola)


avPlots(model.corola)

influence.measures(model.corola)
influenceIndexPlot(model.corola,id.n=3)


model.corola_1 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data = corola[-c(81,222),])
summary(model.corola_1)


model.corola_2 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data = corola[-c(81,222,961),])
summary(model.corola_1)


finalmodel <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data = corola)
summary(finalmodel)
vif(finalmodel)
finalmodel = sqrt(mean(finalmodel$residuals^2))


finalmodel_1 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data = corola[-c(81,222),])
summary(finalmodel_1)
vif(finalmodel_1)
finalmodel_1 = sqrt(mean(finalmodel_1$residuals^2))


finalmodel_2 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data = corola[-c(81,222,961),])
summary(finalmodel_2)
vif(finalmodel_2)
finalmodel_2 = sqrt(mean(finalmodel_2$residuals^2))



finalmodel_3 <- lm(Price~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight, data = corola[-c(81,222,961),])
summary(finalmodel_3)
vif(finalmodel_3)
finalmodel_3 = sqrt(mean(finalmodel_3$residuals^2))



finalmodel_4 <- lm(log(Price)~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight, data = corola[-c(81,222,961),])
summary(finalmodel_4)
vif(finalmodel_4)
finalmodel_4 = sqrt(mean(finalmodel_4$residuals^2))


install.packages("caTools")
library(caTools)
corola_1 <- corola[-81,-222,-961]
split <- sample.split(corola_1$Price,SplitRatio = 0.70)
split


table(split)
corola_1.train <- subset(corola_1,split==TRUE)
corola_1.test <- subset(corola_1,split==FALSE)


model.train <- lm(log(Price) ~ Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight, data = corola_1[-c(81,222,961),])
summary(model.train)
sum(model.train$residuals)
mean(model.train$residuals)
training_RMSE <- sqrt(mean(model.train$residuals^2))
plot(model.train)



predtest <- predict(model.train, corola[-c(81,222,961),])
predtest
testing_errors <- corola_1$Price - predtest
test_RMSE <- sqrt(mean(testing_errors^2))




