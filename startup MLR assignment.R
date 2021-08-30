startup <- read.csv("C:/Users/urade/Downloads/50Startups.csv")

View(startup)
attach(startup)

install.packages("dummies")
library(dummies)

startup1<-dummy.data.frame(startup)
View(startup1)
attach(startup1)
library("moments")
summary(startup1)

cor(startup1)

install.packages("GGally")
install.packages("strings")
library(GGally)
windows()
ggpairs(startup1)
attach(startup1)


install.packages("corpcor")
library(corpcor)
cor2pcor(cor(startup1))

model.startup1 <- lm(Profit~R.D.Spend+Administration+Marketing.Spend,data=startup1)
summary(model.startup1)

model.startup1rd <-lm(Profit~R.D.Spend)
summary(model.startup1rd)


model.startup1A<-lm(Profit~Administration)
summary(model.startup1A)


model.startup1MS <- lm(Profit~Marketing.Spend)
summary(model.startup1MS)


model.startup1AnMS <-lm(Profit~Administration+Marketing.Spend)
summary(model.startup1AnMS)


install.packages("car")
library(car)          
vif(model.startup1)


avPlots(model.startup1)

influence.measures(model.startup1)
influenceIndexPlot(model.startup1,id.n=3)

summary(finalmodel)
vif(finalmodel)
finalmodel = sqrt(mean(finalmodel$residuals^2))
model.startup2<-lm(Profit~R.D.Spend+Administration+Marketing.Spend,data=startup1[-c(50)])
summary(model.startup2)


finalmodel <- lm(Profit~R.D.Spend+Administration+Marketing.Spend,data = startup1)




finalmodel1 <- lm(Profit~R.D.Spend+Administration+Marketing.Spend,data = startup1[-c(50),])
summary(finalmodel1)
vif(finalmodel1)
finalmodel1 = sqrt(mean(finalmodel1$residuals^2))


model.startup3 <- lm(Profit~R.D.Spend+Administration+Marketing.Spend,data=startup1[-c(50,49),])
summary(model.startup3)



model.startup4 <- lm(Profit~R.D.Spend+Administration+Marketing.Spend,data = startup1[-c(50,49,47,46),])
summary(model.startup4)


finalmodel2 <- lm(Profit~R.D.Spend+Administration+Marketing.Spend,data = startup1[-c(50,49,47,46)])
summary(finalmodel2)
vif(finalmodel2)
finalmodel2 = sqrt(mean(finalmodel2$residuals^2))


hist(finalmodel2$residuals)
plot(finalmodel2)


finalmodel3 <- lm(log(Profit)~ R.D.Spend + Administration + Marketing.Spend, startup1[-c(50),])
summary(finalmodel3)
vif(finalmodel3)
avPlots(finalmodel3)
finalmodel3 = sqrt(mean(finalmodel3$residuals^2))



finalmodel4 <- lm(sqrt(Profit)~ R.D.Spend + Administration + Marketing.Spend, startup1[-c(50),])
summary(finalmodel4)
vif(finalmodel4)
avPlots(finalmodel4)
finalmodel4 = sqrt(mean(finalmodel4$residuals^2))




finalmodel5 <- lm(Profit~ sqrt(R.D.Spend) + sqrt(Administration) + sqrt(Marketing.Spend), startup1[-c(50),])
summary(finalmodel5)                  
finalmodel5 = sqrt(mean(finalmodel5$residuals^2))                


install.packages("caTools")
library(caTools)
startup2 <- startup1[-50,]
split <- sample.split(startup2$Profit,SplitRatio = 0.70)
split

table(split)
startup2.train <- subset(startup2,split==TRUE)
startup2.test <- subset(startup2,split==FALSE)



model.train <- lm(log(Profit)~R.D.Spend + Administration + Marketing.Spend, startup2[-c(50),]) 
summary(model.train)
sum(model.train$residuals)
mean(model.train$residuals)
training_RMSE <- sqrt(mean(model.train$residuals^2))
plot(model.train)


predtest <- predict(model.train, startup1[-c(50),])
predtest
testing_errors <- startup2$Profit - predtest
test_RMSE <- sqrt(mean(testing_errors^2))
