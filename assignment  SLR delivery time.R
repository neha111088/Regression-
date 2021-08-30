attach(delivery_time)
summary(delivery_time)

plot(delivery_time)

cor(delivery_time)

is.na(delivery_time)
sum(is.na(delivery_time))

library(moments)
skewness(delivery.Time)
skewness(delivery.time$delivery_time)

sum(is.na(delivery_time))

skewness(Delivery.Time)
skewness(Sorting.Time)

hist(Delivery.Time)
hist(Sorting.Time)

qqnorm(delivery_time$Delivery.Time, main = "Delivery_time")
qqline(Delivery.Time)

qqnorm(Sorting.Time, main = "Sorting_time")
qqline(Sorting.Time)

cor(Delivery.Time,Sorting.Time)

model_l_r = cor(Delivery.Time,Sorting.Time)

reg = lm(Delivery.Time~Sorting.Time)
reg$residuals
mean(reg$residuals)

reg$fitted.values
reg$coefficients

summary(reg)
summary(model_l_r)

model_1_r = cor(Sorting.Time,Delivery.Time)

summary_model_1 = summary(reg)

model_1_RSQ = summary_model_1$r.squared

cor(reg$fitted.values, delivery_time$Delivery.Time)

plot(Delivery.Time , Sorting.Time)



plot(sqrt(Sorting.Time ), Delivery.Time)

model_2_r = cor(Delivery.Time ,sqrt(Sorting.Time))

reg_sqrt = lm(Delivery.Time~sqrt(Sorting.Time))

summary(reg_sqrt)
summary_model_2 = summary(reg_sqrt)

model_2_RSQ = summary_model_2$r.squared






plot(log(Sorting.Time), Delivery.Time)

model_3_r = cor(Delivery.Time , log(Sorting.Time))

reg_log = lm(Delivery.Time~log(Sorting.Time))

summary(reg_log)

reg_log$fitted.values

summary_model_3 = summary(reg_log)

model_3_RSQ =  summary_model_3$r.squared






plot(sqrt(Delivery.Time) , Sorting.Time)

model_4_r = cor(sqrt(Delivery.Time) , Sorting.Time)

reg_sqrt = lm(sqrt(Delivery.Time)~Sorting.Time)

summary(reg_sqrt)

plot(log(Delivery.Time), Sorting.Time)

model_4_r = cor(log(Delivery.Time), Sorting.Time)

reg_log$fitted.values

reg_log = lm(log(Delivery.Time)~Sorting.Time)

summary(reg_log)

summary_model_4 = summary(reg_log)

model_4_RSQ = summary_model_4$r.squared





plot(Delivery.Time,log(Sorting.Time))

model_5_r = cor(Delivery.Time,log(Sorting.Time))

reg_exp = lm(log(Sorting.Time)~Delivery.Time)

summary(reg_exp)

reg_exp$fitted.values

pred_log_y = exp((reg_exp$fitted.values))

err_pred_log_y = Sorting.Time-pred_log_y

summary_model_5 = summary(reg_exp)

model_5_RSQ = summary_model_5$r.squared



model_1_rmse = sqrt(mean(reg$residuals^2))

model_2_rmse = sqrt(mean(reg_sqrt$residuals^2))

model_3_rmse = sqrt(mean(reg_log$residuals^2))

model_4_rmse = sqrt(mean(reg_sqrt$residuals^2))

model_5_rmse = sqrt(mean(reg_exp$residuals^2))



model_results = data.frame(c("SLR","SQRT_X","logerithmic","squareroot_y","exponential"),c("Sorting.Time","SQRT_Sorting.Time","LOG_Sorting.Time","Sorting.Time","Sorting.Time"),c("Delivering.Time","Delivery.Time","Delivery.Time","SQRT_Delivery.Time","LOG_Delivery.Time"),c(model_1_r,model_2_r,model_3_r,model_4_r,model_5_r),c(model_1_RSQ,model_2_RSQ,model_3_RSQ,model_4_RSQ,model_5_RSQ),c(model_1_rmse,model_2_rmse,model_3_rmse,model_4_rmse,model_5_rmse))

colnames(model_results) = c("model_names","output_y","input_x","r","R_ sqaure","rmse")
