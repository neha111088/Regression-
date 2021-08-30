attach(Salary_Data)
summary(Salary_Data)

plot(Salary_Data)

cor(Salary_Data)

is.na(Salary_Data)
sum(is.na(Salary_Data))

library(moments)
skewness(Salary_Data)


hist(Salary)

hist(YearsExperience)

qqnorm(Salary_Data$YearsExperience, main = "Salary_Data")
qqline(YearsExperience)

cor(YearsExperience, Salary)
