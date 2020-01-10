source("Hellwig Method.R")
library(fastDummies)
library(lmtest)
library(car)
library(GGally)
library(het.test)
library(nlme)
ggpairs(data.train)


data <-  read.csv("DaneZ3.csv", sep = ";")
dmmy <- dummy_cols(data$Z7)

data <- cbind(data[, 1:6], dmmy[, c(3, 2)])
colnames(data)[7:8] <- c("Z7.medium", "Z7.large")

set.seed(293478)

train.inx <- sample(1:100, 90)

data.train <- data[train.inx, ]
data.test <- data[-train.inx, ]

boxplot(data.train)
boxplot(data.test)







model.step2 <- lm(Z1^3 ~ Z2 + Z3 + Z4 + Z7.medium + Z7.large, data.train)
summary(model.step2)
shapiro.test(model.step2$residuals)

bptest(model.step2)
ncvTest(model.step2)
resettest(model.step2)

plot(model)

cor(data.train)
