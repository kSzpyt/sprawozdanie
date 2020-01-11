hellwig(data.train[, 1], data.train[, -c(1, 7:9)])

model <- lm(Z1 ~ Z2 + Z7.medium + Z7.large, data.train)
summary(model)
#reszty normalne 
shapiro.test(model$residuals)

cor(data)
# model <- lm(log(Z1 - min(Z1) + 1) ~ log(Z2 - min(Z2) + 1) + log(Z3 - min(Z3) + 1) - 1, data.train)
model <- lm(Z1 ~ Z2 + Z3 - 1, data.train)
#test na istotnośc współcznnika determinacji- wychodzi istotny (statystyka F)
summary(model)
#reszty normalne 
shapiro.test(model$residuals)
#wspó☺łliniowość- niska- easy
vif(model)

#wychodzi 
bptest(model)
ncvTest(model)

resettest(model)

#h0: nieistotne
waldtest(model)
#wycodzi istotne więc sztos

###zad 5
df.pr <- data.frame(-10.5, 8, 51.7, 12.8, 94.6, 0, 0, 1)
names(df.pr) <- names(data)[-1]
pred1 <- predict(model, newdata = df.pr, interval = 'confidence')
################################
#zad 6
pred <- predict(model, newdata = data.test, interval = 'confidence')

diff <- data$Z1 - pred[, 1]

plot(data.test$Z1, type = "l")
lines(pred[, 1], col = "red")
##########
#zad 7
err <- function(real, pred)
{
  mean(real - pred)
}
err(data$Z1, pred[, 1])
#########
#zad 8 
err.bwzg <- function(real, pred)
{
  mean(abs(real - pred))
}

err.bwzg(data$Z1, pred[, 1])
#zad 9 

err.sr.kw <- function(real, pred)
{
  mean((real - pred)^2)
}

err.sr.kw(data$Z1, pred[, 1])

#zad 10
MAPE <- function(real, pred)
{
  mean(abs((real - pred)*100/real))
}

MAPE(data$Z1, pred[, 1])
