hellwig(data.train[, 1], data.train[, -c(1, 7:8)])

model <- lm(Z1 ~ Z2 + Z7.medium + Z7.large, data.train)
summary(model)
#reszty normalne 
shapiro.test(model$residuals)

cor(data)
model <- lm(log(Z1 - min(Z1) + 1) ~ log(Z2 - min(Z2) + 1) + log(Z3 - min(Z3) + 1) - 1, data.train)
model <- lm(Z1 ~ Z2 + Z3 - 1, data.train)
#test na istotnośc współcznnika determinacji- wychodzi istotny (statystyka F)
summary(model)
#reszty normalne 
shapiro.test(model$residuals)
#wspó☺łliniowość- niska- easy
vif(model)

#wychodzi 
bptest(model3)
ncvTest(model3)

resettest(model)

#h0: nieistotne
waldtest(model)
#wycodzi istotne więc sztos


