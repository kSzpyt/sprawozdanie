model.all <- lm(Z1 ~ ., data.train)
step(model.all, direction = "backward")


model.step <- lm(Z1 ~ Z2 + Z3, data.train)
summary(model.step)
#reszty normalne
shapiro.test(model.step$residuals)

cor(data)


bptest(model.step)
ncvTest(model.step)
resettest(model.step)