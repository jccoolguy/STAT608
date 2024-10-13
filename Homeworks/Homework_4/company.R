library(car)

dta <- read.csv("company.csv")
attach(dta)

##
## Exploratory data analysis.
##

summary(Sales)
summary(Assets)

par(mfrow = c(2, 2))
hist(Sales)
hist(Assets)
plot(Assets, Sales)
lines(lowess(Assets, Sales))

##
## Transformations: First, do Box-Cox on Assets to see whether it needs to be 
## transformed. Then regress Sales on (the transformed) Assets and do Box-Cox on the 
## residuals to see whether Sales needs to be transformed in its relationship with 
## Assets. 
##

## Box-Cox on Assets. The optimal power lambda is -0.043, with a 95% CI of [-0.2023, 
## 0.1163]. This is consistent with lambda = 0, corresponding to a log transformation.
summary(powerTransform(Assets))
x_1 <- log(Assets)

## Now regress Sales on log(Assets) and pass the residuals to the Box-Cox function. 
## Again, log appears to be an appropriate transformation.
fit_1 <- lm(Sales ~ x_1)
summary(powerTransform(fit_1))
y_1 <- log(Sales)

##
## Final model.
##

fit_2 <- lm(y_1 ~ x_1)

par(mfrow = c(1, 1))
plot(x_1, y_1, xlab = "log(Assets)", ylab = "log(Sales)")
abline(fit_2)

## Model assumptions look reasonable.
par(mfrow = c(2, 2))
plot(fit_2)





