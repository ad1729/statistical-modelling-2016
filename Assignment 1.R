############################################################
### 1st Assignment Statistical Modelling
############################################################

diagnostic <- function(fit) {
  par(mfrow = c(2,2))
  plot(fit1)
  par(mfrow = c(1,1))
  drop1(fit1, test = "F")
  MASS::stepAIC(fit1, scope = list(upper = ~., lower = ~1))
}

## ------ Question 1 --------
library(faraway)
library(dplyr)
#library(MVN)

?motorins
motor = motorins
names(motor)
str(motor)

hist(motor$perd, probability = TRUE)
dim(motor)
summary(motor)

pairs(motor)

## deciding which correlation method to use; 
## pearson affected by outliers, spearman
## https://stats.stackexchange.com/questions/3730/pearsons-or-spearmans-correlation-with-non-normal-data/3733#3733
## testing multivariate normality of pairs of variables
MVN::mardiaTest(motor[,5:8], qqplot = TRUE)
MVN::mardiaTest(motor[,5:6], qqplot = TRUE)
MVN::mardiaTest(motor[,6:7], qqplot = TRUE)
MVN::mardiaTest(motor[,7:8], qqplot = TRUE)
MVN::mardiaTest(motor[,c(5,7)], qqplot = TRUE)
MVN::mardiaTest(motor[,c(5,8)], qqplot = TRUE)
MVN::mardiaTest(motor[,c(6,8)], qqplot = TRUE)
MVN::mardiaTest(motor[,5:7], qqplot = TRUE)
## not multivariate normal and plenty of outliers based on the pairs plot: chose spearman

# so we stick with spearman correlation
cor(motor[,5:8], method = "spearman")
cor(motor[,5:7], method = "spearman")
cor(motor[,5:8], method = "pearson")
sqrt(diag(solve(cor(motor[,c(5,6,7)], method = "spearman")))) # gives us the VIF
sqrt(diag(solve(cor(motor[,c(5,7)], method = "spearman"))))
sqrt(diag(solve(cor(motor[,c(6,7)], method = "spearman"))))
sqrt(diag(solve(cor(motor[,c(5,6)], method = "spearman"))))

fit1 = glm(perd ~ ., family = Gamma(link = "inverse"), data = motor[,-6])
# default link is inverse, others are identity and log; dropping claims and payment
summary(fit1)
par(mfrow = c(2,2))
plot(fit1)
par(mfrow = c(1,1))
drop1(fit1, test = "F")
MASS::stepAIC(fit1, k = 2, scope = list(upper = ~. ^ 2, lower = ~1) , direction = "both")
anova(fit1, test = "Chisq")
summary(update(fit1, . ~ . - Kilometres))

fit2 = update(fit1, family = Gamma(link = "log"))
summary(fit2)
par(mfrow = c(2,2))
plot(fit2)
par(mfrow = c(1,1))
drop1(fit2, test = "F")
MASS::stepAIC(fit2, k = 2, scope = list(upper = ~. ^ 2, lower = ~1), direction = "both")
anova(fit2, test = "Chisq")

fit3 = update(fit1, family = Gamma(link = "identity"))
summary(fit3)
par(mfrow = c(2,2))
plot(fit3)
par(mfrow = c(1,1))
drop1(fit3, test = "F")
MASS::stepAIC(fit3, k = 2, scope = list(upper = ~. ^ 2, lower = ~1), direction = "both")
anova(fit3, test = "Chisq")


## ------ Question 2 --------
?poly

## ------ Question 4 --------