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

hist(motor$perd, probability = TRUE, breaks = 20)
lines(density(motor$perd))
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

diag(solve(cor(motor[,c(5,6,7)], method = "spearman"))) # gives us the VIF
diag(solve(cor(motor[,c(5,7)], method = "spearman")))
diag(solve(cor(motor[,c(6,7)], method = "spearman")))
diag(solve(cor(motor[,c(5,6)], method = "spearman")))

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
MASS::stepAIC(fit1, k = 2, scope = list(upper = ~., lower = ~1) , direction = "both")
anova(fit1, test = "Chisq")
summary(update(fit1, . ~ . - Kilometres))
AIC(fit1)

## Different link functions

fit2 = update(fit1, family = Gamma(link = "log"))
summary(fit2)
par(mfrow = c(2,2))
plot(fit2)
par(mfrow = c(1,1))
drop1(fit2, test = "F")
MASS::stepAIC(fit2, k = 2, scope = list(upper = ~., lower = ~1), direction = "both")
anova(fit2, test = "Chisq")
AIC(fit2)

## using BIC instead of AIC
BIC(fit1) # inverse link
BIC(fit2) # log link
MASS::stepAIC(fit1, k = log(nrow(motor)), scope = list(upper = ~., lower = ~1) , direction = "both")
MASS::stepAIC(fit2, k = log(nrow(motor)), scope = list(upper = ~., lower = ~1), direction = "both")

## checking overdispersion for claims
fit = glm(Claims ~ ., family = poisson, data = motor[,-8])
summary(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))
fit$deviance/fit$df.residual
var(motor$Claims)/mean(motor$Claims)
summary(update(fit, . ~ . , family = quasipoisson))



## ------ Question 2 --------
?poly

## ------ Question 4 --------
# Consider the R dataset cars with the covariate x=cars$speed and response y=cars$dist. For a grid
# of 100 values of λ, starting at 0, fit a quadratic normal regression model using ridge regression (lambda
# = seq(from = start, to = stop, length = 100) with values for start and stop). Write your
# own R function that constructs the Ridge regression estimator (hence don’t use any of the built-in
# functions that produce Ridge estimators). Plot the values of the estimated coefficients of the quadratic
# effect as a function of λ. Discuss what you see on the plot.

str(cars)
x = cars$dist
y = cars$speed
lambda = seq(from = 0, to = 100, length = 100)
length(lambda)
head(lambda)

design_matrix = data.frame(intercept = rep(1, length(x)),
                           distance = x,
                           distance2 = x ^ 2)

ridge = function(x, y, lambda) {
  
  # x : design matrix
  # y : response variable
  # lambda : scalar; lambda parameter in the ridge estimator
  # \beta = [(X^t * X + \lambda * I_p) ^ -1] * (X^t * Y)
  # p = ncol(x) in the above statement
  
  xt_x = t(x) %*% as.matrix(x)
  lambda_matrix = lambda * diag(x = 1, nrow = ncol(x))
  beta_ridge = solve(xt_x + lambda_matrix) %*% (t(x) %*% y) %>% t() %>% data.frame()
  return(beta_ridge$distance2)
}

## testing if it works
ridge(design_matrix, y, 0.1)

## getting the estimated coefficient values for different lambda values between 0 and 1
estimated_coef = c()

for (i in 1:length(lambda)) {
  estimated_coef[i] = ridge(design_matrix, y, lambda[i])
}

plot(lambda, estimated_coef, xlab = "lambda", ylab = "Coefficient of Quadratic Effect", type = "l")

















