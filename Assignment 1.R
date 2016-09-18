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
library(ggplot2)
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
fit = glm(Claims ~ ., family = "poisson", data = motor[,-8])
summary(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))
fit$deviance/fit$df.residual
var(motor$Claims)/mean(motor$Claims)
summary(update(fit, . ~ . , family = quasipoisson))


## ------ Question 2 --------
# 2. Exercise 22 on page 77 using the U.S. temperature data. You may use the code for the p-value from
# Exercise 21 with the m in that formula a large number (such as 100), and the cn the value of the test
# statistic Tn,OS that you have computed. This exercise requires some R programming for (b) and (c).

library(SemiPar)
data(ustemp)
names(ustemp)
?ustemp

hist(ustemp$min.temp, breaks = 10)

## (a)
mod1 = glm(min.temp ~ latitude + longitude, family = "gaussian", data = ustemp) 
summary(mod1)
mod2 = update(mod1, . ~ latitude * longitude)
summary(mod2)

AIC(mod1, mod2)

## (b)
## pvalue = 1 - exp(-sum((1-pchisq((1:m) * cn, 1:m))/(1:m)))

m = 100 # try m = 100, 1000, 10000
cn = 3.19 # 2 gives us ~0.29, 4.18 ~ 0.05, 6.7 ~ 0.01, 3.19 ~ 0.10

pvalue = 1 - exp(-sum((1 - pchisq((1:m) * cn, 1:m))/(1:m)))
pvalue

##
X = poly(ustemp$latitude, ustemp$longitude)
x1 = X[,1]
x2 = X[,2]
Y  = ustemp$min.temp

pvalue = function(c_n, m = 100) {
  1 - exp((-sum((1 - pchisq((1:m) * c_n, 1:m))/(1:m))))
}
compare_models = function(null_formula, alt_formula, Cn = 4.18){
  null_model = lm(null_formula)
  null_logLik = logLik(null_model)
  altn_model = lm(alt_formula)
  altn_logLik = logLik(altn_model)
  Tn = 2 * (altn_logLik - null_logLik) / (attr(altn_logLik, "df") - attr(null_logLik, "df"))
  reject = Tn > Cn
  out = data.frame(Tn = Tn, Reject = reject)
  return(out)
}
model_list = function(order, interactions = TRUE) {
  model_form = vector()
  k=1
  for(i in 1:order){
    if(interactions == TRUE) {
      for(j in 0:(i-1)){
        model_form[k] = paste("I(x1^",i," * ","x2^", j,")", sep="")
        k=k+1
        model_form[k] = paste("I(x1^",j," * ","x2^", i,")", sep="")
        k=k+1
      }
    }
    model_form[k] = paste("I(x1^",i," * ","x2^", i,")", sep="")
    k=k+1
  }
  return(model_form)
}
order = 3
model_form = model_list(order)
null_model = Y ~ I(x1) + I(x2)

output = sapply(order:length(model_form), function(x){
  compare_models(null_model, paste("Y ~ ", paste(model_form[1:x], collapse = "+")))
})
model_comparison = as.data.frame(t(output))
model_comparison$model_form = model_form[order:length(model_form)]
best_model_idx = which.max(model_comparison$Tn)
best_model = model_comparison[best_model_idx,]
best_formula = paste("Y ~", paste(model_form[1:best_model_idx], collapse = " + ") ) 
best_p_value = pvalue(unlist(best_model$Tn))

str(model_comparison)

## (c)
k = 5
null_form = model_list(k, interactions = FALSE)
null_model = paste("Y ~ ", paste(null_form, collapse = " + "))
model_form = model_list(k)
model_form = setdiff(model_form, null_form)
model_form = c(null_form, model_form)

output = sapply((k+1):length(model_form), function(x){
  compare_models(null_model, paste("Y ~ ", paste(model_form[1:x], collapse = "+")))
})
model_comparison = as.data.frame(t(output))
model_comparison$model_form = model_form[(k+1):length(model_form)]
best_model_idx = which.max(model_comparison$Tn)
best_model = model_comparison[best_model_idx,]
best_formula = paste("Y ~", paste(model_form[1:best_model_idx], collapse = " + ") ) 
best_p_value = pvalue(unlist(best_model$Tn))

model_comparison
best_formula
best_p_value


## ------ Question 3 --------

foo = diag(1,3)
bar = matrix(rnorm(9), ncol = 3)

# assume lambda = 1
a = solve((foo + solve((t(bar) %*% bar))))
b = (solve(foo) + (t(bar) %*% bar))

a
b

## ------ Question 4 --------
# Consider the R dataset cars with the covariate x=cars$speed and response y=cars$dist. For a grid
# of 100 values of λ, starting at 0, fit a quadratic normal regression model using ridge regression (lambda
# = seq(from = start, to = stop, length = 100) with values for start and stop). Write your
# own R function that constructs the Ridge regression estimator (hence don’t use any of the built-in
# functions that produce Ridge estimators). Plot the values of the estimated coefficients of the quadratic
# effect as a function of λ. Discuss what you see on the plot.

str(cars)
#x = cars$speed
#y = cars$dist
# x = scale(cars$speed, scale = FALSE)
# y = scale(cars$dist, scale = FALSE)
# 
# These next three lines are taken from the lm.ridge implementation in MASS for comparison and checking if lm.ridge scales the variables or only centers them. (Note: lm.ridge() uses biased estimator (1/n) of sigma; scale() uses unbiased estimator (1/(n-1)) for sigma)
# 
# n <- nrow(design_matrix); p <- ncol(design_matrix)
# Xscale <- drop(rep(1/n, n) %*% design_matrix^2)^0.5
# X <- design_matrix/rep(Xscale, rep(n, p))

x = scale(cars$speed)
y = scale(cars$dist)

lambda = seq(from = 0, to = 100, length = 100)
length(lambda)
head(lambda)

# design_matrix = data.frame(intercept = rep(1, length(x)),
#                            speed = x,
#                            speed2 = x ^ 2)

design_matrix = data.frame(speed = x, speed2 = x ^ 2)

cor(cars$speed, (cars$speed) ^ 2)
cor(design_matrix[,1], design_matrix[,2])

ridge = function(x, y, lambda) {
  
  # x : design matrix
  # y : response variable
  # lambda : scalar; lambda parameter in the ridge estimator
  # \beta = [(X^t * X + \lambda * I_p) ^ -1] * (X^t * Y)
  # p = ncol(x) in the above statement
  
  xt_x = t(x) %*% as.matrix(x)
  lambda_matrix = lambda * diag(x = 1, nrow = ncol(x))
  beta_ridge = solve(xt_x + lambda_matrix) %*% (t(x) %*% y) %>% t() %>% data.frame()
  #print(beta_ridge)
  #print(str(beta_ridge))
  return(beta_ridge)
}

## testing if it works
ridge(design_matrix, y, 0.1)

## getting the estimated coefficient values for different lambda values between 0 and 1
estimated_coef = data.frame()

for (i in 1:length(lambda)) {
  estimated_coef = rbind(estimated_coef, ridge(design_matrix, y, lambda[i]))
}

plot(lambda, estimated_coef$speed2, xlab = "lambda", ylab = "Coefficient of Quadratic Effect", type = "l")

ggplot(data = estimated_coef, aes(x = lambda, y = speed2)) + geom_line() + xlab("lambda") + ylab("Coefficient of Quadratic Effect") + ggtitle("Plot of beta_2 vs lambda") + theme_bw()

## cross checking with inbuilt methods
lm_ridge = MASS::lm.ridge(y ~ 0 + speed + speed2, data = design_matrix, lambda = lambda)
lm_ridge$Inter
lm_ridge$scales # uses the biased scale (sigma) estimator 1/n instead of unbiased with 1/(n-1) (Bessel's correction)
summary(lm_ridge)
plot(lambda, data.frame(t(lm_ridge$coef))$speed2, type = "l")
MASS::select(lm_ridge)
lm_ridge$coef











