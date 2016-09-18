
library(dplyr)
library(ggplot2)
library(magrittr)

############################################################################################
#
#   Part (A)
#
############################################################################################

rm(list = ls())

fulldata.A = read.table("/home/ad/Desktop/KUL Course Material/Statistical Modelling/Assignment/Examdata2016-A.txt", header = T)
set.seed(475672)
rownumbers = sample(1:589,size = 150)
mydata.A = fulldata.A[rownumbers,]

mydata.A %<>% mutate(province = floor(mydata.A[,1]/10000) + 9*(floor(mydata.A[,1]/1000) == 21) + 8*(floor(mydata.A[,1]/1000) == 25)) 

fulldata.A %<>% mutate(province = floor(fulldata.A[,1]/10000) + 9*(floor(fulldata.A[,1]/1000) == 21) + 8*(floor(fulldata.A[,1]/1000) == 25))

mydata.A = mydata.A[sort.list(mydata.A$IntlMig_In),]
#detach(mydata.A)
#attach(temp_data)

# Column 1 contains the NIS (National Institute of Statistics) code of each municipality. This code is
# unique and determines the province. Numbers starting with 1 indicate ‘Antwerp’, starting with 23 and
# 24 means ‘Flemish Brabant’, starting with 25 means ‘Walloon Brabant’, starting with 21 means ’Brussels
# area’. You may consider the Brussels area as a separate ‘province’. Numbers starting with 3 are ‘West-
# Flanders’, with 4 indicate ‘East-Flanders’, 5 indicates ‘Hainaut’, 6 stands for ‘Li`ege’, 7 for ‘Limburg’, 
# 8 for ‘Luxembourg’ and 9 for ‘Namur’. In total you should identify 11 such provinces (10 plus Brussels).

province_name = c("Antwerp", "Flemish Brabant", "West Flanders", "East Flanders", "Hainaut", "Liege", "Limburg", "Luxembourg", "Namur", "Walloon Brabant", "Brussels")

str(mydata.A)
names(mydata.A)

# heatmap
mydata.A %>% select(-Code) %>% cor(method = "spearman") %>%
  corrplot::corrplot(method = "square", order = "hclust", addCoef.col = "black")

# pairs plot
mydata.A %>% select(-Code) %>% pairs()

attach(mydata.A)

# Question 1
# Present the average number of births per 1000 inhabitants (y) and the average number of incoming interna-
# tional migrants per province and test the hypothesis that the provinces Flemish Brabant and Brussels
# have similar numbers of births per 1000 inhabitants and a similar number of incoming international
# migrants. State the null hypotheses, the assumptions and the test you use. Briefly discuss.

mydata.A %>% 
  group_by(province) %>%
  summarise(birthsPer1000 = mean(y), AvgIntl_in = mean(IntlMig_In))

# H_0:   
mydata.A %>% 
  group_by(province) %>% count(province)

# filter province 2 and 11
temp_df = filter(mydata.A, province %in% c(2,11)) %>% 
  dplyr::select(y, IntlMig_In, province) %>% 
  mutate(province = as.factor(province)) %>% arrange(province)

# wilcoxon test for testing the means of crude birth rate
wilcox.test(formula = y ~ province, conf.int = TRUE, data = temp_df)
wilcox.test(formula = IntlMig_In ~ province, conf.int = TRUE, data = temp_df)

# comparing against the whole dataset

fulldata.A %>% 
  group_by(province) %>% count(province)

# filter province 2 and 11
temp_df_full = filter(fulldata.A, province %in% c(2,11)) %>% 
  dplyr::select(y, IntlMig_In, province) %>% 
  mutate(province = as.factor(province)) %>% arrange(province)

# wilcoxon test for testing the means of crude birth rate
wilcox.test(formula = y ~ province, conf.int = TRUE, data = temp_df_full)
wilcox.test(formula = IntlMig_In ~ province, conf.int = TRUE, data = temp_df_full)

# Question 2
# A.2 (i) Construct a flexible regression fit using splines with the number of births per 1000 inhabitants as response and the number of incoming international migrants as a covariate. You can use transformations of both variables if you decide that that should be more appropriate. 
# 
# (ii) Based on the resulting plot using default values, suggest a simplification with exactly 4 knots. Add to the same plot the fitted line, obtained using these four knots in a linear spline model with truncated polynomials (no penalization). Briefly discuss. 
# 
# (iii) Use two different model selection criteria to decide which, if any, of these four knots in question A.2(ii) should be kept in the model. Write the selected regression model using statistical notation. Briefly discuss.

plot(log(mydata.A$IntlMig_In), mydata.A$y)

library(splines)
library(SemiPar)

y_temp = y * 1000

fit1 = spm(y ~ f(IntlMig_In), family = "poisson")
summary(fit1)
plot(fit1)
points(IntlMig_In, log(y))

# can put knots at 400, 2000, 6000, 10000
fit_part = glm(y ~ IntlMig_In 
              + pmax((IntlMig_In - 50),0)
              + pmax((IntlMig_In - 400),0)
              + pmax((IntlMig_In - 1300),0)
              + pmax((IntlMig_In - 9500),0)
#              + pmax((IntlMig_In - 10000),0)
, family = Gamma(link = "log"))
summary(fit_part)
#plot(fit_part)

drop1(fit_part, test = "LRT")

lrtest(update(fit_part, . ~ . - pmax((IntlMig_In - 50),0) - pmax((IntlMig_In - 9500),0)), fit_part)

# plotting the picewise linear model on the spline fit
plot(fit1, ylim = c(2,3.2), xlab = "Number of Incoming International Migrants", ylab = "log(Expected birth rate per 1000)", lwd = 2)
points(IntlMig_In, log(y))
#plot((mydata.A$IntlMig_In)^0.5, mydata.A$y)
lines(IntlMig_In, log(fit_part$fitted.values), col = "green", lwd = 3)
abline(v = c(50, 400, 1300, 9500), lty = 2, lwd = 2)
legend(2800, 2.4, legend = c("Penalized Spline (33 knots)", "Linear spline w/ 4 knots (no penalization)"), col = c("black", "green"), lwd = c(2,3), lty = c(1,1))
title(main = "knots at 50, 400, 1300, 9500")

# one can be BIC
drop1(fit_part, test = "LRT")

BIC(fit_part)
BIC(update(fit_part, . ~ . - pmax((IntlMig_In - 50),0) - pmax((IntlMig_In - 9500),0)))


# Question 3
# A.3 Fit an additive model with Income and Zero Income as the explanatory variables and the number of births per 1000 inhabitants as the response. Write the model in appropriate notation, and provide a summary, including the value of the degrees of freedom of each additive component. Also construct two plots containing the fitted curves (one with Income as the x variable, the other for Zero Income). In which way do these two variables influence the
# number of births per 1000 inhabitants?

library(gam)
library(mgcv)

fit_additive = spm(y ~ f(Income, basis = "trunc.poly") + f(Zero_Income, basis = "trunc.poly"), family = "poisson")
summary(fit_additive)
par(mfrow = c(1,2))
plot(fit_additive)

fit_gam = gam(y ~ s(Income, bs = "cr") + s(Zero_Income, bs = "cr"), family = Gamma(link = "log"))
summary(fit_gam)
plot(fit_gam)


# Question 4
# A.4 In case the model in A.3 was not fully parametric, first provide a reasonable parametric approximation. Otherwise, work with the parametric model. In this parametric model, test the additivity assumption at the 1% level of significance.

library(hglm)
fit_hglm = hglm(fixed = y ~ Income + Zero_Income, random =  ~ 1|province, family = Gamma(link = log))
summary(fit_hglm)


fit_glm_add = glm(y ~ Income + Zero_Income, family = Gamma("log"))
fit_glm_full = glm(y ~ Income + Zero_Income + Income:Zero_Income + I(Income^2) + I(Zero_Income^2), family = Gamma("log"))
summary(fit_glm_add)
summary(fit_glm_full)

library(lmtest)

lrtest(fit_glm_add, fit_glm_full)
drop1(fit_glm_full, scope = ~Income:Zero_Income + I(Income^2) + I(Zero_Income^2), test = "LRT")

# Question 5
# A.5 Now include all variables in your analysis. Select a reasonable model to explain the number of births per 1000 inhabitants as a function of the other variables. Mention the method you used for the variable search, a description of the collection of models involved in the search and give the final model, including parameter estimates and standard errors. Do not give any “in-between output. Give a brief discussion and interpretation of the final model.

# last chapter in the notes on semiparametric modelling

# fitting splines using spm
fit_all = spm(y ~ Income + Zero_Income + f(Density) + f(Marriages) + f(Divorces) + f(Deaths) + f(IntMig_In) + f(IntMig_Out) + f(IntlMig_In) + f(IntlMig_Out) + f(Nationality_In) + f(Population_Dec), family = "poisson", random = ~1, group = province)

summary(fit_all)
plot(fit_all, jitter.rug = TRUE)


# fitting splines using gam
fit_all_gam = gam(y ~ s(Income) + s(Zero_Income) + s(Density) + s(Marriages) + s(Divorces) + s(Deaths) + s(IntMig_In) + s(IntMig_Out) + s(IntlMig_In) + s(IntlMig_Out) + s(Nationality_In) + s(Population_Dec) + s(province), family = Gamma("log"))

summary(fit_all_gam)
par(mfrow = c(3,2))
plot(fit_all_gam)
par(mfrow = c(1,1))

# fitting spm
fit_all_spm = spm(y ~ Income + Zero_Income + Density + Marriages + Divorces + Deaths + IntMig_In + IntMig_Out + f(IntlMig_In) + IntlMig_Out + Nationality_In + Population_Dec, family = "poisson", random = ~1, group = province)

summary(fit_all_spm)


# GEE taking the correlated nature into account
library(gee)
fit_gee = gee(y ~ Income + Zero_Income + Density + Marriages + Divorces + Deaths + IntMig_In + IntMig_Out + IntlMig_In + I(IntlMig_In ^ 2) + IntlMig_Out + Nationality_In + Population_Dec, id = province, family = Gamma("log"))

summary(fit_gee)

fit_final = glm(y ~ Income + Zero_Income + Density + Marriages + Divorces + Deaths + IntMig_In + IntMig_Out + IntlMig_In + I(IntlMig_In ^ 2) + I(IntlMig_In ^ 3) + IntlMig_Out + Nationality_In + Population_Dec + as.factor(province), family = Gamma("log"))
summary(fit_final)

drop1(fit_final, test = "LRT")

fit_reduced = glm(y ~ Income + Deaths + IntMig_In + IntMig_Out + IntlMig_In + I(IntlMig_In ^ 2) + I(IntlMig_In ^ 3) + IntlMig_Out + Population_Dec, family = Gamma("log"))
summary(fit_reduced)

lrtest(fit_reduced, fit_final)

fit_reduced2 = glm(y ~ Income + Deaths + IntMig_In + IntMig_Out + IntlMig_In + pmax((IntlMig_In - 400),0) + pmax((IntlMig_In - 1300),0) + IntlMig_Out + Population_Dec, family = Gamma("log"))
summary(fit_reduced2)

BIC(fit_reduced)
BIC(fit_reduced2)

############################################################################################
#
#   Part (B)
#
############################################################################################

rm(list = ls())

set.seed(475672)

# num of samples
n = 400 # 50, 400

# number of simulations
nsim = 1000

# list to store the final results
final50 = list()

final400 = list()

# simulation code
for (i in 1:nsim) {

x2 = runif(n, -1, 1)
x3 = runif(n, -1, 1)
x4 = runif(n, -1, 1)
x5 = runif(n, -1, 1)
x6 = runif(n, -1, 1)

mu_i = (1/3) + ((2/3) * x2) + x3 + ((4/3) * x4) + (0 * x5) + (0 * x6)

y = rpois(n, exp(mu_i))

des_mat = data.frame(Int = 1, x2, x3, x4, x5, x6)

L = 5

# if this seems black-box-ish, then run it for L = 1, 2, 3, etc
combinations = function(L){
  comb = NULL
  for (i in 1:L) {comb = rbind(cbind(1,comb),cbind(0,comb))}
  return(comb)
}

# models to be fit
#combination = cbind(1, combinations(5))
combination = cbind(1, combinations(5))
aic = c()
bic = c()
tic = c()
form = c()

# des_mat[,combination[j,] > 0] selects the column from the design matrix corresponding to the combination
# so combination[5,] > 0 gives
# > TRUE  TRUE  TRUE FALSE  TRUE  TRUE
# which selects all the columns except the 4th one (so x4 is dropped)

for (k in seq_along(1:2 ^ L)) {
  
  #k = 3
  #print(k)
  temp_df = des_mat[,combination[k,] > 0]
  #str(temp_df)
  
  # this block is included because when k = 2^L only the intercept term is in the model in which case temp_df returns a vector and not a data frame so we can fit this manually for the last iteration; stupid edge cases
  if (k == 2 ^ L) {
    formulae = "y ~ 1"
    fit1 = glm(formulae, family = poisson(link = "log"))
  } else{
    formulae = temp_df %>% names() %>% paste(collapse = " + ") %>%
      paste("y ~ 0 + ", ., sep = "")
    fit1 = glm(formulae, data = temp_df, family = poisson(link = "log"))
  }
  
  #print(formulae)
  
  #print(summary(fit1))
  aic[k] = AIC(fit1)
  bic[k] = BIC(fit1)
  form[k] = formulae
  
  # calculating TIC
  # use y = y and x = temp_df
  temp_df %<>% as.matrix()
  beta_est = fit1$coefficients %>% as.matrix()
  mu_i_est = (temp_df %*% beta_est) %>% exp()
  
  first_deriv = (temp_df * as.vector((y - mu_i_est))) %>% as.matrix()
  
  # K matrix using the score equation 
  Kmatrix = (t(first_deriv) %*% first_deriv)/n # p x p matrix
  
  # I(\beta) = (X^T)(D(\beta))(X)
  # where D(\beta) = the diagonal matrix with mu_i_est on the diagonal elements
  #temp_diag = diag(as.vector(mu_i_est))
  #dim(as.matrix(temp_diag))
  #dim(t(temp_df) %*% temp_diag %*% temp_df)
  #
  # (Observed) Fisher Information Matrix
  # p x p matrix; can compare with solve(vcov(fit1)); not numerically accurate
  Jmatrix = (t(temp_df) %*% diag(as.vector(mu_i_est)) %*% temp_df)/n 
  penaltyTIC = (solve(Jmatrix) %*% Kmatrix) %>% diag() %>% sum()
  
  tic[k] = (-2 * c(logLik(fit1))) + (2 * penaltyTIC)
 
  
}

results = data.frame(Model = form, AIC = aic, BIC = bic, TIC = tic)
#results %>% head()
bestAIC = results %>% filter(AIC == min(AIC)) %>% select(Model, AIC)
bestBIC = results %>% filter(BIC == min(BIC)) %>% select(Model, BIC)
bestTIC = results %>% filter(TIC == min(TIC)) %>% select(Model, TIC)

#i = 1 # for debugging
if (n == 50) { 
  final50$modelAIC[i] = as.character(bestAIC$Model)
  final50$AIC[i] = bestAIC$AIC
  
  final50$modelBIC[i] = as.character(bestBIC$Model)
  final50$BIC[i] = bestBIC$BIC
  
  final50$modelTIC[i] = as.character(bestTIC$Model)
  final50$TIC[i] = bestTIC$TIC
} 

if (n == 400) { 
  final400$modelAIC[i] = as.character(bestAIC$Model)
  final400$AIC[i] = bestAIC$AIC
  
  final400$modelBIC[i] = as.character(bestBIC$Model)
  final400$BIC[i] = bestBIC$BIC
  
  final400$modelTIC[i] = as.character(bestTIC$Model)
  final400$TIC[i] = bestTIC$TIC
}

stopifnot(n != 50 || n != 400)

}
beepr::beep(1)

## analyzing the results

save(final50, final400, file = "/home/ad/Desktop/KUL Course Material/Statistical Modelling/R programming/ques_b_results.RData")

final50 %<>% data.frame()
# n = 50
table(final50$modelAIC)
table(final50$modelBIC)
table(final50$modelTIC)

final400 %<>% data.frame()
# n = 400
table(final400$modelAIC)
table(final400$modelBIC)
table(final400$modelTIC)


############################################################################################
#
#   Part (C)
#
############################################################################################

rm(list = ls())

library(glmnet)
library(lqa)
library(xtable)

fulldata.C = read.table("/home/ad/Desktop/KUL Course Material/Statistical Modelling/Assignment/Examdata2016-C.txt", header = T, sep = ",")
set.seed(475672)
rownumbers = sample(1:71,61,replace = F)
mytraining = fulldata.C[rownumbers,]
mytest = fulldata.C[-rownumbers,]

dim(mytraining)

# 1st column is response y
# rest 4088 are covariates

# try ridge, lasso, elastic net, adaptive lasso and bridge

# calulating MSPE
# myfit = ModelProcedure(X=mytraining_covariates, Y=mytraining_response)
# mypredictions = predict(myfit, X=mytest_covariates)
# mymspe = sum((mytest_response-mypredictions)^2).

## Ridge regression
#ridge_cv = cv.lqa(y.train = mytraining[,1], x.train = mytraining[,-1], n.fold = 5, loss.func = "gcv.loss", family = "gaussian", penalty.family = "ridge", lambda.candidates = list(seq(0,10,0.1)))

ridge_cv = cv.glmnet(x = data.matrix(mytraining[,-1]), y = data.matrix(mytraining[,1]), nfolds = 5, alpha = 0, type.measure = "mse")
plot(ridge_cv)
ridge_cv$lambda.min

# s = lambda.1se (default) or lambda.min
pred = predict(ridge_cv, newx = data.matrix(mytest[,-1]), s = "lambda.min") %>% as.vector()
sum((mytest[,1] - pred) ^ 2)/10

## lasso regression
lasso_cv = cv.glmnet(x = data.matrix(mytraining[,-1]), y = data.matrix(mytraining[,1]), nfolds = 3, alpha = 1, lambda = seq(0,10, 0.1), type.measure = "mae")
plot(lasso_cv)
lasso_cv$lambda.min

# s = lambda.1se (default) or lambda.min
pred = predict(lasso_cv, newx = data.matrix(mytest[,-1]), s = "lambda.min") %>% as.vector()
(mytest[,1] - pred) ^ 2 %>% mean()

## elastic net
en_cv = cv.glmnet(x = data.matrix(mytraining[,-1]), y = data.matrix(mytraining[,1]), nfolds = 3, alpha = 0.4, lambda = seq(0,10, 0.1), type.measure = "mae")
plot(en_cv)
en_cv$lambda.min

pred = predict(en_cv, newx = data.matrix(mytest[,-1]), s = "lambda.min") %>% as.vector()
(mytest[,1] - pred) ^ 2 %>% mean()

# variables for elastic net
alpha = seq(0, 1, 0.1)
mspe = c()
lambda_min = c()
lambda_1se = c()
sspe = c()
num_coef = c()

for (i in seq_along(alpha)) {
  
  #i = 5
  
  # fitting the model to the training set
  mod_cv = cv.glmnet(x = data.matrix(mytraining[,-1]), y = data.matrix(mytraining[,1]), nfolds = 5, alpha = alpha[i], type.measure = "mse")#, lambda = seq(0,100, 0.1))
  
  plot(mod_cv, main = paste("alpha = ", alpha[i], sep = ""))
  
  lambda_min[i] = mod_cv$lambda.min
  lambda_1se[i] = mod_cv$lambda.1se
  
  # getting the predictions on the test set
  pred = predict(mod_cv, newx = data.matrix(mytest[,-1]), s = "lambda.min") %>% as.vector()
  
  # sum of squared prediction errors
  #sspe[i] = (mytest[,1] - pred) ^ 2 %>% sum() 
  #mspe[i] = (sspe[i]/nrow(mytest))
  mspe[i] = (mytest[,1] - pred) ^ 2 %>% mean()
  
  # number of coefficients in the selected model
  select_coef = coef(mod_cv, s = "lambda.min") %>% as.matrix()
  num_coef[i] = select_coef[select_coef != 0,] %>% length()
  
}

beepr::beep(3)

results = data.frame(alpha, MSPE = mspe,  lambda_min, lambda_1se, num_coef)
results
results %>% xtable()

# nonnegative garrote is out since p > n

## adaptive lasso (using the coefficients from the ridge fit above)
## see Zou 2006 adaptive lasso (or p 91 from course text) for more details
## adaptive.weights = 1/abs(\beta_ols or \beta_ridge)^\gamma; \gamma > 0 and arbitrary

# the ridge approach above seems to pick arbitrary values of lambda, it is interesting to do this a few 1000 times and pick the mode of that distribution of lambda values that are selected

require(doMC)
registerDoMC(cores = 4)

ridge_lambda = c()

for (i in 1:100) {
  
  print(i)
  
  temp_cv = cv.glmnet(x = data.matrix(mytraining[,-1]), y = data.matrix(mytraining[,1]), nfolds = 4, alpha = 0, type.measure = "mse", parallel = TRUE)

  ridge_lambda[i] = temp_cv$lambda.min

}
# 
# beepr::beep(3)

hist(ridge_lambda, breaks = 100)
summary(ridge_lambda) # median is 7 across 1000 runs
mean(ridge_lambda, trim = 0.25)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   3.000   7.000   7.644  11.000  33.000

#ridge_coef = coef(ridge_cv, s = median(ridge_lambda)) %>% as.matrix()
ridge_coef = coef(ridge_cv, s = median(ridge_lambda)) %>% as.matrix()

str(ridge_coef)
summary(ridge_coef)

# Min.   :-9.276814  
# 1st Qu.:-0.002463  
# Median : 0.000067  
# Mean   :-0.002190  
# 3rd Qu.: 0.002449  
# Max.   : 0.026787  

adapt_wt = 1/(abs(ridge_coef)) ^ 0.5
str(adapt_wt)
summary(adapt_wt)

# Min.   :   0.328  
# 1st Qu.:  14.486  
# Median :  20.161  
# Mean   :  28.523  
# 3rd Qu.:  30.412  
# Max.   :3362.385  

# adaptive lasso using glmnet and penalty factor
ada_lass = cv.glmnet(x = data.matrix(mytraining[,-1]), y = data.matrix(mytraining[,1]), alpha = 1, penalty.factor = adapt_wt, nfolds = 5, type.measure = "mse")
plot(ada_lass)
print(ada_lass)

pred = predict(ada_lass, newx = data.matrix(mytest[,-1]), s = 0.1344) %>% as.vector()
(mytest[,1] - pred) ^ 2 %>% mean()

ada_coef = coef(ada_lass, s = 0.1344) %>% as.matrix()
str(ada_coef)
summary(ada_coef)

# which covariates are selected in the final model
final_coef = ada_coef[ada_coef != 0,] %>% round(digits = 4) %>% data.frame()
names(final_coef) = c("Variables", "Est. Coefficients")
  xtable()

ada_coef[ada_coef != 0,] %>% length()

# simple extension can check which variables are selected by different procedures
# can compare this to elastic net as well
# and the lasso fit
