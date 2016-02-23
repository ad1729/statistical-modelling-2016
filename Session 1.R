## Chapter 1
library(car)
library(flexmix)

model.check = function(fit){
  print(summary(fit))
  par(mfrow = c(2,2))
  plot(fit)
  par(mfrow = c(1,1))
}


##################################
##  Question 6
##################################

slid = read.table("/home/ad/Desktop/KUL Course Material/Statistical Modelling/Datasets/SLID.txt", header = T)
str(slid)
dim(slid)
some(slid)

plot(wages ~ education, data = slid)
plot(log(wages) ~ education, data = slid)

qqnorm(slid$wages)
qqnorm(log(slid$wages))

fit1 = glm(log(wages) ~ ., data = slid)
model.check(fit1)

fit2 = glm(wages ~ ., data = slid, family = Gamma(link = "log"))
model.check(fit2) # prefer this model since the response variable is positive

##################################
##  Question 7
##################################

data("bioChemists")
str(bioChemists)
some(bioChemists)
hist(bioChemists$art) # start with poisson with log link
summary(bioChemists$art)

fit3 = glm(art ~ . , data = bioChemists, family = poisson(link = "log"))
model.check(fit3)
mean(bioChemists$art)
var(bioChemists$art)


##################################
##  Question 8
##################################

