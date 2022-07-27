########################################################
# Description:
# 1.for the course "regression"
# 2.Section: 1
# 3.Author: Zhifeng Guo
# 4.Date: 8 11, 2017.
########################################################
#非参数回归部分 Splines, Smoothers, and Kernels
#loading the Splines Packages
require(splines)
#ISLR contains the Dataset
require(ISLR)
attach(Wage)

agelims<-range(age)
#Generating Test Data
age.grid<-seq(from=agelims[1], to = agelims[2])

#3 cutpoints at ages 25 ,50 ,60
fit<-lm(wage ~ bs(age,knots = c(25,40,60)),data = Wage )
summary(fit)
#Plotting the Regression Line to the scatterplot   
plot(age,wage,col="grey",xlab="Age",ylab="Wages")
points(age.grid,predict(fit,newdata = list(age=age.grid)),col="darkgreen",lwd=2,type="l")
#adding cutpoints
abline(v=c(25,40,60),lty=2,col="darkgreen")



fit1<-smooth.spline(age,wage,df=16)
plot(age,wage,col="grey",xlab="Age",ylab="Wages")
points(age.grid,predict(fit,newdata = list(age=age.grid)),col="darkgreen",lwd=2,type="l")
#adding cutpoints
abline(v=c(25,40,60),lty=2,col="darkgreen")
lines(fit1,col="red",lwd=2)
legend("topright",c("Smoothing Spline with 16 df","Cubic Spline"),col=c("red","darkgreen"),lwd=2)



#Cross Validation to select lambda(Tuning Parameter) for a Model which Minimizes CV Error.

fit2<-smooth.spline(age,wage,cv = TRUE)
fit2

plot(age,wage,col="grey")
#Plotting Regression Line
lines(fit2,lwd=2,col="purple")
legend("topright",("Smoothing Splines with 6.78 df selected by CV"),col="purple",lwd=2)



#####################################################################################################
#多元线性回归
# LINEAR REGRESSION STATSLEARN

library(MASS)

#install.packages('ISLR')
library(ISLR)
#package for Datasets

library(ggplot2)

names(Boston)


#Plotting Data first -finding relations b/w the variables in the Dataset and analysing those variables

plot1<-ggplot(aes(x = lstat, y = medv),data = Boston) + 
  geom_point() + 
  geom_smooth(method = 'lm')
#inverse relation b/w the variables-as the lower status population % increases the Median sallaries decreases


# LINEAR MODEL1
mod1<-lm(medv ~ lstat , data = Boston)

summary(mod1)
#Significant p-values and t-values showing a negetive relation b/w X and Y
plot(lstat ~ medv ,data = Boston)
abline(mod1,col='red' ) #fitting the model to the Plot


#Model components such as residuals , fitted Y values etc 
names(mod1)
summary(resid(mod1))
#RESIDUALS SHOULD ALWAYS BE NORMALLY DISTRIBUTED I.E BELL SHAPED
hist(resid(mod1))
#USING GGPLOT2 syntax
ggplot(aes(x = residuals(mod1)),data = mod1)+ 
  geom_histogram(binwidth=5)


#confidence intervals for each regression coefficients
confint(mod1)

#Predictions and Genrelizations
predict(mod1 , data.frame(lstat = c(5,30,10)))
BIC(mod1)

par(mfrow=c(2,2))
#plotting the Linear model
plot(mod1)


#Multiple Regression
mod2<-lm(medv ~ ., data =Boston) 
#including all the sttr as predictors
#Backward Model Selection technique


mod2
summary(mod2)
#Age ,indus variable not significant when all variables included which says tha
# there is correlations of these variables with other variables

AIC(mod2,mod1)

#Updatiing the Model- and removing irrelevetn features(inputs)
mod3<-update(mod2, ~. - age - indus)
summary(mod3)

#interaction between variables
mod4<-lm(medv ~ lstat*age,data = Boston)
summary(mod4)

#non-linear Models
mod5<-lm(medv ~ lstat + I(lstat^2),data = Boston); summary(mod5)

#Plotting The non-linear Models
attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
#plotting the regression line on the scatterplot-cannot use abline now
points(lstat , fitted(mod5), col='blue', pch=20)

#Another method of polynomial regression using poly() function
mod6<-lm(medv~poly(lstat,5))
summary(mod6)
#This model is more complicated and flexible due to higher degree and has lesser training Error

points(lstat , fitted(mod6),col= 'red', pch=20)
######################################################################################



#############################################################################################
#panel regression
rm(list=ls())
library("plm")

data("EmplUK", package = "plm")
head(EmplUK,n=20)
data("Produc", package = "plm")
data("Grunfeld", package = "plm")
data("Wages", package = "plm")

grun.fe<-plm(inv ~ value + capital, data = Grunfeld, model = "within")
grun.re<-plm(inv ~ value + capital, data = Grunfeld, model = "random")
fixef(grun.fe)
summary(fixef(grun.fe))

grun.twfe <- plm(inv ~ value + capital, data = Grunfeld, model = "within",  effect = "twoways")
fixef(grun.twfe, effect = "time")

#############################################################################################

#quantile regression

rm(list=ls())

# (2) library used packages
library(quantreg)                         # for quantile regression

# 1. read Engel's data
data(engel)
mode(engel)
names(engel)
class(engel)
head(engel)

# 2. explore data
# (1) scatter plot
plot(engel$income, engel$foodexp, xlab='income', ylab='foodexp')

# (2) descriptive statistics

summary(engel)

# 3. show distribution
# (1) boxplot
boxplot(engel$foodexp, xlab='foodexp')

# (2) Q-Q plot
qqnorm(engel$foodexp, main='正态性检验')
qqline(engel$foodexp, col='red', lwd=2)


# 4. do linear quantile regression
# (1) estimate model
attach(engel)
income.cent <- income-mean(income)
rq.lm.1 <- rq(foodexp ~ income.cent, tau=2:98/100)
rq.lm.2 <- rq(foodexp ~ income, tau=c(0.05, 0.25, 0.5, 0.75, 0.95))
(sum.rq.lm.1 <- summary(rq.lm.1))
(sum.rq.lm.2 <- summary(rq.lm.2))

# (2) show estimation results
plot(sum.rq.lm.1)
plot(sum.rq.lm.2)
(coef.rq.lm.1 <- coef(rq.lm.1))
(coef.rq.lm.2 <- coef(rq.lm.2))

# (3) fit the data
plot(income, foodexp, cex=0.25, type='n', xlab='income', ylab='foodexp')
points(income, foodexp, cex=0.5, col='blue')
abline(rq(foodexp~income, tau=0.5), col='blue')
abline(lm(foodexp~income), lty=2, col='red')
taus <- c(0.05, 0.1, 0.25, 0.75, 0.9, 0.95)
for (i in 1:length(taus)){
  abline(rq(foodexp~income, tau=taus[i]), col='gray')
}

###################################################################################

#Generalized Additive Models
rm(list = ls())
library(mgcv) ## load the package
data(trees)
ct1 <- gam(Volume ~ s(Height) + s(Girth), family=Gamma(link=log),data=trees)
ct1
plot(ct1,residuals=TRUE)


