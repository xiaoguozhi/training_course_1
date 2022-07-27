####################################################################
# 说明:
# 1.培训：R语言与数据科学系列
# 2.第二部分 统计模型
# 3.目标：掌握R做统计模型
# 4.作者: 郭志峰
# 5.日期: 2018.2.6
###################################################################

#设置工作路径
setwd("D:/Rdata")

#lasso包
library(lars)   
#ridge包       
library(ridge)  

#模拟数据

set.seed(12345)
dat <- data.frame(x1=rnorm(200), x2=rnorm(200),x3=rnorm(200), x4=rnorm(200),x5=rnorm(200))
e <- rnorm(100)
dat$Y <- 3*dat$x1 + 5*dat$x2 +4*dat$x3+ e

head(dat)

#进行线性回归
model.ols <- lm(Y~.-1, data=dat)
summary(model.ols)
coef.ols <- coef(model.ols) #提取系数
coef.ols[coef.ols!=0]       #产看不等于0的系数                



#进行岭回归
model.rid <- linearRidge(Y~ x1 + x2 + x3 + x4, data=dat)
summary(model.rid)
coef.rid <- coef(model.rid)                      #提取系数
coef.rid[coef.rid!=0]                            #查看不等于0的系数

#进行lasso回归
X<-as.matrix(dat[,1:5])
Y<-dat$Y
model.lasso <- lars( X, Y, type='lasso')          # 进行回归
plot(model.lasso)                                 # 画图
summary(model.lasso)
set.seed(12345)
CV.lasso <- cv.lars(X, Y, K=10)                  # 交叉验证
(best <- CV.lasso$index[which.min(CV.lasso$cv)])   # 选择最好的结果
(coef.lasso <- coef.lars(model.lasso, mode='fraction', s=best))
names(coef.lasso) <- colnames(dat)[-1]
coef.lasso[coef.lasso!=0]   # 查看不等于0的系数

#构造共线性的数据

x1=rnorm(200,mean = 10,sd = 2)
x2=rnorm(200,mean = 40,sd = 5)
x3=2*x1
x4=rnorm(200,mean = 15,sd = 1)
x5=rnorm(200,mean = 11,sd = 4)
dat <- data.frame(x1,x2,x3,x4,x5)
e <- rnorm(100)
dat$Y <- 3*dat$x1 + 5*dat$x2 +4*dat$x3+ e

# 线性回归
model.ols <- lm(Y~.-1, data=dat)
summary(model.ols)
coef.ols <- coef(model.ols)
coef.ols[coef.ols!=0]                          

# 岭回归
model.rid <- linearRidge(Y~ x1 + x2 + x3 + x4, data=dat)
summary(model.rid)
coef.rid <- coef(model.rid)
coef.rid[coef.rid!=0]                          

#lasso回归
X<-as.matrix(dat[,1:5])
Y<-dat$Y
model.lasso <- lars( X, Y, type='lasso')          
plot(model.lasso)                                
summary(model.lasso)
set.seed(12345)
CV.lasso <- cv.lars(X, Y, K=10)                  
(best <- CV.lasso$index[which.min(CV.lasso$cv)])   
(coef.lasso <- coef.lars(model.lasso, mode='fraction', s=best))
names(coef.lasso) <- colnames(dat)[-1]
coef.lasso[coef.lasso!=0]  

######################################################################
#构造小n大P型数据
dat<-matrix(data=NA,nrow = 300,ncol = 500)
for (i in 1:500) {
  
  dat[,i]<-rnorm(300,mean = 30,sd=4)
}

dat<-as.data.frame(dat)
names(dat)<-paste('x',1:500,sep ='')

dat$y<-0.5*dat$x1+0.05*dat$x2+0.005*dat$x3+0.0005*dat$x4+5*dat$x5+50*dat$x6+500*dat$x7+0.1*dat$x8+0.01*dat$x9+0.001*dat$x10+0.0001*dat$x11+1*dat$x12+10*dat$x13+100*dat$x14+100*dat$x15
#0.0005-500
#0.0001-100

# 线性回归
model.ols <- lm(y~.-1, data=dat)
summary(model.ols)
coef.ols <- coef(model.ols)
coef.ols[coef.ols!=0]                          
stem(model.ols)


# 岭回归
model.rid <- linearRidge(y~ ., data=dat)
summary(model.rid)
coef.rid <- coef(model.rid)
coef.rid[coef.rid!=0]                          
coef.rid[coef.rid>0.0005]  

# lasso回归
X<-as.matrix(dat[,1:500])
Y<-dat$y
model.lasso <- lars( X, Y, type='lasso')         
plot(model.lasso)                               
summary(model.lasso)
set.seed(12345)
CV.lasso <- cv.lars(X, Y, K=10)                  
(best <- CV.lasso$index[which.min(CV.lasso$cv)])  
(coef.lasso <- coef.lars(model.lasso, mode='fraction', s=best))
names(coef.lasso) <- colnames(dat)[-501]
coef.lasso[coef.lasso!=0]  

coef.lasso[coef.lasso>0.0001]


#######################################################################################
#构造逐步回归的数据
dat<-matrix(data=NA,nrow = 300,ncol = 50)
for (i in 1:50) {
  
  dat[,i]<-rnorm(300,mean = 30,sd=4)
}

dat<-as.data.frame(dat)
names(dat)<-paste('x',1:50,sep ='')

dat$y<-0.5*dat$x1+0.05*dat$x2+0.005*dat$x3+0.0005*dat$x4+5*dat$x5+50*dat$x6+500*dat$x7+0.1*dat$x8+0.01*dat$x9+0.001*dat$x10
# step forward AIC

m1 <- lm(y ~ ., data = dat)
m0<- lm(y ~ 1, data = dat)


summary(m1)
scope_m1 <- formula(lm(y~.,dat))
step_re_1 <- step(m0,direction="forward", scope=scope_m1,data=dat)

step_re_2 <- step(m1,direction="backward", data=dat)



####################################################################################
#广义线性回归模型
#画logit函数
logistic <- function(x) { 1/(1 + exp(-x)) }
x <- seq(-6, 6, length=100)
#
plot(x, logistic(x), type="l",  xlab=expression(paste(beta, "'", bold(x))), ylab="Probability") 



#进行广义线性回归模型
rm(list = ls())
dat<-read.csv("logistic.csv")
m1 <- glm(y ~ ln_D, family=binomial(), data=dat)
summary(m1)
# Table 12.2
m2 <- glm(y ~ ln_D + S, family=binomial(), data=dat)
m3 <- update(m2,  ~ . + ln_D:S)
summary(m2)
summary(m3)

#画出概率密度预测图
plot(dat$ln_D,dat$y,xlab = "自变量",ylab="因变量",main="概率预测")
x <- with(dat, seq(min(ln_D), max(ln_D), length=100))
pred<-data.frame(ln_D=x)
prob<-predict(m1, data.frame(ln_D=x), type="response")
pred$prob<-prob
lines(pred$ln_D, pred$prob, lty=2,col="red")


