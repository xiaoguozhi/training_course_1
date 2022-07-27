
# do factor analysis and do comprehensive evaluation
#########################################################
#设置工作路径
setwd('D:/data')

#清除工作空间
rm(list = ls())                   

# 导入相关的包
library(xlsx)          # 读取xlsx数据
library(nFactors)      # 选择最优因子数
library(readxl)
library(parallel)
library(psy)
# 读取数据
dat <- read_excel('MicEcoData.xls')

#查看数据前五行
head(dat)
# 去除第一列
dat.fact<-dat[,-1]
# 重新命名
names(dat.fact) <- paste('x', 1:ncol(dat.fact), sep='')

# 2. 因子分析
# (1) 决定最优因子
psy::scree.plot(dat.fact)  
ev <- eigen(cor(dat.fact)) # 得到特征值
# (2) 估计，
factor.result <- factanal(x=dat.fact, factor=2, scores="regression")
# scree plot
names(factor.result)
print(factor.result)

# 3.画图
load <- factor.result$loadings[,1:2] 
plot(load, type="n",xlab='因子1',ylab='因子2')                # set up plot 
text(load, labels=names(dat.fact), cex=.7)                   # add variable names

# 4. do comprehensive evaluation
# (1) 计算权重
lambdas <- eigen(factor.result$correlation)$value            # variance of factors
(w <- lambdas[1:2]/sum(lambdas[1:2]))

# (2) evaluate
score <- factor.result$scores                                # get factor scores 
eva <- score %*% w                                           # evluate
eva.result <- data.frame(firm=dat[,1], eva=eva)  
eva.result[order(eva.result$eva, decreasing=TRUE),]          # sort results


