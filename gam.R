# HINT: 
# The program assumes that all files are stored in 
# C:\monographregression\computercode
# change directory if code is located elsewhere
# to store the figures a subfolder `figures' has to be
# created

setwd("D:/data")

library("foreign")
library("quantreg")
library("gamlss")

# read data
rents <- read.dta("rent99.dta")

# estimate model for location and scale with linear effects
gamlss.rent <- gamlss(formula=rent~area+yearc, sigma.formula=~area+yearc, data=rents, family=NO())

# summarize results
summary(gamlss.rent)

# estimate model for location and scale with additive effects
# preliminary model for smoothing parameter choice
# see Lancaster-Booklet.pdf
gamlss.rent.add <- quote(gamlss(formula=rent~pb(area,inter=10,degree=3,order=2,df=p[1])+
                   pb(yearc,inter=10,degree=3,order=2,df=p[2]), 
                   sigma.formula=~pb(area,inter=10,degree=3,order=2,df=p[3])+
                   pb(yearc,inter=10,degree=3,order=2,df=p[4]) 
                   , data=rents, family=NO(), trace=FALSE))

# obtain optimal smoothing parameters
op.rent <- find.hyper(model=gamlss.rent.add, par=c(3,3,3,3),lower=c(1,1,1,1),upper=c(6,6,6,6),
                      steps=c(0.2,0.2,0.2,0.2),pen=2,trace=FALSE)

# final additive model
gamlss.rent.add.final <- gamlss(formula=rent~pb(area,inter=10,degree=3,order=2,df=op.rent$par[1])+
                   pb(yearc,inter=10,degree=3,order=2,df=op.rent$par[2]), 
                   sigma.formula=~pb(area,inter=10,degree=3,order=2,df=op.rent$par[3])+
                   pb(yearc,inter=10,degree=3,order=2,df=op.rent$par[4]) 
                   , data=rents, family=NO(), trace=FALSE)


# plot estimated functions
term.plot(gamlss.rent.add.final,what=c("mu"),se=TRUE)

term.plot(gamlss.rent.add.final,what=c("sigma"),se=TRUE)

