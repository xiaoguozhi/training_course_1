
setwd("D:/data")

# load data set
library("foreign")
rents <- read.dta("rent99.dta")

# load required packages
library("quantreg")
library("gamlss")

par(mfrow=c(2,3))

# order data and construct a data frame for plotting
rents <- rents[order(rents$area),]
areaquant <- data.frame(area=rents$area, rent=rents$rent)

# estimate linear quantile regressions and extract relevant results
tauseq <- c(.01,seq(.1,.9,by=.1),.99)
rq.area <- rq(rent~area, data=rents, tau=tauseq)
with(rents, plot(area, rent), xlab="living area", ylab="net rent")
fits <- fitted(rq.area)
for(i in 1:11)
  {
  areaquant$fit <- fits[,i]
  names(areaquant)[[2+i]] <- paste("fit",i,sep="")
  with(rents,lines(area, fits[,i]))
  }

# plots
with(rents, plot(area, rent), xlab="living area", ylab="net rent")
lm.area <- lm(rent~area, data=rents)
for(i in 1:11)
  {
  areaquant$lmfit <- fitted(lm.area) + qnorm(tauseq[i])*summary(lm.area)$sigma
  with(rents,lines(area, areaquant$lmfit))
  names(areaquant)[[13+i]] <- paste("lmfit",i,sep="")
  }


with(rents, plot(area, rent), xlab="living area", ylab="net rent")
gamlss.area <- gamlss(formula=rent~area, sigma.formula=~area, data=rents,
                      family=NO())
for(i in 1:11)
  {
  areaquant$gamlssfit <- fitted(gamlss.area, what="mu") +
                         qnorm(tauseq[i])*fitted(gamlss.area, what="sigma")
  with(rents,lines(area, areaquant$gamlssfit))
  names(areaquant)[[24+i]] <- paste("gamlssfit",i,sep="")
  }

# now do the same with year of construction
rents <- rents[order(rents$yearc),]
yearquant <- data.frame(area=rents$yearc, rent=rents$rent)

rq.year <- rq(rent~poly(yearc,2), data=rents, tau=tauseq)
with(rents, plot(yearc, rent), xlab="year of construction", ylab="net rent")
fits <- fitted(rq.year)
for(i in 1:11)
  {
  with(rents,lines(yearc, fits[,i]))
  yearquant$fit <- fits[,i]
  names(yearquant)[[2+i]] <- paste("fit",i,sep="")
  }

with(rents, plot(yearc, rent), xlab="year of construction", ylab="net rent")
lm.year <- lm(rent~poly(yearc,2), data=rents)
for(i in 1:11)
  {
  yearquant$lmfit <- fitted(lm.year) + qnorm(tauseq[i])*summary(lm.year)$sigma
  with(rents,lines(yearc, yearquant$lmfit))
  names(yearquant)[[13+i]] <- paste("lmfit",i,sep="")
  }

with(rents, plot(yearc, rent), xlab="year of construction", ylab="net rent")
gamlss.year <- gamlss(formula=rent~poly(yearc,2), sigma.formula=~poly(yearc,2), data=rents,
                      family=NO())
for(i in 1:11)
  {
  yearquant$gamlssfit <- fitted(gamlss.year, what="mu") +
                         qnorm(tauseq[i])*fitted(gamlss.year, what="sigma")
  with(rents,lines(yearc, yearquant$gamlssfit))
  names(yearquant)[[24+i]] <- paste("gamlssfit",i,sep="")
  }


# multivariate quantile regression with both covariates
rq.mult <- rq(rent~area + poly(yearc,2), data=rents, tau=tauseq)
plot(rq.mult)
cf <- t(coef(rq.mult)[2:4,])
sd <- matrix(0,nrow=11,ncol=6)
for(i in 1:11)
  {
  sd[i,1] <- summary(rq.mult)[[i]]$coefficients[2,1] + qnorm(0.975)*sqrt(summary(rq.mult)[[i]]$coefficients[2,2])
  sd[i,2] <- summary(rq.mult)[[i]]$coefficients[3,1] + qnorm(0.975)*sqrt(summary(rq.mult)[[i]]$coefficients[3,2])
  sd[i,3] <- summary(rq.mult)[[i]]$coefficients[4,1] + qnorm(0.975)*sqrt(summary(rq.mult)[[i]]$coefficients[4,2])
  sd[i,4] <- summary(rq.mult)[[i]]$coefficients[2,1] - qnorm(0.975)*sqrt(summary(rq.mult)[[i]]$coefficients[2,2])
  sd[i,5] <- summary(rq.mult)[[i]]$coefficients[3,1] - qnorm(0.975)*sqrt(summary(rq.mult)[[i]]$coefficients[3,2])
  sd[i,6] <- summary(rq.mult)[[i]]$coefficients[4,1] - qnorm(0.975)*sqrt(summary(rq.mult)[[i]]$coefficients[4,2])
  }
ols <- lm(rent ~ area + poly(yearc, 2), data=rents)

par(mfrow=c(1,3))
plot(tauseq, cf[,1], type="b", ylim=c(0,15), xlab="tau", ylab="", main="living area", pch=20)
abline(h=coef(ols)[2], lty=2, col=gray(0.8))
lines(tauseq, sd[,1], col=gray(0.8))
lines(tauseq, sd[,4], col=gray(0.8))

plot(tauseq, cf[,2], type="b", xlab="tau", ylab="", main="year of constructtion (linear)", pch=20)
abline(h=coef(ols)[3], lty=2, col=gray(0.8))
lines(tauseq, sd[,2], col=gray(0.8))
lines(tauseq, sd[,5], col=gray(0.8))

plot(tauseq, cf[,3], type="b", xlab="tau", ylab="", main="year of constructtion (quadratic)", pch=20)
abline(h=coef(ols)[4], lty=2, col=gray(0.8))
lines(tauseq, sd[,3], col=gray(0.8))
lines(tauseq, sd[,6], col=gray(0.8))

taudata <- data.frame(tauseq, cf, sd, coef(ols)[2], coef(ols)[3], coef(ols)[4])
taudata <- round(taudata, 2)
names(taudata) <- c("tau", "area", "year1", "year2", "arealo", "year1lo", "year2lo",
                    "areaup", "year1up", "year2up", "arealm", "year1lm", "year2lm")
write.table(taudata, "figures/quantreg_taudata.raw", col.names=TRUE, row.names=FALSE, sep=" ", quote=FALSE)


yearquant <- data.frame(year=rents$yearc, rent=rents$rent)
res <- residuals(rq.mult)
par(mfrow=c(3,4))
py <- poly(rents$yearc,2)
for(i in 1:11)
  {
  parfit <- py[,1]*cf[i,2] + py[,2]*cf[i,3]
  plot(rents$yearc, res[,i] + parfit)
  lines(rents$yearc, parfit)
  yearquant$resid <- res[,i] + parfit
  yearquant$fit <- parfit
  names(yearquant)[(2+(i-1)*2+1):(2+(i-1)*2+2)] <- c(paste("resid",i,sep=""), paste("fit",i,sep=""))
  }

