library(splines)

# splines
fit = lm(wage~bs(age, knots(25, 40, 6)), data=Wage)
pred = predict(fit, newdata=list(age=age.grid), se=T)
plot(age, wage, col="gray")
lines(age.grid,pred$fit, lwd=2)
lines(age.grid,pred$fit-2*pred$se, lty="dashed")
lines(age.grid,pred$fit+2*pred$se, lty="dashed")

# Natural splines
fit2 = lm(wage~ns(age, df=4), data=Wage)
pred2 = predict(fit2, newdata=list(age=age.grid), se=T)
plot(age, wage, col="gray")
lines(age.grid,pred2$fit, lwd=2)
lines(age.grid,pred2$fit-2*pred2$se, lty="dashed")
lines(age.grid,pred2$fit+2*pred2$se, lty="dashed")

# Smoothing splines
plot(age, wage, xlim=agelimes, cex=.5, col="darkgrey")
title("Smoothing spline")
fit = smooth.spline(age, wage, df=16)
fit2 = smooth.spline(age, wage, cv=TRUE)
fit2$df
lines(fit, col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF", "6.8 DF"), col=c("red", "blue"), lty=1, lwd=2, cex=.8)

# Local regression
plot(age, wage, xlim=agelimes, cex=.5, col="darkgrey")
title("Local regression")
fit = loess(wage~age, span=.2, data=Wage)
fit2 = loess(wage~age, span=.5, data=Wage)
lines(age.grid, predict(fit, data.frame(age=age.grid)), col="red", lwd=2)
lines(age.grid, predict(fit2, data.frame(age=age.grid)), col="blue", lwd=2)
legend("topright", legend=c("span .2", "span .6"), col=c("red", "blue"), lty=1, lwd=2, cex=.8)

# GAM
gam1 = lm(wage~ns(year, 4) + ns(age, 5)+education, data=Wage)
library(gam)
gam.m3 = gam(wage~s(year,4)+s(age,5) + education, data=Wage)
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE, col="blue")
gam.m1 = gam(wage~s(age,5) + education, data=Wage)
gam.m2 = gam(wage~year+s(age,5) + education, data=Wage)
anova(gam.m1, gam.m2, gam.m3, test="F")
preds = predict(gam.m2, newdata=Wage)
gam.lo = gam(wage~s(year,df=4)+lo(age, span=0.7)+education, data=Wage)
plot.Gam(gam.lo, se=T, col="green")
gam.lo.i = gam(wage~lo(year,age, span=0.5)+education, data=Wage)
plot(gam.lo.i)

gam.lr = gam(I(wage>250) ~year+s(age,df=5)+education, data=Wage, family=binomial)
par(mfrow=c(1,3))
plot(gam.lr, se=T, col="green")
table(Wage$education, I(Wage$wage>250))
gam.lr.s = gam(I(wage>250) ~year+s(age,df=5)+education, data=Wage, family=binomial, subset=(education!="1. < HS Grad"))
plot(gam.lr.s, se=T, col="green")