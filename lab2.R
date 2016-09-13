library(car)
library(MASS)

scatterplot(infant.mortality~gdp, data=UN, 
            main="Scatterplot of UN Data", xlab="GDP", ylab="Infant Mortality Rate per 1,000 Births",
            boxplot=FALSE)
# non-linearity issue (may be the cause of heteroskedasticity) => transform the X

scatterplot(infant.mortality~log(gdp), data=UN, 
            main="Scatterplot of UN Data", xlab="log(GDP)", ylab="log(Infant Mortality Rate per 1,000 Births)",
            boxplot=FALSE)
# heteroskedasticity => transform the Y

scatterplot(log(infant.mortality)~log(gdp), data=UN, 
            main="Scatterplot of UN Data", xlab="log(GDP)", ylab="log(Infant Mortality Rate per 1,000 Births)",
            boxplot=FALSE, id.n=4)
# looks reasonably good

lm_UN <- lm(log(infant.mortality)~log(gdp), data=UN)
summary(lm_UN)

#plot(lm_UN)

#homoskedasticity
residualPlot(lm_UN, id.n=4)

#independence
plot(lm_UN$residuals, xlab="Sequence", ylab="Residuals",
     main="Sequence Plot")

#outliers
par(mfrow=c(1,2))
plot(stdres(lm_UN), xlab="log(GDP)", ylab="Standardized Residuals",
     main="Standardized Residual Plot")
abline(h=0)
plot(studres(lm_UN), xlab="log(GDP)", ylab="Studentized Residuals",
     main="Studentized Residual Plot")
abline(h=0)

#normality
par(mfrow=c(1,1))
residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}
residplot(lm_UN)
shapiro.test(lm_UN$residuals)
qqPlot(residuals(lm_UN), main="UN: Normal QQ-Plot", xlab="log(GDP)",
       ylab="log(Infant Mortality Rate per 1,000 Births)", id.n = 4)

#boxcox(lm(UN$infant.mortality~UN$gdp))












