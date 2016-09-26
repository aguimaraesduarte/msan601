# Reset R session
rm(list=ls())
cat("\014")

# Question 1
library(alr4)
transaction <- Transact
transaction$a <- (transaction$t1 + transaction$t2)/2
transaction$d <- transaction$t1 - transaction$t2

lm1 <- lm(transaction$time ~ transaction$t1 + transaction$t2)
lm2 <- lm(transaction$time ~ transaction$a + transaction$d)
lm3 <- lm(transaction$time ~ transaction$t2 + transaction$d)
lm4 <- lm(transaction$time ~ transaction$t1 + transaction$t2+ transaction$a + transaction$d)

summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)

# Reset R session
rm(list=ls())
cat("\014")

# Question 2
UN <- alr4::UN11
lm1 <- lm(log(UN$fertility) ~ UN$pctUrban)
summary(lm1)

lm2 <- lm(log(UN$fertility) ~ log(UN$ppgdp) + UN$lifeExpF)
summary(lm2)
beta0 <- summary(lm2)$coefficients[1]
beta1 <- summary(lm2)$coefficients[2]
beta2 <- summary(lm2)$coefficients[3]
#log(fertility) = beta0 + beta1*log(ppgdp) + beta2*lifeExpF
#fertility = exp(beta0 + beta1*log(ppgdp) + beta2*lifeExpF)
#          = exp(beta0)*ppgdp^beta1 + lifeExpF^beta2
# fertility(1.25ppgdp) = exp(beta0)*(1.25*ppgdp)^beta1 + lifeExpF^beta2
#                      = 1.25^beta1 * exp(beta0)*ppgdp^beta1 + lifeExpF^beta2
#1-1.25^beta1 = 0.0144958 = 1.45\%

# Reset R session
rm(list=ls())
cat("\014")

# Question 3
cakes <- alr4::cakes
pairs(cakes)
cor(cakes[-1])
lm1 <- lm(data=cakes, Y ~ X2)
summary(lm1)

cakes$logY <- log(cakes$Y)
lm2 <- lm(data=cakes, logY ~ X2)
summary(lm2)
summary(lm(data=cakes, logY ~ X2 + X1))

cakes$block <- ifelse(cakes$block=='0', 0, 1)
cor(cakes[-5])
lm3 <- lm(data=cakes, logY~X2+block)
summary(lm3)

# Reset R session
rm(list=ls())
cat("\014")

# Question 4
fuel <- alr4::fuel2001
lm1 <- lm(FuelC ~ ., data = fuel)
summary(lm1)
(anova1 <- anova(lm1))
F_stat <- (sum(anova1$"Sum Sq"[1:6])/sum(anova1$Df[1:6])) / (tail(anova1$"Sum Sq", 1)/tail(anova1$Df, 1))
F_lim <- qf(p = 0.975, df1 = 6, df2 = 44)

# Reset R session
rm(list=ls())
cat("\014")

# Question 5
cakes <- alr4::cakes
lm1 <- lm(data=cakes, Y ~ X1 + I(X1^2) + X2 + I(X2^2) + X1*X2)
summary(lm1)
n <- nrow(cakes)
pf <- 6
dff <- n-pf
SSEf <- tail(car::Anova(lm1)$"Sum Sq", 1)
MSEf <- SSEf / dff

# a
MSR_b5 <- car::Anova(lm1)$"Sum Sq"[5] / 1
Fstar_b5 <- MSR_b5 / MSEf
Flim_b5 <- qf(p=0.975, df1=1 , df2=dff)

# b
MSR_b2 <- car::Anova(lm1)$"Sum Sq"[2] / 1
Fstar_b2 <- MSR_b2 / MSEf
Flim_b2 <- qf(p=0.975, df1=1 , df2=dff)

# c
lm2 <- lm(data=cakes, Y ~ X2 + I(X2^2))
summary(lm2)
pr <- 3
dfr <- n-pr
SSEr <- tail(car::Anova(lm2)$"Sum Sq", 1)
Fstar_b1b2b5 <- ((SSEr - SSEf)/(dfr-dff)) / (SSEf/dff)
Flim_b1b2b5 <- qf(p=0.975, df1=3 , df2=dff)

# Reset R session
rm(list=ls())
cat("\014")

# Question 6
landrent <- alr4::landrent
pairs(landrent)
cor(landrent)
lm1 <- lm(data=landrent, Y ~ X1 + X2)
summary(lm1)

#tests for lm1
# homoskedasticity
residualPlot(lm1)
lmtest::bptest(lm1)
# create factor variable: 0 if X<=44.56; 1 if X>44.56
landrent$Group <- factor(apply(landrent, 1, function(x){
  if(x[1]<=44.56){return(0)} else{return(1)}}))
# Levene test from car package
leveneTest(data=landrent, Y ~ Group, center=median)

# independence
plot(lm1$residuals, xlab="Sequence", ylab="Residuals",
     main="Sequence Plot")
# outliers
par(mfrow=c(1,2))
plot(MASS::stdres(lm1), xlab="Sequence", ylab="Standardized Residual",
     main="Standardized Residual Plot")
abline(h=0)
plot(MASS::studres(lm1), xlab="Sequence", ylab="Studentized Residual",
     main="Studentized Residual Plot")
abline(h=0)

# normality
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
residplot(lm1)
shapiro.test(lm1$residuals)
qqPlot(residuals(lm1), main="landrent: Normal QQ-Plot")

# other possible models
lm3 <- lm(data=landrent, Y ~ X1 + X2 + X3)
summary(lm3)
lm4 <- lm(data=landrent, Y ~ X1 + X2 + X4)
summary(lm4)

# Reset R session
rm(list=ls())
cat("\014")

# Question 7
rateprof <- alr4::Rateprof
rateprof <- as.data.frame(rateprof[!sapply(rateprof, is.factor)]) #remove factor columns

backstep_byPvalue <- function(df, alpha=0.05, yColIndex){
  # get desired order of columns where the first one is Y
  getOrder <- function(numCol, yColIndex){
    if(yColIndex < numCol){
      return(c(yColIndex, 1:(yColIndex-1), (yColIndex+1):numCol))
    } else{
      return(c(yColIndex, 1:(yColIndex-1)))
    }
  }
  
  # rearrange the data frame to have Y in the first column
  df <- df[, getOrder(ncol(df), yColIndex)]
  
  # get the index of the max p-value from a summary if < alpha
  # else return -1
  getIndexOfMaxPvalue <- function(summary_object){
    maxPvalue <- max(summary_object$coefficients[-1,4]) # -1: we don't care about the intercept
    if(maxPvalue > alpha){
      return(which.max(summary_object$coefficients[-1,4])+1) # +1: to compensate for not counting the intercept
    } else{
      return(-1)
    }
  }
  
  lm_full <- lm(data=df, quality~.)
  summary_full <- summary(lm_full)
  
  colToRemove <- getIndexOfMaxPvalue(summary_full)
  
  while(TRUE){
    df <- df[-colToRemove] # subset the data frame
    lm_red <- lm(data=df, quality~.) # get new lm object
    summary_red <- summary(lm_red)
    colToRemove <- getIndexOfMaxPvalue(summary_red)
    if(colToRemove == -1){ # -1: all variables are significant
      break
    }
  }
  return(df) # return the reduced optimal data frame
}

backstep_byAdjR2 <- function(df, yColIndex){
  # get desired order of columns where the first one is Y
  getOrder <- function(numCol, yColIndex){
    return(c(yColIndex, 1:(yColIndex-1), (yColIndex+1):numCol))
  }
  
  # rearrange the data frame to have Y in the first column
  df <- df[, getOrder(ncol(df), yColIndex)]
  
  # get the index of the max p-value from a summary
  # else return -1
  getIndexOfMaxPvalue <- function(summary_object){
    maxPvalue <- max(summary_object$coefficients[-1,4]) # -1: we don't care about the intercept
    return(which.max(summary_object$coefficients[-1,4])+1) # +1: to compensate for not counting the intercept
  }
  
  # get the adjusted R square from a summary
  getAdjR2 <- function(summary_object){
    return(summary_object$"adj.r.squared")
  }
  
  lm_full <- lm(data=df, quality~.)
  summary_full <- summary(lm_full)
  adjR2_full <- getAdjR2(summary_full)
  
  colToRemove <- getIndexOfMaxPvalue(summary_full)
  
  while(TRUE){
    df_red <- df[-colToRemove] # subset the data frame
    lm_red <- lm(data=df_red, quality~.) # get new lm object
    summary_red <- summary(lm_red)
    adjR2_red <- getAdjR2(summary_red)
    if(adjR2_red < adjR2_full){ # if R squared goes back down, then keep df (before reducing)
      break
    }
    colToRemove <- getIndexOfMaxPvalue(summary_red) # else reduce df
    df <- df_red # update df
    #lm_full <- lm_red
    adjR2_full <- adjR2_red # update adjR2 
  }
  return(df) # return the reduced optimal data frame
}

rateprof_p <- backstep_byPvalue(rateprof, 0.05, 4)
rateprof_a <- backstep_byAdjR2(rateprof, 4)
summary(rateprof_p)
summary(rateprof_a)
summary(lm(data=rateprof_p, quality~.))$adj.r.squared
summary(lm(data=rateprof_a, quality~.))$adj.r.squared

# Question 10
non_normal <- replicate(5000, shapiro.test(runif(10))$p.value)
sum(non_normal>0.05)/length(non_normal) * 100 # percent of times non-normal data is considered normal
normal <- replicate(5000, shapiro.test(rnorm(5000))$p.value)
sum(normal<0.05)/length(normal) * 100 # percent of times normal data is considered non-normal
