# Question 1
# 1
senic = read.csv("SENIC_data.csv", header = F)
names(senic) <- c(
  "IdNumber", "LengthOfStay", "Age", "InfectionRisk", "RoutineCulturingRatio", "RoutineChestXRayRatio",
  "NumberOfBeds", "MedSchoolAffiliation", "Regio", "AverageDailyCensus", "NumberOfNurses", "AvailableFacilitiesAndServices"
)
plot(data = senic, LengthOfStay ~ AverageDailyCensus)
lm_senic <- lm(data = senic, LengthOfStay ~ AverageDailyCensus)
abline(lm_senic)
summary_senic <- summary(lm_senic)
b0_senic <- lm_senic$coefficients[[1]]
b1_senic <- lm_senic$coefficients[[2]]
r2_senic <- summary_senic$r.squared

# 2
senic$LengthOfStay2 <- senic$LengthOfStay * 192
senic$AverageDailyCensus2 <- senic$AverageDailyCensus * 192
plot(data = senic, LengthOfStay2 ~ AverageDailyCensus2)
lm_senic2 <- lm(data = senic, LengthOfStay2 ~ AverageDailyCensus2)
abline(lm_senic2)
summary_senic2 <- summary(lm_senic2)
b0_senic2 <- lm_senic2$coefficients[[1]]
b1_senic2 <- lm_senic2$coefficients[[2]]
r2_senic2 <- summary_senic2$r.squared

# 3
senic$LengthOfStay3 <- senic$LengthOfStay * 47
plot(data = senic, LengthOfStay3 ~ AverageDailyCensus)
lm_senic3 <- lm(data = senic, LengthOfStay3 ~ AverageDailyCensus)
abline(lm_senic3)
summary_senic3 <- summary(lm_senic3)
b0_senic3 <- lm_senic3$coefficients[[1]]
b1_senic3 <- lm_senic3$coefficients[[2]]
r2_senic3 <- summary_senic3$r.squared

# 4
senic$AverageDailyCensus3 <- senic$AverageDailyCensus * 12
plot(data = senic, LengthOfStay ~ AverageDailyCensus3)
lm_senic4 <- lm(data = senic, LengthOfStay ~ AverageDailyCensus3)
abline(lm_senic4)
summary_senic4 <- summary(lm_senic4)
b0_senic4 <- lm_senic4$coefficients[[1]]
b1_senic4 <- lm_senic4$coefficients[[2]]
r2_senic4 <- summary_senic4$r.squared

# nice 2x2 plot
par(mfrow=c(2,2))
plot(data = senic, LengthOfStay ~ AverageDailyCensus,
     xlab = "Average Daily Census", ylab = "Length of Stay (days)", main = "Y ~ X",
     xaxt = "n", yaxt = "n")
axis(1, pretty(senic$AverageDailyCensus, 5), labels = prettyNum(pretty(senic$AverageDailyCensus, 5), big.mark = ","))
axis(2, pretty(senic$LengthOfStay, 5), labels = prettyNum(pretty(senic$LengthOfStay, 5), big.mark = ","))
abline(lm_senic)
plot(data = senic, LengthOfStay2 ~ AverageDailyCensus2,
     xlab = "(192*) Average Daily Census", ylab = "(192*) Length of Stay (days)", main = "192Y ~ 192X",
     xaxt = "n", yaxt = "n")
axis(1, pretty(senic$AverageDailyCensus2, 5), labels = prettyNum(pretty(senic$AverageDailyCensus2, 5), big.mark = ","))
axis(2, pretty(senic$LengthOfStay2, 5), labels = prettyNum(pretty(senic$LengthOfStay2, 5), big.mark = ","))
abline(lm_senic2)
plot(data = senic, LengthOfStay3 ~ AverageDailyCensus,
     xlab = "Average Daily Census", ylab = "(47*) Length of Stay (days)", main = "47Y ~ X",
     xaxt = "n", yaxt = "n")
axis(1, pretty(senic$AverageDailyCensus, 5), labels = prettyNum(pretty(senic$AverageDailyCensus, 5), big.mark = ","))
axis(2, pretty(senic$LengthOfStay3, 5), labels = prettyNum(pretty(senic$LengthOfStay3, 5), big.mark = ","))
abline(lm_senic3)
plot(data = senic, LengthOfStay ~ AverageDailyCensus3,
     xlab = "(12*) Average Daily Census", ylab = "Length of Stay (days)", main = "Y ~ 12X",
     xaxt = "n", yaxt = "n")
axis(1, pretty(senic$AverageDailyCensus3, 5), labels = prettyNum(pretty(senic$AverageDailyCensus3, 5), big.mark = ","))
axis(2, pretty(senic$LengthOfStay, 5), labels = prettyNum(pretty(senic$LengthOfStay, 5), big.mark = ","))
abline(lm_senic4)
par(mfrow=c(1,1))

# Question 2
library(car)
library(MASS)
# 1
plasticHardness <- read.table("plasticHardness.txt", header = F, sep = "")
names(plasticHardness) <- c("PlasticHardness", "TimeElapsed")

#scatterplot(PlasticHardness ~ TimeElapsed, data=plasticHardness, 
#            main="Scatterplot of plasticHardness Data", xlab="Time Elapsed (Hours)",
#            ylab="Plastic Hardness (Brinell Unites)",
#            boxplot=FALSE)

# 2
plot(data=plasticHardness, PlasticHardness ~ TimeElapsed)
lm_plastic <- lm(data=plasticHardness, PlasticHardness ~ TimeElapsed)
abline(lm_plastic)
summary_plastic <- summary(lm_plastic)

#homoskedasticity
residualPlot(lm_plastic)

#independence
plot(lm_plastic$residuals, xlab="Sequence", ylab="Residuals",
     main="Sequence Plot")

#outliers
par(mfrow=c(1,2))
plot(stdres(lm_plastic), xlab="Time Elapsed (Hours)", ylab="Standardized Residuals",
     main="Standardized Residual Plot")
abline(h=0)
plot(studres(lm_plastic), xlab="Time Elapsed (Hours)", ylab="Studentized Residuals",
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
residplot(lm_plastic)
shapiro.test(lm_plastic$residuals)
qqPlot(residuals(lm_plastic), main="PlasticHardness: Normal QQ-Plot",
       xlab="Time Elapsed (Hours)", ylab="Plastic Hardness (Brinell Units)")

# 3
# create factor variable: 0 if X<=24; 1 if X>24
plasticHardness$Group <- factor(apply(plasticHardness, 1, function(x){
  if(x[2]<=24){return(0)} else{return(1)}}))
# Levene test from car package
leveneTest(data=plasticHardness, PlasticHardness ~ Group, center=median)

# Question 4
# 1
muscle <- read.table("muscleMass.txt", header = F, sep = "")
names(muscle) <- c("MuscleMass", "Age")

plot(data=muscle, MuscleMass ~ Age)
lm_muscle <- lm(data=muscle, MuscleMass ~ Age)
abline(lm_muscle)
summary_muscle <- summary(lm_muscle)

#homoskedasticity
residualPlot(lm_muscle, id.n = 1)

#independence
plot(lm_muscle$residuals, xlab="Sequence", ylab="Residuals",
     main="Sequence Plot")
#outliers

par(mfrow=c(1,2))
plot(stdres(lm_muscle), xlab="Age", ylab="Standardized Residuals",
     main="Standardized Residual Plot")
abline(h=0)
plot(studres(lm_muscle), xlab="Age", ylab="Studentized Residuals",
     main="Studentized Residual Plot")
abline(h=0)

#normality
par(mfrow=c(1,1))
residplot(lm_muscle)
shapiro.test(lm_muscle$residuals)
qqPlot(residuals(lm_muscle), main="MuscleMass: Normal QQ-Plot",
       xlab="Age (years)", ylab="Muscle Mass (lbs)",
       id.n = 1)

# 2
library(lmtest)
bptest(lm_muscle)
ncvTest(lm_muscle)

# Question 5
lm_senic5 <- lm(data=senic, LengthOfStay ~ InfectionRisk)
predict(lm_senic5, data.frame(InfectionRisk = 11.93), level = 0.97, interval="confidence")
predict(lm_senic5, data.frame(InfectionRisk = 11.93), level = 0.97, interval="prediction")
#cbind(Interval=c("confidence", "prediction"),
#      rbind(predict(lm_senic5, data.frame(InfectionRisk = 11.93), level = 0.97, interval="confidence"),
#            predict(lm_senic5, data.frame(InfectionRisk = 11.93), level = 0.97, interval="prediction")))

alpha = 0.03
n = length(senic$LengthOfStay)
MSE = sum(residuals(lm_senic5)^2)/(n-2)
Xh = 11.93
Xbar = mean(senic$InfectionRisk)
t = qt(1-alpha/2, n-2)
s_pred = 1 + 1/n + ((Xh - Xbar)^2)/(sum((senic$InfectionRisk - Xbar)^2))
s_Yh = 1/n + ((Xh - Xbar)^2)/(sum((senic$InfectionRisk - Xbar)^2))
diff = t * sqrt(MSE) * (sqrt(s_pred) - sqrt(s_Yh))








