dwayne <- read.table("dwaine.txt", header=T)
plot(dwayne)
lm_dwayne <- lm(Sales~targetPop+dispInc, data=dwayne)
lm_dwayne <- lm(Sales~., data=dwayne) #same
#lm(Y~.-X5-X6, data=...) to regress on all Xs except X5, X6
summary(lm_dwayne)
#we expect Y to increase by 1.4546 units when we increase X1 by 1 unit holding X2 remains constant


############# 79
bodyfat <- read.table("bodyFat.txt", header=T)
lm_bodyfat <- lm(data=bodyfat, bodyFat~tricep+thigh+midarm)
anova(lm_bodyfat)
car::Anova(lm_bodyfat)

ssr_tricep_given_thigh_midarm <- car::Anova(lm_bodyfat)$"Sum Sq"[1]
ssr_thigh_given_tricep_midarm <- car::Anova(lm_bodyfat)$"Sum Sq"[2]
ssr_midarm_given_tricep_thigh <- car::Anova(lm_bodyfat)$"Sum Sq"[3]
sse_tricep_thigh <- anova(lm(data=bodyfat, bodyFat~tricep+thigh))$"Sum Sq"[3]
sse_tricep_midarm <- anova(lm(data=bodyfat, bodyFat~tricep+midarm))$"Sum Sq"[3]
sse_thigh_midarm <- anova(lm(data=bodyfat, bodyFat~midarm+thigh))$"Sum Sq"[3]

r2_tricep_given_thigh_midarm <- ssr_tricep_given_thigh_midarm/sse_thigh_midarm
r2_thigh_given_tricep_midarm <- ssr_thigh_given_tricep_midarm/sse_tricep_midarm
r2_midarm_given_tricep_thigh <- ssr_midarm_given_tricep_thigh/sse_tricep_thigh

ssr_tricep_given_thigh <- car::Anova(lm(data=bodyfat, bodyFat~tricep+thigh))$"Sum Sq"[1]
ssr_tricep_given_midarm <- car::Anova(lm(data=bodyfat, bodyFat~tricep+midarm))$"Sum Sq"[1]
ssr_thigh_given_midarm <- car::Anova(lm(data=bodyfat, bodyFat~thigh+midarm))$"Sum Sq"[1]
sse_thigh <- anova(lm(data=bodyfat, bodyFat~thigh))$"Sum Sq"[2]
sse_midarm <- anova(lm(data=bodyfat, bodyFat~midarm))$"Sum Sq"[2]

r2_tricep_given_thigh <- ssr_tricep_given_thigh/sse_thigh
r2_tricep_given_midarm <- ssr_tricep_given_midarm/sse_midarm
r2_thigh_given_midarm <- ssr_thigh_given_midarm/sse_midarm

# Midarm might be correlated with Tricep or Thigh, or it might not explain much. (r2_midarm_given_tricep_thigh)
# But Midarm is not correlated with either Tricep or Thigh. (r2_tricep_given_midarm and r2_thigh_given_midarm)
# So Midarm is not explaining much in general.

################ 92
bodyfat <- read.table("bodyFat.txt", header=T)
pairs(~ bodyFat + tricep + thigh + midarm, data = bodyfat, main="Scatter Plot Matrix")
# we want random scatter in the "smaller square"
# we want linear relationship in first column

# midarm not terribly useful
# tricep and thigh have strong linear relationship with bodyFat
# tricep and thigh have strong linear relationship between themselves -> multicollinearity
# -> model should have either tricep or thigh (probably tricep)

############# 119
library(lmtest)
plot(women$weight~women$height)
lm_women <- lm(women$weight~women$height)
lm_women_quadratic <- lm(women$weight~women$height + I(women$height^2))
lm_women_cubic <- lm(women$weight~women$height + I(women$height^2) + I(women$height^3))
summary(lm_women)$r.squared # 0.991 !!
summary(lm_women_quadratic)$r.squared
summary(lm_women_cubic)$r.squared
resettest(lm_women, power=2, type="fitted") # significant
resettest(lm_women, power=3, type="fitted") # not significant
plot(lm_women$residuals) # very clear quadratic pattern
plot(lm_women_quadratic$residuals) # looks a lot more scattered/random
plot(lm_women_cubic$residuals)

############## 133
adv <- read.csv("Advertising.csv", header=T)
adv <- adv[-1]
pairs(add)
# TV and Radio are not correlated, but there is an interaction

lm_final <- lm(log(adv$Sales)~adv$TV*adv$Radio+I(adv$TV^2))
summary(lm_final)
plot(lm_final)
car::residualPlot(lm_final)
plot(lm_final$residuals)

# coefficient for TV^2 is negative: diminishing returns for high volumes

############### 150
credit <- read.csv("../Credit.csv", header=T)
#credit$Gender <- ifelse(credit$Gender == " Male", 1, 0) #male=1
#credit$Student <- ifelse(credit$Student == "Yes", 1, 0) #student=1
#credit$Married <- ifelse(credit$Married == "Yes", 1, 0) #married=1
#credit$Ethnicity <- ifelse(credit$Ethnicity == "Caucasian", 1, 
#                           ifelse(credit$Ethnicity == "Asian", 2, 3))
credit$X <- NULL
pairs(credit[,c(1,8,11)])
cor(credit[,c(1,8,11)])
lm1 <- lm(data=credit, Balance~Income+Student)
summary(lm1)
car::residualPlot(lm1)
plot(lm1)








