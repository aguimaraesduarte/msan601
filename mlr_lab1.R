dwayne <- read.table("dwaine.txt", header=T)
plot(dwayne)
lm_dwayne <- lm(Sales~targetPop+dispInc, data=dwayne)
lm_dwayne <- lm(Sales~., data=dwayne) #same
#lm(Y~.-X5-X6, data=...) to regress on all Xs except X5, X6
summary(lm_dwayne)
#we expect Y to increase by 1.4546 units when we increase X1 by 1 unit holding X2 remains constant


#############3 79
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
