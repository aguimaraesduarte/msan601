dwayne <- read.table("dwaine.txt", header=T)
plot(dwayne)
lm_dwayne <- lm(Sales~targetPop+dispInc, data=dwayne)
lm_dwayne <- lm(Sales~., data=dwayne) #same
#lm(Y~.-X5-X6, data=...) to regress on all Xs except X5, X6
summary(lm_dwayne)
#we expect Y to increase by 1.4546 units when we increase X1 by 1 unit holding X2 remains constant

