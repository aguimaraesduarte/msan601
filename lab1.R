toluca <- read.table("toluca.txt" , sep = "" , header = T)
lm_toluca <- lm(workHours ~ lotSize , data = toluca)
plot(toluca$lotSize , toluca$workHours , xlab = "Lot Size" ,
     ylab = "Work Hours" , main = "Toluca Company: Refrigeration Manufacturer")
abline(lm_toluca)
summary(lm_toluca)
residuals(lm_toluca)
fitted(lm_toluca)
sqrt(sum((toluca$workHours - fitted.values(lm_toluca))^2)/(length(toluca$workHours)-2)) # sqrt(MSE)
sqrt(sum(residuals(lm_toluca)^2)/(length(toluca$workHours)-2)) # sqrt(MSE)

b1 = (sum((toluca$lotSize - mean(toluca$lotSize))*(toluca$workHours - mean(toluca$workHours))))/(sum((toluca$lotSize - mean(toluca$lotSize))^2))
MSE = sum(residuals(lm_toluca)^2)/(length(toluca$workHours)-2)
s2b1 = MSE/(sum((toluca$lotSize - mean(toluca$lotSize))^2))
(t_star = b1/sqrt(s2b1))
qt(1-.05/2, length(toluca$workHours-2))
ifelse(abs(t_star) <= qt(1-.05/2, length(toluca$workHours-2)), "Do not reject H0", "Reject H0")

confint(lm_toluca)
cbind(Estimate=coef(lm_toluca),confint(lm_toluca))

b0 = mean(toluca$workHours) - b1*mean(toluca$lotSize)
s2b0 = MSE*(1/length(toluca$lotSize) + mean(toluca$lotSize/(sum((toluca$lotSize - mean(toluca$lotSize))^2))))
(t_star = b0/sqrt(s2b0)) #something wrong here
qt(1-.05/2, length(toluca$lotSize-2))

predict(lm_toluca,data.frame(lotSize=33),interval="confidence")
predict(lm_toluca,data.frame(lotSize=c(33,99)),interval="confidence")

summary(lm_toluca)$fstatistic
anova(lm_toluca)
summary(lm_toluca)$r.squared
summary(lm_toluca)$coefficients
