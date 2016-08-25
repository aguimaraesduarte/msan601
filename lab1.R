toluca <- read.table("toluca.txt" , sep = "" , header = F)
names(toluca) <- c("lotSize", "workHours")
lm_toluca <- lm(workHours ~ lotSize , data = toluca)
plot(toluca$lotSize , toluca$workHours , xlab = "Lot Size" ,
     ylab = "Work Hours" , main = "Toluca Company: Refrigeration Manufacturer")
abline(lm_toluca)
summary(lm_toluca)
