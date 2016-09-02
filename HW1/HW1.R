#### Question 8
copier = read.csv("copierMaintenanceData.csv", header = F)
plot(copier$V2, copier$V1)
n = length(copier$V1)

#1
b1 = (sum((copier$V2 - mean(copier$V2))*(copier$V1 - mean(copier$V1))))/(sum((copier$V2 - mean(copier$V2))^2))
b0 = mean(copier$V1) - b1*mean(copier$V2)

#2
print("y_hat = b0 + b1*X")

#3
SSE = sum((copier$V1 - (b0 + b1*copier$V2))^2)

#4
print("no")

#5
y5 = b0+b1*5

#6
MSE = SSE/(n - 2)
s2b1 = MSE/(sum((copier$V2 - mean(copier$V2))^2))
t_star = b1/sqrt(s2b1)
t_lim = qt(1-.05/2, n-2)

#7
t_lim_2 = qt(1-.05, n-2)

#8
s2y5 = MSE * (1/n + ((5 - mean(copier$V2))^2)/(sum((copier$V2 - mean(copier$V2))^2)))
lower = y5 - t_lim*sqrt(s2y5)
upper = y5 + t_lim*sqrt(s2y5)

#9
SSR = sum((b0 + b1*copier$V2 - mean(copier$V1))^2)
MSR = SSR/1
F_star = MSR/MSE
F_lim = qf(1-.05/2, 1, n-2)
t_star_sq = t_star^2

#10
vec_b0 = c(0, 0, 0, 1, -1, 10)
vec_b1 = c(15, 10, 20, 15, 15, 25)
for(i in seq_along(vec_b0)){
  vec_fitted = vec_b0[i] + vec_b1[i]*copier$V2
  vec_SSE = sum((copier$V1 - (vec_b0[i] + vec_b1[i]*copier$V2))^2)
  print(paste("For b0 =", vec_b0[i], "and b1 =", vec_b1[i], ", we get SSE =", vec_SSE))
}

#### Question 9
df9 = data.frame(
  "Country" = c("Philippines",
                "Mexico",
                "Colombia",
                "Yugoslavia",
                "Panama",
                "Romania",
                "Czechoslovakia",
                "Spain",
                "Finland",
                "United Kingdom",
                "Canada",
                "France",
                "Australia",
                "United States",
                "Sweden"),
  "DietaryFat" = c(29,
                   57,
                   47,
                   72,
                   58,
                   67,
                   96,
                   97,
                   112,
                   143,
                   142,
                   137,
                   129,
                   147,
                   132),
  "DeathRate" = c(1.3,
                  4.5,
                  5.4,
                  5.6,
                  7.8,
                  8.8,
                  9.1,
                  10.1,
                  11.7,
                  12.4,
                  13.4,
                  14.4,
                  15.1,
                  16.3,
                  18.4)
)

plot(DeathRate ~ DietaryFat, data = df9, xlim = c(20, 150))
text(df9$DietaryFat, df9$DeathRate, df9$Country, cex = .7, pos = 2)

lm9 = lm(DeathRate ~ DietaryFat, data = df9)
summary(lm9)

cbind(Estimate=coef(lm9), confint(lm9))

#### Question 10
summary(lm(waiting ~ eruptions, data = faithful))
summary(lm(eruptions ~ waiting, data = faithful))

#### Question 12
X = c(1:100)
Y = X ** 2
summary(lm(Y ~ X))$r.squared

X = c(1:100)
Y = 2 * X
summary(lm(Y ~ X))$r.squared


