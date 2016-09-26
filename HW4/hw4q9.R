# Reset R session
rm(list=ls())
cat("\014")

df <- read.csv("hw4q9.csv", header=F)
df <- rateprof
df <- read.table("../bodyFat.txt", header=T)
df <- df[,getOrder(4,4)]
y <- df[,1] # extract the response variable
df <- df[-1] # remove response from df
n <- nrow(df)
p <- ncol(df)

for(i in 1:p){
  lm_basic <- lm(y~df[,i])
  R2 <- summary(lm_basic)$r.squared
  cat(paste("R2 (", i, "): ", round(R2, 3), "\n", sep = ""))
  if(p < 2) break
  for(j in 1:p){
    if(i==j) next
    lm_full <- lm(y~df[,i]+df[,j])
    SSE_full <- tail(anova(lm_full)$"Sum Sq", 1)
    lm_given <- lm(y~df[,j])
    SSE_given <- tail(anova(lm_given)$"Sum Sq", 1)
    R2 <- (SSE_given - SSE_full) / SSE_given
    cat(paste("R2 (", i, " given ", j, "): ", round(R2, 3), "\n", sep = ""))
    if(p < 3) break
    for(k in j:p){
      if(i==k | j==k) next
      lm_full <- lm(y~df[,i]+df[,j]+df[,k])
      SSE_full <- tail(anova(lm_full)$"Sum Sq", 1)
      lm_given <- lm(y~df[,j]+df[,k])
      SSE_given <- tail(anova(lm_given)$"Sum Sq", 1)
      R2 <- (SSE_given - SSE_full) / SSE_given
      cat(paste("R2 (", i, " given ", j, ", ", k, "): ", round(R2, 3), "\n", sep = ""))
      if(p < 4) break
      for(l in k:p){
        print(l)
        if(i==l | j==l | k==l) next
        lm_full <- lm(y~df[,i]+df[,j]+df[,k]+df[,l])
        SSE_full <- tail(anova(lm_full)$"Sum Sq", 1)
        lm_given <- lm(y~df[,j]+df[,k]+df[,l])
        SSE_given <- tail(anova(lm_given)$"Sum Sq", 1)
        R2 <- (SSE_given - SSE_full) / SSE_given
        cat(paste("R2 (", i, " given ", j, ", ", k, ", ", l, "): ", round(R2, 3), "\n", sep = ""))
        if(p < 5) break
        for(m in l:p){
          print(l)
          if(i==m | j==m | k==m | l==m) next
          lm_full <- lm(y~df[,i]+df[,j]+df[,k]+df[,l]+df[,m])
          SSE_full <- tail(anova(lm_full)$"Sum Sq", 1)
          lm_given <- lm(y~df[,j]+df[,k]+df[,l]+df[,m])
          SSE_given <- tail(anova(lm_given)$"Sum Sq", 1)
          R2 <- (SSE_given - SSE_full) / SSE_given
          cat(paste("R2 (", i, " given ", j, ", ", k, ", ", l, "): ", round(R2, 3), "\n", sep = ""))
      }
    }
  }
}


