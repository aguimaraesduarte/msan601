# Reset R session
rm(list=ls())
cat("\014")

getOrder <- function(numCol, yColIndex){
  if(yColIndex < numCol){
    return(c(yColIndex, 1:(yColIndex-1), (yColIndex+1):numCol))
  } else{
    return(c(yColIndex, 1:(yColIndex-1)))
  }
}

rateprof <- alr4::Rateprof
rateprof <- as.data.frame(rateprof[!sapply(rateprof, is.factor)])
rateprof <- rateprof[,getOrder(13, 4)]
df <- rateprof[,1:7]

#df <- read.csv("hw4q9.csv", header=F)

y <- df[,1] # extract the response variable
df <- df[-1] # remove response from df
n <- nrow(df)
p <- ncol(df)

# I tried to make this code scalable, but couldn't find a way to do it.
# So I kept this version since it works, although it is ugly.

for(i in 1:p){
  # "base" R2 values (no given data)
  lm_basic <- lm(y~df[,i])
  R2 <- summary(lm_basic)$r.squared
  cat(paste("R2 (", names(df)[i], "): ", round(R2, 3), "\n", sep = ""))
  for(j in 1:p){
    # given one variable
    if(i==j) next
    lm_full <- lm(y~df[,i]+df[,j])
    SSE_full <- tail(anova(lm_full)$"Sum Sq", 1)
    lm_given <- lm(y~df[,j])
    SSE_given <- tail(anova(lm_given)$"Sum Sq", 1)
    R2 <- (SSE_given - SSE_full) / SSE_given
    cat(paste("R2 (", names(df)[i], " | ", names(df)[j], "): ", round(R2, 3), "\n", sep = ""))
    for(k in j:p){
      # given 2 variables
      if(i==k | j==k) next
      lm_full <- lm(y~df[,i]+df[,j]+df[,k])
      SSE_full <- tail(anova(lm_full)$"Sum Sq", 1)
      lm_given <- lm(y~df[,j]+df[,k])
      SSE_given <- tail(anova(lm_given)$"Sum Sq", 1)
      R2 <- (SSE_given - SSE_full) / SSE_given
      cat(paste("R2 (", names(df)[i], " | ", names(df)[j], ", ", names(df)[k], "): ", round(R2, 3), "\n", sep = ""))
      for(l in k:p){
        # given 3 variables
        if(i==l | j==l | k==l) next
        lm_full <- lm(y~df[,i]+df[,j]+df[,k]+df[,l])
        SSE_full <- tail(anova(lm_full)$"Sum Sq", 1)
        lm_given <- lm(y~df[,j]+df[,k]+df[,l])
        SSE_given <- tail(anova(lm_given)$"Sum Sq", 1)
        R2 <- (SSE_given - SSE_full) / SSE_given
        cat(paste("R2 (", names(df)[i], " | ", names(df)[j], ", ", names(df)[k], ", ", names(df)[l], "): ", round(R2, 3), "\n", sep = ""))
        for(m in l:p){
          # given 4 variables
          if(i==m | j==m | k==m | l==m) next
          lm_full <- lm(y~df[,i]+df[,j]+df[,k]+df[,l]+df[,m])
          SSE_full <- tail(anova(lm_full)$"Sum Sq", 1)
          lm_given <- lm(y~df[,j]+df[,k]+df[,l]+df[,m])
          SSE_given <- tail(anova(lm_given)$"Sum Sq", 1)
          R2 <- (SSE_given - SSE_full) / SSE_given
          cat(paste("R2 (", names(df)[i], " | ", names(df)[j], ", ", names(df)[k], ", ", names(df)[l], ", ", names(df)[m], "): ", round(R2, 3), "\n", sep = ""))
          for(n in m:p){
            # given 6 variables
            if(i==n | j==n | k==n | l==n | m==n) next
            lm_full <- lm(y~df[,i]+df[,j]+df[,k]+df[,l]+df[,m]+df[,n])
            SSE_full <- tail(anova(lm_full)$"Sum Sq", 1)
            lm_given <- lm(y~df[,j]+df[,k]+df[,l]+df[,m]+df[,n])
            SSE_given <- tail(anova(lm_given)$"Sum Sq", 1)
            R2 <- (SSE_given - SSE_full) / SSE_given
            cat(paste("R2 (", names(df)[i], " | ", names(df)[j], ", ", names(df)[k], ", ", names(df)[l], ", ", names(df)[m], ", ", names(df)[n], "): ", round(R2, 3), "\n", sep = ""))
            for(o in n:p){
              # given 7 variables
              if(i==o | j==o | k==o | l==o | m==o | n==o) next
              lm_full <- lm(y~df[,i]+df[,j]+df[,k]+df[,l]+df[,m]+df[,n]+df[,o])
              SSE_full <- tail(anova(lm_full)$"Sum Sq", 1)
              lm_given <- lm(y~df[,j]+df[,k]+df[,l]+df[,m]+df[,n]+df[,o])
              SSE_given <- tail(anova(lm_given)$"Sum Sq", 1)
              R2 <- (SSE_given - SSE_full) / SSE_given
              cat(paste("R2 (", names(df)[i], " | ", names(df)[j], ", ", names(df)[k], ", ", names(df)[l], ", ", names(df)[m], ", ", names(df)[n], ", ", names(df)[o], "): ", round(R2, 3), "\n", sep = ""))
              for(q in o:p){
                # given 8 variables
                if(i==q | j==q | k==q | l==q | m==q | n==q | o==q) next
                lm_full <- lm(y~df[,i]+df[,j]+df[,k]+df[,l]+df[,m]+df[,n]+df[,o]+df[,q])
                SSE_full <- tail(anova(lm_full)$"Sum Sq", 1)
                lm_given <- lm(y~df[,j]+df[,k]+df[,l]+df[,m]+df[,n]+df[,o]+df[,q])
                SSE_given <- tail(anova(lm_given)$"Sum Sq", 1)
                R2 <- (SSE_given - SSE_full) / SSE_given
                cat(paste("R2 (", names(df)[i], " | ", names(df)[j], ", ", names(df)[k], ", ", names(df)[l], ", ", names(df)[m], ", ", names(df)[n], ", ", names(df)[o], ", ", names(df)[q], "): ", round(R2, 3), "\n", sep = ""))
              }
            }
          }
        }
      }
    }
  }
}


