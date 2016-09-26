# Reset R session
rm(list=ls())
cat("\014")

df <- read.csv("hw4q8.csv", header=F)
y <- df[,1] # extract the response variable
df <- df[-1] # remove response from df
n <- nrow(df)
p <- ncol(df)
alpha <- 0.05
delta <- c(1.2, 1.5, 1.7)

df_t <- data.frame(colnames(df), rep(0, p), rep(0, p)) # create result dataframe

for(d in delta){
  cat("\n") # for display purposes
  names(df_t) <- c(paste("d =", d), "Test stat (d=1.2)", "Pr(>|t|)") # make it look like residualPlots
  for(i in 1:p){
    lm_t <- lm(data=df, y~.+I(df[,i]^d)) # MLR on all variables plus one to the power d
    summary_t <- summary(lm_t) # corresponding summary to extract values
    t_star <- tail(summary_t$coefficients[, 1], 1) / tail(summary_t$coefficients[, 2], 1) # t*
    p_t <- 2*pt(abs(t_star), n-p, lower=FALSE) # p-value
    df_t[i, 2] <- round(t_star, 3) # update result dataframe
    df_t[i, 3] <- round(p_t, 3) # update result dataframe
  }
  print(df_t, row.names = FALSE, right=F) # print "prettily'
}
