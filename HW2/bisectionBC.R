data <- read.csv("hw2_q5_data.csv", header = F)
x <- data[,1]
y <- data[,2]

bisectionBC <- function(X, Y){
  lower <- -3
  upper <- 3
  SSE <- Inf
  
  K2 <- prod(Y)^(1/length(Y))
  
  # 0.001 precision
  while(abs(lower-upper)>0.001){
    K1_lower <- 1/(lower*K2^(lower-1))
    K1_upper <- 1/(upper*K2^(upper-1))
    
    Y_lower <- Y^lower
    Y_upper <- Y^upper
    
    if(lower==0){
      W_lower <- K2*(log(Y))
    } else{
      W_lower <- K1_lower*(Y_lower-1)
    }
    
    if(upper==0){
      W_upper <- K2*(log(Y))
    } else{
      W_upper <- K1_upper*(Y_upper-1)
    }
    
    lm_lower <- lm(W_lower~X)
    lm_upper <- lm(W_upper~X)
    
    SSE_lower <- sum(lm_lower$residuals^2)
    SSE_upper <- sum(lm_upper$residuals^2)
    
    # bisect and update variables
    if(SSE_lower<SSE_upper){
      SSE <- SSE_lower
      upper <- (lower+upper)/2.0
    } else{
      SSE <- SSE_upper
      lower <- (lower+upper)/2.0
    }
  }
  
  # return last bisection
  return((lower+upper)/2.0)
}

cat("Optimal lambda is", bisectionBC(x, y))
