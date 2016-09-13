data <- read.csv("hw2_q5_data.csv", header = F)
x <- data[,1]
y <- data[,2]

linearBC <- function(X, Y){
  # 0.1 precision
  lambdas <- seq(-3, 3, 0.1)
  SSE <- rep(0, length(lambdas))
  K2 <- prod(Y)^(1/length(Y))
  
  i <- 1
  for(lambda in lambdas){
    Ylambda <- Y^lambda
    K1 <- 1/(lambda*K2^(lambda-1))
    if(lambda==0){
      W <- K2*(log(Y))
    } else{
      W <- K1*(Ylambda-1)
    }
    lm_lambda <- lm(W~X)
    # store SSE for this lambda
    SSE[i] <- sum(lm_lambda$residuals^2)
    i <- i+1
  }
  # return lambda for minimum SSE
  optimal <- lambdas[which.min(SSE)]
  return(optimal)
}

cat("Optimal lambda is", linearBC(x, y))
