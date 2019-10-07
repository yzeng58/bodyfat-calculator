MSE_Cal <- function(expr,fold = 10,data){
  MSE <- c()
  for(i in 1:fold){
    test_index <- sample(1:nrow(data),floor(nrow(data)/10))
    testdata <- data[test_index,];traindata <- data[-test_index,]
    model_test <- lm(expr  ,data = traindata)
    predicted.values <- predict(object = model_test,testdata)
    MSE[i] <- sqrt(mean((predicted.values-testdata$BODYFAT)^2))
  }
  MSE_mean <- mean(MSE)
  MSE_SD <- sd(MSE)
  Expr <- expr
  res <- cbind(MSE_mean,MSE_SD)
  print(Expr)
  return(res)
}












