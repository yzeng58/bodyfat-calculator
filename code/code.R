if (!require(ggplot2)){
  install.packages("ggplot2")
  stopifnot(require(ggplot2))
}
if (!require(gridExtra)){
  install.packages("gridExtra")
  stopifnot(require(gridExtra))
}

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


bf = read.csv("BodyFat.csv",sep=",")
head(bf)

bf$pd.bf = 495/bf$DENSITY-450; ol.bf = order(abs(bf$pd.bf-bf$BODYFAT), decreasing = TRUE)[1:5]
bf$outlier = 1:dim(bf)[1] %in% ol.bf; bf[ol.bf,c("BODYFAT","pd.bf")]

ol.bf = c(48,76,182,216)

bf$pd.bmi = 0.45359237*bf$WEIGHT/(bf$HEIGHT*0.0254)^2
ol.bmi = order(abs(bf$pd.bmi-bf$ADIPOSITY), decreasing = TRUE)[1:3]
bf$outlier = 1:dim(bf)[1] %in% ol.bmi; bf[ol.bmi,c("ADIPOSITY","pd.bmi","WEIGHT","HEIGHT")]

ol.bmi = c(163,221);bf[42,"HEIGHT"] = 69.45;bf1 = bf[-c(ol.bf,ol.bmi), c(2,4:17)]

model_Army <- lm(BODYFAT ~ log(ABDOMEN - NECK) + log(HEIGHT),data=bf1)

model_BMI_ABDOMEN <- lm(ADIPOSITY ~ ABDOMEN,data = bf1)
bf1$h_1 = 1/(bf1$HEIGHT);bf1$dh_1 = 1/(bf1$ABDOMEN * bf1$HEIGHT);bf1$d_h1 = bf1$ABDOMEN / bf1$HEIGHT
model_Tsuna_V1 <- lm(BODYFAT ~ dh_1 + d_h1 + h_1,data=bf1)

bf1$BMI.ABDOMEN <- 0.308909*bf1$ABDOMEN - 3.162932;bf1$HEIGHT.CM <- bf1$HEIGHT * 2.54
bf1$h_2 = 1/(bf1$HEIGHT.CM);bf1$dh_2 = 1/(bf1$BMI.ABDOMEN * bf1$HEIGHT.CM);bf1$d_h2 = bf1$BMI.ABDOMEN / bf1$HEIGHT.CM
model_Tsuna_V2 <- lm(BODYFAT ~ dh_2 + d_h2 + h_2,data = bf1)

bf1$h_3 = 1/(bf1$HEIGHT.CM);bf1$dh_3 = 1/(bf1$ADIPOSITY * bf1$HEIGHT.CM);bf1$d_h3 = bf1$ADIPOSITY / bf1$HEIGHT.CM
model_Tsuna_V3 <- lm(BODYFAT ~ dh_3 + d_h3 + h_3, data = bf1)

exhaus = read.csv("./image/step_result.csv");exhaus[c(1,14,15,16,105,106),]

bf1 = bf1[-39,]

cat("Model 1: ");MSE_Cal(BODYFAT~log(HEIGHT)+log(ABDOMEN), fold = 10, data = bf1)

cat("Model 2: "); MSE_Cal(BODYFAT ~ dh_1 + d_h1 + h_1, fold = 10, data = bf1)

cat("Model 3: "); MSE_Cal(BODYFAT ~ ABDOMEN + WEIGHT, fold = 10, data = bf1)