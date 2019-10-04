body <- read.csv("BodyFat.csv")
summary(body)
head(body)

body$BMI = body$WEIGHT*705/((body$HEIGHT)^2)
body$BODYFAT_Cal <- 495/body$DENSITY-450
head(BMI)
View(body[,7]-body[,18])
View(body[,2]-body[,19])

#42 153 BMI is not consistent to ADIPOSITY
#42 height is kind of strangely small
#39,41, 216 BMI is strangely large(not so strange)
#39 abdomen
#48 216 38 192 169 205... is inconsistent to body_fat calculated by 495/density - 450
#180 bodyfat is 0
qqplot(body$BODYFAT,1/body$DENSITY)
abline(lm(body$BODYFAT~(1/body$DENSITY)))

body.sub <- body[-c(42,153,39,41,48,216,38,180),]

#model from US Army
model_Army <- lm(BODYFAT ~ log(ABDOMEN) + log(HEIGHT) ,data = body.sub)
summary(model_Army)

plot(model_Army)

#model from Su ???
model_Su <- lm(BODYFAT ~ WEIGHT + HIP + WRIST + ABDOMEN^3 + HIP^3,data = body.sub)
summary(model_Su)


set.seed(2019)
MSE <- c()
for(i in 1:252){
  test_index <- i
  testdata <- body[test_index,]
  traindata <- body[-test_index,]
  model_test <- lm(BODYFAT ~ log(ABDOMEN) + log(HEIGHT)  ,data = traindata)
  predicted.values <- predict(object = model_test,testdata)
  MSE <- c(MSE,sum((predicted.values-testdata$BODYFAT)^2)/floor(nrow(body)/5))
}
mean(MSE)
