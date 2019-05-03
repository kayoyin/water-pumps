#############################################################################################
# dummy.R
# By : Rudi Kruger
# Further processes the data produced by clean_data.R by creating dummy vars
# for all the factors, as required by ALGLIB.
# Do whatever you want with this
#############################################################################################

require(caret)

train <- read.csv("myTrain.csv")
test <- read.csv("myTest.csv")

dummies <- dummyVars(id ~ ., data = train)
train2 <- as.data.frame(predict(dummies, newdata = train))
train2 <- cbind(id=train$id, train2)
test2 <- as.data.frame(predict(dummies, newdata = test))
test2 <- cbind(id=test$id, test2)

write.csv(train2, "myTrainDummy.csv", row.names=FALSE)
write.csv(test2, "myTestDummy.csv", row.names=FALSE)
