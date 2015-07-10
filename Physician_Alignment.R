############################# Physicians Alignment Tool ####################################
############################# Random Forest ####################################

DFW_Align <- read.csv("~/Medicare Referral/Data/Physician_Compare_Databases/2015_1_29_Ed_Levine_Team/INPUT.csv", header = TRUE)
library(reshape)
library(randomForest)
library(caret)
library(nnet)
library(e1071)
names(DFW_Align)
Sample <- DFW_Align[which(DFW_Align$Affiliation != ""),c(1:6, 10:37)]
Sample$Affiliation <- as.character(Sample$Affiliation)
train_data <- rbind(Sample[which(Sample$Affiliation == "BQA")[1:165],], Sample[which(Sample$Affiliation == "BQA/THPG")[1:5],], Sample[which(Sample$Affiliation == "THPG")[1:100],], Sample[which(Sample$Affiliation == "UT employed")[1:7],], Sample[which(Sample$Affiliation == "UTSCAP")[1:25],])
test_data <- rbind(Sample[which(Sample$Affiliation == "BQA")[166:334],], Sample[which(Sample$Affiliation == "BQA/THPG")[6:9],], Sample[which(Sample$Affiliation == "THPG")[101:195],], Sample[which(Sample$Affiliation == "UT employed")[8:13],], Sample[which(Sample$Affiliation == "UTSCAP")[26:52],])
bestmtry <- tuneRF(x = train_data[,7:34], y = as.factor(train_data[,6]), ntree = 5000, nodesize = 1)
DFW_RF <- randomForest(x = train_data[,7:34], y = as.factor(train_data[,6]), ntree = 5000, mtry = 5, proximity = TRUE)
DFW_pred <- predict(DFW_RF, test_data[,7:34])
table(DFW_pred, test_data[,7])

############################## Caret trained model ######################################

RF_trained <- train(x = train_data[,7:34], y = as.factor(train_data[,6]), method = "rf", trControl = trainControl(method = "cv", number = 1000))
DFW_pred_caret <- predict(RF_trained$finalModel, test_data[,7:34])
table(DFW_pred_caret, test_data[,6])

######## Center For Different Groups ###############
classCenter(x = train_data[,7:34], label = as.factor(train_data[,6]), DFW_RF$proximity)

####### Plot and OBs ############
table(oberved = test_data$Affiliation, predicted = DFW_pred)

DFW_RF$confusion

varImpPlot(DFW_RF)

partialPlot(DFW_RF, pred.data = train_data,x.var = "UTSW")

MDSplot(DFW_RF, as.factor(train_data$Affiliation), k = 4)

plot(DFW_RF)

getTree(DFW_RF)

######## Use a Different Package ##############
install.packages("tree")
library(tree)

DFW.data <- as.data.frame(cbind("Affiliation" = as.factor(train_data[,6]),train_data[,7:34]))
dim(DFW.data)
names(DFW.data[,2:29]) <- c(1:28)
DFW.tree <- tree(Affiliation ~.,DFW.data)
DFW.tree$frame
plot(DFW.tree, type = "proportional")

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
DFW.rprt <- rpart(Affiliation ~., DFW.data, )
plot(DFW.rprt)
prp(DFW.rprt)

##### Sample ######
data(iris)
iris.rf <- randomForest(iris[,-5], iris[,5], prox=TRUE)
iris.p <- classCenter(iris[,-5], iris[,5], iris.rf$prox)
plot(iris[,3], iris[,4], pch=21, xlab=names(iris)[3], ylab=names(iris)[4],
     bg=c("red", "blue", "green")[as.numeric(factor(iris$Species))],
     main="Iris Data with Prototypes")
points(iris.p[,3], iris.p[,4], pch=21, cex=2, bg=c("red", "blue", "green"))

ir.tr <- tree(Petal.Width ~ Petal.Length, iris)
plot(ir.tr)
############################# Adding Distance Information in the prediction ########################
DFW_Distance <- read.csv("~/Medicare Referral/Data/Physician_Compare_Databases/2015_1_29_Ed_Levine_Team/INPUT2.csv", header = TRUE)

library(geosphere)
DFW_Distance <- as.matrix(DFW_Distance)
Distance_Mat <- matrix(nrow = nrow(DFW_Distance), ncol = 1)
for(i in c(1:nrow(DFW_Distance))){
  Distance_Mat[i,1] <-distm(as.numeric(DFW_Distance[i,4:5]), as.numeric(DFW_Distance[i,9:10]))
  print(i)
}

Distance_Mat <- as.numeric(Distance_Mat)
write.csv(Distance_Mat, "~/Medicare Referral/Data/Physician_Compare_Databases/2015_1_29_Ed_Levine_Team/OUTPUT.csv")

rm(DFW_Distance)

DFW_Align_wDist <- read.csv("~/Medicare Referral/Data/Physician_Compare_Databases/2015_1_29_Ed_Levine_Team/INPUT2.csv", header = TRUE)
names(DFW_Align_wDist)
odd <- seq(5,60, 2)
even <- seq(6,60,2)
names(DFW_Align_wDist)[odd] <- names(DFW_Align)[10:37]
train_data_2 <- DFW_Align_wDist[which(DFW_Align_wDist[,3] == "Y"),]
test_data_2 <- DFW_Align_wDist[which(DFW_Align_wDist[,3] == "N"),]
for(i in c(1:nrow(train_data_2))){
  train_data_2[i,which(is.na(train_data_2[i,]))] <- 0
}
for(i in c(1:nrow(test_data_2))){
  test_data_2[i,which(is.na(test_data_2[i,]))] <- 0
}

tuneRF(x = train_data_2[,5:60], y = as.factor(train_data_2[,2]), ntree = 5000)
DFW_RF_wDist <- randomForest(x = train_data_2[,5:60], y = as.factor(train_data_2[,2]), ntree = 5000, mtry = 14, proximity = TRUE)
DFW_pred_2 <- predict(DFW_RF_wDist, test_data_2[,5:60])
table(DFW_pred_2, test_data_2[,2])

RF_trained <- train(x = train_data_2[,5:60], y = as.factor(train_data_2[,2]), method = "rf")
DFW_pred_3 <- predict(RF_trained$finalModel, test_data_2[,5:60])
table(DFW_pred_3, test_data_2[,2])

######### 85% accuracy, compared with 78% accuracy without distance factor #########
sum(DFW_pred_2 == test_data_2[,2])/nrow(test_data_2)


varImpPlot(DFW_RF_wDist)

partialPlot(DFW_RF, pred.data = train_data,x.var = "")

MDSplot(DFW_RF_wDist, as.factor(train_data_2$Affiliation), k = 4)

par(mar=c(5,2,4,2))
plot(DFW_RF_wDist)
legend("topright", colnames(DFW_RF_wDist$err.rate),col=1:6,cex=0.8,fill=1:6)

getTree(DFW_RF_wDist, k = 1, labelVar = TRUE)

summary(train_data_2[,2:3])
summary(DFW_Align_wDist[,2:3])

############################# Neural Network ####################################
install.packages("neuralnet")
install.packages("nnet")
install.packages("caret")
library(caret)
library(nnet)
library(e1071)

set.seed(0)

Neural_model <- nnet(Affiliation ~., data = cbind("Affiliation" = train_data[,6], train_data[,7:34]), decay = 1e-4, size = 3)
neural_pred <- predict(Neural_model, newdata = test_data[,7:34],type = "class")
table(neural_pred, test_data[,6])

Neural_train_cv <- train(Affiliation ~.,rang = 0.05, data = cbind("Affiliation" = train_data[,6], train_data[,7:34]), method = "nnet", trControl = trainControl(method = 'cv', number = 100))
neural_pred_2 <- predict(Neural_train_cv$finalModel, newdata = test_data[,7:34],type = "class")
table(neural_pred_2, test_data[,6])

Neural_train_adp_cv <- train(Affiliation ~.,rang = 0.05, data = cbind("Affiliation" = train_data[,6], train_data[,7:34]), method = "nnet", trControl = trainControl(method = 'adaptive_cv', number = 100))
neural_pred_3 <- predict(Neural_train_adp_cv$finalModel, newdata = test_data[,7:34],type = "class")
table(neural_pred_3, test_data[,6])

Neural_train_boot <- train(Affiliation ~.,rang = 0.05, data = cbind("Affiliation" = train_data[,6], train_data[,7:34]), method = "nnet", trControl = trainControl(method = 'boot', number = 100))
neural_pred_4 <- predict(Neural_train_boot$finalModel, newdata = test_data[,7:34],type = "class")
table(neural_pred_4, test_data[,6])

Neural_train_adp_boot <- train(Affiliation ~.,rang = 0.05, data = cbind("Affiliation" = train_data[,6], train_data[,7:34]), method = "nnet", trControl = trainControl(method = 'adaptive_boot', number = 100))
neural_pred_5 <- predict(Neural_train_adp_boot$finalModel, newdata = test_data[,7:34],type = "class")
table(neural_pred_5, test_data[,6])

Neural_train_entrophy <- train(Affiliation ~.,rang = 0.05, entrophy = TRUE, data = cbind("Affiliation" = train_data[,6], train_data[,7:34]), method = "nnet", trControl = trainControl(method = 'cv', number = 1000))
neural_pred_6 <- predict(Neural_train_entrophy$finalModel, newdata = test_data[,7:34],type = "class")
table(neural_pred_6, test_data[,6])

plot(Neural_model)

############## Bagging neural network(weird results) ############################

Neural_bag <- bag(x = train_data[,7:34], B = 2, y = as.factor(train_data[,6]), bagControl = bagControl(fit = nnetBag$fit, predict = nnetBag$pred, aggregate = nnetBag$aggregate), size = 2, entrophy = TRUE)
Neural_bag <- predict(Neural_bag, test_data[,7:34], type = "class")
table(Neural_bag, test_data[,6])

