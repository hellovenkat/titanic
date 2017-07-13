train_data <- read.csv('/home/venkat/Desktop/train.csv')
test_data <- read.csv('/home/venkat/Desktop/test.csv')
dim(train_data)
dim(test_data)
dim(test_data)
train_data$Cabin <- NULL
test_data$Cabin <- NULL
train_data$Ticket <- NULL
test_data$Ticket <- NULL
train_data$Title <- gsub('(.*, )|(\\..*)', '', train_data$Name)
test_data$Title <- gsub('(.*, )|(\\..*)', '', test_data$Name)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don',
'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
train_data$Title[train_data$Title == 'Mlle']        <- 'Miss'
train_data$Title[train_data$Title == 'Ms']          <- 'Miss'
train_data$Title[train_data$Title == 'Mme']         <- 'Mrs'
train_data$Title[train_data$Title %in% rare_title]  <- 'Rare Title'
test_data$Title[test_data$Title == 'Mlle']        <- 'Miss'
test_data$Title[test_data$Title == 'Ms']          <- 'Miss'
test_data$Title[test_data$Title == 'Mme']         <- 'Mrs'
test_data$Title[test_data$Title %in% rare_title]  <- 'Rare Title'
train_data$Name <- NULL
test_data$Name <- NULL
title_list <- c('Mr', 'Miss', 'Mrs', 'Master', 'Rare Title')
master_train <- mean(train_data$Age[train_data$Title=="Master"], na.rm = TRUE)
mr_train <- mean(train_data$Age[train_data$Title=="Mr"], na.rm = TRUE)
mrs_train <- mean(train_data$Age[train_data$Title=="Mrs"], na.rm = TRUE)
miss_train <- mean(train_data$Age[train_data$Title=="Miss"], na.rm = TRUE)
rare_train <- mean(train_data$Age[train_data$Title=="Rare Title"], na.rm = TRUE)
master_test <- mean(test_data$Age[test_data$Title=="Master"], na.rm = TRUE)
mr_test <- mean(test_data$Age[test_data$Title=="Mr"], na.rm = TRUE)
mrs_test <- mean(test_data$Age[test_data$Title=="Mrs"], na.rm = TRUE)
miss_test <- mean(test_data$Age[test_data$Title=="Miss"], na.rm = TRUE)
rare_test <- mean(test_data$Age[test_data$Title=="Rare Title"], na.rm = TRUE)
train_data$Age[which(is.na(train_data$Age) & train_data$Title=='Mr')] <- mr_train
train_data$Age[which(is.na(train_data$Age) & train_data$Title=='Mrs')] <- mrs_train
train_data$Age[which(is.na(train_data$Age) & train_data$Title=='Miss')] <- miss_train
train_data$Age[which(is.na(train_data$Age) & train_data$Title=='Master')] <- master_train
train_data$Age[which(is.na(train_data$Age) & train_data$Title=='Rare Title')] <- rare_train
test_data$Age[which(is.na(test_data$Age) & test_data$Title=='Mr')] <- ((mr_train+mr_test)/2)
test_data$Age[which(is.na(test_data$Age) & test_data$Title=='Mrs')] <- ((mrs_train+mrs_test)/2)
test_data$Age[which(is.na(test_data$Age) & test_data$Title=='Miss')] <- ((miss_train+miss_test)/2)
test_data$Age[which(is.na(test_data$Age) & test_data$Title=='Master')] <- ((master_train+master_test)/2)
test_data$Age[which(is.na(test_data$Age) & test_data$Title=='Rare Title')] <- ((rare_train+rare_test)/2)
train_data$Embarked[c(62, 830)] <- 'C'
train_data$PassengerId <- NULL
test_data$PassengerId <- NULL
log_fit <- glm(Survived ~ Pclass + Fare + Parch +SibSp + Title + Embarked + Sex + Age, data = train_data, family = binomial)
prediction = predict(log_fit,test_data,type="response")
prediction
prediction_round.5 <- ifelse(prediction >0.6,1,0)
test <- read.csv('/home/venkat/Desktop/test.csv')
View(test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction_round.5)
print(solution)
write.csv(solution, file ='/home/venkat/Desktop/log_solution.csv', row.names = F)
summary(solution)
which(is.na(solution$Survived))
solution$Survived[which(is.na(solution$Survived))]
solution$Survived[which(is.na(solution$Survived))] <- 0
write.csv(solution, file ='/home/venkat/Desktop/log_solution.csv', row.names = F)
q()
summary(log_fit)
log_fit <- glm(Survived ~ Pclass + Fare + Parch +SibSp + Title + Embarked  + Age, data = train_data, family = binomial)
prediction = predict(log_fit,test_data,type="response")
prediction_round.5 <- ifelse(prediction >0.6,1,0)
prediction_round.5 <- ifelse(prediction >0.5,1,0)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction_round.5)
write.csv(solution, file ='/home/venkat/Desktop/log_solution1.csv', row.names = F)
summary(solution)
which(is.na(solution$Survived))
solution$Survived[which(is.na(solution$Survived))] <- 0
write.csv(solution, file ='/home/venkat/Desktop/log_solution1.csv', row.names = F)
summary(log_fit)
log_fit <- glm(Survived ~ Pclass + Parch +SibSp + Title + Embarked  + Age, data = train_data, family = binomial)
prediction = predict(log_fit,test_data,type="response")
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction_round.5)
summary(solution)
prediction_round.5 <- ifelse(prediction >0.5,1,0)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction_round.5)
summary(solution)
write.csv(solution, file ='/home/venkat/Desktop/log_solution2.csv', row.names = F)
summary(log_fit)
log_fit <- glm(Survived ~ Pclass + Parch +SibSp + Title   + Age, data = train_data, family = binomial)
prediction = predict(log_fit,test_data,type="response")
prediction_round.5 <- ifelse(prediction >0.5,1,0)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction_round.5)
summary(solution)
write.csv(solution, file ='/home/venkat/Desktop/log_solution3.csv', row.names = F)
prediction_round.6 <- ifelse(prediction >0.6,1,0)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction_round.6)
summary(solution)
write.csv(solution, file ='/home/venkat/Desktop/log_solution4.csv', row.names = F)
log_fit <- glm(Survived ~ Pclass + Fare + Parch +SibSp + Title + Embarked  + Age, data = train_data, family = binomial)
prediction = predict(log_fit,test_data,type="response")
prediction_round.6 <- ifelse(prediction >0.6,1,0)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction_round.6)
summary(solution)
solution$Survived[which(is.na(solution$Survived))] <- 0
write.csv(solution, file ='/home/venkat/Desktop/log_solution5.csv', row.names = F)
solution$Survived[which(is.na(solution$Survived))] <- 1
write.csv(solution, file ='/home/venkat/Desktop/log_solution5.csv', row.names = F)
log_fit <- glm(Survived ~ Pclass + Fare + Parch +SibSp + Title + Embarked  + Age, data = train_data, family = binomial)
prediction = predict(log_fit,test_data,type="response")
prediction_round.6 <- ifelse(prediction >0.6,1,0)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction_round.6)
summary(solution)
which(is.na(solution$Survived))
solution$Survived[which(is.na(solution$Survived))] <- 1
summary(solution)
write.csv(solution, file ='/home/venkat/Desktop/log_solution5.csv', row.names = F)
q()
load("/home/venkat/.RData")
