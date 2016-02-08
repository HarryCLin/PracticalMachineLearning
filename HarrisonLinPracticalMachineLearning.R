# Harrison Lin
# Practical Machine Learning Project
  
  library(forecast)
  library(caret)
  library(ggplot2)
  
  data.Path <- "D:/Cloud/Google Drive/Coursera/Practical Machine Learning/Project/"
  
  data.file.training <- paste(data.Path, "pml-training.csv", sep='')
  data.file.testing <- paste(data.Path, "pml-testing.csv", sep='')
  
  # #####################################################################
  # Data Processing
  # #####################################################################
  # training data
  data.training = read.csv(data.file.training)
  head(data.training)
  colnames(data.training)
  # dependent variable:  classe
  table(data.training$classe)
  
  # testing data
  data.testing = read.csv(data.file.testing)
  head(data.testing)
  colnames(data.testing)
  
  
  # ######################################################################
  # Exploring the training data:
  # ######################################################################
  summary(data.training)

  
  # All features
  #featurePlot(x=data.training,
  #            y=data.training$classe, plot="pairs")
  # Too slow
  
  
  # Drilling down to variables:
  # 
  # roll_belt
  # pitch_belt
  # yaw_belt
  
  featurePlot(x=data.training[, c("roll_belt", "pitch_belt", "yaw_belt", "classe")],
              y=data.training[,c("classe")], plot="pairs")
  # => Belt has some correlations!
  
  qplot(roll_belt, classe, data=data.training)
  qplot(pitch_belt, classe, data=data.training)
  qplot(yaw_belt, classe, data=data.training)
  
  # roll_arm
  # pitch_arm
  # yaw_arm
  
  featurePlot(x=data.training[, c("roll_arm", "pitch_arm", "yaw_arm", "classe")],
              y=data.training$classe, plot="pairs")
  # => Arm has no significant correlations to classe!
  
  # roll_dumbbell
  # pitch_dumbbell
  # yaw_dumbbell
  
  featurePlot(x=data.training[, c("roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell", "classe")],
              y=data.training$classe, plot="pairs")
  # => Hence Dumbbell has no significant correlation to classe dependent variable
  
  # roll_forearm
  # pitch_forearm
  # yaw_forearm
  
  featurePlot(x=data.training[, c("roll_forearm", "pitch_forearm", "yaw_forearm", "classe")],
              y=data.training$classe, plot="pairs")
  
  # => Hence forearm variables have no significant correlation to classe dependent variable
  
  
  
  # Add Regression smoothers
  # roll_belt
  qq1 <- qplot(roll_belt, classe, colour= roll_arm,data=data.training)
  qq1 + geom_smooth(method='lm', formula=y~x)
  
  # pitch_belt
  qplot(pitch_belt, classe, data=data.training)
  
  # yaw_belt
  qplot(yaw_belt, classe, data=data.training)
  

  # ##################################################################################
  # GLM
  # ##################################################################################
  model.glm.fit <- train(classe ~ roll_belt + pitch_belt + yaw_belt, data=data.training, method="glm")
  
    
  set.seed(1234)
  
  # ##################################################################################
  # Trees
  # ##################################################################################
  model.tree.fit <- train(classe ~ roll_belt + pitch_belt + yaw_belt, data=data.training, method="rpart")
  print(model.tree.fit$finalModel)
  model.tree.fit$results
  
  plot(model.tree.fit$finalModel, uniform=TRUE, main="Classification Tree")
  text(model.tree.fit$finalModel, use.n=TRUE, all=TRUE, cex=0.8)
  
  predict(model.tree.fit, newdata=data.testing)
  
  
  # ##################################################################################
  # Random Forests
  # ##################################################################################
  model.randomForest.fit <- train(classe ~ roll_belt + pitch_belt + yaw_belt, data=data.training, method="rf")
  print(model.randomForest.fit)
  #getTree(model.randomForest.fit$finalModel)
  
  model.randomForest.predict <- predict(model.randomForest.fit, data.testing)
  print(model.randomForest.predict)
  
  
  # ##################################################################################
  # Boosting
  # ##################################################################################
  model.boosting.fit <- train(classe ~ roll_belt + pitch_belt + yaw_belt, data=data.training, method="gbm", verbose=FALSE)
  print(model.boosting.fit)
  model.boosting.fit$results
  
  # model.boosting.fit$finalModel$train.error
  
  model.boosting.predict <- predict(model.boosting.fit, data.testing)
  print(model.boosting.predict)
  
  

  
  
