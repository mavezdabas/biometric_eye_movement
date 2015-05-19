# #Glm function on the full model
# #Function: glm
# #Input: The glm function will take the data frame and perform general linear model.
# #Output: The model after fitting the glm model using the
# family = binomial()
# data = eyeMovements_3
# Condition as Response variable
# Duration.of.Fixation as Predictable variable
# Horizontal.Dispersion as Predictable variable
# Vertical.Dispersion as Predictable variable
# Velocity.Vertical as Predictable variable
eyeMovements_3.GLM <- glm(Condition ~ Duration.of.Fixation + Horizontal.Dispersion +
                            Vertical.Dispersion +
                            Velocity.Horizontal +
                            Velocity.Vertical,data = eyeMovements_3,
                          family = binomial())
plot(eyeMovements_3.GLM)

# Splitting the data into training set and test set
splitdata <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)*.80))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}
wholeData4 <- splitdata(eyeMovements_4,123)
eyeTraining4 <- wholeData4$trainset
eyeTest4 <- wholeData4$testset

# Fitting the glm to the training set
# Function: glm
# Input: The glm function will take the training dataframe and perform genaralized linear model.
# Output: The model after fitting the glm model using the
# family = binomial()
# data = eyeTraining4
# Condition as Response variable
# Duration.of.Fixation as Predictable variable
# Horizontal.Dispersion as Predictable variable
# Vertical.Dispersion as Predictable variable
# Velocity.Vertical as Predictable variable
training.GLM <- glm(Condition ~ Duration.of.Fixation +
                      Horizontal.Dispersion +
                      Vertical.Dispersion +
                      Velocity.Horizontal +
                      Velocity.Vertical,data = eyeTraining4,family = binomial())
plot(training.GLM)

# Predicting the values of the test set
predict_training_glm <- round(predict(training.GLM,eyeTest4,type = "response"))
predict_training_glm_continous <- predict(training.GLM,eyeTest4,type = "response")

# Generate the confusion matrix
cMatrix <- confusion.matrix(eyeTest4$Condition,predict_training_glm,threshold = .95)
cAccuracy <- accuracy(eyeTest4$Condition,predict_training_glm,threshold = .95)
cAccuracy$AUC

# Create ROCR prediction and performance objects
pred_training <- prediction(predict_training_glm_continous,eyeTest4$Condition)
performance_training <- performance(pred_training,"tpr","fpr") 
x <- unlist(performance_training@x.values)
y <- unlist(performance_training@y.values)
plot(performance_training)
lines(x,y,col = "yellow")


# Fit the lasso model for the logistic regression
source("LSA.R.txt")
training.GLM.LASSO <- lsa(training.GLM)
training.GLM.LASSO
training.GLM.LASSO$beta.aic

# Fit glm after lasso
training.lasso.glm <- glm(Condition ~ Duration.of.Fixation+
                            Vertical.Dispersion,
                          data = eyeTraining4,family = binomial())
training.lasso.Summary <- summary(training.lasso.glm)

# Predicting the values in the Test data
predict_lasso_training_glm <- round(predict(training.lasso.glm,eyeTest4,type = "response"))
predict_lasso_training_glm_continous <- predict(training.lasso.glm,eyeTest4,type = "response")

# Generate the confusion matrix
cMatrixLasso <- confusion.matrix(eyeTest4$Condition,predict_lasso_training_glm,threshold = .95)
cAccuracyLasso <- accuracy(eyeTest4$Condition,predict_lasso_training_glm,threshold = .95)

# Create ROCR prediction and performance objects on lasso model
pred_lasso_training <- prediction(predict_lasso_training_glm_continous,eyeTest4$Condition)
performance_lasso_training <- performance(pred_lasso_training,"tpr","fpr") 
x.lasso <- unlist(performance_lasso_training@x.values)
y.lasso <- unlist(performance_lasso_training@y.values)
plot(performance_lasso_training)
lines(x,y,col = "red")


# Cross validation function
# Function: Converts the data frame into training set and the test set fit the glm model and extract the AIC and AUC values for the full model as well for the lasso model with different test and training set depending the value of the seed.
# Input: The entire Data frame
# Output: The AIC and AUC values of different training and test data data sets for Full and Lasso fit model
for(i in 200:1000){
  splitdataWORK <- function(dataframe, seed=NULL){
    if (!is.null(seed)) set.seed(seed)
    index <- 1:nrow(dataframe)
    trainindex <- sample(index, trunc(length(index)*.80))
    trainset <- dataframe[trainindex, ]
    testset <- dataframe[-trainindex, ]
    list(trainset=trainset,testset=testset)
  }
  # Entire Dataset
  push1 <- splitdataWORK(eyeMovements_4,i)
  # Extracting the Training Data
  pushTraining <- push1$trainset
  # extracting the Test Data
  pushTest <- push1$testset
  # Implementing the glm on the Training set
  pushTraining.glm <- glm(Condition ~ Duration.of.Fixation +
                            Horizontal.Dispersion +
                            Vertical.Dispersion +
                            Velocity.Horizontal +
                            Velocity.Vertical,data = pushTraining,family = binomial())
  # Saving the AIC value of each model into aicValue
  aicValue <- rbind(aicValue,pushTraining.glm$aic)
  
  # Predicting the values in the test data
  predict_training_glm <- round(predict(training.GLM,
                                        pushTest,
                                        type = "response"))
  # Measuring the accuracy.
  cAccuracy <- accuracy(pushTest$Condition,
                        predict_training_glm,
                        threshold = .95)
  
  # Saving the AUC value from each model into aucValue
  aucValue <- rbind(aucValue,cAccuracy$AUC)
  #===========================LASSO MODEL=============================
  
  pushTraining.glm.lasso <- glm(Condition ~ Duration.of.Fixation+
                                  Vertical.Dispersion,
                                data = pushTraining,family = binomial())
  # Saving the AIC value of each model into aicValueLasso
  aicValueLasso <- rbind(aicValueLasso,pushTraining.glm.lasso$aic)
  
  # Predicting the values in the test data from the lasso model
  predict_lasso_training_glm <- round(predict(pushTraining.glm.lasso,
                                              pushTest,
                                              type = "response"))
  
  # Getting the accureacy
  cAccuracyLasso <- accuracy(pushTest$Condition,
                             predict_lasso_training_glm,
                             threshold = .95)
  # Saving the AUC value from each model into aucValueLasso
  aucValueLasso <- rbind(aucValueLasso,cAccuracyLasso$AUC)
}