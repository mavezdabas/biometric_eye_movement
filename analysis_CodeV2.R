#============================TASK 1====================================
# Loading the data form my local to R. Using 
# My file name is eyeMovements.csv
eyeMovements <- read.csv(file = "eyeMovements.csv")
head(eyeMovements)
#============================TASK 2====================================
# Installing the libraries required
library(gdata)
install.packages("SDMTools")
library(SDMTools)
library(ROCR)
library(bestglm)
install.packages("lars")
library(lars)
#--------------------------Mean Centering Fucnction-------------------
# Define a mean centering function:
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

#--------------------------Centering The Variables-------------------
# Center the variables this is needed for and create a new data frame
attach(eyeMovements)
library(dplyr)
eyeMovementsScale <- mutate(eyeMovements,
                            Fixation.Per.Trial = center_scale(Fixation.Per.Trial),
                            Duration.of.Fixation = center_scale(Duration.of.Fixation),
                            Horizontal.Dispersion = center_scale(Horizontal.Dispersion),
                            Vertical.Dispersion = center_scale(Vertical.Dispersion),
                            Velocity.Horizontal = center_scale(Velocity.Horizontal),
                            Velocity.Vertical = center_scale(Velocity.Vertical))
#Condition = levels(Condition))

head(eyeMovementsScale)
# Change the levels of the conditins as Random  = 0, Reading = 1
eyeMovementsScale$Condition <- ifelse(eyeMovementsScale$Condition == "random",0,1)
detach(eyeMovements)
# This command is used to clean the environment and just use the
# eyeMovementsScale as this is the dataset that we will be working on.
keep(eyeMovementsScale,sure = TRUE)

#============================TASK 3====================================
# Now the task is to divide the data set into training set and the test set
# which is done by the function defined previously in Cleaning.R
# Training 80% and 20% to test
splitdata <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)*.80))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}
wholeDataScale <- splitdata(eyeMovementsScale,123)
eyeTrainingScale <- wholeDataScale$trainset
eyeTestScale <- wholeDataScale$testset

#============================TASK 4===============================
# Writing the Training and Test into .csv Files
write.csv(eyeTrainingScale,file = "TrainingEye_Scale.csv",row.names = FALSE)
write.csv(eyeTestScale,file = "TestEye_Scale.csv",row.names = FALSE)


#============================TASK 5===============================
# Fitting the model to the trainig set with no Random effect
# As we know that we are only considering the lasso test.
attach(eyeTrainingScale)
head(eyeTrainingScale)
training_glm <- glm(Condition ~ Fixation.Per.Trial +
                      Duration.of.Fixation + 
                      Horizontal.Dispersion + 
                      Vertical.Dispersion + 
                      Velocity.Horizontal + 
                      Velocity.Vertical,data = eyeTrainingScale,family = binomial())
detach(eyeTrainingScale)
head(training_glm$coefficients)
head(training_glm$residuals)
head(training_glm$fitted.values)
head(training_glm$effects)
summary(training_glm)
# The AIC Value is 14
plot(training_glm)
#============================TASK 6===============================
# Fit the lasso model for the logistic regression using the file 
# saved in the folder.
source("LSA.R.txt")
lsa(training_glm)
# This fit suggests that we the Velocity.Vertical can be dropped form
# the model.

#============================TASK 7===============================
# Predicting the values in the Test data
# Create a variable of predicted classes from the test set based on the fixed parameter model
predict_training_glm <- round(predict(training_glm,eyeTestScale,type = "response"))
str(predict_training_glm)
predict_training_glm_continous <- predict(training_glm,eyeTestScale,type = "response")
predict_training_glm_continous

#============================TASK 8===============================
# Generate the confusion matrix for the no random effects model
confusion.matrix(eyeTestScale$Condition,predict_training_glm,threshold = .95)


#============================TASK 9===============================
#Create ROCR prediction and performance objects for the random effects model
pred_training <- prediction(predict_training_glm_continous,eyeTestScale$Condition)
pred_training
performance_training <- performance(pred_training,"tpr","fpr")  
performance_training

# x and y values of the model
x <- unlist(performance_training@x.values)
y <- unlist(performance_training@y.values)

plot(performance_training)
lines(x,y)


#============================WITH LASSO MODEL==========================================

training_glm_lasso <- glm(Condition ~ Fixation.Per.Trial +
                            Duration.of.Fixation + 
                            Horizontal.Dispersion + 
                            Vertical.Dispersion + 
                            Velocity.Horizontal,data = eyeTrainingScale,family = binomial())
head(training_glm_lasso$coefficients)
head(training_glm_lasso$residuals)
head(training_glm_lasso$fitted.values)
head(training_glm_lasso$effects)
summary(training_glm_lasso)
# The AIC value is 12
# When we set to compare the values of the AIC we get to know that
# the model picked with Lasso is better.
plot(training_glm_lasso)



#============================TASK 7===============================
# Predicting the values in the Test data
# Create a variable of predicted classes from the test set based on the fixed parameter model
predict_training_glm_lasso <- round(predict(training_glm_lasso,eyeTestScale,type = "response"))
str(predict_training_glm_lasso)
predict_training_glm_continous_lasso <- predict(training_glm_lasso,eyeTestScale,type = "response")
predict_training_glm_continous

#============================TASK 8===============================
# Generate the confusion matrix for the no random effects model
confusion.matrix(eyeTestScale$Condition,predict_training_glm_lasso,threshold = .95)


#============================TASK 9===============================
#Create ROCR prediction and performance objects for the random effects model
pred_training_lasso <- prediction(predict_training_glm_continous_lasso,eyeTestScale$Condition)
pred_training_lasso
performance_training_lasso <- performance(pred_training_lasso,"tpr","fpr")  
performance_training_lasso

# x and y values of the model
x_lasso <- unlist(performance_training_lasso@x.values)
y_lasso <- unlist(performance_training_lasso@y.values)

plot(performance_training_lasso)
lines(x_lasso,y_lasso)

#XXXXXXXXXXXXXXXXX#XXXXXXXXXXXXXXXXX#XXXXXXXXXXXXXXXXX#XXXXXXXXXXXXXXX

#============================RANDOM MIXED EFFECT===============================

# Reading the file from the full_eyemovement_set.csv file 
effectEyeMovements <- read.csv(file = "full_eyemovement_set.csv")
head(effectEyeMovements)

# Renaming the Columns
effectEyeMovements <- rename(effectEyeMovements,
                             "Fixation.Per.Trial" = num_fixations,
                             "Duration.of.Fixation" = mean_fix_dur,
                             "Horizontal.Dispersion" = disp_horz,
                             "Vertical.Dispersion" = disp_vert,
                             "Velocity.Horizontal" = peak_vel_horz,
                             "Velocity.Vertical" = peak_vel_vert,
                             "Condition" = condition,
                             "SubjectID" = subj_id,
                             "Trial.Number" = trial_num)
head(effectEyeMovements)

# Center the variables this is needed for and create a new data frame
effectEyeMovements<- mutate(effectEyeMovements,
                            Fixation.Per.Trial = center_scale(Fixation.Per.Trial),
                            Duration.of.Fixation = center_scale(Duration.of.Fixation),
                            Horizontal.Dispersion = center_scale(Horizontal.Dispersion),
                            Vertical.Dispersion = center_scale(Vertical.Dispersion),
                            Velocity.Horizontal = center_scale(Velocity.Horizontal),
                            Velocity.Vertical = center_scale(Velocity.Vertical))
head(effectEyeMovements)
# Change the levels of the conditins as Random  = 0, Reading = 1
effectEyeMovements$Condition <- ifelse(effectEyeMovements$Condition == "random",0,1)
head(effectEyeMovements)

# Now the task is to divide the data set into training set and the test set
# which is done by the function defined previously in Cleaning.R
# Training 80% and 20% to test and from the function "splitdata" 
# defined above
effectEyeWholeData <- splitdata(effectEyeMovements,123)
effectTrainingScale <- effectEyeWholeData$trainset
effectTestScale <- effectEyeWholeData$testset
head(effectTrainingScale)
head(effectTestScale)

# Writing the Training and Test into .csv Files for random effect
write.csv(effectTrainingScale,file = "TrainingEye_Effect.csv",row.names = FALSE)
write.csv(eyeTestScale,file = "TestEye_Effect.csv",row.names = FALSE)

install.packages("lme4")
library(lme4)

# Fitting the Model which we got after fittng the lasso model into 
# the dataset
effect_training_glmer <- glmer(Condition ~ Fixation.Per.Trial +
                                 Duration.of.Fixation + 
                                 Horizontal.Dispersion + 
                                 Vertical.Dispersion + 
                                 Velocity.Horizontal+
                                 Trial.Number+
                                 #(1 | SubjectID)+
                                 (1 | Trial.Number : SubjectID),data = effectTrainingScale,family = binomial,
                               control = glmerControl(optimizer = "bobyqa"),
                               nAGQ = 10)

# 
# #rand_predicted<-round(predict(model_rand,test_set,type="response"))
# #rand_predicted_continuous<-predict(model_rand,test_set,type="response")#libraries
# 
# 
# 
# rand_pred<-prediction(rand_predicted_continuous,observed)
# rand_perf<-performance(rand_pred,"tpr","fpr")



















