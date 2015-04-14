#====================GETTING FILE FROM THE CSV FILE====================
eyeMovements <- read.csv(file = "full_eyemovement_set.csv")
head(eyeMovements)
#====================DROPPING THE FIXATION PARAMETER====================
# This is use to remove the num_fixations column form the table as
# num_fixations are highly correlated 
library(dplyr)
# Function: select:dplyr
# Input: The data set
# Output: Returns the data set but with the removed columns given in the command
#         which in our case is the num_fixations
eyeMovements <- select(.data = eyeMovements, -(num_fixations))
head(eyeMovements)
#====================CHANGING THE COLUMN NAMES====================
# Used to change the column names as per the our preference

# "Duration.of.Fixation" = mean_fix_dur,
# "Horizontal.Dispersion" = disp_horz,
# "Vertical.Dispersion" = disp_vert,
# "Velocity.Horizontal" = peak_vel_horz,
# "Velocity.Vertical" = peak_vel_vert,
# "Condition" = condition,
# "SubjectID" = subj_id,
# "Trial.Number" = trial_num
# Function: rename:dplyr
# Input: The data set
# Output: The same data set renamed columns
eyeMovements <- rename(eyeMovements,
                       "Duration.of.Fixation" = mean_fix_dur,
                       "Horizontal.Dispersion" = disp_horz,
                       "Vertical.Dispersion" = disp_vert,
                       "Velocity.Horizontal" = peak_vel_horz,
                       "Velocity.Vertical" = peak_vel_vert,
                       "Condition" = condition,
                       "SubjectID" = subj_id,
                       "Trial.Number" = trial_num)
head(eyeMovements)
#====================MEAN CENTERING FUNCTION====================
# Function: center_scale:mean centering function using scale()
# Input: Values in term of Real Numbers List
# Output: Real Number for each columns

center_scale <- function(x) {
  scale(x, scale = FALSE)
}
#====================CREATE DATA AFTER CENTER FUNCTION=============
# Function: mutate:dplyr
# Input: The data frame and the columns to be changes
# Output: The data frame but after applying the function on the specified columns.
#         (Condition SubjectID Trial.Number) not these columns

eyeMovements<- mutate(eyeMovements,
                      Duration.of.Fixation = center_scale(Duration.of.Fixation),
                      Horizontal.Dispersion = center_scale(Horizontal.Dispersion),
                      Vertical.Dispersion = center_scale(Vertical.Dispersion),
                      Velocity.Horizontal = center_scale(Velocity.Horizontal),
                      Velocity.Vertical = center_scale(Velocity.Vertical))
head(eyeMovements)
#====================CHANGING RANDOM = 0 and READING = 1=============
# Change the levels of the conditins as Random  = 0, Reading = 1
eyeMovements$Condition <- ifelse(eyeMovements$Condition == "random",0,1)
head(eyeMovements,n = 10)
eyeMovements_2 <- eyeMovements
#====================DELETING SubjectID Trial.Number=============
eyeMovements_2 <- select(eyeMovements_2,-(SubjectID:Trial.Number))
head(eyeMovements_2)
#=========================COMBINING THE ROWS==========================
eyeMovements_2Length <- nrow(eyeMovements_2)
eyeMovements_2Length
realLenth <-seq(1,eyeMovements_2Length,by = 2)
realLenth

Duration.of.Fixation <- c()
Horizontal.Dispersion <- c()
Vertical.Dispersion <- c()
Velocity.Horizontal <- c()
Velocity.Vertical <- c()

eyeMovements_3 <- data.frame("Condition"= eyeMovements_2$Condition[realLenth])
eyeMovements_3

for( i in realLenth){
  Duration.of.Fixation <- c(Duration.of.Fixation,c(eyeMovements_2$Duration.of.Fixation[i] - eyeMovements_2$Duration.of.Fixation[i+1])) 
  Horizontal.Dispersion <- c(Horizontal.Dispersion,c(eyeMovements_2$Horizontal.Dispersion[i] - eyeMovements_2$Horizontal.Dispersion[i+1]))  
  Vertical.Dispersion <- c(Vertical.Dispersion,c(eyeMovements_2$Vertical.Dispersion[i] - eyeMovements_2$Vertical.Dispersion[i+1]))  
  Velocity.Horizontal <- c(Velocity.Horizontal,c(eyeMovements_2$Velocity.Horizontal[i] - eyeMovements_2$Velocity.Horizontal[i+1]))   
  Velocity.Vertical <- c(Velocity.Vertical,c(eyeMovements_2$Velocity.Vertical[i] - eyeMovements_2$Velocity.Vertical[i+1]))  
}
Duration.of.Fixation

eyeMovements_3$Duration.of.Fixation <- Duration.of.Fixation
eyeMovements_3$Horizontal.Dispersion <- Horizontal.Dispersion
eyeMovements_3$Vertical.Dispersion <- Vertical.Dispersion
eyeMovements_3$Velocity.Horizontal <- Velocity.Horizontal
eyeMovements_3$Velocity.Vertical <- Velocity.Vertical

## eyeMovements_3 will our final set of values which we will work on now
head(eyeMovements_3)
## Writing the files on a file.
write.csv(eyeMovements_3,file = "eyeMovement_3.csv",row.names = FALSE)

#=========================FIT THE GLM ON FULL MODEL======================
eyeMovements_3.GLM <- glm(Condition ~ Duration.of.Fixation + 
                            Horizontal.Dispersion + 
                            Vertical.Dispersion + 
                            Velocity.Horizontal + 
                            Velocity.Vertical,data = eyeMovements_3,family = binomial())
eyeMovements_3.GLM$coefficients
eyeMovements_3.GLM$residuals
eyeMovements_3.GLM$fitted.values
eyeMovements_3.GLM$effects
summary(eyeMovements_3.GLM)
eyeMovements_3.GLM$aic
plot(eyeMovements_3.GLM)

##=========================FINDING THE OUTLIER====================== 
# As we can see after plotting the values we come to know that the
# row 214 and 79 is an outlier and could be a Influential Observation

# Now we will remove the row and againg fit the model and compare the 
# parameters
eyeMovements_4 <- eyeMovements_3[-c(79,214),]
eyeMovements_4.GLM <- glm(Condition ~ Duration.of.Fixation + 
                            Horizontal.Dispersion + 
                            Vertical.Dispersion + 
                            Velocity.Horizontal + 
                            Velocity.Vertical,data = eyeMovements_4,family = binomial())
eyeMovements_4.GLM$coefficients
eyeMovements_4.GLM$residuals
eyeMovements_4.GLM$fitted.values
eyeMovements_4.GLM$effects
summary(eyeMovements_4.GLM)
eyeMovements_4.GLM$aic
plot(eyeMovements_4.GLM)

## As we can see that we have a considerable change in the coefficiens and a decrease in the AIC values 
## hence the rows were potential influential observation.
## THUS now we will work on eyeMovements_4

##======================DIVIDING THE TRAINING AND TEST SET======================
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
wholeData4 <- splitdata(eyeMovements_4,123)
eyeTraining4 <- wholeData4$trainset
eyeTest4 <- wholeData4$testset

# Writing the Training and Test into .csv Files
write.csv(eyeTraining4,file = "TrainingEye_4.csv",row.names = FALSE)
write.csv(eyeTest4,file = "TestEye_4.csv",row.names = FALSE)

##======================FIT THE GLM ON THE TRAINING SET======================
training.GLM <- glm(Condition ~ Duration.of.Fixation + 
                      Horizontal.Dispersion + 
                      Vertical.Dispersion + 
                      Velocity.Horizontal + 
                      Velocity.Vertical,data = eyeTraining4,family = binomial())
training.GLM$coefficients
training.GLM$residuals
training.GLM$fitted.values
training.GLM$effects
summary(training.GLM)
training.GLM$aic
plot(training.GLM)

##======================PREDICTING THE VALUES IN TEST SET======================
# Predicting the values in the Test data
# Create a variable of predicted classes from the test set based on the fixed parameter model
predict_training_glm <- round(predict(training.GLM,eyeTest4,type = "response"))
str(predict_training_glm)
predict_training_glm_continous <- predict(training.GLM,eyeTest4,type = "response")
predict_training_glm_continous

#============================GENERATING THE CONFUSION MATRIX===============================
# Generate the confusion matrix for the no random effects model
cMatrix <- confusion.matrix(eyeTest4$Condition,predict_training_glm,threshold = .95)

#============================CREATE THE ROCR CURVE===============================
#Create ROCR prediction and performance objects for the random effects model
pred_training <- prediction(predict_training_glm_continous,eyeTest4$Condition)
pred_training
performance_training <- performance(pred_training,"tpr","fpr")  
performance_training

# x and y values of the model
x <- unlist(performance_training@x.values)
y <- unlist(performance_training@y.values)

plot(performance_training)
lines(x,y)

#============================FITTING THE LASSO TO THE FULL MODEL===============================

# Fit the lasso model for the logistic regression using the file 
# saved in the folder.
source("LSA.R.txt")
lsa(training.GLM)
# This fit suggests that we the no predictable veriable should ne dropped from the table
# This means that our model is good and we dont need to fit the model using 
# lasso 



#============================CROSS===============================
aicFrame <- c()
aicFrame
for(i in 200:1000){
  splitdataWORK <- function(dataframe, seed=NULL){
    if (!is.null(seed)) set.seed(seed)
    index <- 1:nrow(dataframe)
    trainindex <- sample(index, trunc(length(index)*.80))
    trainset <- dataframe[trainindex, ]
    testset <- dataframe[-trainindex, ]
    list(trainset=trainset,testset=testset)
  }
  push1 <- splitdataWORK(eyeMovements_4,i)
  pushTraining <- push1$trainset
  pushTest <- push1$testset
  pushTraining.glm <- glm(Condition ~ Duration.of.Fixation + 
                            Horizontal.Dispersion + 
                            Vertical.Dispersion + 
                            Velocity.Horizontal + 
                            Velocity.Vertical,data = pushTraining,family = binomial())
  
  aicFrame <- rbind(aicFrame,pushTraining.glm$aic)
}
plot(aicFrame)
summary(pushTraining.glm)
































