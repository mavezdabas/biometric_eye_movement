install.packages("SDMTools")
install.packages("lars")
library(gdata)
library(SDMTools)
library(ROCR)
library(bestglm)
library(lars)
library(dplyr)
#====================GETTING FILE FROM THE CSV FILE====================
eyeMovements <- read.csv(file = "full_eyemovement_set.csv")
head(eyeMovements)
#====================DROPPING THE FIXATION PARAMETER====================
# This is use to remove the num_fixations column form the table as
# num_fixations are highly correlated 

#Just use NULL instead of dplyr, which throws an error -DR
eyeMovements$num_fixations <- NULL
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
eyeMovements <- dplyr::rename(eyeMovements,
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
# Deleting the Subject ID and the trial Number as we are not taking
# these two columns into our consideration
eyeMovements_2 <- select(eyeMovements_2,-(SubjectID:Trial.Number))
head(eyeMovements_2)
#=========================COMBINING THE ROWS==========================
# Creating a New Data Frame with eliminating the rows by subtracting
# consecutive rows.
# As Discussed we need to have a single row for each subject.
# we are considering only one trial in the data set

# This will tell us the length of the dataframe
eyeMovements_2Length <- nrow(eyeMovements_2)
eyeMovements_2Length
# These will be the rows which we need to be extracted after the 
# subtraction of the rows
realLenth <-seq(1,eyeMovements_2Length,by = 2)
realLenth

# Creating empty Character Vectors of each columsn which we need in
# the data frame
Duration.of.Fixation <- c()
Horizontal.Dispersion <- c()
Vertical.Dispersion <- c()
Velocity.Horizontal <- c()
Velocity.Vertical <- c()

# Creating a new data frame with Condition column which will as per
# the underlying condition 
eyeMovements_3 <- data.frame("Condition"= eyeMovements_2$Condition[realLenth])
eyeMovements_3

# Function: Subtracting each row which are assigned to a perticular
#           subject ID
# Input: The Rows which are needed to be used 
# Output: Appending the values to the above defined empty character vectors
#         
for( i in realLenth){
  Duration.of.Fixation <- c(Duration.of.Fixation,c(eyeMovements_2$Duration.of.Fixation[i] - eyeMovements_2$Duration.of.Fixation[i+1])) 
  Horizontal.Dispersion <- c(Horizontal.Dispersion,c(eyeMovements_2$Horizontal.Dispersion[i] - eyeMovements_2$Horizontal.Dispersion[i+1]))  
  Vertical.Dispersion <- c(Vertical.Dispersion,c(eyeMovements_2$Vertical.Dispersion[i] - eyeMovements_2$Vertical.Dispersion[i+1]))  
  Velocity.Horizontal <- c(Velocity.Horizontal,c(eyeMovements_2$Velocity.Horizontal[i] - eyeMovements_2$Velocity.Horizontal[i+1]))   
  Velocity.Vertical <- c(Velocity.Vertical,c(eyeMovements_2$Velocity.Vertical[i] - eyeMovements_2$Velocity.Vertical[i+1]))  
}

# Creating new Columns in the eyeMovement_3 data frame with the 
# column names as respectively
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
# Function: glm
# Input: The gilm function will take the data frame which we plan 
#        to get the glm function out from.
# Output: The model after fitting the glm model using the
#         family = binomial()
#         data = eyeMovements_3
#         Condition as Response variable
#         Duration.of.Fixation as Predictable variable
#         Horizontal.Dispersion as Predictable variable
#         Vertical.Dispersion as Predictable variable
#         Velocity.Vertical as Predictable variable
eyeMovements_3.GLM <- glm(Condition ~ Duration.of.Fixation + 
                            Horizontal.Dispersion + 
                            Vertical.Dispersion + 
                            Velocity.Horizontal + 
                            Velocity.Vertical,data = eyeMovements_3,family = binomial())
# Getting the Coefficients  
eyeMovements_3.GLM$coefficients
# Getting the Residuals
eyeMovements_3.GLM$residuals
# Getting the Fitted values
eyeMovements_3.GLM$fitted.values
# Getting the Effects
eyeMovements_3.GLM$effects
# Saving the summary of the glm object
eyeMovements_3.GLM.Summary <- summary(eyeMovements_3.GLM)
eyeMovements_3.GLM.Summary
# Getting the AIC value of the model
eyeMovements_3.GLM$aic
# Plot the glm object to finf the potential outliers and influential
# observations
png("outlier_detection.png")
par(mfrow=c(2,2))
for (i in 1:4)
  plot(eyeMovements_3.GLM,which=i)
dev.off()

##=========================FINDING THE OUTLIER====================== 
# As we can see after plotting the values we come to know that the
# row 214 and 79 is an outlier and could be a Influential Observation

# Now we will remove the row and againg fit the model and compare the 
# parameters

# Creating a new data frame with elininating the rows 79,214 
eyeMovements_4 <- eyeMovements_3[-c(79,214),]
head(eyeMovements_4)

# Function: glm
# Input: The glm function will take the data frame after eliminating
#        the rows to get the glm function out from.
# Output: The model after fitting the glm model using the
#         family = binomial()
#         data = eyeMovements_4
#         Condition as Response variable
#         Duration.of.Fixation as Predictable variable
#         Horizontal.Dispersion as Predictable variable
#         Vertical.Dispersion as Predictable variable
#         Velocity.Vertical as Predictable variable
eyeMovements_4.GLM <- glm(Condition ~ Duration.of.Fixation + 
                            Horizontal.Dispersion + 
                            Vertical.Dispersion + 
                            Velocity.Horizontal + 
                            Velocity.Vertical,data = eyeMovements_4,family = binomial())
# Getting the Coefficients
eyeMovements_4.GLM$coefficients
# Getting the Residuals
eyeMovements_4.GLM$residuals
# Getting the Fitted values
eyeMovements_4.GLM$fitted.values
# Getting the Effects
eyeMovements_4.GLM$effects
# saving the summary of the glm object
eyeMovements_4.GLM.Summary <- summary(eyeMovements_4.GLM)
# Extracting the AIC value of the model
eyeMovements_4.GLM$aic
# Plotting the glm object to take into account of potential outliers 
# which could be influential observations
png("outliers_removed.png")
par(mfrow=c(2,2))
for (i in 1:4)
  plot(eyeMovements_4.GLM,which=i)
dev.off()
# As we can see that we have a considerable change in the 
# coefficiens and a decrease in the AIC values 
# hence the rows were potential influential observation.
# THUS now we will work on eyeMovements_4

##======================DIVIDING THE TRAINING AND TEST SET======================
# Now the task is to divide the data set into training set and the test set
# which is done by the function defined previously in Cleaning.R
# Training 80% and 20% to test
# Function: (splitdata) Splitting the dataset into training and test
#           set
# Input: The data set which is to be divided and the seed value
# Output: List of Training set as well as the test set. With training
#         being 80% of the total data set

splitdata <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)*.80))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}
# Save the List from the splitdata Function to wholeData4 with seed 123
wholeData4 <- splitdata(eyeMovements_4,123)
# Extracting the Training set from the wholeData4
eyeTraining4 <- wholeData4$trainset
# Extracting the Test set from the wholeData4
eyeTest4 <- wholeData4$testset

# Writing the Training and Test into .csv Files
write.csv(eyeTraining4,file = "TrainingEye_4.csv",row.names = FALSE)
write.csv(eyeTest4,file = "TestEye_4.csv",row.names = FALSE)

##======================FIT THE GLM ON THE TRAINING SET======================
# Function: glm
# Input: The glm function will take the training dataframe.
# Output: The model after fitting the glm model using the
#         family = binomial()
#         data = eyeTraining4
#         Condition as Response variable
#         Duration.of.Fixation as Predictable variable
#         Horizontal.Dispersion as Predictable variable
#         Vertical.Dispersion as Predictable variable
#         Velocity.Vertical as Predictable variable
training.GLM <- glm(Condition ~ Duration.of.Fixation + 
                      Horizontal.Dispersion + 
                      Vertical.Dispersion + 
                      Velocity.Horizontal + 
                      Velocity.Vertical,data = eyeTraining4,family = binomial())
# Coefficients 
training.GLM$coefficients
# Residuals
training.GLM$residuals
# Fitted Values
training.GLM$fitted.values
# Effects
training.GLM$effects
# Saving the training GLM object to training.GLM.Summary
training.GLM.Summary <- summary(training.GLM)
# Extracting the AIC value of the model
training.GLM$aic
# Plot the training.glm object
plot(training.GLM)

##======================PREDICTING THE VALUES IN TEST SET======================
# Predicting the values in the Test data
# Create a variable of predicted classes from the test set based on
#   the fixed parameter model
predict_training_glm <- round(predict(training.GLM,eyeTest4,type = "response"))
str(predict_training_glm)
predict_training_glm_continous <- predict(training.GLM,eyeTest4,type = "response")
predict_training_glm_continous

#============================GENERATING THE CONFUSION MATRIX===============================
# Generate the confusion matrix
cMatrix <- confusion.matrix(eyeTest4$Condition,predict_training_glm,threshold = .95)
cMatrix
# Getting the accureacy
cAccuracy <- accuracy(eyeTest4$Condition,predict_training_glm,threshold = .95)
cAccuracy$AUC
#============================CREATE THE ROCR CURVE===============================
#Create ROCR prediction and performance objects
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
training.GLM.LASSO <- lsa(training.GLM)
training.GLM.LASSO$beta.aic
# This fit suggests that we the no predictable veriable should ne 
# dropped from the table.
# This means that our model is good and we dont need to fit 
# the model using lasso 

#============================CROSS VALIDATION===============================
# Creating an empty character vector which will be used to save AIC values 
aicValue <- c()
aicValue

# Creating an empty character vector which will be used to save AUC values
aucValue <- c()
aucValue

# Function: Concerts the data frame into training set and the test set
#           fit the glm model and extract the AIC and AUC values 
#           for different test and training set depending the value
#           of the seet.
#           seet is initially set to i whih\ch in this case will
#           iterate from 200 -> 1000
# Input: The entire Data frame
# Output: The AIC and AUC values of different training and test data 
#         data sets

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
  predict_training_glm <- round(predict(training.GLM,eyeTest4,type = "response"))
  # Measuring the accuracy.
  cAccuracy <- accuracy(eyeTest4$Condition,predict_training_glm,threshold = .95)
  
  # Saving the AUC value from each model into aucValue
  aucValue <- rbind(aucValue,cAccuracy$AUC)
  
}
head(aicValue)
head(aucValue)





























