#============================TASK 1===============================
# Task it to clean the the data of the 
# random_eyemovement_output reading_eyemovement_output
# The file and removed colums 
head(random_eyemovement_output)
library(dplyr)
# Random Environment
random_Eye <- select(random_eyemovement_output,-(V1:V2),-(V9:V16))
head(random_Eye)
# Reading Environment
reading_Eye <- select(reading_eyemovement_output,-(V1:V2),-(V9:V16))
head(reading_Eye)
#============================TASK 2===============================
# Merge the values
reading_random_Eye <-  merge(random_Eye,reading_Eye,all = TRUE)
head(reading_random_Eye)

# Renaming the Column names as per the Task
reading_random_Eye <- rename(reading_random_Eye,
                             "Fixation Per Trial" = V3,
                             "Duration of Fixation" = V4,
                             "Horizontal Dispersion" = V5,
                             "Vertical Dispersion" = V6,
                             "Velocity Horizontal" = V7,
                             "Velocity Vertical" = V8,
                             "Condition" = condition)
# As the structre of the condition column is a factor
# 1 - Random, 2 -> Reading 
head(reading_random_Eye$Condition)

#============================TASK 3===============================
# Writing the final version to a .csv file in the local directory
write.csv(reading_random_Eye,file = "eyeMovements.csv",row.names = FALSE)

#============================TASK 4===============================
# Divid the data into Training set and Test set data in csv file
# Reading the new csv file that we created.
eyeMovements <- read.csv(file = "eyeMovements.csv")
head(eyeMovements)
# Using this function to create the Training and Test
# Training 80% and 20% to test
splitdata <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)*.80))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}
wholeData <- splitdata(eyeMovements,123)
eyeTraining <- wholeData$trainset
eyeTest <- wholeData$testset

#============================TASK 5===============================
# Writing the Training and Test into .csv Files
write.csv(eyeTraining,file = "TrainingEye.csv",row.names = FALSE)
write.csv(eyeTest,file = "TestEye.csv",row.names = FALSE)


















