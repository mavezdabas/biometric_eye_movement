
# This is the path of the files in the system
# This value will change depending upon system to system.
directory.dummy <- "/Users/mavezsinghdabas/StatisticsForBigData-/eyemovement_data/BioEye2015_DevSets/RAN_30min_dv"

# Function to get all the files in the folder and removing all the unwanted
# data from the files.

completeFile <- function(directory,id = 1:306){
  files_list <- list.files(directory,full.names = TRUE)
  dat <- data.frame()
  for( i in id){
    dat <- rbind(dat,read.table(files_list[i],header = TRUE,fill = TRUE))
  }
  dat<- dat[c(-5,-6,-7,-8,-9,-10)]
  colnames(dat) <- c("SAMPLE","X.DEGREE","Y.DEGREE","VALIDITY")
  dat
}

#To collobrate the whole data set togeather.
#This funciton takes time so for now we will work on a subset
#dataEye_1 <- completeFile(directory.dummy,id = 1:306)
#copydataEye_1 <- dataEye_1

#This is a subset of the with first 50 files
dataVersion_1 <- completeFile(directory.dummy,id = 1:306)

#Function defined as pixel_conversion_formula
#Input: Degree X and Y from the specified datasets
#Output: number of pixels away from the center of the display
#Function: pixel_conversion_formula<-function(data)
#Creating new column X.PIXEL and Y.PIXEL in the dataset
dataVersion_1$X.PIXEL <- pixel_conversion_formula(dataVersion_1$X.DEGREE)
dataVersion_1$Y.PIXEL <- pixel_conversion_formula(dataVersion_1$Y.DEGREE)
head(dataVersion_1)
tail(dataVersion_1)
#Function defined as convert_to_origin_formula
#Input: Takes pairs of x & y pixel values from the dataset which we
#calculated using the function 1
#Output: Shifts their origins to the upper left hand corner of the
#monitor. Works by adding a constant to the pixel coordinate 
#value for each dimension that re-centers them with respect to the 
#upper-left corner of the display treated as the origin (0,0).
dataVersion_1$X_original <- convert_to_origin_formula_x(dataVersion_1$X.PIXEL)
dataVersion_1$Y_original <- convert_to_origin_formula_y(dataVersion_1$Y.PIXEL)



















=======================NOT To be EXECUTED==========================
# dataVersion_2 <- completeFile(directory.dummy,id = 51:100)
# dataVersion_3 <- completeFile(directory.dummy,id = 101:150)
# dataVersion_4 <- completeFile(directory.dummy,id = 151:200)
# dataVersion_5 <- completeFile(directory.dummy,id = 201:250)
# dataVersion_6 <- completeFile(directory.dummy,id = 251:300)
# dataVersion_7 <- completeFile(directory.dummy,id = 301:306)




































