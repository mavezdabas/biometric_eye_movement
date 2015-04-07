#Install needed libraries:
install.packages("CircStats")
install.packages("saccades")
library(CircStats)
library(saccades)
library(plyr)
library(dplyr)

# This is the path of the files in the system
# This value will change depending upon system to system.
#directory.dummy <- "/Users/mavezsinghdabas/StatisticsForBigData-/eyemovement_data/BioEye2015_DevSets/RAN_30min_dv"

#On DR's work system:
#directory.dummy <- "/Users/Dylan/Desktop/git_locals/StatisticsForBigData-/eyemovement_data/BioEye2015_DevSets/RAN_30min_dv"
#On DR's home system:
random_files <- "/Users/dylanrose/Desktop/StatisticsForBigData-/eyemovement_data/BioEye2015_DevSets/RAN_30min_dv"
reading_files<- "/Users/dylanrose/Desktop/StatisticsForBigData-/eyemovement_data/BioEye2015_DevSets/TEX_30min_dv"

#Get all the names in the data folder
random_files_list<-list.files(random_files,full.names=TRUE)
reading_files_list<-list.files(reading_files,full.names=TRUE)

#Define a function to parse the name of text files of data and return subject/trial/condition information
#from the filename.
#subject_information<-function(text_file){
#  split_name <- unlist(strsplit(text_file,"_"))
#  subject <- split_name[6]
#  trial_num <- substr(split_name[7],1,1)
#  subject_trial_info<-c(subject,trial_num)
#  return(subject_trial_info)
#}

#Define a function that will parse a table of eye movement position data and identify saccades.
detect.saccades <- function(samples, lambda, smooth.saccades) {
  # Calculate horizontal and vertical velocities:
  vx <- stats::filter(samples$x, -1:1/2)
  vy <- stats::filter(samples$y, -1:1/2)
  # We don't want NAs, as they make our life difficult later
  # on. Therefore, fill in missing values:
  vx[1] <- vx[2]
  vy[1] <- vy[2]
  vx[length(vx)] <- vx[length(vx)-1]
  vy[length(vy)] <- vy[length(vy)-1]
  msdx <- sqrt(median(vx**2, na.rm=T) - median(vx, na.rm=T)**2)
  msdy <- sqrt(median(vy**2, na.rm=T) - median(vy, na.rm=T)**2)
  radiusx <- msdx * lambda
  radiusy <- msdy * lambda
  sacc <- ((vx/radiusx)**2 + (vy/radiusy)**2) > 1
  if (smooth.saccades) {
    sacc <- stats::filter(sacc, rep(1/3, 3))
    sacc <- as.logical(round(sacc))
  }
  samples$saccade <- ifelse(is.na(sacc), F, sacc)
  samples$vx <- vx
  samples$vy <- vy
  samples
}

#Define a function that takes a text file as input and drops unnecessary columns, and renames 
#columns of interest. Adds these values to new data frame and returns it. 
data_load_and_cleanup<-function(text_file){
  data <- data.frame(read.table(text_file,header = TRUE,fill = TRUE))
  data <- data[c(-5,-6,-7,-8,-9,-10)]
  colnames(data) <- c("sample","x_degree","y_degree","validity")
  return(data)
}

#Define a formula to convert degrees of visual angle to pixels.
#This formula will return a value which is the number of pixels away 
#from the center of the display, which will then need to be adjusted using the second formula in this script.
pixel_conversion_formula<-function(data){
  #Define variables needed for the calculation:
  d=55 #Distance between viewers and monitor in cm. Don't modify this.
  h=297 #Monitor height in cm. Don't modify this.
  r=1050 # Vertical resolution of the monitor. Don't modify this.
  #Calculate the number of pixels per degree of visual angle:
  deg_per_pix<-deg(atan2(.5*h,d)/(.5*r))
  #Calculate the size of data object in pixels instead of degrees
  size_in_pix<-data/deg_per_pix
  return(size_in_pix)
}

#Takes pairs of x & y pixel values and shifts their origins to the upper
#left hand corner of the monitor. Works by adding a constant to the pixel coordinate value for each dimension that re-centers them with respect to the upper-left corner of the display treated as the origin (0,0).
convert_to_origin_formula_x <- function(x){
  original_horizontal_coordinate<-840 # The value of the pixel at the horizontal locaiton of the original origin for this data. Don't modify this.
  con_hori_crd<-x+original_horizontal_coordinate #Adds the re-centering adjustment to the horizontal value in the input tuple.
  return(con_hori_crd) # Generate a tuple of converted coordinate values.
  
}

convert_to_origin_formula_y <- function(y){
  original_vertical_coordinate<-525 # The value of the pixel at the vertical location of the original origin for this data. Don't modify this.
  con_verti_crd<-y+original_vertical_coordinate #Adds the re-centering adjustment to the vertical value in the input tuple.
  return(con_verti_crd) # Generate a tuple of converted coordinate values.
  
}


#A wrapper for the conversion and re-centering formulas using APPLY functions for speed
coordinate_transformation_origin_recentering<-function(data_frame){
  pixel_terms_x <- unlist(lapply(data_frame$x_degree,pixel_conversion_formula))
  pixel_terms_y <- unlist(lapply(data_frame$y_degree,pixel_conversion_formula))
  recentered_x <- unlist(lapply(pixel_terms_x,convert_to_origin_formula_x))
  recentered_y <- unlist(lapply(pixel_terms_y,convert_to_origin_formula_y))
  
  #Generate a "trial" column (a list of 1's given the design of our analyses to this point)
  trial<-unlist(rep(1,length(recentered_x)))
  trial<-as.factor(trial)
  
  output<-data.frame(cbind(data_frame$sample,recentered_x,recentered_y,trial))
  colnames(output)<-c("time","x","y","trial")
  return(output)
}

#A wrapper function that calls all functions defined to this point on a single text file and returns eye movement data
preprocessing_wrapper<-function(text_file){
  #subject_trial_info<-subject_information(text_file)
  cleaned_data<-data_load_and_cleanup(text_file)
  cleaned_transformed_data<-coordinate_transformation_origin_recentering(cleaned_data)
  return(cleaned_transformed_data)
}

#Takes cleaned/pre-processed data and returns eye movement data sets
eyemovement_wrapper<-function(cleaned_transformed_data){
  fixation_data<-detect.fixations(cleaned_transformed_data)
  fixation_data_summary<-calculate.summary(fixation_data)
  eyemovement_means<-as.numeric((fixation_data_summary[,1]))
  eyemovement_sd<-as.numeric((fixation_data_summary[,2]))
  fixation_data_summary<-c(eyemovement_means,eyemovement_sd)
  #saccade_data<-detect.saccades(cleaned_transformed_data,lambda=15,smooth.saccades = FALSE)
  return(fixation_data_summary)
}

#Final wrapper; contains all functions previously defined -- intended to be passed to an APPLY function
final_wrapper<-function(text_file){
  #subject_info<-subject_information(text_file)
  cleaned_transformed_data<-preprocessing_wrapper(text_file)
  eyemovement_data<-eyemovement_wrapper(cleaned_transformed_data)
  return(eyemovement_data)
}

####With these functions defined, generate the actual data for the experiment using a faster APPLY method on all of the potential data files######
random_eyemovement_data_list<-lapply(random_files_list,final_wrapper)
random_eyemovement_output<-as.data.frame(do.call(rbind,random_eyemovement_data_list))
condition<-rep("random",length(random_eyemovement_output$V1))
random_eyemovement_output<-cbind(random_eyemovement_output,condition)
reading_eyemovement_data_list<-lapply(reading_files_list,final_wrapper)
reading_eyemovement_output<-as.data.frame(do.call(rbind,reading_eyemovement_data_list))
condition<-rep("reading",length(random_eyemovement_output$V1))
reading_eyemovement_output<-cbind(reading_eyemovement_output,condition)
