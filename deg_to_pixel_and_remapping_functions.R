#Install needed libraries:
library(CircStats)


#Define a formula to convert degrees of visual angle to pixels.
#This formula will return a value which is the number of pixels away 
#from the center of the display, which will then need to be adjusted using the second formula in this script.
pixel_conversion_formula<-function(data,deg_per_pix){
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
convert_to_origin_formula<-function(x,y){
  original_horizontal_coordinate<-840 # The value of the pixel at the horizontal locaiton of the original origin for this data. Don't modify this.
  original_vertical_coordinate<-525 # The value of the pixel at the vertical location of the original origin for this data. Don't modify this.
  converted_horizontal_coordinate<-x+original_horizontal_coordinate #Adds the re-centering adjustment to the horizontal value in the input tuple.
  converted_vertical_coordinate<-y+original_vertical_coordinate #Adds the re-centering adjustment to the vertical value in the input tuple.
  output<-cbind(converted_horizontal_coordinate,converted_vertical_coordinate) # Generate a tuple of converted coordinate values.
  return(output)
}