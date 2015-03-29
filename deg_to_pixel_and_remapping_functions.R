#Install needed libraries:
library(CircStats)


#Define a formula to convert degrees of visual angle to pixels.
#This formula will return a value which is the number of pixels away 
#from the center of the display, which
#will then need to be adjusted using the second formula in this script.
pixel_conversion_formula<-function(data,deg_per_pix){
  #Define variables needed for the calculation:
  d=55 #Distance between viewers and monitor in cm
  h=297 #Monitor height in cm
  r=1050 # Vertical resolution of the monitor
  #Calculate the number of pixels per degree:
  deg_per_pix<-deg(atan2(.5*h,d)/(.5*r))
  size_in_pix<-data/deg_per_pix
  return(size_in_pix)
}

#Takes pairs of x & y pixel values and shifts their origins to the upper
#left hand corner of the monitor
convert_to_origin_formula<-function(x,y){
  original_horizontal_coordinate<-840 # The value of the pixel at the horizontal locaiton of the original origin for this data
  original_vertical_coordinate<-525 # The value of the pixel at the vertical location of the original origin for this data
  converted_horizontal_coordinate<-x+original_horizontal_coordinate
  converted_vertical_coordinate<-y+original_vertical_coordinate
  output<-cbind(converted_horizontal_coordinate,converted_vertical_coordinate) # Return a tuple of converted coordinate values
  return(output)
}