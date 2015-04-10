#Create a variable of predicted classes from the test set based on the random parameter model
rand_predicted<-round(predict(model_rand,test_set,type="response"))
rand_predicted_continuous<-predict(model_rand,test_set,type="response")#libraries:
library(gdata)
library(SDMTools)
library(ROCR)
library(bestglm)
library(lars)
##Load the data:
full_eyemovement_set <- read.csv("~/Desktop/git_locals/StatisticsForBigData-/full_eyemovement_set.csv")

##Define a mean centering function:
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

##Center the variables this is needed for and create a new data frame
attach(full_eyemovement_set)
cent_num_fix<-center_scale(num_fixations)
cent_mean_fix<-center_scale(mean_fix_dur)
cent_disp_horz<-center_scale(disp_horz)
cent_disp_vert<-center_scale(disp_vert)
cent_peak_vel_horz<-center_scale(peak_vel_horz)
cent_peak_vel_vert<-center_scale(peak_vel_vert)
##Change the levels of condition
levels(condition)<-c(0,1)
##Change subjid to a factor
subj_id<-as.factor(subj_id)
trial_num<-trial_num
detach(full_eyemovement_set)

##Create a new, centered data frame
full_eyemovement_set_centered<-data.frame(cent_num_fix,cent_mean_fix,cent_disp_horz,cent_disp_vert,cent_peak_vel_horz,cent_peak_vel_vert,condition,subj_id,trial_num)
#Cleanup
keep(full_eyemovement_set_centered,sure=TRUE)

#Break this into an 80/20 split training & test sets
training_set<-full_eyemovement_set_centered[sample(nrow(full_eyemovement_set_centered),490),]
test_set<-full_eyemovement_set_centered[sample(nrow(full_eyemovement_set_centered),122),]
#A variable with classes adjusted to have "1" and "0" as values.
observed<-as.numeric(test_set$condition)-1

#Fit the model to the training set with no random effects:
model_norand<-glm(condition~cent_mean_fix+cent_disp_horz+cent_disp_vert+cent_peak_vel_horz+cent_peak_vel_vert,data=training_set,family=binomial)

##Lasso for logistic regression. Needs libraries lars, source textfile for LSA.
source("LSA.R.txt")
lsa(model_norand)

#Create a variable of predicted classes from the test set based on the fixed parameter model
norand_predicted<-round(predict(model_norand,test_set,type="response"))
norand_predicted_continuous<-predict(model_norand,test_set,type="response")

#Generate the confusion matrix for the no random effects model
confusion.matrix(observed,norand_predicted,threshold=0.95)

#Create ROCR prediction and performance objects for the random effects model
norand_pred<-prediction(norand_predicted_continuous,observed)
norand_perf<-performance(norand_pred,"tpr","fpr")

#Norand model x&y values
rand_xvalues<-unlist(rand_perf@x.values)
rand_yvalues<-unlist(rand_perf@y.values)

plot(rand_perf)
lines(rand_xvalues,rand_yvalues)

#Generate the confusion matrix for the random effects model
confusion.matrix(observed,rand_predicted,threshold=0.95)

#Random effects
model_rand<-glmer(condition~cent_mean_fix+cent_disp_horz+cent_disp_vert+cent_peak_vel_horz+cent_peak_vel_vert+trial_num+(1|subj_id),data=training_set,family=binomial,control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

rand_pred<-prediction(rand_predicted_continuous,observed)
rand_perf<-performance(rand_pred,"tpr","fpr")

