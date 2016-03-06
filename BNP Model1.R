######################################################
#BNP Kaggle competition
#Model 1
#Predicting whether a claim is suitable for accelerated
#approval (0 or 1 outcome variable)
#
#######################################################

#rename function
library(plyr)
library(gbm)
#neural nets
library(h2o)
library(extraTrees)


#xgboost
library(DiagrammeR)
library(Ckmeans.1d.dp)
library(xgboost)
library(methods)
library(data.table)
library(magrittr)






#import datasets
train <- read.csv('C:\\Users\\Randy\\Downloads\\Kaggle BNP\\trainAll.csv')
test <- read.csv('C:\\Users\\Randy\\Downloads\\Kaggle BNP\\testAll.csv')


#sets the working directory
setwd('C:/Users/Randy/Downloads/Kaggle BNP/ML3')

#Includes functions in the BNP Function script
source("BNP functions.R")


#calls the split function to divide the train dataset
bothFrames = split(train, .8)
train2 = bothFrames[[1]]
test2 = bothFrames[[2]]

train2[is.na(train2)] = -1
test2[is.na(test2)] = -1




#removing variables
train2 = train2[,-c(19,60,65,109,117)]
test2 = test2[,-c(19,60,65,109,117)]



#runs deep learning model
myout2 = deepL(train2 , test2,explan) 

#runs gbm model
output = gbmParse(train2,test2)


#runs extra trees model
etOut = extraTreesParse(train2[,c(1,2,134:ncol(train2))],test2[,c(1,2,134:ncol(test2))])



################################################################
#	implementation of extreme gradient boosting algorithms(xgboost)
#
#location, severity_type, log_feature(368 variables), volume
#eta = .05, gamma = .05 subsample = .75  log_loss = .5899256
#
#location, severity_type, log_feature(368 variables), volume
#eta = .1, gamma = .1 subsample = .75  log_loss = .5368068
#
#
##location, severity_type, log_feature(368 variables), volume, event_type(53 variables)
#eta = .1, gamma = .1 subsample = .75  log_loss = .5791565
#
###location, severity_type, log_feature(368 variables), 
#volume, resource_type(10 variables)
#eta = .1, gamma = .1 subsample = .75  log_loss = .5530854
#
location, severity_type, log_feature(368 variables), volume
#eta = .1, gamma = .1 subsample = .75  log_loss = .5415364
#test.mlogloss.mean = .546575  train = .9
#
#location, severity_type, log_feature(368 variables), volume
#eta = .1, gamma = .1 subsample = .75  log_loss = .546621
#test.mlogloss.mean = NA  train = full train
#
#
#
########################################################################


#stores the ids in a vector and removes id from data frames
train2id = train2[,1]
train2 = train2[,-c(1)]

test3id = test2[,1]
test3 = test2[,-c(1)]

#checks that the number of ids in the vector is equal to the number of rows in 
#the data frames
length(train2id) == nrow(train2)
length(test3id) == nrow(test3)






#saves the outcome variable into a seperate vector
train2_response = train2[,1]
test3_response = test3[,1]

#removes outcome vector from the data_frame
test3 = test3[,-c(1)]
train2 = train2[,-c(1)]



length(train2_response) == nrow(train2)
length(test3_response) == nrow(test3)


train2Matrix = data.matrix(train2)


test3Matrix = data.matrix(test3)





#used to keep only those variables in the importance matrix
#train2Matrix = train2Matrix[,keep]
#test3Matrix = test3Matrix[,keep]


#train2Matrix = train2Matrix[,452:493]
#test3Matrix = test3Matrix[,452:493]




#cross_validation parameters
numberOfClasses = 2
param = list( "objective" = "multi:softprob",
		"eval_metric" = "mlogloss",
		"num_class" = numberOfClasses
		)
cv.nround <- 250
cv.nfold <- 3

#setting up cross_validation
bst.cv = xgb.cv(param=param, data = train2Matrix, label = train2_response, 
                nfold = cv.nfold, nrounds = cv.nround)

#test for optimal nround
bst.cv[which(min(bst.cv$test.mlogloss.mean) == bst.cv$test.mlogloss.mean),]

#sets the number of rounds based on the number of rounds determined by cross_validation
nround = which(min(bst.cv$test.mlogloss.mean) == bst.cv$test.mlogloss.mean)
#actual xgboost
bst = xgboost(param=param, data = train2Matrix, label = train2_response,
		gamma = .1, eta = .1, nrounds=nround,
		subsample = .75, max_delta_step = 15)







# Get the feature real names
names <- dimnames(train2Matrix)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst); importance_matrix

# Nice graph for importance
xgb.plot.importance(importance_matrix[1:100,])



#the predictions are in a nrow(test3)*3 long vector
#bstPred[1:3] is the probability of 0,1,2 for fault_severity
#for the first observation of test2
#has to be a numeric matrix just like the training set
bstPred = predict(bst, test3Matrix)
is.vector(bstPred)
str(bstPred)


#initialize output frame
xgFrame = data.frame(matrix(nrow= nrow(test2), ncol=4))
xgFrame = rename(xgFrame, c("X1" = "id", "X2" = "predict_0", "X3" = "predict_1",  "X4" = "Actual")) 

#Puts the ids for the observations into the first column of xgFrame[,1]
xgFrame[,1] = test3id
xgFrame[,4] = test3_response
#test to make sure ids are the same
sum(xgFrame[,1] != test3id)



z_element = 1
for (i in 1:nrow(test2))
{
	for (z in 1:2)
	{
		#the ith row of xgFrame is given observation z_element
		#probability of occuring from bstPred
		#column z+1 since id is in column 1
		xgFrame[i,z+1] = bstPred[z_element]
		z_element = z_element + 1
	}
}

xgFrame = xgFrame[,-c(2)]
xgFrame2 = xgFrame



log_loss(xgFrame)









##########################################################################################
#ensemble of gbm with 300 trees and all variables and h2o.deeplearning with variables 3:70
#.5 h20.deeplearning(.4756146) .5 gbm (.4689628) = .4625459
#.5 h20.deeplearning(.4802302) .5 gbm (.4725859) = .468787
#
#########################################################################################



ensembleFrame = data.frame(matrix(nrow= nrow(test2), ncol=3))
ensembleFrame = rename(ensembleFrame, c("X1" = "ID", "X2" = "PredictedProb", "X3" = "actual"))


ensembleFrame[,1] = myout2[,1]
ensembleFrame[,3] = myout2[,3]
ensembleFrame[,2] = .5 * myout2[,2] + .5* output[,2]

log_loss(ensembleFrame)


#tests the strength of the prediction versus the actual outcome
cor(cbind(output[,2],myout2[,2]))









test2 = Normalize(test2)
#backups
train5 = train2
test5 = test2



naDetect <- function (train,test)
{
	largest= 0
	most = 1
	for (i in 1:ncol(train))
	{
		if(sum(is.na(train[,i])) > largest)
		{
			largest = sum(is.na(train[,i]));
			most = i;
		}
	}

}



















