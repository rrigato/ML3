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
library(Matrix)






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



keep = numeric()
keep_count = 1
for(i in 1:ncol(train))
{
	if(is.factor(train[,i]))
	{
		keep[keep_count] = i
		keep_count = keep_count + 1 
	}
}




#one hot encoding on variables
temp2 = sparse.model.matrix(Target~temp -1, data = temp)



#runs deep learning model
deepOut = deepL(train2[c(1,2,keep)],test2[c(1,2,keep)],explan) 

#runs gbm model
gbmOut = gbmParse(train2[c(1,2,keep)],test2[c(1,2,keep)])


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
param = list( "objective" = "binary:logistic",
		"eval_metric" = "logloss"
		)
cv.nround <- 250
cv.nfold <- 3

#setting up cross_validation
bst.cv = xgb.cv(param=param, data = train2Matrix, label = train2_response, 
                nfold = cv.nfold, nrounds = cv.nround)

#test for optimal nround
bst.cv[which(min(bst.cv$test.logloss.mean) == bst.cv$test.logloss.mean),]

#sets the number of rounds based on the number of rounds determined by cross_validation
nround = which(min(bst.cv$test.logloss.mean) == bst.cv$test.logloss.mean)
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
xgFrame = data.frame(matrix(nrow= nrow(test2), ncol=3))
xgFrame = rename(xgFrame, c("X1" = "id", "X2" = "predictedProb", "X3" = "Actual")) 

#Puts the ids for the observations into the first column of xgFrame[,1]
xgFrame[,1] = test3id
xgFrame[,3] = test3_response
#test to make sure ids are the same
sum(xgFrame[,1] != test3id)


#probability of y = 1
xgFrame[,2] = bstPred


xgFrame2 = xgFrame



log_loss(xgFrame)



######################################################################################
#
#
#feature_selection
####################################################################################


importance_matrix = as.data.frame(importance_matrix)


#gets the names of the variables that matter
top124 = importance_matrix[,1]


#gets all the names in train2Matrix
train2Col = colnames(train2Matrix)

#gets the column numbers of the variables you should keep
keep = which(train2Col %in%  top124)






##########################################################################################
#ensemble of gbm with 300 trees and all variables and h2o.deeplearning with variables 3:70
#.5 h20.deeplearning(.4756146) .5 gbm (.4689628) = .4625459
#.5 h20.deeplearning(.4802302) .5 gbm (.4725859) = .468787
#deepOut(.4825387) and xgFrame(.4785925) = .475112
#
#
#.33*gbmOut( 0.4812537) and .33*deepOut( 0.483595) and .33*xgFrame(.4802912) = .4759635
#
#
#.5gbmOut(0.4939747) + .5 xgFrame(0.482375) = .478009
#
#
#
#
#
#########################################################################################



ensembleFrame = data.frame(matrix(nrow= nrow(test2), ncol=3))
ensembleFrame = rename(ensembleFrame, c("X1" = "ID", "X2" = "PredictedProb", "X3" = "actual"))


ensembleFrame[,1] = gbmOut[,1]
ensembleFrame[,3] = gbmOut[,3]
ensembleFrame[,2] = .5* xgFrame[,2] + .25*gbmOut[,2]+ .25*deepOut[,2]

log_loss(ensembleFrame)


#tests the strength of the prediction versus the actual outcome
cor(cbind(xgFrame[,2],deepOut[,2], gbmOut[,2]))









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



















