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
library(h2o)

#import datasets
train <- read.csv('C:\\Users\\Randy\\Downloads\\Kaggle BNP\\train.csv')
test <- read.csv('C:\\Users\\Randy\\Downloads\\Kaggle BNP\\test.csv')


#sets the working directory
setwd('C:/Users/Randy/Downloads/Kaggle BNP/ML3')

#Includes functions in the BNP Function script
source("BNP functions.R")


#calls the split function to divide the train dataset
bothFrames = split(train)
train2 = bothFrames[[1]]
test2 = bothFrames[[2]]





######################################################################
#Deep Learning in H2o log_loss:.4868497
#
#
#####################################################################
h2o.init(nthreads = -1)

train5 = train2[,1:70]; test5 = test2[, 1:70]

train5[,2] = as.factor(train5[,2])
test5[,2] = as.factor(test5[,2])
train5 = as.h2o(train5)
test5 = as.h2o(test5)

trainDL = h2o.deeplearning(x = 3:70,y = 2 , training_frame = train5)

predictions <- h2o.predict(trainDL, newdata = test5, type = "probs")

DLPred = as.data.frame(predictions[,3])


	outputFrame = data.frame(matrix(nrow= nrow(test2), ncol=3))
	outputFrame = rename(outputFrame, c("X1" = "ID", "X2" = "PredictedProb", "X3" = "actual"))
	outputFrame[,1] = test2[,1]
	outputFrame[,2] = DLPred
	outputFrame[,3] = test2$target


log_loss(outputFrame)





#runs gbm model
output = gbmParse(train2,test2)



##########################################################################################
#ensemble of gbm with 300 trees and all variables and h2o.deeplearning with variables 3:70
#.5 h20.deeplearning(.4756146) .5 gbm (.4689628) = .4625459
#
#
#########################################################################################



ensembleFrame = data.frame(matrix(nrow= nrow(test2), ncol=3))
ensembleFrame = rename(ensembleFrame, c("X1" = "ID", "X2" = "PredictedProb", "X3" = "actual"))


ensembleFrame[,1] = outputFrame[,1]
ensembleFrame[,3] = outputFrame[,3]
ensembleFrame[,2] = .5 * outputFrame[,2] + .5* output[,2]

log_loss(ensembleFrame)













outputFrame = data.frame(matrix(nrow= nrow(test2), ncol=2))
outputFrame = rename(outputFrame, c("X1" = "ID", "X2" = "PredictedProb"))

outputFrame = cbind(outputFrame, test2$target)
outputFrame[,1] = test2[,1]
outputFrame[,2] = .76

log_loss(outputFrame)


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



















