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



















