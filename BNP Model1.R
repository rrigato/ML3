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
bothFrames = split(train, .6)
train2 = bothFrames[[1]]
test2 = bothFrames[[2]]

train2[is.na(train2)] = -1
test2[is.na(test2)] = -1

#runs deep learning model
myout2 = deepL(train2 , test2,explan) 

#runs gbm model
output = gbmParse(train2,test2)


###########################################################################
#extraTrees model
#extraTrees cannot have categorical variables for its explanatory variables
#
#This function takes two arguements the train data and the test data
#
#The y variable is assumed to be in the second column of the train and test dataset
#
#depends: library(extraTrees) library(plyr) and log_loss
############################################################################



extraTreesParse <- function(train5, test5){

	#x is the variables used to predict the outcome variable y,
	#which has to be a factor for classification
	x = train5
	y = as.factor(train5[,2])


	 options( java.parameters = "-Xmx5g" )

	#calls the extraTrees function 
	eT = extraTrees(x,y, mtry = 20, nodesize = 2, numRandomCuts = 2,
		na.action ="zero")

	#returns the probabilities and makes it into a dataframe
	etOut = predict(eT, newdata = test5, probability=TRUE)
	etOut = as.data.frame(etOut)


	#initialize output frame
	etFrame = data.frame(matrix(nrow= nrow(test2), ncol=3))
	etFrame = rename(etFrame, c("X1" = "id", "X2" = "PredictedProb", "X3" = "actual")) 

	#Puts the ids for the observations into the first column of outputFrame[,1]
	etFrame[,1] = test5$ID

	#puts the predictions for y being 1 into etFrame2
	#and the actual in the 3rd column
	etFrame[,2] = etOut[,2]
	etFrame[,3] = test5[,3]

	#calls the log loss function to get the actual log_loss from the witheld training data
	log_loss(etFrame)

}










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
ensembleFrame[,2] = .25 * myout2[,2] + .75* output[,2]

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



















