######################################################
#BNP Kaggle competition
#Model 1
#Predicting whether a claim is suitable for accelerated
#approval (0 or 1 outcome variable)
#
#######################################################

#rename function
library(plyr)



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



outputFrame = data.frame(matrix(nrow= nrow(test2), ncol=2))
outputFrame = rename(outputFrame, c("X1" = "ID", "X2" = "PredictedProb"))

outputFrame = cbind(outputFrame, test2$target)
outputFrame[,1] = test2[,1]
outputFrame[,2] = .5


log_loss(outputFrame)
























###############################################################
#
#The log_loss function takes one arguements, a data_frame which has the 
#id, predictions, and actual
#
#
#only works for binary outcome. For multicategory logloss, refer to Kaggle Telstra script
#
#################################################################
log_loss <- function(data_frame)
{
	total = 0
	for( i in 1:nrow(data_frame))
	{
		

			y=0
			#gets the id from the ith row of the data_frame
			#if the actual fault_severity == j-1 then that is when y is 1
			#This is the classification of the point
			#The [1] just takes the first in case their are duplicate observations
			#of the id
			if (data_frame[i,3] == 1)
			{
				y=1;
				
			}

			#total is equal to total plus y times
			# the log of the ith row and the j+1 column
			#it is j+1 because predict_0 is in column 2
			total = total + y*log( max( min( data_frame[i,2], 1-10^(-15) ),  10^(-15) ) ) +
				(1-y)*log( max( min((1- data_frame[i,2]), 1-10^(-15) ),  10^(-15) ) )
			

		
	}
	print("Your logloss score is:")
	print(-total/nrow(data_frame))


}






