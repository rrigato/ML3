#####################################################
#This script includes functions that will  be used in
#BNP models. They will already be tested.
#
#Functions included:
#split
#numericColumns
#Normalize
#log_loss
#gbmParse
######################################################

library(plyr)



################################################################
#	Splitting the train dataset into train2 and test2
#
#
#
#################################################################



split <- function(train)
{
	#edit The percentage of the dataset in the train2 and test2, used to build a model 
	size_of_train = floor(.8*nrow(train))
	ran_num_test = 1:nrow(train)

	#gets random numbers for train2 using a sample
	ran_num_train = sample(1:nrow(train), size_of_train)

	#numbers not randomly selected for train2 are included in test2
	#this command gets the numbers not in ran_num_train
	ran_num_test = ran_num_test[(!(ran_num_test %in% ran_num_train)) == TRUE]
	train2 = train[ran_num_train,]
	test2 = train[ran_num_test,]

	#makes a list of the two frames
	bothFrames = list(train2,test2)
	
	#returns a list of both frames
	return(bothFrames)
}



###############################################################################
#Given a data frame, returns a vector of the column numbers of that data frame
#which are numeric
#
#Skips first two columns because those are the id and target variables
################################################################################
numericColumns <- function(train)
{
	temp = numeric()
	tempCounter = 1
	for( z in 3: ncol(train))
	{
		if (is.numeric(train[,z]))
		{
			temp[tempCounter] = z
			tempCounter = tempCounter + 1
		}
	}
	
	return(temp);
}









###############################################################################################
#Takes a data frame as an arguement
#Depends On: numericColumns
#
#turns all variables that are numeric from numericColumns into standard normal variables
#returns the data frame with numeric variables normalized and all others left in tact
###############################################################################################
Normalize  <- function(train2)
{
	#gets the columns that are numeric
	numberCol = numericColumns(train2)

	#scale() is what normalizes the column, have to cast it as numeric first
	#iterates over ever column deemed numeric by the numericColumns function
	for (i in 1:length(numberCol))
	{
		train2[,numberCol[i]] = as.numeric(scale(train2[,numberCol[i]])) 
	}	
	
	return(train2);
}















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










######################################################################################
##gbmParse 
#takes two dataframes with a train dataset and a test dataset. Builds a gbm model on the train
#dataset and then gets predictions for the test dataset
#
#returns an output data frame with  3 variables: the ids of the test set, the predicted probability of
#1 for the test set and the actual for the test set
#
#
#Depends: log_loss
#
#
#
#
#GBM target~. -ID -v22, distribution = "adaboost", n.trees = 20, shrinkage = .1,
#		interaction.depth =2, log_loss= .4907367
#
#200 trees with adaboost = .4732754
#
#20 trees normalized numeric variables = .4922601
#####################################################################################
gbmParse <- function (train2, test2) {

	#runs gbm takes out id and v22 because v22 has too many levels for a category
	bTree = gbm(target~. -ID -v22, distribution = "bernoulli", n.trees = 300, shrinkage = .1,
			interaction.depth =2,  data = train2)

	#runs the gbm model on the test set giving back an output of probabilities
	bTreeP = predict(bTree, newdata=test2, n.trees = 5000, type="response")
	bTreeP = as.data.frame(bTreeP)
	head(bTreeP)



	#initializes and fills the outputFrame that will be tested in the log_loss function 
	#and then returned
	outputFrame = data.frame(matrix(nrow= nrow(test2), ncol=3))
	outputFrame = rename(outputFrame, c("X1" = "ID", "X2" = "PredictedProb", "X3" = "actual"))
	outputFrame[,1] = test2[,1]
	outputFrame[,2] = bTreeP[,1]
	outputFrame[,3] = test2$target

	head(outputFrame)


	#runs log_loss on train set
	log_loss(outputFrame)
	
	#returns the outputed frame
	return(outputFrame);
}











