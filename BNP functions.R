#####################################################
#This script includes functions that will  be used in
#BNP models. They will already be tested.
#
#Functions included:
#split
#log_loss
#
#
######################################################





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


















