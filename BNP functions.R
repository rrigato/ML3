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




















