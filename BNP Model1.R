######################################################
#BNP Kaggle competition
#Model 1
#Predicting whether a claim is suitable for accelerated
#approval (0 or 1 outcome variable)
#
#######################################################

#import datasets
train <- read.csv('C:\\Users\\Randy\\Downloads\\Kaggle BNP\\train.csv')
test <- read.csv('C:\\Users\\Randy\\Downloads\\Kaggle BNP\\test.csv')


bothFrames = split(train)
str(bothFrames)

train2 = bothFrames[[1]]
test2 = bothFrames[[2]]



































###############################################################
#
#The log_loss function takes two arguements, a data.frame and
#a vector of length 1 num_predict
#The data.frame has ncol = num_predict +1  with column 1 = id, column 2 = predict_0, 
#column 3 = predict_1 ... etc
#
#It will multiply the log of the predicted probability times 1 if the observation
#turned out to be that category, 0 otherwise
#It sums all of those up and divides by -N, where N is the number of 
#observations in data_frame
#
#################################################################
log_loss <- function(data_frame, num_predict)
{
	total = 0
	for( i in 1:nrow(data_frame))
	{
		
		for(j in 1: num_predict)
		{
			y=0
			#gets the id from the ith row of the data_frame
			#if the actual fault_severity == j-1 then that is when y is 1
			#This is the classification of the point
			#The [1] just takes the first in case their are duplicate observations
			#of the id
			if (test2[which(test2$id==data_frame$id[i])[1],3] == (j-1))
			{
				y=1;
				
			}

			#total is equal to total plus y times
			# the log of the ith row and the j+1 column
			#it is j+1 because predict_0 is in column 2
			total = total + y*log( max( min( data_frame[i,(j+1)], 1-10^(-15) ),  10^(-15) ) )
			

		}
		
	}
	print("Your logloss score is:")
	print(-total/nrow(data_frame))


}






