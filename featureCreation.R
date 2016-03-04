##################################################################################
#This script visualizes the data and creates new features using that information
#Test column # 133 has nothing in it to account for the fact that train has an extra 
#variable target 
#
#writes the new train and test to a csv
###############################################################################




#import datasets
train <- read.csv('C:\\Users\\Randy\\Downloads\\Kaggle BNP\\train.csv')
test <- read.csv('C:\\Users\\Randy\\Downloads\\Kaggle BNP\\test.csv')

#plot the variables
for( i in 3:134)
{
	if(i %%6 == 3)
	{
		dev.new()
		par(mfrow=c(2,3))
	}
	plot(train[,i], train[,i+1], col = c("red", "green")
	[as.factor(train[,2])], xlab = paste("v",i),
	ylab = paste("v", i+1), type = "p")
}


train[,134:150] = 0
test[,133:150] = 0

#number of NAs in each row is a variable
train[1:nrow(train),134] = rowSums(is.na(train))
test[1:nrow(test),134] = rowSums(is.na(test))


