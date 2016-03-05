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
for( i in 15:134)
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

#interaction between v5 and v4
train[,135] = train[,6] * train[,7]
test[,135] = test[,5] * test[,6]


#1 v5*v4 <=20
train[which(train[,135] <= 20),136] = 1
test[which(test[,135] <= 20),136]  = 1




#1 v5*v4 >20 <= 40
train[which(train[,135] > 20 & train[,135] <= 40 ), 137] = 1
test[which(test[,135]   > 20  & test[,135] <= 40),  137] = 1



#1 v5*v4 >40 <= 60
train[which(train[,135] > 40 & train[,135] <= 60 ), 138] = 1
test[which(test[,135]   > 40  & test[,135] <= 60),  138] = 1



#1 v5*v4 >40 <= 60
train[which(train[,135] > 60  ), 139] = 1
test[which(test[,135]   > 60  ),  139] = 1



#interaction between v11 and v12
train[,140] = train[,13]*train[,14]
test[,140] = test[,12]*test[,13]


#v11*v12 <=100
train[which(train[,140] <= 100  ),141] = 1
test[which(test[,140]   <= 100 ),  141] = 1

#v11*v12 > 100
train[which(train[,140] > 100  ), 142] = 1
test[which(test[,140]   > 100 ),  142] = 1



#interaction between v15 and v16
train[,143] = train[,17]*train[,18]
test[,143] = test[,16]*test[,17]



#v15*v16 <=7
train[which(train[,143] <= 7  ), 144] = 1
test[which(test[,143]   <= 7 ),  144] = 1


#v15*v16 >7 <=12
train[which(train[,143] > 7 & train[,143] <= 12 ), 145] = 1
test[which(test[,143]   > 7  & test[,143] <= 12),  145] = 1


#v15*v16 <=7
train[which(train[,143] > 12  ), 146] = 1
test[which(test[,143]   > 12 ),  146] = 1


#v27*v28
train[,147] = train[,29]*train[,30]
test[,147] = test[,28]*test[,29]




























