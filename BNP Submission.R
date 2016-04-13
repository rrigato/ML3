

################################################################
#	implementation of extreme gradient boosting algorithms(xgboost)
#
#
##################################################################


test[is.na(test)] = -1

test3id = test[,1]
test3 = test[,-c(1)]

#checks that the number of ids in the vector is equal to the number of rows in 
#the data frames

length(test3id) == nrow(test3)

test3Matrix = sparse.model.matrix(~. - v22 -1, data = test)




test3Matrix = data.matrix(test3)


#testKeep = which(colnames(test3Matrix) %in% colnames(train2Matrix))
#test3Matrix = test3Matrix[,testKeep]





#has to be a numeric matrix just like the training set
bstPred = predict(bst, test3Matrix)
is.vector(bstPred)
str(bstPred)

#initialize output frame
finalFrame = data.frame(matrix(nrow= nrow(test), ncol=2))
finalFrame = rename(finalFrame, c("X1" = "ID", "X2" = "PredictedProb")) 

#Puts the ids for the observations into the first column of finalFrame[,1]
finalFrame[,1] = test3id

#test to make sure ids are the same
sum(finalFrame[,1] != test3id)


#probability of y = 1
finalFrame[,2] = bstPred

#validation
nrow(finalFrame) == length(unique(test3id))
sum(finalFrame$id != unique(test3id))
sum(is.na(finalFrame))

#should be no more than 114393
sum(finalFrame[,2])



write.csv(finalFrame, "C:\\Users\\Randy\\Downloads\\Kaggle BNP\\Results10.csv",
		row.names = FALSE)







##################################################################################
#Ensemble Extra Trees classifier with xgboost
#
#
#
#
###############################################################################

etClassifier = read.csv("C:\\Users\\Randy\\Downloads\\Kaggle BNP\\Results5.csv")

str(etClassifier)

nrow(finalFrame) == nrow(etClassifier)

sum(etClassifier[,2])
sum(finalFrame[,2])

cor(etClassifier[,2], finalFrame[,2])

#initialize output frame
finalEnsemble = data.frame(matrix(nrow= nrow(test), ncol=2))
finalEnsemble = rename(finalEnsemble, c("X1" = "ID", "X2" = "PredictedProb")) 

#Puts the ids for the observations into the first column of finalEnsemble[,1]
finalEnsemble[,1] = test[,1]




finalEnsemble[,2] = .69* finalFrame[,2] + .31* etClassifier[,2]
head(etClassifier); head(finalFrame);head(finalEnsemble)



#validation
nrow(finalEnsemble) == length(unique(test3id))
sum(finalEnsemble$id != unique(test3id))
sum(is.na(finalEnsemble))

#should be no more than 114393
sum(finalEnsemble[,2])




write.csv(finalEnsemble, "C:\\Users\\Randy\\Downloads\\Kaggle BNP\\Results10.csv",
		row.names = FALSE)



############################################################################
#deep learning classifier
#50% deeplearning 50% extratrees classifier
#
#
##############################################################################


result6 = read.csv("C:\\Users\\Randy\\Downloads\\Kaggle BNP\\Results6.csv")

cor(result6[,2],etClassifier[,2], dlFrame[,2])


 which(colnames(train[,3:ncol(train)])!=colnames(test[,2:ncol(test)])
colHold = numeric(nrow(test))

trimTest = cbind(test[,1],colHold, test[,2:ncol(test)])
ncol(trimTest) == ncol(train)

trimTest = as.h2o(trimTest)
#makes probability predictions on the test5 data using the model built
	predictions <- h2o.predict(trainDL, newdata = trimTest, type = "probs")

	#turns h2o output into dataframe
	DLPred = as.data.frame(predictions[,3])


	#initializes dlFrame
	dlFrame = data.frame(matrix(nrow= nrow(test), ncol=2))
	dlFrame = rename(dlFrame, c("X1" = "ID", "X2" = "PredictedProb"))
	
	#adds ids back into dlFrame
	dlFrame[,1] = test[,1]

	#adds the predicted values from the model
	dlFrame[,2] = DLPred
	


#correlation between frames
cor(cbind(result6[,2],dlFrame[,2], etClassifier[,2]))



#initialize output frame
dlEnsemble = data.frame(matrix(nrow= nrow(test), ncol=2))
dlEnsemble = rename(dlEnsemble, c("X1" = "ID", "X2" = "PredictedProb")) 

#Puts the ids for the observations into the first column of dlEnsemble[,1]
dlEnsemble[,1] = test[,1]



#50% deeplearning 50% extratrees classifier
dlEnsemble[,2] = .15* dlFrame[,2] + .85* result6[,2]
head(dlFrame);head(result6); head(dlEnsemble)



#validation
nrow(dlEnsemble) == length(unique(test[,1]))
sum(dlEnsemble$id != unique(test[,1]))
sum(is.na(dlEnsemble))

#should be no more than 114393
sum(dlEnsemble[,2])




write.csv(dlEnsemble, "C:\\Users\\Randy\\Downloads\\Kaggle BNP\\Results8.csv",
		row.names = FALSE)










