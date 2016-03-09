

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

write.csv(finalFrame, "C:\\Users\\Randy\\Downloads\\Kaggle BNP\\Results2.csv",
		row.names = FALSE)










