#calls the split function to divide the train dataset
bothFrames = split(train, .5)
train5 = bothFrames[[1]]
test5 = bothFrames[[2]]

train5[is.na(train2)] = -1
test5[is.na(test2)] = -1

a_out = gbmParse(train5,test5)
b_out = gbmParse(test5,train5)





test[is.na(test)] = -1


test_out = outputFrame



a_out = gbmParse(train5,test5)
b_out = gbmParse(test5,train5)






	#runs gbm takes out id and v22 because v22 has too many levels for a category
	bTree = gbm(actual~PredictedProb, distribution = "bernoulli", n.trees = 30, shrinkage = .1,
			interaction.depth =5,  data = a_out)

	#runs the gbm model on the test set giving back an output of probabilities
	bTreeP = predict(bTree, newdata=b_out, n.trees = 5000, type="response")
	bTreeP = as.data.frame(bTreeP)
	head(bTreeP)
	str(bTreeP)


	#initializes and fills the a_out2 that will be tested in the log_loss function 
	#and then returned
	a_out2 = data.frame(matrix(nrow= nrow(b_out), ncol=3))
	a_out2 = rename(a_out2, c("X1" = "ID", "X2" = "PredictedProb", "X3" = "actual"))
	a_out2[,1] = b_out[,1]
	a_out2[,2] = bTreeP[,1]
	a_out2[,3] = b_out$actual

	head(a_out2)


	#runs log_loss on train set
	log_loss(a_out2)
	
	#returns the outputed frame
	return(a_out2);




























