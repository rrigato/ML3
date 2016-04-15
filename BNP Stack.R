#calls the split function to divide the train dataset
bothFrames = split(train, .7)
train2 = bothFrames[[1]]
test2 = bothFrames[[2]]

train2[is.na(train2)] = -1
test2[is.na(test2)] = -1

a_out = deepL(train2,test2, explanFeatures)
b_out = deepL(test2,train2,explanFeatures)








































