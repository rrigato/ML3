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
	[as.factor(train[,2])], xlab = paste(i),
	ylab = paste(i+1), type = "p")
}


#number of NAs in each row is a variable
train[1:nrow(train),134] = rowSums(is.na(train))
test[1:nrow(test),134] = rowSums(is.na(test))


train[,134:150] = 0
test[,133:150] = 0



#interaction between v4 and v5
train[,135] = train[,6] * train[,7]
test[,135] = test[,5] * test[,6]


#1 v4*v5 <=20
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
train[,140] = train[,11]*train[,12]
test[,140] = test[,10]*test[,11]


#v11*v12 <=100
train[which(train[,140] <= 100  ),141] = 1
test[which(test[,140]   <= 100 ),  141] = 1

#v11*v12 > 100
train[which(train[,140] > 100  ), 142] = 1
test[which(test[,140]   > 100 ),  142] = 1



#interaction between v15 and v16
train[,143] = train[,15]*train[,16]
test[,143] = test[,14]*test[,15]



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


#v39*v40
train[,148] = train[,39]*train[,40]
test[,148] = test[,38]*test[,39]


#v40*v41
train[,149] = train[,40]*train[,41]
test[,149] = test[,39]*test[,40]



#v39*v40 <2
train[which(train[,148] ==0), 150] = 1
test[which(test[,148]   ==0),  150] = 1




train[,151:170] = 0
test[,151:170] = 0



#v39*v40 >=2
train[which(train[,148] > 0), 151] = 1
test[which(test[,148]   > 0),  151] = 1





#v40*v41 <2
train[which(train[,149] == 0), 152] = 1
test[which(test[,149]  == 0),  152] = 1


#v40*v41 >= 2
train[which(train[,149] > 0), 153] = 1
test[which(test[,149]   > 0),  153] = 1



#51*52
train[,154] = train[,51]*train[,52]
test[,154] = test[,50]*test[,51]



#52*53
train[,155] = train[,52]*train[,53]
test[,155] = test[,51]*test[,52]

#51*52 <2
train[which(train[,154] < 2 ), 156] = 1
test[which(test[,154]  <2 ),  156] = 1


#51*52 >=2
train[which(train[,154] >= 2 ), 157] = 1
test[which(test[,154]  >=  2 ),  157] = 1




#53*52 <4
train[which(train[,155] < 4 ), 158] = 1
test[which(test[,155]  <4 ),  158] = 1


#53*52 >=4
train[which(train[,155] >= 4 ), 159] = 1
test[which(test[,155]  >=  4 ),  159] = 1



#60*61
train[,160] = train[,60]*train[,61]
test[,160] = test[,59]*test[,60]




#63*64
train[,161] = train[,63]*train[,64]
test[,161] = test[,62]*test[,63]

#65*64
train[,162] = train[,65]*train[,64]
test[,162] = test[,63]*test[,64]




#74*75
train[,163] = train[,74]*train[,75]
test[,163] = test[,73]*test[,74]




#74*75 <3
train[which(train[,163] < 3 ), 164] = 1
test[which(test[,163]  < 3 ),  164] = 1


#74*75 >=3
train[which(train[,163] >= 3 ), 165] = 1
test[which(test[,163]  >=  3 ),  165] = 1







#90*91
train[,166] = train[,90]*train[,91]
test[,166] = test[,89]*test[,90]


#92*91
train[,167] = train[,92]*train[,91]
test[,167] = test[,90]*test[,91]




#101*102
train[,168] = train[,101]*train[,102]
test[,168] = test[,100]*test[,101]




#107*108
train[,169] = train[,107]*train[,108]
test[,169] = test[,106]*test[,107]




#106*107
train[,170] = train[,106]*train[,107]
test[,170] = test[,105]*test[,106]




#111*110
train[,171] = train[,110]*train[,111]
test[,171] = test[,109]*test[,110]




#116*117
train[,172] = train[,116]*train[,117]
test[,172] = test[,115]*test[,116]



#117*118
train[,173] = train[,117]*train[,118]
test[,173] = test[,116]*test[,117]



#128*129
train[,174] = train[,128]*train[,129]
test[,174] = test[,127]*test[,128]



















#write the data frames with the new variables to csv files
write.csv(train, "C:\\Users\\Randy\\Downloads\\Kaggle BNP\\trainAll.csv",
	row.names = FALSE)



write.csv(test, "C:\\Users\\Randy\\Downloads\\Kaggle BNP\\testAll.csv",
	row.names = FALSE)












