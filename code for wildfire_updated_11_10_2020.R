#Logistic Regression to predict California wildfire risk 

set.seed(31415) # this allows for the result to be the same when you run this code
# because it pull a random sample from the dataset. 
all_conditions <- read.csv("~/Downloads/all_conditions.csv") 
#PLEASE MAKE SURE THE WILDFIRE DATASET IS CALLED all_conditions 
#PLEASE MAKE SURE THE WILDFIRE DATASET IS CALLED all_conditions
#PLEASE MAKE SURE THE WILDFIRE DATASET IS CALLED all_conditions. 


min_max_scaling <- function(datacolumn) { x <- (datacolumn - min(datacolumn, na.rm = TRUE))/(max(datacolumn, na.rm = TRUE)-min(datacolumn, na.rm = TRUE))
return(x)                                     
}
all_conditions$Avg.Air.Temp..F. <- as.integer(all_conditions$Avg.Air.Temp..F.)
typeof(all_conditions$Avg.Air.Temp..F.)
max(all_conditions$Avg.Air.Temp..F., na.rm = TRUE)

#normalizing all the variables within the dataset using a min max function. 
all_conditions$Precip..in. <- min_max_scaling(all_conditions$Precip..in.)
all_conditions$Sol.Rad..Ly.day. <- min_max_scaling(all_conditions$Sol.Rad..Ly.day.)
all_conditions$Avg.Vap.Pres..mBars. <- min_max_scaling(all_conditions$Avg.Vap.Pres..mBars.)
all_conditions$Avg.Air.Temp..F. <- min_max_scaling(all_conditions$Avg.Air.Temp..F.)
all_conditions$Avg.Rel.Hum.... <- min_max_scaling(all_conditions$Avg.Rel.Hum....)
all_conditions$Avg.Soil.Temp..F. <- min_max_scaling(all_conditions$Avg.Soil.Temp..F.)
all_conditions$ETo..in. <- min_max_scaling(as.numeric(all_conditions$ETo..in.))
all_conditions$Max.Air.Temp..F. <-min_max_scaling(all_conditions$Max.Air.Temp..F.)
all_conditions$Min.Air.Temp..F.<- min_max_scaling(all_conditions$Min.Air.Temp..F.)
all_conditions$Max.Rel.Hum.... <- min_max_scaling(all_conditions$Max.Rel.Hum....)
all_conditions$Min.Rel.Hum.... <- min_max_scaling(all_conditions$Min.Rel.Hum....)
all_conditions$Dew.Point..F. <- min_max_scaling(all_conditions$Dew.Point..F.)
all_conditions$Avg.Wind.Speed..mph.<- min_max_scaling(all_conditions$Avg.Wind.Speed..mph.)
all_conditions$Wind.Run..miles. <-min_max_scaling(all_conditions$Wind.Run..miles.)

#normalizing all the variables within the dataset using a min max function. 
write.csv(all_conditions,"normalized_all_conditions.csv")

all_conditions$train_test <- sample(c("train","test"),nrow(all_conditions),replace = TRUE, prob = c(2/3, 1/3))
# the line above creates a variable train_test that populators every row as either test or train with 2/3 being train and 1/3 with test


train_set <- all_conditions[all_conditions$train_test=="train",] # create the training dataset 
test_set <- all_conditions[all_conditions$train_test=="test",0:18] # creates the testing dataset without the target variable 
answers_ <-data.frame(all_conditions[all_conditions$train_test=="test",19]) # this is the results yes or no 

# create the model 
model112 <- glm(Target~ Precip..in.+ Sol.Rad..Ly.day. +Avg.Vap.Pres..mBars. + Avg.Air.Temp..F. +  Avg.Rel.Hum.... 
                +	Avg.Wind.Speed..mph. +	Avg.Soil.Temp..F.,data = train_set,family = 'binomial') 

summary(model112) # shows the model

predict_val_ <- predict(model112,test_set,type='response') # this predict the answers as a percent of likelihood 

predictedlog = data.frame(predict_val_,answers_) # this creates a dataframe from our two answers
funnnnnn <- data.frame(rep(0,100),rep(0,100))

predictedlog$predict_val_ = ifelse(predictedlog$predict_val_  >=.15,1,0) # this says create a new varable on the predictlog dataframe,
# if the predict probs is equal to or greater than 3% mark it as a yes( there is going to be a wild fire) 
# we can play around with this number to improve our accuracy 

no_na <-predictedlog[complete.cases(predictedlog),]

# there were 10 NA in this group for some reason( most likely missing data from one of the column so this line removes them)

install.packages('heuristica')
library(heuristica)
confusionmatrix <- confusionMatrixFor_Neg1_0_1(no_na$all_conditions.all_conditions.train_test.....test...19.,no_na$predict_val_)

confusionmatrix[2:3,2:3] # in a confusion matrix it compares the number of predictions it got right when the model predicted no wildfire and there was no wildfire
# Prediction of no wildfire and no wildfire was observed is in the top left corner. Also the bottom right corner, 1,1( predicted wildfire and there was wildfire) is 
# also a correct answer. If you want more inform on this there are 1000's of youtube vidoes on the topic 

acc <- (confusionmatrix[2,2]+confusionmatrix[3,3])/nrow(no_na) # this gives us the accuracy of the model, right answers over all observation 
acc # at the 3% level we have a 58% accuracy

false_negative <- confusionmatrix[3,2]
false_negative_rate <- confusionmatrix[3,2]/(confusionmatrix[3,2]+confusionmatrix[3,3])
View(all_conditions
     )

  #confusionmatrix <- table(no_na$predict_val_,no_na$all_conditions.all_conditions.train_test.....test...19.) # this creates a confustion matrix 
