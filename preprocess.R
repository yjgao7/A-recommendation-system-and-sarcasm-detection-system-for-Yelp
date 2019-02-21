setwd("/Users/Documents/Yelp")
library(grid)
library(cwhmisc)
library(e1071)
library(rpart)
library(caret)
library(mlbench)
# Load data
users <- read.csv("yelp_academic_dataset_user.csv")
#tip <- read.csv("yelp_academic_dataset_tip.csv")
#checkin <- read.csv("yelp_academic_dataset_checkin.csv")
business <- read.csv("yelp_academic_dataset_business.csv")
business_original <- business
reviews <-read.csv("trial.csv")

# Focus on restaurants
business$categories <- apply(business, 1, function(x) { grepl("restaurant",tolower(x[21]))}) 
business <- business[business$categories== TRUE,]

# Get rid of irrelevant variables
business <- business[, -grep("attributes.Hair+",colnames(business))]
business <- business[, -grep("attributes.Music+",colnames(business))]
business <- business[, !colnames(business) %in%  c("attributes.BYOB","attributes.By.Appointment.Only","categories","attributes.BYOB.Corkage","full_address","attributes.Ages.Allowed","open","type","attributes.Coat.Check"
                                                   ,"attributes.Accepts.Insurance","neighborhoods")]
reviews <- reviews[, c("user_id", "review_id", "business_id", "stars")]
users <- users[, c("review_count", "average_stars", "user_id", "review_count", "fans", "friends")] 

#my idea to deal with the ambience attributes
# 1. calculate a total score for the ambience
# 2. TRUE -> +1
# 3. FALSE -> -1
# 4. Empty or null ->0

#group variables
#park_score
#good_for_score
#ambience_score

#restriction_score

# Calculate the good_for_score 
l <- grep("attributes.Good+", colnames(business))
for(i in l){
  business[,i] <- as.numeric(business[,i])
  business[,i][business[,i]==1] <- 0 
  business[,i][business[,i]==2] <- 0  #can also set to -1, not sure
  business[,i][business[,i]==3] <- 1
}
business$good_for_score <- rowSums(business[,grep("attributes.Good+", names(business))])

# Calculate the park_score
l <- grep("attributes.Parking+", colnames(business)) # index of columns
for(i in l){
  business[,i] <- as.numeric(business[,i])
  business[,i][business[,i]==1] <- 0 # False
  business[,i][business[,i]==2] <- 0 # Blank 
  business[,i][business[,i]==3] <- 1 # True
}
business$park_score <- rowSums(business[,grep("attributes.Parking+", names(business))]) #? 'x' must be numeric

business$stars_c <-ifelse(business$stars>3.5,TRUE,FALSE)
business$stars_c <-as.factor(business$stars_c)

# Record performance of each model 
record_performance <- function(df, name, Predict, test) {
  pred.table <- table(pred = Predict, true=test$stars_c)
  df <- rbind(df, data.frame(model=c(name), score=c(classAgreement(pred.table)$diag)))
  return(df)
}

# Prediction using svm model
predict_svm <- function(model,test){
  Predict <- predict(model, test)
  return(Predict)
}

# Scale and center 
stars_c_index <- grep("stars_c", colnames(business))
pre <- preProcess(business[,-stars_c_index], method = c("center", "scale"))
pre

# Random Sampling: divide the dataset into trainset and testset
# index <- 1:nrow(business)
# testindex <- sample(index, trunc(length(index)/5))
# testset <- business[testindex,]
# trainset <- business[-testindex,]

# Stratified Sampling: based on the distribution of the target variable on different classes.
# Preserve the distribution of the outcome in the training and test sets
set.seed(107)
inTrain <- createDataPartition(y = business$stars_c, p = .75,list = FALSE) 

trainset <- business[inTrain,]
testset  <- business[-inTrain,]
dim(trainset)
dim(testset)

mfc_baseline <- sum(testset$stars > 3.5) / nrow(testset)
results_svm <- data.frame(model=c("MFC"), score=c(mfc_baseline))

business$stars_c <- as.numeric(business$stars_c)
Predict <- predict_svm(svm(stars_c ~ park_score+review_count+good_for_score, data=trainset), testset)
results_svm <- record_performance(results_svm, "trial", Predict, testset)
results_svm

