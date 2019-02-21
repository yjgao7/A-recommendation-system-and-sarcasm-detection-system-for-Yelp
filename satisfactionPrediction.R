setwd("/Users/Documents/Yelp")
library(grid)
library(cwhmisc)
library(e1071)
library(rpart)
library(caret)
library(mlbench)
library(stringr)
library(Metrics)

# Load data
user <- read.csv("yelp_academic_dataset_user.csv")
business <- read.csv("yelp_academic_dataset_business.csv")
business_original <- business
reviews <-read.csv("yelp_academic_dataset_review.csv")

# Focus on restaurants
business$categories <- apply(business, 1, function(x) { grepl("restaurant",tolower(x[21]))}) 
business <- business[business$categories== TRUE,]

# Get rid of irrelevant variables
business <- business[, -grep("attributes.Hair+",colnames(business))]
business <- business[, -grep("attributes.Music+",colnames(business))]
business <- business[, !colnames(business) %in% c("attributes.BYOB","attributes.By.Appointment.Only","categories","attributes.BYOB.Corkage","full_address","attributes.Ages.Allowed","open","type","attributes.Coat.Check"
                                                  ,"attributes.Accepts.Insurance","neighborhoods","attributes.Dietary.Restrictions.vegan","attributes.Dietary.Restrictions.vegetarian")]
business$stars <- factor(business$stars)
reviews <- reviews[, c("user_id", "review_id", "business_id", "stars")]
names(reviews)[names(reviews)=="stars"] <- "review_stars"
user$friends <- (str_count(user$friends, "'")/2)
user$compliments_score <- rowSums(user[,grep("compliments+", names(user))], na.rm = TRUE)
user$votes_score <- rowSums(user[,grep("votes+", names(user))], na.rm = TRUE)
user <- user[, c("review_count", "average_stars", "user_id", "fans", "friends", "compliments_score", "votes_score")] 
names(user)[names(user)=="average_stars"] <- "average_stars_of_user"

# Calculate the good_for_score 
l <- grep("attributes.Good+", colnames(business))
for(i in l){
  business[,i] <- as.numeric(business[,i])
  business[,i][business[,i]==1] <- 0 
  business[,i][business[,i]==2] <- 0  
  business[,i][business[,i]==3] <- 1
}
business$good_for_score <- rowSums(business[,grep("attributes.Good+", names(business))], na.rm = TRUE)

# Calculate the park_score
l <- grep("attributes.Parking+", colnames(business)) # index of columns
for(i in l){
  business[,i] <- as.numeric(business[,i])
  business[,i][business[,i]==1] <- 0 # False
  business[,i][business[,i]==2] <- 0 # Blank 
  business[,i][business[,i]==3] <- 1 # True
}
business$park_score <- rowSums(business[,grep("attributes.Parking+", names(business))], na.rm = TRUE) 

#business$stars <-as.numeric(business$stars)
#business$stars_c <-ifelse(business$stars>3.5,TRUE,FALSE)
#business$stars_c <-as.factor(business$stars_c)

reviewsBusiness <- merge(reviews, business, by="business_id")
total <- merge(reviewsBusiness, user, by="user_id")
total$review_stars_c <-ifelse(total$review_stars>3.5,"Satisfied","Unsatisfied")
total$review_stars_c <-as.factor(total$review_stars_c)
names(total)[names(total)=="review_count.x"] <- "review_count_business"
names(total)[names(total)=="review_count.y"] <- "review_count_user"

# Record performance of starts ("Positive" "Negative") 
record_performance_binary <- function(df, name, predV, test) {
  pred.table <- table(pred = predV, true=test$review_stars_c)
  df <- rbind(df, data.frame(model=c(name), score=c(classAgreement(pred.table)$diag)))
  return(df)
}

#record performacne of prediction model
record_performance <- function(df, name, predV, test) {
  df <- rbind(df, data.frame(model=c(name), RMSE=sqrt(mean((test$review_stars-predV)^2))))
  return(df)
}

# Prediction using svm model
predict_svm <- function(model,test){
  p_svm <- predict(model, test)
  return(p_svm)
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
inTrain <- createDataPartition(y = total$review_stars_c, p = .75,list = FALSE) 

trainset <- total[inTrain,]
testset  <- total[-inTrain,]
dim(trainset)
dim(testset)

# Predict feedback (satisfied, unsatisfied) that a user gives to a particular restaurant
mfc_baseline_binary <- sum(testset$review_stars > 3.5) / nrow(testset)
results_svm_binary <- data.frame(model=c("MFC"), score=c(mfc_baseline_binary))

predBinary <- predict_svm(svm(review_stars_c ~ park_score+review_count_user+average_stars_of_user+good_for_score+fans+friends, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "park_score+review_count_user+average_stars_of_user+good_for_score+fans+friends", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ review_count_user+average_stars_of_user+fans+friends, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "review_count_user+average_stars_of_user+fans+friends", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ average_stars_of_user+fans, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "average_stars_of_user+fans", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ review_count_business+average_stars_of_user+fans+friends, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "review_count_business+average_stars_of_user+fans+friends", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ review_count_user+review_count_business+average_stars_of_user+fans+friends, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "review_count_user+review_count_business+average_stars_of_user+fans+friends", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ stars+review_count_user+review_count_business+average_stars_of_user+fans+friends, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "stars+review_count_user+review_count_business+average_stars_of_user+fans+friends", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ stars+average_stars_of_user+fans+friends, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "stars+average_stars_of_user+fans+friends", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ stars+average_stars_of_user+fans+friends+compliments_score+votes_score, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "stars+average_stars_of_user+fans+friends+compliments_score+votes_score", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ attributes.Noise.Level+attributes.Wi.Fi+attributes.Alcohol, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "attributes.Noise.Level+attributes.Wi.Fi+attributes.Alcohol", predBinary, testset)
#predBinary <- predict_svm(svm(review_stars_c ~ attributes.Price.Range, data=trainset), testset)
#results_svm_binary <- record_performance_binary(results_svm_binary, "attributes.Price.Range", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ hours.Thursday.open, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "hours.Thursday.open", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ attributes.Drive.Thru, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "attributes.Drive.Thru", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ attributes.Drive.Thru+hours.Thursday.open+attributes.Noise.Level+attributes.Wi.Fi+attributes.Alcohol, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "attributes.Drive.Thru+hours.Thursday.open+attributes.Noise.Level+attributes.Wi.Fi+attributes.Alcohol", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Noise.Level+attributes.Wi.Fi+attributes.Alcohol, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Noise.Level+attributes.Wi.Fi+attributes.Alcohol", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Noise.Level+attributes.Wi.Fi, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Noise.Level+attributes.Wi.Fi", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Noise.Level+attributes.Alcohol, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Noise.Level+attributes.Alcohol", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Wi.Fi, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Wi.Fi", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ attributes.Has.TV+attributes.Good.For.lunch+attributes.Good.For.dinner, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "attributes.Has.TV+attributes.Good.For.lunch+attributes.Good.For.dinner", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Wi.Fi+attributes.Has.TV, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Wi.Fi+attributes.Has.TV", predBinary, testset)

results_svm_binary

# Predict stars (1, 2, 3, 4, 5) that a user gives to a particular restaurant
mfc_baseline_stars <- sqrt(mean((testset$review_stars - 4)^2)) 
results_svm_stars <- data.frame(model=c("MFC"), RMSE=c(mfc_baseline_stars))

predStars <- predict_svm(svm(review_stars ~ park_score+review_count_business+average_stars_of_user+good_for_score+fans+friends, data=trainset), testset)
results_svm_stars <- record_performance(results_svm_stars, "park_score+review_count_business+average_stars_of_user+good_for_score+fans+friends", predStars, testset)
predStars <- predict_svm(svm(review_stars ~ review_count_business+average_stars_of_user+review_count_user+stars+fans+friends, data=trainset), testset)
results_svm_stars <- record_performance(results_svm_stars, "review_count_business+average_stars_of_user+review_count_user+stars+fans+friends", predStars, testset)
predStars <- predict_svm(svm(review_stars ~ stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Noise.Level+attributes.Wi.Fi+attributes.Alcohol, data=trainset), testset)
results_svm_stars <- record_performance(results_svm_stars, "stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Noise.Level+attributes.Wi.Fi+attributes.Alcohol", predStars, testset)
predStars <- predict_svm(svm(review_stars ~ stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Noise.Level+attributes.Wi.Fi, data=trainset), testset)
results_svm_stars <- record_performance(results_svm_stars, "stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Noise.Level+attributes.Wi.Fi", predStars, testset)
predStars <- predict_svm(svm(review_stars ~ stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Noise.Level+attributes.Alcohol, data=trainset), testset)
results_svm_stars <- record_performance(results_svm_stars, "stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Noise.Level+attributes.Alcohol", predStars, testset)
predStars <- predict_svm(svm(review_stars ~ stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Wi.Fi, data=trainset), testset)
results_svm_stars <- record_performance(results_svm_stars, "stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Wi.Fi", predStars, testset)
predStars <- predict_svm(svm(review_stars ~ attributes.Has.TV+attributes.Good.For.lunch+attributes.Good.For.dinner, data=trainset), testset)
results_svm_stars <- record_performance(results_svm_stars, "attributes.Has.TV+attributes.Good.For.lunch+attributes.Good.For.dinner", predStars, testset)
predStars <- predict_svm(svm(review_stars ~ stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Wi.Fi+attributes.Has.TV, data=trainset), testset)
results_svm_stars <- record_performance(results_svm_stars, "stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Wi.Fi+attributes.Has.TV", predStars, testset)

results_svm_stars






