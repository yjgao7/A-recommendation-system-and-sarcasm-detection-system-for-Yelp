# Predicting Average Business Rating

library(grid)
library(cwhmisc)
library(e1071)
library(rpart)
library(caret)
library(mlbench)

# Load data
business <- read.csv("yelp_academic_dataset_business.csv")

# Creating backup of data
business_original <- business

# Selecting only restaurants from business Data
business$categories <- apply(business, 1, function(x) { grepl("restaurant",tolower(x[21]))}) 
business <- business[business$categories== TRUE,]

# Removing irrelevant variables
business <- business[, -grep("attributes.Hair+",colnames(business))]
business <- business[, -grep("attributes.Music+",colnames(business))]
business <- business[, !colnames(business) %in%  c("attributes.BYOB","attributes.By.Appointment.Only","categories","attributes.BYOB.Corkage","full_address","attributes.Ages.Allowed","open","type","attributes.Coat.Check"
                                                   ,"attributes.Accepts.Insurance","neighborhoods")]


# New Feature 1
# Calculating New Feature (Good For Score)
l <- grep("attributes.Good+", colnames(business))
for(i in l){
  
  business[,i] <- toupper(business[,i])
  business[,i] <- ifelse(business[,i] == TRUE,1,0)

}
business$good_for_score <- rowSums(business[,grep("attributes.Good+", names(business))])
business$log_good_for <- scale(log(as.numeric(business$good_for_score) + 1))


# New feature 2
# Calculate the parking_score
l2 <- grep("attributes.Parking+", colnames(business)) # index of columns
for(i in l2){
  
  business[,i] <- toupper(business[,i])
  business[,i] <- ifelse(business[,i] == TRUE,1,0)

}
business$park_score <- rowSums(business[,grep("attributes.Parking+", names(business))])
business$log_park_score <- scale(log(as.numeric(business$park_score) + 1))


# Categorizing business ratings into Good or not
business$stars_c <-ifelse(business$stars>3.5,TRUE,FALSE)
business$stars_c <-as.factor(business$stars_c)

# Alcohol
business$attributes.Alcohol[business$attributes.Alcohol == ""] <- 'none'
business$attributes.Alcohol <- as.factor(business$attributes.Alcohol)

# Price Range
business$attributes.Price.Range[business$attributes.Price.Range == 3] <- 4
business$attributes.Price.Range[business$attributes.Price.Range == 4] <- 5
business$attributes.Price.Range[business$attributes.Price.Range == ""] <- 3
business$attributes.Price.Range <- as.factor(business$attributes.Price.Range)
business$log_price_range <- scale(log(as.numeric(business$price_range)))


# Review Count (scaling)
business$log_review_count <- scale(log(as.numeric(business$review_count) + 1))

# Noise Level
business$attributes.Noise.Level[business$attributes.Noise.Level == ""] <- 'average'
business$attributes.Noise.Level <- as.factor(business$attributes.Noise.Level)

# Wi-Fi
business$attributes.Wi.Fi[business$attributes.Wi.Fi == ""] <- 'no'
business$attributes.Wi.Fi <- as.factor(business$attributes.Wi.Fi)

# Outdoor-Seating
business$attributes.Outdoor.Seating[business$attributes.Outdoor.Seating == ""] <- 'False'
business$attributes.Outdoor.Seating <- as.factor(business$attributes.Outdoor.Seating)

# Waiter Service
business$attributes.Waiter.Service[business$attributes.Waiter.Service == ""] <- 'False'
business$attributes.Waiter.Service <- as.factor(business$attributes.Waiter.Service)

# Record performance of each model 
record_performance <- function(df, name, model, test) {
  svm.pred <- predict(model, test)
  svm.table <- table(pred = svm.pred, true=test$stars_c)
  df <- rbind(df, data.frame(SVM_model=c(name), Accuracy=c(classAgreement(svm.table)$diag)))
  return(df)
}

# Scale and center 
stars_c_index <- grep("stars_c", colnames(business))
pre <- preProcess(business[,-stars_c_index], method = c("center", "scale"))
pre

# Stratified Sampling: based on the distribution of the target variable on different classes.
# Preserve the distribution of the outcome in the training and test sets
set.seed(107)
inTrain <- createDataPartition(y = business$stars_c, p = .75,list = FALSE) 

trainset <- business[inTrain,]
testset  <- business[-inTrain,]

#Checking dimensions of trainset and testset
dim(trainset)
dim(testset)

# SVM Model

# If all restuarnts are bad
mfc_baseline <- sum(testset$stars_c == FALSE) / nrow(testset)
results_svm <- data.frame(SVM_model=c("MFC"), Accuracy=c(mfc_baseline))

results_svm <- record_performance(results_svm, "good_for_score", svm(stars_c ~ log_good_for, data=trainset), testset)
results_svm <- record_performance(results_svm, "noise level", svm(stars_c ~ attributes.Noise.Level, data=trainset), testset)
results_svm <- record_performance(results_svm, "alcohol", svm(stars_c ~ attributes.Alcohol, data=trainset), testset)
results_svm <- record_performance(results_svm, "Wi-Fi", svm(stars_c ~ attributes.Wi.Fi, data=trainset), testset)
results_svm <- record_performance(results_svm, "Parking score", svm(stars_c ~ log_park_score, data=trainset), testset)
results_svm <- record_performance(results_svm, "price range", svm(stars_c ~ log_price_range, data=trainset), testset)
results_svm <- record_performance(results_svm, "log_review_count", svm(stars_c ~ log_review_count, data=trainset), testset)
results_svm <- record_performance(results_svm, "outdoor seating", svm(stars_c ~ attributes.Outdoor.Seating, data=trainset), testset)
results_svm <- record_performance(results_svm, "waiter service", svm(stars_c ~ attributes.Waiter.Service, data=trainset), testset)

# New feature Plot
ggplot(business, aes(park_score, fill = stars_c)) + geom_histogram(binwidth = 1) + facet_grid(~ stars_c) + ggtitle('Parking_score')

ggplot(business, aes(good_for_score, fill = stars_c)) + geom_histogram(binwidth = 1) + facet_grid(~ stars_c) + ggtitle('Price Range')


