setwd("/Users/Documents/Yelp")
library(grid)
library(cwhmisc)
library(cwhmisc)
library(e1071)
library(rpart)
library(recommenderlab)
library(reshape2)
library(plyr)
library(caret)
library(mlbench)
library(data.table)

# Load data
reviews_original <- read.csv("trial.csv")
reviews <- reviews_original
business <- read.csv("yelp_academic_dataset_business.csv")
user <- read.csv("yelp_academic_dataset_user.csv")
threshold <- mean(business$review_count,na.rm=FALSE)
business_list <- subset(business, (business$review_count > threshold) & (stars >= 4), select=c("business_id"))
business_list <- as.vector(business_list$business_id)
user_threshold <- mean(user$review_count,na.rm=FALSE)
user_list <- subset(user, user$review_count > user_threshold, select=c("user_id"))
user_list <- as.vector(user_list$user_id)

# Extract the user_id, business_id and rating columns
reviews <- reviews[,c(2,6,8)]
reviews <- reviews[which(reviews$business_id %in% business_list),]
reviews <- reviews[which(reviews$user_id %in% user_list),]

reviews$stars <- as.numeric(reviews$stars)

# Aggregate multiple reviews from the same user-business pair
reviews <- aggregate(stars ~ user_id + business_id, FUN = mean, data=reviews)

# Convert the dataset to a matrix with rows representing "user_id" and columns representing "business_id"
g <- acast(reviews, user_id ~ business_id,value.var="stars")
g <- as.matrix(g)

# Convert it to realRatingMatrix data structure
r <- as(g, "realRatingMatrix")
# Normalize the rating matrix
r_m <- normalize(r)

image(r,main = "Raw Ratings")
image(r_m,main = "Normalized Ratings")

# Create a recommender object (model)
#       They pertain to four different algorithms.
#        UBCF: User-based collaborative filtering
#        IBCF: Item-based collaborative filtering
#      Parameter 'method' decides similarity measure
#        Cosine or Jaccard
rec_UBCF <-Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
# rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=5, minRating=1))
# rec=Recommender(r[1:nrow(r)],method="IBCF", param=list(normalize = "Z-score",method="Jaccard",minRating=1))
rec <- Recommender(r[1:nrow(r)], method="POPULAR")

recom <- predict(rec, r[1:nrow(r)], type="ratings")
recom #7684 x 3923 rating matrix of class 'realRatingMatrix' with 30134492 ratings.
recom_df <- as(recom, "data.frame") # 7684 x 3923 - reviews 9840 = recom_df 30134492
# recom_df[recom_df$item=='--BlvDO_RG2yElKu9XA1_g'& recom_df$user=='MRpr7424VPPicEVPcj7RCg',]
write.csv(recom_df, file="recom_1.csv", row.names=FALSE)

recom_UBCF <- predict(rec_UBCF, r[1:nrow(r)], type="ratings")
recom_UBCF #7684 x 3923 rating matrix of class 'realRatingMatrix' with 14368524 ratings.
recom_UBCF_df <- as(recom_UBCF, "data.frame") # 7684 x 3923 - reviews 9840 = recom_df 30134492
write.csv(recom_UBCF_df, file="recom_UBCF_1.csv", row.names=FALSE)

# recom_TopN <- predict(rec, r[1:nrow(r)], n = 3)
# recom_TopN
# recom_TopN_list <- as(recom_TopN, "list")
# recom_TopN_df <- do.call(rbind.data.frame, recom_TopN_list)
# write.csv(recom_TopN_df, file="recom_TopN_1.csv", row.names=FALSE)




