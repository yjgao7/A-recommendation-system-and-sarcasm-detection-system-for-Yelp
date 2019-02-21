library(dplyr)
library(ggplot2)
library(extrafont)
library(scales)
library(grid)
library(RColorBrewer)

# R Start(for custom visualizations, colours and fonts)
fontFamily <- "Source Sans Pro"
fontTitle <- "Source Sans Pro Semibold"

colors = c("#16a085","#27ae60","#2980b9","#8e44ad","#f39c12","#c0392b","#1abc9c", "#2ecc71", "#3498db", "#9b59b6", "#f1c40f","#e74c3c")

neutral_colors = function(number) {
  return (brewer.pal(11, "RdYlBu")[-c(5:7)][number %% 8])
}

theme_custom <- function() {theme_bw(base_size = 8) + 
    theme(panel.background = element_rect(fill="#eaeaea"),
          plot.background = element_rect(fill="white"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color="#dddddd"),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(family=fontTitle, size=8, vjust=-.3),
          axis.title.y = element_text(family=fontTitle, size=8, vjust=1.5),
          panel.border = element_rect(color="#cccccc"),
          text = element_text(color = "#1a1a1a", family=fontFamily),
          plot.margin = unit(c(0.25,0.1,0.30,0.35), "cm"),
          plot.title = element_text(family=fontTitle, size=9, vjust=1))                          
}

rank_colors = c(brewer.pal(9, "Reds")[c(8,7,6)],brewer.pal(9, "Greens")[c(7,8)])


#Reading reviews from different chunks of data
yelp_reviews_0<-read.csv('chunk0.csv')
yelp_reviews_1<-read.csv('chunk1.csv')
yelp_reviews_2<-read.csv('chunk2.csv')
yelp_reviews_3<-read.csv('chunk3.csv')
yelp_reviews_4<-read.csv('chunk4.csv')
yelp_reviews_5<-read.csv('chunk5.csv')
yelp_reviews_6<-read.csv('chunk6.csv')
yelp_reviews_7<-read.csv('chunk7.csv')
yelp_reviews_8<-read.csv('chunk8.csv')
yelp_reviews_9<-read.csv('chunk9.csv')
yelp_reviews_10<-read.csv('chunk10.csv')
yelp_reviews_11<-read.csv('chunk11.csv')
yelp_reviews_12<-read.csv('chunk12.csv')
yelp_reviews_13<-read.csv('chunk13.csv')
yelp_reviews_14<-read.csv('chunk14.csv')
yelp_reviews_15<-read.csv('chunk15.csv')
yelp_reviews_16<-read.csv('chunk16.csv')
yelp_reviews_17<-read.csv('chunk17.csv')
yelp_reviews_18<-read.csv('chunk18.csv')
yelp_reviews_19<-read.csv('chunk19.csv')
yelp_reviews_20<-read.csv('chunk20.csv')
yelp_reviews_21<-read.csv('chunk21.csv')
yelp_reviews_22<-read.csv('chunk22.csv')
yelp_reviews_23<-read.csv('chunk23.csv')
yelp_reviews_24<-read.csv('chunk24.csv')
yelp_reviews_25<-read.csv('chunk25.csv')
yelp_reviews_26<-read.csv('chunk26.csv')
yelp_reviews_27<-read.csv('chunk27.csv')
yelp_reviews_28<-read.csv('chunk28.csv')
yelp_reviews_29<-read.csv('chunk29.csv')
yelp_reviews_30<-read.csv('chunk30.csv')
yelp_reviews_31<-read.csv('chunk31.csv')
yelp_reviews_32<-read.csv('chunk32.csv')
yelp_reviews_33<-read.csv('chunk33.csv')
yelp_reviews_34<-read.csv('chunk34.csv')
yelp_reviews_35<-read.csv('chunk35.csv')
yelp_reviews_36<-read.csv('chunk36.csv')
yelp_reviews_37<-read.csv('chunk37.csv')
yelp_reviews_38<-read.csv('chunk38.csv')
yelp_reviews_39<-read.csv('chunk39.csv')
yelp_reviews_40<-read.csv('chunk40.csv')
yelp_reviews_41<-read.csv('chunk41.csv')
yelp_reviews_42<-read.csv('chunk42.csv')
yelp_reviews_43<-read.csv('chunk43.csv')
yelp_reviews_44<-read.csv('chunk44.csv')

#Combining reviews into one variable
yelp_reviews <- rbind(yelp_reviews_0,yelp_reviews_1,yelp_reviews_2,yelp_reviews_3,yelp_reviews_4,yelp_reviews_5,yelp_reviews_6,yelp_reviews_7,yelp_reviews_8,yelp_reviews_9,yelp_reviews_10,yelp_reviews_11,yelp_reviews_12,yelp_reviews_13,yelp_reviews_14,yelp_reviews_15,yelp_reviews_16,yelp_reviews_17,yelp_reviews_18,yelp_reviews_19,yelp_reviews_20,yelp_reviews_21,yelp_reviews_22,yelp_reviews_23,yelp_reviews_24,yelp_reviews_25,yelp_reviews_26,yelp_reviews_27,yelp_reviews_28,yelp_reviews_29,yelp_reviews_30,yelp_reviews_31,yelp_reviews_32,yelp_reviews_33,yelp_reviews_34,yelp_reviews_35,yelp_reviews_36,yelp_reviews_37,yelp_reviews_38,yelp_reviews_39,yelp_reviews_40,yelp_reviews_41,yelp_reviews_42,yelp_reviews_43,yelp_reviews_44)

#Reading list of positive words
positiveWords <- read.csv('positiveWords.csv')
positiveWords <-apply(positiveWords, 1, as.list)
positiveWords <-unlist(positiveWords)

#Reading list of negative words
negativeWords <- read.csv('negativeWords.csv')
negativeWords <-apply(negativeWords, 1, as.list)
negativeWords <-unlist(negativeWords)

#Function to find text word length
review_length_count <- function(review_text){
  
  words <- str_split(review_text, ' ')
  words <- unlist(words)
  word_count <- length(words)
  
  return(word_count)
  
}

#Function to find number of positive words
positive_score <- function(review_text){
  sentence <- review_text
  sentence <- tolower(sentence)
  word.list <- str_split(sentence, ' ')
  words <- unlist(word.list)
  pos.matches <- match(words, positiveWords)
  pos.matches <- !is.na(pos.matches)
  pos.count <- as.numeric(sum(pos.matches))
  
  return(pos.count)
}

#Function to find number of negative words
negative_score <- function(review_text){
  
  sentence <- review_text
  sentence <- tolower(sentence)
  word.list <- str_split(sentence, ' ')
  words <- unlist(word.list)
  neg.matches <- match(words, negativeWords)
  neg.matches <- !is.na(neg.matches)
  neg.count <- as.numeric(sum(neg.matches))
  
  return(neg.count)
}

#Function to find overall sentiment
sentiment_score <- function(pos.count, neg.count){

  score <- as.numeric(pos.count) - as.numeric(neg.count)
  return(score)
}

#Find word count
yelp_reviews$review_length <- apply(yelp_reviews, 1, function(x) {review_length_count(x['text'])})
#Find pos words
yelp_reviews$positive_words <- apply(yelp_reviews, 1, function(x) {positive_score(x['text'])})
#Find neg words
yelp_reviews$negative_words <- apply(yelp_reviews, 1, function(x) {negative_score(x['text'])})
#Find sentiment from pos and neg words
yelp_reviews$sentiment_score <- apply(yelp_reviews, 1, function(x) {sentiment_score(x['positive_words'], x['negative_words'])})

#creating copy of reviews dataset
yelp_reviews_copy <- yelp_reviews
#Removing text column to save space
yelp_reviews_copy$text <- NULL

#Creating backup of file
write.csv(yelp_reviews_copy, file = "reviews_plus_sentiment.csv", row.names = FALSE)

#Stars>3--> Poitive, Stars<3-->Negative, Stars=3--> Neutral
yelp_reviews_copy$stars_sentiment <- ifelse(yelp_reviews_copy$stars>3, 'positive', ifelse(yelp_reviews_copy$stars<3, 'negative', 'neutral'))

#Finding 33 and 66 percentile of sentiment score
quantile(yelp_reviews_copy$sentiment_score, c(.33, .66)) 

#Segregating sentiment into pos neg and neutral based on percentiles from above
yelp_reviews_copy$review_sentiment <- ifelse(yelp_reviews_copy$sentiment_score>=0, 'positive', ifelse(yelp_reviews_copy$sentiment_score<=-2, 'negative', 'neutral'))

#Checking if stars_sentiment and review sentiment match
yelp_reviews_copy$sarcasm <- ifelse(yelp_reviews_copy$review_sentiment==yelp_reviews_copy$stars_sentiment, 1, 0)

#Count of number of mismatch(sarcasm)
sarcasm_total_count <- length(subset(yelp_reviews_copy$sarcasm, yelp_reviews_copy$sarcasm==1))

#Sarcasm percent--> 45.8%
sarcasm_percent <- (as.numeric(sarcasm_total_count)/nrow(yelp_reviews_copy))*100

###Linear Regression(Stars ~ word count + pos words + neg words)
reviews_reg <- lm(stars ~ word_count + positive_words + negative_words, data=yelp_reviews_copy)
summary(reviews_reg)


#Result of linear regression

#Call:
#  lm(formula = stars ~ word_count + positive_words + negative_words, 
#     data = yelp_reviews_copy)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7.8030 -0.7631  0.2162  0.9418 10.6803 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     3.746e+00  1.240e-03  3021.4   <2e-16 ***
#  word_count     -4.140e-03  1.412e-05  -293.3   <2e-16 ***
#  positive_words  1.646e-01  2.820e-04   583.7   <2e-16 ***
#  negative_words -1.823e-01  4.851e-04  -375.8   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 1.194 on 2225209 degrees of freedom
#(1 observation deleted due to missingness)
#Multiple R-squared:  0.2368,	Adjusted R-squared:  0.2368 
#F-statistic: 2.302e+05 on 3 and 2225209 DF,  p-value: < 2.2e-16

#INFERENCE FROM SUMMARY
#If a reviewer posted a blank review with no text in it, that review gave an average rating of 3.746.
#For every positive word, the predicted average star rating given is increased by 0.1646 on average (e.g. 8 positive words indicate a 1-star increase)
#For every negative word, the predicted average star rating given is decreased by 0.1823 on average (e.g. 6-7 negative words indicate a 1-star decrease)
#The amount of words in the review has a very small negative effect. 
#This model explains 23.68% of the variation in the number of stars given in a review. This sounds like a low percentage, but is impressive for such a simple model using unstructured real-world data.

#All of these conclusions are extremely statistically significant due to the large sample size.

#Charts

# Positivity Histogram

ggplot(aes(x=(positive_words+0.0001)/word_count, fill=as.factor(stars), color=as.factor(stars)), data=yelp_reviews_copy) +
  geom_histogram() +
  scale_x_continuous(limits=c(0,0.25), label = percent) +
  scale_y_continuous(label = comma) +
  theme_custom() + 
  theme(legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(0.5, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"), panel.margin=element_blank()) +
  labs(title=paste("Yelp Review Positivity, by # of Stars for", format(nrow(yelp_reviews_copy),big.mark=","),"Reviews"), x="% Review Positivity (# Positive Words : # Words)", y="Total # of Reviews") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  annotate("text", x = Inf, y = -Inf, label = "Akhil Gupta",hjust=1.1, vjust=-0.5, col="#1a1a1a", family=fontFamily, alpha = 0.10, size=2)

ggsave("yelp-review-positivity.png", dpi=300, height=3, width=4)

#Conclusion
#This histogram of positivity scores shows that 1-star reviews have lower positivity with rarely high positivity, and 5-star reviews rarely have low positivity and instead have very high positivity. The distribution for each star rating is close to a Normal distribution, with each successive rating category peaking at increasing positivity values.

# Positivity Density

ggplot(aes(x=(positive_words+0.0001)/word_count, fill=as.factor(stars), color=as.factor(stars), y=..density..), data=yelp_reviews_copy) +
  geom_density(alpha = 1, position="fill") +
  scale_x_continuous(limits=c(0,0.25), label = percent) +
  scale_y_continuous(label = percent) +
  theme_custom() + 
  theme(legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(0.5, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"), panel.margin=element_blank()) +
  labs(title=paste("Yelp Positivity Proportion, by # Stars for", format(nrow(yelp_reviews_copy),big.mark=","),"Reviews"), x="% Review Positivity (# Positive Words : # Words)", y="Proportion of Reviews") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  annotate("text", x = Inf, y = -Inf, label = "Akhil Gupta",hjust=1.1, vjust=-0.5, col="#1a1a1a", family=fontFamily, alpha = 0.10, size=2)

ggsave("yelp-review-positivity-density.png", dpi=300, height=3, width=4)

#Conclusion
#Over half of the 0% positivity reviews are 1-star reviews, while over three-quarters of the reviews at the highest positivity levels are 5-star reviews. (note that the 2-star, 3-star, and 4-star ratings are not as significant at either extreme)

# Negativity Histogram

ggplot(aes(x=(negative_words+0.0001)/word_count, fill=as.factor(stars), color=as.factor(stars)), data=yelp_reviews_copy) +
  geom_histogram() +
  scale_x_continuous(limits=c(0,0.25), label = percent) +
  scale_y_continuous(label = comma) +
  theme_custom() + 
  theme(legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(0.5, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"), panel.margin=element_blank()) +
  labs(title=paste("Yelp Review Negativity, by # of Stars for", format(nrow(yelp_reviews_copy),big.mark=","),"Reviews"), x="% Review Negativity (# Negative Words : # Words)", y="Total # of Reviews") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  annotate("text", x = Inf, y = -Inf, label = "Akhil Gupta",hjust=1.1, vjust=-0.5, col="#1a1a1a", family=fontFamily, alpha = 0.10, size=2)

ggsave("yelp-review-negativity.png", dpi=300, height=3, width=4)

#Conclusion
#Even 1-star reviews aren't completely negative all the time. The chart is heavily skewed right, making it difficult to determine the proportions of each rating at first glance

# Negativity Density

ggplot(aes(x=(negative_words+0.0001)/word_count, fill=as.factor(stars), color=as.factor(stars), y=..density..), data=yelp_reviews_copy) +
  geom_density(alpha = 1, position="fill") +
  scale_x_continuous(limits=c(0,0.25), label = percent) +
  scale_y_continuous(label = percent) +
  theme_custom() + 
  theme(legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(0.5, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"), panel.margin=element_blank()) +
  labs(title=paste("Yelp Negativity Proportion, by # Stars for", format(nrow(yelp_reviews_copy),big.mark=","),"Reviews"), x="% Review Negativity (# Negative Words : # Words)", y="Proportion of Reviews") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  annotate("text", x = Inf, y = -Inf, label = "Akhil Gupta",hjust=1.1, vjust=-0.5, col="#1a1a1a", family=fontFamily, alpha = 0.10, size=2)

ggsave("yelp-review-negativity-density.png", dpi=300, height=3, width=4)

#Conclusion
#At low negativity, the proportions of negative review scores (1-star, 2-stars, 3-stars) and positive review scores (4-stars, 5-stars) are about equal, implying that negative reviews can be just as civil as positive reviews. But high negativity is solely present in 1-star and 2-star reviews.

# Time Series

reviews_monthly_stars <- yelp_reviews_copy %>% group_by(date=substr(date,1,7), stars) %>% summarize(count=n()) %>%
arrange(desc(date))
reviews_monthly_stars <- droplevels(reviews_monthly_stars)

# Time Series Stacked

ggplot(aes(x=as.POSIXct(paste(date,"-01",sep="")), y=count, fill=as.factor(stars)), data=reviews_monthly_stars) +
  geom_area(position = "stack") +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  scale_y_continuous(label = comma) +
  theme_custom() + 
  theme(legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(0.25, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"), panel.margin=element_blank()) +
  labs(title=paste("# of Yelp Reviews by Month, by # Stars for", format(nrow(yelp_reviews_copy),big.mark=","),"Reviews"), x="Date of Review Submission", y="Total # of Review Submissions (by Month)") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars"))

ggsave("yelp-review-time-series.png", dpi=300, height=3, width=4)

#For that chart, it appears that each of the five rating brackets have grown at the same rate, but that isn't the case. Here's a chart of the rating brackets showing how the proportions of new reviews of each rating have changed over time.

ggplot(aes(x=as.POSIXct(paste(date,"-01",sep="")), y=count, fill=as.factor(stars)), data=reviews_monthly_stars) +
  geom_area(position = "fill") +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  scale_y_continuous(label = percent) +
  theme_custom() + 
  theme(legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(0.25, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"), panel.margin=element_blank()) +
  labs(title=paste("Rating Proportion Over Time, by # Stars for", format(nrow(yelp_reviews_copy),big.mark=","),"Reviews"), x="Date of Review", y="Proportion of All Yelp Reviews") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars"))

ggsave("yelp-review-time-proportion.png", dpi=300, height=3, width=4)

#Early Yelp had mostly 4-star and 5-star reviews, as one might expect for an early Web 2.0 startup where the primary users who would be the only ones who would put in the effort to write a review would be those who had positive experiences. However, the behavior from 2010 onward is interesting: the relative proportions of both 1-star reviews and 5-star reviews increases over time.


# Year Donut Charts
# Methodology is unusual: Must create each rectangle manually then warp to polar coordinates

# 2005

data_reviews_agg_year <- yelp_reviews_copy %>%
  filter(substr(date,1,4) == 2005) %>%
  group_by(stars) %>%
  summarize(count = n()) %>%
  mutate(fraction = count / sum(count),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)))

ggplot(aes(fill=as.factor(stars), color=as.factor(stars), ymax=ymax, ymin=ymin, xmax=5, xmin=4), data=data_reviews_agg_year) +
  geom_rect(color="white") +
  coord_polar(theta="y") +
  annotate("text", label = paste(format(data_reviews_agg_year$fraction * 100, digits=2),"%",sep=''), x=rep(6,5), y=(data_reviews_agg_year$ymin + data_reviews_agg_year$ymax)/2, col=rank_colors, size=3, family=fontTitle) +
  xlim(c(0, 6)) +
  theme_custom() +
  theme(panel.grid=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background=element_blank(), axis.title.x = element_blank(), axis.title.y=element_blank(),legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(0.5, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"), plot.margin = unit(c(-0.5,-0.5,-0.5,-0.5), "cm"), panel.border= element_blank()) +
  labs(title="Customized ring plot") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  annotate("text", x = 0, y = 0, label = "2005", col="#1a1a1a", family="Source Sans Pro Light", size=11)

ggsave("yelp-proportion-2005.png", dpi=300, width=3, height=3)


# 2014

data_reviews_agg_year <- yelp_reviews_copy %>%
  filter(substr(date,1,4) == 2014) %>%
  group_by(stars) %>%
  summarize(count = n()) %>%
  mutate(fraction = count / sum(count),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)))

ggplot(aes(fill=as.factor(stars), color=as.factor(stars), ymax=ymax, ymin=ymin, xmax=5, xmin=4), data=data_reviews_agg_year) +
  geom_rect(color="white") +
  coord_polar(theta="y") +
  annotate("text", label = paste(format(data_reviews_agg_year$fraction * 100, digits=2),"%",sep=''), x=rep(6,5), y=(data_reviews_agg_year$ymin + data_reviews_agg_year$ymax)/2, col=rank_colors, size=3, family=fontTitle) +
  xlim(c(0, 6)) +
  theme_custom() +
  theme(panel.grid=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background=element_blank(), axis.title.x = element_blank(), axis.title.y=element_blank(),legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(0.5, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"), plot.margin = unit(c(-0.5,-0.5,-0.5,-0.5), "cm"), panel.border= element_blank()) +
  labs(title="Customized ring plot") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  annotate("text", x = 0, y = 0, label = "2014", col="#1a1a1a", family="Source Sans Pro Light", size=11)

ggsave("yelp-proportion-2014.png", dpi=300, width=3, height=3)

#Monthly positivity and negativity charts

reviews_monthly_positivity <- yelp_reviews_copy %>% group_by(date=substr(date,1,7)) %>%
  summarize(count=n(),
            positivity = mean(positive_words) / mean(word_count),
            negativity = mean(negative_words) / mean(word_count)) %>%
  arrange(desc(date))

# Monthly Positivity

ggplot(aes(x=as.POSIXct(paste(date,"-01",sep="")), y=positivity), data=reviews_monthly_positivity) +
  geom_line(color = rank_colors[5]) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  scale_y_continuous(label = percent) +
  theme_custom() + 
  labs(title=paste("Positivity of Yelp Reviews by Month for", format(nrow(yelp_reviews_copy),big.mark=","),"Reviews"), x="Date of Review Submission", y="Average % Review Positivity (by Month)")

ggsave("yelp-review-time-series-positivity.png", dpi=300, height=3, width=4)

# Monthly Negativity

ggplot(aes(x=as.POSIXct(paste(date,"-01",sep="")), y=negativity), data=reviews_monthly_positivity) +
  geom_line(color = rank_colors[1]) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  scale_y_continuous(label = percent) +
  theme_custom() + 
  labs(title=paste("Negativity of Yelp Reviews by Month for", format(nrow(yelp_reviews_copy),big.mark=","),"Reviews"), x="Date of Review Submission", y="Average % Review Negativity (by Month)")

ggsave("yelp-review-time-series-negativity.png", dpi=300, height=3, width=4)


