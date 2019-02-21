from alchemyapi import AlchemyAPI
import pandas as pd

def processSentimentByAPI():
    review = pd.read_csv('yelp_academic_dataset_review.csv')
    for index, row in review.iterrows():
        text = row['text']
        sentimentType = retrieveReviewSentiment(text)
        row['sentimentType'] = sentimentType
        print('%s\n%s' % (index, sentimentType))
    review.to_csv('review_sentiment_output.csv', sep='\t')

def retrieveReviewSentiment(text):
    alchemyapi = AlchemyAPI()
    response = alchemyapi.sentiment("text", text)
    status = response["status"]
    if status == 'OK':
        return response["docSentiment"]["type"]
    else:
        return response['statusInfo']

if __name__ == '__main__':
    processSentimentByAPI()
