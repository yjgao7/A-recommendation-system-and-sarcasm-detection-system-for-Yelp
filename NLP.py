import nltk
from nltk.tokenize import WhitespaceTokenizer
from nltk.corpus import stopwords
import pandas as pd
import scipy.stats as sp

tokenizer = WhitespaceTokenizer()

nltk.download()

stopword_list = stopwords.words('english')

reviews_df = pd.read_csv("C:/Users/Documents/Yelp/yelp_academic_dataset_review.csv", encoding="utf-8")

positive_terms = []
f = open('C:/Users/Documents/Yelp/positive_terms.txt', "r")
positive_terms = f.read().splitlines()
f.close()

negative_terms = []
f = open('C:/Users/Documents/Yelp/negative_terms.txt', "r")
negative_terms = f.read().splitlines()
f.close()

porter = nltk.PorterStemmer()
def normalize_review_text(text):
    text = text.lower()
    text = remove_punctuation(text)
    text = " ".join(text.split())
    text_tokens = tokenizer.tokenize(text)
    text_tokens = [porter.stem(w) for w in text_tokens if w not in stopword_list]

reviews_df["text"] = reviews_df["text"].apply(normalize_review_text)

def calculate_positivity(text):
    num_tokens = len(text)
    num_positive_tokens = 0
    for t in text:
        if t in positive_terms:
            num_positive_tokens = num_positive_tokens + 1
    # The positivity score is the fraction of tokens that were found in the positive dictionary
    return float(num_positive_tokens) / float(num_tokens)

reviews_df["positivity_score"] = reviews_df["text"].apply(calculate_positivity)

def calculate_negativity(text):
    num_tokens = len(text)
    num_negative_tokens = 0
    for t in text:
        if t in negative_terms:
            num_negative_tokens = num_negative_tokens + 1
    # The positivity score is the fraction of tokens that were found in the positive dictionary
    return float(num_negative_tokens) / float(num_tokens)

reviews_df["negativity_score"] = reviews_df["text"].apply(calculate_negativity)

pos_score_stars_corr = sp.pearsonr(reviews_df["stars"].values, reviews_df["positivity_score"].values)

neg_score_stars_corr = sp.pearsonr(reviews_df["stars"].values, reviews_df["negativity_score"].values)
