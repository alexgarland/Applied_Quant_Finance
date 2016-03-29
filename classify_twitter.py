import nltk.classify.util
import nltk.classify
from nltk.sentiment import SentimentAnalyzer
import nltk.sentiment.util
from nltk.corpus import twitter_samples
import json
import pprint

def get_words_in_tweets(tweets):
    all_words = []
    for (words, sentiment) in tweets:
      all_words.extend(words)
    return all_words

def get_word_features(wordlist):
    wordlist = nltk.FreqDist(wordlist)
    word_features = wordlist.keys()
    return word_features

def extract_features(document):
    document_words = set(document)
    features = {}
    for word in word_features:
        features['contains(%s)' % word] = (word in document_words)
    return features

all_pos = []
all_neg = []

with open('positive_tweets.json') as data_file:
    for line in data_file:
        pos_data = json.loads(line)
        tup = (pos_data["text"], "positive")
        all_pos.append(tup)

with open('negative_tweets.json') as data_file:
    for line in data_file:
        neg_data = json.loads(line)
        tup = (neg_data["text"], "negative")
        all_neg.append(tup)

tweets = []
for (words, sentiment) in all_pos + all_neg:
    words_filtered = [e.lower() for e in words.split() if len(e) >= 3]
    tweets.append((words_filtered, sentiment))

word_features = get_word_features(get_words_in_tweets(tweets))
training_set = nltk.classify.apply_features(extract_features, tweets)
classifier = nltk.NaiveBayesClassifier.train(training_set)
