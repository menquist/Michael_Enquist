import tweepy
import time
from tweepy import OAuthHandler
consumer_key = 'xbZHxyhKFtFOlWIYjEBg3830o'
consumer_secret = 'hJmrN3xaR11o1TL7hhL2bhcqplqlprsHTDlDzWYQTz79KOFigQ'
access_token = '826992578075648002-AbfhDdp9hVKZgQV4dnJd6obe3h9do9R'
access_secret = 'myNE88Yf6MIwcvEdoqNXH67WgqvfjaCaCtVj2IbUIouYC'
auth = OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_secret)

# Initializing a listener class that streams from  Twitter
class StdOutListener(tweepy.StreamListener):
    
    def __init__(self, api=None):
        super(StdOutListener, self).__init__()
        self.num_tweets = 0
        self.tweet_limit = 100
        self.file_label = "streamed_tweets"
        
    def set_tweet_limit(self,tweet_quantity):
        self.tweet_limit = tweet_quantity
        
    def set_file_label(self,file_label):
        self.file_label = file_label
        
    def on_data(self, data):
        self.num_tweets += 1
        if self.num_tweets < self.tweet_limit:
            try:
                with open('%s.json' %self.file_label+time.strftime("%x")+time.strftime("%H:%M:%S") , 'a') as f:
                    f.write(data)
                    return True
            except KeyboardInterrupt:
                print("Keyboard Interrupt: Ending Stream")
            except BaseException as e:
                print(str(e))
            return True
        else:
            return False
        
    def on_error(self, status):
        print(status)
        
# Set up function that will initialize this class, and add the constraints we want , 
#as well as filter the feed for the subjects we're interested int
def collect_tweets_from_stream(subjects=['@BBYNews','@BestBuy','@BestBuyCanada',"@BestBuy_Deals","@BestBuySupport",'@BestBuyCanHelp','@BBYCanadaDeals'],auth=auth,max_tweets=10,file_label="best_buy_10_tweets"):  
    my_listener = StdOutListener()
    my_listener.set_tweet_limit(max_tweets)
    my_listener.set_file_label(file_label)
    stream=tweepy.Stream(auth,my_listener)
    try:
        stream.filter(track=subjects)
    except KeyboardInterrupt as e:
        print("Keyboard Interrupt: Stopping Stream")
        
        
################################trudeau

# twitter sends its data as a json
import json
# we will convert the json data into a csv
import csv
#this csv will then be converted to a pandas dataframe
import pandas as pd
#to clean the date & time field of the tweets
from datetime import datetime
import tweepy

from sklearn.feature_extraction.text import TfidfVectorizer, CountVectorizer

from sklearn.decomposition import NMF, LatentDirichletAllocation
# Tweepy's OAuth handler allows us to wrap our auth information in an object and does this transaction for us
from tweepy import OAuthHandler
consumer_key = ''
consumer_secret = ''
access_token = ''
access_secret = ''
auth = OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_secret)

# Initializing a listener class that streams from  Twitter
class StdOutListener(tweepy.StreamListener):
    
    def __init__(self, api=None):
        super(StdOutListener, self).__init__()
        self.num_tweets = 0
        self.tweet_limit = 100
        self.file_label = "streamed_tweets"
        
    def set_tweet_limit(self,tweet_quantity):
        self.tweet_limit = tweet_quantity
        
    def set_file_label(self,file_label):
        self.file_label = file_label
        
    def on_data(self, data):
        self.num_tweets += 1
        if self.num_tweets < self.tweet_limit:
            try:
                with open('%s.json' %self.file_label , 'a') as f:
                    f.write(data)
                    return True
            except KeyboardInterrupt:
                print("Keyboard Interrupt: Ending Stream")
            except BaseException as e:
                print(str(e))
            return True
        else:
            return False
        
    def on_error(self, status):
        print(status)
        
        
# Set up function that will initialize this class, and add the constraints we want , 
#as well as filter the feed for the subjects we're interested int
def collect_tweets_from_stream(subjects,auth=auth,max_tweets,file_label="streamed_tweets"):  
    my_listener = StdOutListener()
    my_listener.set_tweet_limit(max_tweets)
    my_listener.set_file_label(file_label)
    stream=tweepy.Stream(auth,my_listener)
    try:
        stream.filter(track=subjects)
    except KeyboardInterrupt as e:
        print("Keyboard Interrupt: Stopping Stream")
        
collect_tweets_from_stream(subjects=['Trudeau'],max_tweets=1000000,file_label = "Trudeau_streamed_tweets")

def load_json_file(json_file_name):
    # We need to iterate over each line in the json file and parse it using the json.loads function of the json package
    #initialize a list to store the loaded tweets
    loaded_tweets = []
    #Each iteration "i" is a line in the file
    #Since some line will have problems such as unreadable characters, we need to set a "try - except" catch#It will try to read the line, and if it encounters any error will move to the next one

    # if an exception occurs pass over the line
    for i in open(json_file_name):
        try: 
            loaded_tweets.append(json.loads(i))
        except:
            pass

    return loaded_tweets
    
    
tweets = load_json_file("Trudeau_streamed_tweets.json")
tweets[0]
tweets[0]['lang']
tweets[0]['text']
tweets[0]['favorite_count']
# To get each column of the dataframe we want to build, we'll loop over each tweet and extract that specific variable
#After we're done we can combine the columns into a dataframe

# at the i'th tweet take this particular element 
tweet_id = [i['id_str'] for i in tweets]
tweet_text = [i['text'] for i in tweets]
created_at = [i['created_at'] for i in tweets]
language = [i['lang'] for i in tweets]
#remember: some elements are inside other elements
screen_names = [i['user']['screen_name'] for i in tweets]
times_favorited = [i['favorite_count'] for i in tweets]

len(created_at)
created_at = pd.to_datetime(created_at,errors='ignore')
tweets_df = pd.DataFrame()
tweets_df['id']=tweet_id
tweets_df['text']=tweet_text
tweets_df['created_at']=created_at
tweets_df['language']=language
tweets_df['screen_names']= screen_names
tweets_df['favorited']= times_favorited
tweets_df.head()
#Drop any NA's in the data
tweets_df.dropna(axis=0,how='any',inplace=True)
# cheack the data for NA's
tweets_df.isnull().sum()

english_tweets = tweets_df.loc[tweets_df['language'] == 'en']
english_tweets.head()
no_features = 900
documents = english_tweets['text']
documents

from sklearn.feature_extraction.text import CountVectorizer
no_features = 1000
# First we'll let scikitlearn extract the tokens and clean out the stop words
tf_vectorizer = CountVectorizer(max_df=0.95, min_df=2, max_features=no_features, stop_words='english')

tf = tf_vectorizer.fit_transform(documents)
tf_feature_names = tf_vectorizer.get_feature_names()
tf_feature_names

