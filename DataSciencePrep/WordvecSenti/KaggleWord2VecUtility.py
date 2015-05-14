# -*- coding: utf-8 -*-
"""
Created on Mon May 04 13:00:27 2015

@author: SM
"""

#!/usr/bin/env python

import re
import nltk

import pandas as pd
import numpy as np

from bs4 import BeautifulSoup
from nltk.corpus import stopwords


class KaggleWord2VecUtility(object):
    """KaggleWord2VecUtility is a utility class for processing raw HTML text into segments for further learning"""

    @staticmethod
    def review_to_wordlist( review, remove_stopwords=False ):
        # Function to convert a document to a sequence of words,
        # optionally removing stop words.  Returns a list of words.
        #
        # 1. Remove HTML
        review_text = BeautifulSoup(review).get_text()
        #
        # 2. Remove non-letters
        review_text = re.sub("[^a-zA-Z]"," ", review_text)
        #
        # 3. Convert words to lower case and split them
        words = review_text.lower().split()
        #
        # 4. Optionally remove stop words (false by default)
        if remove_stopwords:
            stops = set(stopwords.words("english"))
            words = [w for w in words if not w in stops]
        #
        # 5. Return a list of words
        return(words)

    # Define a function to split a review into parsed sentences
    @staticmethod
    def review_to_sentences( review, tokenizer, remove_stopwords=False ):
        # Function to split a review into parsed sentences. Returns a
        # list of sentences, where each sentence is a list of words
        #
        # 1. Use the NLTK tokenizer to split the paragraph into sentences
        raw_sentences = tokenizer.tokenize(review.decode('utf8').strip())
        #
        # 2. Loop over each sentence
        sentences = []
        for raw_sentence in raw_sentences:
            # If a sentence is empty, skip it
            if len(raw_sentence) > 0:
                # Otherwise, call review_to_wordlist to get a list of words
                sentences.append( KaggleWord2VecUtility.review_to_wordlist( raw_sentence, \
                  remove_stopwords ))
        #
        # Return the list of sentences (each sentence is a list of words,
        # so this returns a list of lists
        return sentences
    @staticmethod        
    def makeFeatureVec(words, model, num_features):
        # Function to average all of the word vectors in a given
        # paragraph
        #
        # Pre-initialize an empty numpy array (for speed)
        featureVec = np.zeros((num_features,),dtype="float32")
        #
        nwords = 0.
        # 
        # Index2word is a list that contains the names of the words in 
        # the model's vocabulary. Convert it to a set, for speed 
        index2word_set = set(model.index2word)
        #
        # Loop over each word in the review and, if it is in the model's
        # vocaublary, add its feature vector to the total
        for word in words:
            if word in index2word_set: 
                nwords = nwords + 1.
                featureVec = np.add(featureVec,model[word])
        # 
        # Divide the result by the number of words to get the average
        featureVec = np.divide(featureVec,nwords)
        return featureVec
    
    @staticmethod
    def getAvgFeatureVecs(reviews, model, num_features):
        # Given a set of reviews (each one a list of words), calculate 
        # the average feature vector for each one and return a 2D numpy array 
        # 
        # Initialize a counter
        counter = 0.
        # 
        # Preallocate a 2D numpy array, for speed
        reviewFeatureVecs = np.zeros((len(reviews),num_features),dtype="float32")
        # 
        # Loop through the reviews
        for review in reviews:
           #
           # Print a status message every 1000th review
           if counter%1000. == 0.:
               print "Review %d of %d" % (counter, len(reviews))
           # 
           # Call the function (defined above) that makes average feature vectors
           reviewFeatureVecs[counter] = makeFeatureVec(review, model, \
               num_features)
           #
           # Increment the counter
           counter = counter + 1.
        return reviewFeatureVecs
    @staticmethod
    def create_bag_of_centroids( wordlist, word_centroid_map ):
        #
        # The number of clusters is equal to the highest cluster index
        # in the word / centroid map
        num_centroids = max( word_centroid_map.values() ) + 1
        #
        # Pre-allocate the bag of centroids vector (for speed)
        bag_of_centroids = np.zeros( num_centroids, dtype="float32" )
        #
        # Loop over the words in the review. If the word is in the vocabulary,
        # find which cluster it belongs to, and increment that cluster count 
        # by one
        for word in wordlist:
            if word in word_centroid_map:
                index = word_centroid_map[word]
                bag_of_centroids[index] += 1
        #
        # Return the "bag of centroids"
        return bag_of_centroids