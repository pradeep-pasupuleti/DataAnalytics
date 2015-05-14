# -*- coding: utf-8 -*-
"""
Created on Tue May 05 13:52:04 2015

@author: SM
"""
import os
import pandas as pd    
import re
import logging
import nltk
tokenizer = nltk.data.load('tokenizers/punkt/english.pickle')
 
   
if __name__ == '__main__':
    train = pd.read_csv(os.path.join(os.path.dirname(__file__), 'data', 'labeledTrainData.tsv'), header=0, \
                    delimiter="\t", quoting=3)
    test = pd.read_csv(os.path.join(os.path.dirname(__file__), 'data', 'testData.tsv'), header=0, delimiter="\t", \
                   quoting=3 )
    unlabeled_train  = pd.read_csv(os.path.join(os.path.dirname(__file__), 'data', 'unlabeledTrainData.tsv'), header=0, delimiter="\t", \
                   quoting=3 )
                  # Verify the number of reviews that were read (100,000 in total)
    print "Read %d labeled train reviews, %d labeled test reviews, " \
     "and %d unlabeled reviews\n" % (train["review"].size,  
     test["review"].size, unlabeled_train["review"].size )
#    print  train["review"][0]
     
    sentences = []  # Initialize an empty list of sentences

    print "Parsing sentences from training set"
    for review in train["review"]:
        sentences += KaggleWord2VecUtility.review_to_sentences(review, tokenizer)
    
    print "Parsing sentences from unlabeled set"
    for review in unlabeled_train["review"]:
        sentences += KaggleWord2VecUtility.review_to_sentences(review, tokenizer)
        
    # Check how many sentences we have in total - should be around 850,000+
    print len(sentences)
    print sentences[0]
    print sentences[1]

# Import the built-in logging module and configure it so that Word2Vec 
# creates nice output messages
   
    logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s',\
        level=logging.INFO)
    
    # Set values for various parameters
    num_features = 300    # Word vector dimensionality                      
    min_word_count = 40   # Minimum word count                        
    num_workers = 4       # Number of threads to run in parallel
    context = 10          # Context window size                                                                                    
    downsampling = 1e-3   # Downsample setting for frequent words
    
    # Initialize and train the model (this will take some time)
    from gensim.models import word2vec
    print "Training model with Word2Vec................................."
    model = word2vec.Word2Vec(sentences, workers=num_workers, \
                size=num_features, min_count = min_word_count, \
                window = context, sample = downsampling)
    
    # If you don't plan to train the model any further, calling 
    # init_sims will make the model much more memory-efficient.
    model.init_sims(replace=True)
    
    # It can be helpful to create a meaningful model name and 
    # save the model for later use. You can load it later using Word2Vec.load()
    model_name = "300features_40minwords_10context"
    model.save(model_name)
    print "finding sentiment from model with Word2Vec........................"
    model.most_similar("awful")