# -*- coding: utf-8 -*-
"""
Created on Wed May 06 14:00:15 2015

@author: SM
"""


import os
import pandas as pd    
import re
import logging
import gensim

logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s',\
        level=logging.INFO)

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
 
   
        # Initialize and train the model (this will take some time)
    from gensim.models import Word2Vec
    model = Word2Vec.load("300features_40minwords_10context")
    print type(model.syn0)
    print model.syn0.shape
    
    # ****************************************************************
# Calculate average feature vectors for training and testing sets,
# using the functions we defined above. Notice that we now use stop word
# removal.

    clean_train_reviews = []
    for review in train["review"]:
        clean_train_reviews.append( KaggleWord2VecUtility.review_to_wordlist( review, \
            remove_stopwords=True ))
    
    trainDataVecs = KaggleWord2VecUtility.getAvgFeatureVecs( clean_train_reviews, model, num_features )
    
    print "Creating average feature vecs for test reviews"
    clean_test_reviews = []
    for review in test["review"]:
        clean_test_reviews.append( KaggleWord2VecUtility.review_to_wordlist( review, \
            remove_stopwords=True ))
    
    testDataVecs = KaggleWord2VecUtility.getAvgFeatureVecs( clean_test_reviews, model, num_features )
        
        # Fit a random forest to the training data, using 100 trees
    from sklearn.ensemble import RandomForestClassifier
    forest = RandomForestClassifier( n_estimators = 100 )
    
    print "Fitting a random forest to labeled training data..."
    forest = forest.fit( trainDataVecs, train["sentiment"] )
    
    # Test & extract results 
    result = forest.predict( testDataVecs )
    
    # Write the test results 
    output = pd.DataFrame( data={"id":test["id"], "sentiment":result} )
    output.to_csv( "Word2Vec_AverageVectors.csv", index=False, quoting=3 )