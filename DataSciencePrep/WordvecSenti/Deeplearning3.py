# -*- coding: utf-8 -*-
"""
Created on Wed May 06 14:44:34 2015

@author: SM
"""

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
import time

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
    print "Creating Kmeans"
    from sklearn.cluster import KMeans
    import time
    
    start = time.time() # Start time
    
    # Set "k" (num_clusters) to be 1/5th of the vocabulary size, or an
    # average of 5 words per cluster
    word_vectors = model.syn0
    num_clusters = word_vectors.shape[0] / 5
    
    # Initalize a k-means object and use it to extract centroids
    kmeans_clustering = KMeans( n_clusters = num_clusters )
    idx = kmeans_clustering.fit_predict( word_vectors )
    
    # Get the end time and print how long the process took
    end = time.time()
    elapsed = end - start
    print "Time taken for K Means clustering: ", elapsed, "seconds."
    
    # Create a Word / Index dictionary, mapping each vocabulary word to
# a cluster number                                                                                            
    word_centroid_map = dict(zip( model.index2word, idx ))
    
    # For the first 10 clusters
    for cluster in xrange(0,10):
        #
        # Print the cluster number  
        print "\nCluster %d" % cluster
        #
        # Find all of the words for that cluster number, and print them out
        words = []
        for i in xrange(0,len(word_centroid_map.values())):
            if( word_centroid_map.values()[i] == cluster ):
                words.append(word_centroid_map.keys()[i])
        print words
 
    print "Creating training centroids "
    # Pre-allocate an array for the training set bags of centroids (for speed)
    train_centroids = np.zeros( (train["review"].size, num_clusters), \
        dtype="float32" )
    
    # Transform the training set reviews into bags of centroids
    counter = 0
    for review in clean_train_reviews:
        train_centroids[counter] = KaggleWord2VecUtility.create_bag_of_centroids( review, \
            word_centroid_map )
        counter += 1
    
    print "Creating testing centroids "
    # Repeat for test reviews 
    test_centroids = np.zeros(( test["review"].size, num_clusters), \
        dtype="float32" )
    
    counter = 0
    for review in clean_test_reviews:
        test_centroids[counter] = KaggleWord2VecUtility.create_bag_of_centroids( review, \
            word_centroid_map )
        counter += 1
         
         # Fit a random forest and extract predictions 
    print "Fit a random forest and extract predictions "        
    forest = RandomForestClassifier(n_estimators = 100)
    
    # Fitting the forest may take a few minutes
    print "Fitting a random forest to labeled training data..."
    forest = forest.fit(train_centroids,train["sentiment"])
    result = forest.predict(test_centroids)
    
    # Write the test results 
    
    output = pd.DataFrame(data={"id":test["id"], "sentiment":result})
    output.to_csv( "BagOfCentroids.csv", index=False, quoting=3 )


