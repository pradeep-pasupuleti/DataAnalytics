# -*- coding: utf-8 -*-
"""
Created on Mon May 04 14:07:02 2015

@author: SM
"""
from bs4 import BeautifulSoup
import pandas as pd    
import re
    
if __name__ == '__main__':
    train = pd.read_csv(os.path.join(os.path.dirname(__file__), 'data', 'labeledTrainData.tsv'), header=0, \
                    delimiter="\t", quoting=3)
    test = pd.read_csv(os.path.join(os.path.dirname(__file__), 'data', 'testData.tsv'), header=0, delimiter="\t", \
                   quoting=3 )
                  
#    print  train["review"][0]
           
    # Get the number of reviews based on the dataframe column size
    num_reviews = train["review"].size
    
    # Initialize an empty list to hold the clean reviews
    clean_train_reviews = []
    print "Cleaning and parsing the training set movie reviews...\n"
    clean_train_reviews = []
    for i in xrange( 0, num_reviews ):
        # If the index is evenly divisible by 1000, print a message
        if( (i+1)%1000 == 0 ):
            print "Review %d of %d\n" % ( i+1, num_reviews )                                                                    
        clean_train_reviews.append(Word2VecUtility.review_to_words( train["review"][i] ))
    
    print "Creating the bag of words...\n"
    from sklearn.feature_extraction.text import CountVectorizer
    
    # Initialize the "CountVectorizer" object, which is scikit-learn's
    # bag of words tool.  
    vectorizer = CountVectorizer(analyzer = "word",   \
                                 tokenizer = None,    \
                                 preprocessor = None, \
                                 stop_words = None,   \
                                 max_features = 5000) 
    
    # fit_transform() does two functions: First, it fits the model
    # and learns the vocabulary; second, it transforms our training data
    # into feature vectors. The input to fit_transform should be a list of 
    # strings.
    train_data_features = vectorizer.fit_transform(clean_train_reviews)
    
    # Numpy arrays are easy to work with, so convert the result to an 
    # array
    train_data_features = train_data_features.toarray()     
    
    print "train_data_features.shape"
    print train_data_features.shape
    # Take a look at the words in the vocabulary
    vocab = vectorizer.get_feature_names()
#    print vocab
    # Sum up the counts of each vocabulary word
    dist = np.sum(train_data_features, axis=0)
    
    # For each, print the vocabulary word and the number of times it 
    # appears in the training set
#    for tag, count in zip(vocab, dist):
#        print count, tag
    
    print "Training the random forest..."
    from sklearn.ensemble import RandomForestClassifier
    
    # Initialize a Random Forest classifier with 100 trees
    forest = RandomForestClassifier(n_estimators = 100) 
    
    # Fit the forest to the training set, using the bag of words as 
    # features and the sentiment labels as the response variable
    #
    # This may take a few minutes to run
    forest = forest.fit( train_data_features, train["sentiment"] )
    
    # Verify that there are 25,000 rows and 2 columns
    print "test initial shape"    
    print test.shape
    
    # Create an empty list and append the clean reviews one by one
    num_reviews = len(test["review"])
    clean_test_reviews = [] 
    
    print "Cleaning and parsing the test set movie reviews...\n"
    for i in xrange(0,num_reviews):
        if( (i+1) % 1000 == 0 ):
            print "Review %d of %d\n" % (i+1, num_reviews)
        clean_review = Word2VecUtility.review_to_words( test["review"][i] )
        clean_test_reviews.append( clean_review )
    
    # Get a bag of words for the test set, and convert to a numpy array
    test_data_features = vectorizer.transform(clean_test_reviews)
    test_data_features = test_data_features.toarray()
    print "test final shape"
    print test_data_features.shape
    print "Predicting  the random forest on the test data..."
    # Use the random forest to make sentiment label predictions
    result = forest.predict(test_data_features)
    
    # Copy the results to a pandas dataframe with an "id" column and
    # a "sentiment" column
    output = pd.DataFrame( data={"id":test["id"], "sentiment":result} )
    print "Writing the prediction into a csv..."
    # Use pandas to write the comma-separated output file
    output.to_csv( "Bag_of_Words_model.csv", index=False, quoting=3 )
    print "Writing the prediction into a csv..."
#==============================================================================
#     print train.shape
#     print train.columns.values
# #    print train["review"][0]
#     example1 = BeautifulSoup(train["review"][0])  
# 
# # Print the raw review and then the output of get_text(), for 
# # comparison
# 
# #    print example1.get_text()
# 
# # Use regular expressions to do a find-and-replace
#     letters_only = re.sub("[^a-zA-Z]",           # The pattern to search for
#                       " ",                   # The pattern to replace it with
#                       example1.get_text() )  # The text to search
# #    print letters_only
#     lower_case = letters_only.lower()        # Convert to lower case
#     words = lower_case.split()               # Split into words
#     print words
#     from nltk.corpus import stopwords # Import the stop word list
#     print stopwords.words("english") 
#     # Remove stop words from "words"
#     words = [w for w in words if not w in stopwords.words("english")]
     # print words
#==============================================================================
   