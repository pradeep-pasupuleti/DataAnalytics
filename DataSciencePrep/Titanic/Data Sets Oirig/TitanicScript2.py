# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""


import csv as csv 
import numpy as np
import pandas as pd
import pylab as P
from sklearn.ensemble import RandomForestClassifier


testpath = r'C:\Pradeep\Working Set\Consulting\Kaggle\Titanic\Data Sets Oirig\test.csv'
trainpath = r'C:\Pradeep\Working Set\Consulting\Kaggle\Titanic\Data Sets Oirig\train.csv'
csv_file_object = csv.reader(open(trainpath, 'rb')) 
header = csv_file_object.next()  # The next() command just skips the 
                                 # first line which is a header
data=[]                          # Create a variable called 'data'.
for row in csv_file_object:      # Run through each row in the csv file,
    data.append(row)             # adding each row to the data variable
data = np.array(data) 	         # Then convert from a list to an array
			         # Be aware that each item is currently
                                 # a string in this format


df = pd.read_csv(trainpath,header=0)
#print df.head(5)
#print type(df)
#print df.dtypes

#print df.Age[0:10]
#print df.Age.mean()
#print df[ ['Sex', 'Pclass', 'Age'] ]
#print df[df.Age>70] [ ['Sex', 'Pclass', 'Age'] ]
#df[df['Age'] > 60][['Sex', 'Pclass', 'Age', 'Survived']]
#print df[df['Age'].isnull()][['Sex', 'Pclass', 'Age']]
#for i in range(1,4):
#    print i, len(df[ (df['Sex'] == 'male') & (df['Pclass'] == i) ])
    
    
#df['Age'].()
#P.show()
#df['Age'].dropna().(bins=16, range=(0,80), alpha = .5)
#P.show()
df['Gender']=4
#df.info()
#df['Gender'] = df['Sex'].map( lambda x: x[0].upper() )
#print df.Gender[0:10]

df['Gender'] = df['Sex'].map( {'female': 0, 'male': 1} ).astype(int)
#print df.Gender[0:10]
#print df.Embarked[0:10]

df ['NEmbark']=5
df['NEmbark'] = df['Embarked'].dropna().map( {'S': 0, 'C': 1, 'Q':2} ).astype(int)

#print df.NEmbark[0:20]
#print df.Embarked[0:20]
#==============================================================================
#print df.Age.median()

df['AgeFill'] = df['Age']

df.head()
#print df[ df['Age'].isnull() ][['Gender','Pclass','Age','AgeFill']].head(10)

median_ages = np.zeros((2,3))

for i in range(0, 2):
    for j in range(0, 3):
        median_ages[i,j] = df[(df['Gender'] == i) & \
                              (df['Pclass'] == j+1)]['Age'].dropna().median()
 

#print median_ages

for i in range(0, 2):
    for j in range(0, 3):
        df.loc[ (df.Age.isnull()) & (df.Gender == i) & (df.Pclass == j+1),\
                'AgeFill'] = median_ages[i,j]
#print df[ df['Age'].isnull() ][['Gender','Pclass','Age','AgeFill']].head(10)

#df['AgeIsNull'] = pd.isnull(df.Age).astype(int)
#print df.AgeIsNull[0:10]

#df['FamilySize'] = df['SibSp'] + df['Parch']
#print df.FamilySize[0:10]
#df['AgeClass'] = df.AgeFill * df.Pclass
#print df.AgeClass[0:10]
#df.AgeClass.hist()
#P.show()


#print df.dtypes

#print train_data
#==============================================================================
# print df.describe()
# print df.info()
# print np.unique(df['NEmbark'])
#==============================================================================

# All missing Embarked -> just make them embark from most common place
if len(df.Embarked[ df.Embarked.isnull() ]) > 0:
    df.Embarked[ df.Embarked.isnull() ] = df.Embarked.dropna().mode().values

Ports = list(enumerate(np.unique(df['Embarked'])))    # determine all values of Embarked,
Ports_dict = { name : i for i, name in Ports }              # set up a dictionary in the form  Ports : index
df.Embarked = df.Embarked.map( lambda x: Ports_dict[x]).astype(int)     # Convert all Embark strings to int
#print df.info()
df = df.drop(['Name', 'Sex', 'Ticket', 'Cabin', 'Age' , 'PassengerId', 'NEmbark'], axis=1) 
#print df.info()

#Test data
test_df = pd.read_csv(testpath,header=0)

# I need to do the same with the test data now, so that the columns are the same as the training data
# I need to convert all strings to integer classifiers:
# female = 0, Male = 1
test_df['Gender'] = test_df['Sex'].map( {'female': 0, 'male': 1} ).astype(int)

# Embarked from 'C', 'Q', 'S'
# All missing Embarked -> just make them embark from most common place
if len(test_df.Embarked[ test_df.Embarked.isnull() ]) > 0:
    test_df.Embarked[ test_df.Embarked.isnull() ] = test_df.Embarked.dropna().mode().values
# Again convert all Embarked strings to int
test_df.Embarked = test_df.Embarked.map( lambda x: Ports_dict[x]).astype(int)

test_df['AgeFill'] = test_df['Age']

for i in range(0, 2):
    for j in range(0, 3):
        median_ages[i,j] = test_df[(test_df['Gender'] == i) & \
                              (test_df['Pclass'] == j+1)]['Age'].dropna().median()
 

for i in range(0, 2):
    for j in range(0, 3):
        test_df.loc[ (test_df.Age.isnull()) & (test_df.Gender == i) & (test_df.Pclass == j+1),\
                'AgeFill'] = median_ages[i,j]
#print df[ df['Age'].isnull() ][['Gender','Pclass','Age','AgeFill']].head(10)

test_df['AgeIsNull'] = pd.isnull(test_df.Age).astype(int)

# All the missing Fares -> assume median of their respective class
if len(test_df.Fare[ test_df.Fare.isnull() ]) > 0:
    median_fare = np.zeros(3)
    for f in range(0,3):                                              # loop 0 to 2
        median_fare[f] = test_df[ test_df.Pclass == f+1 ]['Fare'].dropna().median()
    for f in range(0,3):                                              # loop 0 to 2
        test_df.loc[ (test_df.Fare.isnull()) & (test_df.Pclass == f+1 ), 'Fare'] = median_fare[f]
# Collect the test data's PassengerIds before dropping it
ids = test_df['PassengerId'].values
# Remove the Name column, Cabin, Ticket, and Sex (since I copied and filled it to Gender)
test_df = test_df.drop(['Name', 'Sex', 'Ticket', 'AgeIsNull','Cabin', 'PassengerId', 'Age'], axis=1) 

print test_df.info()        
print df.info()  
train_data = df.values
test_data = test_df.values

print 'Training...'
forest = RandomForestClassifier(n_estimators=100)
forest = forest.fit( train_data[0::,1::], train_data[0::,0] )
#
print 'Predicting...'
output = forest.predict(test_data).astype(int)
#
#
predictions_file = open("C:\Pradeep\Working Set\Consulting\Kaggle\Titanic\Data Sets Oirig\myfirstforest.csv", "wb")
open_file_object = csv.writer(predictions_file)
open_file_object.writerow(["PassengerId","Survived"])
open_file_object.writerows(zip(ids, output))
predictions_file.close()
print 'Done.'