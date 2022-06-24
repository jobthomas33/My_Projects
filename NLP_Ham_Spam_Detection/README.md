# Ham or Spam detection of emails usinf Natural Language Processing
The dataset used for this project is the Enron email dataset. http://www.aueb.gr/users/ion/data/enron-spam/ . The dataset is a 
collection of public domain emails from the Enron corporation. It consisted of 5172 emails. The emails have been manually classified as spam and non-spam. 
The primary goal of this project is to create a supervised classification pipeline to classify emails as spam or non-spam from the training data.
All the implementation is done using Python programming language

### Text Pre-processing
lower case conversion, removal of punctuations, Tokenization, stop words removal, stemming.
After pre processing the data was split into training and testing data sets. Sklearn train_test_split functionality was used for this. 30% of the data was set
aside for testing purpose of the model.

### Data Exploration
The average length of Spam messages was found to be 780, while the average length of non-spam/ham messages were found to be 497. Outliers were checked. 
Most frequently occuring words in Ham and Spam emails were checked.

### Feature Extraction
Both Bag of words model using CountVectorizer and  TF-IDF model using  TfidfVectorizer was conducted. 

### Model Selection
For model selection the transformed training data is given to 4 different classification models for comparison. The models selected were Logistic Regression,
Multinomial Naïve Bayes, Support vector Machine and Decision tree. K-Fold cross validation was used to evaluate the performance of the model.
The models were compared using both CountVectorized data and TfidfVectorized data.  Logistic Regression or SVM models had the highest training accuracy with TfidfVectorizer.

### Hyper parameter Tuning
A model pipeline was created using  TfidfVectorizer and GridSearchCv with cross validation. SVM model was found to be the best model 
with a training accuracy of 99.84%.

### Model Evaluation
The best model SVM was used to for predictions using test data, and it gave a test accuracy of 98.73%. Recall was found to be 98.63%.  Precision was found to be 97.07%.
Classification error was found to be 1.27%. The area under the ROC-AUC curve was found out to be 99.83%.
