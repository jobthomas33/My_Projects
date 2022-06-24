# Automatic Detection of Eligibility for Loan
The dataset of loan status was obtained from Kaggle. The dataset obtained consists of 13 featuresand 614 data points. The features include
Loan Id: A unique loan number assigned toeach customer. Gender, Marital Status,Number of dependents, Education level,Employment type, property area: A
customer’s basic demographic information.Applicant Income, Co-applicant Income:Income of applicants and co-applicants. LoanAmount: The amount applied for loan. Credit
History: Past history of credits by applicants.Target feature - Loan Status: Whether the loan paid off or not.

The dataset went through several preprocessing steps.Imputation of missing values included multiple imputations by chained equations(MICE). 
Square root transformation in numerical variables to avoid skewness was also performed. Both label encoding andone hot encoding was used to encode categorical variables. 
Scaling of numerical variables was done using minmaxscaler. As 68% of the data belonged to one target class SMOTE technique was used to take care of the dataset imbalance. 

Logistic Regression, K Nearest Neighbor, Decision Tree, Support Vector Machine and Random Forest classifiers were initially considered for building the model. 
Out of these Random Forest classifiers gave the best training accuracy. It was further optimized using hyper parameter tuning. Finally, the model gave an 
accuracy of 75.6% with test data. It also gave a precision of 69%, recall of 71% and f1-score of 70% on test data.

