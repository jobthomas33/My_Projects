The dataset consisited of 790 past loan customer cases, with 14 attributes for each case, including attributes such as financial standing, reason for the loan, employment, 
demographic information, foreign national, years residence in the district and the outcome/label variable  Credit Standing - classifying each case as either a good loan
or bad loan.
The goal was to build a classifier that classifies  13 new customers,into good or bad for prospective loans. 
Decision tree algorithm was built from scratch using R without any package. Each split on the node was determined using entropy calculations. Later this was verified 
using decision tree package in R. Also random forest model was implemented with cross validation to improve the accuracy further. 
Accuracy = 78.23%, Recall = 67.9%,  Precision = 75.7%
Also an analysis was conduted to check if a human error occured in deetermining the credit worthiness of customers deliberately.
