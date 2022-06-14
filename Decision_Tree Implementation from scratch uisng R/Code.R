rm(list = ls())                                                           #Removing environment variables
setwd("C:/Users/bless/OneDrive/Desktop/MTU Studies/DSA/Assignment")       #Setting working Directory

#########################Installation of Packages#############################
#install.packages("readxl")
#install.packages("skimr")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("e1071")
#install.packages("tree")
#install.packages("randomForest")
#install.packages("caret")
#install.packages("ggcorrplot")
#install.packages("pROC")
library(readxl)                                                        #reading excel file
library(skimr)                                                         #detailed summary analysis
library(tidyverse)                                                     #data manipulation
library(ggplot2)                                                       #data visualization
library(tidyr)                                                         #data manipulation
library(dplyr)                                                         #data manipulation
library(e1071)                                                         # for skewness and kurtosis
library(tree)                                                          #implementing decision tree algorithm
library(randomForest)                                                  #implementing random forest algorithm
library(caret)                                                         #confusion matrix
library(pROC)                                                          # to get ROC-AUC curve
#########################Reading data into a data frame##############
df <- read_excel("Credit_Risk7_final.xlsx",sheet="Training_Data")
credit_risk_data <- as.data.frame(unclass(df),stringsAsFactors = TRUE) #converting to data frame and
                                                                       #Making character datatype variables to factor for plotting and modelling
#Converting names of variables to shorter, convenient names
names(credit_risk_data)[names(credit_risk_data) == 'Residence.Time..In.current.district.'] <- 'Residence.Time'
names(credit_risk_data)[names(credit_risk_data) == 'Months.since.Checking.Acct.opened'] <- 'Acct.opened.Months'
skim(credit_risk_data)                                                 #Plotting summary statistics
str(credit_risk_data)                                                  #checking data type
head(credit_risk_data)                                                 #checking first few records
#View(credit_risk_data)
summary(credit_risk_data)                                              # taking summary

attach(credit_risk_data)                                               # attaching data frame for calling variables easily


######EXPLORATORY DATA ANALYSIS######

#####Summary statistics ######
###Age
summary(Age)                                                           # Mean> Median. slightly Right skewed data.

skewness(Age,na.rm=T,type=1)                                           # SKEWNESS>0 . Data is skewed to right
kurtosis(Age,na.rm=T)                                                  # greater than 0, hence distribution has higher peak than normal

# calculating Standard error 
std <- function(x) sd(x)/sqrt(length(x))                               # dividing standard deviation by square root of length of variable
std(Age)                                                               
mean(credit_risk_data$Age,na.rm=T)
sd(credit_risk_data$Age,na.rm=T)                                       # standard deviation of age

hist(Age,main="histogram of Age",xlab = "bins of Age",col="steelblue")  # histogram of age
# Most people have age between 20 and 40
stem(Age, width = 100)                                                  # Stem plot of age

boxplot(Age~Credit.Standing,ylab="Age",
        main="Box plot of Age ",col=c("steelblue"),border="black")      # boxplot of age vs credit standing
                                                                        # many extreme outliers exist in data
###################Residence Time
summary(Residence.Time)                                                 # negative value observed
sd(Residence.Time)
skewness(Residence.Time,na.rm=T,type=1)                                 # close to normal
kurtosis(Residence.Time,na.rm=T)                                        # high peak
m<- mean(credit_risk_data$Residence.Time,na.rm=T)

hist(Residence.Time,main="Histogram of Residence time in Current District",
     xlab = "Bins of Residence time",col="steelblue")
stem(Residence.Time, width = 100)
# most people are staying in the district between 1-4 years. Most people are new
boxplot(Residence.Time~Credit.Standing,ylab="Residence.Time",
        main="Box plot of Residence.Time ",col=c("steelblue"),border="black")

###################Account openend months
summary(Acct.opened.Months)
sd(Acct.opened.Months)
skewness(Acct.opened.Months,na.rm=T,type=1)  # SKEWNESS>0 . Data is skewed to right
kurtosis(Acct.opened.Months,na.rm=T) # very high peak
m<- mean(credit_risk_data$Acct.opened.Months,na.rm=T)
m
hist(Acct.opened.Months,main="Histogram of Months since account opened",
     xlab = "Bins of months",col="lightgreen")
stem(Acct.opened.Months, width = 100)

# Months since account opened is mostly between 0-40. most of them are recently opened accounts

boxplot(Acct.opened.Months~Credit.Standing,ylab="Acct.opened.Months",
        main="Box plot of Acct.opened.Months ",col=c("steelblue"),border="black")
# outliers exist

####Uni variate Analysis                                      # contigency table and analysis on all variables
(t <- table(Credit.Standing))                                 # table on target variable, slightly imbalanced data. more customers are in good class
prop.table(table(credit_risk_data$Checking.Acct))             # few customers have High checking acct
prop.table(table(credit_risk_data$Credit.History))            # more than 50% have current credit standing
prop.table(table(credit_risk_data$Loan.Reason))               # 25% customers applied for new car
prop.table(table(credit_risk_data$Employment))                # very few unemployed customers. Half of customers belong to short and very short
prop.table(table(credit_risk_data$Savings.Acct))              # more than 60% have low savings acct
prop.table(table(credit_risk_data$Personal.Status))           # more than half the customers are single
prop.table(table(credit_risk_data$Housing))                   # 67% customers have own housing
prop.table(table(credit_risk_data$Job.Type))                  # 63% customers are skilled             
prop.table(table(credit_risk_data$Foreign.National))          # 68% customers are foreign national

####Bi variate analysis with target variable######################################################

# checking acct vs credit standing

ggplot(credit_risk_data, aes(Checking.Acct, ..count..)) +               # Taking count of customers with checking acct using ggplot
  geom_bar(aes(fill = Credit.Standing), position = "dodge") + ylab("No of customers") +       # plotting bar graph. Based on credit standing, number of customers in each checking acct category
  labs(title = "Barchart of Checking.Acct vs Credit.Standing") + theme(plot.title = element_text(hjust = 0.5))  # adjusting title to the middle of plot
# Large number of accounts are no account with good credit standing.
# Number of high accounts is very less

# credit history vs credit standing ***
ggplot(credit_risk_data, aes(Credit.History, ..count..)) +             # Taking count of customers with credit history using ggplot
  geom_bar(aes(fill = Credit.Standing), position = "dodge") + ylab("No of customers") +
  labs(title = "Barchart of Credit History vs Credit Standing") + theme(plot.title = element_text(hjust = 0.5))

# Target variable credit standing is highly correlated with credit history.
# chi-sqared test
tbl <- table(Credit.History, Credit.Standing)
chi <- chisq.test(tbl)
c(chi$statistic, chi$p.value) # p value is very less than significance level. There is strong association


# credit history vs checking acct
ggplot(credit_risk_data, aes(Credit.History, ..count..)) +
  geom_bar(aes(fill = Checking.Acct), position = "dodge") + ylab("No of customers") +
  labs(title = "Barchart of Credit History vs Checking.Acct") + theme(plot.title = element_text(hjust = 0.5))
# most 0balance accounts are current . all paid mostly by no account


# Loan vs credit standing
ggplot(credit_risk_data, aes(Loan.Reason, ..count..)) +
  geom_bar(aes(fill = Credit.Standing), position = "dodge")
# bad credit standings are more than good in education loan. Car and furniture loans have fairly large number of bad credit standings.


# savings accnt vs credit standing
ggplot(credit_risk_data, aes(Savings.Acct, ..count..)) +
  geom_bar(aes(fill = Credit.Standing), position = "dodge")
# Most data is in low accounts. Hence hard to get into conclusions


# Employment vs credit standing ***
ggplot(credit_risk_data, aes(Employment, ..count..)) +
  geom_bar(aes(fill = Credit.Standing), position = "dodge") + ylab("No of customers") +
  labs(title = "Barchart of Employment vs Credit Standing") + theme(plot.title = element_text(hjust = 0.5))
# customers with short employment have more bad credit standing and the ones with long employment have more good credit standings


# Personal status vs credit standing
ggplot(credit_risk_data, aes(Personal.Status, ..count..)) +
  geom_bar(aes(fill = Credit.Standing), position = "dodge") + ylab("No of customers") +
  labs(title = "Barchart of Personal Status vs Credit Standing") + theme(plot.title = element_text(hjust = 0.5))
# Most data is single people and have good credit


# Housing vs credit standing
ggplot(credit_risk_data, aes(Housing, ..count..)) +
  geom_bar(aes(fill = Credit.Standing), position = "dodge") + ylab("No of customers") +
  labs(title = "Barchart of Housing vs Credit Standing") + theme(plot.title = element_text(hjust = 0.5))
# Most are own housing and have good credit standing


# Job.Type vs credit standing
ggplot(credit_risk_data, aes(Job.Type, ..count..)) +
  geom_bar(aes(fill = Credit.Standing), position = "dodge") + ylab("No of customers") +
  labs(title = "Barchart of Job Type vs Credit Standing") + theme(plot.title = element_text(hjust = 0.5))
# most are skilled . They have fairly high number of good and bad credit standings



# Foreign national vs credit standing
ggplot(credit_risk_data, aes(Foreign.National, ..count..)) +
  geom_bar(aes(fill = Credit.Standing), position = "dodge") + ylab("No of customers") +
  labs(title = "Barchart of Foreign National vs Credit Standing") + theme(plot.title = element_text(hjust = 0.5))
# most are foreign national and have high number of good and  bad credit standings


# months since account opened vs credit standing  (Box plot)
ggplot.data <- ggplot(credit_risk_data)
ggplot.data +
  geom_boxplot(aes(x = Credit.Standing, y = Acct.opened.Months, fill = Credit.Standing))
+ labs(title = "Boxplot of months since account opened vs credit standing")
# presence of extreme outliers in good and bad credit standings


# Residence time in current district vs credit standing
ggplot.data +
  geom_boxplot(aes(x = Credit.Standing, y = Residence.Time, fill = Credit.Standing))
+ labs(title = "Boxplot of Residence time in current district vs credit standing")
# outliers are present


# Age vs credit standing
ggplot.data +
  geom_boxplot(aes(x = Credit.Standing, y = Age, fill = Credit.Standing))
+ labs(title = "Age vs credit standing")
# extreme outliers observed in both categories

#################################### Tri variate analysis #############################3

# credit standing, Job type, credit history (2 categorical variables vs target variable)
ggplot(data = credit_risk_data) +    # bar plot using ggplot. credit history on color. credit standing on x axis.
  geom_bar(
    aes(x = Credit.Standing, fill = Credit.History),
    stat = "count",
    postion = position_dodge()
  ) + # position_dodge() for adjusting the position of bars and overlaps
  facet_grid(Job.Type ~ .) + ylab("No of Customers") +               # separated by job type using facet grid
  labs(title = "Credit History & Job Type vs Credit Standing") + theme(plot.title = element_text(hjust = 0.5)) # justifying title to center
# a lot of all paid customers are skilled and have good credit standing

# credit standing, Employment, credit history (2 categorical variables vs target variable)
ggplot(data = credit_risk_data) +    # bar plot using ggplot. credit history on color. credit standing on x axis.
  geom_bar(aes(x = Credit.Standing, fill = Employment),
           stat = "count",
           postion = position_dodge()) +  # position_dodge() for adjusting the position of bars and overlaps
  facet_grid(Credit.History ~ .) + ylab("No of Customers") + # separated by Employment using facet grid
  labs(title = "Credit History & Employment vs Credit Standing")+
  theme(plot.title = element_text(hjust = 0.5)) # justifying title to center


# Age , Acct opened in months, credit standing ( 2 numerical variables vs target)
ggplot(data = credit_risk_data,                  # Age on x axis, account opened in y axis
       aes(x = Age, y = Acct.opened.Months, color = Credit.Standing)) +
  geom_point(size = 3, alpha = .6) +
  labs(title = "Credit Standing by Age and Months since account opened") +
  theme(plot.title = element_text(hjust = 0.5))

#Account opened, Residence time , credit standing ( 2 numerical variables vs target)
ggplot(data = credit_risk_data,
       aes(x = Acct.opened.Months, y = Residence.Time, color = Credit.Standing)) +
  geom_point(size = 3, alpha = .6) +
  labs(title = "credit standing by age and Months since account opened")



 ##################### Dealing with missing and invalid values ###################

is.na(credit_risk_data)                   # checking fo NA values
any(is.na(credit_risk_data))              # any NA values returned true
skim(credit_risk_data)                    # missing values found in employment, personal status and housing

#calculating mode
getmode.credit <- function(v) {
  uniqv.credit <- unique(v)                                # getting unique values in an attribute
  uniqv.credit[which.max(tabulate(match(v, uniqv.credit)))] # tabulate() counts the frequency of occurrence of element inside match()
                                                            # match() gives the position of matches of first argument with second
}

# REPLACING negative value in residence time  with mode of residence time (4)
credit_risk_data$Residence.Time[credit_risk_data$Residence.Time<0] <- getmode.credit(credit_risk_data$Residence.Time)

# Replacing outlier age 99 with median 32 years
credit_risk_data$Age[credit_risk_data$Age==99] <- median(credit_risk_data$Age)



##Dropping missing values
#library(tidyr)
credit_risk_data <- credit_risk_data %>% 
  drop_na(Employment,Personal.Status,Housing )             # Function to drop NA values
any(is.na(credit_risk_data))                               # Returned false . No NA values present
       


Data_ID <- credit_risk_data[,1]                            # Storing ID values in a separate variable
Data_ID <- as.data.frame(Data_ID)                          # making it a data fame of ID values
credit_risk_data <- credit_risk_data[,-1]                  # remove the first column, this is the ID column. Id values is not providing any value in analysis. Hence dropped

dim(credit_risk_data)                                    # checking new dimension after EDA. 769 rows, 13 columns
head(credit_risk_data)
#View(credit_risk_data)
# correlation
library(ggcorrplot)
model.matrix(~0+., data=credit_risk_data) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)
# Does not see any multicollinearity between predictor variables ( no major correaltion between variables)
# variables correlated and bringing same information to a model can lead to poor performance of the model


############################B. SPLITTING DATASET#########################################


#taking  75% rows
(train_size <- floor(0.75*nrow(credit_risk_data)))            # specifying 75% of number of rows in data set. floor() returns largest integer less than or equal to  the decimal number obtained
set.seed(427)                                                 # specifying a constant number in seed() to get the same data points and results for each run
train_credit_risk_data <- sample(1:nrow(credit_risk_data), size=train_size) # generating random numbers from 1 to number of rows in the data set with 75% size.

# splitting data into train and test
credit_train <- credit_risk_data[train_credit_risk_data,]     #All row numbers randomly generated into the training set
credit_test <- credit_risk_data[-train_credit_risk_data,]     # Everything except random number of rows into test set. 25% of data

nrow(credit_train);ncol(credit_train) # 75% (576,13)         # checking dimension of train and test set
nrow(credit_test);ncol(credit_test) # 25% (193,13)    

##########################C. Taking categorical predictor variables for training###############################################
str(credit_risk_data)                               # checking for data types


train_data_categorical <-credit_train[,-c(10,11,12)]  # removing numerical columns from the data set
class(train_data_categorical)                         # checking if the class has changed
#View(train_data_categorical)
nrow(train_data_categorical);ncol(train_data_categorical) # (576,10)  # checking the dimension of data set
############################################



## calculating entropy at root level ( credit standing)
Credit.Standing.tot <- prop.table(table(credit_train$Credit.Standing))   # calculating prop table of credit standing. This gives probability (p) for each class
(credit_entropy_tot<-sum(-Credit.Standing.tot*log(Credit.Standing.tot,2))) # using entropy formula (-p * log p) for each class and summing it up element wise
# Entropy (E) = 0.9798688

# checking the working of entropy formula  with 1 st predictor and target
prob_val1 <- prop.table(table(train_data_categorical[,1],train_data_categorical$Credit.Standing),1)

#calculating probability of each category of  checking account with each class of credit standing. margin of 1 is given to 
#get probability for each class per category of checking account (probability of high accounts with good or probability of high accounts with bad class)

sum(prop.table(table(train_data_categorical[,1]))*rowSums(-prob_val1*log(prob_val1,2))) 

# using entropy formula (-p * log p) for each category (Example- probability of high accounts with good class and then the same with bad classes is used in entropy formula) 
# This is summed up using rowsum(). Each category of checking account is multiplied by its weight.
# Weight is the number of observations of a category. Example- number of high accounts out of total records.
# likewise , values obtained for each category of an attribute is summed up using sum() 
# Entropy of checking acct is 0.9438

#function to calculate entropy for each attribute
credit_entropy_node <- function(x){         # function receives column number as argument and does entropy calculations just as mentioned above for each column
  prob_val<- prop.table(table(train_data_categorical[,x],train_data_categorical$Credit.Standing) + 1e-6, margin = 1) # laplace smoothening applied. A very small number added to probability to avoid getting zeroes
  sum(prop.table(table(train_data_categorical[,x]))*rowSums(-prob_val*log(prob_val,2)))
}

# Looping through all the training categorical variables to find the one with minimum entropy
x=ncol(train_data_categorical)-1             # one minus number of columns. To avoid target variable
entropy_vec <- NULL                          # initializing a null vector
for (i in 1:x) {                             # to iterate through each column
  entropy_val <- credit_entropy_node(i)      # passing column number as parameter. Stores the entropy value returned in a variable
  entropy_vec <- c(entropy_vec,entropy_val)  # appending entropy value to the vector defined
  if(i==ncol(train_data_categorical)-1){     # if last column is reached before credit standing/ target
    print(c("Entropy minimum is for ",colnames(train_data_categorical[which.min(entropy_vec)]),round(min(entropy_vec),5))) # calculating minimum value of entropy 
    #and the attribute name corresponding to it.Rounding entropy value to 5 decimal places
    print(c("Information Gain maximum is",round(credit_entropy_tot-min(entropy_vec),5))) # Information gain calculated by subtracting the least found entropy value with the total entropy of credit standing found earlier
  }
}
# "Credit.History"          "0.71681"  
#"Information Gain maximum is" "0.26306"
# Hence first node split is credit history, which supports EDA

# getting all entropy values and predictor variables in a data frame
entropy_table <- data.frame(cbind(colnames(train_data_categorical[-10]),round(entropy_vec,3))) # binding all column names and its entropy values except target column
colnames(entropy_table) <- c("Predictor.Variable", "entropy.value")  # changing column names of new data frame

ggplot(data=entropy_table,aes(x=Predictor.Variable,y=entropy.value))+  # plotting entropy values
  geom_point(size = 3, alpha = .6)+ylab("Entropy")+xlab("Attributes")+
  labs(title="Entropy of  Categorical predictor Variables")+theme(plot.title = element_text(hjust = 0.5))
# credit history has lowes. foreign national has highest entropy

####################################### Binary split (Binning multiple sub categories in categorical attributes into 2 sub categories) #########
# Checking each attribute individually using proportionality with target class and binning them
##checking acct
table(train_data_categorical[,1],train_data_categorical$Credit.Standing)
prop.table(table(train_data_categorical[,1],train_data_categorical$Credit.Standing),1)
# Low and No.Acct have a large number of good credit standings. so they were combined.
# similarly the other two
train_data_categorical <- mutate(train_data_categorical, Checking.Acct = ifelse(Checking.Acct=='No Acct', "No Acct", "Yes Acct"))
# using mutate() to add new categories based on logical if else statements

##credit history
table(train_data_categorical[,2],train_data_categorical$Credit.Standing)
prop.table(table(train_data_categorical[,2],train_data_categorical$Credit.Standing),1)
#All paid  bank paid and current can be clubbed to one column as these have no credits due & moslty have accounts with good credit standings
# critical and delay can be clubbed together as not paid since they have credits due
train_data_categorical <- mutate(train_data_categorical, Credit.History = ifelse(Credit.History=='All Paid'|Credit.History=='Bank Paid'|Credit.History=='Current', "Paid", "NotPaid"))

### Loan reason
table(train_data_categorical[,3],train_data_categorical$Credit.Standing)
prop.table(table(train_data_categorical[,3],train_data_categorical$Credit.Standing),1)
# cars,furniture and small appliances can be clubbed together- mostly belong to house holds
# business, large appliance, others,repairs, retraining, education can be clubbed as second category
train_data_categorical <- mutate(train_data_categorical, Loan.Reason = ifelse(Loan.Reason=='Car New'|Loan.Reason=='Car Used'|Loan.Reason=='Furniture'|Loan.Reason=='Small Appliance'| Loan.Reason=='Education', "Household", "Business"))


### Savings.Acct
table(train_data_categorical[,4],train_data_categorical$Credit.Standing)
prop.table(table(train_data_categorical[,4],train_data_categorical$Credit.Standing),1)
# No Acct, low  are clubbed to one group, medlow, medhigh & high is clubbed to second group
train_data_categorical <- mutate(train_data_categorical, Savings.Acct = ifelse(Savings.Acct=='No Acct'|Credit.History=='Low', "Low.NoSavAcct", "Med.High.SavAcct"))


#Employment
table(train_data_categorical[,5],train_data_categorical$Credit.Standing)
prop.table(table(train_data_categorical[,5],train_data_categorical$Credit.Standing),1)
# medium and long term employment and unemployed types seems to have more good credit standings
# short and unemployed have more bad credit standings
train_data_categorical <- mutate(train_data_categorical, Employment = ifelse(Employment=='Short'|Credit.History=='Very Short', "Short", "Long.Unemployed"))

#Personal.status
table(train_data_categorical[,6],train_data_categorical$Credit.Standing)
prop.table(table(train_data_categorical[,6],train_data_categorical$Credit.Standing),1)
# married and divorced into one category
train_data_categorical <- mutate(train_data_categorical, Personal.Status = ifelse(Personal.Status=='Single', "Single", "Married.Divorced"))


# Housing
table(train_data_categorical[,7],train_data_categorical$Credit.Standing)
prop.table(table(train_data_categorical[,7],train_data_categorical$Credit.Standing),1)
# rent and other is clubbed to one group
train_data_categorical <- mutate(train_data_categorical, Housing = ifelse(Housing=='Own', "Own", "Rent.Other"))


#Job.Type
table(train_data_categorical[,8],train_data_categorical$Credit.Standing)
prop.table(table(train_data_categorical[,8],train_data_categorical$Credit.Standing),1)
# Management and skilled and unemployed to one group, they have more good credit standings
# unskilled to other group, they have more bad credit standings
train_data_categorical <- mutate(train_data_categorical, Job.Type = ifelse(Job.Type=='Management'|Job.Type=='Skilled'|Job.Type=='Unemployed', "Skilled.Unemployed", "Unskilled"))


#View(train_data_categorical)                               # View the data after binning and check them

any(is.na(train_data_categorical))                          # check for null values to see if anything is missed

str(train_data_categorical)                                # checking data type. binned column are not factors
ncol(train_data_categorical);nrow(train_data_categorical)  # checking dimension after conversion
#converting column to factors
col_names <- names(train_data_categorical)                 # getting all column names to a variable
train_data_categorical[col_names] <- lapply(train_data_categorical[col_names] , factor)
# converting to factor using lapply(). it returns data frame of same length
str(train_data_categorical)                                # all columns are factors           

########################################################
## calculating entropy at overall level ( credit standing)
Credit.Standing.tot <- prop.table(table(train_data_categorical$Credit.Standing))
(credit_entropy_tot<-sum(-Credit.Standing.tot*log(Credit.Standing.tot,2)))
# Entropy(E) = 0.9798688


#function to calculate entropy for each attribute
credit_entropy_node <- function(x){
  prob_val<- prop.table(table(train_data_categorical[,x],train_data_categorical$Credit.Standing) + 1e-6, margin = 1)
  sum(prop.table(table(train_data_categorical[,x]))*rowSums(-prob_val*log(prob_val,2)))
}

# Looping through all the training categorical variables to find the one with minimum entropy
x=ncol(train_data_categorical)-1
entropy_vec <- NULL
for (i in 1:x) {
  entropy_val <- credit_entropy_node(i)
  entropy_vec <- c(entropy_vec,entropy_val)
  if(i==ncol(train_data_categorical)-1){
    print(c("Entropy minimum is for ",colnames(train_data_categorical[which.min(entropy_vec)]),round(min(entropy_vec),5)))
    print(c("Information Gain maximum is",round(credit_entropy_tot-min(entropy_vec),5)))
  }
}
#Entropy has increased for all the categories. still maximum information gain is for credit history
# minimum entropy is for "Credit.History" = "0.88576"  
#Information Gain maximum is" "0.09411"

entropy_table <- data.frame(cbind(colnames(train_data_categorical[-10]),round(entropy_vec,3)))
colnames(entropy_table) <- c("Predictor.Variable", "entropy.value")

# Plotting all entropy values . Bar graph with ggplot
ggplot(data=entropy_table,aes(x=Predictor.Variable,y=entropy.value,fill=entropy.value))+
  geom_bar(stat='identity')+ylab("Entropy")+xlab("Attributes")+
  labs(title="Entropy of Categorical  Predictor Variables After Binary Split")+theme(plot.title = element_text(hjust = 0.5))

############Including numeric predictor variables as well for entropy ##################
# Binding categorical and numerical columns in order into a new data frame
x_train_data_new <- cbind(train_data_categorical[1:9],credit_train[10:12],train_data_categorical[10])
#View(x_train_data_new)
ncol(x_train_data_new);nrow(x_train_data_new) #(576,13)
str(x_train_data_new)

# Finding mean value of numerical variables
(m1<- mean(x_train_data_new$Age,na.rm=T))# mean value of age 34.4
(m2<- mean(x_train_data_new$Acct.opened.Months,na.rm=T))# mean = 23.24
(m3<- mean(x_train_data_new$Residence.Time,na.rm=T))# mean 2.83

######################Calculating least entropy for Age#################
# Function defined below takes in two attributes.First attribute is the point in which
# the attribute has to be cut using cut() to categorize the numerical variable into two classes.(class A or B)
# second attribute is the column number for entropy calculation with target variable
# According to the first argument, numerical column is binned and then its entropy is calculated.
# The function returns the entropy value
entropy_age <- function(a,b){
  x_train_data_new$Age <- cut(x_train_data_new$Age, breaks = c(0,a, Inf),
                              labels = c("classA", "classB"),
                              include.lowest = TRUE)
  prob_val<- prop.table(table(x_train_data_new[,b],x_train_data_new[,ncol(x_train_data_new)]) + 1e-6, margin = 1)
  sum(prop.table(table(x_train_data_new[,b]))*rowSums(-prob_val*log(prob_val,2)))
  
}

max_age=max(x_train_data_new$Age)   # to store max value
min_age=min(x_train_data_new$Age)   # to store min value
entropy_vec_age <- NULL             # vector to store entropy
cut_age_vec <- NULL                 # vector to store age values
for (i in seq(min_age,max_age,by=2)) {  # seq() to generate a sequence of numbers between min and max values of age, step factor of 2
  entropy_val_age <- entropy_age(i,12)  # calling the function to bin and return entropy. passing age and column number as parameters
  entropy_vec_age <- c(entropy_vec_age,entropy_val_age) # storing entropy values into vector
  cut_age_vec <- c(cut_age_vec,i)                       # storing age values into another vector
  
}
entropy_table_age <- data.frame(cbind(cut_age_vec,round(entropy_vec_age,5))) # binding entropy and age values in a data frame
colnames(entropy_table_age) <- c("Age", "Entropy Value") # naming columns
print(entropy_table_age)

(Age_Bin <- entropy_table_age$Age[which.min(entropy_table_age$`Entropy Value`)])  # finding the age with minimum entropy.
#This is the optimum cut to bin age into 2 categories. which() gives the position value in a vector
# minimum entropy is found at age 28. This value can classify target more purely.This is different from mean value

x_train_data_new$Age <- cut(x_train_data_new$Age, breaks = c(0,Age_Bin, Inf),  # using optimum value of age to bin it.
                            labels = c("Young", "Adult"),
                            include.lowest = TRUE)

###########################Calculating least entropy for Acct opened Months###########
# Similarly to the one defined for Age
entropy_acct <- function(a,b){
  x_train_data_new$Acct.opened.Months <- cut(x_train_data_new$Acct.opened.Months, breaks = c(0,a, Inf),
                                             labels = c("classA", "classB"),
                                             include.lowest = TRUE)
  prob_val<- prop.table(table(x_train_data_new[,b],x_train_data_new[,ncol(x_train_data_new)]) + 1e-6, margin = 1)
  sum(prop.table(table(x_train_data_new[,b]))*rowSums(-prob_val*log(prob_val,2)))
  
}

max_acct=max(x_train_data_new$Acct.opened.Months)
min_acct=min(x_train_data_new$Acct.opened.Months)
entropy_vec_acct <- NULL
cut_acct_vec <- NULL
for (i in seq(min_acct,max_acct,by=1)) {
  entropy_val_acct <- entropy_acct(i,10)
  entropy_vec_acct <- c(entropy_vec_acct,entropy_val_acct)
  cut_acct_vec <- c(cut_acct_vec,i)
  
}
entropy_table_acct <- data.frame(cbind(cut_acct_vec,round(entropy_vec_acct,5)))
colnames(entropy_table_acct) <- c("Acct.opened.Months", "Entropy Value")
print(entropy_table_acct)

(Acct_Bin <- entropy_table_acct$Acct.opened.Months[which.min(entropy_table_acct$`Entropy Value`)])
# optimum value found is 12 months . This value can classify target more purely . Diffeent value from mean
x_train_data_new$Acct.opened.Months <- cut(x_train_data_new$Acct.opened.Months, breaks = c(0,Acct_Bin, Inf),
                                           labels = c("less.than.one.years", "more.than.one.year"),
                                           include.lowest = TRUE)


###########################Calculating least entropy for Residence Time ###########
entropy_residence <- function(a,b){
  x_train_data_new$Residence.Time <- cut(x_train_data_new$Residence.Time, breaks = c(0,a, Inf),
                                         labels = c("classA", "classB"),
                                         include.lowest = TRUE)
  prob_val<- prop.table(table(x_train_data_new[,b],x_train_data_new[,ncol(x_train_data_new)]) + 1e-6, margin = 1)
  sum(prop.table(table(x_train_data_new[,b]))*rowSums(-prob_val*log(prob_val,2)))
  
}

max_residence=max(x_train_data_new$Residence.Time)
min_residence=min(x_train_data_new$Residence.Time)
entropy_vec_residence <- NULL
cut_residence_vec <- NULL
for (i in seq(min_residence,max_residence,by=1)) {
  entropy_val_residence <- entropy_residence(i,11)
  entropy_vec_residence <- c(entropy_vec_residence,entropy_val_residence)
  cut_residence_vec <- c(cut_residence_vec,i)
  
}
entropy_table_residence <- data.frame(cbind(cut_residence_vec,round(entropy_vec_residence,5)))
colnames(entropy_table_residence) <- c("Residence.Time", "Entropy Value")
print(entropy_table_residence)

(Residence_Bin <- entropy_table_residence$Residence.Time[which.min(entropy_table_residence$`Entropy Value`)])
# optimum value to cut is 1 year. This value can classify target more purely
x_train_data_new$Residence.Time <- cut(x_train_data_new$Residence.Time, breaks = c(0,Residence_Bin, Inf),
                                       labels = c("less.than.1.year", "more.than.1.year"),
                                       include.lowest = TRUE)

#View(x_train_data_new)
str(x_train_data_new)   # all are factors
nrow(x_train_data_new);ncol(x_train_data_new) #(576,13)
any(is.na(x_train_data_new))
# Now all the numerical variables are categorized
################################Calculating Entropy with numerical variables##############
credit_entropy_node <- function(x){  # calculating entropy using function
  prob_val<- prop.table(table(x_train_data_new[,x],x_train_data_new[,ncol(x_train_data_new)]) + 1e-6, margin = 1)
  sum(prop.table(table(x_train_data_new[,x]))*rowSums(-prob_val*log(prob_val,2)))
}
entropy_vec <- NULL
for (i in 1:12) {
  entropy_val <- credit_entropy_node(i)
  entropy_vec <- c(entropy_vec,entropy_val)
  if(i==ncol(x_train_data_new)-1){
    print(entropy_vec)
    print(c("Entropy minimum is for ",colnames(x_train_data_new[which.min(entropy_vec)]),round(min(entropy_vec),5)))
    print(c("Information Gain maximum is",round(credit_entropy_tot-min(entropy_vec),5)))
    entropy_level1 <- round(min(entropy_vec),5)
  }
}
entropy_level1
# minimum Entropy is for credit history= 0.88576
# information gain for credit history is maximum  =0.09411

entropy_table <- data.frame(cbind(colnames(x_train_data_new[-13]),round(entropy_vec,3)))
colnames(entropy_table) <- c("Predictor.Variable", "entropy.value")

# pllotting entropy of all attributes
ggplot(data=entropy_table,aes(x=Predictor.Variable,y=entropy.value,fill=entropy.value))+
  geom_bar(stat='identity')+ylab("Entropy")+xlab("Attributes")+
  labs(title="Entropy of all  Predictor Variables After Binary Split")+theme(plot.title = element_text(hjust = 0.5))
#credit history has the highest information gain, followed by employment


###########Working out results with GINI IMPURITY#################

###calculating gini impurity (gini ranges from 0-1. higher the value of gini more will be the purity of the nodes)
# works only with categorical data , and only do binary splits
# gini impurity = 1- gini, 1-Pi^2   # GINI IMPURITY LOWER , THE BETTER

credit_gini_node <- function(x){
  prob_val_gini<- prop.table(table(x_train_data_new[,x],x_train_data_new[,ncol(x_train_data_new)]) + 1e-6, margin = 1)
  sum(prop.table(table(x_train_data_new[,x]))*(1-rowSums(prob_val_gini*prob_val_gini))) #(1-Pi^2 implemented)
}

gini_vec <- NULL
for (i in 1:12) {
  gini_val <- credit_gini_node(i)
  gini_vec <- c(gini_vec,gini_val)
  if(i==ncol(x_train_data_new)-1){
    print(gini_vec)
    print(c(" minimum gini impurity is for ",colnames(x_train_data_new[which.min(gini_vec)]),round(min(gini_vec),5)))
  
  }
}
#" minimum gini impurity is for " "Credit.History"=  "0.42329"

gini_table <- data.frame(cbind(colnames(x_train_data_new[-13]),round(gini_vec,3)))
colnames(gini_table) <- c("Predictor.Variable", "gini.value")

# PLOTTING GINI IMPURITY VALUES FOR ALL ATTRIBUTES
ggplot(data=gini_table,aes(x=Predictor.Variable,y=gini.value))+
  geom_point(size = 3, alpha = .6) 

###########################################################################################
#####################################FINDING SECOND LEVEL SPLIT###########################
##########################################################################################
#splitting data frame into 2 according to first split.credit history(paid ,not paid)

x_train_data_new_2.1 <- x_train_data_new %>%
  filter(Credit.History=="Paid")             # taking all paid categories in credit history into one data frame (left side of root node)

#View(x_train_data_new_2.1)
x_train_data_new_2.1 <- x_train_data_new_2.1[,-2]      # Removing credit history from the data set as  it is very pure. Entropy will be minimum for it
nrow(x_train_data_new_2.1)#455
ncol(x_train_data_new_2.1)#12
#str(x_train_data_new_2.1)


# 2nd split 1st branch- paid
#calculating entropy of new data frame  using target variable
Credit.Standing_2.1 <- prop.table(table(x_train_data_new_2.1$Credit.Standing))
(credit_entropy_2.1 <- sum(-Credit.Standing_2.1*log(Credit.Standing_2.1,2)))   # entropy= 0.9100256

# Function to calculate entropy value of each attribute with target
credit_entropy_node <- function(x){
  prob_val<- prop.table(table(x_train_data_new_2.1[,x],x_train_data_new_2.1[,12]) + 1e-6, margin = 1)
  sum(prop.table(table(x_train_data_new_2.1[,x]))*rowSums(-prob_val*log(prob_val,2)))
}
ncol2.1 <- ncol(x_train_data_new_2.1)-1
entropy_vec1 <- NULL
info_gain_vec <- NULL
for (i in 1:ncol2.1) {
  entropy_val <- credit_entropy_node(i)
  entropy_vec1 <- c(entropy_vec1,entropy_val)
  info_gain_val <- credit_entropy_2.1- entropy_val
  info_gain_vec <- c(info_gain_vec,info_gain_val)
  if(i==ncol2.1){
    print(entropy_vec1)
    print(c("Entropy minimum is for ",colnames(x_train_data_new_2.1[which.min(entropy_vec1)]),round(min(entropy_vec1),5)))
    print(c("Information Gain maximum is",round(credit_entropy_2.1-min(entropy_vec1),5)))
    entropy_level2 <- round(min(entropy_vec1),5)
  }
}
# Entropy for Employment= 0.84513 . Employment has the least entropy. Hence it becomes child node at left side
# information gain is maximum for employment =0.0649
# Taking all attributes and their corresponding info gain values in a data frame
entropy_table.2.1 <- data.frame(cbind(colnames(x_train_data_new_2.1[-12]),round(entropy_vec1,3),round(info_gain_vec,3)))
colnames(entropy_table.2.1) <- c("Predictor.Variable", "entropy.value","Information_Gain") # changing column names

entropy_table.2.1[-1] <- lapply(entropy_table.2.1[-1], as.numeric) # for plotting a continuous bar plot

# Information Gain plot
ggplot(data=entropy_table.2.1,aes(x=Predictor.Variable,y=Information_Gain,fill=Information_Gain))+
  geom_bar(stat='identity')+ylab("Information Gain")+xlab("Attributes")+
  labs(title="Information Gain of Predictor Variables on Subtree 1")+theme(plot.title = element_text(hjust = 0.5))




########################################################## 2nd part
x_train_data_new_2.2 <- x_train_data_new %>%  # taking all not paid categories in credit history into one data frame (right side of root node)
  filter(Credit.History=="NotPaid")                   
#View(x_train_data_new_2.2)
x_train_data_new_2.2 <- x_train_data_new_2.2[,-2]      # Removing credit history from the dataset as  it is very pure. Entropy will be minimum for it
nrow(x_train_data_new_2.2)#121     # checking number of rows and columns
ncol(x_train_data_new_2.2)#12
str(x_train_data_new_2.2)


# Total entropy- #calculating entropy of new data frame  using target variable
(Credit.Standing_2.2 <- prop.table(table(x_train_data_new_2.2$Credit.Standing)))
(credit_entropy_2.2<-sum(-Credit.Standing_2.2*log(Credit.Standing_2.2,2)))   # entropy = 0.7944901

# Function to calculate entropy value of each attribute with target
credit_entropy_node <- function(x){
  prob_val<- prop.table(table(x_train_data_new_2.2[,x],x_train_data_new_2.2[,12]) + 1e-6, margin = 1)
  sum(prop.table(table(x_train_data_new_2.2[,x]))*rowSums(-prob_val*log(prob_val,2)))
}
ncol2.2 <- ncol(x_train_data_new_2.2)-1
entropy_vec2 <- NULL
info_gain_vec2 <- NULL
for (i in 1:ncol2.2) {
  entropy_val <- credit_entropy_node(i)
  entropy_vec2 <- c(entropy_vec2,entropy_val)
  info_gain_val <- credit_entropy_2.2- entropy_val
  info_gain_vec2 <- c(info_gain_vec2,info_gain_val)
  if(i==ncol2.2){
    print(entropy_vec2)
    print(c("Entropy minimum is for ",colnames(x_train_data_new_2.2[which.min(entropy_vec2)]),round(min(entropy_vec2),5)))
    print(c("Information Gain maximum is",round(credit_entropy_2.2-min(entropy_vec2),5)))
    entropy_level2 <- round(min(entropy_vec2),5)
  }
}
# Entropy for Checking Acct= 0.70634 . Checking Acct has the least entropy. Hence it becomes child node at right side after root node
# information gain is maximum for employment =0.08815. Information gain is more at right side than left side of root node

# Taking all attributes and their corresponding info gain values in a data frame
entropy_table.2.2 <- data.frame(cbind(colnames(x_train_data_new_2.2[-12]),round(entropy_vec2,3),round(info_gain_vec2,3)))
colnames(entropy_table.2.2) <- c("Predictor.Variable", "entropy.value",'Information_Gain')

entropy_table.2.2[-1] <- lapply(entropy_table.2.2[-1], as.numeric)  # to get continuous plot
ggplot(data=entropy_table.2.2,aes(x=Predictor.Variable,y=Information_Gain,fill=Information_Gain))+
  geom_bar(stat='identity')+ylab("Information Gain")+xlab("Attributes")+
  labs(title="Information Gain of Predictor Variables on Subtree 2")+theme(plot.title = element_text(hjust = 0.5))


###################################### Implementing Decision Tree using tree()

#install.packages("tree")
library(tree)                   # calling library tree
tree.credit = tree(Credit.Standing~., data=credit_risk_data) # calling tree() with credit standing as target variable. credit_risk_data is the initial data after EDA
summary(tree.credit)               # plotting summary of tree
print(tree.credit)                # printing tree
plot(tree.credit)                 # plotting tree
text(tree.credit,pretty=0)
table(credit_risk_data$Credit.Standing)/nrow(credit_risk_data)  # Baseline accuracy of 58%
 
####train data for training the model
(train_size1 <- floor(0.75*nrow(credit_risk_data)))
set.seed(427)                                                            # setting constant seed value
tree_train_sample <- sample(1:nrow(credit_risk_data), size=train_size1)  # taking sample of 75%

# splitting data into train and test
credit_train_tree <- credit_risk_data[tree_train_sample,]          # making train data
credit_test_tree <- credit_risk_data[-tree_train_sample,]          # making test data
tree.credit = tree(Credit.Standing~., data=credit_train_tree)      # creating tree object with train data
summary(tree.credit)                                               # creating summary of tree
print(tree.credit)
plot(tree.credit)                                                  # plotting the tree. 10 leaf nodes
text(tree.credit,pretty=0)
misclass.tree(tree.credit,detail=T)                                # reports miss-classifications at each node

tree.pred = predict(tree.credit,credit_test_tree, type="class")    #predicting values of target using test data. Type given as 'class' to get good and bad classes as predictions in each data point
table(tree.pred,credit_test_tree$Credit.Standing)                  #comparing predictions vs actual labels
(46+104)/193 # Accuracy of 77.7 % ( model was able to predict 77.7 % of classifications correctly when compared with true labels in the data)

table(credit_risk_data$Credit.Standing)/nrow(credit_risk_data)
# model is better than baseline accuracy
#confusion matrix
library(caret)
cm=confusionMatrix(tree.pred,credit_test_tree$Credit.Standing,positive = "Bad") # plotting confusion matrix to get more parameters to evaluate model
print(cm)

#cv to prune the tree                                          # to reduce the tree complexity
set.seed(427)
cv.credit <- cv.tree(tree.credit,K=10, FUN=prune.misclass)     # 10 fold cross validation. cost complexity pruning
cv.credit
plot(cv.credit)
plot(cv.credit$size,cv.credit$dev,xlab="Tree size",ylab="Error",type="b",col='red') # plotting residual deviance vs tree size
plot(cv.credit$k,cv.credit$dev,xlab="No of crossfold validation sets",ylab="Error",type="b",col='steelblue') # plotting number of cross validation sets vs residual deviance


# Best size of tree is  4 ( 4 leaf nodes)
set.seed(427)
prune.credit<-prune.misclass(tree.credit,best=4)  # pruning model tree to size 4
plot(prune.credit)
text(prune.credit,pretty=0)
tree.pred<-predict(prune.credit,credit_test_tree,type="class")
table(tree.pred,credit_test_tree$Credit.Standing)
(47+103)/193
# pruned with same accuracy 77.7% . But complexity of tree has reduced
cm=confusionMatrix(tree.pred,credit_test_tree$Credit.Standing,positive = "Bad") # plotting confusin matrix
print(cm)

##############################RANDOM FOREST MODEL#################################
library(randomForest)    #calling library
dim(credit_train_tree)   # CHECKING DIMENSION OF DATA SET
set.seed(427)            # setting constant seed value

                                                                  
# calling randomforest() with 1000 trees and credit standing as target variable.
#credit_risk_data is the initial data after EDA and removing missing values. 
#Asking model to get important variables used. Asking model to trace OOB errors
credit.rf <- randomForest(Credit.Standing~., data=credit_train_tree, ntree = 1000, importance=TRUE, do.trace=TRUE)
print(credit.rf)
plot(credit.rf)       # plotting errors of classes and OOB error
importance(credit.rf) # printing important variables
varImpPlot(credit.rf) # plotting important variables. credit history is the most important variable
#predict

rf.pred = predict(credit.rf,credit_test_tree,type = "class") # predicting the values of credit standing using test data
table(rf.pred,credit_test_tree$Credit.Standing) # calculating accuracy
(53+98)/(193) # 78.23% accuracy
##model was able to predict 78.23 % of classifications correctly when compared with true target labels in the data
## Accuracy improved when compared to decision tree. Errors are minimized

#Tuning the model using tuneRF()
set.seed(427)
# everything except target variable is forst parameter.Target variable is second parameter.
# Number of attributes considered in each split is mtry. starting mtry with 2 and increasing it by 2. 
# Only if OOB error improve by 0.05, then move to next mtry. Number of trees used is 1000
tuneRF(credit_train_tree[,-13],credit_train_tree[,13], mtryStart = 2,stepFactor = 2, ntreeTry = 1000,improve = 0.05)

#best is mtry=4. It has least OOB error
set.seed(427)
# creating random forest model again but with mtry 4
credit.rf <- randomForest(Credit.Standing~., data=credit_train_tree, mtry=4, importance=TRUE, do.trace=TRUE,ntree = 1000)
print(credit.rf) # 74.4% training accuracy
rf.pred = predict(credit.rf,credit_test_tree,type = "class")
table(rf.pred,credit_test_tree$Credit.Standing)
(54+98)/(193) # 78.75% accuracy 
#model was able to predict 78.75 % of classifications correctly when compared with true target labels in the data
# Accuracy improved slightly when compared to original random forest model without tuning. OOB Errors are minimized
# Accuracy can be improved more by better hyper parameter tuning and boosting techniques
#confusion matrix
cm=confusionMatrix(rf.pred,credit_test_tree$Credit.Standing,positive = "Bad")
print(cm)
# Better sensitivity than decision tree

#plotting ROC-AUC curve
#install.packages("pROC")
library(pROC)
rf.pred2 <- predict(credit.rf,credit_test_tree, type = "prob")   # predictions are done with probabilities not classes
ROC_rf.cred <- roc(credit_test_tree$Credit.Standing, rf.pred2[,2]) # capturing roc
ROC_rf_auc_cred <- auc(ROC_rf.cred)       # calculating area under the curve
plot(ROC_rf.cred, col = "green", main = "ROC for Random Forest") # plotting roc curve

paste("Area under curve of random forest: ", ROC_rf_auc_cred) # AUC is 83.2% 
# Auc is good. not the best. 

#### Using best model to predict on scoring data . TUNED RANDOM FOREST MODEL
df1 <- read_excel("Credit_Risk7_final.xlsx",sheet="Scoring_Data")
credit.risk.scoring <- as.data.frame(unclass(df1),stringsAsFactors = TRUE)
summary(credit.risk.scoring)
str(credit.risk.scoring)
credit.risk.scoring <- credit.risk.scoring[,-1]             # removing ID column
credit.risk.scoring$Credit.Standing <- NA                   # Adding credit standing empty column to data frame for capturing predictions
names(credit.risk.scoring)[names(credit.risk.scoring) == 'Months.since.Checking.Acct.opened'] <- 'Acct.opened.Months' # changing names of columns to suitable format
credit.risk.scoring$Credit.Standing <- as.factor(credit.risk.scoring$Credit.Standing) # converting credit standing to factor format

# To Make scoring data same level as training data for all variables in factors
credit.risk.scoring <- rbind(credit_train_tree[1, ] , credit.risk.scoring)  # adding one row of training data to scoring data
credit.risk.scoring <- credit.risk.scoring[-1,]                            # removing the added row 
#Predicting
Credit.Standing.Score = predict(credit.rf,credit.risk.scoring,type = "class") # using random forest model to predict classes on scoring data
Credit.Standing.Score <- as.data.frame(Credit.Standing.Score)                 # getting predicted values into a data frame
credit.risk.scoring <- cbind(credit.risk.scoring[,c(1:12)],Credit.Standing.Score) # binding the predicted values into scoring data

View(credit.risk.scoring)




#####GDPR Omiting credit.history, personal.status and foreign national

credit_train_tree_gdpr <- credit_risk_data[tree_train_sample,c(-2,-6,-9)]          # making train data by omiting sensitive attributes
credit_test_tree_gdpr <- credit_risk_data[-tree_train_sample,c(-2,-6,-9)]          # making test data
tree.credit.gdpr = tree(Credit.Standing~., data=credit_train_tree_gdpr)            # creating tree object with train data
summary(tree.credit.gdpr)
print(tree.credit.gdpr)
plot(tree.credit.gdpr)                                                              # tree with 14 nodes
text(tree.credit.gdpr,pretty=0)
misclass.tree(tree.credit.gdpr,detail=T)                                            # reports miss-classifications at each node

tree.pred.gdpr = predict(tree.credit.gdpr,credit_test_tree_gdpr, type="class")      #predicting using test data
table(tree.pred.gdpr,credit_test_tree_gdpr$Credit.Standing)                         #comparing predictions vs actual labels
(48+77)/(48+77+38+30)  #64.7 % accuracy                                              #Accuracy has decreased, due to the loss of major attributes


#cv to prune the tree
set.seed(427)
cv.credit.gdpr <- cv.tree(tree.credit.gdpr,K=10, FUN=prune.misclass)              # 10 fold cross validation
cv.credit.gdpr
plot(cv.credit.gdpr)
names(cv.credit.gdpr)

plot(cv.credit.gdpr$size,cv.credit.gdpr$dev,xlab="Tree size",ylab="Error",type="b",col='red')
plot(cv.credit.gdpr$k,cv.credit.gdpr$dev,type="b")


# best size 5 (leaf nodes)
prune.credit.gdpr<-prune.misclass(tree.credit.gdpr,best=5)
plot(prune.credit.gdpr)
text(prune.credit.gdpr,pretty=0)
tree.pred.gdpr<-predict(prune.credit.gdpr,credit_test_tree_gdpr,type="class")
table(tree.pred.gdpr,credit_test_tree_gdpr$Credit.Standing)
(34+89)/(34+89+44+26)                                                       # pruned to same accuracy

# using random forest model
library(randomForest)

set.seed(427)
credit.rf.gdpr <- randomForest(Credit.Standing~., data=credit_train_tree_gdpr, ntree = 1000, importance=TRUE, do.trace=TRUE)
print(credit.rf.gdpr)
plot(credit.rf.gdpr)
importance(credit.rf.gdpr)
varImpPlot(credit.rf.gdpr) # employment is most important according to mean decrease in accuracy

#predict
rf.pred.gdpr = predict(credit.rf.gdpr,credit_test_tree_gdpr,type = "class")  # predicting using test data
table(rf.pred.gdpr,credit_test_tree_gdpr$Credit.Standing)
(53+92)/(53+92+23+25) # 75.13% accuracy. Accuracy has decreased compared to the random forest model without gdpr.
# But decrease in accuracy is very less compared to tree. loss of variables did not have a very significant impact on random forest model accuracy

#Tuning the model
set.seed(427)
tuneRF(credit_train_tree_gdpr[,-10],credit_train_tree_gdpr[,10], subset=credit_train_tree_gdpr, mtryStart = 2,stepFactor = 2, ntreeTry = 500)
#OOB error is high as compared to model without gdpr
#best is mtry=4
# creating random forest model again but with best mtry
credit.rf.gdpr <- randomForest(Credit.Standing~., data=credit_train_tree_gdpr, mtry=4, importance=TRUE, do.trace=TRUE)
print(credit.rf.gdpr)
rf.pred.gdpr = predict(credit.rf.gdpr,credit_test_tree_gdpr,type = "class")
table(rf.pred.gdpr,credit_test_tree_gdpr$Credit.Standing)
(53+92)/(53+92+23+25) # 75.1% accuracy. Accuracy remained same after tuning
library(caret)
#confusion amtrix
cm=confusionMatrix(rf.pred.gdpr,credit_test_tree_gdpr$Credit.Standing,positive = "Bad")
print(cm)


############Pattern Recognition

library(tree)
credit_train_tree <- credit_risk_data[tree_train_sample,]          # making train data. 75% of data
credit_test_tree <- credit_risk_data[-tree_train_sample,]          # making test data
tree.credit.pattern = tree(Credit.Standing~., data=credit_train_tree)      # creating tree object with train data
summary(tree.credit.pattern)                                        # tree with 10 terminal nodes
print(tree.credit.pattern)
plot(tree.credit.pattern)
text(tree.credit.pattern,pretty=0)
misclass.tree(tree.credit.pattern,detail=T)                                # reports miss-classifications at each node

# PREDICT classes for entire data
tree.pred.pattern = predict(tree.credit.pattern,credit_risk_data, type="class")    #predicting using the entire dataset as test data
table(tree.pred.pattern,credit_risk_data$Credit.Standing)                  #comparing predictions vs actual labels
#Accuracy 78.5%
predictions<- as.data.frame(tree.pred.pattern)                             # predicted response variables converted to a dataframe



vec.pattern <- NULL                                                       # creating an empty vector to get suspicius data point's ID value
combine_data <-
  cbind(Data_ID, credit_risk_data, predictions)                          # combining ID column ,data set and predicted response variable into a new dataframe called combine_data
for (i in 1:nrow(combine_data)) {
  if (combine_data$Credit.Standing[i] != combine_data$tree.pred.pattern[i]) {  # iterating through the data frame to check if the original response variable is matching with the predicted response variable
    vec.pattern <- c(vec.pattern, combine_data$Data_ID[i])                     # if not matching, get the non matching IDs to the vector
  }
  
}

View(combine_data[combine_data$Data_ID %in% vec.pattern, ])                 # data frame of non matching records

# Continuous non matching values are obtained in  ID values from 314 - 326
# Several non matching values are seen from 300-350 range
###################################################################################################
