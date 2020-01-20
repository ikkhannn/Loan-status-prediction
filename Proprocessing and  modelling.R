library(skimr)
library(tidyverse)
library(stringr)
library(ggplot2)
##Load dataset
mydata_train_raw <- read.csv("C:/Users/me/Desktop/bank loan status prediction/my-dataset/credit_train.csv", header=T)
mydata_test_raw <- read.csv("C:/Users/me/Desktop/bank loan status prediction/my-dataset/credit_test.csv", header=T)
##colnames_train <- names(mydata_train_raw)
##colnames_test <- names(mydata_test_raw)

## Let's check our data , what type of data it is, and what are the variations? 

str(mydata_train_raw)

#check type of columns
sapply(mydata_train_raw,class)

##remove loan.id and customer.id

drops <- c("Loan.ID","Customer.ID")
mydata_train_raw <-mydata_train_raw[ , !(colnames_train %in% drops)]
mydata_test_raw <- mydata_test_raw[ , !(colnames_test %in% drops)]


# see number of missing values in each column
missing_data <- colSums(is.na(mydata_train_raw))[colSums(is.na(mydata_train_raw)) > 0] %>% sort(decreasing=T)
missing_data

# Let's check How much % data is missing
nrow(mydata_train_raw[!complete.cases(mydata_train_raw), ])/nrow(mydata_train_raw)*100


#check what proportion of loans are paid and what are charged off.
ggplot(data = mydata_train_raw) + geom_bar(mapping = aes(x = Loan.Status))


# cross classication counts for LoanStatus by Purpose
table(mydata_train_raw$Purpose, mydata_train_raw$Loan.Status)


ggplot(mydata_train_raw, aes(x = Years.in.current.job)) +
  geom_bar() +
  xlab("Years_in_current_job") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Loan_Status feature is our target variable







##Lets check variables which are in raw_train set but not in raw_test

setdiff (colnames_train, colnames_test)



##str(mydata_train_raw)

## for checking summary but in a more oganized manner
skim(mydata_train_raw)

skim(mydata_test_raw)




