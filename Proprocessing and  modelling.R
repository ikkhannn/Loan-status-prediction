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

drops <- c("Customer.ID")
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



ggplot(mydata_train_raw, aes(x = Years.in.current.job)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Years_in_current_job") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  facet_grid(~ Loan.Status) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(mydata_train_raw, aes(x = Term)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Term") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  facet_grid(~ Loan.Status) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#plot  the same plot by Term and by Loan Status.
ggplot(mydata_train_raw, aes(x = Purpose)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Purpose") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  facet_grid(Term ~ Loan.Status) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




ggplot(data = mydata_train_raw, mapping = aes(x = Years.of.Credit.History)) + 
  geom_histogram(binwidth = 0.25)


#Summary of credit score
summary(mydata_train_raw$Credit.Score)


#Remove duplicate row
mydata_train_raw=mydata_train_raw[!duplicated(mydata_train_raw), ]


#Count frequent of Loan_ID
a=as.data.frame((table(mydata_train_raw$Loan.ID)))
head(a)

names(a)[names(a) == "Var1"] <- "Loan.ID"
head(a)


mydata_train_raw=merge(x = mydata_train_raw, y = a, by = "Loan.ID", all.x = TRUE)
mydata_train_raw$Credit.Score.flag = ifelse(is.na(mydata_train_raw$Credit.Score),1,0)
head(mydata_train_raw)

mydata_train_raw$Credit.Score.Freq.flag = ifelse(mydata_train_raw$Credit.Score.flag == 1 & mydata_train_raw$Freq > 1,1,0)


#Now I drop Credit_Score_Freq_flag column has 1 value.
mydata_train_raw = mydata_train_raw[mydata_train_raw['Credit.Score.Freq.flag'] == 0,]



mydata_train_raw$current.loan.amount.flag = ifelse(mydata_train_raw$Current.Loan.Amount==99999999,1,0)
mydata_train_raw$loan.Drop.flag = ifelse(mydata_train_raw$current.loan.amount.flag == 1 & mydata_train_raw$Freq > 1,1,0)

#Now I drop loan_Drop_flag column has 1 value.
mydata_train_raw = mydata_train_raw[mydata_train_raw['loan.Drop.flag'] == 0,]


drop <- c("Freq","Credit.Score.flag","Credit.Score.Freq.flag","current.loan.amount.flag","loan.Drop.flag")
mydata_train_raw = mydata_train_raw[,!(names(mydata_train_raw) %in% drop)]


s=mydata_train_raw[mydata_train_raw$Current.Loan.Amount != 99999999,]
mean(s$Current.Loan.Amount)


mydata_train_raw$Current.Loan.Amount = ifelse(mydata_train_raw$Current.Loan.Amount==99999999,308587,mydata_train_raw$Current.Loan.Amount)


#Divide data into 2 sets for better na values filling
train1=mydata_train_raw[mydata_train_raw$Loan.Status=='Fully Paid',]
train2=mydata_train_raw[mydata_train_raw$Loan.Status=='Charged Off',]


# Fill missing values with mean

train1$Current.Loan.Amount[is.na(train1$Current.Loan.Amount)] <- mean(train1$Current.Loan.Amount,na.rm = TRUE)
train1$Credit.Score[is.na(train1$Credit.Score)] <- mean(train1$Credit.Score, na.rm = TRUE)
train1$Annual.Income[is.na(train1$Annual.Income)] <- mean(train1$Annual.Income, na.rm = TRUE)
train1$Monthly.Debt[is.na(train1$Monthly.Debt)] <- mean(train1$Monthly.Debt, na.rm = TRUE)
train1$Years.of.Credit.History[is.na(train1$Years.of.Credit.History)] <- mean(train1$Years.of.Credit.History, na.rm = TRUE)
train1$Months.since.last.delinquent[is.na(train1$Months.since.last.delinquent)] <- mean(train1$Months.since.last.delinquent, na.rm = TRUE)
train1$Number.of.Open.Accounts[is.na(train1$Number.of.Open.Accounts)] <- mean(train1$Number.of.Open.Accounts, na.rm = TRUE)
train1$Number.of.Credit.Problems[is.na(train1$Number.of.Credit_Problems)] <- mean(train1$Number.of.Credit.Problems, na.rm = TRUE)
train1$Current.Credit.Balance[is.na(train1$Current.Credit.Balance)] <- mean(train1$Current.Credit.Balance, na.rm = TRUE)
train1$Maximum.Open.Credit[is.na(train1$Maximum.Open.Credit)] <- mean(train1$Maximum.Open.Credit, na.rm = TRUE)
train1$Bankruptcies[is.na(train1$Bankruptcies)] <- mean(train1$Bankruptcies, na.rm = TRUE)
train1$Tax.Liens[is.na(train1$Tax.Liens)] <- mean(train1$Tax.Liens, na.rm = TRUE)


# Fill missing values for train 2
train2$Current.Loan.Amount[is.na(train2$Current.Loan.Amount)] <- mean(train2$Current.Loan.Amount,na.rm = TRUE)
train2$Credit.Score[is.na(train2$Credit.Score)] <- mean(train2$Credit.Score, na.rm = TRUE)
train2$Annual.Income[is.na(train2$Annual.Income)] <- mean(train2$Annual.Income, na.rm = TRUE)
train2$Monthly.Debt[is.na(train2$Monthly.Debt)] <- mean(train2$Monthly.Debt, na.rm = TRUE)
train2$Years.of.Credit.History[is.na(train2$Years.of.Credit.History)] <- mean(train2$Years.of.Credit.History, na.rm = TRUE)
train2$Months.since.last.delinquent[is.na(train2$Months.since.last.delinquent)] <- mean(train2$Months.since.last.delinquent, na.rm = TRUE)
train2$Number.of.Open.Accounts[is.na(train2$Number.of.Open.Accounts)] <- mean(train2$Number.of.Open.Accounts, na.rm = TRUE)
train2$Number.of.Credit.Problems[is.na(train2$Number.of.Credit_Problems)] <- mean(train2$Number.of.Credit.Problems, na.rm = TRUE)
train2$Current.Credit.Balance[is.na(train2$Current.Credit.Balance)] <- mean(train2$Current.Credit.Balance, na.rm = TRUE)
train2$Maximum.Open.Credit[is.na(train2$Maximum.Open.Credit)] <- mean(train2$Maximum.Open.Credit, na.rm = TRUE)
train2$Bankruptcies[is.na(train2$Bankruptcies)] <- mean(train2$Bankruptcies, na.rm = TRUE)
train2$Tax.Liens[is.na(train2$Tax.Liens)] <- mean(train2$Tax.Liens, na.rm = TRUE)



#using round function in train1 and train2
train2$Current.Loan.Amount=round(train2$Current.Loan.Amount,0)
train2$Credit.Score=round(train2$Credit.Score,0)
train2$Annual.Income=round(train2$Annual.Income,0)
train2$Monthly.Debt=round(train2$Monthly.Debt,0)
train2$Years.of.Credit.History=round(train2$Years.of.Credit.History,0)
train2$Months.since.last.delinquent=round(train2$Months.since.last.delinquent,0)
train2$Number.of.Open.Accounts=round(train2$Number.of.Open.Accounts,0)
train2$Number.of.Credit.Problems=round(train2$Number.of.Credit.Problems,0)
train2$Current.Credit.Balance=round(train2$Current.Credit.Balance,0)
train2$Maximum.Open.Credit=round(train2$Maximum.Open.Credit,0)
train2$Bankruptcies=round(train2$Bankruptcies,0)
train2$Tax.Liens=round(train2$Tax.Liens,0)


train1$Current.Loan.Amount=round(train1$Current.Loan.Amount,0)
train1$Credit.Score=round(train1$Credit.Score,0)
train1$Annual.Income=round(train1$Annual.Income,0)
train1$Monthly.Debt=round(train1$Monthly.Debt,0)
train1$Years.of.Credit.History=round(train1$Years.of.Credit.History,0)
train1$Months.since.last.delinquent=round(train1$Months.since.last.delinquent,0)
train1$Number.of.Open.Accounts=round(train1$Number.of.Open.Accounts,0)
train1$Number.of.Credit.Problems=round(train1$Number.of.Credit.Problems,0)
train1$Current.Credit.Balance=round(train1$Current.Credit.Balance,0)
train1$Maximum.Open.Credit=round(train1$Maximum.Open.Credit,0)
train1$Bankruptcies=round(train1$Bankruptcies,0)
train1$Tax.Liens=round(train1$Tax.Liens,0)


#Combine both train sets
final_train = rbind(train1, train2)



#Years in Current Job Brackets
final_train$current.job.year <- ifelse((final_train$Years.in.current.job == ('< 1 year') | final_train$Years.in.current.job == ('1 year')
                                        | final_train$Years.in.current.job ==('2 years')
                                        | final_train$Years.in.current.job == ('3 years') 
                                        | final_train$Years.in.current.job == ('4 years')),'0-4 years',
                                       ifelse((final_train$Years.in.current.job == ('5 years') | final_train$Years.in.current.job ==('6 years')
                                               | final_train$Years.in.current.job == ('7 years') | final_train$Years.in.current.job ==('8 years')
                                               | final_train$Years.in.current.job == ('9 years') | final_train$Years.in.current.job == ('n/a')),'5-9 years','>=10 years'))


#Grouping Credit Type
final_train$Credit.Type <- ifelse((final_train$Purpose == ('Business Loan') | final_train$Purpose == ('small_business')
                                   | final_train$Purpose ==('renewable_energy')),'Business Credit',
                                  ifelse((final_train$Purpose == ('Home Improvements') | final_train$Purpose ==('Buy House')
                                          | final_train$Purpose == ('moving')),'Mortgage Credit',
                                         ifelse((final_train$Purpose == ('Buy a Car')),'Vehicle Credit',
                                                ifelse((final_train$Purpose == ('Debt Consolidation')),'Debt Consolidation',
                                                       ifelse((final_train$Purpose == ('Educational Expenses') | final_train$Purpose ==('major_purchase')
                                                               | final_train$Purpose == ('Medical Bills') | final_train$Purpose ==('Take a Trip')
                                                               | final_train$Purpose == ('vacation') | final_train$Purpose == ('wedding')),'Consumer  Credit',
                                                              'Other')))))


df1=final_train

#generate dummy variables 
library(dummies)
df1 <- cbind(df1, dummy(df1$Term))
df1 <- cbind(df1, dummy(df1$Home.Ownership))
df1 <- cbind(df1, dummy(df1$Credit.Type))
df1 <- cbind(df1, dummy(df1$current.job.year))


#Drop categorical columns because we converted them to dummy variables.
df1 = subset(df1, select = -c(Loan.ID, Term, Years.in.current.job, Home.Ownership, Purpose,current.job.year,Credit.Type))

df1$Loan.Status=ifelse(df1$Loan.Status== 'Charged Off', 1,0)



#Normalization




##Lets check variables which are in raw_train set but not in raw_test

setdiff (colnames_train, colnames_test)



##str(mydata_train_raw)

## for checking summary but in a more oganized manner
skim(mydata_train_raw)

skim(mydata_test_raw)




