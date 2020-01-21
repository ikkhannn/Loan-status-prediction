library(skimr)
library(tidyverse)
library(stringr)
library(ggplot2)
drops <- c("Customer.ID")
mydata_train_raw <-mydata_train_raw[ , !(names(mydata_train_raw) %in% drops)]
mydata_test_raw <- mydata_test_raw[ , !(names(mydata_train_raw) %in% drops)]


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

library(clusterSim)
normFunc=data.Normalization(df1, type="n4", normalization="column")
df1N=as.data.frame(lapply(1, function(x) normFunc))



#building model
library(caret)
set.seed(1)
trainIndis=createDataPartition(y=df1$Loan.Status, p=.90, list=FALSE)
train=df1[trainIndis,]
test=df1[-trainIndis,]


#num_train$LoanStatus=ifelse(num_train$LoanStatus== 1,'Charged Off','Fully Paid')
#test$LoanStatus=ifelse(test$LoanStatus== 1,'Charged Off','Fully Paid')


#train$Loan.Status <- as.factor(train$Loan.Status)


str(train)




library(tree)
library(ISLR)


## Convert all columns to num

num_train<-  train[,]

#num_train$`df1Long Term`<- as.numeric(num_train$`df1Long Term`)
#num_train$`df1Short Term`<- as.numeric(num_train$`df1Short Term`)
#num_train$`df1HaveMortgage`<- as.numeric(num_train$`df1HaveMortgage`)
#num_train$`df1Home Mortgage`<- as.numeric(num_train$`df1Home Mortgage`)
#num_train$`df1Own Home`<- as.numeric(num_train$`df1Own Home`)
#num_train$`df1Rent`<- as.numeric(num_train$`df1Rent`)
#num_train$`df1Business Credit`<- as.numeric(num_train$`df1Business Credit`)
#num_train$`df1Consumer  Credit`<- as.numeric(num_train$`df1Consumer  Credit`)
#num_train$`df1Debt Consolidation`<- as.numeric(num_train$`df1Debt Consolidation`)
#num_train$`df1Mortgage Credit`<- as.numeric(num_train$`df1Mortgage Credit`)
#num_train$`df1Other`<- as.numeric(num_train$`df1Other`)
#num_train$`df1Vehicle Credit`<- as.numeric(num_train$`df1Vehicle Credit`)
#num_train$`df1>=10 years`<- as.numeric(num_train$`df1>=10 years`)
#num_train$`df10-4 years`<- as.numeric(num_train$`df10-4 years`)
#num_train$`df15-9 years`<- as.numeric(num_train$`df15-9 years`)


## Change column names 
colnames(num_train)[which(names(num_train) == "Loan.Status")] <- "LoanStatus"
colnames(num_train)[which(names(num_train) == "Current.Loan.Amount")] <- "CurrentLoanAmount"

colnames(num_train)[which(names(num_train) == "Credit.Score")] <- "CreditScore"

colnames(num_train)[which(names(num_train) == "Annual.Income")] <- "AnnualIncome"

colnames(num_train)[which(names(num_train) == "Monthly.Debt")] <- "MonthlyDebt"

colnames(num_train)[which(names(num_train) == "Years.of.Credit.History")] <- "YearsofCreditHistory"

colnames(num_train)[which(names(num_train) == "Months.since.last.delinquent")] <- "Monthssincelastdelinquent"

colnames(num_train)[which(names(num_train) == "Number.of.Open.Accounts")] <- "NumberofOpenAccounts"

colnames(num_train)[which(names(num_train) == "Number.of.Credit.Problems")] <- "NumberofCreditProblems"

colnames(num_train)[which(names(num_train) == "Current.Credit.Balance")] <- "CurrentCreditBalance"

colnames(num_train)[which(names(num_train) == "Maximum.Open.Credit")] <- "MaximumOpenCredit"


colnames(num_train)[which(names(num_train) == "Tax.Liens")] <- "TaxLiens"

colnames(num_train)[which(names(num_train) == "df1Long Term")] <- "dfLongTerm"

colnames(num_train)[which(names(num_train) == "df1Short Term")] <- "dfShortTerm"

colnames(num_train)[which(names(num_train) == "df1HaveMortgage")] <- "dfHaveMortgage"
colnames(num_train)[which(names(num_train) == "df1Home Mortgage")] <- "dfHomeMortgage"

colnames(num_train)[which(names(num_train) == "df1Own Home")] <- "dfOwnHome"

colnames(num_train)[which(names(num_train) == "df1Rent")] <- "dfRent"

colnames(num_train)[which(names(num_train) == "df1Business Credit")] <- "dfBusinessCredit"
colnames(num_train)[which(names(num_train) == "df1Consumer  Credit")] <- "dfConsumerCredit"
colnames(num_train)[which(names(num_train) == "df1Debt Consolidation")] <- "dfDebtConsolidation"
colnames(num_train)[which(names(num_train) == "df1Mortgage Credit")] <- "dfMortgageCredit"
colnames(num_train)[which(names(num_train) == "df1Other")] <- "dfOther"
colnames(num_train)[which(names(num_train) == "df1Vehicle Credit")] <- "dfVehicleCredit"
colnames(num_train)[which(names(num_train) == "df1>=10 years")] <- "dfgreaterthantenyears"
colnames(num_train)[which(names(num_train) == "df10-4 years")] <- "dfzerotofouryears"
colnames(num_train)[which(names(num_train) == "df15-9 years")] <- "dffivetonineyears"



colnames(test)[which(names(test) == "Loan.Status")] <- "LoanStatus"
colnames(test)[which(names(test) == "Current.Loan.Amount")] <- "CurrentLoanAmount"

colnames(test)[which(names(test) == "Credit.Score")] <- "CreditScore"

colnames(test)[which(names(test) == "Annual.Income")] <- "AnnualIncome"

colnames(test)[which(names(test) == "Monthly.Debt")] <- "MonthlyDebt"

colnames(test)[which(names(test) == "Years.of.Credit.History")] <- "YearsofCreditHistory"

colnames(test)[which(names(test) == "Months.since.last.delinquent")] <- "Monthssincelastdelinquent"

colnames(test)[which(names(test) == "Number.of.Open.Accounts")] <- "NumberofOpenAccounts"

colnames(test)[which(names(test) == "Number.of.Credit.Problems")] <- "NumberofCreditProblems"

colnames(test)[which(names(test) == "Current.Credit.Balance")] <- "CurrentCreditBalance"

colnames(test)[which(names(test) == "Maximum.Open.Credit")] <- "MaximumOpenCredit"


colnames(test)[which(names(test) == "Tax.Liens")] <- "TaxLiens"

colnames(test)[which(names(test) == "df1Long Term")] <- "dfLongTerm"

colnames(test)[which(names(test) == "df1Short Term")] <- "dfShortTerm"

colnames(test)[which(names(test) == "df1HaveMortgage")] <- "dfHaveMortgage"
colnames(test)[which(names(test) == "df1Home Mortgage")] <- "dfHomeMortgage"

colnames(test)[which(names(test) == "df1Own Home")] <- "dfOwnHome"

colnames(test)[which(names(test) == "df1Rent")] <- "dfRent"

colnames(test)[which(names(test) == "df1Business Credit")] <- "dfBusinessCredit"
colnames(test)[which(names(test) == "df1Consumer  Credit")] <- "dfConsumerCredit"
colnames(test)[which(names(test) == "df1Debt Consolidation")] <- "dfDebtConsolidation"
colnames(test)[which(names(test) == "df1Mortgage Credit")] <- "dfMortgageCredit"
colnames(test)[which(names(test) == "df1Other")] <- "dfOther"
colnames(test)[which(names(test) == "df1Vehicle Credit")] <- "dfVehicleCredit"
colnames(test)[which(names(test) == "df1>=10 years")] <- "dfgreaterthantenyears"
colnames(test)[which(names(test) == "df10-4 years")] <- "dfzerotofouryears"
colnames(test)[which(names(test) == "df15-9 years")] <- "dffivetonineyears"



#convert to factors because they are in 0 and 1 and these are numbers hence model will want factors
num_train$LoanStatus=ifelse(num_train$LoanStatus== 1,'Charged Off','Fully Paid')
test$LoanStatus=ifelse(test$LoanStatus== 1,'Charged Off','Fully Paid')

num_train$LoanStatus=as.factor(num_train$LoanStatus)
test$LoanStatus=as.factor(test$LoanStatus)

library(e1071)


#classifier = naiveBayes(LoanStatus~., num_train)
#predicted = predict(classifier, test_x)
#confm=table(test[,which(names(test) %in% c("LoanStatus"))], predicted)
#recall_accuracy(test[,which(names(test) %in% c("LoanStatus"))], predicted)

#accuracy <- sum(diag(confm))/sum(confm)


test_x<- test[,-which(names(test) %in% c("LoanStatus"))]
test_y<- test[,which(names(test) %in% c("LoanStatus"))]
