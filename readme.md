The dataset was downloaded from the below url
https://www.kaggle.com/zaurbegiev/my-dataset


## OVERVIEW
Our dataset is a record of the bank accounts who took loan from the bank, and whether they returned the loan to the bank or they were charged off? So that when a new person wants to take loan from the bank. The bank can actually predict whether this person is more likely to pay off his loan fully. Or is he/she going to be charged off.

Let's discuss what does the data actually consist of. There are total 19 features. The dependent feature is named "Loan Status"
The loan status has value either "Fully Paid" or "Charged off". This is the column which we have to predict. Since our column which needs to be predicted takes binary classes, hence it is a binary classification problem. 


## EXPLORATORY DATA ANALYSIS
Let's plot a bar plot for visualizing the count of those people who were charged off and those who returned the loan fully.

![Image description](https://github.com/ikkhannn/Loan-status-prediction/blob/master/images/Rplot1.png)


Now let's visualize the count of people by their job history.

![](https://github.com/ikkhannn/Loan-status-prediction/blob/master/images/Rplot2.png)
<br />
This tell's us that most of the people who took loan from the bank were mostly those who had spent more than 10 years in current job.

Let's check the proportion of people who were charged off and who weren't.
![](https://github.com/ikkhannn/Loan-status-prediction/blob/master/images/Rplot3.png)
<br />
This tells us the same thing which we saw in the previous plot but the additional thing is that those were charged off, and those who weren't , both had the most proportion when the job years they spent was more than 10.



Let's see visualize the proportion of charged off and fully paid loans when we group by short term and long term loans.
![](https://github.com/ikkhannn/Loan-status-prediction/blob/master/images/Rplot4.png)
<br />
This describes, that loan which were short term were more likely to be fully paid.

Let's see for what purpose were the loans taken. Seperately by long term and short term. And whether charged off or fully paid.
<br/>
![Image description](https://github.com/ikkhannn/Loan-status-prediction/blob/master/images/Rplot5.png)
<br/>
We can see that the loans were mostly taken for the purpose of buying a house.


## DATA CLEANING AND PREPARATION
The data contains alot of missing values. <br />
We will split the data by loan status type and then take mean of every column and fill those n/a values.<br />
Changed the column names.<br />
Applied n4 normalization.<br />
Rounded values off.<br />

## MODELLING
The Algorithm which i have used is "decision trees".<br />
Splitted the data into 90 % training and 10 % testing.<br />
This produced results with 92 % accuracy.<br />

