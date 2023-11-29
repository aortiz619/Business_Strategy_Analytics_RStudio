


# - - - - - -  Don't Change Any lines - - - - - - - 

# clean up the memory of your current R session
rm(list=ls(all=TRUE))

# lets load our database
# Remember that whatever you decide to name your value is what needs to be input in the next formula "in this case we choose datatot"
datatot=read.table('DATA_3.02_HR2.csv', header = T,sep=',')

# Now let's have a look at our variables and see some summary statistics
# The str() function shows the structure of your dataset and details the type of variables that it contains
# This function shows us that we have 7 variables (represented by $)
str(datatot)

# The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles
# This tells us that for example in the "left" column that "0.16 or 17%" of employees have left# 
# This function is great to just familiarize quickly with your dataset
summary(datatot)

# look at the frequencies for the left variable
# This formula lets us know that "0" is the number of employees that stayed and "1" is the number of employees that left
table(datatot$left)

# look at percentages for the left variable
#This formulat is the same information but just in a percentage format "0" is employees who stayed and "1" is employees who left
table(datatot$left)/nrow(datatot) 

# alternatively, plot a histogram
#This lets us see the same information but in a graph form, if you need to show leardersihp
hist(datatot$left)

# Let's check out the correlations
# This tells us the linear correlation between 2 linear varaibles (meaning that relate to one another)
# Here we can a correlation of "left" variable, for example newborn negatively impact a left result
cor(datatot)


# Estimate the drivers of attrition
# But we dont' want to know the impact that one has on everything
# To know the impact that each has on one another and their intereaciton, we want to create a logistic regression, so we don't miss details.
logreg = glm(left ~ ., family=binomial(logit), data=datatot) 


# See the proportion of employee attrition according to the model
# This formula lets us base on a "0" employees staying to "1" employees leaving, how many employee are likely to leave or "left"
# But this is just an estimation formula
hist(logreg$fitted.values) 

# We want to get an actual 
# To assess the correlation between estimated attrition and actual
# This gives us the linear relationship between the two
# a correlation that lands between "0" or +1 means it's a strong relationship
# a "0" correlation tipically means there is no relationship
cor(logreg$fitted.values,datatot$left)


# Now that we know their is a correlation or relationship between the variables we want to set a benchmark or rule
# We will name this benchmark as "cuttoff=.05", now "above .05 is left" and "below .05 is stays"
# Cutoff to determine when P[leaving] should be considered as a leaver or not. Note you can play with it...
# But you'll realize that a "large cutoff value" will mean that we can rarely predict an outcome in this case that the employee is leaving
# But you'll realize that a "small cutoff value" will mean that we can rarely predict an outcome in this case that the employee is staying
cutoff=.5

# Now we established and will know that anything below is a "stay" in the company and anything above is a "left" the company
# But we want set up how the system will let us know. This formula lets us know by saying True or False.
# False being predicted
# True being Actually happened
table(logreg$fitted.value>=cutoff,datatot$left)


# Now lets compute the percentage of correctly classified employees who stayed
# This is the manual version "9464/(9464+536)" but below is the prefer way of doing it
sum((logreg$fitted.values<=cutoff)&(datatot$left==0))/sum(datatot$left==0)


# And lets compute the percentage of correctly classified employees who left
# This is the manual version "381/(1619+381)" but below is the prefer way of doing it
sum((logreg$fitted.values>cutoff)&(datatot$left==1))/sum(datatot$left==1) 

# Compute the overall percentage of correctly classified employees
# This is the manual version "(9464+381)/(9464+536+381)" but below is the prefer way of doing it
mean((logreg$fitted.values>cutoff)==(datatot$left==1)) 


# Report the results of the logistic regression
# This lets us know what distinguishes our catergories
# For example here we can see that our satisfaction level is the most important since it has a higher negative value
summary(logreg) 

#Lets create a chart
# Let's use a more visual way to see the effect of one of the most important driver: TIC
# This chart won't show us anything, because it takes the "0" and "1" values in the "left" variable confuses the system
plot(datatot$TIC,datatot$left,main= "Time and Employee Attrition", ylab="Attrition", xlab= "Time spent")

#How dow we fix this problem?
# So lets try computing the "left" by years it took to for a person to leave
# Firs lets save our data as an aggregated plot
tempdata=datatot

# We compute the average attrition rate for each value of TIC
# Since the "left" variable or column has numeral values "0" or "1" to identify stayed or left we will use TIC to aggregate these
aggbTimeRank=aggregate(left~ TIC, data=tempdata, FUN=mean)


# Now we can see 2 values
# TIC which represent the years that an employee stayed
# Left which represent the "population percentage"
aggbTimeRank


# Let try visualizing it again
# Much better but we don't know how many employees each point represents which is TIC or years and employee stayed
plot(aggbTimeRank$TIC,aggbTimeRank$left,main= "Time and Employee Attrition", ylab="Average Attrition Rate", xlab= "Time spent")

# We compute the number of employees for each value of TIC
cntbTimeRank=aggregate(left~ TIC, data=tempdata, FUN=length)

# BUt lets get an accurate number rather than a percentage by using the below formula
cntbTimeRank

# Now that the system identified this we can try again with a symbols formula
# This formula visualizes the TIC or year an employee stayed population by size red circle
symbols(aggbTimeRank$TIC,aggbTimeRank$left,circles=cntbTimeRank$left, inches=.75, fg="white", bg="red",main= "Time and Employee Attrition", ylab="Average Attrition Rate", xlab= "Time spent") 


#Let now work with another variable
# Lets save our data again
tempdata=datatot

# Let's use a more visual way to see the effect of the most important driver: Satisfaction
# We create categories of employee satisfaction ranking. We create 20 groups (because it will work well later...)
tempdata$rankSatis = round(rank(-tempdata$S)/600) 

#let check out the output using the following
# This will show satisfaction number between 0-20
# This allow us to have 21 groups of satisfcation
tempdata$rankSatis

# We compute the average attrition rate for each category
aggbSatisRank = aggregate(left~ rankSatis, data=tempdata, FUN=mean) 

# We compute the number of employees for each value of TIC
cntbSatisRank = aggregate(left~ rankSatis, data=tempdata, FUN=length) 

symbols(aggbSatisRank$rankSatis,aggbSatisRank$left,circles=cntbSatisRank$left, inches=.2, fg="white", bg="red",main= "Satisfaction and Employee Attrition", ylab="Average Attrition Rate", xlab= "Rank of Satisfaction")

# This lets us see that people who stay "0" what their satisfaction level is from 0 to 20
# Same with people who are "1" who left.
# with 0 being unsatisfied 