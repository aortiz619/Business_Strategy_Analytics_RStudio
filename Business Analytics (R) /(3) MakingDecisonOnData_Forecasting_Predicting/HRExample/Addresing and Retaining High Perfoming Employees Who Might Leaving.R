

############################################################
####        EXAMPLE - HR ANALYTICS                     ####
############################################################

# We are going to build a module on employees with us, to see the probability of them leaving us
# so we can take actions are retaining them


############################################################
####             CLEAR DATA COMMAND                     ####
############################################################

# Set your directory to the folder where you have downloaded the Insurance 2 dataset 
# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))

############################################################
####             LOAD OLD & NEW DATA COMMAND           ####
############################################################

# Let's load our dataset
dataold = read.table('DATA_3.02_HR2.csv', header=T, sep=',') # The function read.table enables us to read flat files such as .csv files
datanew = read.table('DATA_4.02_HR3.csv', header=T, sep=',') # The new dataset on which we want to make the prediction

############################################################
#### sUMMARIZED/vIEW "NEW & OLD DATA" VARIABLE COMMAND  ###
############################################################

# The str() function shows the structure of your dataset and details the type of variables that it contains
# Here should examine each variable or column (Here it show us the "average")
# observation means rows
str(datanew)

# The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles
# Here should examine each variable or column (Min, Medium, and Max)
# (Insight 1) We can see here that for example if we look at Newborn "mean" = 0.197 or 20% of our employees have newborns within the last 12 months. We can use this information for example when designing employee perks or benefits.
# (Insight 2) We can see here that for example if we look at Annual Hours or ANH "mean" = 200.2 we can see that our employees worked 200 hours per month
# (Insight 3) We can see here that for exmaple if we look at Time IN Company or TIC "mean" = 3.103 that our employees have spent at least 3 years in our company before leaving
summary(datanew)

# The str() function shows the structure of your dataset and details the type of variables that it contains
# Here should examine each variable or column (Here it show us the "average")
# observation means rows
str(dataold)

# The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles
# (Insight 1) We can see here that for example if we look at Newborn "mean" = 0.154 or 15% of our employees have newborns within the last 12 months. We can use this information for example when designing employee perks or benefits.
# (Insight 2) We can see here that for example if we look at Annual Hours or ANH "mean" = 200.4 we can see that our employees worked 200 hours per month
# (Insight 3) We can see here that for exmaple if we look at Time IN Company or TIC "mean" = 3.229 that our employees have spent at least 3 years in our company before leaving

summary(dataold)


##############################################################
#### Building Old Data Logistic Regression Model COMMAND  ####
##############################################################

# Estimate the drivers of attrition
# Here the "Left" variable is our dependent variable (relates to other point) and the "." references all other variables are independent variables (don't relate to one another)
logreg = glm(left ~ ., family=binomial(logit), data=dataold)

######################################################################################
####  Predicting "Out of Sample New Data predictive logistic regression COMMAND   ####
######################################################################################

# Make predictions on the out-of-sample data
#This data the system has never seen before, so we need to use the predict function with linear as "predict(logreg"
# The purpose of this formula is for the system to give us predictive on "leaving/left" column for the observation of our new data set or "datanew"
# The "response" is telling the system to tell us the result from the function "probaToleave"
probaToLeave=predict(logreg,newdata=datanew,type="response")

##########################################################################
####   View "Left" Column Predictive Out Of Sample New Data Results   ####
##########################################################################

# Use the following command to view the comand of the previous formula above of "probaToLeave = predict(logreg, newdata=datanew, type="response")"
# This formula predicts the probability of every leaving in the "newdata" from "1000 observation" as staed in our "datanew" record count
probaToLeave

#####################################################################
####   Inserting and Organizing "ProbaToLeave" into a Data Frame ####
#####################################################################

# Structure the prediction output in a table
# This formula should take all "1000 observation/rows" from the "datanew" while referencing "dataold"
# The outcome is a list of the "1000 observation/rows" result each listed with a percentage of their probability of leaving the company
predattrition = data.frame(probaToLeave)

##################################################################
####   Viewing "ProbaToLeave" into a Data Frame Table         ####
##################################################################

# View the predattrition dataframe
# This formula will help us visualize the "predattrition = data.frame(probaToLeave)"
# This formula should create a table with a first column numbering up to the "1000 observations/rows" from the "datanew" and next to it another column with their percentage probability of leaving
View(predattrition)

###################################################################################
####   Adding "Performance" Prediction Column Result Into a Data Frame Table   ####
###################################################################################

# We must now add a performance column to the chart we created with "predattrition = data.frame(probaToLeave)"
# This adds a column to the predattrition dataframe containing the performance
# LPE stands for Last Performance Evaluation meaning "what performance reflects today"
predattrition$performance = datanew$LPE

###################################################################################
####   Viewing "Performance" Prediction Column Result Into a Data Frame Table ####
###################################################################################

# View the predattrition dataframe
View(predattrition)

###################################################################################
####   Creating a Plot Chart To Visualize Probability To Leave vs Performance  ####
###################################################################################

# Plot the predicted probability to leave vs. the performance
plot(predattrition$probaToLeave, predattrition$performance)

#######################################################################################
#### Adding Text To A Plot Chart To Visualize Probability To Leave vs Performance  ####
#######################################################################################

# Represent the horizontal "lay flat" line
#abline(h=0.53,col="red")
abline(h=0.53,col="red")

# Represents the vertical "up and down" line
#abline(v=0.39, col="red")
abline(v=0.39, col="red")

# Represents normal everyday workers, they perform high, and leave within an average amount of time in the industry
#text(0.27, 0.76,"As usual", col = "red")
text(0.27, 0.76,"Typical Employee", col = "red")

# Represents those who underperform and because of it, typically leave the company
#text(0.4,0.47,"Up or Out", col = "red")
text(0.4,0.47,"Low yielding Employee", col = "red")

# Represent those who we want to retain, they are high performers but are very likely to leave the company
#text(0.7, 0.76, "To be retained", col="red")
text(0.7, 0.76, "Employees To Retain", col="red")

############################################################################################
####        Adding "Priority Score" Prediction Column Result In a Data Frame Table      ####
############################################################################################

# Now that we can visualize in a chart what effect performance has on employees leaving the company we can rank the important
# We now need to be able to identify employees that should be prioritize and prevent them from leaving base on their probability of leaving and performance
# We will use this formulat to calculate the priority based on performance and probability to leave
predattrition$priority = predattrition$performance * predattrition$probaToLeave

############################################################################################
####       Viewing "Priority Score" Prediction Column Result In a Data Frame Table      ####
############################################################################################

# View the predattrition dataframe from the last formula "predattrition$priority = predattrition$performance * predattrition$probaToLeave"
View(predattrition)

#############################################################################################
#### Adding High to Low "Priority Score" Prediction Column Result In a Data Frame Table  ####
#############################################################################################

# Now that we got the "priority score" added to the data frame table we know who to talk to, but who do we star with?
# The following formula will order the predictions by priority in descending order
# This way we know which employee is the first priority
orderpredattrition = predattrition[order(predattrition$priority, decreasing=TRUE),]

##############################################################################################
#### Viewing High-to-Low "Priority Score" Prediction Column Result In a Data Frame Table  ####
##############################################################################################

# View the ordered predictions from the formula before "orderpredattrition = predattrition[order(predattrition$priority, decreasing=TRUE),]"
# Now the company can decide how many they want to star or prioritize base on the company
View(orderpredattrition)

#############################################################################################
#### Adding Low To High "Priority Score" Prediction Column Result In a Data Frame Table  ####
#############################################################################################

# Now that we got the "priority score" added to the data frame table we know who to talk to, but who do we star with?
# The following formula will order the predictions by priority in descending order
# This way we know which employee is the first priority
orderpredattrition = predattrition[order(predattrition$priority, decreasing=FALSE),]

##############################################################################################
#### Viewing Low-To-High "Priority Score" Prediction Column Result In a Data Frame Table  ####
##############################################################################################

# View the ordered predictions from the formula before "orderpredattrition = predattrition[order(predattrition$priority, decreasing=TRUE),]"
# Now the company can decide how many they want to star or prioritize base on the company
View(orderpredattrition)

##################################################################
#### Calculating LEss Likely To Leave Base on Performance     ####
##################################################################


# This calculation will tell us the ID of the person who is less likely to leave according to the model
# Here performance is "0" 
# Remember in this example "0"= low performance and "1"= high performance. So the closer to "1" the higher the performance
predattrition$performance=datanew$LPE 
x<- which(predattrition$performance >0)
which.min(predattrition[x,1])


# This calculation will tell us the ID of the person who is less likely to leave according to the model, but performance is larger than .90
# Here performance is "0.90" 
# Remember in this example "0"= low performance and "1"= high performance. So the closer to "1" the higher the performance
predattrition$performance=datanew$LPE 
x<- which(predattrition$performance >0.90)
which.min(predattrition[x,1])


