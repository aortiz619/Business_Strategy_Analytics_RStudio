
############################################################
####    EXAMPLE N°1 - CREDIT SCORE ANALYTICS 3          ####
############################################################

# This example is focused on making prediction and forecasting
# This example works with out of sample data (old data) and comparing it to out of sample data (new data) which is data never seen by the system yet 


############################################################
####             CLEAR DATA COMMAND                     ####
############################################################

# Set your directory to the folder where you have downloaded the Insurance 2 dataset 
# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))

############################################################
####             LOAD OLD & NEW DATA COMMAND           ####
############################################################

# Let's load our dataset and call it data
dataold = read.table('DATA_3.01_CREDIT.csv', sep=',',header=TRUE) # The function read.table enables us to read flat files such as .csv files
datanew = read.table('DATA_4.01_CREDIT2.csv', sep=',',header=TRUE) # The function read.table enables us to read flat files such as .csv files

############################################################
#### sUMMARIZED/vIEWing "NEW & OLD DATA" VARIABLE COMMAND  #
############################################################

# Now let's have a look at our variables and see some summary statistics
# The str() function shows the structure of your dataset and details the type of variables that it contains
# Here should examine each variable or column (Here it show us the "average")
# observation means rows
str(datanew)

# The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles
# Here should examine each variable or column (Min, Medium, and Max)
summary(datanew)

# Lets compare it to the old data
# The str() function shows the structure of your dataset and details the type of variables that it contains
# Here should examine each variable or column (Here it show us the "average")
# observation means rows
str(dataold)

# The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles
# Here should examine each variable or column (Min, Medium, and Max), and how they changed compare to the new data
summary(dataold)

############################################################
#### Building Old Data Linear Regression Model COMMAND  ####
############################################################

# For our old model we want to get it ready to test data
# Estimate a linear regression model of Rating as a function of everything else.
# Here the "Rating" variable is our dependent variable (relates to other point) and the "." references all other variables are independent variables (don't relate to one another)
linreg = lm(Rating ~ ., data=dataold)

#######################################################################
####    Predicting New Data predictive linear regression COMMAND   ####
#######################################################################

# Make predictions on the out-of-sample data
#This data the system has never seen before, so we need to use the predict function with linear as "predict(lingreg"
# The purpose of this formula is for the system to give us predictive credit scores for the observation of our new data set or "datanew"
# The "response" is telling the system to tell us the result from the function "predcreditscore"
predcreditscore = predict(linreg, newdata=datanew, type="response") 

############################################################
####    View CREDIT SCORE Predictive NEW Data Results   ####
############################################################

# Use the following command to view the command of the previous formula above of "predcreditscore = predict(linreg, newdata=datanew, type="response")"
# This formula predicts the credit score of every one in the "newdata" from "100 observation" as staed in our "datanew" record count
predcreditscore

##################################################################################
#### Finding Correlation of Fitted vs Actual Values In Old Data (In Sample)   ####
##################################################################################

# Computes the correlation between the fitted values first and the actual value second 
# Gives a result of "0.9867324", we have to wait for the new data correlation finding to compare
cor(linreg$fitted.values, dataold$Rating)

##########################################################################################
#### Vizualizing the Correlation of Fitted vs Actual Values In Old Data (In Sample)   ####
##########################################################################################

# Plot the fitted values vs. the actual ones
plot(dataold$Rating, linreg$fitted.values)

##############################################################################################
#### Finding the Correlation of Fitted vs Actual Values In NEW DATA (Out Of Sample)       ####
##############################################################################################

# Computes the correlation between the fitted values and the actual ones
# Gives result of "0.988097", when comparing it to the old data correlation finding a stronger output
cor(predcreditscore, datanew$Rating)

##############################################################################################
#### Vizualizing the Correlation of Fitted vs Actual Values In NEW DATA (Out Of Sample)   ####
##############################################################################################

# Plot the fitted values vs. the actual ones
# We see that we here that we get less data points because we only have "100" observation/records in "datanew" vs "dataold" has "300 observations/record". Makes sense
# But this module or "datanew" does a pretty good job for semi high values but not so good for lower values
plot(datanew$Rating, predcreditscore)
