

###########################################################
####        EXAMPLE N°3 - PREDICTIVE MAINTENANCE       ####
###########################################################

# In this example we are focusing on learning how to do a survival analysis
# In this example, we want to estimate the remaining lifetime of our machine pieces to better organize maintaince efforts 

############################################################
####             CLEAR DATA COMMAND                     ####
############################################################

# Set your directory to the folder where you have downloaded the Predictive Maintenance dataset 
# to clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))

############################################################
####             LOAD DATA COMMAND                       ####
############################################################

# Let's load the data
data=read.table('DATA_4.03_MNT.csv',sep=',',header=TRUE)

############################################################
#### sUMMARIZED/vIEWing DAT" VARIABLE COMMAND  #
############################################################

# Now let's have a look at our variables and see some summary statistics
# The str() function shows the structure of your dataset and details the type of variables that it contains
# Here should examine each variable or column (Here it show us the "average")
# observation means rows
str(data)

# The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles
# Here should examine each variable or column (Min, Medium, and Max)
# (Insight 1) We can see here that for example if we look at broke "mean" = 0.397 or 40% meaning around 40% of our machine pieces are broken
# (Insight 2) We can see here that for example if we look at pressureInd "mean" = 98.60 
# (Insight 3) We can see here that for exmaple if we look at moistureInd "mean" = 99.38
# (Insight 4) We can see here that for exmaple if we look at temperatureInd "mean" = 100.63
summary(data)

############################################################
####  Additional Summary Variable COMMAND (If Needed)   ####
############################################################

# We weren't able to get an accurate count on teams using the "summary(data)
# We have to use a manual command of "variable_counts <- table(tablename$variable)
team_counts <- table(data$team)

# Now we want to be able to display the summary
# In this case 'print" is used rather than "view" because  it can be beneficial in more complex scenarios or when you want to include additional information or formatting.
print(team_counts)


# We weren't able to get an accurate count on teams using the "summary(data)
# We have to use a manual command of "variable_counts <- table(tablename$variable)
provider_counts <- table(data$provider)

# Now we want to be able to display the summary
# In this case 'print" is used rather than "view" because  it can be beneficial in more complex scenarios or when you want to include additional information or formatting.
print(provider_counts)


############################################################
####    Building Linear Regression Model COMMAND        ####
############################################################

# Build a linear regression model
# Here the "lifetime" variable is our dependent variable (relates to other point) and the "." references all other variables are independent variables (don't relate to one another)
# We also see "-broken" which mean we won't count this variable
# You could use all the variable to the model after the tilde and seperate each of them by a plus sign
# But its not ideal or efficient 
linregmodel = lm(lifetime~.-broken,data=data) 

#################################################################
####    Viewing Linear Regression Model Result COMMAND        ####
##################################################################

# The summary() function shows the output of your model
# Lets us see coefficients (constant quantity)
# But remember that using the output of a liner regression isn't reliable so its not practical to use
# Why? For example, in this scenario 60% of the observations are not broken, and we don't know if lifetime is properly measured when the machine piece was broken, but rather lifetime as it is, even if the machine piece is functioning
summary(linregmodel) 

#################################################################
####        Installing Survival Package COMMAND               ####
##################################################################

# Install the survival package to your computer
install.packages("survival")

#################################################################
####        Loading The Survival Package COMMAND            ####
##################################################################

# Load the survival package
# Know that now that the "survival" package is install you won't have to install it again or do the "install.packages("survival") function again to install it
# However, you will have to run the "library(survival) command everytime you start a new session or clean up the system memory
library(survival)

#################################################################
####        Setting Dependent Variables COMMAND             ####
##################################################################

# choose the dependant variables to be used in the survival regression model with the Surv() function
# In this case the dependant is the "lifetime" of a machine piece, and whether or not if that machine piece is "broken"
dependantvars = Surv(data$lifetime, data$broken)

#################################################################
####                Building Survival Model                  ####
##################################################################

# Create your survival regression model using survreg()
# We use the "gaussian" distance in order to evaluate the distance base on that method
survreg = survreg(dependantvars~pressureInd+moistureInd+temperatureInd+team+provider, dist="gaussian",data=data)

#################################################################
####                Checking Survival Model                  ####
##################################################################

# The summary() function shows the output of your model
# Here we can now check the "p value" to check which variable is statistically significant
# (Significant Variable 1) - we see that a positive "p value" on "pressuredInd" makes it significant
# (Significant Variable 2) - we see that a positive "p value" on "moistureInd" makes it significant
# But how do we access if the effect was positive or negative base on the expected lifetime we look at the "value" column.
# (Value Row Insight 1) the values "moistureInd", "provider 2", and "provider 4" has a positive expected lifetime on machine pieces
# So these 3 value(s) extend the lifetime somehow
summary(survreg) 

#################################################################
####      Calculating Expected Machine Piece Lifetime        ####
##################################################################

# Make predictions based on the model. 
# Here we estimate the median lifetime as the expected moment of "death"
# Only measuring those pieces that are currently not broke but will in the future
# The "Ebreak=predict" formula will help us with this, and the type "quantile" as it up
# "p" is set at ".5" benchmark to measure broke or operational machine pieces. Remember in "p" a "0' means no change and closer to "1" means change
Ebreak=predict(survreg, newdata=data, type="quantile", p=.5) 

######################################################################################
####  Creating Statement "Expected Machine Piece Lifetime" To a Dataframe/Table  ####
######################################################################################

# Create a dataframe to store the ouput of Ebreak
Forecast=data.frame(Ebreak)

#################################################################################
####  Adding Column "Expected Machine Piece Lifetime" To a Dataframe/Table  ####
#################################################################################

# Add a column in the Forecast dataframe indicating the lifetime of the piece
Forecast$lifetime=data$lifetime

######################################################################################
####  Adding Column "Is Machine Piece Expected To Be Broken" To a Dataframe/Table  ####
######################################################################################

# Add a column in the Forecast dataframe indicating whether or not the piece is broken
Forecast$broken=data$broken 

#########################################################################################
####  Adding Column "Remaining Machine Piece Expected Lifetime" To a Dataframe/Table  ####
########################################################################################

# Computed Expected Remaining Lifetime
Forecast$RemainingLT=Forecast$Ebreak-data$lifetime

#########################################################################################
####    Viewing all "Expected Machine Piece Lifetime" columns in a Dataframe/Table  ####
########################################################################################

# View the complete Forecast dataframe
# When viewing this dataframe we see the following questions 
# (observation 1) "broken column" includes both "0" and "1" which are broken and operational, but broken pieces are to late for us to address if we are seeking pieces not broken but that we want to catch before they break
# (Observation 2) "reamining machine piece expected lifetime column" does not have our most important priority first,we need to create a filter for it to do that for us, so we know where to star
View(Forecast)

###############################################################################################
####    Reorder High-To-Low "Expected Machine Piece Lifetime" columns in a Dataframe/Table  ####
###############################################################################################

# Order the elements by Expected Remaining Lifetime
Forecast=Forecast[order(Forecast$RemainingLT),]

#######################################################################################################
####  Removing broken results from "Is Machine Piece Expected To Be Broken" To a Dataframe/Table"   ####
######################################################################################################

# And keep only those who are not broken yet
ActionsPriority=Forecast[Forecast$broken==0,] 

#########################################################################################
####    Viewing all "Expected Machine Piece Lifetime" columns in a Dataframe/Table  ####
########################################################################################

# View the output and take actions!
View(ActionsPriority)


# __________________________  Independent QUestions _____________________________________________

#########################################################################################
####    What if we changed the P-Value to 0.01 rather than 0.05                      ####
########################################################################################

# Here we are changing p value to think of signinficant starting at 0.05 to 0.01
# This change in P value is done when we want to prevent propability or errors since closer to "0" means strong evidence or accurate
# So the p value rule stills applies - anything closer to "0" means its evidence is significant, anything closer to "1" is hypothesis
# Base on this calculation, if anything over 0.01 is non-significant we can make the following observations
# pressureInd "p" value is 0.557 = non-significant since its .4 over 0.01
# moistureInd "p" value is 0.012 = non-significant since its .002 over 0.01
# temperatureInd "p" value is <2e-16 or 0.0000000000000002 = Significant since its under 0.01
data=read.table('DATA_4.03_MNT.csv',sep=',',header=TRUE)
linregmodel = lm(lifetime~.-broken,data=data)
library(survival)
dependantvars = Surv(data$lifetime, data$broken)
survreg = survreg(dependantvars~pressureInd+moistureInd+temperatureInd+team+provider, dist="gaussian",data=data)
summary(survreg)

##############################################################################################################################
####  What if we changed the P-Value to 0.01 rather than 0.05 & Only Called Specific Dependent/Explanatory Variables #####
##############################################################################################################################

# Here we will use the normal formula of survreg(dependantvars) to call variables using gaussian
# However, we will only call the dependant variables "pressureInd", "moistureInd", and "temperatureInd"
# We will exclude the other variables like "team" and "provider"
# Here we are changing p value to think of signinficant starting at 0.05 to 0.01
# This change in P value is done when we want to prevent propability or errors since closer to "0" means strong evidence or accurate
# So the p value rule stills applies - anything closer to "0" means its evidence is significant, anything closer to "1" is hypothesis
# Base on this calculation, if anything over 0.01 is non-significant we can make the following observations
# pressureInd "p" value is 0.36 = non-significant since its .4 over 0.026
# moistureInd "p" value is 0.43 = non-significant since its .002 over 0.33
# temperatureInd "p" value is 0.37 = non-Significant since its under 0.027
# The decision to include or exclude variables also depends on the research question. 
# If the goal is to understand the relationship between the predictors included in the model and the response variable, unnecessary variables may be omitted.
data=read.table('DATA_4.03_MNT.csv',sep=',',header=TRUE)
linregmodel = lm(lifetime~.-broken,data=data)
library(survival)
survreg = survreg(dependantvars~pressureInd+moistureInd+temperatureInd, dist="gaussian",data=data)
summary(survreg)

###############################################################################################################
####  What If We Wanted To Calculate The Machine Piece With The Largerst Expected Remaining Lifetime       ####
##############################################################################################################

# These are normal formulas that don't neccesary need to change when answering questions
data=read.table('DATA_4.03_MNT.csv',sep=',',header=TRUE)
linregmodel = lm(lifetime~.-broken,data=data)

# This load the survival command
library(survival)

#This line is fititng a survival regression model 
survreg = survreg(dependantvars~pressureInd+moistureInd+temperatureInd+team+provider, dist="gaussian",data=data)

# This line predicts the breakpoint using the survival regression model.
# we assume our p-value is the normal standard of .05 with anything close to "0" meaning accurate and anything close to "1" being hypothesis or less evident
Ebreak=predict(survreg, newdata=data, type="quantile", p=.5)

# This looks at our dataset named "data" and forecast for equipment breaking or "Ebreak"
Forecast=data.frame(Ebreak)

# This lines add columns "lifetime" from the original data to the Forecast data frame. 
Forecast$lifetime=data$lifetime

# This lines add columns "broken" from the original data to the Forecast data frame. 
Forecast$broken=data$broken

# This line calculates the remaining lifetime by subtracting the original lifetime from the predicted breakpoint.
Forecast$RemainingLT=Forecast$Ebreak-data$lifetime

#This line finds the index of the maximum value in the "RemainingLT" column of the Forecast data frame, indicating the row with the maximum remaining lifetime.
which.max(Forecast$RemainingLT)

###############################################################################################################
####  What If We Wanted To Calculate The 10 Machine Piece With The Largerst Expected Remaining Lifetime       ####
##############################################################################################################

# Assuming you've already executed the previous code

# Sort the Forecast data frame based on RemainingLT in descending order
Forecast_sorted <- Forecast[order(-Forecast$RemainingLT), ]

# Select the top 10 rows
top_10 <- head(Forecast_sorted, 10)

# Print the top 10
print(top_10)



