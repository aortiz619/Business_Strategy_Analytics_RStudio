# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    MODULE 3 - SCORING
# __________________________________________________________
# //////////////////////////////////////////////////////////


# --- COMPUTING PREDICTORS AND TARGET VARIABLES ------------

############################################################
####             CLEAR DATA COMMAND                     ####
############################################################

# Set your directory to the folder where you have downloaded the dataset 
# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))


############################################################
####                    LOAD DATA                      ####
############################################################

# Load text file into local variable called 'data'
data = read.delim(file = 'purchases.txt', header = FALSE, sep = '\t', dec = '.')


############################################################
####                 ASSIGNING HEADERS                   ####
############################################################

# Add headers and interpret the last column as a date, extract year of purchase
colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')

############################################################
####                 CREATE HEADERS                     ####
############################################################


# Tell the system to create a new variable (column) with the years displayed as YYYY
data$date_of_purchase = as.Date(data$date_of_purchase, "%Y-%m-%d")

# Tell the system to create a new variable (column) with how many days its been since purchasing last up to todaY
data$year_of_purchase = as.numeric(format(data$date_of_purchase, "%Y"))

# The day we will define as today is 2016-01-01
data$days_since       = as.numeric(difftime(time1 = "2016-01-01",
                                            time2 = data$date_of_purchase,
                                            units = "days"))

############################################################
####                DISPLAY HEADERS                     ####
############################################################

# Display the data after transformation
head(data)
summary(data)

############################################################
####                LOAD SQL LANGUAGE                   ####
############################################################

# Load the packages
# Compute key marketing indicators using SQL language
library(gsubfn)
library(proto)
library(RSQLite)
library(sqldf)

# Compute key marketing indicators using SQL language
library(sqldf)


# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#   REMOVING 2015 YEAR - CREATING A MANAGERIAL SEGMENTATION (YEAR 2014 DATASET)
# __________________________________________________________
# //////////////////////////////////////////////////////////


########################################################################################
####   COMPUTE TOTAL RECENCY, FREQUENCY, AND MONETARY/AMOUNT FROM CUSTOMERS 2014    ###
########################################################################################

# --- SEGMENTING A DATABASE RETROSPECTIVELY ----------------

# Compute recency, frequency, and average purchase amount
# Here we will create a new dataset named "customers_2014"
# What we are doing here is pretending we are in year 2014 not 2015
# At the bottom of the formula the "  WHERE days_since > 365" is telling the system to not consired the 365 days after what day we tell it
# We remove 2015 from 2014 by using the "MIN(days_since) - 365 AS 'recency'" and "MAX(days_since) - 365 AS 'first_purchase'" subtracting 365 days from our 2015 data remove the year
# Frequency has no change because the system will just count the customer it sees from 2014 in "recency" and "first_purchase" since number of observations (rows) will reduce
customers_2014 = sqldf("SELECT customer_id,
                               MIN(days_since) - 365 AS 'recency',
                               MAX(days_since) - 365 AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'avg_amount',
                               MAX(purchase_amount) As 'max_amount'
                        FROM data
                        WHERE days_since > 365
                        GROUP BY 1")


########################################################################################
####            FINDING cuRRENT PURCHASES MADE BY SEGMENT ONLY IN 2015               ###
########################################################################################

# We want to compute how much revenue is generated by segments
# Here we are using an SQL function to use "customer id" as our dependent variable 
# We will now have the system sum up all purchases of customers IDs that where made in 2015 (which we self named revenue_2015) that is what this line says [revenue_2015 = sqldf("SELECT customer_id, SUM(purchase_amount) AS 'revenue_2015']
# Notice that people with no revenue in 2015 do NOT appear
# Since are only interested in purchases for year 2015 we tell the system to only look at 2015 "WHERE year_of_purchase = 2015"
revenue_2015 = sqldf("SELECT customer_id, SUM(purchase_amount) AS 'revenue_2015'
                      FROM data
                      WHERE year_of_purchase = 2015
                      GROUP BY 1")


######################################################################
####  MERGING 2014 RESULTS WITH 2015 DATA TO PREDICT REVENUE  ####
######################################################################


# Merge 2014 customers and 2015 revenue
# This ensure that all customer in 2014 are considered when calculating revenue in 2014
in_sample = merge(customers_2014, revenue_2015, all.x = TRUE)

#######################################################################################
#### CREATING NEW DATASET (IN_SAMPLE) - COUNTING WHO HAS MADE A PURCHASED IN 2015  ####
#######################################################################################

# This tells the system to mark any customer in 2014 and not in 2015 using "0" if they have no revenue in 2014 
# the idea is that 0 customer revenue will in 2014 will predict 0 revenue in 2015
in_sample$revenue_2015[is.na(in_sample$revenue_2015)] = 0

# Here we create a new variable (column)
# It says if the customer spend anything greater than 0 ">0" they spent (system will mark a 1 automatically), and under 0 "<0" as a no spend (system keep it at 0)
in_sample$active_2015 = as.numeric(in_sample$revenue_2015 > 0)

#######################################################################
####                Viewing newly created dataset "in_sample        ###
########################################################################

# Display calibration (in-sample) data
head(in_sample)
summary(in_sample)


# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#   PREDICTING 2014 CUSTOMER REVENUE IN 2014
# __________________________________________________________
# //////////////////////////////////////////////////////////

# --- CALIBRATE THE MODELS ---------------------------------


#######################################################################
####    CALCULATING STANDARD DEVIATION AND COEFFICIENT            ###
########################################################################

# Calibrate probability model
# This models purpose is to just to calculate "0" or "1" binary result
# Since we told the system to crate a dataset called "in_sample" that uses binary result on the "active_2015" variable or row, this should work
# This formula tells the system to look at dataset "active_2015~" and make a decision when looking at variables or rows "recency + first_purchase + frequency + avg_amount + max_amount"
# When making predictions "coefficients" and "standard.errors" are important to ensuring accuracy so we want to extract those
library(nnet)
prob.model = multinom(formula = active_2015 ~ recency + first_purchase + frequency + avg_amount + max_amount,
                      data = in_sample)
coef = summary(prob.model)$coefficients
std  = summary(prob.model)$standard.errors

# Coefficients simply tell us how different groups are, its a reinforce to the standard deviation
# The bigger the coefficient the more different the groups are
print(coef)

# standard deviation remember measures dataset difference from average value
# In other words, it measures if segment or groups "are spread out or close together"
# The smaller the standard deviation the closer the relationships and the bigger the further the relationships between variable
print(std)

# Typically good if it lands between -2 and 2.
# This formulate takes both coefficient and standard deviation
# Calculating how close together our segment are and how similar
print(coef / std)


#######################################################################
####    CALCULATING ONLY FOR PURCHASING CUSTOMER IN 2015          ###
########################################################################

# For the monetary model, select only those who made a purchase
z = which(in_sample$active_2015 == 1)
head(in_sample[z, ])

# When summarizing the data, if we look at "max_amount" we can make the conclusion 
# customer in 2015 spent as little at $5 and as much as $4,500 in the year.
summary(in_sample[z, ])

#######################################################################
####    FORECASTING ONLY FOR PURCHASING CUSTOMER IN 2015          ###
########################################################################

# Calibrate the monetary model (version 1)
# Here we are telling the system to look at dataset "revenue_2015" and use the "z" value or purchasing customer in 2105
# This should calculate only customer in 2014 that made purchases in 2015
amount.model = lm(formula = revenue_2015 ~ avg_amount + max_amount, data = in_sample[z, ])
summary(amount.model)

# Now we can use the "fitted.value" to FORECAST
# Plot the results of the monetary model
# Here the chart doesn't look well becuase everyone hasn't spent as much
# Which is good for a birds eye view, but not to present
plot(x = in_sample[z, ]$revenue_2015, y = amount.model$fitted.values)


# Let RESIZE it so that we can get a better representation 
# We will create a linear regression "lm" model and apply the "log()" function to make the numbers more manageble
# Re-calibrate the monetary model, using a log-transform (version 2)
amount.model = lm(formula = log(revenue_2015) ~ log(avg_amount) + log(max_amount), data = in_sample[z, ])
summary(amount.model)

# Plot the results of this new monetary model
plot(x = log(in_sample[z, ]$revenue_2015), y = amount.model$fitted.values)


# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    --- APPLY THE MODELS TO TODAY'S DATA TO PREDICT PURCHASES ---------------------
# __________________________________________________________
# //////////////////////////////////////////////////////////


# Here we want to calculate for out "data" set and then split it so that we can create a "customer_2014" and "customer_2015_ datasets and then use this formula.
# Compute RFM variables as of today
customers_2015 = sqldf("SELECT customer_id,
                               MIN(days_since) AS 'recency',
                               MAX(days_since) AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'avg_amount',
                               MAX(purchase_amount) AS 'max_amount'
                        FROM data GROUP BY 1")

# Predict the target variables based on today's data
customers_2015$prob_predicted    = predict(object = prob.model, newdata = customers_2015, type = "probs")
customers_2015$revenue_predicted = exp(predict(object = amount.model, newdata = customers_2015))
customers_2015$score_predicted   = customers_2015$prob_predicted * customers_2015$revenue_predicted
# Doesn't mean much unless define in the past, where for example the mean of ".22" is higher than year record profit of ".17" 
summary(customers_2015$prob_predicted)
# Tells you how much customer are forecasted to spend overall in the year
summary(customers_2015$revenue_predicted)
# Tells you how much customer are forecasted to spend averagely per transactions
summary(customers_2015$score_predicted)
hist(customers_2015$score_predicted)

# How many customers have an expected revenue of more than $50
z = which(customers_2015$score_predicted > 50)
print(length(z))


#######################################################################
####                    Exploring the data                         ###
########################################################################

# Here we can see our top 10 customer
head(customers_2015, 10)

# Here we can ONLY see the 80th customer
summary(customers_2015[80, ])

#Here we can see customer with ID 80
summary(customers_2015[customers_2015$customer_id == 80, ])


