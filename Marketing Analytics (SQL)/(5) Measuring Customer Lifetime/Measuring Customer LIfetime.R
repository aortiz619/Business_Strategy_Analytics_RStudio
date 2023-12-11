# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    MODULE 4 - CUSTOMER LIFETIME VALUE
# __________________________________________________________
# //////////////////////////////////////////////////////////


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


############################################################
####       COMPUTE CUSTOMER SEGMENTS IN 2015           ####
############################################################

# Compute recency, frequency, and average purchase 
# We will start by letting the system to use customer ID as our dependent variable
# We will compute "Recency" by telling it to get us the minimum amount of days visited
# We will compute "First_purchase" by telling it to get us the max or latest day since purchase
# We will compute "Frequency" by telling it to count the times of "visit times"
# Segment customers in 2015
customers_2015 = sqldf("SELECT customer_id,
                               MIN(days_since) AS 'recency',
                               MAX(days_since) AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'amount'
                        FROM data GROUP BY 1")
customers_2015$segment = "NA"
customers_2015$segment[which(customers_2015$recency > 365*3)] = "inactive"
customers_2015$segment[which(customers_2015$recency <= 365*3 & customers_2015$recency > 365*2)] = "cold"
customers_2015$segment[which(customers_2015$recency <= 365*2 & customers_2015$recency > 365*1)] = "warm"
customers_2015$segment[which(customers_2015$recency <= 365)] = "active"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$first_purchase <= 365*2)] = "new warm"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$amount < 100)] = "warm low value"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$amount >= 100)] = "warm high value"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$first_purchase <= 365)] = "new active"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$amount < 100)] = "active low value"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$amount >= 100)] = "active high value"
# This order the segments (important)
customers_2015$segment = factor(x = customers_2015$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", "new active"))


############################################################
####       COMPUTE CUSTOMER SEGMENTS IN 2014           ####
############################################################

# Segment customers in 2014
customers_2014 = sqldf("SELECT customer_id,
                               MIN(days_since) - 365 AS 'recency',
                               MAX(days_since) - 365 AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'amount'
                        FROM data
                        WHERE days_since > 365
                        GROUP BY 1")
customers_2014$segment = "NA"
customers_2014$segment[which(customers_2014$recency > 365*3)] = "inactive"
customers_2014$segment[which(customers_2014$recency <= 365*3 & customers_2014$recency > 365*2)] = "cold"
customers_2014$segment[which(customers_2014$recency <= 365*2 & customers_2014$recency > 365*1)] = "warm"
customers_2014$segment[which(customers_2014$recency <= 365)] = "active"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$first_purchase <= 365*2)] = "new warm"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount < 100)] = "warm low value"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount >= 100)] = "warm high value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$first_purchase <= 365)] = "new active"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount < 100)] = "active low value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount >= 100)] = "active high value"
# This order the segments (important)
customers_2014$segment = factor(x = customers_2014$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", "new active"))


###############################################################
####  MERGING 2014 & 2015 DATASET INTO "NEW_DATA"           ####
################################################################


# NOTE: Since dataframes in "2014" and "2015" have similar variable (column) names, we gotta tell the system to merge by the datasets by "customer ID" on the left only.
# Otherwise, If we don't specify the system will replace the variable (column) names since they are duplicated.
# Compute transition matrix
new_data = merge(x = customers_2014, y = customers_2015, by = "customer_id", all.x = TRUE)
head(new_data)


#############################################################################
####  DISPLAYING 2014 TO 2015 DATASET RESULTS INTO "NEW_DATA"           ####
###########################################################################

# Now that they are merged, lets tell the system to combine results from "new_data" and give us the combine totals for the varibles "columns" we created 
# This shows us what happened from 2014 to 2015
# Example 1: from 2014 to 2015 7k accounts became inactive base on our criteria of not visiting after 3 years.
# Example 2: New Customer "new active" from 2014 to 2015 had 1.4k, 500 which made purchased but 900 which didn't and became "new warm"
transition = table(new_data$segment.x, new_data$segment.y)
print(transition)

#################################################################################
####  DISPLAYING 2014 TO 2015 DATASET PERCENTAGE CHANGE "NEW_DATA"            ####
#################################################################################

# Here we can see the percentage each customer change base on a 1 scale
# Example: inactive customer "96%" change of staying inactive and a "4%" change of becoming active
# Divide each row by its sum (as a decimal)
transition = transition / rowSums(transition)
print(transition)

# Divide each row by its sum (as a percerntage)
transition_percentage <- (transition / rowSums(transition)) * 100
print(transition_percentage)


##############################################################################
####                     CREATING A 2015 - 2025 TABLE                       ###
################################################################################

# Here we create a table to output our segments if needed
# We tell the sytem how many columns and rows to output
# Initialize a matrix with the number of customers in each segment today and after 10 periods
segments = matrix(nrow = 8, ncol = 11)

# Let's start by telling the system to put customers from (customer_2015) on the first column, assuming it has more customer "18417" than 2014 "`16905"
segments[, 1] = table(customers_2015$segment)

# Now lets name the columns
# Here rathe than using "name, name" we will want to measure over time, so we will used a date range "YYYY:YYYY"
colnames(segments) = 2015:2025

# Now we will tell the system to add our "customer_2015" segments of "inactive...." and add it to "segment" table
row.names(segments) = levels(customers_2015$segment)

#Now lets view it in the RScript
print(segments)

##############################################################################
####        Predicting 2016 - 2025 RESULTS ON CREATED TABLE                ###
################################################################################

# "for "(i in 2:11)" simply means look results until column 11
# Here we tell the system now use a transition metric "for (i in 2:11) {" meaning it will do an action over and over again for columns 2:11"
# In this case 2:11 is our years in the columns
# 
# Compute for each an every period
for (i in 2:11) {
   segments[, i] = segments[, i-1] %*% transition
}

#####################################################################################
####       Viewing Predicting 2016 - 2025 RESULTS ON CREATED TABLE                ###
####################################################################################

# Plot inactive, active high value customers over time

# Column 1 Results = inactive users (user will become more inactive overtime)
barplot(segments[1, ])
#Column 2 Results = cold customers (shows that cold customer will become warm then cold and eventually will settle at lower vaule
barplot(segments[2, ])

# Display how segments will evolve over time
print(round(segments))

#####################################################################################
####                    COMPUTING 2015 YEARLY REVENUE PER SEGMENT                 ###
####################################################################################

revenue_2015 = sqldf("SELECT customer_id, SUM(purchase_amount) AS 'revenue_2015'
                      FROM data
                      WHERE year_of_purchase = 2015
                      GROUP BY 1")

# Let view the result
summary(revenue_2015)
actual = merge(customers_2015, revenue_2015, all.x = TRUE)
actual$revenue_2015[is.na(actual$revenue_2015)] = 0
aggregate(x = actual$revenue_2015, by = list(customers_2015$segment), mean)

# Show average revenue per customer and per segment
aggregate(x = actual$revenue_2015, by = list(customers_2015$segment), mean)


########################################################################################
####             STORING 2015 REVENUE PER SEGMETN RESULTS                           ###
########################################################################################

# Yearly revenue per segment
# This comes directly from module 2, lines 160-161
yearly_revenue = c(0, 0, 0, 0, 0, 323.57, 52.31, 79.17)

########################################################################################
####      TAKING "SEGMENT" PREDICTION OF 2015 - 2025 AND PREDICT REVENUE              ###
########################################################################################

# Here we take our predicted "segments" database wiht predictions on OF Customer purchasing trends to from "2015-2025"
# We then tell the system take those "segments" and multiply it by "yearly_revenue" to predict revenue
# Compute revenue per segment
revenue_per_segment = yearly_revenue * segments
print(revenue_per_segment)


# Now we take this and Compute yearly revenue
# Here we get predictive revenue
yearly_revenue = colSums(revenue_per_segment)
print(round(yearly_revenue))

# Here we plot it
barplot(yearly_revenue)

########################################################################################
####        TOTAL COMPANY TOTAL ADDED REVENUE GENERATED OVER THE YEARS              ###
########################################################################################

# Compute cumulated revenue
cumulated_revenue = cumsum(yearly_revenue)
print(round(cumulated_revenue))
barplot(cumulated_revenue)

########################################################################################
####                  COMPUTING DOLLAR DEPRECIATION FROM 2015 - 2025             ###
########################################################################################

# Create a discount factor
discount_rate = 0.10
discount = 1 / ((1 + discount_rate) ^ ((1:11) - 1))
print(discount)

###########################################################################################
####        TOTAL COMPANY TOTAL ADDED REVENUE GENERATED OVER THE YEARS (10% INFLATION)   ###
###########################################################################################

# Compute discounted yearly revenue
disc_yearly_revenue = yearly_revenue * discount
print(round(disc_yearly_revenue))
barplot(disc_yearly_revenue)
lines(yearly_revenue)

# Compute discounted cumulated revenue
disc_cumulated_revenue = cumsum(disc_yearly_revenue)
print(round(disc_cumulated_revenue))
barplot(disc_cumulated_revenue)
lines(cumulated_revenue)


###########################################################################################
####              WHAT IS THE COMPANY EVALUATION IN THE YEAR 2025                       ###
###########################################################################################

# What is the database worth?
# ONLY YEAR 2025
print(disc_cumulated_revenue[11] - yearly_revenue[1])


# What is the database worth?
# ALL YEARS
print(disc_cumulated_revenue[1:11] - yearly_revenue[1])
#or
print(disc_cumulated_revenue - yearly_revenue[1])


##########################################
####      CHARTING RESULTS              ###
##########################################

# Create a discount factor
discount = 1 / ((1 + discount_rate) ^ (0:10))  # Adjusted to use 0:10 instead of (1:11) - 1

# Compute discounted yearly revenue
disc_yearly_revenue = yearly_revenue * discount

# Compute discounted cumulated revenue
disc_cumulated_revenue = cumsum(disc_yearly_revenue)

# Compute non-discounted cumulated revenue
non_disc_cumulated_revenue = cumsum(yearly_revenue)

# Plotting
barplot(disc_cumulated_revenue, 
        names.arg = 2015:2025,  # Adjusted to use the desired labels
        main = "Company Evaluation Year-Year",
        xlab = "Year",
        ylab = "Cumulated Revenue with Depreciation")

# Add a line for non-discounted cumulated revenue
lines(non_disc_cumulated_revenue, col = "blue", type = "b")

# Add a legend on the top-left
legend("topleft", 
       legend = c("Company Evaluation W/O Depreciation"),
       col = c("gray", "blue"),
       lty = 1:1,
       pch = 1,
       bty = "n",
       x.intersp = 0.2, # Adjust the distance between legend items
       y.intersp = 0.2  # Adjust the distance between legend and plot
)




