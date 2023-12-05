# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#                  MANAGERIAL SEGMENTATION
# __________________________________________________________
# //////////////////////////////////////////////////////////

# 

############################################################
####             CLEAR DATA COMMAND                     ####
############################################################

# Set your directory to the folder where you have downloaded the dataset 
# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))


############################################################
####                    LOAD DATA                      ####
############################################################


# --- COMPUTING RECENCY, FREQUENCY, MONETARY VALUE ---------


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
####  COMPUTE RECENCY, FREQUENCY, AND MONETARY/AMOUNT     ####
############################################################

# Compute recency, frequency, and average purchase 
# We will start by letting the system to use customer ID as our dependent variable
# We will compute "Recncy" by telling it to get us the minimum amount of days visited
# We will compute "First_purchase" by telling it to get us the max or latest day since purchase
# We will compute "Frequency" by telling it to count the times of "visit times"
customers_2015 = sqldf("SELECT customer_id,
                               MIN(days_since) AS 'recency',
                               MAX(days_since) AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'amount'
                        FROM data GROUP BY 1")

############################################################
####             EXPLORE HISTOGRAM DATA                 ####
############################################################

# Explore the data
head(customers_2015)
summary(customers_2015)
hist(customers_2015$recency)
hist(customers_2015$frequency)
hist(customers_2015$amount)
hist(customers_2015$amount, breaks = 100)

# __________________________________________________________
# //////////////////////////////////////////////////////////
#
  #      --- GETTING MANAGERIAL SEGMENTATION (2015 DATASET) ----------------
# __________________________________________________________
# //////////////////////////////////////////////////////////

#############################################################################
####    CALCULATING UNDER 3 YEAR ACTIVES/INACTIVES FOR SINGLE SEGMENT        ###
###############################################################################


# Simple 2-segment solution based on recency alone
# We will use the data we created called "customer_2015" that we created after identifying our headers fron our original dataset "data"
# Here we want to know any customer whose "recency" or hasn't visited a store in over 3 years  
# N/A = under 3 years and Inactive = over 3 years
customers_2015$segment = ifelse(test = customers_2015$recency > 365*3, yes = "inactive", no = "NA")

# The table will tell use how many customer visited 3 years ago and how many visited under 3 years
table(customers_2015$segment)

# Now we can compute averages again, with relevant information from our "customer_2015" dataset
# However, since we want to ensure we can cluster (group) in the future we always want to work with means "averages"
# We will use a aggregate to calculate mean, and looking at the 2nd to 5th rows "recency, first_purchase_, amounts" headers
aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)

##################################################################################
####    CALCULATING UNDER 2 YEAR ACTIVES/INACTIVES (FOR ONLY FOR 1 SEGMENT)     ###
##################################################################################

# IF functions are good to calculate 1 segment "group" but not ideal for multiple
# If we added 9 segment for example, we would need to create 9 "of these statement, for each segment "group"
customers_2015$segment = ifelse(test = customers_2015$recency > 365*3,
                           yes = "inactive",
                           no = ifelse(test = customers_2015$recency > 365*2,
                                       yes = "cold",
                                       no = "NA"))
table(customers_2015$segment)
aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)

##################################################################################
####    CALCULATING UNDER 3 YEAR ACTIVES/INACTIVES (FOR MULTIPLE SEGMENTS)     ###
##################################################################################

#  2-segment solution using the which statement
# wHICH functions are good to calculate multipe segments "groups" 
# If we added 9 segment for example, we wouldn't need to create 9 "of these statements, for each segment "group"
# The reason being the "WHICH" statement tells the system to automatically calculate each segment "group"
customers_2015$segment = "NA"
customers_2015$segment[which(customers_2015$recency > 365*3)] = "inactive"
table(customers_2015$segment)
aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)

##################################################################################
####    CALCULATING MULTIPLE YEAR ACTIVES/INACTIVES (FOR MULTIPLE SEGMENTS)     ###
##################################################################################

# 4-segment solution using which statement
# Here we add more lines to the example previous so each with a different year "3" or "2" or "1" 
# We tell the system to automatically calculate and label every group that lands under each year specified
# We should create 1 label so that each so that it qualifies each customer to only 1 segment
customers_2015$segment = "NA"
customers_2015$segment[which(customers_2015$recency > 365*3)] = "inactive"
customers_2015$segment[which(customers_2015$recency <= 365*3 & customers_2015$recency > 365*2)] = "cold"
customers_2015$segment[which(customers_2015$recency <= 365*2 & customers_2015$recency > 365*1)] = "warm"
customers_2015$segment[which(customers_2015$recency <= 365)] = "active"
table(customers_2015$segment)
aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)

########################################################################################
####    CALCULATING MULTIPLE YEAR ACTIVE/INACTIVES (DEFINING AND ASSIGNING GROUPS)   ###
########################################################################################

# Complete segment solution using which, and exploiting previous test as input
# Here we will group customer into different segment "groups" and identify them as much as we want
# JUST DON'T EVER CROSS SEGMENTS or data will be label to a different group and mistakes
# This remove the "ifelse" statement long list/errors
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
table(customers_2015$segment)
aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)

########################################################################################
####           CALCULATING MULTIPLE GROUPS CREATED (HIGH TO LOW)                    ###
########################################################################################

# Re-order factor in a way that makes sense
customers_2015$segment = factor(x = customers_2015$segment, levels = c("inactive", "cold",
                                                             "warm high value", "warm low value", "new warm",
                                                              "active high value", "active low value", "new active"))
# Lets display the information
table(customers_2015$segment)

# Now we can compute averages again, with relevant information from our "customer_2015" dataset
# However, since we want to ensure we can cluster (group) in the future we always want to work with means "averages"
# We will use a aggregate to calculate mean, and looking at the 2nd to 5th rows "recency, first_purchase_, amounts" headers
aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)


# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#   REMOVING 2015 YEAR - CREATING A MANAGERIAL SEGMENTATION (YEAR 2014 DATASET)
# __________________________________________________________
# //////////////////////////////////////////////////////////


########################################################################################
####           MAKING OUR 2015 DATA INTO ONLY 2014 DATA (REMOVING A YEAR)           ###
########################################################################################

# --- SEGMENTING A DATABASE RETROSPECTIVELY ----------------


# Compute key marketing indicators using SQL language
library(sqldf)

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
                               AVG(purchase_amount) AS 'amount'
                        FROM data
                        WHERE days_since > 365
                        GROUP BY 1")
########################################################################################
####  CALCULATING MULTIPLE GROUPS FOR 2014 (DEFINIG AND ASSIGNING GROUPS)           ####
########################################################################################

# Complete segment solution using which, and exploiting previous test as input
# Here we will group customer into different segment "groups" and identify them as much as we want
# JUST DON'T EVER CROSS SEGMENTS or data will be label to a different group and mistakes
# This remove the "ifelse" statement long list/errors
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

########################################################################################
####          STRUCTURING 2014 GROUPS So THEY BELONG TO LABELS                        ###
########################################################################################

# Re-order factor in a way that makes sense
# This will reorder the values so the result are shown under the headers, for exmaple "cold"
customers_2014$segment = factor(x = customers_2014$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", "new active"))
# Show segmentation results
table(customers_2014$segment)

########################################################################################
####           DISPLAYING 2014 MULTIPLE GROUPS CREATED IN A PIE CHART              ###
########################################################################################

# We will not display it in a pie chart
pie(table(customers_2014$segment), col = rainbow(24))

OR

# Load the necessary libraries
library(ggplot2)
# Assuming 'customers_2014' is your data frame
# Create a ggplot pie chart with a legend and darker shades
ggplot(data = customers_2014, aes(x = "", fill = segment)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar("y", start = 0) +
  scale_fill_manual(
    values = hsv(seq(0, 1, length = length(unique(customers_2014$segment))), 
                 0.7, 0.7)
  ) +
  theme_void() +
  labs(fill = "Segment in 2014") +
  ggtitle("Distribution of Segments in 2014")


########################################################################################
####           CALCULATING 2014 MULTIPLE GROUPS CREATED (HIGH TO LOW)                ###
########################################################################################


# Now we can compute averages again, with relevant information from our "customer_2015" dataset
# However, since we want to ensure we can cluster (group) in the future we always want to work with means "averages"
# We will use a aggregate to calculate mean, and looking at the 2nd to 5th rows "recency, first_purchase_, amounts" headers
aggregate(x = customers_2014[, 2:5], by = list(customers_2014$segment), mean)



# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#      ---  REVENUE GENERATED BY SEGMENTS (2015 DATASET) ----------------
# __________________________________________________________
# //////////////////////////////////////////////////////////


########################################################################################
####            FINDING PURCHASES MADE BY SEGMENT ONLY IN 2015                      ###
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

# Let view the result
summary(revenue_2015)

########################################################################################
####       CALCULATING REVENUE BASE ON  PURCHASES MADE BY SEGMENTS ONLY IN 2015    ###
########################################################################################

# DO NOT USE TO CALCULATE, HERE'S WHY, but you can run the calculation
# This is an example of what formula not to use because it excludes customers
# It excludes customer who haven't made purchases but may have revenue
# Merge 2015 customers and 2015 revenue (the wrong way)
actual = merge(customers_2015, revenue_2015)

# USE THIS CALCULATION, HERE'S WHY, but you can run the calculation and compare
# This tells the system that all observations (row) results or (all.x = TRUE) from variable (column) customers_2015 should fill all observations (row) from variable (column) "revenue_2015"  
# Merge 2015 customers and 2015 revenue (correct)
# But if there is any blank observations (rows) we have to tell the system to replace it with "0" using [actual$revenue_2015[is.na(actual$revenue_2015)] = 0]
actual = merge(customers_2015, revenue_2015, all.x = TRUE)
actual$revenue_2015[is.na(actual$revenue_2015)] = 0

########################################################################################
####              CALCULATING SEGMENT REVENUE MEAN FOR PURCHASES ONLY IN 2015        ###
########################################################################################

# Show average revenue per customer and per segment
aggregate(x = actual$revenue_2015, by = list(customers_2015$segment), mean)

########################################################################################
####    MERGING SEGMENT 2014 CUSTOMERS WITH 2015 REVENUES TO FORECAST SALES           ###
########################################################################################

# This will create a new dataset called "forward" which predict sales
# Merge 2014 customers and 2015 revenue (correct)
forward = merge(customers_2014, revenue_2015, all.x = TRUE)
forward$revenue_2015[is.na(forward$revenue_2015)] = 0

# R will be the table were we can see these results
# Show average revenue per customer and per segment
# This can tell us for the example that when looking at average single purchase warm high values of "$114" is worth paying more effort than a new active client who only brings in "$31"
r = aggregate(x = forward$revenue_2015, by = list(customers_2014$segment), mean)
print(r)

#################################################################################################
####    Reorganizing SEGMENT 2014 CUSTOMERS WITH 2015 REVENUES TO FORECAST SALES (HIGH-TO-LOW)  ###
#################################################################################################

# Re-order and display results
r = r[order(r$x, decreasing = TRUE), ]
print(r)

#################################################################################################
####              PLOTTING 2014 CUSTOMER SEGMENTS AND REVENUE EXPECTED IN 2015                 ###
#################################################################################################

# Here we can see how the 2014 customer segments and what their expected 2015 revenue are
# This graph says that taht x-axis = 1 customer and y=axis equals revenue per that customer
# Here it is important for us to understand that losing 1 high value active high value customer, is like losing 1 active low value customer. 
# This is important an valuable to know
barplot(r$x, names.arg = r$Group.1)

OR

# Load the necessary libraries
library(ggplot2)
library(dplyr)
# Assuming 'r' is your data frame
# Reorganize 'Group.1' based on the descending values of 'x'
r$Group.1 <- reorder(r$Group.1, desc(r$x))
# Create the ggplot with reordered bars and grey fill
ggplot(data=r, aes(x=Group.1, y=x)) +   
  geom_bar(colour="black", stat="identity", fill="grey") + 
  theme(axis.text.x=element_text(angle=20,hjust=0.5,vjust=0.5)) +
  labs(x="Segment in 2014", y="Revenue in 2015") +
  ggtitle("Revenue in 2015 as a function of Segment in 2014")



# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#      ---  EXPLORING THE DATA FURTHER ----------------
# __________________________________________________________
# //////////////////////////////////////////////////////////

#################################################################################################
####              CALCULATING HOW MANY "NEW ACTIVE LOW" CUSTOMER WHERE IN 2015               ###
#################################################################################################

# Lets calculate how many customer in 2015 belong to the "new active low"
table(customers_2015$segment)


#################################################################################################
####              FINDING GROWTH OF "NEW ACTIVE HIGH" CUSTOMERS FROM 2014 TO 2015              ###
#################################################################################################

# Now lets calculate how many customers has the "new active high" increased from 2014 to 2015
table(customers_2014$segment)
table(customers_2015$segment)



#################################################################################################
####              FINDING 2015 REVENUE FROM 2014 NEW WARM GROUP                              ###
#################################################################################################

# Let explore what is the expected revenue of all 2014 "new warm" segment in 2015
aggregate(x = forward$revenue_2015, by = list(customers_2014$segment), mean)

#################################################################################################
####              CALCULATING AVERAGE PURCHASE OF CUSTOMER IN "NEW ACTIVE HIGH" SEGMENT      ###
#################################################################################################

aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)


