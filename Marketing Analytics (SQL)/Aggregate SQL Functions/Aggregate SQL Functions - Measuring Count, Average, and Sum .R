
# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    MODULE 0 - INTRODUCTION
# __________________________________________________________
# //////////////////////////////////////////////////////////


############################################################
####             CLEAR DATA COMMAND                     ####
############################################################

# Set your directory to the folder where you have downloaded the dataset 
# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))



############################################################
####             LOAD DATA COMMAND                      ####
############################################################

# --- EXPLORE THE DATA -------------------------------------

# Load text file into local variable called 'data'
# The line "read.delim(file" allows us to be able to read the text file
# since the Header is "FALSE" this mean our text file didn't come with any header
data = read.delim(file = 'purchases.txt', header = FALSE, sep = '\t', dec = '.')

# Display what has been loaded
head(data)
summary(data)

############################################################
####   CREATING HEADERS TO HEADERLESS DATA              ####
############################################################

# Add headers and interpret the last column as a date, extract year of purchase
colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')

# Let R know to read the 'date_of_purchase' header we just created as date using the format "YYYY-MM-DD"
data$date_of_purchase = as.Date(data$date_of_purchase, "%Y-%m-%d")

###################################################################
####  ADDING ANOTHER HEADER OF JUST YEAR or "YYYY" TO HEADERS  ####
###################################################################

# Now that R know hows to read the date we want to let it know to just read the "date_of_purchase" as "YYYY"
# This make it easier for us to get insight if we wan to get Year vs year comparison over several years
data$year_of_purchase = as.numeric(format(data$date_of_purchase, "%Y"))

############################################################
####         DISPLAYING NEW HEADERS                     ####
############################################################

#We now display the new data with the year being "YYYY" rather than "YYYY-MM-DD"
# Display the data set after transformation
head(data)
summary(data)

####################################################################
####  USING SQL To Manage EXTERNAL & IMPORTED PLATFORM DATA     ###
#####################################################################


# Most time when we extract data, we won't have it as it's supose to be, rather broken in certain data
# Example of broken data, is R not recognizing year dates as "YYYY-MM-DD" until we told it to recognize it
# Explore the data using simple SQL statements
# "(sqldf)" runs SQUL on R uploaded dataframe natively, rather than using API or connector
install.packages("gsubfn")
install.packages("proto")
install.packages("RSQLite")
library(gsubfn)
library(proto)
library(RSQLite)
library(sqldf)

####################################################################
####  USING SELECT COMMAND TO RUN BASIC BAR CHARTS              ###
#####################################################################


# Number of purchases per year
# "COUNT(" means it counts number of observation (rows) in a specific variable (column) or condition
x = sqldf("SELECT year_of_purchase, COUNT(year_of_purchase) AS 'counter' FROM data GROUP BY 1 ORDER BY 1")
barplot(x$counter, names.arg = x$year_of_purchase)

# Average purchase amount per year
# "AVG("  means it get a mean
x = sqldf("SELECT year_of_purchase, AVG(purchase_amount) AS 'avg_amount' FROM data GROUP BY 1 ORDER BY 1")
barplot(x$avg_amount, names.arg = x$year_of_purchase)

# Total purchase amounts per year
# "SUM(" mean it gets totals added regardling of conditions
x = sqldf("SELECT year_of_purchase, SUM(purchase_amount) AS 'sum_amount' FROM data GROUP BY 1 ORDER BY 1")
barplot(x$sum_amount, names.arg = x$year_of_purchase)

# All in one
x = sqldf("SELECT year_of_purchase,
                  COUNT(year_of_purchase) AS 'counter',
                  AVG(purchase_amount) AS 'avg_amount',
                  SUM(purchase_amount) AS 'sum_amount'
           FROM data GROUP BY 1 ORDER BY 1")
print(x)