###########################################################
####        EXAMPLE N°4 - SEASONAL SALES OF CHOCOLATES ####
###########################################################

# In this exmaple we will talk about forecasting chocolate sales 

############################################################
####             CLEAR DATA COMMAND                     ####
############################################################

# Set your directory to the folder where you have downloaded the Chocolate dataset 
# to clean up the memory of your current R session run the following line
rm(list=ls())

############################################################
####             LOAD DATA COMMAND                       ####
############################################################

# Let's load our dataset and call it data
# The function read.table enables us to read flat files such as .csv files
data=read.table('DATA_4.04_CHOC.csv',sep=',',header=TRUE) 

############################################################
####    SUMMARIZED/vIEWing Data VARIABLE COMMAND        ####
############################################################

# Now let's have a look at our variables and see some summary statistics
# The str() function shows the structure of your dataset and details the type of variables that it contains
# Here we see the following insights, 
# (base insight) we have "120 variables/rows"
# (Insight 1) our time goes from 1 to 120 which means we have have 10 years of data (or 120 months)
# (Insight 2) sales variable tells us the company sales in thousands of units
# (Inishgt 3) year variable tells us the year time we are looking a the variable on
# (Insight 4) month tells us the month we are looking at the variable on
str(data) 

# The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles
# Here should examine each variable or column (Min, Medium, and Max)
# However, in this scenario the mean "average" doesn't really tell us much, especially on variables like time, year, month
summary(data)

############################################################
#### sUMMARIZED/vIEWing "Specific" VARIABLE COMMAND     ####
############################################################

# The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles
# We already saw this information using the "str(data)" code but it's nice to know how to do it at individual levels
summary(data$sales) 


############################################################
####       Creating a Sales and Time Plot Graph        ####
############################################################

# Here is how to plot it
# Time goes first, since it's on the horizonal axis
# Sales goes second, since it's on the vertical axis
# Here we see up and downs, which can be said chocolate is seasonal, meaning desirable in specific months.
plot(data$time,data$sales,main="Chocolate sales over time",xlab="Time (in month)",ylab="Monthly sales",ylim=c(0,max(data$sales*1.2)),type='l')

###############################################################
####  Building a Sales and Time Regression Module Chart      ####
###############################################################

# Build a linear regression model
# Here the sales are the dependent variable, meaning it changes base on everything
# Month will be the independent variable, meaning everything around it changes because of it
regres=lm(sales~month,data=data) 

##################################################################
####  Visualizing a Sales and Time Regression Module Chart      ####
###################################################################

# Let view it now
# Here we get in detail insight from the chart, that the graph couldn't tell us
# If we look at the "Signif. codes" section, here we see the star system here relies on the "p" value where "0.5 or 1 star" mean change
# anything below ".05" according to "Signif. codes" section means that importance increases. Example "3 star months have  a p value of 0"
# So anything closer to "0" p value is extremely of importance here or significant
# (Insight 1) looking at the stars on each result we see that statically, the most sales happen in those time with 3 stars being more important than 2
# (Insight 2) Most important months that generate most sales are (February, August, November, and December)
# (Insight 3) Second most important month that generate most sales are (June, July, and September)
summary(regres)

########################################################################################
####       Creating Distribution Of Chocolate Sales Per Month with BoxPlot        ####
#######################################################################################

# Plot Boxplot using base R
# Here we can see the distribution of all sales in January (regardless of year)
# Meaning we can visualize which months at any year will produce more sales (since they are compress to show trend)
plot(data$month,data$sales,main="Chocolate sales by month",xlab="Month",ylab="Monthly sales",ylim=c(0,max(data$sales*1.2)))

# or use the following formula

# Create a boxplot of 'sales' for each 'month' with increased spacing
boxplot(data$sales ~ data$month,
        main = "Chocolate Sales by Month",
        xlab = "Month",
        ylab = "Monthly Sales",
        col = "white",
        notch = FALSE,
        ylim = c(0, max(data$sales) * 1.2),
        at = 1:12 + 0.5)  # Increase spacing by adjusting the at parameter

# Add a horizontal line at the mean of 'sales'
# This represent the mean of the sales variable in our dataset but its not that important when looking at months
abline(h = mean(data$sales), col = "red", lty = 2)

######################################################################################
####     Revisit and Re-loading Our "Sales and Time  Plot Graph"                  ####
#######################################################################################

# Recovery thanks to the model:
# To view the plot graph we had create earlier, we just copy and paste the same line
plot(data$time,data$sales,main="Chocolate sales over time",xlab="Time (in month)",ylab="Monthly sales",ylim=c(0,max(data$sales)*1.2),type='l')

######################################################################################
####             Adding Fitted/Forecast Plot Graph Model to our Plot Graph"       ####
#######################################################################################

# The line function helps us visualize the sales module "aka forecast sales" against actual sales
lines(data$time,regres$fitted.values,type='l',col='blue',lty=2)

######################################################################################
####       Adding Legend to Regression Plot Graph Model to BoxPlot Graph"          ####
#######################################################################################

#Let add a legend
legend("topleft",c("Actual sales","Sales by the model"),lty=c(1,2),col=c('black','blue'))



