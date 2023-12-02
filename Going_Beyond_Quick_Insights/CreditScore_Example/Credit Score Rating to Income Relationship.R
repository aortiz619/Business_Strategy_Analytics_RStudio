

# - - - - - -  Don't Change Any lines - - - - - - - 

# clean up the memory of your current R session
rm(list=ls(all=TRUE))

# lets load our database
data=read.table('DATA_3.01_CREDIT.csv',sep=',',header=TRUE)

# If you need to know how to do formulas and arguments
?read.table

# Now lets have a look at our variables and some summary statistics. 
# The str() function shows the structure of your database and details the type of variable it contains
str(data)

# Now lets more information about our variables like database minimus, mean, maximum and quantities
summary(data)


#- - - - - -  NOW LETS STRUCTURE THE SAMPLE DATA - - - - - - - - - - - - - 

# Produce a histogram of the credit scores with the numerical value you want to explore in this case "Rating" for "hist(data$value)"
hist(data$Rating)

# Compute the correlation between all the numerical variables of the sample that exclude categorical variables (gender,student, married, ethnicity)
cor(data[,c(1:5,10)])

 # Estimate a linear regression model of Rating as a function of everything else.
linreg=lm(Rating~.,data=data)

 # Computes the correlation coenficient between the fitted values and the actual ones
cor(linreg$fitted.values,data$Rating)

 # Plot the fitted values vs. the actual ones
plot(data$Rating,linreg$fitted.values)

# Reports the results of the regression. This with the star system tells us the sicnificant variables where T is largest
# In the Estimate section, a negative coeficient equals a negative impact and positive equals positive, meaning if credit score goes up or down
summary(linreg)

# Allows to visualize the relationship between Balance and Rating
# Use if we wanted to show our technical findings to Business leaders
# We can see that the higher the balance the higher the rating
plot(data$Balance,data$Rating)

# Allows to visualize the relationship between Income and Rating
#Use if we wanted to show our technical finding to Business leaders
#We can see that the higher the income the higher the rating
plot(data$Income,data$Rating)





