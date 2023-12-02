
# - - - - - -  Don't Change Any lines - - - - - - - 

# clean up the memory of your current R session
rm(list=ls(all=TRUE))

# lets load our database
data=read.table('DATA_2.03_Telco.csv',header = T,sep=',')

# Now lets have a look at our variables and some summary statistics. 
# The str() function shows the structure of your database and details the type of variable it contains
str(data)

# Now lets more information about our variables like database minimus, mean, maximum and quantities
summary(data)

# Now lets normalize our variables
# To keep our database safe, lets create a copy of it called "testdata"
testdata=data

#To scale function automatically perform data normalization on all your variables
testdata = scale(testdata)

# The dist() function computes the distances of all the observations in our database
d = dist(testdata, method = "euclidean")

# The hclust() function performs hierarchical clustering, we pass it the distances, and we se the reference
hcward = hclust(d, method="ward.D")

#- - - - - -  NOW LETS STRUCTURE THE SAMPLE DATA - - - - - - - - - - - - - 

# assign our points our k=8 clusters
data$groups = cutree(hcward,k=8)

# Aggregation by group and computation of the mean values
aggdata = aggregate(.~ groups, data=data, FUN=mean)

# Computation of the nmber of observations by group
proptemp=aggregate(Calls~ groups, data=data, FUN=length)

# Type promptemp to see the output
proptemp

# Computation of the proportion by group
aggdata$proportion=(proptemp$Calls)/sum(proptemp$Calls)

# Ordering from the largerst group to the smallest 
aggdata=aggdata[order(aggdata$proportion,decreasing=T),]

# To see our output we type aggdata on the command line
aggdata

# To view the taable we type view(aggdata) to the command line
View(aggdata)

# - - - - - - - - LETS PLAY WITH THE DATA - - - - - - 

# Let's try again with creating K=5 segments
data$groups= cutree(hcward,k=5)

# Aggreate by group and computation of the mean values
aggdata= aggregate(.~ groups, data=data, FUN=mean)

# Compute of the number of observations by group
proptemp=aggregate(Calls~ groups, data=data, FUN=length)

# Computatioin of the proportion by group
aggdata$proportion=(proptemp$Calls)/sum(proptemp$Calls)

# Lets order the groups for the larger to the smaller
aggdata=aggdata[order(aggdata$proportion,decreasing=T),]

# Lets see the output by calling our aggdata variable
aggdata

#  - - - - - - - TO SAVE INSTRUCTIONS - - - - - - - - - - - 

# To export the output of our results, we execute the following line using the write.table() function
#  write.csv(aggdata, "HR_example_Numerical_Ouput.csv", row.names=FALSE)
# This allows to import the data in Excel for instance where we can prepare it for the presentation. E.g. change the name
# Instead of write.csv, you can also use write.csv2() if you encounter an erro due to regional setting for seperators

# - - - - -- - - CONTINUE PLAYING WITH THE DATA - - - - - 

# Let's draw the radar chart with the function stars()
palette(rainbow(12, s = 0.6, v = 0.75))

# Select the colors to use
stars(aggdata[,2:(ncol(data))],len = 0.6, key.loc = c(11,6),xlim=c(2,12),main = "Segments", draw.segments = TRUE,nrow = 2, cex = .75,labels=aggdata$groups)


