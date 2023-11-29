
# clean up the memory of your current R session
rm(list=ls(all=TRUE))

# lets load our database
data=read.table('DATA_2.02_HR.csv',header = T,sep=',')

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

# assign our points our k=4 clusters
data$groups = cutree(hcward,k=4)

# The aggregate () function presents a summary of a statistic, broken down by groups
aggdata = aggregate(.~ groups, data=data, FUN=mean)

# One thing we would like to have is the proportion of our data that is in each cluster
proptemp=aggregate(S~ groups, data=data, FUN=length)

# Type promptem to see the output
proptem

#we create a varaible called promptemp whcih computes the number of observation
aggdata$proportion=(proptemp$S)/sum(proptemp$S)

# Proportion of observations in each group we compute the ratio between proptem and 
aggdata=aggdata[order(aggdata$proportion,decreasing=T),]

# To see our output we type aggdata on the command line
aggdata

# To view the taable we type view(aggdata) to the command line
View(aggdata)

# As discussed, let's remove the Newborn variable, which is no really relevant and by being a dummy drives the cluster
# To create a new data, we create one by naming it "testdata" which includes all the rows and the 5 first columns of the original dataset
testdata=data[,1:5]

# We then return the code used above
# We normalize again our orginal variables
testdata = scale(testdata)

# We compute the distances between observations
d = dist(testdata, method = "euclidean")

# Hierachical Clustering using Ward criterion
hcward = hclust(d, method ="ward.D")

# Create segment for k=4
# Note that we re-use the original dataset "data" (where the variable Newborn is still present) and not "testdata" (where the variable is not present
# Hence we'll be able to produce a summary statistics also for the Newborn variable regardless if it wasn't included
data$groups = cutree(hcward, k=4)

# Aggreate the values again
aggdata = aggregate(.~groups, data=data, FUN=mean)

# Compute the number of observations per group
proptemp=aggregate(S~ groups, data=data, FUN=length)
# Compute the proportion 
aggdata$proportion=(proptemp$S)/sum(proptemp$S)
# Lets order the groups for the larger to the smaller
aggdata=aggdata[order(aggdata$proportion,decreasing=T),]

# Lets see the output by calling our aggdata variable
aggdata

# To export the output of our results, we execute the following line using the write.table() function
#  write.csv(aggdata, "HR_example_Numerical_Ouput.csv", row.names=FALSE)
# This allows to import the data in Excel for instance where we can prepare it for the presentation. E.g. change the name
# Instead of write.csv, you can also use write.csv2() if you encounter an erro due to regional setting for seperators







