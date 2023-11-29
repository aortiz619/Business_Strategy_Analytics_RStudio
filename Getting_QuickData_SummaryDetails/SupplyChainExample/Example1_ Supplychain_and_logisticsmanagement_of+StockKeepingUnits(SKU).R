  
# clean up the memory of your current R session
rm(list=ls(all=TRUE))  

# lets load our database
data <- read.csv(file = 'DATA_2.01_SKU.csv')

# Now lets have a look at our variables and some summary statistics
str(data)

# Now lets more information about our variables like database minimus, mean, maximum and quantities
summary(data)

# Now lets plot our data to see if we can identify groups visually
plot(data$CV, data$ADS, main = "SKU Example", ylab="Average Daily Sales", xlab= "Coefficient of Variation")

# Lets draw a vertical line by using the abline function and possing it the v argument
abline(v=0.2, col = "red")

# Lets draw a vertical line by using the abline function and possing it to h argument 
abline (h=4, col="red")

# We can add 1 text to our plot by using the text () fucntion, here to label group "Horse" 
text(0.15,9.7, "Horses", col = "red")

# We can add 2 text to our plot by using the text () function, here to label group "Wild Bulls"
text(0.65,9, "Wild Bulls", col = "red")

# We can add 3 text to our plot by using the text () function, here to label group "Crickets"
text(0.8,2, "Crickets", col = "red")



# Lets find groups using hierarchical clustering and check if we obtain similar results
# To keep our database safe, let's create a copy of it called "testdata"
testdata=data

# Now to keep our dataset safe, let's create a copy of it called "testdata"
testdata = scale(testdata)

# the dist () fucntion computes the distances of all the observations in our dataset
d = dist(testdata, method = "euclidean")

# hclust() function performs hierarchical clustering, we pass it the distance, and we set the 
hcward = hclust(d, method="ward.D")

# assign our points to our k=3 clusters
data$groups<-cutree(hcward,k=3)

# The lattice library provides a complete set of functions for producing advance plots.
# install the lattice package by using the install.package() function
install.packages("lattice")

# load the lattice package by using the libary() function and passing it the name of the package you wish to look
library(lattice)

xyplot(ADS~ CV,main = "After Clustering", type="p",group=groups,data=data,
# define the groups to be differentiated
auto.key=list(title="Group", space = "left", cex=1.0, just = 0.95),
# to produce the legend we use to auto.key list ()
par.settings = list(superpose.line=list(pch = 0.18, cex=1)),
# the par.settings argument allow us to pass a list of display
col=c('blue','green','red'))
# finally we choose the color of our plotted points per group


