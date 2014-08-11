# The Practical Programmer: Introduction to Data Mining
# Author: Brandon Bass

# Introduction to variables & data storage ####
myStringVariable<-"This is a string variable"
myNumericVariable<-9 # This is a numeric variable
myVectorVariable<-c(myNumericVariable, 10, 11) # This is a vector variable created by concatenating myNumericVariable, and one being the value 9.  
myStringVectorVariable<- c("a", "b", "c", "d")
# Why does this matter?
myNumbers<-c("1", "2", "3")
# myNumbers + 3 will cause an error
as.numeric(myNumbers)+3

myMatrixVariable<-cbind(myVectorVariable, c(1,2,3)) # This is a matrix concatenating the column vector myVectorVariable with a new column vector.  Note: the columns must have the same number of rows or else an error will arise.

colnames(myMatrixVariable)<-c("column_1", "column_2") # Sets the column names of myMatrixVariable to column_1 and column_2

firstCol<-myMatrixVariable[,"column_1"]

myMatrixVariable[1,2]<-8 # This will take the value in the first row, second column, and assign the value 8 to it.  
myMatrixVariable[,1]<-5 # The comma indicates "ALL", so this will take all of the rows, and the first column, and reassign their value to 5.

myMatrixVariable<-myMatrixVariable[-2:-3,] # This removes the 2nd and 3rd rows from the variable and assigns the new data to the existing variable name.  This uses the sequence generator operator :, e.g. 1:3 = 1,2,3.  

# Data Import  & Manipulation ####
# NOTE: R works best and fastest with CSV files.  
# To import a csv, use the command read.csv("filepath").  More information on specific imports can be found in google.  

# Note to install a library, need to run install.packages("packageName") in command line, and then run library("packageName") in code.


library(xlsx)
# Can check working directory with getwd()
# Set filepath variable
filePath<-"documents/realLife/ThePracticalProgrammer/"
fileName<-"OlympicAthletes_0.xlsx"
# Import Data
myRawData<-read.xlsx2(paste(filePath,fileName, sep=""), sheetIndex=1) # The 1 is sheet index, could have replaced with the name of the worksheet in the xlsx file.  

# NOTE: I have found better performance with read.xlsx2, as opposed to read.xlsx 

View(myRawData)

# Let's not be concerned about closing ceremony date since we have the year.  We'll remove that column. Also want to turn into matrix because R is excellent at matrix calculations. We will fix the total medals column though.

# Remove 5th column. Set new myData variable to output
myData<-myRawData[,-5]
# Could also use subset function

myData[,6:8]<-sapply(myData[,6:8], as.numeric)

# Let's fix the Total.Medals column to be the sum of the Gold.Medals, Silver.Medals, and Bronze.Medals columns
myData[,"Total.Medals"]<-rowSums(myData[,6:8])


meanGoldMedals<-mean(myData[,"Gold.Medals"])
standardDevGoldMedals<-sd(myData[,"Gold.Medals"])

# Now let's use logical indexing to subset the data and see only athletes who won (above 2 standard deviations from the mean) gold medals
goldAthleteData<-myData[myData[,"Gold.Medals"]>meanGoldMedals + 2.5*standardDevGoldMedals,]

# See how many athletes won > 3.5 std above mean # of gold medalscompared to how many didn't
# NOTE: sometimes you need to play around with nrow vs NROW, and ncol vs NCOL.  I don't know why :( !
numAthletes<-NROW(unique(myData[,"Athlete"]))
numGoldAthletes<-NROW(unique(goldAthleteData[,"Athlete"]))

# Basic For Loop
# How many total medals has each country gotten?
# Get a list of all of the unique countries
countryVector<-as.matrix(unique(myData[,"Country"]))

# For each country, sum up total medals.  Store the sum in a new vector called countryMedals. Note: sometimes you need to play with NROW vs nrow
haystack<-myData[,"Country"]
countryMedalsVector<-NULL
for(i in 1:NROW(countryVector)){
  needle<-countryVector[i]
  # Find the indices in haystack which are equal to needle
  countryIdx<-which(haystack %in% needle)
  #Sum up those entries and store the value in countryMedals. The order of countryMedals corresponds to the order of countryVector
  countryMedalsVector[i]<-sum(as.numeric(myData[countryIdx,"Total.Medals"]))
  
}

# Concatenate countryVector and countryMedals into a dataframe
countryMedalsDf<-data.frame(Country=countryVector, Total.Medals = countryMedalsVector)

# Order by Total.Medals. Note for data frames, you can access columns with the $ operator as seen below. 
countryMedalsDf<-countryMedalsDf[order(-countryMedalsDf$Total.Medals),]

# Data Visualization ####
# Generally how many medals are won? Let's plot a histogram of the total number of medals won
# Breaks define the boundary that you want.  
hist(myData[,"Total.Medals"], breaks=0:10)
hist(myData[,"Gold.Medals"], breaks=0:10)

# What are the top countries in terms of total number of medals won?
barplot(countryMedalsDf$Total.Medals[1:30], names.arg=countryMedalsDf$Country[1:30], las=2)
