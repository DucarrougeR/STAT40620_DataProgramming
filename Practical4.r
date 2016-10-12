# Romain D
# Data Programming
# Practical 4

# loading the dataset
# Data from a case-control study of (o)esophageal cancer in Ille-et-Vilaine, France.
data(esoph)

# look at the data
str(esoph)


## QUESTION 2 ##
# rename columns
colnames(esoph) <- c("age", "alcohol", "tobacco","numberCases","numberControls")

# see the column's names change
str(esoph)


## QUESTION 3 ##
# find 3 ways to access the number of cases in 15th record
first <- esoph[15:15,4]
first

second <- esoph$numberCases[15]
second

third <- esoph[15, "numberCases"]
third


## QUESTION 4 ##
# Create a new variable which gives the number of observations in each record
recordTotal <- esoph$numberCases + esoph$numberControls
recordTotal

# get the sum of columns that are numeric
casesSum <- colSums(Filter(is.numeric, esoph))
casesSum
# get the total sum of these columns
totalSum <- sum(casesSum)
totalSum

## QUESTION 5 ##
# Create a new data frame which contains only the number of cases and the number of controls columns. 
# Use sapply to get the mean, standard deviation, and interquartile range
df <- esoph[c("numberCases", "numberControls")]
sapply(df, mean)
sapply(df, sd)
sapply(df, IQR)


## QUESTION 6 ##
# find the mean number of cases in the set of records which have alcohol intake <0-39g and the 
# mean number of cases in the set of records which have alcohol intake 120+.
lowAlcohol <- subset(esoph, alcohol = '0-39g/day', select='numberCases')
sapply(lowAlcohol, mean)

highAlcohol <-subset(esoph, alcohol = '120+', select='numberCases')
sapply(highAlcohol, mean)

## QUESTION 7 ##
irishTimesText <- "Facebook is fighting back against US tax authorities, which the social network 
says are asking for too much information over allegations that the company undervalued assets when 
it transferred them to Ireland. The world's largest social network has accused the IRS of being 
'extraordinarily broad' when it issued summonses to obtain documents that may help it establish if 
Facebook's accountants undervalued the assets by billions of dollars. In documents filed in court 
on Tuesday, Facebook argues the tax authority is responsible for delays in the process, having 
frequently asked for extensions on its audit, being inconsistent about when it works and uncoordinated 
internally."

findwords = function(tf) {
  #NORMALIZING TEXT
  gsub("[[:punct:]]", "", tf) #remove punctuation from text
  gsub("\\d", "", tf)         #remove numbers from text
  tolower(tf)                 # text to lowercase
  gsub('\n', '', tf)          #remove newline characters
  
  # Read in the words from the text and separate into a vector
  txt = unlist(strsplit(tf,' '))
  # Create a list to store the words and their positions
  wl = list()
  # Loop through each word
  for(i in 1:length(txt)) {
    # Get the current word
    wrd = txt[i]
    # Add its position to the list with the appropriate tag
    wl[[wrd]] = c(wl[[wrd]],i)
  }
  # Return the answer as a list
  return(wl)
}
findwords(irishTimesText)


