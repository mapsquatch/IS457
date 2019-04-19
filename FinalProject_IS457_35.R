

###########################
# PART 1: Data Processing #
###########################

# Q1
# 1.1: What variables have missing values? What types/forms of missing values are they?

# airbnb <- read.csv("http://more.atorium.net/AirbnbSydney.csv") #broken?
airbnb <- read.csv(paste(getwd(),"/data/AirbnbSydney.csv", sep = ""), stringsAsFactors = FALSE, na.strings = c("N/A",""))
dim(airbnb)
class(airbnb)
summary(airbnb)
missingvals <- sapply(airbnb, function(x) sum(is.na(x)))

# Number of missing values
missingvals[missingvals>0]

# Percent missing values
round(missingvals[missingvals >0]/length(airbnb[,1]), 2)
summary(airbnb[,missingvals>0])

sapply(airbnb[,missingvals>0], function(x) str(x))

# My answer
# Neighborhood_overview and house_rules are both text-heavy (paragraph or more) fields, NA on 6 and 15%.
# Host_response_time and host_response_rate are both absent from 23% of the observations. Response Time is an
# ordinal categorization of how long it takes to respond. Response Rate is a percentage (that needs cleaning if
# it is to be used)>
# City and zipcode are address components useful for aggregation. They are missing from 8 and 21 records (nearly
# zero percent).
# Cleaning_fee is a numerical (after cleaning) and missing from 6% of records.

# 1.2: How will you deal with missing values? Justify your methods.

# 1.3: Describe how your choice method may impact later analysis.

# 1.4: Implement methods to deal with missing values.

# 1.5: After dealing with missing values, show the dimensions of the data.

# 1.6: Comment on and explain any other data cleaning or preparation steps you think would be 
#      necessary from your inspection of the data (you do not have to carry them out).




# Q2
# Conduct a preliminary exploration and describe what you find interesting or unexpected.



# Q3
# Explore comprehensively with charts, tables, and graphs
# 3.1: Think about types of variables; choose appropriate graphs to find distributions and trends

# 3.2: Compare different graph types to see which ones best convey trends, outliers, and patterns

# 3.3: Describe what you find from the graphs



#Q4