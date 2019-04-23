# Load libraries
require(ggplot2)
#require(RColorBrewer)

###########################
# PART 1: Data Processing #
###########################

# Q1
# 1.1: What variables have missing values? What types/forms of missing values are they?

# Initial read of the data:
# airbnb <- read.csv(paste(getwd(),"/data/AirbnbSydney.csv", sep = ""), stringsAsFactors = FALSE)

# Read in airbnb data -- accounting for NA values
airbnb <- read.csv(paste(getwd(),"/data/AirbnbSydney.csv", sep = ""), stringsAsFactors = FALSE, na.strings = c("N/A","", "NA"))


# Number of missing values
missingvals <- sapply(airbnb, function(x) sum(is.na(x)))
missingvals[missingvals>0]

# Percent missing values
round(missingvals[missingvals >0]/length(airbnb[,1]), 2)
summary(airbnb[,missingvals>0])

#sapply(airbnb[,missingvals>0], function(x) str(x))

# Clean the prices (drop $ and comma, as.numeric)
airbnb$price <- as.numeric(gsub("^\\$|,","",airbnb$price))
airbnb$cleaning_fee <- as.numeric(gsub("^\\$|,","",airbnb$cleaning_fee))
airbnb$extra_people <- as.numeric(gsub("^\\$|,","",airbnb$extra_people))

# Clean percents
airbnb$host_response_rate <- as.numeric(gsub("%$","", airbnb$host_response_rate))

# Store host_since as a date
airbnb$host_since <- as.Date(airbnb$host_since, format = "%m/%d/%y") #  add column of day units for comparing ages
# Add a column -- number of days as host as of max(date)
airbnb$host_number_of_days <- difftime(max(airbnb$host_since), airbnb$host_since, units = "days")

# Store logicals as logical. R requires a capial T or F
airbnb$host_is_superhost <- as.logical(toupper(airbnb$host_is_superhost))
airbnb$host_identity_verified <- as.logical(toupper(airbnb$host_identity_verified))



# My answer
#* Neighborhood_overview and house_rules are both text-heavy (paragraph or more) fields, NA on 6 and 15%. NA values
# are shown as empty strings: ""
#* Host_response_time and host_response_rate are both absent from 23% of the observations. Response Time is an
# ordinal categorization of how long it takes to respond. Response Rate is a percentage that is imported as
# character, but will be converted to numeric. NA values are shown as valid character element "N/A".
# City and zipcode are address components useful for aggregation. They are missing from 8 and 21 records (nearly
# zero percent) of these character vectors.
#* Bathrooms is a numeric with decimals (due to half bath, .5). NA values are characters "NA".
#* Bedrooms is an integer numeric. NA values are characters "NA".
# Cleaning_fee is a numerical (after cleaning) and missing from 6% of records.
#* Review_scores_rating and review_scores_xxx are missing one observation in each column. These are integer values.
# NA values are characters "NA".


# 1.2: How will you deal with missing values? Justify your methods.

#Distribution of response_time
table(as.factor(airbnb$host_response_time))
# Maybe these are tied to communication rating?
by(as.numeric(airbnb$review_scores_communication), as.factor(airbnb$host_response_time), mean)



# 1.3: Describe how your choice method may impact later analysis.
# The reduced n values 

# 1.4: Implement methods to deal with missing values.
# NA neighborhood_overview and house_rules
# Do nothing, because these are descriptive text values and there is no way to impute them.

getmode <- function(v){
  # funcname: getmode
  # inputs  : a vector of elements
  # outputs : a single-value vector containing the most-frequently occuring value
  # purpose : Calculate the mode
  # related : mean(), median()
  # auth/dt : ID35, 
  
  uniqv <- unique(v)
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}

# NA host_response_time
# To preserve the integrity of these categorical data for comparative purposes,
# I will create a new category for NA values called "unknown"
airbnb$host_response_time[is.na(airbnb$host_response_time)] <- "unknown"

# NA host_response_rate
airbnb$host_response_rate[is.na(airbnb$host_response_rate)] <- median(airbnb$host_response_rate, na.rm = TRUE)

# NA city
airbnb$city[is.na(airbnb$city)] <- "unknown"

# NA zipcode
airbnb$zipcode[is.na(airbnb$zipcode)] <- "unknown"

# NA bathrooms
airbnb$bathrooms[is.na(airbnb$bathrooms)] <- getmode(airbnb$bathrooms)

# NA bedrooms
# Description calls this one a studio (no bedroom)
airbnb$bedrooms[is.na(airbnb$bedrooms)] <- 0

# NA cleaning fee
# The mode is 50
getmode(airbnb$cleaning_fee)
# The distribution is positively skewed. This explains why the mean (94.4) is greater than the median (80)
hist(airbnb$cleaning_fee, breaks = 1000)
# To clean this, I will use median. It lies between the mode and the mean
airbnb$cleaning_fee[is.na(airbnb$cleaning_fee)] <- median(airbnb$cleaning_fee, na.rm = TRUE)


# NA review scores
# The review scores (review_scores_rating, review_scores_accuracy, 
# review_scores_cleanliness, review_scores_checkin, and review_scores_communication)
# are each missing one value. I will use median as the measure of centrality
# to fill in the missing values.

# These columns are being handled in the same way, and are next to each other.
# I will use a for loop to go through each column, and assign the median value
# of that column to any NA values.
for(i in 28:34){
  airbnb[is.na(airbnb[,i]),i] <- median(airbnb[,i], na.rm = TRUE)
}


# 1.5: After dealing with missing values, show the dimensions of the data.
dim(airbnb)

# 1.6: Comment on and explain any other data cleaning or preparation steps you think would be 
#      necessary from your inspection of the data (you do not have to carry them out).

# host_since will need to be converted to date.
# host_response_time should become a factor (to compare levels)
# host_response_rate need the % sign scrubbed and converted to numeric (maybe even percentage, e.g. /100)
# price and cleaning_fee need $ scrubbed and converted to numeric



# Q2
# Conduct a preliminary exploration and describe what you find interesting or unexpected.
counts <- lapply(airbnb[,c(7,11,13:20,35)], function(x) table(x))
summaries <- lapply(airbnb[,c(6,8,22:34,36)], function(x) summary(x))

# Property types
barplot(counts$property_type)



# Findings
# With a 1st quartile of 100%, over 75% of listings have a 100% response rate

# Q3
# Explore comprehensively with charts, tables, and graphs
# 3.1: Think about types of variables; choose appropriate graphs to find distributions and trends

# 3.2: Compare different graph types to see which ones best convey trends, outliers, and patterns

# What is the distribution of the different review scores?
boxplot(airbnb[29:34], las=1, main = "Distribution of Scores")

# 3.3: Describe what you find from the graphs



# Q4
# 4.1: Compare and contrast review_per_month and number_of_reviews

# A simple scatterplot; garbage-y
#plot(airbnb$reviews_per_month, airbnb$number_of_reviews)

#head(sort(airbnb$reviews_per_month, decreasing = TRUE), 100)
top100rpm <- head(order(airbnb$reviews_per_month, decreasing = TRUE), 100)

#head(sort(airbnb$number_of_reviews, decreasing = TRUE), 100)
top100nrev <- head(order(airbnb$number_of_reviews, decreasing = TRUE), 100)

# How many values in the top 100 rate (rev per mo) are in the top 100 total (num of reviews)
table(top100rpm %in% top100nrev)

# 4.2: Analyze at least three other groups as in 4.1



# Q5
# Propose three different hypotheses for business analysis



#########################
# PART 2: Data Analysis #
#########################

# Q6
# 6.1: Make ONE plot to visualize relationship between review_scores_rating and number of reviews
# for all categories of property_type. Explain your findings.

# 6.2: Make ONE plot to show relationship among property types, room types, bed types, and reviews
# per month. Explain your findings.

# 6.3: Make some plots to explore hypotheses in Q5. Explain your choice and describe interesting findings.



# Q7
# 7.1: Clean the price
# Price was cleaned back in step 1 with the following:
# airbnb$price <- as.numeric(gsub("^\\$|,","",airbnb$price))

# 7.2: Add number of amenities as column
# Amenities are separated by a comma


# 7.3: Calculate mean review_scores_rating against cancellation policies. What do you find?


# Q8
# Linear Modeling


############################
# PART 3: Further Analysis #
############################


# Q9
# 9.1: Explore relationships (if any) between superhost and host_since, host_response_time, host_response_rate.
# host_verifications, host_identity_verified

# 9.2: Create mosaic plot for host_response_time by superhost. What do you learn?



# Q10
# 10.1: Extract unique words in description and eliminate stop words. Store in dataframe and sort decreasing.
# What do you infer from words with top 10 frequency?

# 10.2: Explore whether beach affects price of a listing. Explore multiple high frequency words.

# 10.3: Select at least 3 other words from your dataframe and do similar analysis. What conclusions do you find?


# Q10(2)
# Q10(2).1 LOOK IT UP

# Q10(2).2: Choose two other aspects from description that may improve the weighted mean of review_scores_rating



#####################
# PART 4: Your Turn #
#####################

# Conduct further analysis





######################
# PART 5: Conclusion #
######################



#####################################
# PART 6: Lifecycle of Data Science #
#####################################



