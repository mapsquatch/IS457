

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
# I will omit observations with missing data from analysis. Even with the higher levels of missing data (host
# response data at 23%), I will still be left with over 8,000 observations, which is a high enough n value
# to generate valid results.

# 1.3: Describe how your choice method may impact later analysis.
# The reduced n values 

# 1.4: Implement methods to deal with missing values.


# 1.5: After dealing with missing values, show the dimensions of the data.

# 1.6: Comment on and explain any other data cleaning or preparation steps you think would be 
#      necessary from your inspection of the data (you do not have to carry them out).

# host_since will need to be converted to date.
# host_response_time should become a factor (to compare levels)
# host_response_rate need the % sign scrubbed and converted to numeric (maybe even percentage, e.g. /100)
# price and cleaning_fee need $ scrubbed and converted to numeric



# Q2
# Conduct a preliminary exploration and describe what you find interesting or unexpected.



# Q3
# Explore comprehensively with charts, tables, and graphs
# 3.1: Think about types of variables; choose appropriate graphs to find distributions and trends

# 3.2: Compare different graph types to see which ones best convey trends, outliers, and patterns

# 3.3: Describe what you find from the graphs



# Q4
# 4.1: Compare and contrast review_per_month and number_of_reviews

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

# 7.2: Add number of amenities as column

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



