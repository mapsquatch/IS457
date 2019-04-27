# Load libraries
library(ggplot2)
library(dplyr)
library(lattice)
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

# PHIL


# 1.3: Describe how your choice method may impact later analysis.

# PHIL

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
# To clean this, I will use median. It lies between the mode and the mean, so I'm using the central measure of centrality. :)
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

# Show dimensions; I added a column for days listed
dim(airbnb)

# 1.6: Comment on and explain any other data cleaning or preparation steps you think would be 
#      necessary from your inspection of the data (you do not have to carry them out).

# host_since will need to be converted to date.
# host_response_time should become a factor (to compare levels)
# host_response_rate need the % sign scrubbed and converted to numeric (maybe even percentage, e.g. /100)
# price and cleaning_fee need $ scrubbed and converted to numeric



# Q2
# Conduct a preliminary exploration and describe what you find interesting or unexpected.

# This will create two lists: counts (table()) for categorical data, and summaries for continuous data
# This allows me to visually inspect the numerical distribution of each variable (using View())
col_categories <- c(7,9,11:20,35)
col_continuous <- c(6,8,22:34,36)

counts <- lapply(airbnb[,col_categories], function(x) table(x))
summaries <- lapply(airbnb[,col_continuous], function(x) summary(x))

counts

# host_response_time: Most hosts respond quickly (within an hour), although the imputed unknown category makes up the second-largest percentage.
# host_is_superhost: 2795 of the 10,815 are superhosts.
# host_identity_verified: just under half of listing have a verified host. This even number could make for good comparisons if splitting the data.
# city: What a mess! Invalid charactersets, inconsistent naming and upper/lower casing of city names (see: Bondi Beach, bondi beach, Bondi beach, "Bondi Beach, Sydney"). I will ignore if possible.
# zipcode: The distribution is uneven, and I don't know anything about the districts themselves. However, it's the cleanest geographic data available.
# property_type: Mostly Apartment (~58%) and House (~24%), although 31 property types total.
# room_type: Three categories. Predominantly Entire home/apt, 26% Private room, and only 84 in a Shared room.
# accommodates: Mostly 2, 4, 6, 3, 1, 5, then 8, 7, 10, and up to 16. Even numbers are more common.
# bathrooms: Mostly 1 or 2, but goes up to 10!
# bedrooms: Zero to four are "common" (>500), but continous up to 7 BR, then an outlier with 14 bedrooms.
# beds: Zero to 14, then 16, 18, and 29(!). Interesting about zero beds; maybe a couch?
# bed_type: Aha, couch is a type. 99.28% Real bed, with the remainder scattered around lesser bed types. There are 77 non-Real bed observations. It is plausible that the 69 zero beds observations are included in the 77.
# cancellation_policy: Five categories -- flexible, moderate, strict and two levels of super_strict. I wonder if this affects reviews?

summaries

# Summaries help with quartiles and ranges, skew can be somewhat interpreted by comparing median to mean.

# host_since: median and mean almost identical; six years from min to median, 3.5 years median to max. Listings have been added more frequently over time.
# host_response_rate: 1st quartile is 100%, so most hosts respond to inquiries.
# price: median of 150 and mean of 203 indicates positive skew. This is expected on this type of variable; prices have no upper limit, but are bound on the lower end by zero.
# cleaning_fee: same positive skew as price
# guests_included: strong positive skew. median is 1, 3rd quar is 2, and mean is 1.969. Max of 16
# extra_people: the max value of 410 may be influencing the mean
# minimum_nights: skewed HEAVILY by the max of 500. A 500 night minimum seems to go against the typical use case for AirBnB that I understand. Higher minimum night values will suppress the possible number of reviews per month. I want to explore this.
# number_of_reviews: median is 12; max is 493. Indicates popularity and longevity of a listing.
# review_scores_rating: the overall rating on a scale of 20 to 100 (the minimum is 20). Can I assume this relates to a 5 star rating scale, where each star is worth 20 points?
# review_scores_xxx: the xxx refers to subcategories of six review components: accuracy (of listing description), cleanliness, checkin, communication, location, and value.
# reviews_per_month: effectively -- number_of_reviews / number of months listed. median .95, mean 1.5. max of 15.18 (averaging a new guest every other night!)


# Q3
# Explore comprehensively with charts, tables, and graphs
# 3.1: Think about types of variables; choose appropriate graphs to find distributions and trends

# PHIL

# Date / Growth Over Time
ggplot(data = airbnb, aes(x = host_since)) +
  stat_ecdf(geom = "step") +
  ggtitle("AirBnB Growth Over Time (Sydney, Australia)") +
  xlab("Host Since") +
  ylab("Percent of Data (n = 10,815)")

# Categorical Graphs
for(i in names(airbnb[col_categories])){
  print(i)
  ggplot(data = airbnb, aes(x = i)) +
    geom_histogram(stat="count")
}

lapply(airbnb[col_categories], function(thiscol){
  print(names(thiscol))
         ggplot(data = airbnb, aes(x = thiscol)) +
         geom_histogram(stat="count")+
         ggtitle(names(thiscol))}
)

ggplot(data = airbnb, aes(x = host_identity_verified)) +
  geom_histogram(stat="count") +
  xlab("Host Since") +
  ylab("Percent of Data (n = 10,815)")


# 3.2: Compare different graph types to see which ones best convey trends, outliers, and patterns

# What is the distribution of the different review scores?
boxplot(airbnb[29:34], las=1, main = "Distribution of Scores")

# PHIL

# 3.3: Describe what you find from the graphs

# PHIL

# Q4
# 4.1: Compare and contrast review_per_month and number_of_reviews

# A simple scatterplot; garbage-y
#plot(airbnb$reviews_per_month, airbnb$number_of_reviews)

#head(sort(airbnb$reviews_per_month, decreasing = TRUE), 100) # DELETE
# order() pulls the row values; I can use these to subset the IDs
top100rpm <- airbnb$id[head(order(airbnb$reviews_per_month, decreasing = TRUE), 100)]

#head(sort(airbnb$number_of_reviews, decreasing = TRUE), 100)
top100nrev <- airbnb$id[head(order(airbnb$number_of_reviews, decreasing = TRUE), 100)]

# How many values in the top 100 rate (rev per mo) are in the top 100 total (num of reviews)
length(intersect(top100rpm, top100nrev))
# Which IDs are in both lists?
intersect(top100rpm, top100nrev)

# Discussion and findings
# PHIL
# 22 listings are in both the top 100 review_per_month and the top 100 number_of_reviews.

# 4.2: Analyze at least three other groups as in 4.1

#PHIL1
#PHIL2
#PHIL3


# Q5
# Propose three different hypotheses for business analysis

#PHIL1
#PHIL2
#PHIL3


#########################
# PART 2: Data Analysis #
#########################

# Q6
# 6.1: Make ONE plot to visualize relationship between review_scores_rating and number of reviews
# for all categories of property_type. Explain your findings.

xyplot(airbnb$number_of_reviews ~ airbnb$review_scores_rating | airbnb$property_type, groups = airbnb$property_type, xlab = "Review Rating (20 = 1 star, 100 = 5 star)", ylab = "Number of Reviews", main="AirBnB Review Scores vs Number of Reviews (Sydney, Australia)")

# Findings
# Although certain property types are more common than others, the distributions are largely similar: it is rare for 
# a rental to get over 100 reviews if it is not at least a four-star property (rating = 80). This is why under the 100 
# review line, a variety of scores can be found (although still concentrated toward four- and five-star reviews). Poorly
# reviewed rentals will see fewer guests and thus, reviews.

# I'm unsure if all of these property types have been available for the same length of time. For example, hostels and boutique
# hotels seem to have a moderate number of reviews, but none over 100. I wonder if this is a newer addition to the property types.


# 6.2: Make ONE plot to show relationship among property types, room types, bed types, and reviews
# per month. Explain your findings.

# Get a target row count (10,546)
sum(head(sort(table(airbnb$property_type), decreasing = TRUE), 10))

# Subset the data frame to top property types
# Get a vector of property types
i <- names(head(sort(table(airbnb$property_type), decreasing = TRUE), 10))
# Subset
prairbnb <- airbnb[airbnb$property_type %in% i,]
# A double check
table(prairbnb$property_type)

# For best results, view on an 80" plasma 4k TV
bwplot(prairbnb$reviews_per_month ~ prairbnb$property_type | prairbnb$bed_type + prairbnb$room_type,
       scales=list(x=list(rot=90, cex=0.5)),
       xlab='Bed Type', ylab='Reviews Per Month',
       par.strip.text=list(cex=.55),
       cex = .5,
       main = "Reviews Per Month by Property, Room, and Bed Types")


# Findings

# This layout is a grid of plots: each row contains one of three room types, and each column contains one of five bedtypes.
# The property types are shown in the boxplot inside each grid cell. This layout shows that "real bed" is the most common
# bed type (due to the visual density of the boxplots). Real beds are also found at the most popular listings: all listings
# with over 10 reviews per month have a real bed. Futons, couches, and airbeds are not common, and the listings that 
# have them do not have more than five reviews per month. The pull-out sofa is the second-most popular bed type, and it
# has a couple listings that break the five review per month level. In the Real Bed > Entire Home and Private Room graphs,
# the large amount of outliers above the whiskers indicate a positively-skewed distribution.


# 6.3: Make some plots to explore hypotheses in Q5. Explain your choice and describe interesting findings.

# PHIL FOLLOW UP TO 5


# Q7
# 7.1: Clean the price

# Price was cleaned back in step 1 with the following:
# airbnb$price <- as.numeric(gsub("^\\$|,","",airbnb$price))

# 7.2: Add number of amenities as column

# Amenities are separated by a comma and opened with a curly brace. Count the curly brace and commas for num of amenities
# This statement is using lapply to make a vector 10815 elements long. gregexpr returns a list, and I need to get the 
# length of the first element of the list
airbnb$number_of_amenities <- sapply(airbnb$amenities, function(x) length(gregexpr("\\{|,",x)[[1]]))

# Problem with the above is that it counts empty curly brace as 1
airbnb$number_of_amenities <- sapply(airbnb$amenities, function(x) length(strsplit(x,",")[[1]]))
# This does the same. 

# Clean up the ones; using a within() statment to minimize typing airbnb$
airbnb <- within(airbnb, number_of_amenities[number_of_amenities == 1] <- 0)

# 7.3: Calculate mean review_scores_rating against cancellation policies. What do you find?
# Using Base R
by(airbnb$review_scores_rating, airbnb$cancellation_policy, mean)
# Using dplyr
airbnb %>% group_by(cancellation_policy) %>% summarise(mean = mean(review_scores_rating))

# Findings
# The super-strict cancellation policies have the worst scores -- under 90. These are less guest-friendly.
# The highest mean was for the moderate policy, which sits between flexible and strict_14_with_grace_period
# on the guest-friendly scale. Why wouldn't flexible -- as the most guest-friendly --  have the highest ratings? 
# Perhaps the hosts who choose the flexible policy are more care-free and less professional in their rental?
# It could also be statistical noise; are 95.00 and 94.15 meaningfully different?

#PHIL NOT SURE
t.test(airbnb$review_scores_rating[airbnb$cancellation_policy == "flexible"],airbnb$review_scores_rating[airbnb$cancellation_policy == "moderate"])

# PHIL MORE DATA MANIP
# DPLYR
# DONT FORGET TO GRAB COOL AMENITY DATA; YOUTHS LOVE WIFI
airbnb %>% group_by(property_type) %>% summarise(mean = mean(review_scores_rating))
airbnb %>% group_by(room_type) %>% summarise(mean = mean(review_scores_rating))
airbnb %>% group_by(bed_type) %>% summarise(mean = mean(review_scores_rating))
airbnb %>% group_by(number_of_amenities) %>% summarise(mean = mean(review_scores_rating))


# Q8
# Linear Modeling



############################
# PART 3: Further Analysis #
############################


# Q9
# 9.1: Explore relationships (if any) between superhost and host_since, host_response_time, host_response_rate.
# host_verifications, host_identity_verified

# PHIL Q9 is FUCKED
plot(lm(airbnb$host_is_superhost ~ airbnb$host_since))


# 9.2: Create mosaic plot for host_response_time by superhost. What do you learn?

mtab <- table(airbnb$host_is_superhost, airbnb$host_response_time)
mosaicplot(table(airbnb$host_is_superhost, airbnb$host_response_time), sort=c(c(0,1),c(5,4,3,1,2)))

library("ggmosaic")
ggplot(data = airbnb) +
  geom_mosaic(aes(x = product(as.factor(airbnb$host_response_time), as.factor(airbnb$host_is_superhost)), fill=as.factor(airbnb$host_response_rate)), na.rm=TRUE) +
  labs(x="Is Superhost", y="Cut", title="Diamond Cut vs Clarity") 


# Q10
# 10.1: Extract unique words in description and eliminate stop words. Store in dataframe and sort decreasing.
# What do you infer from words with top 10 frequency?

# Get all "words", splitting on space
#all_words <- unlist(strsplit(airbnb[1:3,2], "[[:space:]]"))
# Extract only alpha letters
#regmatches(all_words, regexpr("[[:alpha:]]+", all_words))
#grep("[[:alpha:]]",unlist(strsplit(airbnb[1:3,2], "[[:space:]]|,|.")), value = TRUE)
#unlist(grep("[[:alpha:]]", airbnb[1:3,2], value=TRUE))

# After wrangling with "what is a word" and handling punctuation, I've decided to leverage libraries built to deal with this
# https://www.tidytextmining.com/tidytext.html

#install.packages("tidytext")
library(tidytext)

# Load the stop words (to be omitted from analysis) into a tidytext-compatible data frame
#is457_stop_words <- c("a", "able", "about", "across", "after", "all", "almost", "also", "among", "and", "are", "almost", "at", "almost", "also", "am", "among", "an", "and", "any", "are", "as", "at", "be", "because", "been", "but", "by", "can", "cannot", "could", "dear", "did", "do", "does", "either", "else", "ever", "every", "for", "from", "get", "got", "had", "has", "have", "he", "her", "hers", "him", "his", "how", "however", "i", "if", "in", "into", "is", "it", "its", "just", "least", "let", "like", "likely", "may", "me", "might", "most", "must", "my", "neither", "no", "nor", "not", "of", "off", "often", "on", "only", "or", "other", "our", "own", "rather", "said", "say", "says", "she", "should", "since", "so", "some", "than", "that", "the", "their", "them", "then", "there", "these", "they", "this", "is", "to", "too", "was", "us", "wants", "was", "we", "were", "what", "when", "where", "which", "while", "who", "whom", "why", "will", "with", "would", "yet", "you", "your")
is457_stop_words_df <- data.frame(lexicon = "is457", word = c("a", "able", "about", "across", "after", "all", "almost", "also", "among", "and", "are", "almost", "at", "almost", "also", "am", "among", "an", "and", "any", "are", "as", "at", "be", "because", "been", "but", "by", "can", "cannot", "could", "dear", "did", "do", "does", "either", "else", "ever", "every", "for", "from", "get", "got", "had", "has", "have", "he", "her", "hers", "him", "his", "how", "however", "i", "if", "in", "into", "is", "it", "its", "just", "least", "let", "like", "likely", "may", "me", "might", "most", "must", "my", "neither", "no", "nor", "not", "of", "off", "often", "on", "only", "or", "other", "our", "own", "rather", "said", "say", "says", "she", "should", "since", "so", "some", "than", "that", "the", "their", "them", "then", "there", "these", "they", "this", "is", "to", "too", "was", "us", "wants", "was", "we", "were", "what", "when", "where", "which", "while", "who", "whom", "why", "will", "with", "would", "yet", "you", "your"))

# Subset ID and Description
text_df <- airbnb[,1:2]

# Unnest_tokens does some heavy-lifting: it splits out each word, converts to lowercase
tidy_txt <- text_df %>% unnest_tokens(word, description)

# Remove ("anti-join") the stop words for this class (tidytext comes with its own dictionaries of stop words)
tidy_txt <- tidy_txt %>% anti_join(is457_stop_words_df)

# Count'em and keep the top 10
tidy_txt %>% count(word, sort = TRUE) %>% head(10)

# 10.2a: Explore whether beach affects price of a listing. What is the difference in average price?

# Get "beach" and "beaches" listing IDs
has_beach <- tidy_txt$id[tidy_txt$word == "beach"]
has_beaches <- tidy_txt$id[tidy_txt$word == "beaches"]

# These vectors have every mention of the word
paste(length(has_beach),"mentions of beach, and",length(has_beaches),"mentions of beaches.")
# Show unique listings with these words
paste(length(unique(has_beach)),"listings have beach, and",length(unique(has_beaches)),"listings have beaches.")

# union() removes duplicates
beachy <- union(has_beach, has_beaches)
paste(length(beachy),"listings in total mention beach or beaches.")

# How do beach listings prices compare to non-beach?
# Convert beachy vector to a data frame to merge into airbnb as a logical column
beachy <- data.frame(id = beachy, beach_desc = TRUE)

airbnb <- merge(airbnb, beachy, all.x = TRUE, by="id")
airbnb$beach_desc[is.na(airbnb$beach_desc)] <- FALSE

# I still have 4655 listings with beach, now as a logical column in airbnb
table(airbnb$beach_desc)

mean(airbnb$price)
by(airbnb$price, airbnb$beach_desc, mean)
airbnb %>% group_by(beach_desc) %>% summarise(mean(price))

# Findings
# The average price for all listings is $203.16. Listings that mention "beach" or "beaches" average price is 
# $236.91 ($33.75 above average). Listings without those words average $177.25 ($25.51 below average).
# Compared to each other, a beach listing is priced 33% higher.


# 10.2b: Explore multiple high frequency words. Write a function to get word frequency by row.

count_word_in_desc <- function(w, v){
  # funcname: count_word_in_desc
  # inputs  : A word (character string) to search for, a vector to search in
  # outputs : count of words (integer)
  # purpose : Count appearances of a string
  # related : N/A
  # auth/dt : ID35, 2019-04-25
  
  w <- tolower(w)
  v <- tolower(v)
  
  if(attr(gregexpr(w,v)[[1]], "match.length")[1] == -1){
    return(0)
  }
  else{
    return(length(attr(gregexpr(w,v)[[1]], "match.length")))
  }
}

# Now to find some top words! I dug a little
# Beach, just for comparison to the tidytext. This one finds 4,694 beach listings, but the results are similar.
airbnb$wc_beach <- unlist(lapply(airbnb$description, function(x) count_word_in_desc("beach",x)))
airbnb %>% group_by(wc_beach>0) %>% summarise(n = n(), avg_price = mean(price), avg_review = mean(review_scores_rating))

# What listings are different?
diffs <- airbnb$description[airbnb$beach_desc == FALSE & airbnb$wc_beach > 0]
# beachfront, beachside, beachy, beachvolleyball for a few examples

# 10.3: Select at least 3 other words from your dataframe and do similar analysis. What conclusions do you find?

airbnb$wc_kitchen <- unlist(lapply(airbnb$description, function(x) count_word_in_desc("kitchen",x)))
airbnb$wc_restaurants <- unlist(lapply(airbnb$description, function(x) count_word_in_desc("restaurants",x)))
airbnb$wc_bus <- unlist(lapply(airbnb$description, function(x) count_word_in_desc("bus",x)))
airbnb$wc_walk <- unlist(lapply(airbnb$description, function(x) count_word_in_desc("walk",x)))
airbnb$wc_balcony <- unlist(lapply(airbnb$description, function(x) count_word_in_desc("balcony",x)))

#airbnb$wc_kitchen[airbnb$wc_kitchen > 0]
airbnb %>% group_by(wc_kitchen>0) %>% summarise(n = n(), avg_price = mean(price), avg_review = mean(review_scores_rating))
airbnb %>% group_by(wc_restaurants>0) %>% summarise(n = n(), avg_price = mean(price), avg_review = mean(review_scores_rating))
airbnb %>% group_by(wc_bus>0) %>% summarise(n = n(), avg_price = mean(price), avg_review = mean(review_scores_rating))
airbnb %>% group_by(wc_walk>0) %>% summarise(n = n(), avg_price = mean(price), avg_review = mean(review_scores_rating))
airbnb %>% group_by(wc_balcony>0) %>% summarise(n = n(), avg_price = mean(price), avg_review = mean(review_scores_rating))

# Findings

# Scanning the top words, I looked for words that indicated something possibly unique about a listing (e.g. kitchen) versus
# something more routine (e.g., bed). I focused on a few areas: food (kitchen vs restaurants), transportation (bus vs walking),
# and the balcony (just curious about it more than anything).

# What I found is that these words had a small effect on the average review (the largest spread was 0.4, or just under
# a quarter-star difference: "walk" and "restaurants" both scored higher than their non-walk and non-restaurant counterparts).
# PHIL yammer on

# Q10(2)
# Q10(2).1 Choose between zip code or city. Justify. Calculate number of listings for each in category. Filter to top 100.
# Explore whether top 100 have higher weighted ratings. Graph and explain your findings.

# The city data is very messy, with case and formatting issues. I'm going to use zip code data.

# Grab the top 100, but exclude the "unknown" that I imputed earlier.
#head(sort(table(airbnb$zipcode[airbnb$zipcode != "unknown"]), decreasing = TRUE), 100)

# I have just been learning the dplyr tools with this project, and I find this much more readable:
airbnb %>% select(zipcode) %>% filter(zipcode != "unknown") %>% table() %>% sort(decreasing = TRUE) %>% head(100)

# PHIL

# Q10(2).2: Choose two other aspects from description that may improve the weighted mean of review_scores_rating

# PHIL

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



