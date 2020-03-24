# Create the managers data frame
# Refer to notes "creating a dataset - problem" on Blackboard 
# for more information

column_names <- c("Date", "Country", "Gender", "Age", "Q1", "Q2", "Q3", "Q4", "Q5")

# Enter data into vectors before constructing the data frame
date_col <- c("2018/15/10", "2018/01/11", "2018/21/10", "2018/28/10", "2018/01/05")
country_col <- c("US", "US", "IRL", "IRL", "IRL")
gender_col <- c("M", "F", "F", "M", "F")
age_col <- c(32, 45, 25, 39, 99) # 99 is one of the values in the age attribute - will require recoding
q1_col <- c(5, 3, 3, 3, 2)
q2_col <- c(4, 5, 5, 3, 2)
q3_col <- c(5, 2, 5, 4, 1)
q4_col <- c(5, 5, 5, NA, 2) # NA is inserted in place of the missing data for this attribute
q5_col <- c(5, 5, 2, NA, 1)

# Construct a data frame using the data from all vectors above
managers <- data.frame(date_col, country_col, gender_col, age_col, q1_col, q2_col, q3_col, q4_col, q5_col)

# Add column names to data frame using column_names vector
colnames(managers) <- column_names

# Recode the incorrect 'age' data to NA
managers$Age[managers$Age == 99] <- NA

# Create a new attribute called AgeCat and set relevant valuess
# in AgeCat to the following if true:
# <= 25 = Young
# >= 26 & <= 44 = Middle Aged
# >= 45 = Elderly

# We will also recode age 'NA' to Elder
managers$AgeCat[managers$Age >= 45] <- "Elder"
managers$AgeCat[managers$Age >= 26 & managers$Age <= 44] <- "Middle Aged"
managers$AgeCat[managers$Age <= 25] <- "Young"
managers$AgeCat[is.na(managers$Age)] <- "Elder"

# Recode AgeCat so that is ordinal and factored with the
# order Young, Middle aged, Elder
# We'll store the ordinal factored data in variable 'AgeCat'
AgeCat <- factor(managers$AgeCat, order = TRUE, levels = c("Young", "Middle Aged", "Elder"))

# Replace managers AgeCat attribute with newly ordinal foctored data
managers$AgeCat <- AgeCat

# Create a new column called 'summary_col' that
# contains a summary of each row
summary_col <- managers$Q1 + managers$Q2 + managers$Q3 + managers$Q4 + managers$Q5
summary_col

# Add summary_col to the end of the data frame
managers <- data.frame(managers, summary_col)

# Calculate mean value for each row
mean_value <- rowMeans(managers[5:9])

# Add mean_value to end of managers data frame
managers <- data.frame(managers, mean_value)

# Show data frame contents
managers

# Change the name of this column to "mean value"
names(managers)[12] <- "mean value"

# Change name of summary_col to "Answer total"
names(managers)[11] <- "Answer total"

# Show managers structure
str(managers)


#adding new dataset

getwd()

new_managers_data <- read.csv ("MoreData.csv")

#show structure

str(new_managers_data)

#choosing records for merging

names(new_managers_data)

include_list <- new_managers_data[c("Date", "Country", "Gender", "Age", "Q1", "Q2", "Q3", "Q4", "Q5")]
include_list

#rbind

rbind(managers, include_list)

# creating the blabk columns

blank_cols <- c("Agecat", "Answer total", "mean value")
include_list[, blank_cols] <- NA

attach(include_list)
include_list$Agecat[Age >= 45] <- "Elder"
include_list$Agecat[Age >= 26 & Age <= 44] <- 'Middle aged'
include_list$Agecat[Age <= 25] <- 'Young'
include_list$Agecat[is.na(Age)] <- 'Elder'
detach(include_list)

# or also define

attach(include_list)
include_list$`Answer total` <- Q1 + Q2 + Q3 + Q4 + Q5
 #MEAN VALUE

 include_list$`mean value` <- rowMeans(include_list[5:9])
 
 # changing the date field
 
 str(include_list$Date)
 
 # the default R date structure is yyyy-mm-dd
 # the current date structure is mm/dd/yyyy
 restructured_date <- as.character(include_list$Date)
 str(restructured_date)
 
 
 restructured_date <- as.Date(include_list$Date, "%m/%d/%Y")
 restructured_date
 str(restructured_date)
 
 # assign restructured date into the date field in include_list

 str(include_list$Date)
  include_list$Date <- restructured_date
  str(include_list$Date)
  
  
 #convert the date field of managers to be in the same oas include_list
 
str(managers$Date)
managers_converted_date <- as.Date(managers$Date, "%Y-%d-%m")
str(managers_converted_date)

# assign new date to managers
managers$Date <- managers_converted_date

str(managers$Date)

#merge both

nrow(managers)
nrow(include_list)
managers <- rbind(managers, include_list)
nrow(managers)

# Change the date structure from the factor
# to the required date structure

# Note : We cannot convert a factor variable to date
# without first converting to a character variable
# from the default factor variable
date_field   <- as.character(managers$Date)
new_date <- as.Date(date_field, "%Y/%d/%m")
str(new_date)

# Now overwrite the contents of the date field with new date structure
managers$Date = new_date
str(managers)


# -------------------------------------------------------------------------------
# Dealing with missing data 
# -------------------------------------------------------------------------------

# removes any rows that contains NA - listwise deletion
new_data <- na.omit(managers)
new_data

# Use complete.cases to show rows where data is available
complete_data <- complete.cases(managers)
complete_data
# Show sum of missing rows
sum(complete_data)

# list the rows that do not have missing values
# Note that the ',' and no number inside square brackets means "all columns"
complete_data <- managers[complete.cases(managers),]
complete_data

# List rows with missing values
managers[!complete.cases(managers),]

# Find sum of all missing values in the age attribute
sum(is.na(managers$Age))

# Find the mean of missing values from the Age attribute
mean(is.na(managers$Age))

# Find the mean of rows with no incomplete data
# Note that we dont need to add the ',' as we're only
# looking for an overall mean of rows with missing values
mean(!complete.cases(managers))

#----------------------------------------------------------------------------
# Graphically display missing data
#----------------------------------------------------------------------------

install.packages("mice")
library(mice)
md.pattern(managers)

# Use VIM package to show missing values
install.packages("VIM")
library(VIM)
missing_values <- aggr(managers, prop = FALSE, numbers = TRUE)
# Show summary of the contents of missing_values
summary(missing_values)
# See this link for more info https://www.rdocumentation.org/packages/VIM/versions/4.8.0/topics/aggr 