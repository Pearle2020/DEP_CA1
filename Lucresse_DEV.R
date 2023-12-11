#input data set  into data frame 
crimeDS <- read.csv(file = "crime.csv", stringsAsFactors = TRUE)

#we get the dimension of the dataset
dim(crimeDS)

#we check the structure of the data
str(crimeDS)

# show the first ten records 
crimeDS[1:10,]

# we find the name of all our column
column_names <- names(crimeDS)
print(column_names)

#========================================================================================================
#variable category
typeOfVar <- sapply(crimeDS, function(col) {
  if (is.factor(col) | is.character(col)) {
    return("Categorical")
  } else if (is.numeric(col)) {
    if (length(unique(col)) < 20) {
      return("Discrete")
    } else {
      return("Continuous")
    }
  } else {
    return("Other")
  }
})
#create a data frame with the summary
variable_summary <- data.frame(Var = names(typeOfVar), Typeofvar = typeOfVar)
#pritning the summary
print(variable_summary)
str(variable_summary)



#==============================================================================
#visualization of the variable type with a heatmap
install.packages("pheatmap")
library(pheatmap)

# Create a matrix for the heatmap
heatmapOfVar <- table(variable_summary$Var, variable_summary$Typeofvar) 
# Create the heatmap
pheatmap(heatmapOfVar,
         color = c("lightyellow", "green"),
         main = "Variable Types ",
         display_numbers = FALSE,
         cluster_cols = FALSE,
         fontsize_col = 12,
         fontsize = 8)

#=========================================================================================================
#data cleaning

#count total missing values in each column
sapply(crimeDS, function(x) sum(is.na(x)))

# we will handle the empty string in the shooting attribute
# we will replace the empty space by N 
crimeDS$SHOOTING <- as.character(crimeDS$SHOOTING)
crimeDS$SHOOTING <- factor(
  ifelse(is.na(crimeDS$SHOOTING), "N", crimeDS$SHOOTING),
  levels = c("Y", "N")
)

#check the missing space
summar1 <- colSums(crimeDS ==" ")
print(summar1)
crimeDS2 <-crimeDS
print(crimeDS2)

# Remove specified columns
crimeDS_Clean <- crimeDS2[, !(names(crimeDS2) %in% c("Lat", "Long", "Location","UCR_PART","STREET"))]

#check all the empty values for the variables
missing_val <- is.na(crimeDS_Clean)
summary(missing_val)

#Replacing the NAS in the reporting_area column by the mean
reporting_area_mean <- mean(crimeDS_Clean$REPORTING_AREA, na.rm = TRUE)
crimeDS_Clean$REPORTING_AREA[is.na(crimeDS_Clean$REPORTING_AREA)] <- reporting_area_mean
colSums(is.na(crimeDS_Clean))
head(crimeDS_Clean)

# check the variables with the Na
colSums(is.na(crimeDS_Clean))

# function to remove outliers 
remove_outliers <- function(crimeDS_Clean, variable, threshold = 1.5) {
  q1 <- quantile(crimeDS_Clean[[variable]], 0.25)
  q3 <- quantile(crimeDS_Clean[[variable]], 0.75)
  iqr <- q3 - q1
  lower_limit <- q1 - threshold * iqr
  upper_limit <- q3 + threshold * iqr
  crimeDataSet_filtered <- crimeDS_Clean[crimeDS_Clean[[variable]] >= lower_limit & crimeDS_Clean[[variable]] <= upper_limit, ]
  return(crimeDataSet_filtered)
}
#Removing outliers
 variable_outliers <- "HOUR"
crimeDS_Clean <- remove_outliers( crimeDS_Clean, variable_outliers)

#printing theh clean dataset
 print(crimeDS_Clean)

#========================================================================================
#lets calculate the statistical parameter for each of the numerical variables
# statistical  of HOUR
hour_mean <- mean(crimeDS_Clean$HOUR)
print(hour_mean)
hour_median <- median(crimeDS_Clean$HOUR)
print(hour_median)
hour_min <- min(crimeDS_Clean$HOUR)
print(hour_min)
hour_max <- max(crimeDS_Clean$HOUR)
print(hour_max)
hour_sd <- sd(crimeDS_Clean$HOUR)
print(hour_sd)

# statistical  of  year
year_min <- min(crimeDS_Clean$YEAR)
print(year_min)
year_max <- max(crimeDS_Clean$YEAR)
print(year_max)

# statistical  of month
month_mean <- mean(crimeDS_Clean$MONTH)
print(month_mean)
month_median <- median(crimeDS_Clean$MONTH)
print(month_median)
month_min <- min(crimeDS_Clean$MONTH)
print(month_min)
month_max <- max(crimeDS_Clean$MONTH)
print(month_max)
month_sd <- sd(crimeDS_Clean$MONTH)
print(month_sd)

# statistical  of reporting area
reporting_area_mean <- mean(crimeDS_Clean$REPORTING_AREA)
print(reporting_area_mean)
reporting_area_median <- median(crimeDS_Clean$REPORTING_AREA)
print(reporting_area_median)
reporting_area_min <- min(crimeDS_Clean$REPORTING_AREA)
print(reporting_area_min)
reporting_area_max <- max(crimeDS_Clean$REPORTING_AREA)
print(reporting_area_max)
reporting_area_sd <- sd(crimeDS_Clean$REPORTING_AREA)
print(reporting_area_sd)



# ================================================================================================
#min and max normalization
# required libraries
library(ggplot2)
library(robustbase)
library(gridExtra)
# the function
normalizeMinMax <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# applying Min-Max Normalization to the numerical columns
columnsNormalize <- c("YEAR", "MONTH", "HOUR","REPORTING_AREA")
crimeDS_minmax <- crimeDS_Clean
crimeDS_minmax[, columnsNormalize] <- apply(crimeDS_Clean[, columnsNormalize], 2, normalizeMinMax)
# print the results
print(crimeDS_minmax)

  
# Robust Standardization function
columnsNormalize <- c("YEAR", "MONTH", "HOUR", "REPORTING_AREA")
crimeDS_robust <- crimeDS_Clean
crimeDS_robust[, columnsNormalize] <- scale(crimeDS_Clean[, columnsNormalize], center = TRUE, scale = TRUE)
print(crimeDS_robust)
 
#z-score standardization
standardizeZScore <- function(x) {
  return ((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}
columnsNormalize <- c("YEAR", "MONTH", "HOUR", "REPORTING_AREA")
crimeDS_zscore <- crimeDS_Clean
crimeDS_zscore[, columnsNormalize] <- apply(crimeDS_Clean[, columnsNormalize],2, standardizeZScore)
print(crimeDS_zscore)



#================================================================================================================


#========================================================================
#Data visualization and correlation between the features of the dataset
library(ggplot2)
ggplot(crimeDS_Clean, aes(x = YEAR)) +
  geom_density(fill = "blue", color = "black") +
  labs(title = " Crime Incidents by Year")

ggplot(crimeDS_Clean, aes(x = 1, y = MONTH)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "Crime Incidents by Month")

ggplot(crimeDS_Clean, aes(x = REPORTING_AREA)) +
  geom_histogram(binwidth = 1, fill = "black", color = "purple") +
  labs(title = "Distribution of Crime Incidents by Reporting Area")


#CREATING A boxplot to show the distribution og the shooting in reported area
ggplot(crimeDS_Clean, aes(x=SHOOTING, y=REPORTING_AREA)) + 
  geom_boxplot(fill="green")+
  labs(title = "Shouting in  Reporting Area")

#this a plot that show the repartition of the shooting HOURS across the year
positif_shooting <- subset(crimeDS_Clean, SHOOTING =="Y")
ggplot(positif_shooting, aes(x=YEAR, y=HOUR)) + 
  geom_point(size = 4 , color ="purple")+
  labs(title = "The correlation of shouting hours in  the years")

# Convert DAY_OF_WEEK to a factor with a specific order
crimeDS_Clean$DAY_OF_WEEK <- factor(crimeDS_Clean$DAY_OF_WEEK)
# Aggregate the data to get the total sum of hours for each day
agg_data <- crimeDS_Clean %>%
  group_by(DAY_OF_WEEK) %>%
  summarise(Total_Hours = sum(HOUR, na.rm = TRUE))
# Create a bar plot
ggplot(agg_data, aes(x = DAY_OF_WEEK, y = Total_Hours)) +
  geom_bar(stat = "identity", fill= "blue") +
  labs(title = "Total Sum of Hours Per Day", x = "Day of Week", y = "Total Hours") +
  theme_minimal()


#creating the plot that show the area with positif shooting
# subset for the scatter plot that takes in consideration the positif shooting
crimeDS_Subset <- crimeDS_Clean[crimeDS_Clean$SHOOTING == "Y", c("REPORTING_AREA", "SHOOTING")]
# Creating a scatter plot
ggplot(crimeDS_Subset, aes(x = REPORTING_AREA, y=2, size= 15)) +
  geom_point(shape= 21,fill = "lightpink") +
  labs(title = "Shooting in Various Reporting Areas",
       x = "Reporting Area",
       y = ""
       )


#creation of a pie chart to show what are the most commun offense description
#we start by creating a data frame that counts the number of each offense
library(dplyr)
offense_DS <- crimeDS_Clean %>%
  group_by(OFFENSE_CODE_GROUP) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
#we create a pie chart
ggplot(offense_DS, aes(x = "", y = percentage, fill = OFFENSE_CODE_GROUP)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_discrete(labels = paste(offense_DS$OFFENSE_CODE_GROUP, sprintf("%.1f%%", offense_DS$percentage)))
  labs(title = "Distribution of OFFENSES per percentage")
  
  
#lets create a line plot to show the corrolation between the offense code and the the area this offense where reported during the years
ggplot(crimeDS_Clean, aes(x = YEAR, y = OFFENSE_CODE, group = REPORTING_AREA, color = REPORTING_AREA)) +
  geom_point() +
  labs(title = "OFFENSE_CODE REPORTED IN AREA in the years",
       x = "YEAR",
       y = "OFFENSE_CODE")
  



#================================================================================================
install.packages("caret")
library(caret)
#lets apply dummy encoding to our categorical variable
#we choose the variable that we want to encode
encoded_Data <- crimeDS_Clean$DAY_OF_WEEK
#to check the dta
encoded_Data
#we create a dummy variable usint the dummy vars from the caret library
dummy_Inf <- dummyVars("~ .",data = data.frame(DAY_OF_WEEK = encoded_Data) )
#we apply the encoding
dummy_Inf <- predict(dummy_Inf, newdata = crimeDS_Clean)
dummy_Inf



#==============================================================================================================================
#lets determine the PCA 
install.packages("FactoMineR")
library(FactoMineR)
library(dplyr)
# we create a new datset with just nemerical varaible 
numerical_variable <- crimeDS_Clean %>%
  select_if(is.numeric)
#we check the dataset
str(numerical_variable)
# wwe normalize the data
norm <- scale(numerical_variable)
#we check the norm
norm
pca1<- princomp(norm)
summary(pca1)
# screeplot to visualise the variance
screeplot(pca1, type = "bar")
# we biplot to visualise
biplot(pca1, choices = c(1, 5), scale = 0)
 





