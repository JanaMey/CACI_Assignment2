# This R script is to help you with reshaping and bringing the data into a more
# useful format to work with
# Adjust the path for setting the working directory
# and run the code step-by-step to understand what is happening in each step
# Read the comments!

# Preliminary steps ==============================================================
# Set the working directory ------------------------------------------------------
#path <- file.path(setwd("C:/Users/Lilli/Google Drive/2021CACI/Assignment02/CACI_Assignment2")) #path Lilli
#setwd(path)
# Der Pfad wird mit dem Link unten nicht benötigt.

# Install and load the required libraries ----------------------------------------
pacman::p_load(reshape2, ggplot2, dplyr, stringr)

urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment2/main/QuestionaireData_CityTrips.csv?token=AIBBFR5OGL'

# Load required dataset ----------------------------------------------------------
data.wide <- read.csv(urlfile)

summary(data.wide)
head(data.wide)
dim(data.wide) # 266 462

# Create a new unique ID index ===================================================
# The ID index resets for each Sample. Therefore, we need to create a unique id 
# index
data.wide$id_unique <- 1:nrow(data.wide)
head(data.wide)

# Reshaping data from wide to long format ========================================
# Separate attribute evaluations (here, I am going to use dplyr package, 
# the same can be accomplished in other ways working with base R)
# However, dplyr has some nice functions, which allow us to select variables
# with similar names with much less lines of code

# First let's separate attribute evaluations and preference ratings --------------
# which are on individual-city (only 6 cities in the sample) level
names(data.wide)

# We want to select all columns that have _Att and Pref in the names and 
# save this object
data.long <- data.wide %>% 
  select(c("Sample", "ID", "id_unique"), # we still need the respondent index
         contains("_Att"),  # any column that contains _Att in the name
         starts_with("Pref_")) # any column that starts with Pref_              

head(data.long)
dim(data.long) # 266 402

# Reshape to long format
data.long <- melt(data.long, id.vars = c("Sample", "ID", "id_unique"))
head(data.long)
dim(data.long)

# Let's delete the NAs
data.long <- data.long[!is.na(data.long$value), ]
dim(data.long)


# Now we have both attribute evaluations of each city and preference rating stacked
# under each other
# Let's create a variable that differentiates whether it is attribute eval.
# or preference rating
data.long$type <- ifelse(str_detect(data.long$variable, "_Att"), "Attribute", "Pref")
head(data.long)


# Now we want to create a column City which is the 1st part of the variable
# string (before _) if it's type = "Attribute" and 2nd part (after _) if
# it's type = "Pref"
# For this we will use a useful function strsplit(), which 
# splits a string by the provided delimiter (here, underscore _)
# Let's run this part separately for the 1st row (observation)
strsplit(data.long[1, ]$variable, "_") # not working as variable is a factor
str(data.long)

# so we need to convert it to a character class and then apply strsplit()
strsplit(as.character(data.long[1, ]$variable), "_")

# Now we want to select only the 1st element. 
# We can do this using sapply()
sapply(strsplit(as.character(data.long[1, ]$variable), "_"), `[`, 1)

# if we wanted to choose the 2nd element, then
sapply(strsplit(as.character(data.long[1, ]$variable), "_"), `[`, 2)

# Ok, now let's apply it to each row and use ifelse() by column type
data.long$City <- ifelse(data.long$type == "Attribute",
                         sapply(strsplit(as.character(data.long$variable), "_"), `[`, 1),
                         sapply(strsplit(as.character(data.long$variable), "_"), `[`, 2))
head(data.long)
unique(data.long$City)

# Now let's create column Attribute, which is the index for attributes now contained
# in column variable if type = "Attribute" and let's set the value to "Pref" if type = "Pref
data.long$Attribute <- ifelse(data.long$type == "Attribute",
                              sapply(strsplit(as.character(data.long$variable), "_"), `[`, 2),
                              "Pref")
head(data.long)
unique(data.long$Attribute)

# Attribute column is a character. Let's save it as a factor and
# assign meaningful labels from the documentation file
# Define the vector of labels
attLabels <- c("friendly", "historical", "affordable", "trendy",
               "vibrant nightlife", "delicious food", "easy-to-get-around",
               "good shopping", "cultural events", "interesting museums",
               "clean", "green", "international", "too touristic",
               "fun", "noisy", "romantic", "safe", "beautiful", 
               "english-speaker-friendly")
length(attLabels) # should be 20


# Set Attribute variable as a factor, define levels from 1 to 20, and 
# define the labels 
data.long$Attribute <- factor(data.long$Attribute, levels = c(paste0("Att", 1:20), "Pref"),
                              labels = c(attLabels, "Pref"))
head(data.long)

# Let's reshape to wide format 
# so that we have one column for each attribute and one column for Pref
data.long <- dcast(data.long, 
                   Sample + ID + id_unique + City ~ Attribute, 
                   value.var = "value")
head(data.long)
dim(data.long) # 1590   25

# Are these correct dimensions?
# Each respondent was supposed to evaluate 6 cities
length(unique(data.long.attr$id_unique)) # 266
266 * 6 # 1596 

# How come?
# There are still NAs in the data, these are the real missing values:
# item non-response: respondents didn't give an answer to this questions.

# We can save this data frame separately as a csv and load and work with it later
write.csv(data.long, file = "data_long.csv", row.names = FALSE)


# Separate individual-level variables ============================================
indivData <- data.wide %>% select(!contains("Pref") & !contains("_Att"))
head(indivData)
dim(indivData)

# Some checks
unique(data.long$City)
length(unique(data.long$City)) # 20


# We can save this data frame separately as a csv and load and work with it later
write.csv(indivData, file = "indivData.csv", row.names = FALSE)
