# Name: Matthias Aulbach
# 31.01.2017
# This is the exercise for Week 2

learning2014 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep = "\t", header = TRUE)
str(learning2014)
dim(learning2014)

# The dataset learning2014 has 183 rows and 60 columns, representing 183 observations of 60 variables, i.e. a sample of 183 participants

# Accessing the dplyr library
library(dplyr)

# Summarizing the questions for deep, surface, and strategic
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D07","D14","D22","D30")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

#creating aggregated columns for deep, surf, stra
deep_columns <- select(learning2014, one_of(deep_questions))
learning2014$deep <- rowMeans(deep_columns)

surface_columns <- select(learning2014, one_of(surface_questions))
learning2014$surf <- rowMeans(surface_columns)

strategic_columns <- select(learning2014, one_of(strategic_questions))
learning2014$stra <- rowMeans(strategic_columns)

#select relevant columns
keep_columns <- c("gender", "Age", "Attitude", "deep", "stra", "surf", "Points")
learning2014a <- select(learning2014, one_of(keep_columns))

#change column names
colnames(learning2014a)[2] <- "age"
colnames(learning2014a)[3] <- "attitude"
colnames(learning2014a)[7] <- "points"

#filter out observations with zero points and creating the dataset for analysis "learning2014b"
learning2014b <- filter(learning2014a, points > 0)

#change working directory
setwd("D:/PhD University of Helsinki/Open Data Science/IODS-project/Data")

#save the analysis dataset to the 'data' folder
write.table(learning2014b, file = "learning2014.txt", sep = ",", row.names = FALSE)

#read the data again
learning2014b <- read.table("D:/PhD University of Helsinki/Open Data Science/IODS-project/Data/learning2014.txt", header = TRUE, sep = ",")

#check structure and headers
str(learning2014b)
head(learning2014b)
