#read datasets
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

# structure and dimensions; summaries
str(hd)
dim(hd)
str(gii)
dim(gii)
summary(hd)
summary(gii)

# rename variables
colnames(hd) <- c ("HDI_rank", "Country", "HDI", "Life.Exp", "Edu.Exp", "edu_years", "GNI", "GNI_rank")
colnames(gii) <- c ("GII_rank", "Country", "gender_ineq", "Mat.Mor", "Ado.Birth", "Parli.F", "Edu2.F", "Edu2.M", "Labo.F", "Labo.M")
colnames (gii)

# New gender inequality variables
Edu2.FM <- gii$Edu2.F / gii$Edu2.M
Labo.FM <- gii$Labo.F / gii$Labo.M
summary(Edu2.FM)
gii["Edu2.FM"] <- Edu2.FM
gii["Labo.FM"] <- Labo.FM

# join datasets
human <- merge (hd, gii, by = "Country")
str(human)
dim(human)
colnames(human)

write.table(human, file = "human")

# Transforming GNI
library(stringr)
human <- mutate(human, GNI = str_replace(human$GNI, pattern = ",", replacement = "") %>% as.numeric(as.character))
str(human)
# Selecting necessary columns
library(dplyr)
keep <- c( "Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")
human <- select (human, one_of(keep))
dim(human)
# Remove rows with missing values
complete.cases(human)
human_ <- filter (human, complete.cases(human) == TRUE)
str(human_)

# Remove regions
human_$Country
human_[- c(4, 44, 50, 81, 132, 135, 159), ]
human_countries <- human_[- c(4, 44, 50, 81, 132, 135, 159), ]
str(human_countries)

# Use country names as column names
rownames(human_countries) <- human_countries$Country
human <- select(human_countries, - Country)
str(human)

write.table(human, file = "human")
