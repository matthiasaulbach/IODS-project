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
colnames(hd) <- c ("HDI_rank", "Country", "HDI", "life_exp", "edu_exp", "edu_years", "GNI", "GNI_rank")
colnames(gii) <- c ("GII_rank", "Country", "gender_ineq", "mat_mor", "adol_bir", "repr_fem", "sec_ed_fem", "sec_ed_mal", "lab_fem", "lab_mal")
colnames (gii)

# New gender inequality variables
edu2F <- gii$sec_ed_fem / gii$sec_ed_mal
labF <- gii$lab_fem / gii$lab_mal
summary(edu2F)
gii["edu2F"] <- edu2F
gii["labF"] <- labF

# join datasets
human <- merge (hd, gii, by = "Country")
str(human)
dim(human)
colnames(human)

write.table(human, file = "human")
