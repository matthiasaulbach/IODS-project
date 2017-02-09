# Matthias Aulbach, 07.02.2017
# This is the exercise for week 3 - logistic regression
# The data for this exercise comes from https://archive.ics.uci.edu/ml/machine-learning-databases/00356/

# reading the data
student_mat <- read.csv("D:/PhD University of Helsinki/Open Data Science/IODS-project/Data/student-mat.csv", sep = ";", header = TRUE)
student_por <- read.csv("D:/PhD University of Helsinki/Open Data Science/IODS-project/Data/student-por.csv", sep = ";", header = TRUE)

# exploring the structure and dimensions of the data
str(student_mat)
dim(student_mat)

str(student_por)
dim(student_mat)

# Joining the datasets
library(dplyr)
join_by <- c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery","internet")
mat_por <- inner_join(student_mat, student_por, by = join_by)

str(mat_por)
dim(mat_por)

# combining duplicated answers in the data
alc <- select(mat_por, one_of("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery","internet"))

notjoined_columns <- colnames(student_mat)[!colnames(student_mat)%in%join_by]

for (column_name in notjoined_columns){
  two_columns <- select(mat_por, starts_with(column_name))
  first_column <- select(two_columns, 1)[[1]]
  
  if (is.numeric(first_column)){
    alc [column_name] <- round(rowMeans(two_columns))
  }  else 
      alc [column_name] <- first_column
  }

# column alc_use
alc <- mutate(alc, alc_use = (Dalc + Walc)/2)
alc <- mutate(alc, high_use = alc_use > 2)


glimpse(alc)

setwd("D:/PhD University of Helsinki/Open Data Science/IODS-project/Data")
write.csv(alc, file = "alc.csv")
