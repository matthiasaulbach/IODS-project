#Reading the data file into R
students2014 <- read.table("D:/PhD University of Helsinki/Open Data Science/IODS-project/Data/learning2014.txt", sep = ",", header = TRUE)
# Getting an overview
str(students2014)
dim(students2014)

# We have a dataset of 166 observations of 7 variables (gender, age, attitude, deep, stra, surf, points)
# The variables "deep", "stra", and "surf" have been aggregated over different items and describe different learning styles
# The variable attitude measures the attitude towards the course
# The variable points measure the total amount of points in the course

# accessing ggplot2
library(ggplot2)

# Plotting variables against each other with different genders in different colors
pairs (students2014[-1], col = students2014$gender)

# Summary of the data
summary(students2014)

# The sample consists of 110 women and 56 men. Age range is from 17 to 55, with a mean of 25.51.
# Attitude towards the course varied from 14 to 55 with a mean of 31.43.
# Points varied form 7 to 33 with a mean of 23 points.
# Of the three learning strategies, "deep" had the highest mean (3.8), followed by "stra" (3.1), "surf" (2.8).


# Regression model with "points" as the target variable and "age", "attitude", and "stra" as prdeictors.
my_model <- lm(points ~ age + attitude + stra, data = students2014)
summary (my_model)
# "age" had an estimate of -0.089, "attitude" of 0.348, and "stra" of 1.00. The estimated intercept was 10.9.
# only the intercept and attitude reached statistical significance.
# The adjusted R-square statistics for the whole model is 0.204, with F=15.1, highly significant.
# The model will be re-run with the only significant predictor, "attitude"
my_model2 <- lm(points ~ attitude, data=students2014)
summary (my_model2)
# The estimate of the intercept is highly significant at a value of 11.6.
# The estimate of attitude is highly significant at a value of 0.353.
# The adjusted R-square statistics for the whole model is 0.186, with F=38,6, highly significant.
# Attitude about the course has significant positive predicitve power for the points in the course.

# Plotting Residuals versus Fitted Values, Normal QQ-Plot, and Residuals vs Leverage
plot (my_model2, which = c(1, 2, 5))

# In the first plot we can see that the red line is close to 0, indicating that the fit of the model does not depend on the amount of points: it works for high and low amounts of points
# The Q-Q-Plot show us that for the highest and lowest quantiles (below -2 and above 2), the prediction is not accurate, indicating that a linear model might not fit the data best. However, for the majority of data points, the linear model fits well
# The residuals vs leverage graph shows that there are no outliers that heavily influence our regression model; the red line is rather flat.