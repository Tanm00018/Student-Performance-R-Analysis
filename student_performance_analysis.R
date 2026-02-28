# ----------------------------------------
# STUDENTS PERFORMANCE ANALYSIS
# ----------------------------------------

# 1. Import Dataset
data <- read.csv("StudentsPerformance.csv")

# 2. Overview
dim(data)
str(data)
summary(data)

# 3. Missing Value Check
colSums(is.na(data))
any(is.na(data))

# 4. Convert Character to Factor
data$gender <- as.factor(data$gender)
data$race.ethnicity <- as.factor(data$race.ethnicity)
data$parental.level.of.education <- as.factor(data$parental.level.of.education)
data$lunch <- as.factor(data$lunch)
data$test.preparation.course <- as.factor(data$test.preparation.course)

str(data)

# 5. Outlier Detection
boxplot(data$math.score, main="Math Score Distribution")
boxplot(data$reading.score, main="Reading Score Distribution")
boxplot(data$writing.score, main="Writing Score Distribution")

# 6. Feature Engineering
data$total_score <- data$math.score + data$reading.score + data$writing.score
data$average_score <- data$total_score / 3

summary(data$average_score)

# 7. Performance Band Creation
data$performance_band <- cut(data$average_score,
                             breaks = c(0,50,70,85,100),
                             labels = c("Poor","Average","Good","Excellent"),
                             include.lowest = TRUE)

table(data$performance_band)

# 8. Test Preparation Impact
aggregate(average_score ~ test.preparation.course, data, mean)

# 9. Correlation Analysis
cor(data[,c("math.score","reading.score","writing.score")])

