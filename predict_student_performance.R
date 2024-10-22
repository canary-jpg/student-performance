library("tidyverse")
library("readr")
library("caret")
library("gridExtra")
library("metan")

#load data
StudentPerformanceFactors -> read_csv("StudentPerformanceFactors.csv")

#data preprocessing
glimpse(StudentPerformanceFactors)
head(StudentPerformanceFactors)
names(StudentPerformanceFactors)
summary(StudentPerformanceFactors)

#changing column names to lowercase
names(StudentPerformanceFactors) <- tolower(names(StudentPerformanceFactors))

#are there any null values
sum(is.na(StudentPerformanceFactors))

#deleting missing values
StudentPerformanceFactors <- na.omit(StudentPerformanceFactors)
sum(is.na(StudentPerformanceFactors))

#inspecting numeric columns
df_numeric <- StudentPerformanceFactors %>%
  select_if(negate(is.character))

#convert character columns to factors
StudentPerformanceFactors$family_income <- factor(StudentPerformanceFactors$family_income,
                                                  levels = c("Low", "Medium", "High"),
                                                  labels = c("Low", "Medium", "High"),
                                                  ordered = T)
StudentPerformanceFactors$parental_education_level <- factor(StudentPerformanceFactors$parental_education_level,
                                                             levels = c("High School", "College", "Postgraduate"),
                                                             labels = c("High School", "College", "Postgraduate"),
                                                             ordered = T)
StudentPerformanceFactors$gender <- as.factor(StudentPerformanceFactors$gender)


#EDA
p1 <- ggplot(StudentPerformanceFactors, aes(hours_studied, fill = gender)) +
  geom_bar() +
  theme_minimal() +
  facet_grid(gender ~ .) +
  labs(title = "Studied Hours per Week by Gender", x ="Studied Hours") +
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(StudentPerformanceFactors, aes(sleep_hours, fill = gender)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Average Hours of Sleep per Week by Gender", x = "Sleeping Hours") +
  theme(plot.title = element_text(hjust = 0.5))


p3 <- ggplot(StudentPerformanceFactors, aes(parental_education_level, fill = gender)) +
  geom_bar(position = position_dodge()) +
  theme_minimal() +
  labs(title = "Parental Education Level by Gender", x = "Parental Education level") +
  theme(plot.title = element_text(hjust = 0.5))


p4 <- ggplot(StudentPerformanceFactors, aes(family_income, fill = gender)) +
  geom_bar(position = position_dodge()) +
  theme_minimal() +
  labs(title = "Family Income by Gender", x = "Family Income") +
  theme(plot.title = element_text(hjust = 0.5))


p5 <- ggplot(StudentPerformanceFactors, aes(peer_influence, fill = gender)) +
  geom_bar(position = position_dodge()) +
  theme_minimal() +
  labs(title = "Peer Influence by Gender", x = "Level of Peer influence") +
  theme(plot.title = element_text(hjust = 0.5))


p6 <- ggplot(StudentPerformanceFactors, aes(exam_score, fill = gender)) +
  geom_histogram(color = "white") +
  theme_minimal() +
  facet_grid(gender ~ .) +
  labs(title = "Exam Scores by Gender", x = "Exam Scores") +
  theme(plot.title = element_text(hjust = 0.5))

#arranging plots in a grid
grid.arrange(p1, p2, ncol= 1)
grid.arrange(p3, p4, p5, p6, ncol = 2)


#correlation matrix
corr1 <- corr_coef(df_numeric)
plot(corr1)


#building ML models

set.seed(42)
n = nrow(StudentPerformanceFactors)
id <- sample(1:n, size = 0.8 *n, replace = FALSE)
train_data <- StudentPerformanceFactors[id, ]
test_data <- StudentPerformanceFactors[-id, ]

#Linear regression model
set.seed(42)
ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)
lm_model <- train(exam_score ~ previous_scores + hours_studied + attendance +
                    tutoring_sessions, data = train_data, method = "lm", 
                  trControl = ctrl)
print(lm_model)

p_lm <- predict(lm_model, newdata = test_data)
rmse_lm <- sqrt(mean( (p_lm - test_data$exam_score)**2))
print(rmse_lm)

#KNN with K-fold CV
set.seed(42)
ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)

knn_model <- train(exam_score ~ previous_scores + hours_studied + attendance +
                     tutoring_sessions, data = train_data, method = "knn", 
                   trControl = ctrl, tunelength = 4)
print(knn_model)

p_knn <- predict(knn_model, newdata = test_data)
rmse_knn <- sqrt(mean( (p_knn - test_data$exam_score) **2))
print(rmse_knn)

#random forest with K-fold CV
set.seed(42)
ctrl <- trainControl(method = "cv",
                     number = 5, 
                     verboseIter = TRUE)
rf_model <- train(exam_score ~ previous_scores + hours_studied + attendance +
                    tutoring_sessions, data = train_data, method = "rf",
                  trControl = ctrl)
print(rf_model)

p_rf <- predict(rf_model, newdata = test_data)
rmse_rf <- sqrt(mean( (p_rf - test_data$exam_score)**2 ))
print(rmse_rf)


#comparing models 
model_summary <- data.frame(
  model = c("Linear regression with K-Fold CV", "KNN with K-Fold CV", "Random Forest with K-Fold CV"),
  rmse_train = c("2.543691", "2.698024", "2.677984"),
  rmse_test = c(rmse_lm, rmse_knn, rmse_rf)
)

print(model_summary)
