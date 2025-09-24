
# Install required packages (run only once)
# install.packages('caTools')
# install.packages('ggplot2')

# Load libraries
library(ggplot2)
library(caTools)

# Create dataset
data <- data.frame(
  YearsExperience = c(1.1, 1.3, 1.5, 10.3, 10.5, 2.0, 2.2, 2.9, 3.0, 3.2, 3.2, 3.7,
                      3.9, 4.0, 4.0, 4.1, 4.5, 4.9, 5.1, 5.3, 5.9, 6.0, 6.8, 7.1,
                      7.9, 8.2, 8.7, 9.0, 9.0, 9.5, 9.6),
  Salary = c(39343.00, 46205.00, 37731.00, 122391.00, 121872.00, 43525.00, 39891.00,
             56642.00, 60150.00, 54445.00, 64445.00, 57189.00, 63218.00, 55794.00,
             56957.00, 57081.00, 61111.00, 67938.00, 66029.00, 83088.00, 81363.00,
             93940.00, 91738.00, 98273.00, 101302.00, 113812.00, 109431.00, 105582.00,
             116969.00, 112635.00)
)

# Quick plot
plot(data$YearsExperience, data$Salary, 
     main = "Years of Experience vs Salary", 
     xlab = "Years of Experience", ylab = "Salary", pch = 19, col = "blue")

# Split dataset into training and test sets
set.seed(123) # for reproducibility
split <- sample.split(data$Salary, SplitRatio = 0.7)
trainingset <- subset(data, split == TRUE)
testset <- subset(data, split == FALSE)

# Train Linear Regression model
lm_r <- lm(formula = Salary ~ YearsExperience, data = trainingset)

# Model summary
summary(lm_r)

# Predict on test set
test_predictions <- predict(lm_r, newdata = testset)

# Predict on new values
new_data <- data.frame(YearsExperience = c(4.0, 4.5, 5.0))
predicted_salaries <- predict(lm_r, newdata = new_data)
print(predicted_salaries)

# Visualization using ggplot2
ggplot(data, aes(x = YearsExperience, y = Salary)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Linear Regression: Salary vs Experience") +
  xlab("Years of Experience") +
  ylab("Salary")
