# Load required libraries
library(klaR)
library(caret)

script_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_directory)

# Load your data (replace "your_file_path.csv" with the actual path to your CSV file)
house <- read.csv("house_data.csv")

# Assuming you want to create four pricing categories: Low, Medium, High, Very High
# You may need to adjust the breaks based on your data distribution
house$price_category <- cut(house$price,
                            breaks = c(-Inf, quantile(house$price, 0.25),
                                       quantile(house$price, 0.5),
                                       quantile(house$price, 0.75), Inf),
                            labels = c("Low", "Medium", "High", "Very High"),
                            include.lowest = TRUE)

# Convert date variable to Date
house$date <- as.Date(house$date, format = "%Y%m%dT%H%M%S")

# Convert the 'price_category' column to a factor
house$price_category <- as.factor(house$price_category)

# Set a seed for reproducibility
set.seed(123)

# Split the data into training and testing sets (80% training, 20% testing)
splitIndex <- createDataPartition(house$price_category, p = 0.80, list = FALSE)
train_data <- house[splitIndex, ]
test_data <- house[-splitIndex, ]

# Build the Naive Bayes model using the NaiveBayes function with laplace smoothing
nb_model <- NaiveBayes(price_category ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors +
                         waterfront + view + condition + grade + sqft_above + sqft_basement +
                         yr_built + yr_renovated,
                       data = train_data, laplace = .1)  # You can experiment with different values for laplace

# Make predictions on the test data

predictions <- predict(nb_model, newdata = test_data)

tab_nb <- table(predictions$class, test_data$price_category)
caret::confusionMatrix(tab_nb)  


