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

set.seed(123)

# Use the new price_category variable in the data partitioning
training_data <- createDataPartition(house$price_category, p = 0.80, list = FALSE)

house.trn <- house[training_data, ]
house.tst <- house[-training_data, ]

# Specify the features (independent variables) you want to use for classification
features <- c("bedrooms", "bathrooms", "sqft_living", "sqft_lot", "floors", "waterfront", "view", "condition", "grade", "sqft_above", "sqft_basement", "yr_built", "yr_renovated")

ctrl <- trainControl(method = "cv", number = 10)

# Introduce a complexity parameter (adjust the value as needed)
# ... (previous code remains unchanged)

# Introduce a smaller complexity parameter (adjust the value as needed)
complexity_values <- seq(0.01, 0.1, by = 0.001)  # Adjust the range and step size
tuneGrid <- expand.grid(cp = complexity_values)

# Specify the new response variable (price_category) in the formula
fit.cv <- train(price_category ~ ., data = house.trn[, c(features, "price_category")], method = "rf",
                trControl = ctrl, tuneGrid = tuneGrid)

# ... (rest of the code remains unchanged)

# Predict on the test set
pred <- predict(fit.cv, house.tst[, features])

# Evaluate the confusion matrix
confusionMatrix(table(house.tst$price_category, pred))

par(mfrow = c(1, 2), cex = 0.7)
par(mfrow = c(1, 1), cex = 0.7, mar = c(5, 10, 4, 2) + 0.1)

print(fit.cv)
plot(fit.cv)
plot(fit.cv$finalModel)
text(fit.cv$finalModel)
