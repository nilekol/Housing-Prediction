# Install and load necessary packages
library(rpart)
script_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)

setwd(script_directory)
# Load your data (replace "your_file_path.csv" with the actual path to your CSV file)
house_data <- read.csv("house_data.csv")

house_data$price_category <- cut(house_data$price,
                            breaks = c(-Inf, quantile(house_data$price, 0.25),
                                       quantile(house_data$price, 0.5),
                                       quantile(house_data$price, 0.75), Inf),
                            labels = c("Low", "Medium", "High", "Very High"),
                            include.lowest = TRUE)


# Set the seed for reproducibility
set.seed(123)

sampled_indices <- sample(1:nrow(house_data), 0.05 * nrow(house_data))
sampled_data <- house_data[sampled_indices, ]

# Split the data into training and testing sets (70% training, 30% testing)
train_indices <- sample(1:nrow(house_data), 0.7 * nrow(house_data))
train_data <- house_data[train_indices, ]
test_data <- house_data[-train_indices, ]

# Define the target variable (price_category) and predictor variables
target_variable <- "price_category"
predictor_variables <- colnames(house_data)[colnames(house_data) != target_variable]

# Train the decision tree model
decision_tree_model <- rpart(formula = as.formula(paste(target_variable, "~", paste(predictor_variables, collapse = "+"))),
                             data = train_data,
                             method = "class",  # "class" for classification
                             control = rpart.control(minsplit = 10, cp = 0.001))  # Adjust parameters as needed

# Visualize the decision tree (optional)
plot(decision_tree_model)
text(decision_tree_model, cex = 0.8)

# Make predictions on the test set
predictions <- predict(decision_tree_model, newdata = test_data, type = "class")

# Evaluate the model performance
confusion_matrix <- table(predictions, test_data$price_category)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy, "\n")
