script_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the script directory
setwd(script_directory)

house <- read.csv("house_data.csv")

# Assuming house is your data frame
prices <- house$price

# Create a histogram of housing prices
hist(prices, col = "lightblue", main = "Histogram of Housing Prices", xlab = "Price")

# Add labels and title
xlabel <- "Price"
ylabel <- "Frequency"
title <- "Histogram of Housing Prices"
hist_result <- hist(prices, col = "lightblue", main = title, xlab = xlabel, ylab = ylabel)

breaks <- hist_result$breaks
counts <- hist_result$counts

# Print breaks and counts
cat("Breaks:\n", breaks, "\n\n")


# Add a grid
grid()

# Assuming house is your data frame
# Assuming house is your data frame
grades <- table(house$grade)

# Create a barplot without text labels first
barplot(grades, main = "Bar Plot of Grades", xlab = "Grade", ylab = "Frequency", col = "skyblue")

# Use text() to add count labels on top of each bar

# Bar plot for conditions with counts on top
#barplot(conditions, main = "Bar Plot of Conditions", xlab = "Condition", ylab = "Frequency", col = "lightgreen")


