# Load required libraries
library(rpart)          # For decision tree
library(rpart.plot)     # For enhanced visualization
library(ggplot2)        # For data exploration

# ============================================
# 1. BASIC DECISION TREE
# ============================================

# Load iris dataset (already in R)
data(iris)

# Explore the data structure
head(iris)
str(iris)
summary(iris)

# Split data into training (70%) and testing (30%)
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(iris), 0.7 * nrow(iris))
train_data <- iris[train_indices, ]
test_data <- iris[-train_indices, ]

# Build the decision tree model
# Predicting Species using all other variables
tree_model <- rpart(Species ~ ., 
                    data = train_data, 
                    method = "class",      # Classification tree
                    control = rpart.control(
                      minsplit = 5,         # Minimum observations in a node to split
                      minbucket = 2,        # Minimum observations in a terminal node
                      cp = 0.01             # Complexity parameter
                    ))

# Print the model details
print(tree_model)
summary(tree_model)

# Make predictions on test data
predictions <- predict(tree_model, test_data, type = "class")

# -------------------------------------------------------------------------


# Evaluate model accuracy
confusion_matrix <- table(Predicted = predictions, Actual = test_data$Species)
print("Confusion Matrix:")
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# ============================================
# 2. VISUALIZE THE TREE
# ============================================

# Method 1: Basic rpart.plot visualization (recommended)
par(mfrow = c(1, 1))  # Reset plot layout
rpart.plot(tree_model, 
           type = 1,           # Type of plot (1 = show all nodes)
           extra = 1,          # Display number of observations
           under = TRUE,       # Put extra info under node labels
           fallen.leaves = TRUE, # Align leaves at bottom
           main = "Decision Tree for Iris Species Classification")

# Method 2: Detailed visualization with percentages
rpart.plot(tree_model, 
           type = 4,           # Type 4 = show probabilities
           extra = 101,        # Show percentages and counts
           box.palette = "GnBu", # Color palette
           branch.lty = 3,     # Branch line style
           shadow.col = "gray", # Shadow under boxes
           nn = TRUE,          # Display node numbers
           main = "Iris Decision Tree - Detailed View")

# Method 3: Simple text-based tree
print("Text-based tree representation:")
print(tree_model)

# ============================================
# 3. USER INPUT PREDICTION - MULTIPLE METHODS
# ============================================

cat("\n", rep("=", 60), "\n")
cat("IRIS SPECIES PREDICTION SYSTEM")
cat("\n", rep("=", 60), "\n\n")

# METHOD 1: Interactive console input function
predict_from_user_input <- function() {
  cat("METHOD 1: Manual Input\n")
  cat("----------------------\n")
  
  # Get user input with validation
  sepal_length <- as.numeric(readline(prompt = "Enter Sepal Length (cm) [4.0-8.0]: "))
  sepal_width <- as.numeric(readline(prompt = "Enter Sepal Width (cm) [2.0-4.5]: "))
  petal_length <- as.numeric(readline(prompt = "Enter Petal Length (cm) [1.0-7.0]: "))
  petal_width <- as.numeric(readline(prompt = "Enter Petal Width (cm) [0.1-2.5]: "))
  
  # Create data frame with user input
  user_data <- data.frame(
    Sepal.Length = sepal_length,
    Sepal.Width = sepal_width,
    Petal.Length = petal_length,
    Petal.Width = petal_width
  )
  
  # Make prediction
  prediction <- predict(tree_model, user_data, type = "class")
  probabilities <- predict(tree_model, user_data, type = "prob")
  
  # Display results
  cat("\n", rep("-", 40), "\n")
  cat("PREDICTION RESULTS:\n")
  cat(rep("-", 40), "\n")
  cat("Input measurements:\n")
  cat(sprintf("  • Sepal Length: %.2f cm\n", sepal_length))
  cat(sprintf("  • Sepal Width:  %.2f cm\n", sepal_width))
  cat(sprintf("  • Petal Length: %.2f cm\n", petal_length))
  cat(sprintf("  • Petal Width:  %.2f cm\n", petal_width))
  cat("\nPredicted Species:", as.character(prediction), "\n")
  cat("\nPrediction Probabilities:\n")
  cat(sprintf("  • setosa:     %.2f%%\n", probabilities[1] * 100))
  cat(sprintf("  • versicolor: %.2f%%\n", probabilities[2] * 100))
  cat(sprintf("  • virginica:  %.2f%%\n", probabilities[3] * 100))
  
  return(list(prediction = prediction, probabilities = probabilities))
}

# METHOD 2: Pre-defined test cases function
predict_multiple_cases <- function() {
  cat("\n", rep("=", 60), "\n")
  cat("METHOD 2: Batch Prediction from Test Cases\n")
  cat(rep("=", 60), "\n\n")
  
  # Create test cases representing different species
  test_cases <- data.frame(
    Sepal.Length = c(5.1, 6.5, 7.2, 4.8, 6.0),
    Sepal.Width = c(3.5, 3.0, 3.2, 3.1, 2.7),
    Petal.Length = c(1.4, 4.5, 6.0, 1.5, 4.9),
    Petal.Width = c(0.2, 1.5, 2.0, 0.2, 1.7),
    Description = c("Typical Setosa", "Typical Versicolor", "Typical Virginica", 
                    "Small Setosa", "Intermediate")
  )
  
  # Make predictions for all test cases
  test_cases$Prediction <- predict(tree_model, test_cases[,1:4], type = "class")
  
  # Add probability columns
  probs <- predict(tree_model, test_cases[,1:4], type = "prob")
  test_cases$Setosa_Prob <- round(probs[,1] * 100, 1)
  test_cases$Versicolor_Prob <- round(probs[,2] * 100, 1)
  test_cases$Virginica_Prob <- round(probs[,3] * 100, 1)
  
  # Display results
  print(test_cases[, c("Description", "Sepal.Length", "Sepal.Width", 
                       "Petal.Length", "Petal.Width", "Prediction", 
                       "Setosa_Prob", "Versicolor_Prob", "Virginica_Prob")])
  
  return(test_cases)
}

# METHOD 3: Single value prediction function (for programming use)
predict_species <- function(sepal_length, sepal_width, petal_length, petal_width) {
  new_data <- data.frame(
    Sepal.Length = sepal_length,
    Sepal.Width = sepal_width,
    Petal.Length = petal_length,
    Petal.Width = petal_width
  )
  prediction <- predict(tree_model, new_data, type = "class")
  probabilities <- predict(tree_model, new_data, type = "prob")
  
  result <- list(
    species = as.character(prediction),
    probabilities = setNames(as.vector(probabilities), c("setosa", "versicolor", "virginica"))
  )
  
  return(result)
}

# METHOD 4: Interactive menu system
interactive_prediction_menu <- function() {
  repeat {
    cat("\n", rep("=", 60), "\n")
    cat("INTERACTIVE PREDICTION MENU")
    cat("\n", rep("=", 60), "\n")
    cat("1. Make a single prediction (manual input)\n")
    cat("2. Run test cases\n")
    cat("3. Predict custom values programmatically\n")
    cat("4. Visualize the decision tree again\n")
    cat("5. Exit\n")
    cat(rep("-", 60), "\n")
    
    choice <- as.numeric(readline(prompt = "Enter your choice (1-5): "))
    
    if (choice == 1) {
      predict_from_user_input()
    } else if (choice == 2) {
      predict_multiple_cases()
    } else if (choice == 3) {
      cat("\nEnter custom values for prediction:\n")
      sl <- as.numeric(readline(prompt = "Sepal Length: "))
      sw <- as.numeric(readline(prompt = "Sepal Width: "))
      pl <- as.numeric(readline(prompt = "Petal Length: "))
      pw <- as.numeric(readline(prompt = "Petal Width: "))
      
      result <- predict_species(sl, sw, pl, pw)
      cat("\nPredicted Species:", result$species, "\n")
      cat("Probabilities:\n")
      cat(sprintf("  setosa: %.2f%%\n", result$probabilities["setosa"] * 100))
      cat(sprintf("  versicolor: %.2f%%\n", result$probabilities["versicolor"] * 100))
      cat(sprintf("  virginica: %.2f%%\n", result$probabilities["virginica"] * 100))
      
    } else if (choice == 4) {
      rpart.plot(tree_model, type = 4, extra = 101, 
                 main = "Decision Tree - Ready for Predictions")
    } else if (choice == 5) {
      cat("Exiting... Goodbye!\n")
      break
    } else {
      cat("Invalid choice. Please try again.\n")
    }
    
    if (choice != 5) {
      readline(prompt = "\nPress [Enter] to continue...")
    }
  }
}




--------------------------------------------------------------------------------------------------------------------------------
# ============================================
# 4. DEMONSTRATION AND EXAMPLES
# ============================================

# Example 1: Single prediction using function
cat("\n", rep("=", 60), "\n")
cat("EXAMPLE PREDICTIONS")
cat("\n", rep("=", 60), "\n\n")

# Test with known species characteristics
example1 <- predict_species(5.1, 3.5, 1.4, 0.2)  # Should be setosa
cat("Example 1: (5.1, 3.5, 1.4, 0.2)")
cat("\n  → Predicted:", example1$species, "\n")

example2 <- predict_species(6.3, 2.8, 4.9, 1.8)  # Should be virginica
cat("Example 2: (6.3, 2.8, 4.9, 1.8)")
cat("\n  → Predicted:", example2$species, "\n")

example3 <- predict_species(5.9, 3.0, 4.2, 1.3)  # Should be versicolor
cat("Example 3: (5.9, 3.0, 4.2, 1.3)")
cat("\n  → Predicted:", example3$species, "\n")

# ============================================
# 5. VISUAL COMPARISON OF PREDICTIONS
# ============================================

# Create a visualization showing the decision boundaries
cat("\nCreating decision boundary visualization...\n")

# Generate grid of values for Petal.Length and Petal.Width (most important features)
petal_length_grid <- seq(min(iris$Petal.Length), max(iris$Petal.Length), length.out = 100)
petal_width_grid <- seq(min(iris$Petal.Width), max(iris$Petal.Width), length.out = 100)
grid <- expand.grid(Petal.Length = petal_length_grid, 
                    Petal.Width = petal_width_grid,
                    Sepal.Length = mean(iris$Sepal.Length),
                    Sepal.Width = mean(iris$Sepal.Width))

# Predict on grid
grid$Prediction <- predict(tree_model, grid, type = "class")

# Plot decision boundaries
p <- ggplot() +
  geom_tile(data = grid, aes(x = Petal.Length, y = Petal.Width, fill = Prediction), alpha = 0.6) +
  geom_point(data = iris, aes(x = Petal.Length, y = Petal.Width, color = Species), size = 3) +
  scale_fill_manual(values = c("setosa" = "lightpink", 
                               "versicolor" = "lightgreen", 
                               "virginica" = "lightblue")) +
  scale_color_manual(values = c("setosa" = "red", 
                                "versicolor" = "darkgreen", 
                                "virginica" = "darkblue")) +
  labs(title = "Decision Tree Decision Boundaries",
       subtitle = "Based on Petal Length and Petal Width (most important features)",
       x = "Petal Length (cm)",
       y = "Petal Width (cm)",
       fill = "Predicted Region",
       color = "Actual Species") +
  theme_minimal()

print(p)








#------------------------------------------------------------------------------------------------

# ============================================
# 6. START INTERACTIVE SESSION
# ============================================

cat("\n", rep("=", 60), "\n")
cat("READY FOR USER PREDICTIONS")
cat("\n", rep("=", 60), "\n")
cat("\nThe decision tree model has been trained on", nrow(train_data), "iris samples.\n")
cat("Model accuracy:", round(accuracy * 100, 2), "%\n\n")

# Uncomment the line below to start interactive menu
# interactive_prediction_menu()

# Or use simple single prediction function directly
cat("Quick prediction example:\n")
user_result <- predict_species(6.0, 2.9, 4.8, 1.4)
cat(sprintf("Input: (6.0, 2.9, 4.8, 1.4) → Predicted: %s (%.1f%% confidence)\n", 
            user_result$species, 
            max(user_result$probabilities) * 100))

# ============================================
# FUNCTION TO SAVE MODEL FOR FUTURE USE
# ============================================

save_model_for_future <- function() {
  # Save the model to a file
  saveRDS(tree_model, file = "iris_decision_tree_model.rds")
  cat("\nModel saved as 'iris_decision_tree_model.rds'\n")
  cat("To load it later: model <- readRDS('iris_decision_tree_model.rds')\n")
}

# Uncomment to save the model
# save_model_for_future()
