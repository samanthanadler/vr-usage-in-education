#1. box plot for age vs improvement
boxplot(Virtual_Reality_in_Education_Impact_right$Age ~ Virtual_Reality_in_Education_Impact_right$Improvement_in_Learning_Outcomes,
        data = Virtual_Reality_in_Education_Impact_right,
        main = "Age vs Improvement",
        xlab = "Improvement (Yes/No)",
        ylab = "Age",
        col = c("skyblue", "lightgreen"),
        border = "black")



#2. box plot for feedback vs Perceived effectiveness of VR
data <- Virtual_Reality_in_Education_Impact_right
# Convert Improvement in Learning Outcomes to a factor
data$Improvement_in_Learning_Outcomes <- as.factor(data$Improvement_in_Learning_Outcomes)

# Create a side-by-side box plot
boxplot(
  data$`Perceived_Effectiveness_of_VR` ~ data$`Feedback_from_Educators_on_VR`,
  xlab = "Feedback from Educators",
  ylab = "Perceived Effectiveness of VR",
  main = "Feedback vs Perceived Effectiveness of VR",
  col = c("lightblue", "lightgreen") # Optional: Color the boxes
)
_______________________________________________________________________
#3. T-tests for age vs improvement

# Convert "Improvement in Learning Outcomes" to a factor
data$Improvement_in_Learning_Outcomes <- as.factor(data$Improvement_in_Learning_Outcomes)

# Ensure the "Age" column is numeric
data$Age <- as.numeric(data$Age)

# Check the structure of the data
str(data)

# Perform a two-sample t-test
t_test_result <- t.test(
  Age ~ Improvement_in_Learning_Outcomes,
  data = data,
  var.equal = TRUE # Use TRUE if you assume equal variances, FALSE otherwise
)

# Print the results
print(t_test_result)

______________________________________________________
#4. table of p-values for feedback from educators
#Step 1: Load necessary libraries (if needed)
  install.packages("dplyr")   # For data manipulation
library(dplyr)

# Step 2: Filter data for each feedback group
negative <- data %>% filter(Feedback_from_Educators_on_VR == "Negative") %>% pull(Perceived_Effectiveness_of_VR)
neutral <- data %>% filter(Feedback_from_Educators_on_VR == "Neutral") %>% pull(Perceived_Effectiveness_of_VR)
positive <- data %>% filter(Feedback_from_Educators_on_VR == "Positive") %>% pull(Perceived_Effectiveness_of_VR)

# Step 3: Perform pairwise t-tests and store p-values
p_neg_neu <- t.test(negative, neutral, var.equal = TRUE)$p.value
p_neu_pos <- t.test(neutral, positive, var.equal = TRUE)$p.value
p_pos_neg <- t.test(positive, negative, var.equal = TRUE)$p.value

# Step 4: Create a table of results
p_values_table <- data.frame(
  Comparison = c("Negative vs Neutral", "Neutral vs Positive", "Positive vs Negative"),
  P_Value = c(p_neg_neu, p_neu_pos, p_pos_neg)
)

# Print the table
print(p_values_table)
View(p_values_table)
_______________________________________________________________________
#5. Table of Proportions for all

data <- Virtual_Reality_in_Education_Impact_right
# Create the contingency table
result_table <- table(data$Improvement_in_Learning_Outcomes, data$Field_of_Study)

# Convert to a data frame for better visualization
result_table_df <- as.data.frame.matrix(result_table)

# View the table
print(result_table_df)
# Convert the matrix to a data frame
prop_values_df <- as.data.frame(result_table_df)
# Display the p-values table in the Viewer pane
View(prop_values_df)

___________________________________________________________________
#6. Table of p values for all from proportions test

# Summarize data by Field_of_Study
summary_table <- table(data$Field_of_Study, data$Improvement_in_Learning_Outcomes)

# Convert table into counts
success_counts <- summary_table[, "No"] # Number of "Yes" (improvements)
total_counts <- rowSums(summary_table)   # Total observations per subject

# View summary
print(success_counts)
print(total_counts)

# Get all unique fields of study
fields <- rownames(summary_table)

# Create an empty matrix to store p-values
p_values <- matrix(NA, nrow = length(fields), ncol = length(fields),
                   dimnames = list(fields, fields))

# Loop through pairs of fields to perform prop.test()
for (i in 1:(length(fields) - 1)) {
  for (j in (i + 1):length(fields)) {
    # Extract data for the two groups being compared
    successes <- c(success_counts[fields[i]], success_counts[fields[j]])
    totals <- c(total_counts[fields[i]], total_counts[fields[j]])
    
    # Perform proportion test
    test <- prop.test(successes, totals)
    
    # Store the p-value in the matrix
    p_values[i, j] <- test$p.value
    p_values[j, i] <- test$p.value # Symmetric matrix
  }
}

# View p-value matrix
print(p_values)

# Convert the matrix to a data frame
p_values_df <- as.data.frame(p_values)
# Display the p-values table in the Viewer pane
View(p_values_df)