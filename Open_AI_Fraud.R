# ============================================================================
# OPENAI FRAUD DETECTION ANALYSIS SYSTEM
# Analyzes patterns of AI-related fraud and detects suspicious activities
# ============================================================================

# Install required packages (run once)
install.packages(c("tidyverse", "caret", "randomForest", "e1071", "ggplot2", "plotly", "lubridate", "scales",  "corrplot", "rpart", "rpart.plot"))

# Load libraries
library(tidyverse)
library(caret)
library(randomForest)
library(e1071)
library(ggplot2)
library(plotly)
library(lubridate)
library(scales)
library(corrplot)
library(rpart)
library(rpart.plot)

# ============================================================================
# 1. GENERATE SYNTHETIC FRAUD DATASET
# ============================================================================

set.seed(123)

# Generate 10,000 API usage records
n_records <- 10000

fraud_data <- data.frame(
  # User Identifiers
  user_id = paste0("USER_", sprintf("%05d", 1:n_records)),
  
  # Temporal Features
  timestamp = seq(as.POSIXct("2024-01-01"), by = "5 min", length.out = n_records),
  hour_of_day = sample(0:23, n_records, replace = TRUE),
  day_of_week = sample(1:7, n_records, replace = TRUE),
  
  # API Usage Patterns
  api_calls_per_hour = rpois(n_records, lambda = 50),
  tokens_used = rnorm(n_records, mean = 1000, sd = 500),
  unique_ips_used = sample(1:10, n_records, replace = TRUE),
  failed_auth_attempts = rpois(n_records, lambda = 1),
  
  # Content Patterns
  suspicious_keywords_count = rpois(n_records, lambda = 2),
  content_similarity_score = runif(n_records, 0, 1),
  avg_response_length = rnorm(n_records, mean = 500, sd = 200),
  
  # User Behavior
  account_age_days = sample(1:365, n_records, replace = TRUE),
  previous_violations = rpois(n_records, lambda = 0.5),
  payment_method_changes = sample(0:5, n_records, replace = TRUE),
  
  # Geographic
  location_changes = sample(0:10, n_records, replace = TRUE),
  high_risk_country = sample(c(0, 1), n_records, replace = TRUE, prob = c(0.85, 0.15)),
  
  # Technical
  user_agent_changes = sample(0:5, n_records, replace = TRUE),
  api_key_regenerations = sample(0:3, n_records, replace = TRUE),
  rate_limit_violations = rpois(n_records, lambda = 0.3)
)

# ============================================================================
# 2. CREATE FRAUD LABELS BASED ON RISK FACTORS
# ============================================================================

fraud_data <- fraud_data %>%
  mutate(
    # Calculate fraud risk score
    risk_score = 
      (api_calls_per_hour > 100) * 15 +
      (unique_ips_used > 5) * 20 +
      (failed_auth_attempts > 3) * 25 +
      (suspicious_keywords_count > 5) * 30 +
      (content_similarity_score > 0.8) * 20 +
      (location_changes > 5) * 15 +
      (high_risk_country == 1) * 10 +
      (payment_method_changes > 2) * 20 +
      (rate_limit_violations > 2) * 25 +
      (account_age_days < 7) * 15,
    
    # Label as fraud if risk score exceeds threshold
    is_fraud = ifelse(risk_score > 80, 1, 0),
    
    # Fraud type classification
    fraud_type = case_when(
      is_fraud == 0 ~ "Legitimate",
      suspicious_keywords_count > 5 & content_similarity_score > 0.7 ~ "Content Fraud",
      unique_ips_used > 6 & location_changes > 5 ~ "Account Takeover",
      api_calls_per_hour > 120 ~ "API Abuse",
      failed_auth_attempts > 4 ~ "Credential Stuffing",
      payment_method_changes > 3 ~ "Payment Fraud",
      TRUE ~ "Suspicious Activity"
    )
  )

# Convert to factors
fraud_data$is_fraud <- as.factor(fraud_data$is_fraud)
fraud_data$fraud_type <- as.factor(fraud_data$fraud_type)

# ============================================================================
# 3. EXPLORATORY DATA ANALYSIS
# ============================================================================

cat("\n=== FRAUD DETECTION DATASET SUMMARY ===\n")
cat(sprintf("Total Records: %d\n", nrow(fraud_data)))
cat(sprintf("Fraud Cases: %d (%.2f%%)\n", 
            sum(fraud_data$is_fraud == 1), 
            mean(fraud_data$is_fraud == 1) * 100))
cat(sprintf("Legitimate Cases: %d (%.2f%%)\n", 
            sum(fraud_data$is_fraud == 0), 
            mean(fraud_data$is_fraud == 0) * 100))

cat("\n=== FRAUD TYPE DISTRIBUTION ===\n")
print(table(fraud_data$fraud_type))

# ============================================================================
# 4. VISUALIZATION: FRAUD PATTERNS
# ============================================================================

# Plot 1: Fraud Distribution by Type
p1 <- ggplot(fraud_data, aes(x = fraud_type, fill = fraud_type)) +
  geom_bar() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Distribution of Fraud Types",
       x = "Fraud Type",
       y = "Count") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14))

print(p1)

# Plot 2: API Calls vs Tokens Used (Fraud Detection)
p2 <- ggplot(fraud_data, aes(x = api_calls_per_hour, y = tokens_used, 
                             color = is_fraud)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("0" = "green", "1" = "red"),
                     labels = c("Legitimate", "Fraud")) +
  theme_minimal() +
  labs(title = "API Usage Pattern: Fraud vs Legitimate",
       x = "API Calls per Hour",
       y = "Tokens Used",
       color = "Status") +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p2)

# Plot 3: Risk Score Distribution
p3 <- ggplot(fraud_data, aes(x = risk_score, fill = is_fraud)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("0" = "green", "1" = "red"),
                    labels = c("Legitimate", "Fraud")) +
  theme_minimal() +
  labs(title = "Risk Score Distribution",
       x = "Risk Score",
       y = "Frequency",
       fill = "Status") +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p3)

# Plot 4: Fraud by Hour of Day
fraud_by_hour <- fraud_data %>%
  group_by(hour_of_day, is_fraud) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(hour_of_day) %>%
  mutate(fraud_rate = count / sum(count) * 100) %>%
  filter(is_fraud == 1)

p4 <- ggplot(fraud_by_hour, aes(x = hour_of_day, y = fraud_rate)) +
  geom_line(color = "red", size = 1.2) +
  geom_point(color = "darkred", size = 3) +
  theme_minimal() +
  labs(title = "Fraud Rate by Hour of Day",
       x = "Hour (24-hour format)",
       y = "Fraud Rate (%)") +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p4)

# ============================================================================
# 5. CORRELATION ANALYSIS
# ============================================================================

# Select numeric features for correlation
numeric_features <- fraud_data %>%
  select(api_calls_per_hour, tokens_used, unique_ips_used, 
         failed_auth_attempts, suspicious_keywords_count,
         content_similarity_score, location_changes,
         rate_limit_violations, risk_score) %>%
  as.matrix()

# Correlation matrix
cor_matrix <- cor(numeric_features, use = "complete.obs")

# Plot correlation matrix
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         title = "Feature Correlation Matrix",
         mar = c(0, 0, 2, 0))

# ============================================================================
# 6. MACHINE LEARNING: FRAUD DETECTION MODEL
# ============================================================================

cat("\n=== BUILDING FRAUD DETECTION MODEL ===\n")

# Prepare data for modeling
model_data <- fraud_data %>%
  select(-user_id, -timestamp, -fraud_type, -risk_score)

# Split data: 70% training, 30% testing
set.seed(456)
train_index <- createDataPartition(model_data$is_fraud, p = 0.7, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

cat(sprintf("Training Set: %d records\n", nrow(train_data)))
cat(sprintf("Testing Set: %d records\n", nrow(test_data)))

# ============================================================================
# MODEL 1: RANDOM FOREST
# ============================================================================

cat("\n--- Training Random Forest Model ---\n")

rf_model <- randomForest(
  is_fraud ~ .,
  data = train_data,
  ntree = 100,
  importance = TRUE
)

# Predictions
rf_predictions <- predict(rf_model, test_data)
rf_probs <- predict(rf_model, test_data, type = "prob")[, 2]

# Confusion Matrix
rf_cm <- confusionMatrix(rf_predictions, test_data$is_fraud, positive = "1")
print(rf_cm)

# Feature Importance
cat("\n--- Feature Importance (Random Forest) ---\n")
importance_df <- data.frame(
  Feature = rownames(importance(rf_model)),
  Importance = importance(rf_model)[, "MeanDecreaseGini"]
) %>%
  arrange(desc(Importance))

print(head(importance_df, 10))

# Plot Feature Importance
p5 <- ggplot(head(importance_df, 10), 
             aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Features for Fraud Detection",
       x = "Feature",
       y = "Importance (Gini)") +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p5)

# ============================================================================
# MODEL 2: DECISION TREE (INTERPRETABLE)
# ============================================================================

cat("\n--- Training Decision Tree Model ---\n")

dt_model <- rpart(
  is_fraud ~ .,
  data = train_data,
  method = "class",
  control = rpart.control(cp = 0.01, maxdepth = 5)
)

# Plot decision tree
rpart.plot(dt_model, 
           main = "Fraud Detection Decision Tree",
           extra = 104,
           box.palette = "RdYlGn",
           shadow.col = "gray",
           nn = TRUE)

# Predictions
dt_predictions <- predict(dt_model, test_data, type = "class")
dt_cm <- confusionMatrix(dt_predictions, test_data$is_fraud, positive = "1")

cat("\n--- Decision Tree Performance ---\n")
print(dt_cm)

# ============================================================================
# 7. MODEL PERFORMANCE COMPARISON
# ============================================================================

cat("\n=== MODEL PERFORMANCE COMPARISON ===\n")

performance_comparison <- data.frame(
  Model = c("Random Forest", "Decision Tree"),
  Accuracy = c(rf_cm$overall["Accuracy"], dt_cm$overall["Accuracy"]),
  Precision = c(rf_cm$byClass["Precision"], dt_cm$byClass["Precision"]),
  Recall = c(rf_cm$byClass["Recall"], dt_cm$byClass["Recall"]),
  F1_Score = c(rf_cm$byClass["F1"], dt_cm$byClass["F1"])
)

print(performance_comparison)

# Plot comparison
perf_long <- performance_comparison %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")

p6 <- ggplot(perf_long, aes(x = Metric, y = Value, fill = Model)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal() +
  labs(title = "Model Performance Comparison",
       x = "Performance Metric",
       y = "Score") +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p6)

# ============================================================================
# 8. REAL-TIME FRAUD DETECTION FUNCTION
# ============================================================================

detect_fraud <- function(api_calls, tokens, unique_ips, failed_auth,
                         suspicious_keywords, similarity_score,
                         location_changes, high_risk_country,
                         rate_violations, account_age) {
  
  # Calculate risk score
  risk <- 
    (api_calls > 100) * 15 +
    (unique_ips > 5) * 20 +
    (failed_auth > 3) * 25 +
    (suspicious_keywords > 5) * 30 +
    (similarity_score > 0.8) * 20 +
    (location_changes > 5) * 15 +
    (high_risk_country == 1) * 10 +
    (rate_violations > 2) * 25 +
    (account_age < 7) * 15
  
  # Determine fraud status
  if (risk > 100) {
    status <- "HIGH RISK - FRAUD DETECTED"
    color <- "🔴"
  } else if (risk > 60) {
    status <- "MEDIUM RISK - SUSPICIOUS"
    color <- "🟡"
  } else {
    status <- "LOW RISK - LEGITIMATE"
    color <- "🟢"
  }
  
  cat(sprintf("\n%s FRAUD DETECTION RESULT %s\n", color, color))
  cat(sprintf("Risk Score: %d\n", risk))
  cat(sprintf("Status: %s\n", status))
  cat(sprintf("\nRisk Factors:\n"))
  cat(sprintf("  - API Calls/Hour: %d\n", api_calls))
  cat(sprintf("  - Unique IPs: %d\n", unique_ips))
  cat(sprintf("  - Failed Auth: %d\n", failed_auth))
  cat(sprintf("  - Suspicious Keywords: %d\n", suspicious_keywords))
  cat(sprintf("  - Content Similarity: %.2f\n", similarity_score))
  
  return(list(risk_score = risk, status = status))
}

# ============================================================================
# 9. EXAMPLE USAGE: DETECT FRAUD IN REAL-TIME
# ============================================================================

cat("\n=== REAL-TIME FRAUD DETECTION EXAMPLES ===\n")

# Example 1: Legitimate User
cat("\n--- Example 1: Legitimate User ---")
detect_fraud(
  api_calls = 45,
  tokens = 800,
  unique_ips = 2,
  failed_auth = 0,
  suspicious_keywords = 1,
  similarity_score = 0.3,
  location_changes = 1,
  high_risk_country = 0,
  rate_violations = 0,
  account_age = 120
)

# Example 2: Suspicious Activity
cat("\n--- Example 2: Suspicious Activity ---")
detect_fraud(
  api_calls = 95,
  tokens = 1500,
  unique_ips = 4,
  failed_auth = 2,
  suspicious_keywords = 4,
  similarity_score = 0.7,
  location_changes = 3,
  high_risk_country = 1,
  rate_violations = 1,
  account_age = 5
)

# Example 3: High Risk Fraud
cat("\n--- Example 3: High Risk Fraud ---")
detect_fraud(
  api_calls = 150,
  tokens = 2000,
  unique_ips = 8,
  failed_auth = 5,
  suspicious_keywords = 8,
  similarity_score = 0.95,
  location_changes = 7,
  high_risk_country = 1,
  rate_violations = 4,
  account_age = 2
)

# ============================================================================
# 10. SAVE RESULTS
# ============================================================================

cat("\n=== SAVING RESULTS ===\n")

# Save trained model
saveRDS(rf_model, "fraud_detection_model.rds")
cat("✓ Model saved: fraud_detection_model.rds\n")

# Save predictions
results <- data.frame(
  actual = test_data$is_fraud,
  predicted = rf_predictions,
  probability = rf_probs
)
write.csv(results, "fraud_predictions.csv", row.names = FALSE)
cat("✓ Predictions saved: fraud_predictions.csv\n")

# Save dataset
write.csv(fraud_data, "fraud_dataset.csv", row.names = FALSE)
cat("✓ Dataset saved: fraud_dataset.csv\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("All visualizations and models have been generated.\n")
cat("Check the plots and saved files for detailed results.\n")

