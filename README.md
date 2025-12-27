# OpenAI Fraud Detection Analysis System

## 📋 Overview
This system is an end-to-end R-based pipeline designed to simulate, analyze, and detect fraudulent patterns in AI API usage. [cite_start]It generates a synthetic dataset of 10,000 records, performs Exploratory Data Analysis (EDA), and trains Machine Learning models to identify various types of abuse.



## 🚀 Features
* [cite_start]**Synthetic Data Generation**: Creates 10,000 records with features like API call frequency, token usage, unique IPs, and geographic risk factors.
* [cite_start]**Risk Scoring Engine**: Implements a weighted heuristic to label fraud based on behavioral thresholds (e.g., failed auth attempts, rate limit violations).
* [cite_start]**Exploratory Data Analysis**: Visualizes fraud distribution, risk score frequency, and usage patterns using `ggplot2`.
* **Machine Learning Models**:
    * [cite_start]**Random Forest**: A high-accuracy ensemble model for robust detection.
    * [cite_start]**Decision Tree**: An interpretable model using `rpart` to understand the logic behind fraud flags.
* [cite_start]**Real-Time Detection Function**: A utility function to input live metrics and receive an immediate risk assessment (Low, Medium, or High Risk).

## 🛠 Tech Stack
* [cite_start]**Language**: R 
* **Core Libraries**: 
    * [cite_start]`tidyverse` & `lubridate`: Data manipulation and temporal analysis.
    * [cite_start]`caret`, `randomForest`, `rpart`: Machine learning and model evaluation.
    * [cite_start]`ggplot2`, `plotly`, `corrplot`: Advanced data visualization.

## 📁 Project Structure
The script is organized into 10 logical sections:
1.  [cite_start]**Data Generation**: Building the synthetic user base.
2.  [cite_start]**Labeling**: Creating fraud categories based on risk factors.
3.  [cite_start]**Summary Statistics**: Initial dataset audit.
4.  [cite_start]**Visualization**: Plotting usage patterns and risk scores.
5.  [cite_start]**Correlation**: Mapping feature relationships.
6.  [cite_start]**Modeling**: Splitting data (70/30) and training Random Forest.
7.  [cite_start]**Comparison**: Evaluating models against Decision Trees.
8.  [cite_start]**Real-Time Logic**: The `detect_fraud` function.
9.  [cite_start]**Usage Examples**: Testing legitimate vs. high-risk scenarios.
10. [cite_start]**Persistence**: Saving results to `.rds` and `.csv` files.

## 📊 Fraud Types Detected
The system classifies fraudulent behavior into specific categories:
* [cite_start]**API Abuse**: Excessive calls per hour.
* [cite_start]**Account Takeover**: Rapid changes in location and high number of unique IPs.
* [cite_start]**Credential Stuffing**: Repeated failed authentication attempts.
* [cite_start]**Content Fraud**: High suspicious keyword counts and content similarity.
* [cite_start]**Payment Fraud**: Frequent changes to payment methods.



## 🚦 Getting Started

### Prerequisites
Install the required R packages:
```r
install.packages(c("tidyverse", "caret", "randomForest", "e1071", "ggplot2", 
                   "plotly", "lubridate", "scales", "corrplot", "rpart", "rpart.plot"))
