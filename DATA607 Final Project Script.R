# Install necessary packages
install.packages("COVID19")
install.packages("tidyverse")
install.packages("forecast")
install.packages("caret")  # For machine learning

# Load libraries
library(COVID19)
library(tidyverse)
library(forecast)
library(caret)
library(forcats)

# Extract worldwide COVID19 data by state
df <- covid19(level = 2)

# View the first few rows
head(df)

# Check for missing values
summary(df)

# Step 1: Impute missing values for "tests" with the mean
df$tests <- ifelse(is.na(df$tests), mean(df$tests, na.rm = TRUE), df$tests)

# Step 2: Remove rows where critical information is missing (e.g., cases, deaths)
df_clean <- df %>%
  filter(!is.na(confirmed), !is.na(deaths), !is.na(recovered))

# Step 3: Simplify the factor labels for administrative_area_level_2
df_clean <- df_clean %>%
  mutate(administrative_area_level_2 = fct_lump(administrative_area_level_2, n = 10))

# Verify the changes
unique(df_clean$administrative_area_level_2)

# Distribution of Confirmed Cased by Region
df_clean %>%
  filter(!is.na(administrative_area_level_2)) %>%
  ggplot(aes(x = administrative_area_level_2, y = confirmed)) +
  geom_boxplot() +
  labs(title = "Distribution of Confirmed Cases by Region",
       x = "Region",
       y = "Confirmed Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# COVID-19 Case Trends Over Time by Region
df_clean %>%
  filter(!is.na(administrative_area_level_2)) %>%
  ggplot(aes(x = date, y = confirmed, color = administrative_area_level_2, group = administrative_area_level_2)) +
  geom_line(linewidth = 0.8, alpha = 0.7) +  # Explicitly draw lines, no fill
  scale_color_brewer(palette = "Paired") +  # Adjust color palette for better distinction
  labs(title = "COVID-19 Case Trends Over Time by Region",
       x = "Date",
       y = "Confirmed Cases",
       color = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

df_clean %>%
  ggplot(aes(x = stringency_index, y = confirmed)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Relationship Between Stringency Index and Confirmed Cases",
       x = "Stringency Index",
       y = "Confirmed Cases") +
  theme_minimal()

# Create a linear regression model
model <- lm(confirmed ~ stringency_index + government_response_index + population, data = df_clean)
summary(model)

# Evaluate model performance
residuals <- resid(model)
hist(residuals, main = "Residuals of the Regression Model", xlab = "Residuals", breaks = 30)

# Plot the residuals
plot(model$fitted.values, residuals)
abline(h = 0, col = "red")

