# Covid-19 Data Analysis: A Study on COVID-19 Cases over the Course of the Pandemic

# Introduction

In the year 2019, the SARS (Severe Acute Respiratory Syndrome) Coronavirus 2019, shortened to COVID-19, arose, and gradually started spreading throughout the world, infecting countless people with SARS and killing many of them in the process. The disease and the spread of the virus became a worldwide pandemic, forcing the whole world into lockdown as governments worked to contain its spread, and then eventually make a vaccine to help people become immune to the virus, slowing its spread and danger. Various governments across the world took measures to slow the spread, but how effective it was has varied across countries and regions. We are primarily focusing on the top 10 regions where COVID was present by administrative area, and in doing so, we aim to highlight how well it was contained in the most prominent regions it spread. Namely, we do this by singling out the prominent administrative areas and analyzing how many cases there were, how many recovered, and how many died. 

# Data Description

The dataset we are working with is a very large dataset that is available in an R format and able to be extracted from R's library. The data covers different administrative regions, counts the number of cases confirmed, how many recovered, and how many died. It also covers other variables such as vaccine distribution and how many individuals were vaccinated against the virus, and how many were hospitalized or were in the intensive care unit, as well as how many official schools and workplaces closed down during the lockdown period of the pandemic. However, for this particular study, we are aiming for mortality rates among certain reasons, thus we single out administrative areas, and focus on the categories and variables of COVID-19 cases that existed: how many were infected, how many recovered from the disease, and how many failed to recover and died. We also compiled a number of categorical variables into a single calculation known as the Stringency Index, which will be discussed later in the report. 

There were several categories in the data. Our main categories of focus were "confirmed", "deaths", "recovered", and "administrative_area_level_2". There were also nine additional columns that were compiled into one mean value in a column known as the "stringency_index" that we assessed as well.

To start, we imported the following libraries:

````
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
````

Since this is an unusually large set of data that is also extraordinarily messy with many missing values across all cases, we had to do a thorough cleaning and preprocessing of the data before we could work with it. First, we checked for missing values. Then, we imputed missing values for any "tests" with the mean, using the following line of code:

````
# Step 1: Impute missing values for "tests" with the mean
df$tests <- ifelse(is.na(df$tests), mean(df$tests, na.rm = TRUE), df$tests)

````

After imputing missing values, we then proceeded to drop any rows that were missing critical information for our dataset, namely, if they lacked any data on confirmed cases, cases of deaths, and cases of recovery. We used the following code to do so:

````
# Step 2: Remove rows where critical information is missing (e.g., cases, deaths)
df_clean <- df %>%
  filter(!is.na(confirmed), !is.na(deaths), !is.na(recovered))
````

Lastly, we proceeded to simplify the factor labels of the "administrative_area_level_2" variable, which is our primary target categorical variable as we are looking at the Top 10 Regions where COVID was present by administrative area. We used the code shown below to do the simplification then verified that it was done.

````
# Step 3: Simplify the factor labels for administrative_area_level_2
df_clean <- df_clean %>%
  mutate(administrative_area_level_2 = fct_lump(administrative_area_level_2, n = 10))

# Verify the changes
unique(df_clean$administrative_area_level_2)
````

Upon verification of the simplification, we narrowed down the dataset to the following regions:

````
 [1] Other                 Friuli Venezia Giulia P.A. Bolzano         
 [4] Molise                Campania              Veneto               
 [7] Basilicata            Lazio                 Lombardia            
[10] P.A. Trento           Piemonte              Valle d'Aosta        
[13] Sicilia               Marche                Calabria             
[16] Liguria               Umbria                Emilia-Romagna       
[19] Abruzzo               Puglia                Toscana              
[22] Sardegna             
22 Levels: Abruzzo Basilicata Calabria Campania ... Other
````

# Methodology

For this analysis, we used several methods and visualizations in order to conduct our analysis and visualize the results. After cleaning the data, we presented various factors using multiple visualizations. 

First, we created a boxplot showcasing the distribution of confirmed cases by the region, using the following code:

````
# Distribution of Confirmed Cases by Region
df_clean %>%
  filter(!is.na(administrative_area_level_2)) %>%
  ggplot(aes(x = administrative_area_level_2, y = confirmed)) +
  geom_boxplot() +
  labs(title = "Distribution of Confirmed Cases by Region",
       x = "Region",
       y = "Confirmed Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
````

In the above code, we utilized code from the ggplot2 library to create a box plot using the administrative_area_level_2 as our x variable, and "confirmed" as our y variable, to compare the administrative regions compared to the number of confirmed cases.

Next, we then created a time series plot showcasing the number of COVID-19 cases over time from 2020 to present by the highlighted administrative region. The code to do so is shown below:

````
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
````

Above, we once again used ggplot2 to create a Time Series plot. The x axis is the date, covering 2020 to the present year 2024. The y axis is the number of confirmed cases. 

After this, we then created a scatter plot covering the relationship between the stringency index and the number of confirmed cases. In this case, the stringency index is the composite measure of nine different metrics that comprise different categories in this data: school closures, workplace closures, cancellation of public events, restrictions on public gatherings, closures of public transport, stay-at-home requirements, public information campaigns, restrictions on internal movements, and international travel controls. The stringency index on any given day is calculated on a mean score of the nine metrics, and takes a value between 0 and 100. We created the scatter plot using the code below:

````
# Scatter plot between the Stringency Index and the Confirmed Cases
df_clean %>%
  ggplot(aes(x = stringency_index, y = confirmed)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Relationship Between Stringency Index and Confirmed Cases",
       x = "Stringency Index",
       y = "Confirmed Cases") +
  theme_minimal()
````

Above is the code. The x axis is the Stringency Index, computed between 0 and 100, and the y axis covers the number of confirmed cases.

Next, we created a linear regression model. A linear regression is a statistical technique that uses a linear equation to predict the value of an unknown variable based on a known variable. The unknown variable is the target variable, and the known variable is the predictor variable. In this particular case, the target variable we performed the linear regression on is the number of confirmed cases, using three predictor variables to perform the linear regression: the stringency index, the government response index, and the total population of the administrative area. The code we used to create it is shown below:

````
# Create a linear regression model
model <- lm(confirmed ~ stringency_index + government_response_index + population, data = df_clean)
summary(model)
````

The resulting linear regression model is shown below:

````
Call:
lm(formula = confirmed ~ stringency_index + government_response_index + 
    population, data = df_clean)

Residuals:
     Min       1Q   Median       3Q      Max 
-2823327  -182412   -90630    30083  7126435 

Coefficients:
                            Estimate Std. Error t value Pr(>|t|)    
(Intercept)                1.745e+05  1.394e+03   125.2   <2e-16 ***
stringency_index           1.060e+04  9.946e+01   106.6   <2e-16 ***
government_response_index -8.830e+03  1.018e+02   -86.7   <2e-16 ***
population                 1.285e-02  6.527e-05   196.9   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 532300 on 305553 degrees of freedom
  (40503 observations deleted due to missingness)
Multiple R-squared:  0.1409,	Adjusted R-squared:  0.1408 
F-statistic: 1.67e+04 on 3 and 305553 DF,  p-value: < 2.2e-16
````

After performing linear regression, we created a histogram showcasing the residuals of the regression model in order to evaluate its performance. Residual analysis in particular is namely predicated in the evaluating the accuracy of our model by examining the differences between the predicted values of the model and the observed values of the mode. The code we used is shown below:

````
# Evaluate model performance
residuals <- resid(model)
hist(residuals, main = "Residuals of the Regression Model", xlab = "Residuals", breaks = 30)
````

Lastly, we created a plot of the residuals that we received, using the code below:

````
# Plot the residuals
plot(model$fitted.values, residuals)
abline(h = 0, col = "red")
````

In short, we used various visualization techniques to assess multiple different relationships we needed to explore for our project, such as a boxplot, time series plot, and a scatter plot exploring our necessary relationships. We performed linear regression and residual analysis as well. 

# Results

We had quite a few results from our analysis. 

## Boxplot: Distribution of Confirmed Cases by Region

First, our boxplot is shown below:

![Distribution of Confirmed Cases by Region](https://github.com/user-attachments/assets/0febb3d5-ab17-4ab7-bf91-39f7c779ae0a)

As shown in the above boxplot, a few regions had wider ranges of numbers of confirmed cases over the years compared to others. Bar none the most prolific case is the Lombardy region, located slightly north of Italy which was hit harder than most, with a wide range of values ranging from 0 to a little over 4 million cases overall within the region, with the highest maximum value as well as the highest median value of confirmed cases at roughly 3 million cases. The Veneto region has the second largest number of cases overall at nearly 3 million with a median of roughly 2 million cases. Camparia, Emilia-Rogmagna, and Lazio come in next at over 2 million cases overall with medians near 2 million but slightly less so in terms of number of confirmed cases. The regions Plemonte, Puglia, Sicilia, and Torscana come in next at close to, but less than 2 million cases with median numbers of cases above 1 million but below 2 million.

## Time Series: COVID-19 Case Trends over Time by Region

![COVID-19 Case Trends Over Time by Region](https://github.com/user-attachments/assets/7f84cd6a-2a28-4fc9-bd39-086460c5eaef)

Above, we see a Time series showcasing the number of cases per region. The region of Lombaria continues to show how it is the most affected, though the number of cases gradually approached 1 million by 2022 and then rapidly spiked over the course of that year heading into 2023 before plateauing in 2023 and into 2024. Other regions that were discussed include Emilia-ROmagna, Lazio, and Calabria which spiked a lot in 2022 and are fairly high, but not to the same degree. Multple other regions saw a spike in 2022 to a less extreme degree but a similar plateau into 2023 and 2024. Several other regions linger between 0 and 1 million cases overall but saw sharp increases in 2022. All in all, it seems 2022 was the peak of number of COVID-19 infections increasing rapidly before plateauing in the following two years.

## A Scatter Plot: The Stringency Index compared to Number of Cases

![Stringency Index](https://github.com/user-attachments/assets/f10b4a21-65f4-4dfa-b74c-17c12850ccc0)

Shown above is the Stringency Index compared to the number of confirmed cases over time, ranging between 0 and 100. The number of cases overall tended to be higher when the stringency index score was lower. As the stringency index is a mean value based on 9 different categories, this means a higher stringency score tends to correlate to much stricter regulatory measures, and anything below 0 is either erroneously recorded or had a complete lack of regulation by government to contain the spread of COVID-19. In general, however, stricter policies tended to lower the number of cases spread across regions over time.

## Results of the Linear Regression

Next, we performed a linear regression. The results are show below:

````
Call:
lm(formula = confirmed ~ stringency_index + government_response_index + 
    population, data = df_clean)

Residuals:
     Min       1Q   Median       3Q      Max 
-2823327  -182412   -90630    30083  7126435 

Coefficients:
                            Estimate Std. Error t value Pr(>|t|)    
(Intercept)                1.745e+05  1.394e+03   125.2   <2e-16 ***
stringency_index           1.060e+04  9.946e+01   106.6   <2e-16 ***
government_response_index -8.830e+03  1.018e+02   -86.7   <2e-16 ***
population                 1.285e-02  6.527e-05   196.9   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 532300 on 305553 degrees of freedom
  (40503 observations deleted due to missingness)
Multiple R-squared:  0.1409,	Adjusted R-squared:  0.1408 
F-statistic: 1.67e+04 on 3 and 305553 DF,  p-value: < 2.2e-16
````

As seen above, the regression model showed a weak but positive correlation between the number of cases and the three variables: stringency index, government response index, and population, with an R-squared value of 0.1409, suggesting a very weak relationship. However, all variables had a p value of less than 0.05, suggesting that though weak, the relationship is indeed significant. This suggests that population size, government response, and policies to control the spread of COVID all contribute to the spread of the disease in some way, though how they together contribute varies, meaning that most likely a larger population is more likely to have higher cases, but this could be counteracted by stricter controls by government powers. Individually, population seemed to correlate to more confirmed cases, government response correlated to fewer cases, and stringency index oddly correlated to more confirmed cases. The F-statistic of 1.67e04 is very large, and in relation to the p value, reinforces that this regression analysis is significant and explains a significant portion of the variance and explains how different factors influence the outcome of the number of confirmed cases.

## Residual Analysis

We then evaluated our model with residual analysis.

![Residuals of the Regression Model](https://github.com/user-attachments/assets/6d359124-e76f-452e-b12a-16da851a0ef2)

As seen above, the frequency of the residuals is mainly centered around 0, with the mode, median, and mean all being close to 0. This is very good, as this means that on average, the model's predictions are close to the actual values of the model. This means there is no significant bias in the predictions, and thus most predictions of the model are very accurate, with few significant errors. The model is also relatively symmetric with very little skew, and this reinforces the accuracy and reliability of the model as it implies that this model does not systematically overpredict or underpredict the outcomes. The range goes between -2e+06 and 6e+06, and though very few, there are some cases where the model's predictions deviate significantly, though they comprise an extremely small minority of the predictions and the vast majority otherwise are very accurate, meaning this model is very reliable and trustworthy overall, and the results can be trusted as significant in terms of assessing and drawing relatively accurate conclusions.

![Residual Plot](https://github.com/user-attachments/assets/eda5e78f-285c-48a3-9877-8474cd5c2910)

Finally, shown above is a scatter plot of the residuals. The red line shown above is where the data is centered around, which is 0, with very little residuals or error. However, this more clearly denotes how many cases deviate significantly, and a good number do reach 6e06, particularly with fitted values around 500,000 and 1,000,000, and the cases that go below 0 are with extremely high fitted values around 2,500,000.

# Discussion

There is a lot that can be taken away from this. First is that policy has shown a significant impact in controlling and regulating the spread of COVID-19. Stricter stringency measure tend to correlate to fewer COVID-19 cases in a given region overall, and implies that governmental policies and interventions play a crucial role in controlling the spread of the virus and in turn the SARS disease. However, stringency measures may not always be successful: stricter measures does not necessarily equate to good measures or effective execution, as the correlation score still showed in the regression analysis. Policymakers can use this information to their advantage to design effective strategies for any future situation such as this. The positive correlation between population size and the number of confirmed cases is to be expected, as the larger the population, the more the disease spreads, and this emphasizes a necessity of more tailored public health responses in more densely populated areas, as these areas contain more people in total and many more in close contact with one another, making the virus easier to spread around without proper measures taken. These regions require more resources and more targeted interventions to prevent the disease from spreading. 

Some regions, such as Lombardy and Veneto, both of which are in Italy, will likely need more healthcare resources, such as hospital beds, medical staff, and vaccination supplies. Lombardy in particular is known to be an industrialized region with one of the worst air qualities in Europe, which may have been a contributing factor. Long term exposure to particulate matter, meteorological variables (weather and climate), and socioeconomic variables were all discovered to be contributing factors within these particular regions, especially Lombardy. Poorer countries may be more vulnerable due to lack of proper healthcare, and places with climate and poor air quality that are friendly to the spread of a virus such as COVID-19 are particularly vulnerable and is crucial to know in order to understand how to allocate resources efficiently.

The correlation between government response and cases further implies that public awareness and compliance with government regulations is key to controlling virus spread. This is shown in the usage of masks and receiving vaccines.

There are quite a few limitations in our data. The data had missing values in several places that had to be either imputed or removed, which could introduce some biases and inaccuracies, though not to an extreme degree in this case. It is also uncertain if this data is easy to generalize across various populations. The R-squared value of 0.1409 suggests this model only explains a small portion of the variance in all confirmed cases: thus, we will need to investigate other variables such as socio-economic factors, climate, and weather. Healthcare infrastructure and public behavior are also potential factors worth investigating. 

In the future, we could perform longitudinal studies to analyze changes of case outbreak over time, cross-country studies to study COVID-19 outbreak and measures taken across different countries and continents based on government and socioeconomic level. We can also employ machine learning techniques such as random forests, gradient boosting, and neural networks to uncover complex relationships between variables and improve predictive accuracy. We could also perform behavioral analysis of individual and community behavior such as mask-wearing, social distancing, and vaccine administration, to understand public health interventions.

# Conclusion

In short, this was a long and extensive study of the COVID-19 pandemic. It was most prevalent in certain regions, particularly those with poor air quality and lower socioeconomic level, and it was especially prevalent around Italy, suggesting that was a significant region where the pandemic outbreak was rampant. Moreover, higher stringency index meaning stricter policies and compliance to such policies helped greatly in controlling the spread of the virus, and this is vital to controlling the spread of any virus or disease as a whole. Thus, what happened during the Coronavirus-2019 pandemic and the measures that were taken over time to regulate its spread and reduce its threat level are things that can be learned from, both by governments and healthcare associations and by the general populace alike, especially in future situations where a disease as deadly as COVID may be prevalent. 

# References

Ferguson, N. M., Laydon, D., Nedjati-Gilani, G., Imai, N., Ainslie, K., Baguelin, M., Bhatia, S., Boonyasiri, A., Cucunuba, Z., Cuomo-Dannenburg, G., Dighe, A., Dorigatti, I., Fu, H., Gaythorpe, K., Green, W., Hamlet, A., Hinsley, W., Okell, L. C., Elsland, S. L. v., ... Ghani, A. C. (2020). Impact of non-pharmaceutical interventions (NPIs) to reduce COVID-19 mortality and healthcare demand. Imperial College London. https://doi.org/10.25561/77482

Haug, N., Geyrhofer, L., Londei, A., Dervic, E., Desvars-Larrive, A., Loreto, V., Pinior, B., Thurner, S., & Klimek, P. (2020). Ranking the effectiveness of worldwide COVID-19 government interventions. Environmental Research, 191, 110087. https://doi.org/10.1016/j.envres.2020.110087

Roser, M., Ritchie, H., Ortiz-Ospina, E., & Hasell, J. (2020). Coronavirus Pandemic (COVID-19) – Our World in Data. Retrieved from https://ourworldindata.org/metrics-explained-covid19-stringency-index
