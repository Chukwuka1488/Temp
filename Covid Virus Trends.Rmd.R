install.packages("lintr")
install.packages("styler")
install.packages("tidyverse", dependencies = TRUE)

library(dplyr)
require(tidyverse)
library(tibble)
# Set working directory
getwd()

# Load this file
library(readr)
covid19_df <- read_csv("covid19.csv")

# Determine the dimension of the dataframe
dim(covid19_df)

# Determine the column names
vector_cols <- colnames(covid19_df)
vector_cols

# What is the data structure
str(vector_cols)

# Display the first few row
head(covid19_df)

# Display the summary of the covid_df dataset using the function glimpse()
# from the tibble package.

glimpse(covid19_df)

covid19_df_all_states <- covid19_df %>%
  filter(Province_State == "All States") %>%
  select(-Province_State)
covid19_df_all_states

covid19_df_all_states_daily <- covid19_df_all_states %>%
  select(Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive)
covid19_df_all_states_daily
# Write code to summarize the covid_df_all_states_daily dataframe by computing the sum of the number of tested, positive, active and hospitalized cases grouped by the Country_Region column.
#
# Use the function group_by() to group rows by Country_Region column.
# Combine the function summarize() and the function sum() to compute the sum for each column.
#
# Assign the sum of daily_tested to the column name tested.
# Assign the sum of daily_positive to the column name positive.
# Assign the sum of active to the column name active.
# Assign the sum of hospitalizedCurr to the column name hospitalized.
# Arrange the tested column in descending order using the function arrange().
#
# Store the result in the variable covid_df_all_states_daily_sum.
# Display this dataframe.
covid19_df_all_states_daily_sum <- covid19_df_all_states_daily %>%
  group_by(Country_Region) %>%
  summarise(
    tested = sum(daily_tested),
    positive = sum(daily_positive),
    active = sum(active),
    hospitalized = sum(hospitalizedCurr)
  ) %>%
  arrange(-tested)
print(covid19_df_all_states_daily_sum)
# Extract the top ten rows from the covid_df_all_states_daily_sum dataframe using the command head(covid_df_all_states_daily_sum, 10)
#
# Store the result in the variable named covid_top_10.
covid_top_10 <- head(covid19_df_all_states_daily_sum, 10)
covid_top_10

# Create the following vectors from the covid_top_10 dataframe.
#
# Create the countries vector that contains the Country_Region column values. We can use covid_top_10$Country_Region to extract this column from the covid_top_10 dataframe.
# Create the tested_cases vector that contains the tested column values.
# Create the positive_cases vector that contains the positive column values.
# Create the active_cases vector that contains the active column values.
# Create the hospitalized_cases vector that contains the hospitalized column values.

countries <- covid_top_10$Country_Region
countries
tested_cases <- covid_top_10$tested
tested_cases
positive_cases <- covid_top_10$positive
active_cases <- covid_top_10$active
hospitalized_cases <- covid_top_10$hospitalized
# Write code to name the previous vectors: tested_cases, positive_cases, active_cases, and hospitalized_cases with the country names' vector countries using the function names()
names(tested_cases) <- countries
names(positive_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries

# Identify the top three positive against tested cases.
#
# Divide the vector positive_cases by the vector tested_cases using the operator /.
# Identify the top three ratio. You can do this operation manually by looking at the result of the division.
# Store the result as the named vector, positive_tested_top_3, where each country name is associated with its ratio.
positive_tested_top_10 <- positive_cases / tested_cases
positive_tested_top_10_descending <- positive_tested_top_10[order(-positive_tested_top_10)]
dim(positive_tested_top_10_descending)
positive_tested_top_3 <- positive_tested_top_10_descending[1:3]
format(round(positive_tested_top_3, 3), nsmall = 2)

# Create the following vectors from the table above.
# Create the united_kingdom vector using this vector: c(0.11, 1473672, 166909, 0, 0).
# Create the united_states vector using this vector: c(0.10, 17282363, 1877179, 0, 0).
# Create the turkey vector using this vector: c(0.08, 2031192, 163941, 2980960, 0)

united_kingdom <- c(0.11, 1473672, 166909, 0, 0)
united_states <- c(0.10, 17282363, 1877179, 0, 0)
turkey <- c(0.08, 2031192, 163941, 2980960, 0)

covid_mat <- rbind(united_kingdom, united_states, turkey)
covid_mat

colnames(covid_mat) <- c("Ratio", "tested", "positive", "active", "hospitalized")

covid_mat

question <- "Which countries have had the highest number of positive cases against the number of tests?"

answer <- c("Positive tested cases" = positive_tested_top_3)

datasets_df <- list(covid19_df, covid19_df_all_states, covid19_df_all_states_daily, covid_top_10)
datasets_mat <- list(covid_mat)
datasets_vec <- list(vector_cols, countries)
data_structure_list <- list("dataframe" = datasets_df, "matrix" = datasets_mat, "vector" = datasets_vec)

covid_analysis_list <- list(question, answer, data_structure_list)
covid_analysis_list[2]
