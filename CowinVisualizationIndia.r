---
title: "Exploring Covid-19 trends in India: Vulnerabilities and Mitigation Strategies(A Pilot Study) By Swayamjit Saha"
output:
  word_document:
    toc: yes
  pdf_document: default
  html_document:
    highlight: pygments
    theme: readable
    toc: yes
---

```{r setup, include=FALSE}
# Sometimes fig.width=4.5 is good, other times 3.5
knitr::opts_chunk$set(echo=TRUE, cache=TRUE, fig.asp=0.65, fig.width=6.5)
require(tidyverse)
require(maps)
theme_set(theme_bw(base_size = 14)) # Good font size for my computer
```

# Installing necessary libraries
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("maps")

# Load the necessary libraries
library(tidyverse)
library(ggplot2)
library(maps)

# Cleaning the datasets and making it ready for processing
# As an essential step of cleaning the dataset, we remove the blank spaces and replace them with dots in the column names for example "First Doses Administered" to "First.Doses.Administered". This was basically done so that fetching the column names becomes easy during function calling. We save the new data in a new CSV and name the file as `dataset1.csv`.
#In order to do the above steps, the following code chunk has been implemented:

# Read data from CSV file
covid_data <- read.csv("covid_vaccine_statewise.csv", stringsAsFactors = FALSE)

# Remove blank spaces from column names
colnames(covid_data) <- gsub(" ", "", colnames(covid_data))

# Save the modified data to the existing CSV file
write.csv(covid_data, "dataset1.csv", row.names = FALSE)

##############################

# Explore the structure of the raw data

str(covid_data)  

# Tidying the covid_vaccine_statewise.csv data

covid_data <- read.csv("dataset1.csv", stringsAsFactors = FALSE) # Loading the dataset1.csv to the covid_data variable

tidy_covid_data <- covid_data %>%
  # Handle missing values if necessary
  # Example: drop rows with any missing values
  drop_na() %>%           
  
  # Convert Date to a Date object
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"))
  
  # Example: If you have separate columns for confirmed, recovered, and deaths, pivot to long format
  pivot_longer(cols = c(Confirmed, Deaths),
               names_to = "Status",
               values_to = "Count")

# Finally exploring the structure of the new tidy data
str(tidy_covid_data)

# Save the tidy data to a new CSV file
write.csv(tidy_covid_data, "tidy_dataset1.csv", row.names = FALSE)

# Creating plots for visualization

# Example 1. Visualizing Male vs. Female Vaccine Doses Administered for a particular date of the month

# Load libraries
library(ggplot2)
library(tidyverse) 

# Read data from CSV file
vaccine_data <- read.csv("dataset2.csv", stringsAsFactors = FALSE)

# Convert Date to a Date object
vaccine_data$Updated.On <- as.Date(vaccine_data$Updated.On, format = "%d/%m/%Y")

# Specify the particular date (replace '2022-01-15' with your desired date)
selected_date <- as.Date('16/01/2021')

# Filter data for the selected date
filtered_data <- vaccine_data %>% filter(Updated.On == selected_date)

# Create a bar graph
ggplot(filtered_data, aes(x = Gender, y = Total.Individuals.Vaccinated., fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = paste("Individuals Vaccinated by Gender on", format(selected_date, "%d/%m/%Y")),
       x = "Gender",
       y = "Total Individuals Vaccinated",
       fill = "Gender") +
  theme_minimal()
  
#The output of the above code is given below:
#example1.png

# Example 2. Time series of Initial Cases in Kerala from 1/30/2020 to 3/1/2020

# Filter data for the specified date range
covid_data <- read.csv("covid_19_india.csv")
filtered_covid_data1 <- covid_data %>%
  filter(Sno >= 1, Sno <=32)
  
ggplot(data = filtered_covid_data1, mapping = aes(x = Sno, y = Confirmed)) +
  geom_point(mapping = aes(color = "Red"))
  
#The output of the above code is given below:
#example2.png
  
# Example 3. Time series of Total Doses Administered Vaccination Initially in Random states
covid_data <- read.csv("dataset1.csv")

filtered_covid_data2 <- covid_data %>%
  filter(State == "India")

ggplot(data = filtered_covid_data2, mapping = aes(x = Total.Doses.Administered, y = Sno)) +
  geom_point(mapping = aes(color = "Red"))
  
#The output of the above code is given below:
#example3.png

# Example 4. Graph plot to visualize females vaccination status from West Bengal in the period 27/06/2021 to 07/07/2021
covid_data3 <- read.csv("dataset1.csv")
filtered_covid_data3 <- covid_data3 %>%
  filter(State == "West Bengal", Female.Individuals.Vaccinated. >= 1)
  
ggplot(data = filtered_covid_data3, mapping = aes(x = Female.Individuals.Vaccinated., y = Sno)) +
  geom_point(mapping = aes(color = "Red"))+
  labs(title = paste("Female Individuals Vaccinated in the period 27/06/2021 to 07/07/2021"))
  
In this case it does not show any plotting in the graph. This is because data is missing in this region of the dataset. Hence, there is some level of inconsistency in the dataset.

#The output of the above code is given below:
#example4.png

# Example 5. Graph plot to visualize the installation of vaccine sites throughout 2020 to 2021

Now, let's try to visualize how the vaccine sites accomodation has changed over the time with respect to the sessions of vaccine doses. 

covid_data3 <- read.csv("covid_vaccine_statewise.csv")

ggplot(data = covid_data3, mapping = aes(x = Sessions, y= Sites)) +
  geom_point(alpha = 1/3) +
  geom_line(alpha = 1/3) +
  geom_smooth()
  
This graph plot shows that India has made an exponential growth (mostly) in developing vaccine sites for conducting vaccination sessions on the common populi.

#The output of the above code is given below:
#example5.png

The same data if produced in a histogram produces the result:

ggplot(data = covid_data3) +
  geom_histogram(mapping = aes(x = Sessions), bins = 100)
  
#The output of the above code is given below:
#example5histogram.png

# Example 6. Graph plot to visualize the state of Odisha in India has made an exponential growth (mostly) in developing vaccine sites for conducting vaccination sessions

This graph plot shows for the state of Odisha in India has made an exponential growth (mostly) in developing vaccine sites for conducting vaccination sessions on the common populi after making a slight degradation it has made a steep positive slope indicating the success in setting up new vaccination sites for vaccination purpose.

covid_data4 <- filter(covid_data3, State == "Odisha")
ggplot(data = covid_data4, mapping = aes(x = Sessions, y = Sites)) +
  geom_point(mapping = aes(size = Sessions), alpha = 1/3) +
  geom_smooth()

#The output of the above code is given below:
#example6.png  
