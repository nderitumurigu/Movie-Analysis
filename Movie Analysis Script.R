# This script contains all the code that has been used in the movie analysis


#The first step is to load the packages that will be used.
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(lubridate)
#Then load the data which is in .xlsx format and preview it.
movie_data <- read_excel("C:\\Users\\Joseph\\Downloads\\Movie-Data-Starter-Project.xlsx")


#Lets select the columns of data that we need and put them in another dataframe
movie_data_1 <- select(movie_data, 
                       Movie_Title = "Movie Title",
                       Release_Date = "Release Date",
                       Genre = "Genre (1)",
                       Budget = "Budget ($)",
                       Revenue = "Box Office Revenue ($)")
#To make the analysis of these movies by year, it is important to separate the date column into month and year. The mutate function becomes useful here
movie_data_1 <- mutate(movie_data_1, 
                       Month = month(Release_Date, label = TRUE),
                       Year = year(Release_Date))
#We can also include a column for profitability which will be calculated by getting difference between revenue and budget
movie_data_1 <- mutate(movie_data_1,
                       Total_Profit = (Revenue - Budget))
#We shall also include the cost revenue ratio
movie_data_1 <- mutate(movie_data_1,
                       Cost_Revenue_Ratio = (Budget / Revenue) *100)
head(movie_data_1)


#We shall get a summary of the number of movies of each genre each year.(grouped by the genre and year)
viz_1 <- summarise(group_by(movie_data_1,
                            Genre, Year),
                   number_of_movies = n())
#We shall then create different vizualizations to show this numbers
ggplot(data = viz_1, 
       aes(x = Genre, y = number_of_movies)) +
  geom_col(aes(fill = Year)) +
  facet_wrap(~Year) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip() +
  labs(title = "Number of Movies of the different genres grouped by year",
       caption = "Data obtained from the movies dataset")


#We shall get a summary of the profits of each genre each year.(grouped by the genre and year)
viz_2 <- summarise(group_by(movie_data_1,
                            Genre, Year),
                   Average_Profits = mean(Total_Profit),
                   Average_Cost_Revenue_ratio = mean(Cost_Revenue_Ratio))
#We shall then create different visualizations to show this numbers
ggplot(data = viz_2, 
       aes(x = Genre, y = Average_Cost_Revenue_ratio)) +
  geom_col(aes(fill = Year)) +
  facet_wrap(~Year) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip() +
  labs(title = "Mean Cost Revenue Ratio for the genres grouped by year",
       caption = "Data obtained from the movies dataset")
#Summary statistics
viz_3 <- summarise(group_by(viz_2,
                            Genre),
                   mean_crr = mean(Average_Cost_Revenue_ratio))
#visualise the summary statistics
ggplot(data = viz_3, aes(x = Genre, y = mean_crr)) +
  geom_col(aes(fill = Genre)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Mean Cost Revenue Ratio for the genres from 2012 to 2015",
       caption = "Data obtained from the movies dataset")


viz_4 <- summarise(group_by(movie_data_1,
                            Month,
                            Year),
                   Average_Monthly_Profit = mean(Total_Profit))
#Make a simple visualization to shpw the average profit per month for each year.
ggplot(data = viz_4, aes(x = Month, y = Average_Monthly_Profit)) +
  geom_col(aes(fill = Month)) +
  facet_wrap(~Year) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Average Monthly profit for all the movies from 2012 to 2015",
       caption = "Data obtained from the movies dataset")