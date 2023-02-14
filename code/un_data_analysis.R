
#R gives an error message
#load packages
library(tidyr)
library(tidyverse)
gapminder_data <- read.csv("gapminder_data.csv")
gapminder_data
View(gapminder_data)
gapminder_data <- read_csv("gapminder_data.csv")
#calculating the mean value of life expectancy
summarize(gapminder_data, averageLifeExp=mean(lifeExp))
#instead of including the data as an argument, we can use the pipe operator to pass the data value to the summarize function
gapminder_data %>% summarize(averageLifeExp = mean (lifeExp))
gapminder_data %>% 
  summarize(averageLifeExp = mean (lifeExp))
#find the mean population using the piping function
gapminder_data %>% 
  summarize(averagePop = mean (pop))
#find the mean population of recent years using piping function
gapminder_data %>% 
  filter(year == 2007) %>% 
  summarize(average = mean(lifeExp))
#average GDP per capita for the first year in the dataset
gapminder_data %>% 
  summarize(first_year = min(year))
gapminder_data %>% 
  filter(year == 1952) %>% 
  summarize(first_year_GDP = mean(gdpPercap))
#group data by the year
gapminder_data %>%
  group_by(year) %>%
  summarize(average=mean(lifeExp))
#group by continent to get average life expectancy
gapminder_data %>% 
  group_by(continent) %>% 
  summarize(average=mean(lifeExp))
#creat more columns using summarize
gapminder_data %>%
  group_by(continent) %>%
  summarize(average=mean(lifeExp), min=min(lifeExp))

#make new variables with mutate function
gapminder_data %>%
  mutate(gdp = pop * gdpPercap)

#make a new column showing population in millions
gapminder_data %>% 
  mutate(gdp = pop * gdpPercap, popinMillions = pop/1000000)

#subset columns using select
gapminder_data %>%
  select(pop, year)

gapminder_data %>%
  select(-continent)

#creat a subset of data with only country, continent, year, and lifeExp
gapminder_data %>% 
  select(country, continent, year, lifeExp)

#helper functions in select
gapminder_data %>%
  select(year, starts_with("c"))

gapminder_data %>% 
  select(ends_with("p"))

#make the dataset wide instead of long format
gapminder_data %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp )

#creat a dataframe with 2007 america data, dropping year and continent
gapminder_data_2007 <-
  gapminder_data %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent)
View(gapminder_data_2007)

gapminder_data_2007_tutorial <- read_csv("gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent)
View(gapminder_data_2007_tutorial)

#both dataframe worked

#cleaning the data
read.csv("co2-un-data.csv")
read_csv("co2-un-data.csv")

#skip the first line
read_csv("co2-un-data.csv", skip=1)

co2_emissions_dirty <- read_csv("co2-un-data.csv", skip=2,
                                col_names=c("region", "country", "year", "series", "value", "footnotes", "source"))

co2_emissions_dirty

co2_emissions_dirty %>% 
  select(country, year, series, value)

co2_emissions_dirty %>% 
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"))

co2_emissions_dirty %>%
  select(country,year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emission",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emission")) %>%
  pivot_wider(names_from=series, values_from=value) %>% 
  filter(year == 2005) %>% 
  select(-year)

#assign dataframe name 
co2_emissions <- co2_emissions_dirty %>%
  select(country,year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emission",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emission")) %>%
  pivot_wider(names_from=series, values_from=value) %>% 
  filter(year == 2005) %>% 
  select(-year)

View(co2_emissions)

#joining data frames
gapminder_data_2007 <- read_csv("gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent)
inner_join(gapminder_data, co2_emissions)

anti_join(gapminder_data, co2_emissions, by="country")

co2_emissions <- read_csv("co2-un-data.csv", skip=2,
                          col_names=c("region", "country", "year",
                                      "series", "value", "footnotes", "source")) %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita")) %>%
  pivot_wider(names_from=series, values_from=value) %>%
  filter(year==2005) %>%
  select(-year) %>%
  mutate(country=recode(country,
                        "Bolivia (Plurin. State of)" = "Bolivia",
                        "United States of America" = "United States",
                        "Venezuela (Boliv. Rep. of)" = "Venezuela")
  )

gapminder_data <- read_csv("gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent) %>%
  mutate(country = recode(country, "Puerto Rico" = "United States")) %>% 
  group_by(country) %>% 
  summarize(lifeExp = sum(lifeExp * pop)/sum(pop), 
            gdpPercap = sum(gdpPercap * pop)/sum(pop),
            pop = sum(pop))

gapminder_co2 <- inner_join(gapminder_data, co2_emissions, by="country")

gapminder_co2 %>%  
  mutate(region = if_else(country == "Canada" | country == "United States" | country == "Mexico", "north", "south"))

write_csv(gapminder_co2, "gapminder_co2.csv")

#analyze data
ggplot(gapminder_co2, aes(x=gdpPercap, y=per_capita)) +
  geom_point() +
  labs(x="GDP (per capita)",
       y="CO2 emitted (per capita)",
       title="There is a strong association between a nation's GDP \nand the amount of CO2 it produces"
  )+
  geom_smooth(method = "lm")