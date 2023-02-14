library(readr)
gapminder_1997 <- read_csv("gapminder_1997.csv")
View(gapminder_1997)
?read_csv
sum(5,6)
Sys.Date()
round(x=3.1415)
round(x=3.1415, digits=2)
round(digits=2, x=3.1415)
round(2, 3.1415)
?round
library(tidyverse)
library(tidyverse)
View(gapminder_1997)
#Creating a plot
ggplot(data=gapminder_1997)+aex(X=gdpPercap)
library(readr)
ggplot(data=gapminder_1997)+aex(X=gdpPercap)
library(tidyverse)
ggplot(data=gapminder_1997)+aex(X=gdpPercap)
ggplot(data=gapminder_1997)+
  aes(x=gdpPercap, y = lifeExp, color = continent, size = pop/1000000) +
  labs(x = "GDP Per Capita", y = "Life Expectancy (yrs)", 
       title = "Do people in wealthy countries live longer", 
       size = "Population (in millions)")+
  #adding point to the graph and change size of data point geom_point (size = 2)
  geom_point()+
  #change color palettes to the data point
  scale_color_brewer(palette = "Set1")+
  #add theme color
  theme_classic()

# Read in full gapminder dataset
gapminder_data <- read_csv(file = "gapminder_data.csv")

#Plot time (x-axis) and life expect (y-axis) points
ggplot(data = gapminder_data)+
  aes(x = year, y = lifeExp,  group = country, color = continent)+
  labs(x = "Year", y = "Life Expectancy (yrs)")+
  geom_line()+
  scale_color_brewer(palette="Set1")

# Discret Plots/violin
ggplot(data=gapminder_1997)+
  aes(x=continent, y = lifeExp, color = continent) +
  labs(x = "Continent", y = "Life Expectancy (yrs)")+
  geom_boxplot(outlier.shape = NA)+
  #alpha will change the intensidty of the color
  geom_jitter(alpha = 0.5)

#load tidyverse
library(tidyverse)

#recreating the plot
ggplot(data=gapminder_1997)+
  aes(x=continent, y = lifeExp, color = continent) +
  labs(x = "Continent", y = "Life Expectancy (yrs)")+
  geom_boxplot(outlier.shape = NA)+
  #alpha will change the intensidty of the color
  geom_jitter(alpha = 0.5)

#use violin plot and change the transparency by changing alpha value (<1)
ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp) +
  geom_violin(fill="darkblue", alpha = 0.2)

#Univariate Plots
ggplot(gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins=50)

#bonus exercise
ggplot(gapminder_1997)+
  aes(x = continent) +
  geom_dotplot()

#Plot Themes

#Facets
ggplot(gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point()+
  facet_grid(rows=vars(continent))

#Saving plots
ggsave("awesone_plot.jpg", width=6, height=4)
violin_plot <- ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp) +
  geom_violin(aes(fill=continent))
violin_plot
violin_plot + theme_bw()
violin_plot<-violin_plot+theme_bw()
violin_plot
ggsave("awesome_violin_plot.jpg", plot = violin_plot, width=6, height=4)

#Bonus exercise
dotplot_continent <- ggplot(gapminder_1997)+
  aes(x = continent)+
  lab(title="hopefully this works")
  geom_dotplot()
dotplot_continent

#Animated plots
install.packages(c("gganimate", "gifski"))
library (gganimate)
library (gifski)
stationcHansPlot<- ggplot(data = gapminder_data)+
  aes (x = log(gdpPercap), y = lifeExp, size = pop/1000000, color = continent)+
  geom_point(alpha = 0.5) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "GDP Per Capita", y = "Life Expectancy", color = "Continent",
       size = "Population (in millions)") +
  theme_classic()
animateHansPlot <- stationcHansPlot +
  transition_states(year, transition_length = 1, state_length = 1) +
  ggtitle("{closeest_state}")
animateHansPlot

#map data
mapdata <- map_data("world") %>%
  mutate(region = recode(region,
                         USA="United States",
                         UK="United Kingdom"))
install.packages("mapproj")
library(mapproj)
gapminder_1997 %>%
  ggplot() +
  geom_map(aes(map_id=country, fill=lifeExp), map=mapdata) +
  expand_limits(x = mapdata$long, y = mapdata$lat) +
  coord_map(projection = "mollweide", xlim = c(-180, 180)) +
  ggthemes::theme_map()

?ggsave
