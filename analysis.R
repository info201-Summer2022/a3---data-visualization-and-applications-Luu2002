library(plotly)
library(dplyr)
library(ggplot2)
library(maps)
library(usmapdata)

# Filter the data for an better understanding
df <- read.csv('incarceration_trends.csv') %>%
select(year, state, county_name, total_pop, total_jail_pop, black_jail_pop, aapi_jail_pop, latinx_jail_pop, white_jail_pop) %>%
  filter(year %in% c('2018'))%>%
  filter(state %in% c('WY'))

# Trends Over Time Chart
# Number represent the total number of people in jail different year and in different states
states <- c('WA', 'OR', 'NY', 'WY','TN')
total_2015 <- c(12264, 5675, 25975, 1540, 28010)
total_2016 <- c(12319, 5649, 25821, 1534, 28698)
total_2017 <- c(12556, 5487, 25821, 1527, 28513)
total_2018 <- c(12765, 5493, 24253, 1540, 29404)
df <- data.frame(states, total_2015, total_2016, total_2017, total_2018)

fig <- plot_ly(df, x = ~states, y = ~total_2015, type = 'bar', name = '2015')
fig <- fig %>% add_trace(y = ~total_2016, name = '2016')
fig <- fig %>% add_trace(y = ~total_2017, name = '2017')
fig <- fig %>% add_trace(y = ~total_2018, name = '2018')
fig <- fig %>% layout(title = "Jail Population in Different States (2015 - 2018)",
                      xaxis = list(title = "States"),
                      yaxis = list (title = "Total Population"), barmode = 'group')
fig


# Comparison Chart
# The number in each states for white and black are the total black jail population and 
# the total white jail population from 2015 to 2018
location <- c('NY', 'TN', 'WA', 'OR', 'WY')
race_Black <- c(45404, 40084, 7755, 2055, 591)
race_White <- c(32659, 71302, 31096, 16097, 3947)
df <- data.frame(location, race_Black, race_White)

fig <- plot_ly(df, x = ~location, y = ~race_Black, type = 'bar', name = 'Black')
fig <- fig %>% add_trace(y = ~race_White, name = 'White')
fig <- fig %>% layout(title = "Jail Population Between Two Races (2015-2018)",
                      xaxis = list(title = "States"),
                      yaxis = list (title = "Jail Population"), barmode = 'stack')
fig


# Map

# find population of black in jail in the usa
usa <- df %>% 
  #select(year, black_jail_pop, state) %>%
  filter(year %in% c(2018))
usa_df <- usa %>%
  group_by(state) %>%
  summarise(black_jail_pop) %>%
  arrange(desc(black_jail_pop))

# change state to lower case
usa_df$state <- tolower(usa_df$state)
dataframe <- merge(s, usa_df,
                   by.x = "region",
                   by.y = "state")

ggplot(dataframe, aes(x = long, y = lat, group = group, fill = black_jail_pop)) +
  geom_polygon(color = "pink") +
  coord_map('polyconic') +
  scale_fill_gradient2(low = "red", high = 'blue')+
  theme_void() +
  ggtitle('Black Population in Jail the Year of 2018')


