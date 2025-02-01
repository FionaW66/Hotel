library(tidyverse)
library(gapminder)
my_gap <- gapminder
my_gap %>% filter(country == "Canada")
my_precious <- my_gap %>% filter(country == "Canada")
my_gap %>% 
  mutate(gdp = pop * gdpPercap)
ctip <- my_gap %>% 
  filter(country == "Canada") %>% 
  select(gdpPercap)
my_gap <- my_gap %>% 
  mutate(
    tmp = rep(ctip$gdpPercap, nlevels(country)),
    gdpPercapRel = gdpPercap / tmp,
    tmp = NULL
    )
my_gap %>% 
  filter(country == "Canada")  %>% 
  select(country, year, gdpPercapRel)
summary(my_gap$gdpPercapRel)
my_gap %>% 
  filter(country == "United States") %>% 
  select(gdpPercapRel)
my_gap %>% 
  filter(gdpPercapRel == max(gdpPercapRel))
my_gap %>% 
  arrange(year, country)
my_gap %>% 
  filter(year == 2007) %>% 
  arrange(desc(lifeExp))
my_gap %>% 
  rename(
    life_exp = lifeExp,
    gdp_percap = gdpPercap,
    gdp_percap_rel = gdpPercapRel
  )
my_gap %>% 
  filter(country == "Burundi")
my_gap %>% 
  filter(country == "Burundi", year > 1996) %>% 
  select(yr = year, lifeExp, gdpPercap) %>% 
  select(gdpPercap, everything()) %>% 
  arrange(desc(lifeExp))
my_gap %>% 
  group_by(continent) %>% 
  summarize(n=n())
table(gapminder$continent)
str(table(gapminder$continent))
my_gap %>% 
  group_by(continent) %>% 
  tally()
my_gap %>% 
  count(continent)
my_gap %>% 
  group_by(continent) %>% 
  summarize(
    n = n(),
    n_countries = n_distinct(country)
  )
my_gap %>% 
  group_by(continent) %>% 
  summarize(avg_lifeExp = mean(lifeExp))
my_gap %>% 
  group_by(continent) %>% 
  group_by(year) %>% 
  filter(year == 1952, 2007) %>% 
  summarize_at(lifeExp, gdpPercap)
my_gap %>% 
  filter(year %in% c(1952, 2007)) %>% 
  group_by(continent, year) %>% 
  summarize_at(vars(lifeExp, gdpPercap), list(~mean(.), ~median(.)))
my_gap %>% 
  filter(continent == "Asia") %>% 
  group_by(year) %>% 
  summarize_at(vars(lifeExp), list(~min(.), ~max(.)))
my_gap %>% 
  group_by(country) %>% 
  select(country, year, lifeExp) %>% 
  mutate(lifeExp_gain = lifeExp - first(lifeExp)) %>% 
  filter(year < 1963)
my_gap %>% 
  filter(continent == "Asia") %>%
  group_by(year) %>% 
  summarize_at(vars(lifeExp), list(~min(.), ~max(.)))
my_gap %>% 
  filter(continent == "Asia") %>% 
  group_by(year) %>% 
  select(country, year, lifeExp) %>% 
  filter(min_rank(desc(lifeExp)) < 2 | min_rank(lifeExp) < 2) %>% 
  arrange(year) %>% 
  print(n = Inf)
x = c(7, 1, 5, 3, 19, 35, 6, 2)
min_rank(x) < 2
filter(min_rank(x) < 2)
print(min_rank(x) < 2)
asia <- my_gap %>% 
  filter(continent == "Asia") %>% 
  select(year, country, lifeExp) %>% 
  group_by(year)
asia %>% 
  filter(min_rank(lifeExp) < 2 | min_rank(desc(lifeExp)) < 2) %>% 
  print(n = Inf)
asia %>% 
  mutate(
    le_rank = min_rank(lifeExp),
    le_desc_rank = min_rank(desc(lifeExp))
  ) %>% 
  filter(country %in% c("Afghanistan", "Japan", "Thailand"), year < 1995)
my_gap %>% 
  filter(continent == "Asia") %>% 
  select(year, country, lifeExp) %>% 
  arrange(year) %>% 
  group_by(year) %>% 
  top_n(1, wt = (lifeExp))
my_gap %>% 
  select(country, year, continent, lifeExp) %>%
  group_by(continent, country) %>% 
  mutate(le_delta = lifeExp - lag(lifeExp)) %>% 
  summarize(worst_le_delta = min(le_delta, na.rm=TRUE)) %>% 
  top_n(1, wt = desc(worst_le_delta)) %>% 
  arrange(worst_le_delta)
getwd()
