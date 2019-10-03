#installed.package("tidyverse")
library(tidyverse)
read_csv("data/gapminder_data.csv")
gapminder <- read_csv("data/gapminder_data.csv")
summarise(gapminder, min(year), max(year))
gapminder_1977 <- read_csv("data/gapminder_data.csv") %>% 
  filter(year == 1977)
gapminder_1977
# Using ggplot2
ggplot(data = gapminder_1977)
?ggplot
ggplot(
  data = gapminder_1977, 
  mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop)
) +
  geom_point() +
  scale_x_log10()

#ggplot(gapminder_1977, aes(x = <VAR1>, y = <VAR2>, colour = <VAR3>)) + geom_point()
ggplot(gapminder_1977, aes(x = pop, y = lifeExp, colour = continent, size = gdpPercap)) + geom_point() +
     scale_x_log10()

ggplot(gapminder_1977, aes(x = pop, y = lifeExp, colour = continent, size = gdpPercap)) + geom_point() +
  scale_x_log10()

ggplot(
  data = gapminder_1977, 
  mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop)
) +
  geom_point(alpha = 0.1, shape = "star", colour= "blue", size = 5) +
  scale_x_log10()

ggplot(
  data = gapminder_1977, 
  mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop)
) +
  geom_point(alpha = 0.5, shape = "star") +
  scale_x_log10()

ggplot(
  data = gapminder_1977
) +
  geom_point(mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop)) +
  scale_x_log10()


ggplot(data = gapminder, aes(x = year, y = lifeExp, group = country, color = continent)) +
  geom_line()

ggplot(data = gapminder, aes(x = year, y = lifeExp, group = country, color = continent)) +
  geom_line() + 
  geom_point(colour = "black")

ggplot(data = gapminder, aes(x = year, y = lifeExp, group = country, color = continent)) +
  geom_point(colour = "black") +
  geom_line()


gapminder_mean <- gapminder %>% 
  group_by(continent,year) %>% 
  summarise(mean_lifeexp = mean(lifeExp))
gapminder_mean

?geom_point

read_csv("data/OveatingDwellings.csv")
overheat <- read_csv("data/OveatingDwellings.csv")
# Using ggplot2
ggplot(data = overheat)
?ggplot
ggplot(
  data = overheat, 
  mapping = aes(x = State, y = Orig_criteria)
) +
  geom_col(width = 0.75,colour= "red")
