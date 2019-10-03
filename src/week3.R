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

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point(alpha = 0.3) +
  scale_x_log10() +
  geom_smooth(method = "lm",size = 1.5)

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point(aes(color = continent),alpha = 0.3) +
  scale_x_log10() +
  scale_colour_manual(values = c("red", "green", "purple", "blue", "yellow"))


ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point()

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point(alpha = 0.5) + 
  scale_x_log10() +
  scale_colour_manual(values = c("red", "green", "purple", "blue", "yellow"))+
  geom_smooth(method = "lm", colour = "red",size = 1.5) 


ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = continent), alpha = 0.5) + 
  scale_x_log10() +
  geom_smooth(method = "lm", colour = "red",size = 1.5)

a_countries <- filter(gapminder, str_starts(country, "A"))

ggplot(data = a_countries, aes(x = year, y = lifeExp, color = continent)) +
  geom_line() + 
  facet_wrap( ~ country)

ggplot(
  data = gapminder, 
  mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop)
) +
  geom_point() +
  scale_x_log10()+ 
  facet_wrap( ~ year)

ggplot(
  data = gapminder, 
  mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop)
) +
  geom_point() +
  scale_x_log10()+ 
  facet_wrap( ~ year)

b_countries <- group_by(gapminder, str_starts(country, "A"))

ggplot(data = b_countries, aes(x = pop, y = lifeExp, color = continent)) +
  geom_point() +
  scale_x_log10()+ 
  facet_wrap( ~ year)

ggplot(
  data = gapminder_1977, 
  mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop, label = country)
) +
  geom_point(alpha = 0.5, shape = "star") +
  scale_x_log10() +
  geom_text()

gapminder_rich <- filter(gapminder_1977, gdpPercap >= 20000)

ggplot(
  data = gapminder_1977, 
  mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop, label = country)
) +
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  geom_text(data = gapminder_rich)

rough_plot <- ggplot(data = a_countries, aes(x = year, y = lifeExp, color = continent)) +
  geom_line() + 
  facet_wrap( ~ country)
#rough_plot + scale_colour_brewer(paletter = "1")

rough_plot + 
  labs(title = "Figure 1",
       x = "Year",
       y = "Life_expectanccy",
       colour = "Continent")+
  theme(
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 24),
    axis.line = element_line(colour = "blue", size = 0.5)
  )

ggsave("fig/myfirstplot.png", plot = rough_plot, width = 12,
       height = 10, units = "cm")


?geom_point

read_csv("data/OveatingDwellings.csv")
overheat <- read_csv("data/OveatingDwellings.csv")
overheat_ga <- gather(overheat, criteria, value , -State)
overheat_ga
# Using ggplot2
ggplot(data = overheat_ga)
?ggplot
ggplot(
  data = overheat_ga, 
  mapping = aes(x = State, y = value, fill = criteria, label = value*100 )
) +
  geom_col(width = 0.7) + #  geom_col(fill = "green", width = 0.75) +
  facet_wrap( ~ criteria) +
  geom_text() +
  labs(title = "Figure 1",
       x = "State",
       y = "%"
)

