library("tidyverse")
library("modelr")
library("broom")

gapminder <- gapminder::gapminder

gapminder

gapminder %>% 
  ggplot(mapping = aes(x = year, y = lifeExp)) +
  geom_line(mapping = aes(group = country), alpha = 0.2) +
  geom_smooth()

no <- gapminder %>% 
  filter(country == "Norway")

no %>% 
  ggplot(mapping = aes(x = year, y = lifeExp)) +
  geom_line()

no_mod <- lm(formula = lifeExp ~ year, data = no)

no %>% 
  add_predictions(no_mod) %>% 
  ggplot(mapping = aes(x = year, y = pred)) + 
  geom_line() +
  geom_point(mapping = aes(x = year, y = lifeExp), colour = "red")


no %>% 
  add_residuals(no_mod) %>% 
  ggplot(mapping = aes(x = year, y = resid)) +
  geom_hline(yintercept = 0, colour = "white", size = 3) +
  geom_line()

by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()


by_country 


by_country %>% 
  filter(country == "Norway") %>% 
  unnest(data)

country_model <- function(df) {
  lm(formula = lifeExp ~ year, data = df)
}

by_country <- by_country %>% 
  mutate(model = map(data, country_model))

by_country %>% 
  filter(continent == "Europe")

by_country <- by_country %>% 
  mutate(resids = map2(data, model, add_residuals))

resids <- by_country %>% 
  unnest(resids)

resids %>% 
  ggplot(mapping = aes(x = year, y = resid)) +
  geom_line(mapping = aes(group = country), alpha = 0.2) +
  geom_smooth(mapping = aes(group = continent)) +
  facet_wrap(~ continent)

by_country %>% 
  mutate(augment = map2(model, data, augment))

augment(x = no_mod, data = no)
tidy(x = no_mod)
summary(no_mod)

glance(x = no_mod)

by_country %>% 
  mutate(augment = map2(model, data, augment)) %>% 
  unnest(augment)

by_country %>% 
  mutate(tidy = map(model, tidy)) %>% 
  unnest(tidy)

glance <- by_country %>% 
  mutate(glance = map(model, glance)) %>% 
  unnest(glance, .drop = TRUE)

glance %>% 
  arrange(r.squared)

glance %>% 
  ggplot(mapping = aes(x = continent, y = r.squared)) +
  geom_jitter(mapping = aes(colour = r.squared), width = 0.2) +
  scale_color_gradient(low = "red", high = "blue", guide = FALSE)

bad_fit <- glance %>% 
  filter(r.squared < 0.25)

gapminder %>% 
  semi_join(bad_fit, by = "country") %>% 
  ggplot(mapping = aes(x = year, y = lifeExp, colour = country)) +
  geom_line()



