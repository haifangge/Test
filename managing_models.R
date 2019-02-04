library(gapminder)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

gapminder
gapminder <- gapminder %>%  mutate(year1950 = year - 1950)

# netsted data
by_country <- gapminder %>% 
  group_by(continent, country) %>% 
  nest()

# str(by_country)
by_country$data[[1]]

########################
# Functional programming
# Get mean for each column of the d.f. matcars
mtcars %>% map_dbl(mean)
# Get median for each column of the d.f. matcars
mtcars %>% map_dbl(median)

# We can think of functions as data
funs <- list(mean, median, sd)
# Pass this data to function by map
funs %>% map(~ mtcars %>% map_dbl(.x))

#########################
# Fit a model to each country
country_model <- function(df) {
  lm(lifeExp ~ year1950, data = df)
}

models <- by_country %>%
  mutate(
    mod = data %>% map(country_model)
  )

models
models %>% filter(continent == "Africa")

# Models -> tidy data
library(broom)
models <- models %>% 
  mutate(
    tidy = map(mod, broom::tidy),  # estimates
    glance = map(mod, broom::glance),  # summary
    rsq = glance %>% map_dbl("r.squared"), 
    augment = map(mod, broom::augment) # observation
  )

models

# Sort model by r-squared
models %>% arrange(desc(rsq))

models %>% filter(continent == "Africa")


# Plot r-squared
models %>% 
  ggplot(aes(rsq, reorder(country, rsq))) + 
    geom_point(aes(colour = continent))


# Unnest
unnest(models, data)
unnest(models, glance, .drop = TRUE) %>% View()

models %>% 
  unnest(tidy) %>% 
  select(continent, country, term, estimate, rsq) %>% 
  spread(term, estimate) %>% 
  ggplot(aes(`(Intercept)`, year1950)) +
    geom_point(aes(colour = continent, size = rsq)) + 
    geom_smooth(se = FALSE) +
    xlab("Life Expectancy (1950") +
    ylab("Yearly improvement") +
    scale_size_area()
