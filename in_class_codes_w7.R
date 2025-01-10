#basics of ggplot2
library(tidyverse)
library(dslabs)
library(ggthemes)
library(ggrepel)
data(murders)


### first define the slope of the line
r <- murders |> summarize(rate = sum(total) / sum(population) * 10^6) |> pull(rate)

## now make the plot. 
murders |> ggplot(aes(x=population/10^6, y=total, label = abb)) + 
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() + 
  scale_x_log10() + 
  scale_y_log10() + 
  xlab("Populations in millions (log scale)") +    
  ylab("Total number of murders (log scale)") +   
  ggtitle("US Gun Murders in US 2010") + 
  scale_color_discrete(name = "Region") + 
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  theme_economist()


# histogram
data(heights)

p <- heights |> filter(sex == "Male") 
p <- p |> ggplot(aes(x = height))
p + geom_histogram()
p + geom_histogram(binwidth = 1, fill = "blue", col = "black") + xlab("Male heights in inches") + ggtitle("Histogram")

#smooth density
p + geom_density()


# Summarizing Data with dplyr
library(tidyverse)
library(dslabs)
data("heights")
data("murders")

# filter males or females
males_df <- heights |> filter(sex == "Male")
females_df <- heights |> filter(sex == "Female")


# Add a new column named murder_rate
murders_with_murder_rate <- murders |> mutate(murder_rate = total / population*100000)


# compute average and standard deviation for males
s <- heights |> filter(sex == "Male") |> summarize(average = mean(height), standard_deviation = sd(height))
s

# access the components with the accessor dollar sign.
s$average
s$standard_deviation

#compute median, minimum, maximum
heights |> filter(sex == "Male") |> summarize(median = median(height), minimum = min(height), maximum = max(height))


# return vectors instead of data frames
us_murder_rate <- murders |> summarize(rate = sum(total) / sum(population)*100000)
us_murder_rate
class(us_murder_rate)

# alternative approaches
us_murder_rate <- murders |> summarize(rate = sum(total) / sum(population)*100000)
us_murder_rate |> pull(rate)
us_murder_rate |> pull()
us_murder_rate$rate

us_murder_rate <- murders |> summarize(rate = sum(total) / sum(population)*100000) |> pull(rate)
us_murder_rate

# we can use dplyr's %>% operator instead of |> to use .$ (dotplaceholder)

murders |> summarize(rate = sum(total) / sum(population)*100000) %>% .$rate



# groupby
heights |> group_by(sex)

# mean and standard deviaton
heights |> group_by(sex) |> 
  summarize(average = mean(height), standard_deviation = sd(height))

# median murder rate in the four regions of the country
murders <- murders |> mutate(murder_rate = total / population*100000)
murders |> group_by(region) |> summarize(median_rate = median(murder_rate))
murders |> group_by(region) |> summarize(median_rate = median(murder_rate))


# sorting data tables
# order by population
murders |> arrange(population) |> head()

#order by murder_rate
murders |> arrange(murder_rate) |> head()

# to make it descending instead of ascending
murders |> arrange(desc(murder_rate)) |> head()


# Nested sorting
murders |> arrange(region, murder_rate) |> head()

# top n rows based on a variable (returns without ordering)
murders |> arrange(desc(murder_rate)) |> head(n=10)
murders |> slice_max(order_by=murder_rate, n=10)

murders |> arrange(murder_rate) |> head(n=10) 
murders |> slice_min(order_by=murder_rate, n=10)






# case study
library(dslabs)
library(tidyverse)
data(gapminder)
head(gapminder)

# Compare Siri Lanka and T??rkiye
gapminder |> filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) |> select(country, infant_mortality)

# Let???s start with an older year, 1962.
gapminder |> filter(year == 1962) |> 
  ggplot(aes(x=fertility, y=life_expectancy)) + 
  geom_point()

# add colors for continents            
gapminder |> filter(year == 1962) |> 
  ggplot(aes(x=fertility, y=life_expectancy, color = continent)) + 
  geom_point()

# faceting
gapminder |> filter(year %in% c(1962, 2012)) |> 
  ggplot(aes(fertility, life_expectancy, color = continent)) + 
  geom_point() + 
  facet_grid(continent ~ year)

# We can ignore continents and facet only using year.
gapminder |> filter(year %in% c(1962, 2012)) |> 
  ggplot(aes(x= fertility, y= life_expectancy, color= continent)) + 
  geom_point() + 
  facet_grid(. ~ year)


# add more years to our plot.
# facet_grid
gapminder |> filter(year %in% c(1962, 1970, 1980, 1990, 2000, 2012)) |>
  ggplot(aes(x=fertility, y=life_expectancy, color = continent)) + 
  geom_point() + 
  facet_grid(.~year)

# facet_wrap
gapminder |> filter(year %in% c(1962, 1970, 1980, 1990, 2000, 2012)) |>
  ggplot(aes(x=fertility, y=life_expectancy, color = continent)) + 
  geom_point() + 
  facet_wrap(~year)


# time series plot
gapminder |> filter(country == "United States") |>
  ggplot(aes(x = year, y = fertility)) + geom_line()

# Compare two countries (South Korea and Germany) with a time series plot
countries <- c("South Korea", "Germany")
#fertility rate
gapminder |> filter(country %in% countries) |> 
  ggplot(aes(x=year, y=fertility, color = country)) + 
  geom_line()

#life expectancy
gapminder |> filter(country %in% countries) |> 
  ggplot(aes(x=year, y=life_expectancy, color = country)) + 
  geom_line()


# boxplots
# find GDP
gapminder <- gapminder |>
  mutate(dollars_per_day = gdp/population/365)
gapminder |> head()

# stack boxplots next to each other
past_year <- 1970
p <- gapminder |> filter(year == past_year & !is.na(gdp)) |>
  ggplot(aes(region, dollars_per_day))

p + geom_boxplot()

# make x axis labels readable
p + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


# add colors
p <- gapminder |> filter(year == past_year & !is.na(gdp)) |>
  ggplot(aes(region, dollars_per_day, fill=continent))
p + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# reorder regions by median income levels
p <- gapminder |> filter(year == past_year & !is.na(gdp)) |>
  mutate(region = reorder(region, dollars_per_day, FUN = median)) |>
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  xlab("")
p

# scale y axis
p + scale_y_continuous(trans = "log2")

# show the data
p + scale_y_continuous(trans = "log2") + geom_point(show.legend = FALSE)




# In order to compare two years, we need to make sure that the list of countries are the same in these two years.

country_list_1 <- gapminder |> filter(year == 1970 & !is.na(dollars_per_day)) |> pull(country)

country_list_2 <- gapminder |> filter(year == 2010 & !is.na(dollars_per_day)) |> pull(country)

country_list <- intersect(country_list_1, country_list_2)

p <- gapminder |> filter(year %in% c(1970, 2010) & country %in% country_list) |>
  mutate(region = reorder(region, dollars_per_day, FUN = median)) |>
  ggplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab(" ") + scale_y_continuous(trans = "log2")

p + geom_boxplot(aes(region, dollars_per_day, fill = continent)) + 
  facet_grid(year~.)

# ease comparisons

p <- gapminder |> filter(year %in% c(1970, 2010) & country %in% country_list) |>
  mutate(region = reorder(region, dollars_per_day, FUN = median)) |>
  ggplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab(" ") + 
  scale_y_continuous(trans = "log2")

p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year))) 






