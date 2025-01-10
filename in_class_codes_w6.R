# required libraries
library(tidyverse)
library(dslabs)

# We have to describe the heights of our classmates

data(heights)
str(heights)
head(heights)
nrow(heights)


# For example, the sex can be summarized by the proportions of categories:
prop.table(table(heights$sex))

# Normal distribution
# assume x is a vector
index <- heights$sex == "Male"
x <- heights$height[index]
average <- mean(x)
sd <- sd(x)
c(average, sd)


# Standard units
# convert normally distributed data into standard units
(x - average) / sd
z <- scale(x)


# number of z's that are less than 2 and bigger than negative 2
sum(abs(z) < 2)
sum(abs(z) < 2) / length(z)
mean(abs(z) < 2)


# If we use normal distribution approximation, we don???t need to see entire data set.
# What is the probability that a randomly selected student is taller than 70.5 inches.
1 - pnorm((70.5-mean(x))/sd(x), mean(z), sd(z))


# 1. Empirical Probability Calculations (calculated directly from data)
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

#2. Theoretical Probability Calculations (for a normal distribution with the mean and standard deviation of the data set ????)
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))


mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

# quantiles
mean(x <= 69.5)
p <- seq(0.05, 0.95, 0.05)

observed_quantiles <- quantile(x, p) # observed quantiles

theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x)) # theoretical quantiles 
plot(theoretical_quantiles, observed_quantiles)
abline(0, 1)

# It is much easier if we use standard units --> we do not need to define the mean and standard deviation
p <- seq(0.05, 0.95, 0.05) 
z <- scale(x)
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles) 
abline(0, 1) 




# Basics of ggplot2
library(ggplot2)
library(dplyr)
library(dslabs)
data(murders)

# first option
ggplot(data=murders)


# second option
murders |> ggplot()

# We can also assign it to a variable:
p <- murders |> ggplot()
class(p)
print(p)

# mapping
murders |> ggplot() + geom_point(aes(x = population/10^6, y = total))
p + geom_point(aes(x = population/10^6, y = total))


# adding a label to each point.
p + geom_point(aes(x = population/10^6, y = total)) + geom_text(aes(population/10^6, total, label = abb))

# Layers: Tinkering
p + geom_point(aes(x = population/10^6, y = total), size = 3) + geom_text(aes(population/10^6, total, label = abb), nudge_x = 1)


# more efficient mapping
p <- murders |> ggplot(aes(population/10^6, total, label = abb))
p + geom_point(size = 3) + geom_text(nudge_x = 1.5)


# We can override global mappings by local mappings.
p <- murders |> ggplot(aes(population/10^6, total, label = abb)) 
p + geom_point(size = 3) + geom_text(aes(x = 10, y = 800, label = "Hello there!")) 

#Layers: Scales, labels and colors

p <- murders |> ggplot(aes(population/10^6, total, label = abb)) 
p + geom_point(size = 3) + geom_text(nudge_x = 0.05) + scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10")


p + geom_point(size = 3) + geom_text(nudge_x = 0.075) + scale_x_log10() + scale_y_log10()


# Add labels to x and y axis
p + geom_point(size = 3) + geom_text(nudge_x = 0.075) + scale_x_log10() + scale_y_log10() + xlab("Populations in millions (log scale)") + ylab("Total number of murders (log scale)") + ggtitle("US Gun Murders in US 2010")

# Change colors
p <- murders |> ggplot(aes(population/10^6, total, label = abb)) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() + 
  scale_y_log10() + 
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale)") + 
  ggtitle("US Gun Murders in US 2010")

# make colors all blue
p + geom_point(size = 3, color = "blue")

# map colors to regions
p + geom_point(aes(col = region), size = 3)


# Finally, we want to add a line that represent the average murder rate for the entire country.
r <- murders |> summarize(rate = sum(total) / sum(population)*10^6.) |> pull(rate)
# to add a line we use geom_abline
# intercept a and slope b
# default has slope 1, intercept 0

p + geom_point(aes(col = region), size = 3) + geom_abline(intercept = log10(r))


#  we can change line style
p + geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") + 	geom_point(aes(col=region), size = 3)

# capitalize legend
p <- p + scale_color_discrete(name = "Region")
p <- p + geom_point(aes(col = region), size = 3) + geom_abline(intercept = log10(r))


# We can change the theme style by adding a layer.
library(ggthemes)
p + theme_economist()

p + theme_fivethirtyeight()


# Start from scratch

library(ggthemes)
library(ggrepel)
### first define the slope of the line
r <- murders |>summarize(rate = sum(total) / sum(population) * 10^6) |> pull(rate)
## now make the plot.
murders |> ggplot(aes(population/10^6, total, label = abb)) + 
  geom_abline(intercept = log10(r), lty =2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() + 
  scale_x_log10() + 
  scale_y_log10() + 
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale)") + 
  ggtitle("US Gun Murders in US 2010") + 
  scale_color_discrete(name = "Region") + 
  theme_economist()



# histogram
library(dslabs)
data(heights)
p <- heights |> filter(sex == "Male") 
p <- p |> ggplot(aes(x = height))
p + geom_histogram()
p  + geom_histogram(binwidth = 1)
p + geom_histogram(binwidth = 1, fill = "blue", col= "black") + xlab("Male heights in inches") + ggtitle("Histogram")
p + geom_density()
p + geom_density(fill="blue")


p <- heights |> filter(sex == "Male") |> ggplot(aes(sample = height)) 
p + geom_qq()

params <- heights |> filter(sex == "Male") |> summarize(mean = mean(height), sd = sd(height))
p + geom_qq(dparams = params)
p + geom_qq(dparams = params) + geom_abline()


heights |> filter(sex == "Male") |> 
  ggplot(aes(sample = scale(height))) + 
  geom_qq() +
  geom_abline()


# griding
library(gridExtra)
p <- heights |> filter(sex == "Male") |>  ggplot(aes(x = height))

p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col = "black")
grid.arrange(p1, p2, p3, ncol = 3)


# dplyr
# compute average and standard deviation for males
s <- heights |> filter(sex == "Male") |> 
  summarize(average = mean(height), standard_deviation = sd(height))
s
s$average
s$standard_deviation


#compute median, minimum, maximum
heights |> filter(sex == "Male") |> 
  summarize(median = median(height), minimum = min(height), maximum = max(height))

heights |> filter(sex == "Male") |> reframe(range = quantile(height, c(0, 0.5, 1)))

# return vectors instead of data frames.
data("murders")
us_murder_rate <- murders |> summarize(rate = sum(total) / sum(population)*100000)
us_murder_rate
class(us_murder_rate)

us_murder_rate$rate

us_murder_rate |> pull(rate)

us_murder_rate |> pull()


us_murder_rate <- murders |> summarize(rate = sum(total) / sum(population)*100000) |> pull(rate)



#group_by
heights |> group_by(sex)

# Mean and Standard Deviation
heights |> group_by(sex) |> summarize(average = mean(height), standard_deviation = sd(height))

# Median murder rate in the four regions of the country
murders <- murders %>% mutate(murder_rate = total / population*100000)
murders |> group_by(region) |> summarize(median_rate = median(murder_rate))


# sorting data tables
murders |> arrange(population) |> head()

# Order states by murder_rate
murders |> arrange(murder_rate) |> head()

# To make it descending instead of ascending
murders |> arrange(desc(murder_rate)) |> head()

# nested sorting
murders |> arrange(region, murder_rate) |> head()

# top_n
murders |> top_n(10, murder_rate)

murders |> arrange(desc(murder_rate)) |> top_n(10)