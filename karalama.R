library(tidyverse)
library(rvest)
library(stringr)
url1 <- "https://m.imdb.com/search/title/?title_type=feature&release_date=2010-01-01,2023-12-31&num_votes=2500,&country_of_origin=TR&count=250"
url2 <- "https://m.imdb.com/search/title/?title_type=feature&release_date=,2009-12-31&num_votes=2500,&country_of_origin=TR&count=250"
urls <- c(url1, url2)
read_html(urls[1])

# Başlıklar
titles <- page %>% 
  html_nodes(".ipc-title__text") %>% 
  html_text(trim = TRUE)

# Yıllar
years <- page %>% 
  html_nodes(".dli-title-metadata-item:nth-child(1)") %>% 
  html_text(trim = TRUE)

# Süreler
durations <- page %>% 
  html_nodes(".dli-title-metadata-item:nth-child(2)") %>% 
  html_text(trim = TRUE)

# Oy Sayısı
votes <- page %>% 
  html_nodes(".ipc-rating-star--voteCount") %>% 
  html_text(trim = TRUE)

# Puanlar
ratings <- page %>% 
  html_nodes(".ipc-rating-star--rating") %>% 
  html_text(trim = TRUE)

movie_data <- data.frame(
  Title = titles,
  Year = years,
  Duration = durations,
  Votes = votes,
  Rating = ratings,
  stringsAsFactors = FALSE
)

