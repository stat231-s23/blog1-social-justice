# Code for scraping Opinon articles from The Student

# loading packages
library(tidyverse)
library(rvest)
library(robotstxt)
library(purrr)
library(rlist)
library(lubridate)

# confirming we are allowed access to opinions page
opinion_list_url <- "https://amherststudent.com/section/opinion"
robotstxt::paths_allowed(opinion_list_url)

# scraping article urls

opinion_urls <- opinion_list_url %>%
  read_html() %>%
  html_elements("a") %>%
  html_attr("href") %>%
  as.data.frame() %>%
  janitor::clean_names() %>%
  rename(url = x) %>%
  #selecting only the articles
  filter(str_detect(url, "https://amherststudent.com/article/"))

#removing duplicate urls
opinion_urls2 <- distinct(opinion_urls)

#converting dataframe to list
opinions_list <- as.list(opinion_urls2)

# make list of all article bodies
# takes ~5 min to run (don't run more than once)
opinions_text <- vector(mode='list', length=100)
for(i in 1:100) {
  # scrape all paragraphs
  temp <- opinions_list$url[i] %>%
    read_html() %>%
    html_elements("p") %>%
    html_text2()
  
  # paste all paragraphs into a single string, paragraphs separated by new line
  str <- ""
  for(j in 2:length(temp)) {
    str <- paste(str, temp[j], sep = "\n")
  }
  # save combined paragraphs to list
  opinions_text[i] <- str
}

# scrape article titles
titles <- read_html(opinion_list_url) %>%
  html_elements("h2") %>%
  html_text2()

dates <- read_html(opinion_list_url) %>%
  html_elements("p") %>%
  html_text2() %>%
  data.frame() %>%
  filter(row_number() %% 3 == 2) %>% ## Delete every 3rd row starting from 1
  filter(row_number() != 102 & row_number() != 1)

# combine title and body lists into dataframe
opinions_text2 <- data.frame(unlist(titles), unlist(opinions_text)) %>%
  cbind(dates)

# renaming columns
names(opinions_text2) = c("Title", "Body", "Date")

# filter out cartoons/comics
opinions_text2 <- filter(opinions_text2, !grepl("cartoon|Cartoonist|comic", Body)) %>%
  filter(!grepl("American Punch Bowl", Title)) %>%
  select(Date, Title, Body) %>%
  mutate(Date = as.Date(mdy(Date)))

#saving dataframe as a csv
write_csv(as.data.frame(opinions_text2), file = "opinions.csv")
