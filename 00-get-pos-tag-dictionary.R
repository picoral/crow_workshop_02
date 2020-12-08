# load libraries
library(tidyverse)
library(rvest)

# read html in
my_url <- "https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html"
my_html <- read_html(my_url)

# read table in
my_table <- my_html %>%
  html_node("table") %>%
  html_table(trim = TRUE) %>%
  filter(X3 != "Description") %>%
  rename(pos = X2,
         pos_description = X3) %>%
  select(-X1)

# write table to file
write_csv(my_table, "auxiliary_data/pos_descriptions.csv")
