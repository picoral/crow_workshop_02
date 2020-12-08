# load library
library(tidyverse)

# read data in
annotated_files <- read_csv("processed_corpus/micusp_engl_subset.csv")

# get concordance lines
search_expression <- "(argue)"

# simplest way
annotated_files %>%
  filter(grepl(search_expression, lemma)) %>% 
  select(sentence_number, sentence) %>%
  mutate(sentence = gsub(search_expression, "**\\1", sentence))

# kwic way
annotated_files %>%
  mutate(kwic = ifelse(grepl(search_expression, lemma),
                       TRUE, FALSE)) %>%
  mutate(before = gsub("NA\\s", "", paste(lag(token, 3), lag(token, 2), lag(token))),
         after = gsub("NA\\s", "", paste(lead(token), lead(token, 2), lead(token, 3)))
  ) %>%
  filter(kwic) %>%
  select(before, token, after) %>%
  view()

# another way, which gets the whole sentence
search_results <- annotated_files %>%
  mutate(kwic = ifelse(grepl(search_expression, lemma),
                       TRUE, FALSE)) 

concordance_lines <- data.frame()
for (i in 1:nrow(search_results)) {
  if (search_results$kwic[i]) {
    # get sentence for this kwic
    selected_sentence_number <- search_results$sentence_number[i]
    selected_sentence <- search_results %>%
      filter(sentence_number == selected_sentence_number)
    
    kwic_token_number <- search_results$token_number[i]
    
    context_before <- selected_sentence %>%
      filter(token_number < kwic_token_number) %>%
      mutate(context_before = paste(token, collapse = " ")) %>%
      distinct(context_before) %>%
      pull(context_before)
    
    context_after <- selected_sentence %>%
      filter(token_number > kwic_token_number) %>%
      mutate(context_after = paste(token, collapse = " ")) %>%
      distinct(context_after) %>%
      pull(context_after)
    
    this_concordance_line <- data.frame(context_before = context_before,
                                        kwic = search_results$token[i],
                                        context_after = context_after)
    
    concordance_lines <- bind_rows(concordance_lines,
                                   this_concordance_line)
  }
}

############# INCLUDE PART OF SPEECH IN SEARCH #############################
# read pos_description data
pos_descriptions <- read_csv("auxiliary_data/pos_descriptions.csv")

# add pos_description to data
annotated_files <- left_join(annotated_files,
                             pos_descriptions)

# simplest way -- with lemma and pos
annotated_files %>%
  filter(grepl(search_expression, lemma)) %>% 
  filter(grepl("^V", pos)) %>%
  select(pos, pos_description, lemma, token, sentence_number, sentence) %>%
  mutate(sentence = gsub(search_expression, "**\\1", sentence)) %>%
  view()

# simplest way -- with just pos
annotated_files %>%
  filter(grepl("^V", pos)) %>%
  select(pos, pos_description, lemma, token, sentence_number, sentence) %>%
  mutate(sentence = gsub(search_expression, "**\\1", sentence)) %>%
  view()
