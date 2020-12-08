# load libraries
library(tidyverse)

# create list of files to read in
files <- list.files(path = "raw_corpus",
                    pattern = "*.txt.conll",
                    full.names = TRUE)

# read in all files
# first create empty data frame
annotated_files <- data.frame()
# for each file in our list of files
for (i in 1:length(files)){
  # read the tab separated file in
  this_file <- read_tsv(files[i], col_names = FALSE)
  # create a column with the filename, do some clean up of the file name
  this_file$filename <- gsub("\\.txt\\.conll|raw_corpus\\/", "", files[i])
  # combine this file with all the others
  annotated_files <- bind_rows(annotated_files,
                               this_file)
}

# change column names
annotated_files <- annotated_files %>%
  rename(token_number = X1,
         token = X2,
         lemma = X3,
         pos = X4,
         entity = X5,
         dependency = X6,
         dep_label = X7)


# add sentence number
# this for loop takes a while to run
# start with creating a column for sentence number
annotated_files$sentence_number <- NA
# start the sentence counter
sentence_count <- 0
# for every row/token in our corpus
for (i in 1:nrow(annotated_files)) {
  # if the token number is one, that indicates a new sentence start
  if (annotated_files$token_number[i] == 1) {
    # add to sentence counter
    sentence_count <- sentence_count + 1
  }
  # add sentence_count to the appropriate column and row (i.e., row i)
  annotated_files$sentence_number[i] <- sentence_count
}

# now that we have sentence number, collapse tokens by sentence in a new
# sentence column/variable
annotated_files <- annotated_files %>%
  group_by(sentence_number) %>%
  mutate(sentence = paste(token, collapse = " ")) %>%
  ungroup()

# write file out so we don't have to run the whole thing again
write_csv(annotated_files, "processed_corpus/micusp_engl_subset.csv")
