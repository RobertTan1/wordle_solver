# Load libraries and import word list -------------------------------------

library(tidyverse)
library(magrittr)
library(profvis)
library(data.table)
library(microbenchmark)
# library(ReinforcementLearning)

# Full list
# wordle_word_list <-
#   read_tsv(
#     'https://gist.githubusercontent.com/RobertTan1/24b6bffab84362b33f495d0ad7ed77b8/raw/ca9018b32e963292473841fb55fd5a62176769b5/valid-wordle-words.txt',
#     col_names = 'word',
#     progress = show_progress()
#   ) %>% as.data.table()

# Human friendly list
wordle_word_list <-
  t(read_delim(
    'https://gist.githubusercontent.com/RobertTan1/f913cf44623e95ee48f7e083cd75e9ea/raw/f0653c72b0a2bab03e837c26772e012371d36c17/human_friendly_wordle_words.csv',
    col_names = F,
    progress = show_progress(), delim = ', '
  ) %>% head()) %>% as.data.table()

names(wordle_word_list) <- 'word'

# find optimal starting words in order ------------------------------------

letter_count <- data.frame(letter = letters, count = 0)

for (i in 1:nrow(letter_count)) {
  letter_count$count[i] <-
    sum(str_count(wordle_word_list$word, letter_count$letter[i]))
}

letter_count %>% arrange(-count) %>% View()

# manually construct starter word vector
wordle_word_list %>% 
  filter(str_detect(word, '^(?=.*y)(?=.*c)(?=.*p)(?=.*k)(?=.*).*$')) %>% head()

# starter_words <- c('aloes', 'rutin','pocky') # only need 3 since they contain all vowels
starter_words <- c('orate', 'incus','dumpy') # only need 3 since they contain all vowels

# construct matrix of wordle words for matching purposes
wordle_word_list_matrix <- wordle_word_list %>% separate(word,
                                                         into = c(paste0('pos_', 0:5)),
                                                         sep = '', remove = F) %>% select(-pos_0)

