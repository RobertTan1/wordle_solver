candidate_set <- wordle_word_list_matrix[sample(500,10)]
current_row <- 3

green_hits <- data.frame(letter = c('c','g'))

yellow_hits <- data.frame(letter = NA)
yellow_hits$letter <- c('a')


if(current_row == 3 & nrow(candidate_set) >5) {
  prune_letter_count <- data.frame(letter = letters, count = 0)
  prune_letter_count %<>% filter(!(letter %in% yellow_hits$letter)) %>%
    filter(!(letter %in% green_hits$letter))
  for (i in 1:nrow(candidate_set)) {
    prune_letter_count$count[i] <-
      sum(str_count(candidate_set$word, prune_letter_count$letter[i]))
  }
  
  top_letters <- prune_letter_count %>% arrange(-count)
  
  sub_candidates <-
    wordle_word_list_matrix %>% filter(str_detect(word, top_letters$letter[1]))
  
  for (i in 2:nrow(top_letters)) {
    if (nrow(sub_candidates %>% filter(str_detect(word, top_letters$letter[i]))) == 0) {
      guess <- sub_candidates$word[sample.int(nrow(sub_candidates), 1)]
      return(guess)
    } else {
      sub_candidates <-
        sub_candidates %>% filter(str_detect(word, top_letters$letter[i]))
    }
  }
}


# Manual helper

guess_matrix_letter <- matrix(nrow = n_tries, ncol = 5)
guess_matrix_index <- matrix(nrow = n_tries, ncol = 5)

guess_matrix_letter[1,] <- strsplit('orate','')[[1]]
guess_matrix_letter[2,] <- strsplit('incus','')[[1]]
guess_matrix_letter[3,] <- strsplit('spill','')[[1]]

guess_matrix_index[1,] <- c('grey', 'yellow', 'grey', 'grey', 'yellow')
guess_matrix_index[2,] <- c('grey', 'grey', 'grey', 'yellow', 'grey')
guess_matrix_index[3,] <- c('green', 'grey', 'green', 'green', 'green')

word_giver(guess_matrix_letter,
           guess_matrix_index,
           current_row = 4,
           past_guesses = c(''),
           verbose = T)
