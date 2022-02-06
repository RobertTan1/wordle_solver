word_giver <- function(guess_matrix_letter, guess_matrix_index, current_row, past_guesses, verbose, candidate_set = candidate_set, master_list = wordle_word_list_matrix) {
  
  green_hits <- data.table(letter = character(0), position = integer(0))
  for (row_check in 1:(current_row-1)) {
    for (j in 1:5) {
      if (guess_matrix_index[row_check, j] == 'green') {
        data_row = data.table(letter = guess_matrix_letter[row_check, j], position = j)
        green_hits <- unique(rbindlist(list(green_hits, data_row)))
      }
    }
  }
  
  yellow_hits <- data.table(letter = character(0), position = integer(0))
  for (row_check in 1:(current_row-1)) {
    for (j in 1:5) {
      if (guess_matrix_index[row_check, j] == 'yellow') {
        data_row = data.table(letter = guess_matrix_letter[row_check, j], position = j)
        yellow_hits <- unique(rbindlist(list(yellow_hits, data_row)))
      }
    }
  }
  
  grey_hits <- data.table(letter = character(0))
  for (row_check in 1:(current_row-1)) {
    for (j in 1:5) {
      if (guess_matrix_index[row_check, j] == 'grey') {
        data_row = data.table(letter = guess_matrix_letter[row_check, j])
        grey_hits <- unique(rbindlist(list(grey_hits, data_row)))
      }
    }
  }
  
  grey_hits %<>% filter(!(letter %in% yellow_hits$letter))
  grey_hits %<>% filter(!(letter %in% green_hits$letter))
  
  if (nrow(yellow_hits) == 0 & nrow(green_hits) == 0 & current_row == 2) {
    return(starter_words[2])
  } else if (nrow(yellow_hits) == 0 & nrow(green_hits) == 0 & current_row == 3) {
    return(starter_words[3])
  }

  # filter greens first
  if (nrow(green_hits) > 0) {
    for (i in 1:nrow(green_hits)) {
      temp_var <- paste0('pos_', green_hits$position[i])
      candidate_set <-
        candidate_set[which(candidate_set[,temp_var, with = F] == green_hits$letter[i]),]
    }
  }
  
  # then greys
  if (nrow(grey_hits) > 0) {
    for (i in 1:nrow(grey_hits)) {
      candidate_set <-
        candidate_set[which(str_detect(
          string = candidate_set$word,
          pattern = grey_hits$letter[i],
          negate = T
        )), ]
    }
  }
  
  # then yellows
  if (nrow(yellow_hits) > 0) {
    for (i in 1:nrow(yellow_hits)) {
      candidate_set <-
        candidate_set[which(str_detect(string = candidate_set$word, pattern = yellow_hits$letter[i])),]
      temp_var <- paste0('pos_', yellow_hits$position[i])
      candidate_set <-
        candidate_set[which(candidate_set[, temp_var, with = F] != yellow_hits$letter[i]), ]
    }
  }
  
  if(verbose) {
    print(candidate_set)
    print(guess_matrix_index)
  }
  
  if(current_row == 3 & nrow(candidate_set) > 10) {
    prune_letter_count <- data.frame(letter = letters, count = 0)
    prune_letter_count %<>% filter(!(letter %in% yellow_hits$letter)) %>%
      filter(!(letter %in% green_hits$letter))

    for (i in 1:nrow(prune_letter_count)) {
      prune_letter_count$count[i] <-
        sum(str_count(candidate_set$word, prune_letter_count$letter[i]),
            na.rm = T)
    }

    top_letters <- prune_letter_count %>% filter(count > 0) %>% arrange(-count)

    sub_candidates <- master_list %>% filter(str_detect(word, top_letters$letter[1]))

    for (i in 2:nrow(top_letters)) {

      if(nrow(sub_candidates %>% filter(str_detect(word, top_letters$letter[i]))) == 0) {
        guess <- sub_candidates$word[sample.int(nrow(sub_candidates), 1)]
        return(guess)
      } else {
        sub_candidates <- sub_candidates %>% filter(str_detect(word, top_letters$letter[i]))
      }
    }
  }
  
  candidate_set %<>% filter(!(word %in% past_guesses))
  guess <- candidate_set$word[sample.int(nrow(candidate_set), 1)]
  return(guess)
}
