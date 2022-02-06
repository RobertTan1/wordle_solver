# setup game --------------------------------------------------------------

# DEBUGGING
# target_word <- 'allll'
# DEBUGGING

run_wordle_simulation <- function(verbose = F) {
  target_word <-
    # wordle_word_list$word[sample.int(nrow(wordle_word_list), 1)]
    as.character(wordle_word_list[,.SD[sample(.N, 1)]])
  
  target_word_split <- strsplit(target_word, '')[[1]]
  
  target_word_letter_count <- data.frame(letter = letters, count = 0)
  
  past_guesses <- c()
  
  for (i in 1:nrow(target_word_letter_count)) {
    target_word_letter_count$count[i] <-
      sum(str_count(target_word, letter_count$letter[i]))
  }
  
  n_tries = 9
  # guess_word <- ''
  guess_matrix_letter <- matrix(nrow = n_tries, ncol = 5)
  guess_matrix_index <- matrix(nrow = n_tries, ncol = 5)
  candidate_set <- wordle_word_list_matrix
  
  # The Game
  
  for (current_row in 1:(n_tries + 1)) {
    # DEBUG
    # guess_word <- 'allll'
    # guess_matrix_letter[1,] <- strsplit(guess_word,'')[[1]]
    # DEBUG
    
    # Suggest Word
    if (current_row == 1) {
      guess_word = starter_words[1]
      guess_matrix_letter[1,] <- strsplit(guess_word, '')[[1]]
      
      interstial_letter_count_keeper <- target_word_letter_count
      interstial_letter_count_keeper$current_row_count <- 0
    } 
    else if (current_row == 2) {
      guess_word = starter_words[2]
      guess_matrix_letter[2,] <- strsplit(guess_word, '')[[1]]
      interstial_letter_count_keeper$current_row_count <- 0
    }
    # else if (current_row == 3) {
    #   guess_word = starter_words[3]
    #   guess_matrix_letter[3,] <- strsplit(guess_word, '')[[1]]
    #   interstial_letter_count_keeper$current_row_count <- 0
    # }
    else {
      guess_word <-
        word_giver(guess_matrix_letter, guess_matrix_index, current_row, past_guesses, verbose, candidate_set)
      # guess_word <- word_giver_ai(guess_matrix_letter, guess_matrix_index, current_row)
      
      guess_matrix_letter[current_row,] <-
        strsplit(guess_word, '')[[1]]
      
      interstial_letter_count_keeper$current_row_count <- 0
    }
    
    past_guesses <- append(past_guesses, guess_word)
    
    if(verbose ==T) {
      print(paste(guess_word, target_word))
    }
    
    if (guess_word == target_word) {
      # result <- c(current_row, target_word)
      return(current_row)
    }
    
    # Green detection
    for (i in 1:5) {
      guess_matrix_index[current_row, i] <-
        ifelse(target_word_split[i] == guess_matrix_letter[current_row, i],
               'green',
               'grey')
      
      if (guess_matrix_index[current_row, i] == 'green') {
        letter_index <-
          which(interstial_letter_count_keeper$letter == guess_matrix_letter[current_row, i])
        interstial_letter_count_keeper$current_row_count[letter_index] = interstial_letter_count_keeper$current_row_count[letter_index] + 1
      }
    }
    
    # Yellow detection
    
    for (i in 1:5) {
      guess_letter <- guess_matrix_letter[current_row, i]
      if (guess_matrix_index[current_row, i] == 'green') {
        next
      } else if (str_detect(pattern = paste0('(?=.*',
                                             guess_matrix_letter[current_row, i],
                                             ')'),
                            string = target_word)) {
        letter_index <-
          which(interstial_letter_count_keeper$letter == guess_letter)
        if (interstial_letter_count_keeper$count[letter_index] == interstial_letter_count_keeper$current_row_count[letter_index]) {
          next
        } else {
          guess_matrix_index[current_row, i] <- 'yellow'
        }
      }
    }
    
    # DEBUG
    # guess_matrix_index
    # DEBUG
  }
}
