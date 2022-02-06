# average guess rate per 1000 games

performance_history <- data.frame(game_num = 1:1000, guesses_needed = 0)

for (i in 1:nrow(performance_history)) {
  performance_history$guesses_needed[i] <- as.integer(run_wordle_simulation())
  # performance_history$guesses_needed[i] <- as.integer(result[1])
  # performance_history$target_word[i] <- result[2]
}

summary(as.integer(performance_history$guesses_needed))

microbenchmark(run_wordle_simulation(), times = 50)
profvis(run_wordle_simulation())


guess_matrix_letter <- matrix(nrow = n_tries, ncol = 5)
guess_matrix_index <- matrix(nrow = n_tries, ncol = 5)

guess_matrix_letter[1,] <- strsplit('aloes','')[[1]]
guess_matrix_letter[2,] <- strsplit('rutin','')[[1]]
guess_matrix_letter[3,] <- strsplit('spill','')[[1]]

guess_matrix_index[1,] <- c('grey', 'yellow', 'grey', 'grey', 'yellow')
guess_matrix_index[2,] <- c('grey', 'grey', 'grey', 'yellow', 'grey')
guess_matrix_index[3,] <- c('green', 'grey', 'green', 'green', 'green')

word_giver(guess_matrix_letter,
           guess_matrix_index,
           current_row = 4,
           past_guesses = c(''),
           verbose = T)
