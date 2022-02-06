# average guess rate per 1000 games

performance_history <-
  data.frame(
    game_num = 1:1000,
    guesses_needed = 0,
    target_word = NA
  )

for (i in 1:nrow(performance_history)) {
  result <- run_wordle_simulation()
  # performance_history$guesses_needed[i] <-
    # as.integer(run_wordle_simulation())
  performance_history$guesses_needed[i] <- as.integer(result[1])
  performance_history$target_word[i] <- result[2]
}

summary(as.integer(performance_history$guesses_needed))

performance_history %>% group_by(guesses_needed) %>% summarize(n = n()) %>% mutate(ratio = n /
                                                                                     sum(n)) %>% filter(guesses_needed <= 6) %$% sum(ratio)

performance_history %>% filter(guesses_needed > 7)

# 3 & >3
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.000   3.000   4.000   3.967   4.000   9.000

# 4 & >10
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.000   3.000   4.000   3.938   4.000   8.000

# 4 & >5
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.000   3.000   4.000   3.955   4.000   7.000 

# 3 & >10
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.000   3.000   4.000   3.904   4.000   9.000 

# 5 & >3
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.000   3.000   4.000   3.959   4.000   8.000 

# Nothing at all
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.000   3.000   4.000   3.954   4.000   8.000 

# Ascending 3 >3
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.00    3.00    4.00    4.25    5.00    7.00 

# Ascending 5 >3
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.00    3.00    4.00    3.93    4.00    9.00 

run_wordle_simulation()
run_wordle_simulation(verbose = T)

microbenchmark(run_wordle_simulation(), times = 50)