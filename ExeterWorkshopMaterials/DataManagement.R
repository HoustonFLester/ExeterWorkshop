# Description: Data Management from wide to long and vice versa
# Notes: This is important, because most data are in a wide format
# Cont.  and we will need the data to be in a long format. 

# Setup ----
  # install.packages("ggplot2")
  # install.packages("dplyr")
  # install.packages("mvtnorm")
  # install.packages("tidyr")
  # install.packages("lmertest")
  
  library(ggplot2)
  library(dplyr)
  library(mvtnorm)
  library(tidyr)

# Data Management wide -> long -> wide small example---- 
  # Some data for us to manipulate 
  
  
  dw <- read.table(header=T, text='
   pid O1_avg O2_avg O3_avg O4_avg  gender
     1   10    6     50     10      1
     2   12    5     70     11      1
     3   20    7     20     8       0
     4   22    8     22     9       0
   ')
  
  # Wide to long for a single variable 
  data_long <- dw %>%
    gather(key = var_label, value = O_long, O1_avg:O4_avg) %>%
    arrange(pid)
  
  # The code below can also be used with the "pipes - %>%", but it is much harder to read
  data_long2 <- arrange(gather(dw, key = var_label, value = O_long, O1_avg:O4_avg), pid)
  all.equal(data_long, data_long2) # Tests approximate equality. Long decimals can cause trouble if testing
                                   # equality using ==
    ## Aside
      .2 == .2
      weird1 <- .3 - .1 
      weird2 <- .9 - .7
      weird2 == weird1 # Don't use this to test equality

  # Long back to wide
  back_to_wide <- data_long %>%
    separate(var_label, into = c("Time", "Variable"), sep = "_") %>%
    spread(Time, O_long)

  # The wide dataset has slightly different variable names and variable ordering. We are changing them
  # back to make them match. 

  orig_names <- paste0(names(back_to_wide)[4:7], "_avg")
  names(back_to_wide)[4:7] <- orig_names
  
  back_to_original_wide <- back_to_wide %>%
    select(-3) %>%
    select("pid", 3:6, "gender")
  
  all.equal(back_to_original_wide, dw)

# A more realistic example ---- 

  # Starting from the Attitude example dataset provided in R
  
  data("attitude")
  View(attitude)
  attitude$id <- 1:30  

  # Adding some time varying or Level-1 variables to the data
  mean_vec <- matrix(data = 0, nrow = 25, ncol = 1)
  cov_matrix <- matrix(data = 0, nrow = 25, ncol = 25)
  diag(cov_matrix) <- 1

  tv_performance <- rmvnorm(n = 30, mean = mean_vec, sigma = cov_matrix)
  tv_performance <- as.data.frame(tv_performance)
  names(tv_performance)

  outcomes <- c("current_losses", "current_out", "misc", "projected_losses", "projected_out")
  times <- c(".t1", ".t2", ".t3", ".t4", ".t5")
  out_times_df <- expand.grid(times, outcomes)
  better_names <- paste0(out_times_df$Var2, out_times_df$Var1)
  names(tv_performance) <- better_names
  tv_performance$id <- 1:30
  attitude$id <- 1:30
  
  total_wide <- merge(attitude, tv_performance, by = "id")


  # Wide to long
  total_long <- total_wide %>% 
    gather(key = new_var_label, value = actual_data, current_losses.t1:projected_out.t5) %>% 
    separate(new_var_label, c("variable", "time"), sep = "\\.") %>% 
    arrange(id) %>% 
    spread(variable, actual_data)

  # Long to wide
  back_to_wide2 <- total_long %>%
    gather(key = another_id, value = new_dat, current_losses:projected_out) %>% 
    unite("to_spread", another_id, time, sep = ".") %>%
    spread(to_spread, new_dat)

  all.equal(back_to_wide2, total_wide)
  # Linear regression 

  rating_on_complaints <- lm(rating ~ complaints, data = total_wide)
  summary(rating_on_complaints)
