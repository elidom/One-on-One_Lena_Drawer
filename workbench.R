
# simulate a dataset of the type I want to work with

library(tidyverse)


speaker <- rep(c("marcos", "nicky", "nariman", "anel"), each = 50)
listener <- rep(c("susan",  "linda", "marc", "karsten"), times = 50)
turn <- 1:200
misc <- runif(200, 0, 100)
comments <- rep("hello", 200)

(df <- tibble(turn, speaker, listener, misc, comments))

write_csv(df, "toy_dataframe_1.csv")

# # # # # # 

summ <- df %>% group_by(child_id, spkr, lang, age) %>% 
  count(recording)

table(summ$n)
