library(tidyverse)

blp_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/isurr/refs/heads/isurr25/data/blp-trials-short.txt")

# dplyr verbs
# * select
# * rename
# * relocate
# * slice 
# * filter
# * mutate
# * summarize 
# Don't forge the pipe :) 

select(blp_df, lex, resp, rt)
blp_df2 <- select(blp_df, lex, resp, rt)
# blp_df <- select(blp_df, lex, resp, rt) # this is destructive .. 

select(blp_df, 1, 5, 3)
select(blp_df, participant:rt)
select(blp_df, 1:5)
select(blp_df, rt, 1:3)
select(blp_df, starts_with('r'))
select(blp_df, starts_with('rt'))
select(blp_df, ends_with('t'))
select(blp_df, contains('rt'))
select(blp_df, matches('^rt'))
select(blp_df, matches('rt$'))
select(blp_df, matches('^rt|rt$'))
select(blp_df, -participant)
select(blp_df, -starts_with('rt'))

relocate(blp_df, rt)
relocate(blp_df, starts_with('rt'))
relocate(blp_df, starts_with('rt'), .after = participant)

select(blp_df, ID = participant)
rename(blp_df, ID = participant)
rename(blp_df, ID = participant, reaction_time = rt)

slice(blp_df, c(1, 200, 250))
slice(blp_df, 1:15)
slice_head(blp_df, n = 15)
slice_tail(blp_df, n = 15)


filter(blp_df, rt > 500) # selecting rows where rt > 500
filter(blp_df, rt > 500, rt < 1000) 
filter(blp_df, rt >= 500, rt <= 1000) 

filter(blp_df, lex == 'W', rt >= 500, rt <= 1000)

blp_df3 <- mutate(blp_df, accuracy = lex == resp)
blp_df3 <- mutate(blp_df, accuracy = lex == resp, .after = resp)

mutate(blp_df, rt_speed = rt < median(rt, na.rm = TRUE))
mutate(blp_df, 
       rt_speed = if_else(rt < median(rt, na.rm = T), 
                          'fast', 
                          'slow'))

summarise(blp_df, avg_rt = mean(rt.raw), mad_rt = mad(rt.raw),
          accuracy = mean(lex == resp))

summarise(blp_df, avg_rt = mean(rt.raw), mad_rt = mad(rt.raw),
          accuracy = mean(lex == resp),
          .by = lex)

summarise(blp_df, avg_rt = mean(rt.raw), mad_rt = mad(rt.raw),
          accuracy = mean(lex == resp),
          .by = resp)

# The pipe ----------------------------------------------------------------

# |> native pipe
# %>% magrittr/tidyverse pipe

primes <- c(2, 3, 5, 7, 11, 13)
log(primes)
sqrt(log(primes))
sum(sqrt(log(primes)))
log(sum(sqrt(log(primes))))

log_primes <- log(primes)
sqrt_log_primes <- sqrt(log_primes)

primes |> log() # exactly the same as log(primes)
primes |> log() |> sqrt() |> sum() |> log()

# Data processing pipeline ------------------------------------------------

read_csv("https://raw.githubusercontent.com/mark-andrews/isurr/refs/heads/isurr25/data/blp-trials-short.txt") |> 
  mutate(accuracy = lex == resp) |> 
  select(ID = participant, lex, resp, rt = rt.raw, accuracy) |> 
  mutate(ID = as.factor(ID)) |> 
  drop_na() |> 
  summarize(avg_rt = mean(rt), sd_rt = sd(rt), .by = c(lex, accuracy))

