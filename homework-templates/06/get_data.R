# remotes::install_github("ivan-rivera/RedditExtractor")
library(RedditExtractoR)


sprog_comments <- get_user_content("poem_for_your_sprog")
schnoodle_comments <- get_user_content("SchnoodleDoodleDo")

library(dplyr)
library(tidyr)
library(stringr)
poems <- sprog_comments$poem_for_your_sprog$comments %>%
  mutate(comment = str_replace_all(comment, "&gt;", "> ") %>%
           str_replace_all("\\031", "'"))

write.csv(poems, file = "sprog_poems-20230806.csv", row.names = F)

poems <- schnoodle_comments$SchnoodleDoodleDo$comments %>%
  mutate(comment = str_replace_all(comment, "&gt;", "> ") %>%
           str_replace_all("\\031", "'"))
write.csv(poems, file = "schnoodle_poems-20230806.csv", row.names = F)
