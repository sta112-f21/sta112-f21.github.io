library(readr)
library(dplyr)
library(tidyr)

sfn <- read_csv("alex-sample.csv")

sfn <- sfn %>%
  filter(author_exp >= 0)

sfn <- sfn %>%
  mutate(forum_group = ifelse(forum_id %in% c(33, 35), "Homework",
                              ifelse(forum_id %in% c(6, 7, 8, 9, 10, 18, 
                                                     5, 60, 61, 4, 15, 20, 
                                                     52, 14, 104, 105, 36, 25, 56), "Science",
                                     ifelse(forum_id %in% c(38, 11, 12, 16, 88, 17), "Math",
                                            ifelse(forum_id %in% c(22, 19, 21, 23), "Medicine",
                                                   ifelse(forum_id %in% c(103, 102, 27, 3, 34), "Discussion",
                                                          ifelse(forum_id %in% c(29), "Speculation", "Misc")))))))


sfn <- sfn %>%
  mutate(age = as.numeric(as.Date(max(startdate)) - as.Date(startdate)))

sfn <- sfn %>%
  select(age, state, posts, views, duration, authors, 
         author_exp, deleted_posts, forum_group, author_banned) %>%
  mutate(author_banned = ifelse(author_banned == 1, 'banned', 'not banned'))

colnames(sfn) <- c('Age', 'State', 'Posts', 'Views',
                   'Duration', 'Authors', 'AuthorExperience',
                   'DeletedPosts', 'Forum', 'AuthorBanned')

write_csv(sfn, "sfn.csv")

