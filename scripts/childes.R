


# load the library
library(childesr)
library(dplyr)


word_list <- c("dog", 
               "doggy", 
               "eat", 
               "sit", 
               "sleep", 
               "drink",
               "hug",
               "kiss",
               "point",
               "stand",
               "play",
               "apple",
               "baby",
               "ball",
               "banana",
               "bird",
               "boat",
               "book",
               "bottle",
               "car",
               "carrots",
               "bat",
               "cookie",
               "cow",
               "cup",
               "duck",
               "fish",
               "foot",
               "frog",
               "hand",
               "horse",
               "juice",
               "lion",
               "milk",
               "monkey",
               "shoe",
               "sock",
               "spoon",
               "train",
               "truck")

d_prod <- get_tokens(collection = "Eng-NA",
                     age = c(12, 36),
                          role = "target_child",
                          token = word_list)




# get number of utterances per child, per word

prod_sum <- d_prod %>%
  group_by(gloss, corpus_name, target_child_age, speaker_id)%>%
  summarise(word_utt = n())


speaker_stats <- get_speaker_statistics(collection = "Eng-NA",
                                        age = c(12, 36),
                                        role = "target_child")


speaker_stats <- speaker_stats[,c("speaker_id", "target_child_age", "num_utterances", "num_tokens")]



prod_stats <- merge(prod_sum, speaker_stats, by = c("speaker_id", "target_child_age"))

# num_tokens is 0 for some children
# remove them, can't have those 0's!

prod_stats <- subset(prod_stats, num_tokens != 0)


prod_stats$token_ratio <- prod_stats$word_utt/prod_stats$num_tokens


prod_stats$gloss <- tolower(prod_stats$gloss)


sp <- prod_stats %>%
  group_by(gloss)%>%
  summarise(avg_token_ratio = mean(token_ratio, na.rm = T))





word_list <- as.data.frame(word_list)
word_list$order <- rownames(word_list)


dat <- merge(sp, word_list, by.x = "gloss", by.y = "word_list", all.y = T)

dat <- dat[,c("order", "gloss", "avg_token_ratio")]

dat$order <- as.numeric(dat$order)
