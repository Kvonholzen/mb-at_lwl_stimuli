



d_prod <- get_types(collection = childes_lang,
                     age = c(12, 36),
                          role = "target_child",
                          type = word_list)

speaker_stats <- get_speaker_statistics(collection = childes_lang,
                                        age = c(12, 36),
                                        role = "target_child")


# get number of utterances per child, per word
speaker_stats <- speaker_stats[,c("speaker_id", "target_child_age", "transcript_id", "num_utterances", "num_tokens")]

prod_stats <- merge(d_prod, speaker_stats, by = c("speaker_id", "target_child_age", "transcript_id"))


prod_stats <- prod_stats %>%
  #filter(num_tokens != 0) %>% #remove children w/o tokens, can't comput anything for them
  mutate(token_ratio = count/num_tokens)

if(childes_lang != "German"){

prod_stats$gloss <- tolower(prod_stats$gloss)

}

prod_stats <- prod_stats %>%
  group_by(gloss)%>%
  summarise(CHILDES_token_freq = mean(token_ratio, na.rm = T))



