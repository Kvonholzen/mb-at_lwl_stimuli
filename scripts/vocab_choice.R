# principled output, per category
# first, is there Wordbank_AoA_understands info?
# if not, then look for Wordbank_AoA_produces
# if still not, then look for CHILDES_token_freq

d$source_measure <- ifelse(!is.na(measures$Wordbank_AoA_understands), "Wordbank_AoA_understands",
                           ifelse(!is.na(measures$Wordbank_AoA_produces), "Wordbank_AoA_produces",
                                  ifelse(!is.na(measures$CHILDES_token_freq), "CHILDES_token_freq",
                                         NA)))

d$source_variable <- ifelse(d$source_measure == "Wordbank_AoA_understands", d$Wordbank_AoA_understands,
                            ifelse(d$source_measure == "Wordbank_AoA_produces", d$Wordbank_AoA_produces,
                                   ifelse(d$source_measure == "CHILDES_token_freq", d$CHILDES_token_freq,
                                          NA)))



min_vocab <- d %>%
  group_by(category) %>%
  slice_min(source_variable, n=8)
min_vocab$vocab_category <- "easy"

# # optional?
# min_vocab2 <- min_vocab %>%
#   group_by(category) %>%
#   slice_min(Wordbank_AoA_produces, n=8)
#   



max_vocab <- d %>%
  group_by(category) %>%
  slice_max(source_variable, n=8)
max_vocab$vocab_category <- "hard"



vocab_sum <- rbind(min_vocab, max_vocab)
vocab_sum$language <- wordbank_lang
