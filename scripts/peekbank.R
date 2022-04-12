
# Demo with the data on item difficulty prepared by Martin Zettersten: 
# https://mzettersten.github.io/peekbank-vignettes/peekbank_items/peekbank_item_vignette.html 



# install.packages("remotes") # can also use devtools
# devtools::install_github("langcog/peekbankr")


FIRST_TIME = TRUE # set to true first time to download data from DB

library(peekbankr)
library(tidyverse)
library(here)
library(lme4)
library(lmerTest)
#library(cowplot)
#theme_set(theme_cowplot())

# #connect to the database
# con <- connect_to_peekbank()
# #get all of the tables you need
# datasets <- get_datasets(connection = con) %>% collect()
# administrations <- get_administrations(connection = con) %>% collect()
# subjects <- get_subjects(connection = con) %>% collect()
# aoi_timepoints <- get_aoi_timepoints(connection = con) %>% collect()
# stimuli <- get_stimuli(connection = con) %>% collect()
# trial_types <- get_trial_types(connection = con) %>% collect()
# trials <- get_trials(connection = con)  %>% collect()
# 
# aoi_data_joined <- aoi_timepoints %>%
#   right_join(administrations) %>%
#   right_join(subjects) %>%
#   right_join(trials) %>%
#   right_join(trial_types) %>%
#   right_join(datasets) %>%
#   mutate(stimulus_id = target_id) %>% #just joining in the target properties. Add a second join here if the distractor info is needed too
#   right_join(stimuli)
# 
# save(file = here("peekbank_items","data","aoi_data_joined.Rds"), aoi_data_joined)


load(file = here("peekbank_items","data","aoi_data_joined.Rds"))



#### PARAMETERS TO SET ####
#critical window dimensions roughly consistent with e.g., Swingley & Aslin, 2002
t_min <- 300
t_max <- 2000
#proportion missing trials threshold (any trial in which over half of the critical window missing is looking data is excluded )
max_prop_missing <- 0.5
#age bin size (number of months per bin)
age_bin_size <- 6


by_trial_means <- aoi_data_joined %>%
  #restrict to english datasets (this is just because there are so few non-English datasets atm)
  filter(native_language == "eng") %>%
  #restrict age range
  filter(age > 12, age <= 60) %>%
  # familiar target items only %>%
  filter(stimulus_novelty == "familiar") %>%
  #window of analysis
  filter(t_norm >= t_min, t_norm <= t_max) %>%
  #bin ages (can adjust size of age bins here)
  mutate(age_binned = cut(age, seq(12,60,age_bin_size))) %>%
  rename(target_label = english_stimulus_label) %>%
  group_by(dataset_name,subject_id, trial_id, target_label, 
           age, age_binned) %>%
  summarise(prop_target_looking = sum(aoi == "target", na.rm = TRUE) / 
              (sum(aoi == "target", na.rm=TRUE) + 
                 sum(aoi=="distractor", na.rm=TRUE)),
            prop_missing = mean(aoi %in% c("missing","other"), na.rm = TRUE)) %>%
  #remove trials with insufficient looking to target or distractor
  filter(prop_missing<=max_prop_missing)



by_subj_item_means <- by_trial_means %>%
  group_by(dataset_name,subject_id, target_label, 
           age, age_binned) %>%
  summarise(
    trial_num=n(),
    avg_target_looking = mean(prop_target_looking,na.rm=TRUE)
  )



by_subj_item_means$needed_bins <- ifelse(by_subj_item_means$age_binned == "(12,18]", "(12,18)",
                          ifelse(by_subj_item_means$age_binned == "(18,24]", "(18,24)", "(24,60)"))


# here I change Martin's script to average over dataset_name as well
by_item_means <- by_subj_item_means %>%
  #group_by(dataset_name, target_label,age_binned) %>%
  group_by(target_label,needed_bins) %>%
  summarise(
    target_looking = mean(avg_target_looking,na.rm=TRUE)
  )


# here starts Katie's mess


word_list <- c("dog", 
               "doggy", 
               "eat", 
               "sit", 
               "sleep", 
               "drink (action)",
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
               "fish (animal)",
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


word_list <- as.data.frame(word_list)
word_list$order <- rownames(word_list)


dat <- merge(by_item_means, word_list, by.x = "target_label", by.y = "word_list", all.y = T)


dat <- dat %>%
  pivot_wider(names_from = needed_bins, values_from = target_looking)

dat <- dat[,c("order", "target_label", "(12,18)", "(18,24)", "(24,60)")]

dat$order <- as.numeric(dat$order)
