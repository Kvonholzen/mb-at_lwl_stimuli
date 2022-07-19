

# library(wordbankr)
# library(dplyr)
# library(tidyr)
# library(purrr)
# library(stringi)
# library(kableExtra)

# REMEMBER TO SET THE LANGUAGE BEING STUDIED!

# you can query how languages are labeled using get_instruments()
# gi <- get_instruments()
# View(gi)

# then set the language:
# wordbank_lang <- "English (American)"


# get information about what forms are available in this language
admin_info <- get_administration_data(language = wordbank_lang)
form_list <- as.list(unique(admin_info$form))


# go through each form type and extract comprehension and production information
aoa.mat <- data.frame(matrix(NA, nrow=0, ncol=10))

# use a for loop, easy to follow/explain
for(i in 1:length(form_list)){

# wordbankR function that queries Wordbank
# for this language, particular form (as we cycle through in the loop)
# we want both administrations and item info, so that we can calculate AoA
  f.data <- get_instrument_data(language = wordbank_lang,
                                form = form_list[i],
                                administrations = TRUE,
                                iteminfo = TRUE)
  
# Get Age of Acquisition

# At what age do 50% of children understand this word?
# this is calculated using the fit_aoa() function
  aoa.comp <- fit_aoa(f.data, 
          measure = "understands", 
          method = "empirical", 
          proportion = 0.5)

# give a label for comprehension 
  aoa.comp$aoa_type <- "Wordbank_AoA_understands"

# At what age do 50% of children produce this word? 
# this is calculated using the fit_aoa() function

  aoa.prod <- fit_aoa(f.data, 
                      measure = "produces", 
                      method = "empirical", 
                      proportion = 0.5)  
  
# give a label for production  
  aoa.prod$aoa_type <- "Wordbank_AoA_produces"
  
# bring comprehension and production data together  
  aoa.cp <- rbind(aoa.comp, aoa.prod)

# add information about what this particular form was called    
  aoa.cp$form <- form_list[i]

# add current data.frame back into matrix data.frame
  aoa.mat <- rbind(aoa.mat, aoa.cp)
  
  
}




aoa.m <- aoa.mat %>%
  # narrow down to just nouns and verbs, correct semantic categories
  filter(category == "food_drink" | category == "body_parts" | category == "action_words") %>% 
  ungroup(num_item_id)%>%
  # only columns we're interested in
  select(definition, category, lexical_class, uni_lemma, aoa_type, form, aoa) %>%
  # make wider so that we have one column for each form
  pivot_wider(names_from = form, values_from = aoa) 


# identify the lowest AoA value for each word found in the forms
aoa.m <- aoa.m %>%
  # nest rest of columns apart from id columns
  nest(-definition, -category, -lexical_class, -uni_lemma, -aoa_type) %>% 
  # identify the lowest AoA value, putting it in the column min_aoa
  mutate(min_aoa = map(data, min, na.rm = T)) %>%  
  unnest(cols = c(data, min_aoa)) %>%
  # only columns we're interested in
  select(definition, category, lexical_class, uni_lemma, aoa_type, min_aoa)%>% 
  # make wider so we have one column for understands and one for produces
  pivot_wider(names_from = aoa_type, values_from = min_aoa) %>% 
  # filter out the rows with Inf values, which result when both understands and produces don't have an AoA
  #(because there is no point at which AoA is above 50% in the measured ages)
  filter(is.finite(Wordbank_AoA_understands))  


# return potential body part words
body_parts <- aoa.m %>%
  filter(category == "body_parts")

# return potential food/drink words
food_drink <- aoa.m %>%
  filter(category == "food_drink")

# return potential verbs
verbs <- aoa.m %>%
  filter(category == "action_words")



