
# get information about what forms are available in this language
admin_info <- get_administration_data(language = wordbank_lang)


form_list <- as.list(unique(admin_info$form))


# go through each form type and extract comprehension and production information
aoa.mat <- data.frame(matrix(NA, nrow=0, ncol=10))

for(i in 1:length(form_list)){
  
  f.data <- get_instrument_data(language = wordbank_lang,
                                form = form_list[i],
                                administrations = TRUE,
                                iteminfo = TRUE)

  
  #pare down to word_list
  f.data <- f.data %>%
     filter(uni_lemma %in% word_list) # only look at words in word_list
  
    
# Get Age of Acquisition

# At what age do 50% of children understand this word?    
  aoa.comp <- fit_aoa(f.data, 
          measure = "understands", 
          method = "empirical", 
          proportion = 0.5)

  aoa.comp$aoa_type <- "Wordbank_AoA_understands"

# At what age do 50% of children understand this word?    
  aoa.prod <- fit_aoa(f.data, 
                      measure = "produces", 
                      method = "empirical", 
                      proportion = 0.5)  
  
  aoa.prod$aoa_type <- "Wordbank_AoA_produces"
  
  aoa.cp <- rbind(aoa.comp, aoa.prod)
  
  aoa.cp$form <- form_list[i]
  
  aoa.mat <- rbind(aoa.mat, aoa.cp)
  
  
}




aoa.m <- aoa.mat %>%
  ungroup(num_item_id)%>%
  select(definition, category, lexical_class, uni_lemma, aoa_type, form, aoa) %>% # only columns we're interested in
  pivot_wider(names_from = form, values_from = aoa) ## make wider so that we have one column for each form


# identify the lowest AoA value for each word found in the forms
aoa.m <- aoa.m %>%
  nest(-definition, -category, -lexical_class, -uni_lemma, -aoa_type) %>% # nest rest of columns apart from id columns
  mutate(min_aoa = map(data, min, na.rm = T)) %>%  # identify the lowest AoA value, putting it in the column min_aoa
  unnest(cols = c(data, min_aoa)) %>%
  select(definition, category, lexical_class, uni_lemma, aoa_type, min_aoa)%>% # only columns we're interested in
  pivot_wider(names_from = aoa_type, values_from = min_aoa) %>% # make wider so we have one column for understands and one for produces
  filter(is.finite(Wordbank_AoA_understands)) # filter out the rows with Inf values, which result when both understands and produces don't have an AoA 
                                 #(because there is no point at which AoA is above 50% in the measured ages)

# 
# # for ease of matching up with other sets of data, remove qualifiers from definition and uni_lemma
# # object, food, animal, action, beverage
# aoa.m$definition <- sub("* \\(.*\\)", "", aoa.m$definition)
# 
# aoa.m$definition <- tolower(aoa.m$definition)
# 
# 
# # get list of words to be used with other databases
# # this is the translated list
list_de_mot <- pull(aoa.m, definition)  


#rm(list=setdiff(ls(), c("aoa.m", "word_list")))
