
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
  
# Get Age of Acquisition

# At what age do 50% of children understand this word?    
  aoa.comp <- fit_aoa(f.data, 
          measure = "understands", 
          method = "empirical", 
          proportion = 0.5)

  aoa.comp$aoa_type <- "understands"

# At what age do 50% of children understand this word?    
  aoa.prod <- fit_aoa(f.data, 
                      measure = "produces", 
                      method = "empirical", 
                      proportion = 0.5)  
  
  aoa.prod$aoa_type <- "produces"
  
  aoa.cp <- rbind(aoa.comp, aoa.prod)
  
  aoa.cp$form <- form_list[i]
  
  aoa.mat <- rbind(aoa.mat, aoa.cp)
  
  
}


# narrow down to just nouns and verbs
# make wider so that we have one column for understands and one for produces
# for each form?
aoa.m <- aoa.mat %>%
  filter(lexical_class == "nouns" | lexical_class == "verbs") %>%
  pivot_wider(names_from = form, values_from = aoa)




# next:
# need some sort of function that will check the understand column
# and pick the "lower" aoa
# same for produces column
# "these columns after the column aoa_type"





# get list of words to be used with other databases
# this is "the list" that researchers will look at and judge
# will need to think about getting rid of information in parenthesis here
word_list <- list(unique(aoa.m$definition))
















# PRODUCTION DATA

id <- get_instrument_data(
  language = "English (American)",
  form = "WS",
  administrations = TRUE,
  iteminfo = TRUE
)

# get percent producing at each age, filtering for only ages where percent is greater than .50

id_prod <- id %>%
  dplyr::mutate(produces = value == "produces") %>%
  dplyr::group_by(age, definition, category, lexical_category) %>%
  dplyr::summarise(prod = sum(produces, na.rm = T),
                   completed = dplyr::n_distinct(data_id)) %>%
  dplyr::mutate(perc_producing = prod/completed)%>%
  dplyr::filter(perc_producing >= .50)


# get minimum age where percent producing is .50
idp <- id_prod %>%
  dplyr::group_by(definition, category, lexical_category) %>%
  dplyr::summarise(aoa50_prod = min(age, na.rm = T))
  

idp <- merge(idp, word_list, by.x = "definition", by.y = "word_list", all.y = T)



# COMPREHENSION DATA

id_c <- get_instrument_data(
  language = "English (American)",
  form = "WG",
  administrations = TRUE,
  iteminfo = TRUE
)

# get percent producing at each age, filtering for only ages where percent is greater than .50

id_comp <- id_c %>%
  dplyr::mutate(ifelse(value == "yes" | value == "understands" | value == "often" | value == "sometimes" |
                         value == "produces", "understands", "")) %>%
  dplyr::mutate(comprehends = value == "understands") %>%
  dplyr::group_by(age, definition, category, lexical_category) %>%
  dplyr::summarise(comp = sum(comprehends, na.rm = T),
                   completed = dplyr::n_distinct(data_id)) %>%
  dplyr::mutate(perc_comp = comp/completed)


# get minimum age where percent producing is .50
idc <- id_comp %>%
  dplyr::group_by(definition, category, lexical_category)%>%
  dplyr::filter(perc_comp >= .50)%>%
  dplyr::summarise(aoa50_comp = min(age, na.rm = T))


idc <- merge(idc, word_list, by.x = "definition", by.y = "word_list", all.y = T)



# bring together summary of comprehension and production


dat <- merge(idp, idc, by = c("definition", "order"), all = T)
dat$order <- as.numeric(dat$order)
