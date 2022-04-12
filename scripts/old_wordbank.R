
# [Default] [64-bit] C:\Users\user\Documents\R\R-3.6.1

#devtools::install_github("langcog/wordbankr")
library(wordbankr)
library(tidyr)



word_list <- c("dog", 
               "doggie", 
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
