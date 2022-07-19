
library(wordbankr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

# list of all languages in Wordbank
gi <- get_instruments()

# languages we have a stimuli set for
lang.list <- c("English (American)",
               "French (French)",
               "Korean",
               "German",
               "Norwegian",
               "Italian",
               "Spanish (European)")

####################################################
### get the AoA for the languages we've "done" so far
####################################################



# English words
Eng.list <- c("ball",
              "car",
              "sock",
              "banana",
              "book",
              "milk",
              "kiss",
              "sleep",
              "sit", # only shows up in Words & Sentences
              "eat")

w <- data.frame(lang = "English (American)", words = Eng.list)
words <- w

# French words
Fre.list <- c("balle",
              "voiture",
              "chaussettes", # changed from chaussette
              "banane",
              "livre",
              "biberon",
              "embrasser", # missing
              "dormir",
              "s'asseoir",
              "manger")

w <- data.frame(lang = "French (French)", words = Fre.list)
words <- rbind(words, w)

# Korean words
# Kor.list <- c("kong",
#               "cha",
#               "yangmal",
#               "panana",
#               "chek",
#               "uyu",
#               "p'op'ohe",
#               "ja",
#               "anja",
#               "meogeo")
Kor.list <- c("ball",
              "banana",
              "sock",
              "milk",
              "book",
              "car",
              "kiss",
              "sleep",
              "sit",
              "eat")


w <- data.frame(lang = "Korean", words = Kor.list)
words <- rbind(words, w)

# German words
Ger.list <- c("Ball",
              "Schuh",
              "Banane",
              "Milch",
              "Buch",
              "Auto",
              "k√ºssen", # k¸ssen
              "sitzen",
              "schlafen", # missing?????
              "essen")

w <- data.frame(lang = "German", words = Ger.list)
words <- rbind(words, w)

# Norwegian words
Nor.list <- c("ball",
              "bil",
              "sokker", # sokk
              "banan",
              "bok",
              "melk",
              "kysse",
              "sitte",
              "sove",
              "spise")
w <- data.frame(lang = "Norwegian", words = Nor.list)
words <- rbind(words, w)

# Italian words
Ita.list <- c("palla",
              "automobile", #macchina
              "scarpe", # scarpa?
              "banana",
              "libro",
              "calze", #calzino
              "baciare",
              "sedersi", #si siede
              "dormire",
              "mangiare")

w <- data.frame(lang = "Italian", words = Ita.list)
words <- rbind(words, w)

# Spanish words
Spa.list <- c("pelota",
              "cuchara",
              "	biber√≥n", #biberÛn
              "pl√°tano", #pl·tano
              "cuento/libro",
              "coche",
              "besar", #missing
              "sentar(se)", #sentarse
              "dormir(se)", #dormir
              "comer(se)") #come

w <- data.frame(lang = "Spanish (European)", words = Spa.list)
words <- rbind(words, w)


# for each language
# get the item info from "get_item_data"
# then load instrument data for those items
# then calculate AoA

# go through each form type and extract comprehension and production information
aoa.mat <- data.frame(matrix(NA, nrow=0, ncol=11))


dat.aoa <- data.frame(matrix(NA, nrow=0, ncol=7))


for (i in 1:length(lang.list)){
  
  wordbank_lang <- lang.list[i]
  
  word_set <- words %>%
    filter(lang == wordbank_lang)%>%
    select(words)
  
  word_set <- word_set$words
  
  # get information about what forms are available in this language
  admin_info <- get_administration_data(language = wordbank_lang)
  form_list <- as.list(unique(admin_info$form))
  
  
  for(ii in 1:length(form_list)){
    
    word_items <- get_item_data(language = wordbank_lang, form = form_list[ii])
    
    if(wordbank_lang == "Korean"){
      
      word_items <- word_items%>%
        filter(uni_lemma %in% word_set)
      
    } else {
      
      word_items <- word_items%>%
        filter(definition %in% word_set)
      
    }
    
    
    
    
    f.data <- get_instrument_data(language = wordbank_lang,
                                  form = form_list[ii],
                                  items = word_items$item_id,
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
    
    aoa.cp$form <- form_list[[ii]]
    aoa.cp$language <- wordbank_lang
    
    aoa.mat <- rbind(aoa.mat, aoa.cp)
  
} # different forms

  
  aoa.m <- aoa.mat %>%
    ungroup(num_item_id)%>%
    select(definition, category, lexical_class, uni_lemma, aoa_type, form, aoa, language) %>% # only columns we're interested in
    pivot_wider(names_from = form, values_from = aoa) ## make wider so that we have one column for each form
  
  
  # identify the lowest AoA value for each word found in the forms
  aoa.m <- aoa.m %>%
    nest(-definition, -category, -lexical_class, -uni_lemma, -aoa_type, -language) %>% # nest rest of columns apart from id columns
    mutate(min_aoa = map(data, min, na.rm = T)) %>%  # identify the lowest AoA value, putting it in the column min_aoa
    unnest(cols = c(data, min_aoa)) %>%
    select(definition, category, lexical_class, uni_lemma, aoa_type, min_aoa, language)%>% # only columns we're interested in
    pivot_wider(names_from = aoa_type, values_from = min_aoa) 
  
  # %>% # make wider so we have one column for understands and one for produces
  #   filter(is.finite(Wordbank_AoA_understands)) # filter out the rows with Inf values, which result when both understands and produces don't have an AoA 
  # #(because there is no point at which AoA is above 50% in the measured ages)
  
  
  
  dat.aoa <- rbind(aoa.m, dat.aoa)
  
  
} # different languages

dat.aoa <- unique(dat.aoa)




# sometimes too many words get included, should get rid of these

dat.aoa$sort <- ifelse(dat.aoa$language == "Korean" & dat.aoa$uni_lemma == "eat" & dat.aoa$definition == "(ÏùOÏ<ùÏù")Î®πÏ-¥", "out",
                       ifelse(dat.aoa$language == "Korean" & dat.aoa$uni_lemma == "kiss" & dat.aoa$definition == "ÎΩ???ÎΩ???Ì.¥", "out", "in"))

dat.aoa <- dat.aoa %>%
  filter(sort == "in")

dat.aoa %>%
  group_by(language)%>%
  summarize(words = n_distinct(definition))


plot.aoa <- dat.aoa %>%
  pivot_longer(cols = understands:produces,
    names_to = "mode",
    values_to = "AoA"
  )



ggplot(plot.aoa, aes(x = language, y = AoA, color = language)) +
  facet_wrap(.~mode)+
  geom_boxplot()+
  geom_jitter()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom")


####################################################
### get the AoA for main 10 across all Wordbank languages
####################################################

library(wordbankr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)


words <- c("ball",
              "car",
              "sock",
              "banana",
              "book",
              "milk",
              "kiss",
              "sleep",
              "sit", # only shows up in Words & Sentences
              "eat")



# list of all languages in Wordbank
gi <- get_instruments()

gi2 <- gi %>%
  filter(unilemma_coverage != 0)

lang.list <- unique(gi2$language)



# for each language
# get the item info from "get_item_data"
# then load instrument data for those items
# then calculate AoA

# go through each form type and extract comprehension and production information
aoa.mat <- data.frame(matrix(NA, nrow=0, ncol=11))


dat.aoa <- data.frame(matrix(NA, nrow=0, ncol=7))


for (i in 1:length(lang.list)){
  
  wordbank_lang <- lang.list[i]
  
  word_set <- words
  
  # get information about what forms are available in this language
  admin_info <- get_administration_data(language = wordbank_lang)
  form_list <- as.list(unique(admin_info$form))
  
  if(wordbank_lang == "English (British)"){
    
    form_list <- form_list[3] #only use Oxford CDI
    
  }
  
  
  for(ii in 1:length(form_list)){
    
    word_items <- get_item_data(language = wordbank_lang, form = form_list[ii])
    
      word_items <- word_items%>%
        filter(uni_lemma %in% word_set)

    
    
    
    f.data <- get_instrument_data(language = wordbank_lang,
                                  form = form_list[ii],
                                  items = word_items$item_id,
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
    
    aoa.cp$form <- form_list[[ii]]
    aoa.cp$language <- wordbank_lang
    
    aoa.mat <- rbind(aoa.mat, aoa.cp)
    
  } # different forms
  
  
  aoa.m <- aoa.mat %>%
    ungroup(num_item_id)%>%
    select(definition, category, lexical_class, uni_lemma, aoa_type, form, aoa, language) %>% # only columns we're interested in
    pivot_wider(names_from = form, values_from = aoa) ## make wider so that we have one column for each form
  
  
  # identify the lowest AoA value for each word found in the forms
  aoa.m <- aoa.m %>%
    nest(-definition, -category, -lexical_class, -uni_lemma, -aoa_type, -language) %>% # nest rest of columns apart from id columns
    mutate(min_aoa = map(data, min, na.rm = T)) %>%  # identify the lowest AoA value, putting it in the column min_aoa
    unnest(cols = c(data, min_aoa)) %>%
    select(definition, category, lexical_class, uni_lemma, aoa_type, min_aoa, language)%>% # only columns we're interested in
    pivot_wider(names_from = aoa_type, values_from = min_aoa) 
  
  # %>% # make wider so we have one column for understands and one for produces
  #   filter(is.finite(Wordbank_AoA_understands)) # filter out the rows with Inf values, which result when both understands and produces don't have an AoA 
  # #(because there is no point at which AoA is above 50% in the measured ages)
  
  
  
  dat.aoa <- rbind(aoa.m, dat.aoa)
  
  
} # different languages

dat.aoa <- unique(dat.aoa)

# sometimes too many words get included, should get rid of these

# English (British) is missing "sit"
# Norwegian is missing "sit"
# Swedish is missing "sit"

sort.out <- data.frame(language = c("Korean", 
                                    "Korean", 
                                    "American Sign Language", 
                                    "Cantonese", 
                                    "Cantonese", 
                                    "Cantonese", 
                                    "Czech",
                                    "Czech",
                                    "Czech",
                                    "Czech",
                                    "French (French)",
                                    "French (Quebecois)",
                                    "Greek (Cypriot)",
                                    "Greek (Cypriot)",
                                    "Hebrew",
                                    "Italian",
                                    "Kiswahili",
                                    "Kiswahili",
                                    "Mandarin (Beijing)",
                                    "Mandarin (Beijing)",
                                    "Mandarin (Beijing)",
                                    "Mandarin (Beijing)",
                                    "Mandarin (Beijing)",
                                    "Mandarin (Beijing)",
                                    "Russian",
                                    "Slovak",
                                    "Slovak",
                                    "Spanish (Mexican)"),
                       definition = c("(ÏùOÏ<ùÏù")Î®πÏ-¥", 
                                      "ÎΩ???ÎΩ???Ì.¥", 
                                      "CAR/DRIVE", 
                                      "ÁßÅÂÆ∂ËªS",
                                      "È£YÈ£Ø",
                                      "Áz"ÊTèË¶∫/Ë¶∫Ë¶∫",
                                      "ham",
                                      "sednout si",
                                      "haji",
                                      "haƒçi",
                                      "ballon",
                                      "ballon",
                                      "	Œ±œ.œ"ŒøŒ∫ŒπŒΩŒ∑œ"Œ¨Œ∫Œπ",
                                      "Œ±ŒºŒ¨ŒæŒπ",
                                      "◊ê◊.◊~◊./ ◊z◊>◊.◊ ◊T◊™",
                                      "favola/storia",
                                      "Keti",
                                      "Gari",
                                      "Áêf",
                                      "Ê±ΩÁêf",
                                      "‰π¶",
                                      "ËΩ¶",
                                      "ÂêfÈ•≠",
                                      "‰∫≤Ôº^‰∏???‰∏™Ôº???",
                                      "–µ—Å—,—O",
                                      "lopta / g√≥l",
                                      "jes≈•",
                                      "calcet√≠n")
)

sort.out$sort <- "out"

d.aoa <- merge(sort.out, dat.aoa, by = c("language", "definition"), all.y = T)







dat.aoa <- dat.aoa %>%
  filter(sort == "in")



save(dat.aoa, file = "data/All_Lang_10words.RData")


load("data/All_Lang_10words.RData")


aoa.sum <- dat.aoa %>%
  group_by(language)%>%
  summarize(words = n_distinct(definition))


# lang.sum <- dat.aoa %>%
#   filter(language == "Swedish")



vplot.aoa <- dat.aoa %>%
  pivot_longer(cols = understands:produces,
               names_to = "mode",
               values_to = "AoA"
  )



ggplot(plot.aoa, aes(x = language, y = AoA, color = language)) +
  facet_wrap(.~mode)+
  geom_boxplot()+
  geom_jitter()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom")
