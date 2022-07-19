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
                                    "Korean",
                                    "Korean",
                                                 "American Sign Language", 
                                    "Cantonese", 
                                    "Cantonese", 
                                    "Cantonese", 
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
                                    "Mandarin (Beijing)",
                                    "Mandarin (Beijing)",
                                    "Mandarin (Beijing)",
                                    "Mandarin (Beijing)",
                                    "Russian",
                                    "Russian",
                                                 "Slovak",
                                                 "Slovak",
                                                 "Spanish (Mexican)"),
            definition = c("(ìOì<ì\")ë¨¹ì-´", 
                    "ë½???ë½???í.´", 
                    "(ìOì<ì")ë¨¹ì-´",
                    "ë½???ë½???í.´",
                    "CAR/DRIVE", 
                    "ç§å®¶è»S",
                    "é£Yé£¯",
                    "çz"è¦º",
                    "ç§å®¶è»S",
                    "é£Yé£¯",
                    "çz\"æTè¦º/è¦ºè¦º",
                    "ham",
                    "sednout si",
                    "haji",
                    "haÄi",
                    "ballon",
                    "ballon",
                    "Î±Ï.Ï\"Î¿ÎºÎ¹Î½Î·Ï\"Î¬ÎºÎ¹",
                    "Î±Î¼Î¬Î¾Î¹",
                    "Ï???Î¹Î»ÏZ",
                    "××.×~×./ ×z×>×.× ×T×ª",
                    "favola/storia",
                    "Keti",
                    "Gari",
                    "çf",
                    "æ±½çf",
                    "ä¹¦",
                    "è½¦",
                    "åfé¥­",
                    "äº²ï¼^ä¸???ä¸ªï¼???",
                    "æ±½çf",
                    "çf",
                    "åfé¥­",
                    "äº²ï¼^ä¸???ä¸ªï¼???",
                    "ĞµÑÑ,ÑO",
                    "ĞµÑÑ,ÑO",
                    "lopta / gÃ³l",
                    "jesÅ¥",
                    "calcetÃ­n")
  )

sort.out$sort <- "out"
 
d.aoa <- merge(sort.out, dat.aoa, by = c("language", "definition"), all.y = T)

d.aoa$sort <- ifelse(d.aoa$language == "Hebrew" & d.aoa$uni_lemma == "car" & d.aoa$understands == "Inf", "out",
                     d.aoa$sort)


d.aoa <- d.aoa %>%
  filter(is.na(sort)) 

lang.sum <- d.aoa %>%
  group_by(language)%>%
  summarize(words = n_distinct(definition))


# indiv.lang <- d.aoa%>%
#   filter(language == "Mandarin (Beijing)")


save(d.aoa, file = "data/force_10.RData")



