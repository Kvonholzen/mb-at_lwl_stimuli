

# Load Word Properties google sheets


Italian <- read_sheet("https://docs.google.com/spreadsheets/d/1X_fSToEDGQ-jj03pY_xocKfT940LxmYPkoWkI4LDHRo/edit#gid=1227373847",
                      sheet = "Italian",
                      range = cell_cols(1:26),
                      na = "NA")

Spanish_euro <- read_sheet("https://docs.google.com/spreadsheets/d/1X_fSToEDGQ-jj03pY_xocKfT940LxmYPkoWkI4LDHRo/edit#gid=1227373847",
                           sheet = "Spanish (European)",
                           range = cell_cols(1:26),
                           na = "NA")


Norwegian <- read_sheet("https://docs.google.com/spreadsheets/d/1X_fSToEDGQ-jj03pY_xocKfT940LxmYPkoWkI4LDHRo/edit#gid=1227373847",
                           sheet = "Norwegian",
                        range = cell_cols(1:26),
                        na = "NA")


Polish <- read_sheet("https://docs.google.com/spreadsheets/d/1X_fSToEDGQ-jj03pY_xocKfT940LxmYPkoWkI4LDHRo/edit#gid=1227373847",
                        sheet = "Polish",
                     range = cell_cols(1:26),
                     na = "NA")


Dutch <- read_sheet("https://docs.google.com/spreadsheets/d/1X_fSToEDGQ-jj03pY_xocKfT940LxmYPkoWkI4LDHRo/edit#gid=1227373847",
                        sheet = "Dutch",
                    range = cell_cols(1:26),
                    na = "NA")


German <- read_sheet("https://docs.google.com/spreadsheets/d/1X_fSToEDGQ-jj03pY_xocKfT940LxmYPkoWkI4LDHRo/edit#gid=1227373847",
                        sheet = "German",
                     range = cell_cols(1:26),
                     na = "NA")


Korean <- read_sheet("https://docs.google.com/spreadsheets/d/1X_fSToEDGQ-jj03pY_xocKfT940LxmYPkoWkI4LDHRo/edit#gid=1227373847",
                        sheet = "Korean",
                     range = cell_cols(1:26),
                     na = "NA")


Hebrew <- read_sheet("https://docs.google.com/spreadsheets/d/1X_fSToEDGQ-jj03pY_xocKfT940LxmYPkoWkI4LDHRo/edit#gid=1227373847",
                        sheet = "Hebrew",
                     range = cell_cols(1:26),
                     na = "NA")

French <- read_sheet("https://docs.google.com/spreadsheets/d/1X_fSToEDGQ-jj03pY_xocKfT940LxmYPkoWkI4LDHRo/edit#gid=1227373847",
                     sheet = "French",
                     range = cell_cols(1:26),
                     na = "NA")


English_usa <- read_sheet("https://docs.google.com/spreadsheets/d/1X_fSToEDGQ-jj03pY_xocKfT940LxmYPkoWkI4LDHRo/edit#gid=1227373847",
                     sheet = "English (American)",
                     range = cell_cols(1:26),
                     na = "NA")




data <- as.data.frame(rbind(Italian, Spanish_euro,
              Norwegian, Polish,
              Dutch, German,
              Korean, Hebrew,
              French, English_usa))

data <- data[,c("Uni_Lemma", "Language", "Word_Lexeme", "Syntactic Category", 
                "AoA CDI production (50%)", "AoA CDI comprehension (50%)",
                "Token Frequency CHILDES")]


names(data) <- c("Uni_Lemma", "Language", "Word_Lexeme", "Syntactic_Category",
                 "Wordbank_Prod_AoA", "Wordbank_Comp_AoA", "CHILDES_Token_Freq")

data <- data %>%
  filter(!is.na(Language))



