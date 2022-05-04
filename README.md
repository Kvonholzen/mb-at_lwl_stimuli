# mb-at_lwl_stimuli

The purpose of this repository is to provide code that can be used to generate information about potential stimuli and to make decisions about experimental design and stimuli selection. The code here works in tandem with the data in the Google Sheet "Word Properties" (https://docs.google.com/spreadsheets/d/1X_fSToEDGQ-jj03pY_xocKfT940LxmYPkoWkI4LDHRo/edit?usp=sharing).

## Stimuli_Acquisition_Data.Rmd

This Markdown file combines the information produced by the following scripts:


### wordbank.R

- Semantic Category
- OnCDI?
- AoA CDI production (50%)
- AoA CDI comprehension (50%)

*Note* the comprehension calculations with this script differ from that shown on the wordbank GUI. We will have to decide which values we would like to take (with the GUI values taking much longer to calculate!)

### childes.R

- Token Frequency CHILDES

### peekbank.R

Not yet!

- Peekbank LWL score 12-18
- Peekbank LWL score 18-24
- Peekbank LWL score 24+

*Note* Martin Zettersten did all the heavy lifting on this one! Here's his tutorial: https://mzettersten.github.io/peekbank-vignettes/peekbank_items/peekbank_item_vignette.html



## Vocab_Measures_Variation.Rmd

This Markdown file gathers the AoA data (from Wordbank) in the Word Properties file and calculates different types of stats, with the goal of using this information to choose the final set of words in our experiment.




