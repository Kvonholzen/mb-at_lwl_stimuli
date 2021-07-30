# mb-at_lwl_stimuli

The purpose of this repository is to provide code that can be used to generate information about potential stimuli, to be added to the Google Sheet "Word Properties" (https://docs.google.com/spreadsheets/d/1X_fSToEDGQ-jj03pY_xocKfT940LxmYPkoWkI4LDHRo/edit?usp=sharing)

## wordbank.R

The 'wordbank.R' script can be adapted to calculate the following columns:

- Semantic Category
- OnCDI?
- AoA CDI production (50%)
- AoA CDI comprehension (50%)

*Note* the comprehension calculations with this script differ from that shown on the wordbank GUI. We will have to decide which values we would like to take (with the GUI values taking much longer to calculate!)


## peekbank.R

The 'peekbank.R' script can be adapted to calculate the following columns:

- Peekbank LWL score 12-18
- Peekbank LWL score 18-24
- Peekbank LWL score 24+

*Note* Martin Zettersten did all the heavy lifting on this one! Here's his tutorial: https://mzettersten.github.io/peekbank-vignettes/peekbank_items/peekbank_item_vignette.html


## childes.R

The 'childes.R' script can be adapted to calculate the following columns:

- Token Frequency CHILDES


## Coming Soon: Swaedesh.R





