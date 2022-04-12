# mb-at_lwl_stimuli

The purpose of this repository is to provide code that can be used to generate information about potential stimuli, to be added to the Google Sheet "Word Properties" (https://docs.google.com/spreadsheets/d/1X_fSToEDGQ-jj03pY_xocKfT940LxmYPkoWkI4LDHRo/edit?usp=sharing)

<<<<<<< HEAD
# Update!

Currently this project is being updated to automatically calculate Wordbank and CHILDES information for each language in the Many Babies At Home Looking-While-Listening project.
=======
The 'wordbank.R' script can be adapted to calculate the following columns:
>>>>>>> parent of 7c4b723 (added scripts for childes and peekbank)

The RMarkdown file "Stimuli_Selection" currently calculates the minimum age of acquisition (AoA) for understanding and producing based on values reported in Wordbank as well as the token frequency for these words from CHILDES. It then creates a .csv file with the 8 words with the youngest and the oldest AoA (easy vs. hard) for each category:

<<<<<<< HEAD
- animals
- vehicles
- toys
- food_drink
- clothing
- body_parts
- furniture_rooms
- household
- outside
- action_words

This currently only works for American English, decisions need to be made in the group to see whether this is the way we want to move forward. It should be easily adaptable to other languages.


## To be added/decided:

- way of calculating AoA scores similar to those available in Wordbank for languages that aren't in Wordbank. Labs would/could be able to adapt their own lab-specific vocabulary data to estimate words.
- What to do for languages where CDI and CHILDES data are not available?
- which categories should we choose?


## peekbank.R

The 'peekbank.R' script can be adapted to calculate the following columns:

- Peekbank LWL score 12-18
- Peekbank LWL score 18-24
- Peekbank LWL score 24+

*Note* Martin Zettersten did all the heavy lifting on this one! Here's his tutorial: https://mzettersten.github.io/peekbank-vignettes/peekbank_items/peekbank_item_vignette.html






=======
*Note* the comprehension calculations with this script differ from that shown on the wordbank GUI. We will have to decide which values we would like to take (with the GUI values taking much longer to calculate!)
>>>>>>> parent of 7c4b723 (added scripts for childes and peekbank)
