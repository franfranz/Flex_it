# Noun_inflection_ITA
Database of frequencies of word forms of italian nouns, annotated for morphological inflection, gender, number, inflectional class. The data were obrained

## Contents 
### Database
The [database](https://github.com/franfranz/Noun_inflection_ITA/blob/main/Italian_Nouns_Inflection.csv) contains columns indicating: 
-"Form", the word form \
-"Lemma", indication for lemma, derived from Morph-it!       \         
-"Freq", raw token frequency, obtained from the ItWaC corpus \
-"Fpmw", token frequency (normalized per million tokens)\
-"Zipf", token frequency (on a Zipf scale)  \
-"POS", part of speech, also tagged for gender and number\
-"Baseform", lemma stripped from inflectional suffix (contains derivational affixes, where present)\
-"Gender", gender tag (fem. - masc.)\
-"Number", number tag (plur. - sing.)\
-"Form_amb", gender and number features in which homographs or invariant forms appear (as in "cameriere"-femm.plur / "cameriere"-masc.sing)\
-"Ending", inflectional suffix, or last grapheme of a word\
-"Inf_class", inflectional class, with indication of "ending of singular"_ "ending of plural"
 
### Code
Code used to create the annotated database. [v3.4.0](https://github.com/franfranz/Noun_inflection_ITA/blob/main/v3_4_0.R)
