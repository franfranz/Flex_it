# Flex_it
Database of frequencies of word forms of italian nouns, annotated for morphological inflection, gender, number, inflectional class. The data were obrained from ItWac (Baroni, Bernardini, Ferraresi & Zanchetta, 2009) and Morph-It! (Zanchetta & Baroni, 2005). 
Please refer to () if you are using this database in your work. 

## Contents 
### Database
**NOUNS**

[Flex_it_nouns](https://github.com/franfranz/Noun_inflection_ITA/blob/main/Flex_ita_nouns.csv)  database, annotated for: \
-"Form", the word form \
-"Lemma", indication for lemma, derived from Morph-it! \         
-"Freq", raw token frequency, obtained from the ItWaC corpus \
-"Fpmw", token frequency (normalized per million tokens)\
-"Zipf", token frequency (on a Zipf scale)  \
-"POS", part of speech, also tagged for gender and number\
-"Baseform", lemma stripped from inflectional suffix (contains derivational affixes, where present)\
-"Gender", gender tag (fem. - masc.)\
-"Number", number tag (plur. - sing.)\
-"Form_amb", gender and number features in which homographs or invariant forms appear (as in _cameriere_-femm.plur / -masc.sing)\
-"Ending", inflectional suffix, or last grapheme of a word\
-"Inf_class", inflectional class, with indication of "ending of singular"_ "ending of plural"
 
**ADJECTIVES**

[Flex_it_adj](https://github.com/franfranz/Flex_it/blob/main/Flex_it_adj.csv)  database, annotated for: \
-"Form", the word form \
-"Lemma", indication for lemma, derived from Morph-it! \         
-"Freq", raw token frequency, obtained from the ItWaC corpus \
-"Fpmw", token frequency (normalized per million tokens)\
-"Zipf", token frequency (on a Zipf scale)  \
-"POS", part of speech, also tagged for gender and number\
-"Baseform", lemma stripped from inflectional suffix (contains derivational affixes, where present)\
-"Gender", gender tag (fem. - masc.)\
-"Number", number tag (plur. - sing.)\
-"Form_amb", gender and number features in which homographs or invariant forms appear (as in _cameriere_-femm.plur / -masc.sing)\
-"Ending", inflectional suffix, or last grapheme of a word\
-"Inf_class", inflectional class, with indication of "ending of singular Fem"_ "ending of plural Fem" / "ending of singular Masc"_ "ending of plural Masc"

### [Code](https://github.com/franfranz/Flex_it/tree/main/code)
Code used to build the annotated databases and its descriptive stats and graphs. 
* Nouns [v3.4.1](https://github.com/franfranz/Noun_inflection_ITA/blob/main/code/Flex_ita_nouns_v3_4_1.R)
* Adjectives 

