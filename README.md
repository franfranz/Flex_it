# Flex_it
(REFERENCE)
Database of frequencies of word forms of italian nouns, annotated for morphological inflection, gender, number, inflectional class. 


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
* [Nouns](https://github.com/franfranz/Flex_it/blob/main/code/build_FLEXIT_nouns.R)
* [Adjectives](https://github.com/franfranz/Flex_it/blob/main/code/build_FLEXIT_adj.R)

### References

Flex_it is based on these lists of [nouns](https://github.com/franfranz/Word_Frequency_Lists_ITA/blob/main/itwac_nouns_lemmas_notail_2_0_0.csv) and [adjectives](https://github.com/franfranz/Word_Frequency_Lists_ITA/blob/main/itwac_adj_lemmas_notail_2_1_0.csv). The lists were obtained from data from [Morph-It!](https://docs.sslmit.unibo.it/doku.php?id=resources:morph-it) (Zanchetta & Baroni, 2005) and [ItWac](https://cqpweb.lancs.ac.uk/itwac/) (Baroni, Bernardini, Ferraresi & Zanchetta, 2009). 

 
 
