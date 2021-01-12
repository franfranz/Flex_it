################################################################
# ITALIAN INFLECTIONAL MORPHOPHONOLOGY ########################
# PESCUMA FRANZON ZANINI #######################################
# v 2.1 09 JAN 2021 ############################################
################################################################

# clear environment

rm(list=ls())

#required packages: 


# ### #
#
#   INPUT REQUIRED: PATHS
#


#codewd
#codewd=("C:\\Users\\FF\\Documents\\Analisi varie\\Italian Morphophonology")

# input directory
#inwd <- '/Users/valentinapescuma/Documents/PhD/ItalianMorphophonolgy'
inwd="C:\\Users\\FF\\Documents\\Analisi varie\\Italian Morphophonology\\noun_lists_v2.0.0"

inwd=("/Users/valentinapescuma/Documents/PhD/ItalianMorphophonology")
# output directory 
#outwd=


# ### #
#
#   INPUT REQUIRED: GRAPHICAL SETTINGS 
#



# ### #
#
#   IMPORT AND PREPROCESS
#

setwd(inwd)

# import complete ITWAC list of nouns ####
itwac_all <- read.csv('itwac_nouns_lemmas_notail_2_0_0.csv', header=T, sep=',')#, encoding='UTF-8') itwac_nouns_lemmas_notail_2_0_0
#itwac_all <- read.csv('itwac_nouns_lemmas_notail_unique_utf8.csv', header=T, sep=',', encoding='UTF-8')


summary(itwac_all)
head(itwac_all, 20)
itwac_all$Freq <- as.integer(itwac_all$Freq)

# import morphit####
morphit <- read.delim('morph-it_048.txt', sep='\t', header = F, encoding = 'UTF-8')

colnames(morphit) <- c('form', 'lemma', 'POS')
morphit$form <- as.character(morphit$form)

table(morphit$POS)

# select nouns in morphit####
nouns_morphit <- morphit[morphit$POS %in% c('NOUN-F:s','NOUN-F:p','NOUN-M:s', 'NOUN-M:p'),]
nouns_morphit$POS <- factor(nouns_morphit$POS)
summary(nouns_morphit) #misura perdita occorrenze

# merge morphit and itwac nouns ####
nouns_all <- merge(nouns_morphit, itwac_all, by.x='form', by.y='Form') #33976
ourcols= c("form","lemma.y", "POS.x",  "Freq", "fpmw", "Zipf")
nouns_all <- nouns_all[ourcols]

thecolnames_nounsall=c("Form", "Lemma", "POS", "Freq_raw", "Fpmw", "Zipf")
colnames(nouns_all)=thecolnames_nounsall
summary(nouns_all)


# a column with Gender value
nouns_all$Gender=ifelse(grepl("NOUN-M:",nouns_all$POS),  "masc", "femm")
checkgender= ifelse(grepl("NOUN-F:",nouns_all$POS),  "femm", "masc")
summary(checkgender==nouns_all$Gender)
nouns_all$Gender=as.factor(nouns_all$Gender)

# a column with Number value
nouns_all$Number=ifelse(grepl(":p",nouns_all$POS),  "plur", "sing")
checknumber= ifelse(grepl(":s",nouns_all$POS),  "sing", "plur")
summary(checknumber==nouns_all$Number)
nouns_all$Number=as.factor(nouns_all$Number)

# output csv, that can work as a lighter input for what comes from now on
#write.csv(nouns_all, )

# use csv as input and start from here


# ### #
#
#   AMBIGUOUS FORM - FEATURE MAPPING 
#

#   How many noun forms (type and token) cannot be unambiguously related to one inflectional feature?

## Possible ambiguity:

# (1) ambiguous across lemmas "Lem_Amb" = (e.g., signore MS and FP; fine MS and FS) pairwise comparison: same form, different lemma 
#     more than one feature surfaces in the same form, but lemmas are different 
#     a. two lemmas: one with a |, for example cameriere|cameriera; the other one is cameriere, but we keep the form of the latter.
#     b. invariants for gender  but two distinct forms between numbers (abitante, abitanti). Lemma>=4, form>=2, Gender>=2, Number>=2

# (2) invariant (number) "Num_Inv" = (e.g., città ) pairwise comparison: same form, same lemma, same gender, different number 

# (3) invariant (gender) "Gen_Inv" = (e.g., musicista ) pairwise comparison: same form in the singular number, same lemma, different gender. Three forms for one lemma expected.

# (4) invariant (gender+number) "Full_Inv" = (e.g., capolista) pairwise comparison: same form, same lemma, different gender, different number 
#type "4" is mostly represented by compounds or loanwords denoting animates "apripista" "leader", "manager".
#(4b) is a subset of 4: mostly compounds that can be inflected or invariant "capogruppo"MP|"capigruppo"MP


# subset to try out the operations. comment when ready
nouns_allZ= nouns_all

# (1) Lem_Amb
# a. two lemmas: one with a |, for example cameriere|cameriera
# some are already tagged 
nouns_allY <- nouns_allZ

Lem_Amb_1_all=nouns_allY[grep ("\\|", nouns_allY$Lemma), ]

# check
head(Lem_Amb_1_all$Form,50)
table(Lem_Amb_1_all$Gender)
table(Lem_Amb_1_all$Number)

# subtract Lem_amb from nouns_allY, and obtain a new df nouns_allY
nouns_allY= nouns_allY[!(nouns_allY$Form %in% Lem_Amb_1_all$Form), ]

# nouns_allY$form=as.factor(nouns_allY$form)
summary(nouns_allY)

# # b. invariants for gender  but two distinct forms between numbers (abitante, abitanti). Lemma>=4, form>=2, Gender>=2, Number>=2
# same form, different lemma --> number of lemmas > number of forms & number of POS >= number of forms
Lem_Amb_2_all= nouns_allY[nouns_allY$Lemma %in% names(which(table(nouns_allY$Lemma) >= 4)) &
                        nouns_allY$Form %in% names(which(table(nouns_allY$Form) >= 2)) &
                        nouns_allY$Gender %in% names(which(table(nouns_allY$Gender) >= 2)) &
                        nouns_allY$Number %in% names(which(table(nouns_allY$Number) >= 2))  ,]

#check 
head(Lem_Amb_2_all)
table(Lem_Amb_2_all$Gender)
table(Lem_Amb_2_all$Number)

nouns_allY= nouns_allY[!(nouns_allY$Form %in% Lem_Amb_2_all$Form), ]

# (2) invariant (number) "Num_Inv" = (e.g., città ) pairwise comparison: same form, same lemma, same gender, different number 

Num_Inv_all = nouns_allY[ nouns_allY$Lemma %in% names(which(table(nouns_allY$Lemma) >= 2)) &
                               nouns_allY$Form %in% names(which(table(nouns_allY$Form) >= 2)) &
                               nouns_allY$Gender %in% names(which(table(nouns_allY$Gender) >= 2)) &
                               nouns_allY$Number %in% names(which(table(nouns_allY$Number) >= 1)),  ]

# check
head(Num_Inv_all)
table(Num_Inv_all$Gender)
table(Num_Inv_all$Number)

nouns_allX= nouns_allY[!(nouns_allY$Form %in% Num_Inv_all$Form), ]
1-nrow(nouns_allX)/nrow(nouns_allZ) # data loss from original merged dataset = 11.37% 

### NOTE: with Num_Inv_all we have already deleted all 16 items that we couldn't delete when splitting 
#into fem and masc subset (see below, now commented out), 
#which means that all the gender invariant nouns seem to have been taken care of by code above.
 
# # subsets with fem and masc nouns only
# # NB: this doesn't work well, as some nouns remain excluded 
# fem_nouns <- nouns_allY[nouns_allY$Gender == 'femm',]
# masc_nouns <- nouns_allY[nouns_allY$Gender == 'masc',]
# 
# Num_Inv_fem = fem_nouns[ fem_nouns$Lemma %in% names(which(table(fem_nouns$Lemma) >= 2)) &
#                             fem_nouns$form %in% names(which(table(fem_nouns$form) >= 2)) &
#                             fem_nouns$Number %in% names(which(table(fem_nouns$Number) >= 1)),  ]
# 
# Num_Inv_masc = masc_nouns[ masc_nouns$Lemma %in% names(which(table(masc_nouns$Lemma) >= 2)) &
#                            masc_nouns$form %in% names(which(table(masc_nouns$form) >= 2)) &
#                            masc_nouns$Number %in% names(which(table(masc_nouns$Number) >= 1)),  ]
# 
# All_Inv <- rbind(Num_Inv_fem, Num_Inv_masc)
# 
# nouns_allNew <- nouns_allY[!(nouns_allY$form %in% All_Inv$form), ]
# 
# All_Inv_2_sub <- All_Inv_2[All_Inv_2$form %in% names(which(table(All_Inv_2$form)==3)),]
# 
# setdiff(Num_Inv_all$form, All_Inv$form)
# 
# test <- Num_Inv_all[!(Num_Inv_all$form %in% All_Inv$form), ]

