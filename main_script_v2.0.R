################################################################
# ITALIAN INFLECTIONAL MORPHPOPHONOLOGY ########################
# PESCUMA FRANZON ZANINI #######################################
# v 2.0 31 OCT 2020 ############################################
################################################################

# clear environment

rm(list=ls())

#codewd
#codewd=("C:\\Users\\FF\\Documents\\Analisi varie\\Italian Morphophonology")

# input directory
#inwd <- '/Users/valentinapescuma/Documents/PhD/ItalianMorphophonolgy'
inwd=("/Users/valentinapescuma/Documents/PhD/ItalianMorphophonology")
# output directory 
#outwd=

setwd(inwd)

# ### #
#
#   IMPORT AND PREPROCESS
#

# import complete ITWAC list of nouns ####
#itwac_all <- read.csv('itwac_nouns_lemmas_notail_unique_utf8.csv', header=T, sep=',', encoding='UTF-8')
itwac_all <- read.csv('itwac_nouns_lemmas_notail_unique.csv', header=T, sep=',')

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
nouns_all <- nouns_all[,c(1,3,4,5)]

summary(nouns_all)


# a column with Gender value
nouns_all$Gender=ifelse(grepl("NOUN-M:",nouns_all$POS.x),  "masc", "femm")
checkgender= ifelse(grepl("NOUN-F:",nouns_all$POS.x),  "femm", "masc")
summary(checkgender==nouns_all$Gender)
nouns_all$Gender=as.factor(nouns_all$Gender)

# a column with Number value
nouns_all$Number=ifelse(grepl(":p",nouns_all$POS.x),  "plur", "sing")
checknumber= ifelse(grepl(":s",nouns_all$POS.x),  "sing", "plur")
summary(checknumber==nouns_all$Number)
nouns_all$Number=as.factor(nouns_all$Number)
#nouns_all$Number=as.factor(nouns_all$Number)



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
#nouns_all=nouns_allZ

nouns_all=nouns_all[nouns_all$Lemma=="abitante"| nouns_all$Lemma=="città"| nouns_all$Lemma=="abate" |  nouns_all$Lemma=="banco" |nouns_all$form=="cameriere" |nouns_all$Lemma=="musicista" , ]
nouns_all

(nouns_all$Lemma)
nouns_all$Lemma=as.character(nouns_all$Lemma)
nouns_all$Lemma=as.factor(nouns_all$Lemma)
table(nouns_all$Lemma, nouns_all$form)

# Tag nouns based on the type of ambiguity

# (1) Lem_Amb
# a. two lemmas: one with a |, for example cameriere|cameriera
# some are already tagged 
Lem_Amb_1=nouns_all[grep ("\\|", nouns_all$Lemma), ]

# subtract Lem_amb from nouns_all, and obtain a new df nouns_all2
nouns_all2= nouns_all[!(nouns_all$form %in% Lem_Amb_1$form), ]

# nouns_all2$form=as.factor(nouns_all2$form)
summary(nouns_all2)

# b. invariants for gender  but two distinct forms between numbers (abitante, abitanti). Lemma>=4, form>=2, Gender>=2, Number>=2
# same form, different lemma --> number of lemmas > number of forms & number of POS >= number of forms 
Lem_Amb_2= nouns_all2[nouns_all2$Lemma %in% names(which(table(nouns_all2$Lemma) >= 4)) & 
                        nouns_all2$form %in% names(which(table(nouns_all2$form) >= 2)) &
                        nouns_all2$Gender %in% names(which(table(nouns_all2$Gender) >= 2)) &
                        nouns_all2$Number %in% names(which(table(nouns_all2$Number) >= 2))  ,]


nouns_all3= nouns_all2[!(nouns_all2$form %in% Lem_Amb_2$form), ]

# (2) invariant (number) "Num_Inv" = (e.g., città ) pairwise comparison: same form, same lemma, same gender, different number 
Lem_Amb_3= nouns_all3[(nouns_all3$Lemma %in% names(which(table(nouns_all3$Lemma) <= 2)) 
                       & nouns_all3$form %in% names(which(table(nouns_all3$form) == 2))) 
                      & nouns_all3$POS %in% names(which(table(nouns_all3$POS) >= 2))
                      ,  ]

nouns_all= nouns_all2[!(nouns_all2$form %in% Lem_Amb_2$form), ]
#banco, abate for control dataset
Lem_Amb_4= nouns_all2[(nouns_all2$Lemma %in% names(which(table(nouns_all2$Lemma) <= 2)) 
                       & nouns_all2$form %in% names(which(table(nouns_all2$form) == 1))) 
                      & nouns_all2$POS %in% names(which(table(nouns_all2$POS) >= 2))
                      ,  ]

Lem_Amb_3
Lem_Amb_4
# |nouns_all$Lemma %in% names(which((table(nouns_all$form) != 2 ), ]

#subtract Lem_Amb_2 from nouns_all2 and obtain a new df nouns_all3
nouns_all3= nouns_all2[!(nouns_all2$form %in% Lem_Amb_3$form), ]


# (2) invariant (number) "Num_Inv" = (e.g., città ) pairwise comparison: same form, same lemma, same gender, different number 

# same form, same lemma --> number of lemmas == number of forms & gender1==gender2 & Number1!=number2 

Num_Inv= nouns_all3[(nouns_all3$Lemma %in% names(which(table(nouns_all3$Lemma) >= 2)) 
                     & nouns_all3$form %in% names(which(table(nouns_all3$form) >= 2))) 
                    & nouns_all3$POS %in% names(which(table(nouns_all3$POS) >= 2))
                    & (
                      nouns_all3$Gender == "femm"  & nouns_all3$Number== "sing"  & nouns_all3$Gender == "femm"  & nouns_all3$Number== "plur" 
                    #& nouns_all3$Gender != "masc"  & nouns_all3$Number== "sing"  & nouns_all3$Gender != "masc"  & nouns_all3$Number== "plur"
                    )
                    ,  ]

Num_Inv= nouns_all3[(nouns_all3$Lemma %in% names(which(table(nouns_all3$Lemma) >= 2)) 
                     & nouns_all3$form %in% names(which(table(nouns_all3$form) >= 2))) 
                    & nouns_all3$POS %in% names(which(table(nouns_all3$POS) >= 2))
                    ,  ]





# nouns_all$form
# matr_ambi=NULL
# 
# for (i in nouns_all$form) {
#   formcol= grepl(i, nouns_all$Lemma) 
#   #colnames(formcol)= i
#   summary(formcol)
#   matr_ambi = cbind(formcol, matr_ambi) 
#   colnames(matr_ambi, nouns_all$form)
# }
# colnames(matr_ambi) <- paste (nouns_all$form, "form", sep="_")
# rownames(matr_ambi) <- paste( (rev(nouns_all$Lemma)), "lemma", sep = "_") 
#                               
# View(matr_ambi)
# 
# 
# 
# nouns_all$form_lemma <- paste(nouns_all$form, nouns_all$Lemma, sep = '_')


