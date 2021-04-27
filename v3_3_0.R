
###
#
#     N O U N   I N F L E C T I O N
#
#     Ver 3.3.0 
#
#     https://github.com/franfranz/Noun_inflection_ITA
#
### 


#   Database with the forms of inflectional suffixes in Italian nouns 
#   
#   Merge word frequencies collected from Itwac (Baroni et al., 2009) 
#   and a list of morphologically tagged words (Morph-it!, Zanchetta & Baroni, 2005)


# clear ws
rm(list=ls())

# required packages: stringr, pryr, treemap # tidyverse, viridis



###
#                                   
#
#           S E T   I N P U T S  
#
#   
#
###

# ### #
#
#   INPUT REQUIRED: GRAPHICAL SETTINGS 
#

library(pryr)

## input required: graphical parameters 

#
# WARNING!! CHECK AGAIN THE PARAMETERS AFTER THE FIRST GRAPHS. 
# WHAT ARE THE MOST RELEVANT CONTRASTS TO HIGHLIGHT?? possibly type-token? 


# palette in colors - different hues for masc and femm, plur are darker than sing
# this is better for presentations: light colors are better recognizable than light colors in viridis
# col_fp = "#008B00" #green4       # "#00008B" # "blue4" 
# col_fs = "#43CD80" #seagreen3    # "#00B2EE" # "deepskyblue2" 
# col_mp = "#CD2990"  #maroon3     # "#CD2626" # "firebrick3"  
# col_ms = "#EEAEEE" #plum2        # "#FF8C00" # "darkorange"  

# palette in colors - viridis 
# resists grayscale transformation and is good for printing but lighter colors are not that visible in presentations
col_fp = viridisLite::viridis(8)[1]
col_fs = viridisLite::viridis(8)[2]
col_mp = viridisLite::viridis(8)[6]
col_ms = viridisLite::viridis(8)[7]

# palette in greyscale 
# col_fp = "#171717" #grey10
# col_fs = "#303030" #grey20
# col_mp = "#7D7D7D"  #grey50
# col_ms = "#B3B3B3" #grey70

pal_01=c(col_fp, col_fs, col_mp, col_ms)


# textures for better visualization in greyscale 

# angle
ang_fem_typ= 25 #90 
ang_mas_typ= 135 #180 
plot_ang_typ= c(ang_fem_typ, ang_fem_typ, ang_mas_typ, ang_mas_typ)

ang_fem_tok = 0
ang_mas_tok = 0
plot_ang_tok= c(ang_fem_tok, ang_fem_tok, ang_mas_tok, ang_mas_tok)

#density
dens_plu_typ = 85
dens_sin_typ = 35
plot_dens_typ=c(dens_plu_typ, dens_sin_typ)

dens_plu_tok = 100
dens_sin_tok = 100
plot_dens_tok = c(dens_plu_tok, dens_sin_tok)

#lwd_txtu_sin=1
#lwd_txtu_plu=1

# bar borders 
bar_bor1="#FFFFFF" #white
bar_bor2= "#4D4D4D" #grey30 

# legend settings
legendcontent=c("Fem. Plur.","Fem. Sing.","Masc. Plur.", "Masc. Sing.")
colcontent= c("fp", "fs", "mp", "ms")
myfavbty= "n"

# other constants
roundnum = 3


# ### #
#
#   INPUT REQUIRED: PATHS
#


# input directory
#inwd <- '/Users/valentinapescuma/Documents/PhD/ItalianMorphophonolgy'
inwd="C:\\Users\\FF\\Documents\\Analisi varie\\Italian Morphophonology\\noun_lists_v2.0.0"

# output directories
outwd=inwd
#graphwd= whre the graphs are stored 

# where the code is stored
#codewd=("C:\\Users\\FF\\Documents\\Analisi varie\\Italian Morphophonology\\main_morpheme_count\\Noun_inflection_ITA")


# filename of the itwac list currently in use
  # currently using the "raw" version
#thefilename_itwac="itwac_nouns_lemmas_raw_2_0_0.csv"

thefilename_itwac="itwac_nouns_lemmas_notail_2_0_0.csv"




###
#                                   
#
#           I M P O R T   a n d   P R E P R O C E S S   
#
#   
#
###

setwd(inwd)

# import complete ITWAC list of nouns ####
#itwac_all <- read.csv('itwac_nouns_lemmas_notail_2_0_0.csv', header=T, sep=',')#, encoding='UTF-8') itwac_nouns_lemmas_notail_2_0_0
#itwac_all <- read.csv('itwac_nouns_lemmas_notail_unique_utf8.csv', header=T, sep=',', encoding='UTF-8')


itwac_all = read.csv(thefilename_itwac, 
                     header = T,
                     sep=",",
                     enc="utf-8")

summary(itwac_all)
head(itwac_all, 20)
itwac_all$Freq <- as.integer(itwac_all$Freq)
unique(itwac_all$POS)=="NOUN"

# drop itwac POS column
itwac_all$POS=NULL

# import morphit####
morphit <- read.delim('morph-it_048.txt', 
                      header = F, 
                      sep='\t', 
                      enc = 'UTF-8')

colnames(morphit) <- c('form', 'lemma_morphit', 'POS')


# morphit$lemma_morphit=NULL # not sure this is the right move - try it out 
morphit$form <- as.character(morphit$form)

# get only forms tagged as nouns in morphit
morphit_nouns= morphit[morphit$POS=="NOUN-F:p"
                       |morphit$POS=="NOUN-F:s"
                       |morphit$POS=="NOUN-M:p"
                       |morphit$POS=="NOUN-M:s", ]


# MERGE RESOURCES
#

nouns_all=merge(itwac_all, morphit_nouns, by.x="Form", by.y = "form")
head(nouns_all)


# STRIP LAST GRAPHEME
#

nouns_all$lastchar=stringr::str_sub(nouns_all$Form, -1,-1)




###
#                                   
#
#           F I L T E R 
#
#   
#
###

# ### #
#
#   SPLIT across INFLECTIONAL FEATURES 
#

# a column with Gender tag (femm - masc) 
nouns_all$Gender=ifelse(grepl("NOUN-M:",nouns_all$POS),  "masc", "femm")
checkgender= ifelse(grepl("NOUN-F:",nouns_all$POS),  "femm", "masc")
summary(checkgender==nouns_all$Gender)
nouns_all$Gender=as.factor(nouns_all$Gender)

# a column with Number tag (plur - sing) 
nouns_all$Number=ifelse(grepl(":p",nouns_all$POS),  "plur", "sing")
checknumber= ifelse(grepl(":s",nouns_all$POS),  "sing", "plur")
summary(checknumber==nouns_all$Number)
nouns_all$Number=as.factor(nouns_all$Number)

nouns_all$POS=as.factor(nouns_all$POS)
postab=table(nouns_all$POS)

#
## Graph: number of types in each feature: all nouns #--------------- mygraph_num_allnouns_types  
# 

mygraph_num_allnouns_types %<a-% { 
  barplot(postab, 
        main = "Number of noun types \n per inflectional feature",
        ylab = "Frequency",
        ylim = c(0,12000),
        col=pal_01, 
        border = bar_bor1, 
        density= plot_dens_typ,
        angle= plot_ang_typ)
}
mygraph_num_allnouns_types

# forms tagged as Feminine
# forms tagged as Masculine
# forms tagged as Plural
# forms tagged as Singular

# forms tagged as Feminine Plural
alln_fp= nouns_all[nouns_all$POS=="NOUN-F:p", ]
summary(alln_fp)
alln_fp$POS=as.factor(as.character(alln_fp$POS))

## forms tagged as Feminine Singular
alln_fs= nouns_all[nouns_all$POS=="NOUN-F:s", ]
summary(alln_fs)
str(alln_fs$FREQ)
alln_fs$POS=as.factor(as.character(alln_fs$POS))

# forms tagged as Masculine Plural
alln_mp= nouns_all[nouns_all$POS=="NOUN-M:p", ]
summary(alln_mp)
alln_mp$POS=as.factor(as.character(alln_mp$POS))

# forms tagged as Masculine Singular
alln_ms= nouns_all[nouns_all$POS=="NOUN-M:s", ]
summary(alln_ms)
alln_ms$POS=as.factor(as.character(alln_ms$POS))


###
#                                   
#
#            F O R M   to  F E A T U R E  correspondence
#
#   
#
###

###
# CORRESPONDENCE FORM- FEATURES
#


# distribution of forms within features (in non-invariants) what are the graphemes for ms? etc.

# distribution of features for each form. if i see "-a", what feature could it be? fs? ms? ...



# ### #
#
#   DETECT INVARIANT/AMBIGUOUS FORMS 
#


# AMB - 01 - - - "portavoce"
#

# what nouns are present with the same form in the four features ("portavoce", reporter")
all_nouns_amb01fp=alln_fp[(alln_fp$Form %in% alln_fs$Form==T)
                         &(alln_fp$Form %in% alln_mp$Form==T)
                         &(alln_fp$Form %in% alln_ms$Form==T), ]

all_nouns_amb01fs=alln_fs[(alln_fs$Form %in% alln_fp$Form==T)
                         &(alln_fs$Form %in% alln_mp$Form==T)
                         &(alln_fs$Form %in% alln_ms$Form==T), ]

all_nouns_amb01mp=alln_mp[(alln_mp$Form %in% alln_fp$Form==T)
                         &(alln_mp$Form %in% alln_fs$Form==T)
                         &(alln_mp$Form %in% alln_ms$Form==T), ]

all_nouns_amb01ms=alln_ms[(alln_ms$Form %in% alln_fp$Form==T)
                         &(alln_ms$Form %in% alln_fs$Form==T)
                         &(alln_ms$Form %in% alln_mp$Form==T), ]

summary((unique(all_nouns_amb01fp$Form)==unique(all_nouns_amb01fs$Form) ) 
        & (unique(all_nouns_amb01fp$Form))== (unique(all_nouns_amb01ms$Form))
        & (unique(all_nouns_amb01fp$Form))== (unique(all_nouns_amb01mp$Form))
)
# total token frequency of all_nouns_amb01
sum(all_nouns_amb01fp$Freq)

# bind all the nouns in a df
all_nouns_amb01=rbind(all_nouns_amb01fp, all_nouns_amb01fs, all_nouns_amb01mp, all_nouns_amb01ms)
all_nouns_amb01$Amb=paste0("amb", "01")
all_nouns_amb01$Form_amb="fp_fs_mp_ms"

# inspect object
head(all_nouns_amb01)

# subtract the df from the df with all nouns
all_nouns_01=nouns_all[!(nouns_all$Form %in% all_nouns_amb01$Form), ]

# check whether any of the forms was left out or any form was wrongfully included 
# it ==T, everything is ok
length(nouns_all$Form) - length(all_nouns_01$Form)== length(all_nouns_amb01$Form)


# AMB - 02  - - - "none"
#

# what nouns are present with the same form in fp, fs, mp and not in ms
all_nouns_amb02fp=alln_fp[(alln_fp$Form %in% alln_fs$Form==T)
                         &(alln_fp$Form %in% alln_mp$Form==T)
                         &(alln_fp$Form %in% alln_ms$Form==F), ]

all_nouns_amb02fs=alln_fs[(alln_fs$Form %in% alln_fp$Form==T)
                         &(alln_fs$Form %in% alln_mp$Form==T)
                         &(alln_fs$Form %in% alln_ms$Form==F), ]

all_nouns_amb02mp=alln_mp[(alln_mp$Form %in% alln_fp$Form==T)
                         &(alln_mp$Form %in% alln_fs$Form==T)
                         &(alln_mp$Form %in% alln_ms$Form==F), ]

summary((unique(all_nouns_amb02fp$Form)==unique(all_nouns_amb02fs$Form) ) 
        & (unique(all_nouns_amb02fp$Form))== (unique(all_nouns_amb02mp$Form))
)

# total token frequency of all_nouns_amb02
sum(all_nouns_amb02fp$Freq)

# bind all the nouns in a df
all_nouns_amb02=rbind(all_nouns_amb02fp, all_nouns_amb02fs, all_nouns_amb02mp)
all_nouns_amb02$Amb=paste0("amb", "02")
all_nouns_amb02$Form_amb="fp_fs_mp"

# inspect object
head(all_nouns_amb02)

# subtract the df from all_nouns_01
all_nouns_02=all_nouns_01[!(all_nouns_01$Form %in% all_nouns_amb02$Form), ]

# check whether any of the forms was left out or any form was wrongfully included 
# it ==T, everything is ok
length(all_nouns_01$Form) - length(all_nouns_02$Form)== length(all_nouns_amb02$Form)



# AMB - 03 - - - "radio"
#

# what nouns are present with the same form in fp, fs, ms and not in mp
all_nouns_amb03fp=alln_fp[(alln_fp$Form %in% alln_fs$Form==T)
                         &(alln_fp$Form %in% alln_mp$Form==F)
                         &(alln_fp$Form %in% alln_ms$Form==T), ]

all_nouns_amb03fs=alln_fs[(alln_fs$Form %in% alln_fp$Form==T)
                         &(alln_fs$Form %in% alln_mp$Form==F)
                         &(alln_fs$Form %in% alln_ms$Form==T), ]

all_nouns_amb03ms=alln_ms[(alln_ms$Form %in% alln_fp$Form==T)
                         &(alln_ms$Form %in% alln_fs$Form==T)
                         &(alln_ms$Form %in% alln_mp$Form==F), ]

summary((unique(all_nouns_amb03fp$Form)==unique(all_nouns_amb03fs$Form) ) 
        & (unique(all_nouns_amb03fp$Form))== (unique(all_nouns_amb03ms$Form))
)

# total token frequency of all_nouns_amb03
sum(all_nouns_amb03fp$Freq)

# bind all the nouns in a df
all_nouns_amb03=rbind(all_nouns_amb03fp, all_nouns_amb03fs, all_nouns_amb03ms)
all_nouns_amb03$Amb=paste0("amb", "03")
all_nouns_amb03$Form_amb="fp_fs_ms"

# inspect object
head(all_nouns_amb03)

# subtract the df from from all_nouns_02
all_nouns_03=all_nouns_02[!(all_nouns_02$Form %in% all_nouns_amb03$Form), ]

# check whether any of the forms was left out or any form was wrongfully included 
# it ==T, everything is ok
length(all_nouns_02$Form) - length(all_nouns_03$Form)== length(all_nouns_amb03$Form)



# AMB - 04 - - - "marine"
#

# what nouns are present with the same form in fp, mp, ms and not in fs

all_nouns_amb04fp=alln_fp[(alln_fp$Form %in% alln_fs$Form==F)
                         &(alln_fp$Form %in% alln_mp$Form==T)
                         &(alln_fp$Form %in% alln_ms$Form==T), ]

all_nouns_amb04mp=alln_mp[(alln_mp$Form %in% alln_fp$Form==T)
                         &(alln_mp$Form %in% alln_fs$Form==F)
                         &(alln_mp$Form %in% alln_ms$Form==T), ]

all_nouns_amb04ms=alln_ms[(alln_ms$Form %in% alln_fp$Form==T)
                         &(alln_ms$Form %in% alln_fs$Form==F)
                         &(alln_ms$Form %in% alln_mp$Form==T), ]


summary((unique(all_nouns_amb04fp$Form)==unique(all_nouns_amb04mp$Form) ) 
        & (unique(all_nouns_amb04fp$Form))== (unique(all_nouns_amb04ms$Form))
)

# total token frequency of all_nouns_amb04
sum(all_nouns_amb04fp$Freq)

# bind all the nouns in a df
all_nouns_amb04=rbind(all_nouns_amb04fp, all_nouns_amb04mp, all_nouns_amb04ms)

# subtract the df from from all_nouns_03
all_nouns_04=all_nouns_03[!(all_nouns_03$Form %in% all_nouns_amb04$Form), ]


# check for duplicates
amb04fp_dup=  all_nouns_amb04fp[duplicated(all_nouns_amb04fp$Form)==T, ]
head(amb04fp_dup)
amb04mp_dup= all_nouns_amb04mp[duplicated(all_nouns_amb04mp$Form)==T, ]
head(amb04mp_dup)
amb04ms_dup= all_nouns_amb04ms[duplicated(all_nouns_amb04ms$Form)==T, ]
head(amb04ms_dup)

#remove duplicates
all_nouns_amb04mp=all_nouns_amb04mp[all_nouns_amb04mp$Form %in% amb04mp_dup$Form==F, ]

# total token frequency of all_nouns_amb04
sum(all_nouns_amb04fp$Freq)

# bind all the nouns in a df
all_nouns_amb04=rbind(all_nouns_amb04fp, all_nouns_amb04mp, all_nouns_amb04ms)
all_nouns_amb04$Amb=paste0("amb", "04")
all_nouns_amb04$Form_amb="fp_mp_ms"

# inspect object
head(all_nouns_amb04)

# subtract the amb and the duplicates df from all_nouns_03
all_nouns_04x=all_nouns_03[!(all_nouns_03$Form %in% all_nouns_amb04$Form), ]

all_nouns_04=all_nouns_04x[!(all_nouns_04x$Form %in% amb04mp_dup$Form), ]

#length of the vector of duplicate words= number of features * redup * number of words
lengdup04raw= length(amb04fp_dup$Form)+(length(amb04mp_dup$Form))
lengdup04mult=2
lengdup04=lengdup04raw*lengdup04mult

# check whether any of the forms was left out or any form was wrongfully included 
# it ==T, everything is ok
(length(all_nouns_03$Form) - length(all_nouns_04$Form)) == (length(all_nouns_amb04$Form) + lengdup04)



# AMB - 05 - - - "boa"
#

# what nouns are present with the same form in fs, mp, ms and not in fp


all_nouns_amb05fs=alln_fs[(alln_fs$Form %in% alln_fp$Form==F)
                         &(alln_fs$Form %in% alln_mp$Form==T)
                         &(alln_fs$Form %in% alln_ms$Form==T), ]

all_nouns_amb05mp=alln_mp[(alln_mp$Form %in% alln_fp$Form==F)
                         &(alln_mp$Form %in% alln_fs$Form==T)
                         &(alln_mp$Form %in% alln_ms$Form==T), ]

all_nouns_amb05ms=alln_ms[(alln_ms$Form %in% alln_fp$Form==F)
                         &(alln_ms$Form %in% alln_fs$Form==T)
                         &(alln_ms$Form %in% alln_mp$Form==T), ]


summary((unique(all_nouns_amb05fs$Form)==unique(all_nouns_amb05mp$Form) ) 
        & (unique(all_nouns_amb05fs$Form))== (unique(all_nouns_amb05ms$Form))
)

# total token frequency of all_nouns_amb05
sum(all_nouns_amb05fs$Freq)

# bind all the nouns in a df
all_nouns_amb05=rbind(all_nouns_amb05fs, all_nouns_amb05mp, all_nouns_amb05ms)

# subtract the df from from all_nouns_04
all_nouns_05=all_nouns_04[!(all_nouns_04$Form %in% all_nouns_amb05$Form), ]
all_nouns_amb05$Amb=paste0("amb", "05")
all_nouns_amb05$Form_amb="fs_mp_ms"

# inspect object
head(all_nouns_amb05)

# check whether any of the forms was left out or any form was wrongfully included 
# it ==T, everything is ok
length(all_nouns_04$Form) - length(all_nouns_05$Form)== length(all_nouns_amb05$Form)



# AMB - 06 - - - "analisi"
#

# what nouns are present with the same form in fp and fs and not in mp or ms 
all_nouns_amb06fp=alln_fp[(alln_fp$Form %in% alln_fs$Form==T)
                         &(alln_fp$Form %in% alln_mp$Form==F)
                         &(alln_fp$Form %in% alln_ms$Form==F)
                         , ]

all_nouns_amb06fs=alln_fs[(alln_fs$Form %in% alln_fp$Form==T)
                         &(alln_fs$Form %in% alln_mp$Form==F)
                         &(alln_fs$Form %in% alln_ms$Form==F), ]

summary((unique(all_nouns_amb06fp$Form)==unique(all_nouns_amb06fs$Form) ) )

length((all_nouns_amb06fp$Form))==length(summary((unique(all_nouns_amb06fp$Form)==unique(all_nouns_amb06fs$Form))))
length((all_nouns_amb06fs$Form))==length(summary((unique(all_nouns_amb06fp$Form)==unique(all_nouns_amb06fs$Form))))

# detect duplicates
amb06fp_dup=  all_nouns_amb06fp[duplicated(all_nouns_amb06fp$Form)==T, ]
head(amb06fp_dup)
amb06fs_dup= all_nouns_amb06fs[duplicated(all_nouns_amb06fs$Form)==T, ]
head(amb06fs_dup)

#remove duplicates
all_nouns_amb06fp=all_nouns_amb06fp[all_nouns_amb06fp$Form %in% amb06fp_dup$Form==F, ]
all_nouns_amb06fp=all_nouns_amb06fp[all_nouns_amb06fp$Form %in% amb06fs_dup$Form==F, ]

all_nouns_amb06fs=all_nouns_amb06fs[all_nouns_amb06fs$Form %in% amb06fs_dup$Form==F, ]
all_nouns_amb06fs=all_nouns_amb06fs[all_nouns_amb06fs$Form %in% amb06fp_dup$Form==F, ]


# total token frequency of all_nouns_amb06
sum(all_nouns_amb06fp$Freq)

# bind all the nouns in a df
all_nouns_amb06=rbind(all_nouns_amb06fp, all_nouns_amb06fs)

# subtract the amb and the duplicates df from all_nouns_05
all_nouns_06x=all_nouns_05[!(all_nouns_05$Form %in% all_nouns_amb06$Form), ]
all_nouns_06y=all_nouns_06x[!(all_nouns_06x$Form %in% amb06fp_dup$Form), ]
all_nouns_06z=all_nouns_06y[!(all_nouns_06y$Form %in% amb06fs_dup$Form), ]

# reduplicated word forms are discarded from the amb-06 and from the remaining words
all_nouns_06=all_nouns_06z
all_nouns_amb06$Amb=paste0("amb", "06")
all_nouns_amb06$Form_amb="fp_fs"

#length of the vector of duplicate words= number of features * redup * number of words
lengdup06raw= length(amb06fp_dup$Form)+(length(amb06fs_dup$Form))
lengdup06mult=3
lengdup06=lengdup06raw*lengdup06mult

# check whether any of the forms was left out or any form was wrongfully included 
# it ==T, everything is ok
(length(all_nouns_05$Form) - length(all_nouns_06$Form)) == (length(all_nouns_amb06$Form) + lengdup06)

# inspect object
head(all_nouns_amb06)



# AMB - 07 - - - "abitanti"
#

# what nouns are present with the same form in fp and mp and not in fs or ms 
all_nouns_amb07fp=alln_fp[(alln_fp$Form %in% alln_fs$Form==F)
                         &(alln_fp$Form %in% alln_mp$Form==T)
                         &(alln_fp$Form %in% alln_ms$Form==F)
                         , ]


all_nouns_amb07mp=alln_mp[(alln_mp$Form %in% alln_fp$Form==T)
                         &(alln_mp$Form %in% alln_fs$Form==F)
                         &(alln_mp$Form %in% alln_ms$Form==F), ]


summary((unique(all_nouns_amb07fp$Form)==unique(all_nouns_amb07mp$Form) ) )

length((all_nouns_amb07fp$Form))==length(summary((unique(all_nouns_amb07fp$Form)==unique(all_nouns_amb07mp$Form))))
length((all_nouns_amb07mp$Form))==length(summary((unique(all_nouns_amb07fp$Form)==unique(all_nouns_amb07mp$Form))))

# inequal length: some duplicate forms may be present 
amb07fp_dup=  all_nouns_amb07fp[duplicated(all_nouns_amb07fp$Form)==T, ]
head(amb07fp_dup)
amb07mp_dup= all_nouns_amb07mp[duplicated(all_nouns_amb07mp$Form)==T, ]
head(amb07mp_dup)

#remove duplicates
all_nouns_amb07fp=all_nouns_amb07fp[all_nouns_amb07fp$Form %in% amb07fp_dup$Form==F, ]
all_nouns_amb07fp=all_nouns_amb07fp[all_nouns_amb07fp$Form %in% amb07mp_dup$Form==F, ]

all_nouns_amb07mp=all_nouns_amb07mp[all_nouns_amb07mp$Form %in% amb07mp_dup$Form==F, ]
all_nouns_amb07mp=all_nouns_amb07mp[all_nouns_amb07mp$Form %in% amb07fp_dup$Form==F, ]


# total token frequency of all_nouns_amb07
sum(all_nouns_amb07fp$Freq)

# bind all the nouns in a df
all_nouns_amb07=rbind(all_nouns_amb07fp, all_nouns_amb07mp)

# inspect object
head(all_nouns_amb07)

# subtract the amb and the duplicates df from all_nouns_06
all_nouns_07x=all_nouns_06[!(all_nouns_06$Form %in% all_nouns_amb07$Form), ]
all_nouns_07y=all_nouns_07x[!(all_nouns_07x$Form %in% amb07fp_dup$Form), ]
all_nouns_07z=all_nouns_07y[!(all_nouns_07y$Form %in% amb07mp_dup$Form), ]

# reduplicated word forms are discarded from the amb-06 and from the remaining words
all_nouns_07=all_nouns_07z
all_nouns_amb07$Amb=paste0("amb", "07")
all_nouns_amb07$Form_amb="fp_mp"

#length of the vector of duplicate words= number of features * redup * number of words
lengdup07raw= length(amb07fp_dup$Form)+(length(amb07mp_dup$Form))
lengdup07mult=3
lengdup07=lengdup07raw*lengdup07mult

# check whether any of the forms was left out or any form was wrongfully included 
# it ==T, everything is ok
(length(all_nouns_06$Form) - length(all_nouns_07$Form)) == (length(all_nouns_amb07$Form) + lengdup07)

# inspect object
head(all_nouns_amb07)



# AMB - 8 - - - "signore"
#

# what nouns are present with the same form in fp and ms and not in fs or mp 
all_nouns_amb08fp=alln_fp[(alln_fp$Form %in% alln_fs$Form==F)
                         &(alln_fp$Form %in% alln_mp$Form==F)
                         &(alln_fp$Form %in% alln_ms$Form==T)
                         , ]

all_nouns_amb08ms=alln_ms[(alln_ms$Form %in% alln_fp$Form==T)
                         &(alln_ms$Form %in% alln_fs$Form==F)
                         &(alln_ms$Form %in% alln_mp$Form==F), ]


summary(unique(all_nouns_amb08fp$Form)==unique(all_nouns_amb08ms$Form))

# check for duplicate forms 
amb08fp_dup=  all_nouns_amb08fp[duplicated(all_nouns_amb08fp$Form)==T, ]
head(amb08fp_dup)
amb08ms_dup= all_nouns_amb08ms[duplicated(all_nouns_amb08ms$Form)==T, ]
head(amb08ms_dup)

# total token frequency of all_nouns_amb08
sum(all_nouns_amb08fp$Freq)

# bind all the nouns in a df
all_nouns_amb08=rbind(all_nouns_amb08fp, all_nouns_amb08ms)
all_nouns_amb08$Amb=paste0("amb", "08")
all_nouns_amb08$Form_amb="fp_ms"

# inspect object
head(all_nouns_amb08)

# subtract the amb and the duplicates df from all_nouns_07
all_nouns_08=all_nouns_07[!(all_nouns_07$Form %in% all_nouns_amb08$Form), ]

# check whether any of the forms was left out or any form was wrongfully included 
# it ==T, everything is ok
(length(all_nouns_07$Form) - length(all_nouns_08$Form)) == (length(all_nouns_amb08$Form))


# AMB - 9 - - - "sequestri"
#

# what nouns are present with the same form in fs and mp and not in fp or ms 

all_nouns_amb09fs=alln_fs[(alln_fs$Form %in% alln_fp$Form==F)
                          &(alln_fs$Form %in% alln_mp$Form==T)
                          &(alln_fs$Form %in% alln_ms$Form==F)
                          , ]

all_nouns_amb09mp=alln_mp[(alln_mp$Form %in% alln_fp$Form==F)
                          &(alln_mp$Form %in% alln_fs$Form==T)
                          &(alln_mp$Form %in% alln_ms$Form==F), ]



summary(unique(all_nouns_amb09fs$Form)==unique(all_nouns_amb09mp$Form))

# check for duplicate forms 
amb09fp_dup=  all_nouns_amb09fs[duplicated(all_nouns_amb09fs$Form)==T, ]
head(amb09fp_dup)
amb09ms_dup= all_nouns_amb09mp[duplicated(all_nouns_amb09mp$Form)==T, ]
head(amb09ms_dup)

# total token frequency of all_nouns_amb09
sum(all_nouns_amb09fs$Freq)

# bind all the nouns in a df
all_nouns_amb09=rbind(all_nouns_amb09fs, all_nouns_amb09mp)
all_nouns_amb09$Amb=paste0("amb", "09")
all_nouns_amb09$Form_amb="fs_mp"

# inspect object
head(all_nouns_amb09)

# subtract the amb and the duplicates df from all_nouns_08
all_nouns_09=all_nouns_08[!(all_nouns_08$Form %in% all_nouns_amb09$Form), ]

# check whether any of the forms was left out or any form was wrongfully included 
# it ==T, everything is ok
(length(all_nouns_08$Form) - length(all_nouns_09$Form)) == (length(all_nouns_amb09$Form))


# AMB - 10 - - - "abitante"
#

# what nouns are present with the same form in fs and ms and not in fp or mp 

all_nouns_amb10fs=alln_fs[(alln_fs$Form %in% alln_fp$Form==F)
                         &(alln_fs$Form %in% alln_mp$Form==F)
                         &(alln_fs$Form %in% alln_ms$Form==T)
                         , ]


all_nouns_amb10ms=alln_ms[(alln_ms$Form %in% alln_fp$Form==F)
                         &(alln_ms$Form %in% alln_fs$Form==T)
                         &(alln_ms$Form %in% alln_mp$Form==F), ]


summary((unique(all_nouns_amb10fs$Form)==unique(all_nouns_amb10ms$Form) ) )

# check for duplicate forms 
amb10fs_dup=  all_nouns_amb10fs[duplicated(all_nouns_amb10fs$Form)==T, ]
head(amb10fs_dup)
amb10ms_dup= all_nouns_amb10ms[duplicated(all_nouns_amb10ms$Form)==T, ]
head(amb10ms_dup)


# total token frequency of all_nouns_amb10
sum(all_nouns_amb10fs$Freq)

# bind all the nouns in a df
all_nouns_amb10=rbind(all_nouns_amb10fs, all_nouns_amb10ms)
all_nouns_amb10$Amb=paste0("amb", "10")
all_nouns_amb10$Form_amb="fs_ms"

# inspect object
head(all_nouns_amb10)

# subtract the amb and the duplicates df from all_nouns_09
all_nouns_10=all_nouns_09[!(all_nouns_09$Form %in% all_nouns_amb10$Form), ]

# check whether any of the forms was left out or any form was wrongfully included 
# it ==T, everything is ok
(length(all_nouns_09$Form) - length(all_nouns_10$Form)) == length(all_nouns_amb10$Form) 


# AMB - 11 - - - "quiz"
#

# what nouns are present with the same form in mp and ms and not in fp or mp 


all_nouns_amb11mp=alln_mp[(alln_mp$Form %in% alln_fp$Form==F)
                          &(alln_mp$Form %in% alln_fs$Form==F)
                          &(alln_mp$Form %in% alln_ms$Form==T)
                          , ]


all_nouns_amb11ms=alln_ms[(alln_ms$Form %in% alln_fp$Form==F)
                          &(alln_ms$Form %in% alln_fs$Form==F)
                          &(alln_ms$Form %in% alln_mp$Form==T), ]


summary((unique(all_nouns_amb11mp$Form)==unique(all_nouns_amb11ms$Form) ) )

# check for duplicate forms 
amb11mp_dup=  all_nouns_amb11mp[duplicated(all_nouns_amb11mp$Form)==T, ]
head(amb11mp_dup)
amb11ms_dup= all_nouns_amb11ms[duplicated(all_nouns_amb11ms$Form)==T, ]
head(amb11ms_dup)


#remove duplicates
all_nouns_amb11mp=all_nouns_amb11mp[all_nouns_amb11mp$Form %in% amb11mp_dup$Form==F, ]
all_nouns_amb11ms=all_nouns_amb11ms[all_nouns_amb11ms$Form %in% amb11ms_dup$Form==F, ]

# total token frequency of all_nouns_amb07
sum(all_nouns_amb11mp$Freq)

# bind all the nouns in a df
all_nouns_amb11=rbind(all_nouns_amb11mp, all_nouns_amb11ms)

# inspect object
head(all_nouns_amb11)


# subtract the amb and the duplicates df from all_nouns_10
all_nouns_11x=all_nouns_10[!(all_nouns_10$Form %in% all_nouns_amb11$Form), ]
all_nouns_11y=all_nouns_11x[!(all_nouns_11x$Form %in% amb11mp_dup$Form), ]
all_nouns_11z=all_nouns_11y[!(all_nouns_11y$Form %in% amb11ms_dup$Form), ]



# reduplicated word forms are discarded from the amb-11 and from the remaining words
all_nouns_11=all_nouns_11z

all_nouns_amb11$Amb=paste0("amb", "11")
all_nouns_amb11$Form_amb="mp_ms"

#length of the vector of duplicate words
lengdup11raw= length(all_nouns_amb11mp$Form)+(length(all_nouns_amb11ms$Form))

#
#   warning!! check length
#
# check whether any of the forms was left out or any form was wrongfully included 
(length(all_nouns_10$Form) - length(all_nouns_11$Form)) == (length(all_nouns_amb11$Form) +lengdup11raw*2)

# inspect object
head(all_nouns_amb11)

head(all_nouns_11)




###
#                                  
#          C O L L E C T E D  A M B I G U O U S   F O R M S  
#           


# bind together all ambiguous in a same object

amb_nouns_all_list= ls()[grep(("all_nouns_amb..$"), ls())]
amb_nouns_all=NULL

for (eachdf in amb_nouns_all_list){
  tempambdf=(eval(str2lang(eachdf)))
  amb_nouns_all=rbind(amb_nouns_all, tempambdf)
}

#
#   OUTPUT: 
#   a data frame with all "ambiguous types"
setwd(outwd)
 #write.csv(amb_nouns_all, "ambiguous_forms_ITA.csv")
setwd(inwd)
# how many types were discarded?

summary(nouns_all)
num_all_types= length(nouns_all$Form)
summary(amb_nouns_all)
num_amb_types= length(amb_nouns_all$Form)
num_amb_types
prop_amb_types=num_amb_types/num_all_types
prop_amb_types

# what is the most represented kind of "ambiguous" form
amb_nouns_all$Amb=as.factor(amb_nouns_all$Amb)
table(amb_nouns_all$Form_amb, amb_nouns_all$POS)
ambtabformchar=as.data.frame.matrix(table(amb_nouns_all$Form_amb, amb_nouns_all$POS))
colnames(ambtabformchar)=colcontent
ambtabformchar=  ambtabformchar[order(- ambtabformchar$fp, 
                                      - ambtabformchar$fs, 
                                      - ambtabformchar$mp,
                                      - ambtabformchar$ms), ]    
#
# distribution of discarded types per feature
#
ambtabformchar


#  how are the suffixes distributed?
table(amb_nouns_all$POS, amb_nouns_all$lastchar)
ambtabPOSchar=as.data.frame.matrix(table(amb_nouns_all$POS, amb_nouns_all$lastchar))
ambtabPOSchar_t=t(ambtabPOSchar)

colnames(ambtabPOSchar_t)=colcontent
ambtabPOSchar_t=as.data.frame(ambtabPOSchar_t)

#
# distribution of discarded types: suffixes per feature
#
ambtabPOSchar_t
#
#   graphs: the distributions of nouns x lastchar x feature
#



###
#                                   
#           U N A M B I G U O U S   F O R M S  
#

all_nouns_11$Amb=rep("no_amb")
all_nouns_11$Form_amb=rep("no_amb")

# forms tagged as Feminine Plural
unamb_fp= all_nouns_11[all_nouns_11$POS=="NOUN-F:p", ]
summary(unamb_fp)
unamb_fp$POS=as.factor(as.character(unamb_fp$POS))

## forms tagged as Feminine Singular
unamb_fs= all_nouns_11[all_nouns_11$POS=="NOUN-F:s", ]
summary(unamb_fs)
unamb_fs$POS=as.factor(as.character(unamb_fs$POS))

# forms tagged as Masculine Plural
unamb_mp= all_nouns_11[all_nouns_11$POS=="NOUN-M:p", ]
summary(unamb_mp)
unamb_mp$POS=as.factor(as.character(unamb_mp$POS))

# forms tagged as Masculine Singular
unamb_ms= all_nouns_11[all_nouns_11$POS=="NOUN-M:s", ]
summary(unamb_ms)
unamb_ms$POS=as.factor(as.character(unamb_ms$POS))

all_nouns_11$POS=as.factor(all_nouns_11$POS)
postab_unamb=table(all_nouns_11$POS)

#
## Graph: number of types in each feature: all nouns #--------------- mygraph_num_allnouns_types_unamb  
# 

mygraph_num_allnouns_types_unamb %<a-% { 
  barplot(postab_unamb, 
          main = "Number of unambiguous noun types \n per inflectional feature",
          ylab = "Frequency",
          ylim = c(0,12000),
          col=pal_01, 
          border = bar_bor1, 
          density= plot_dens_typ,
          angle= plot_ang_typ)
  legend("topleft",
         legend=legendcontent,
         col=pal_01, 
         border = bar_bor1, 
         density= plot_dens_typ,
         angle= plot_ang_typ,
         bty = myfavbty)
}

mygraph_num_allnouns_types_unamb

# comparison between graphs 
par(mfrow=c(1,2))
mygraph_num_allnouns_types
mygraph_num_allnouns_types_unamb

par(mfrow=c(1,1))


#
# Proportion of discarded noun forms
#

# all nouns
postab

# unambiguous nouns
postab_unamb=table(all_nouns_11$POS)
postab_unamb

postab_nouns_sumup=merge(as.data.frame(postab), as.data.frame(postab_unamb), by.x = "Var1", by.y="Var1")
colnames(postab_nouns_sumup)<-list("POS", "all_types", "unamb_types")
postab_nouns_sumup$discarded_types=postab_nouns_sumup$all_types-postab_nouns_sumup$unamb_types
postab_nouns_sumup$prop_discarded=round((postab_nouns_sumup$discarded_types/postab_nouns_sumup$all_types),roundnum)

# distribution of suffixes in unambiguous features 

###
#                                   
#           U N A M B I G U O U S   F O R M S  
#

head(all_nouns_11)
head(all_nouns_amb11)



#
#   Unambiguous - Feminine Plural
# 
head(unamb_fp)
unique(unamb_fp$lastchar)

## Graph: number of suffixes in each feature: unambiguous nouns #--------------- mygraph_num_types_unamb_fp 
# 
mygraph_num_types_unamb_fp%<a-% { 
barplot(table(unamb_fp$lastchar),
        beside=T,
        col=col_fp, 
        angle=ang_fem_typ,
        density=dens_plu_typ, 
        border = bar_bor1,
        main = "Unambiguous inflection: types \n Feminine Plural"
        )
}
mygraph_num_types_unamb_fp


#
#   Unambiguous - Feminine Singular
# 
head(unamb_fs)
unique(unamb_fs$lastchar)

## Graph: number of suffixes in each feature: unambiguous nouns #--------------- mygraph_num_types_unamb_fs 
# 

mygraph_num_types_unamb_fs%<a-% { 
barplot(table(unamb_fs$lastchar),
        beside=T,
        col=col_fs, 
        angle=ang_fem_typ,
        density=dens_sin_typ, 
        border = bar_bor1,
        main = "Unambiguous inflection: types \n Feminine Singular"
)
}
mygraph_num_types_unamb_fs


#
#   Unambiguous - Masculine Plural
# 
head(unamb_mp)
unique(unamb_mp$lastchar)

## Graph: number of suffixes in each feature: unambiguous nouns #--------------- mygraph_num_types_unamb_mp 
# 
mygraph_num_types_unamb_mp%<a-% { 
barplot(table(unamb_mp$lastchar),
        beside=T,
        col=col_mp, 
        angle=ang_mas_typ,
        density=dens_plu_typ, 
        border = bar_bor1,
        main = "Unambiguous inflection: types \n Masculine Plural"
)
}
mygraph_num_types_unamb_mp


#
#   Unambiguous - Masculine Singular
# 
head(unamb_ms)
unique(unamb_ms$lastchar)

## Graph: number of suffixes in each feature: unambiguous nouns #--------------- mygraph_num_types_unamb_ms 
# 
mygraph_num_types_unamb_ms%<a-% { 
barplot(table(unamb_ms$lastchar),
        beside=T,
        col=col_ms, 
        angle=ang_mas_typ,
        density=dens_sin_typ, 
        border = bar_bor1,
        main = "Unambiguous inflection: types \n Masculine Singular"
)
}
mygraph_num_types_unamb_ms

##
##  !!INSERT TOKEN AS WELL
##

###
#                                   
#
#            F E A T U R E   to   F O R M  correspondence
#
#   
#
###

###
# CORRESPONDENCE  FEATURES - FORMS
#

# distribution of features for each form. if i see "-a", what feature could it be? fs? ms? ...
char_to_feat=as.data.frame.matrix(table( all_nouns_11$lastchar, all_nouns_11$POS))
#colnames(char_to_feat)<-colcontent

barplot(as.matrix(t(char_to_feat)), col=pal_01)

barplot(as.matrix(t(char_to_feat[char_to_feat$fp> 50|char_to_feat$fs> 50|char_to_feat$mp> 50|char_to_feat$ms> 50, ])))
legend("topright", legendcontent, fill=pal_01)

barplot(as.matrix((char_to_feat[char_to_feat$fp> 50|char_to_feat$fs> 50|char_to_feat$mp> 50|char_to_feat$ms> 50, ])))


###
#                                   
#
#            I N F L E C T I O N A L   C L A S S E S  
#
#   
#
###

# how "regular" is inflection? 

#inflectional classes
#which lemma is in the classes

# Class A lemma in fs, fp, ms, mp and form in amb 1
# amb 2 is empty
# Class B - lemma in fs and fp, same form Invariant feminine 
# Invariant masculine 



# lemma in 
# AMB - 03 - - - "radio" # what nouns are present with the same form in fp, fs, ms and not in mp
# AMB - 04 - - - "marine"# what nouns are present with the same form in fp, mp, ms and not in fs
# AMB - 05 - - - "boa" # what nouns are present with the same form in fs, mp, ms and not in fp
# AMB - 06 - - - "analisi"# what nouns are present with the same form in fp and fs and not in mp or ms 
# AMB - 07 - - - "abitanti" # what nouns are present with the same form in fp and mp and not in fs or ms 
# AMB - 08 - - - "signore" # what nouns are present with the same form in fp and ms and not in fs or mp 
# AMB - 09 - - - "sequestri" # what nouns are present with the same form in fs and mp and not in fp or ms 
# AMB - 10 - - - "abitante"# what nouns are present with the same form in fs and ms and not in fp or mp 
# AMB - 11 - - - "quiz" # what nouns are present with the same form in mp and ms and not in fp or mp 


# ----

# lemma in fp and in fs and not in ms or mp
# lemma in 

# in c1form, and lemma fp in fs and not in mp, ms = feminine, number invariant (crisi) 
# in c2form, and lemma in c9 "abitante"
# in c3form, and lemma in mp and in fs "cameriere" a/i - 3 uscite, ms=fp
# in 
# in c7 and lemma = fp in fs, mp, ms
# in c9 and and lemma in mp and in fs "musicista" i/e - 3 uscite, ms=fs

#----


#---

# 2) what nouns in the fem plur are also present in the fem sing (Feminine - Number invariant)
# e.g. crisi




# 3) what nouns in the fem plur are also present in the masc plur (Plural - Gender invariant)
# e.g. 






# distributions of the discarded nouns (amb1 - should be even .25)
# Graph: proportion of discarded types in each feature: all nouns #--------------- mygraph_prop_disp_nouns_types  


library(treemap)

amb_table=as.data.frame(table(all_nouns_amb1$POS))
colnames(amb_table)<- list("POS", "Freq")
amb_table$prop=amb_table$Freq/sum(amb_table$Freq)
amb_table$prop=round(amb_table$prop, 3)

mygraph_amb1_types %<a-% {
  amb_table$label <- paste(legendcontent, " ", amb_table$prop)
  treemap(amb_table,
          
          # data
          index=c("label"),
          vSize="Freq",
          type="index",
          
          # Main
          title="Amb1 - Types",
          palette=pal_01,
          
          # Borders:
          border.col=c("white"),             
          border.lwds=1,                         
          
          # Labels
          fontsize.labels=20,
          fontcolor.labels="white",
          fontface.labels=1,            
          bg.labels=c("transparent"),              
          align.labels=c("left", "top"),                                  
          overlap.labels=0.5,
          inflate.labels=F      )                  # If true, labels are bigger when rectangle is bigger.
}
mygraph_amb1_types


# HOW MANY INVARIANTS 



















# -- DO NOT USE --#


# CAMERIERE
# subtract nouns whose forms are tagged as ambiguously related to more than one inflectional feature
morphit2=morphit_nouns[grep ("\\|", morphit_nouns$Lemma), ]

#APRIPISTA 4=

#MUSICISTA 2+1+1


# NUMBER INVARIANT
#

# GENDER INVARIANT
#


# subtract Lem_amb from nouns_all, and obtain a new df nouns_all2
nouns_all2= nouns_all[!(nouns_all$form %in% Lem_Amb_1$form), ]


###
#   Split across INFLECTIONAL FEATURES - All nouns
###

# count: frequency observed in all inflection (token)
# sum partials 
tokenfreq_alln_fs=sum(alln_fs$FREQ)
tokenfreq_alln_fp=sum(alln_fp$FREQ)
tokenfreq_alln_ms=sum(alln_ms$FREQ)
tokenfreq_alln_mp=sum(alln_mp$FREQ)
tokenfreq_total_alln=sum(datallnouns$FREQ)
tokenfreq_alln_fs+tokenfreq_alln_fp+tokenfreq_alln_ms+tokenfreq_alln_mp==tokenfreq_total_alln

#
# ENTROPY - Features  All nouns
#

# entropy - type - allnouns
freq_types_all=table(datallnouns$POS)/length(datallnouns$POS)
entropy_type_alln=-sum(freq_types_all*log2(freq_types_all))
entropy_type_alln=round(entropy_type_alln, 3)

# entropy - token - allnouns
freq_tokens_alln=c(tokenfreq_alln_fp, tokenfreq_alln_fs,tokenfreq_alln_mp, tokenfreq_alln_ms)/tokenfreq_total_alln
entropy_token_alln= -sum(freq_tokens_alln*log2(freq_tokens_alln))
entropy_token_alln=round(entropy_token_alln, 3)  



# Graph: proportion of types in each feature: all nouns #--------------- mygraph_prop_allnouns_types  

library(treemap)

prop_allnouns_types=as.data.frame(freq_types_all)
prop_allnounscols=c("POS", "Freq")
colnames(prop_allnouns_types)<- prop_allnounscols
prop_allnouns_types$Freq=round(prop_allnouns_types$Freq, 3)



#prop_allnouns_types$label <- paste(prop_allnouns_types$POS, prop_allnouns_types$Freq, sep = "\n")
mygraph_prop_allnouns_types %<a-% {
  prop_allnouns_types$label <- paste(legendcontent, " ", prop_allnouns_types$Freq)
  treemap(prop_allnouns_types,
          
          # data
          index=c("label"),
          vSize="Freq",
          type="index",
          
          # Main
          title="All nouns - Types",
          palette=pal_01,
          
          # Borders:
          border.col=c("white"),             
          border.lwds=1,                         
          
          # Labels
          fontsize.labels=20,
          fontcolor.labels="white",
          fontface.labels=1,            
          bg.labels=c("transparent"),              
          align.labels=c("left", "top"),                                  
          overlap.labels=0.5,
          inflate.labels=F      )                  # If true, labels are bigger when rectangle is bigger.
}
mygraph_prop_allnouns_types



# Graph: proportion of tokens in each feature: all nouns #--------------- mygraph_prop_allnouns_tokens 
prop_allnouns_token=as.data.frame(freq_tokens_alln)
prop_allnouns_token$Freq=prop_allnouns_token$freq_tokens_alln
prop_allnouns_token$POS=legendcontent
prop_allnouns_token$freq_tokens_alln=NULL

prop_allnouns_token$Freq=round(prop_allnouns_token$Freq, 3)

mygraph_prop_allnouns_tokens %<a-% {
  #prop_allnouns_token$label <- paste(prop_allnouns_token$POS, prop_allnouns_token$Freq, sep = "\n")
  prop_allnouns_token$label <- paste(legendcontent, " ", prop_allnouns_token$Freq)
  treemap(prop_allnouns_token,
          
          # data
          index=c("label"),
          vSize="Freq",
          type="index",
          
          # Main
          title="All nouns - Tokens",
          palette=pal_01,
          
          # Borders:
          border.col=c("white"),             
          border.lwds=1,                         
          
          # Labels
          fontsize.labels=20,
          fontcolor.labels="white",
          fontface.labels=1,            
          bg.labels=c("transparent"),              
          align.labels=c("left", "top"),                                  
          overlap.labels=0.5,
          inflate.labels=F      )                 
}
mygraph_prop_allnouns_tokens


# Package
# plot to use instead of pie 

library(treemap)

prop_allnouns=as.data.frame(freq_types_all)
prop_allnounscols=c("POS", "Freq")
colnames(prop_allnouns)<- prop_allnounscols
prop_allnouns$Freq=round(prop_allnouns$Freq, 3)

# Plot

prop_allnouns$label <- paste(prop_allnouns$POS, prop_allnouns$Freq, sep = "\n")
prop_allnouns$label <- paste(prop_allnouns$POS, " ", prop_allnouns$Freq)
treemap(prop_allnouns,
        
        # data
        index=c("label"),
        vSize="Freq",
        type="index",
        
        # Main
        title="",
        palette=pal_01, 
        
        # Borders:
        border.col=c("white"),             
        border.lwds=1,                         
        
        # Labels
        fontsize.labels=20,
        fontcolor.labels="white",
        fontface.labels=1,            
        bg.labels=c("transparent"),              
        align.labels=c("left", "top"),                                  
        overlap.labels=0.5,
        inflate.labels=F      )                  # If true, labels are bigger when rectangle is bigger.

# print graphs
complete_graphlist=ls()[grep(("mygraph"), ls())]

