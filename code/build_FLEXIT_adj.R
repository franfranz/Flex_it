
### 
#
#     F L E X   I T 
#
#             +++ a d j e c t i v e s +++   
#
#     a large scale database of inflected wordforms in Italian 
#
#     https://github.com/franfranz/Flex_it 
#
### 



#   Database with the forms of inflectional suffixes in Italian adjectives 
#   
#
#
#  Merge word frequencies collected from Itwac (Baroni et al., 2009) and a list of morphologically tagged words (Morph-it!, Zanchetta & Baroni, 2005)
#  Please make sure you have the morph-it list and the adj_lemma list before proceeding. 
#  The adj_lemma list of word frequencies is available here (https://github.com/franfranz/Flex_it/tree/main/code)

# clear ws
rm(list=ls())

# filename of the itwac list currently in use
thefilename_itwac="itwac_adj_lemmas_notail_2_1_0.csv"

# required packages: stringr, pryr, xtable, viridis 



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
dens_plu_typ = 65
dens_sin_typ = 35
plot_dens_typ=c(dens_plu_typ, dens_sin_typ)

dens_plu_tok = 100
dens_sin_tok = 100
plot_dens_tok = c(dens_plu_tok, dens_sin_tok)

# bar borders 
bar_bor1="#FFFFFF" #white
bar_bor2= "#4D4D4D" #grey30 

# legend settings
legendcontent=c("Fem. Plur.","Fem. Sing.","Masc. Plur.", "Masc. Sing.")
colcontent= c("fp", "fs", "mp", "ms")
myfavbty= "n"

# other parameters 
# round digits
roundnum = 4


# ### #
#
#   INPUT REQUIRED: PATHS
#


# input directory

inwd=getwd()

# output directories

# to save database
outwd="path"

# to save graphs
graphwd="path"

# where the code is stored
codewd="path"


###
#                                   
#
#           I M P O R T   a n d   P R E P R O C E S S   
#
#   
#
###

setwd(inwd)


itwac_all = read.csv(thefilename_itwac, 
                     header = T,
                     sep=",",
                     enc="utf-8")

summary(itwac_all)
head(itwac_all, 20)
itwac_all$Freq <- as.integer(itwac_all$Freq)

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

morphit_adj=morphit[grep("^ADJ", morphit$POS), ]

unique(morphit_adj$POS)

# create tag for each grade of adj
morphit_adj$grade= stringr::str_sub(morphit_adj$POS,5, 7 ) 


# MERGE RESOURCES
#

adj_all=merge(itwac_all, morphit_adj, by.x="Form", by.y = "form")
head(adj_all)

adj_all$POS=stringr::str_sub(adj_all$POS, -3, -1)
adj_all$POS=stringr::str_sub(adj_all$POS, -3, -1)
adj_all$POS=stringr::str_replace_all(adj_all$POS, "\\+", "")

# STRIP LAST GRAPHEME
#

adj_all$lastchar=stringr::str_sub(adj_all$Form, -1,-1)

# STRIP UNINFLECTED "BASE"
# careful! This does not remove derivation and is not meaningful for invariant nouns
adj_all$baseform=stringr::str_sub(adj_all$lemma_morphit, 1,-2)


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
adj_all$Gender=ifelse(grepl("m",adj_all$POS),  "masc", "femm")
checkgender= ifelse(grepl("f",adj_all$POS),  "femm", "masc")
summary(checkgender==adj_all$Gender)
adj_all$Gender=as.factor(adj_all$Gender)

# a column with Number tag (plur - sing) 
adj_all$Number=ifelse(grepl("p",adj_all$POS),  "plur", "sing")
checknumber= ifelse(grepl("s",adj_all$POS),  "sing", "plur")
summary(checknumber==adj_all$Number)
adj_all$Number=as.factor(adj_all$Number)

adj_all$POS=as.factor(adj_all$POS)
postab=table(adj_all$POS)

#
## Graph: number of types in each feature: all nouns #--------------- mygraph_num_alladj_types  
# 

mygraph_num_alladj_types %<a-% { 
  barplot(postab, 
          main = "Number of adjective types \n per inflectional feature",
          ylab = "Frequency",
          ylim = c(0,12000),
          col=pal_01, 
          border = bar_bor1, 
          density= plot_dens_typ,
          angle= plot_ang_typ)
}
mygraph_num_alladj_types


# forms tagged as Feminine Plural
alln_fp= adj_all[adj_all$POS=="fp", ]
summary(alln_fp)
alln_fp$POS=as.factor(as.character(alln_fp$POS))

## forms tagged as Feminine Singular
alln_fs= adj_all[adj_all$POS=="fs", ]
summary(alln_fs)
str(alln_fs$FREQ)
alln_fs$POS=as.factor(as.character(alln_fs$POS))

# forms tagged as Masculine Plural
alln_mp= adj_all[adj_all$POS=="mp", ]
summary(alln_mp)
alln_mp$POS=as.factor(as.character(alln_mp$POS))

# forms tagged as Masculine Singular
alln_ms= adj_all[adj_all$POS=="ms", ]
summary(alln_ms)
alln_ms$POS=as.factor(as.character(alln_ms$POS))

nouns_all=adj_all

###
#                                   
#
#            F O R M   to  F E A T U R E  correspondence
#
#   
#
###

# ### #
#
#   DETECT INVARIANT/AMBIGUOUS FORMS 
#


# AMB - 01 - - - fp, fs, mp, ms
#
# what adj are present with the same form in the four features 

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


# AMB - 02  - - - fp, fs, mp
#
# what adj are present with the same form in fp, fs, mp and not in ms

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



# AMB - 03 - - - fp, fs, ms
#
# what adj are present with the same form in fp, fs, ms and not in mp

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



# AMB - 04 - - - fp, mp, ms
#
# what adj are present with the same form in fp, mp, ms and not in fs

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



# AMB - 05 - - - fs, mp, ms
#
# what adj are present with the same form in fs, mp, ms and not in fp


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



# AMB - 06 - - - fp, fs
#
# what adj are present with the same form in fp and fs and not in mp or ms 

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



# AMB - 07 - - - fp, mp
#
# what adj are present with the same form in fp and mp and not in fs or ms 

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



# AMB - 8 - - - fp, ms
#
# what adj are present with the same form in fp and ms and not in fs or mp 

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


# AMB - 9 - - - fs, mp
#
# what adj are present with the same form in fs and mp and not in fp or ms 

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


# AMB - 10 - - - fs, ms
#

# what adj are present with the same form in fs and ms and not in fp or mp 

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


# AMB - 11 - - - mp, ms
#
# what adj are present with the same form in mp and ms and not in fp or mp 


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
#   a data frame with all and only "ambiguous types"
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

# discarded forms
ambtabformchar_reduced=ambtabformchar
ambtabformchar_reduced$num_of_types=apply(X=ambtabformchar_reduced, MARGIN=1, FUN=max)

# number of discarded forms
amb_forms_num=sum(ambtabformchar_reduced$num_of_types)
# prop of discarded forms
amb_forms_num/length(unique(nouns_all$Form))


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
print(xtable::xtable(ambtabPOSchar))

#
#   graphs: the distributions of nouns x lastchar x feature
#



###
#                                   
#           Quantify discarded forms
#

all_nouns_11$Amb=rep("no_amb")
all_nouns_11$Form_amb=rep("no_amb")

# number of unambiguous forms, types, lemmas
head(all_nouns_11)
length((all_nouns_11$Form))
length(unique(all_nouns_11$Form))
length(unique(all_nouns_11$lemma))

# number of ambiguous forms, types, lemmas
head(amb_nouns_all)
length((amb_nouns_all$Form))
length(unique(amb_nouns_all$Form))
length(unique(amb_nouns_all$lemma))



# forms tagged as Feminine Plural
unamb_fp= all_nouns_11[all_nouns_11$POS=="fp", ]
summary(unamb_fp)
unamb_fp$POS=as.factor(as.character(unamb_fp$POS))

## forms tagged as Feminine Singular
unamb_fs= all_nouns_11[all_nouns_11$POS=="fs", ]
summary(unamb_fs)
unamb_fs$POS=as.factor(as.character(unamb_fs$POS))

# forms tagged as Masculine Plural
unamb_mp= all_nouns_11[all_nouns_11$POS=="mp", ]
summary(unamb_mp)
unamb_mp$POS=as.factor(as.character(unamb_mp$POS))

# forms tagged as Masculine Singular
unamb_ms= all_nouns_11[all_nouns_11$POS=="ms", ]
summary(unamb_ms)
unamb_ms$POS=as.factor(as.character(unamb_ms$POS))

all_nouns_11$POS=as.factor(all_nouns_11$POS)
postab_unamb=table(all_nouns_11$POS)

#
## Graph: number of types in each feature: all nouns #--------------- mygraph_num_alladj_types_unamb  
# 

mygraph_num_alladj_types_unamb %<a-% { 
  barplot(postab_unamb, 
          main = "Number of unambiguous adjective types \n per inflectional feature",
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

mygraph_num_alladj_types_unamb

# comparison between graphs 
par(mfrow=c(1,2))
mygraph_num_alladj_types
mygraph_num_alladj_types_unamb

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



#
#   Unambiguous - Feminine Plural
# 
head(unamb_fp)
unique(unamb_fp$lastchar)

## Graph: number of suffixes in each feature: unambiguous nouns fp #--------------- mygraph_num_adjtypes_unamb_fp 
# 
mygraph_num_adjtypes_unamb_fp%<a-% { 
  barplot(table(unamb_fp$lastchar),
          beside=T,
          col=col_fp, 
          angle=ang_fem_typ,
          density=dens_plu_typ, 
          border = bar_bor1,
          main = "Unambiguous inflection: adjective types \n Feminine Plural"
  )
}
mygraph_num_adjtypes_unamb_fp


fp_a=unamb_fp[unamb_fp$lastchar=="a", ]
fp_a_typ=length(fp_a$Form)
fp_a_tok=sum(fp_a$Freq)

fp_e=unamb_fp[unamb_fp$lastchar=="e", ]
fp_e_typ=length(fp_e$Form)
fp_e_tok=sum(fp_e$Freq)

fp_i=unamb_fp[unamb_fp$lastchar=="i", ]
fp_i_typ=length(fp_i$Form)
fp_i_tok=sum(fp_i$Freq)

fp_o=unamb_fp[unamb_fp$lastchar=="o", ]
fp_o_typ=length(fp_o$Form)
fp_o_tok=sum(fp_o$Freq)


#
#   Unambiguous - Feminine Singular
# 
head(unamb_fs)
unique(unamb_fs$lastchar)

## Graph: number of suffixes in each feature: unambiguous nouns fs #--------------- mygraph_num_adjtypes_unamb_fs 
# 

mygraph_num_adjtypes_unamb_fs%<a-% { 
  barplot(table(unamb_fs$lastchar),
          beside=F,
          col=col_fs, 
          angle=ang_fem_typ,
          density=dens_sin_typ, 
          border = bar_bor1,
          main = "Unambiguous inflection: adjective types \n Feminine Singular"
  )
}
mygraph_num_adjtypes_unamb_fs

fs_a=unamb_fs[unamb_fs$lastchar=="a", ]
fs_a_typ=length(fs_a$Form)
fs_a_tok=sum(fs_a$Freq)

fs_e=unamb_fs[unamb_fs$lastchar=="e", ]
fs_e_typ=length(fs_e$Form)
fs_e_tok=sum(fs_e$Freq)

fs_i=unamb_fs[unamb_fs$lastchar=="i", ]
fs_i_typ=length(fs_i$Form)
fs_i_tok=sum(fs_i$Freq)

fs_o=unamb_fs[unamb_fs$lastchar=="o", ]
fs_o_typ=length(fs_o$Form)
fs_o_tok=sum(fs_o$Freq)



#
#   Unambiguous - Masculine Plural
# 
head(unamb_mp)
unique(unamb_mp$lastchar)

## Graph: number of suffixes in each feature: unambiguous nouns mp #--------------- mygraph_num_adjtypes_unamb_mp 
# 
mygraph_num_adjtypes_unamb_mp%<a-% { 
  barplot(table(unamb_mp$lastchar),
          beside=T,
          col=col_mp, 
          angle=ang_mas_typ,
          density=dens_plu_typ, 
          border = bar_bor1,
          main = "Unambiguous inflection: adjective types \n Masculine Plural"
  )
}
mygraph_num_adjtypes_unamb_mp


mp_a=unamb_mp[unamb_mp$lastchar=="a", ]
mp_a_typ=length(mp_a$Form)
mp_a_tok=sum(mp_a$Freq)

mp_e=unamb_mp[unamb_mp$lastchar=="e", ]
mp_e_typ=length(mp_e$Form)
mp_e_tok=sum(mp_e$Freq)

mp_i=unamb_mp[unamb_mp$lastchar=="i", ]
mp_i_typ=length(mp_i$Form)
mp_i_tok=sum(mp_i$Freq)

mp_o=unamb_mp[unamb_mp$lastchar=="o", ]
mp_o_typ=length(mp_o$Form)
mp_o_tok=sum(mp_o$Freq)



#
#   Unambiguous - Masculine Singular
# 
head(unamb_ms)
unique(unamb_ms$lastchar)

## Graph: number of suffixes in each feature: unambiguous nouns ms #--------------- mygraph_num_adjtypes_unamb_ms 
# 
mygraph_num_adjtypes_unamb_ms%<a-% { 
  barplot(table(unamb_ms$lastchar),
          beside=T,
          col=col_ms, 
          angle=ang_mas_typ,
          density=dens_sin_typ, 
          border = bar_bor1,
          main = "Unambiguous inflection: adjective types \n Masculine Singular"
  )
}
mygraph_num_adjtypes_unamb_ms

ms_a=unamb_ms[unamb_ms$lastchar=="a", ]
ms_a_typ=length(ms_a$Form)
ms_a_tok=sum(ms_a$Freq)

ms_e=unamb_ms[unamb_ms$lastchar=="e", ]
ms_e_typ=length(ms_e$Form)
ms_e_tok=sum(ms_e$Freq)

ms_i=unamb_ms[unamb_ms$lastchar=="i", ]
ms_i_typ=length(ms_i$Form)
ms_i_tok=sum(ms_i$Freq)

ms_o=unamb_ms[unamb_ms$lastchar=="o", ]
ms_o_typ=length(ms_o$Form)
ms_o_tok=sum(ms_o$Freq)


#
# Type and token frequency per suffix
#

cols_toktyp=c( "types", "token")
rows_char= c("a", "e", "i", "o")


# Feminine plural
#

typnums_fp=rbind(fp_a_typ, fp_e_typ, fp_i_typ, fp_o_typ)
toknums_fp=rbind(fp_a_tok, fp_e_tok, fp_i_tok, fp_o_tok)
toktyp_fp=(cbind(typnums_fp, toknums_fp))
colnames(toktyp_fp)=cols_toktyp
rownames(toktyp_fp)=rows_char
toktyp_fp=toktyp_fp[order(toktyp_fp[,1],toktyp_fp[,2],decreasing=T),]

barplot(t(toktyp_fp), beside = T)



# Feminine singular
#
typnums_fs=rbind(fs_a_typ, fs_e_typ, fs_i_typ, fs_o_typ)
toknums_fs=rbind(fs_a_tok, fs_e_tok, fs_i_tok, fs_o_tok)
toktyp_fs=(cbind(typnums_fs, toknums_fs))
colnames(toktyp_fs)=cols_toktyp
rownames(toktyp_fs)=rows_char
toktyp_fs=toktyp_fs[order(toktyp_fs[,1],toktyp_fs[,2],decreasing=T),]

barplot(t(toktyp_fs), beside = T)



# Masculine plural
#
typnums_mp=rbind(mp_a_typ, mp_e_typ, mp_i_typ, mp_o_typ)
toknums_mp=rbind(mp_a_tok, mp_e_tok, mp_i_tok, mp_o_tok)
toktyp_mp=(cbind(typnums_mp, toknums_mp))
colnames(toktyp_mp)=cols_toktyp
rownames(toktyp_mp)=rows_char
toktyp_mp=toktyp_mp[order(toktyp_mp[,1],toktyp_mp[,2],decreasing=T),]

barplot(t(toktyp_mp), beside = T)


# Masculine singular
#

typnums_ms=rbind(ms_a_typ, ms_e_typ, ms_i_typ, ms_o_typ)
toknums_ms=rbind(ms_a_tok, ms_e_tok, ms_i_tok, ms_o_tok)
toktyp_ms=(cbind(typnums_ms, toknums_ms))
colnames(toktyp_ms)=c(cols_toktyp)
rownames(toktyp_ms)=rows_char
toktyp_ms=toktyp_ms[order(toktyp_ms[,1],toktyp_ms[,2],decreasing=T),]

barplot(t(toktyp_ms), beside = T)
toktyp_ms=as.data.frame(toktyp_ms)


#
#  Type and token frequency per suffix
#  All features
#
# Types
#
feat_typs=cbind(typnums_fp, typnums_fs, typnums_mp, typnums_ms)
colnames(feat_typs)=colcontent  
rownames(feat_typs)=rows_char  

log_feat_typs=round(log(feat_typs+1), roundnum)

## Graph: number of suffixes in each feature: unambiguous nouns - types #--------------- mygraph_adjnum_types_unamb_per_suffix 
# 
mygraph_adjnum_types_unamb_per_suffix%<a-% { 
  barplot(t(log_feat_typs), 
          main="Distribution of adjective types", 
          xlab="Ending",
          ylab="Type Frequency (log)",
          beside = F,
          ylim=c(0,40),
          col=pal_01, 
          width=.2, 
          border = bar_bor1, 
          density =plot_dens_typ, 
          angle = plot_ang_typ )
  legend("topleft", legend=legendcontent, 
         fill=pal_01, 
         density = plot_dens_typ, 
         angle = plot_ang_typ,
         bty="n", ncol = 2)
}
mygraph_adjnum_types_unamb_per_suffix


#
#  Type and token frequency per suffix
#  All features
#
# Tokens
#

feat_toks=cbind(toknums_fp, toknums_fs, toknums_mp, toknums_ms)
colnames(feat_toks)=colcontent  
rownames(feat_toks)=rows_char  

log_feat_toks=round(log(feat_toks+1), roundnum)


## Graph: number of suffixes in each feature: unambiguous nouns - tokens #--------------- mygraph_adjnum_tokens_unamb_per_suffix 
# 
mygraph_adjnum_tokens_unamb_per_suffix%<a-% { 
  barplot(t(log_feat_toks), 
          main="Distribution of adjective tokens", 
          xlab="Ending",
          ylab="Token Frequency (log)",
          beside = F,
          ylim=c(0,80),
          col=pal_01, 
          width=.2, 
          border = bar_bor1 
          #density =plot_dens_tok, 
          #angle = plot_ang_tok 
  )
  legend("topleft", legend=legendcontent, 
         fill=pal_01, 
         #  density = plot_dens_tok, 
         #   angle = plot_ang_tok,
         bty="n", ncol = 2)
  # grid()
}
mygraph_adjnum_tokens_unamb_per_suffix




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

#barplot(as.matrix(t(char_to_feat)), col=pal_01, border = bar_bor2 )
#legend("topright", legendcontent, fill=pal_01, bty = "n")


#
#   Types
#

#add 1 to frequency in feat_toks
feat_typs0=feat_typs
#feat_typs=feat_typs+1

a_typ=feat_typs[1, ]
names(a_typ)=NULL
a_sums_typ=sum(a_typ)

e_typ=feat_typs[2, ]
names(e_typ)=NULL
e_sums_typ=sum(e_typ)

i_typ=feat_typs[3, ]
names(i_typ)=NULL
i_sums_typ=sum(i_typ)

o_typ=feat_typs[4, ]
names(o_typ)=NULL
o_sums_typ=sum(o_typ)

# suffix: A
probs_a_typs_s=a_typ/a_sums_typ
probs_a_typs=probs_a_typs_s[probs_a_typs_s>0]
H_a_typs= -sum(probs_a_typs * log2(probs_a_typs))
H_a_typs=round(H_a_typs, roundnum)
probs_a_typs_round=round(probs_a_typs_s, roundnum)

# suffix: E
probs_e_typs_s=e_typ/e_sums_typ
probs_e_typs=probs_e_typs_s[probs_e_typs_s>0]
H_e_typs= -sum(probs_e_typs * log2(probs_e_typs))
H_e_typs=round(H_e_typs, roundnum)
probs_e_typs_round=round(probs_e_typs_s, roundnum)


# suffix: I
probs_i_typs_s=i_typ/i_sums_typ
probs_i_typs=probs_i_typs_s[probs_i_typs_s>0]
H_i_typs= -sum(probs_i_typs * log2(probs_i_typs))
H_i_typs=round(H_i_typs, roundnum)
probs_i_typs_round=round(probs_i_typs_s, roundnum)


# suffix: O
probs_o_typs_s=o_typ/o_sums_typ
probs_o_typs=probs_o_typs_s[probs_o_typs_s>0]
H_o_typs= -sum(probs_o_typs * log2(probs_o_typs))
H_o_typs=round(H_o_typs, roundnum)
probs_o_typs_round=round(probs_o_typs_s, roundnum)


## type count - 4 most represented inflectional endings
feat_typs_sum=as.data.frame(feat_typs)
feat_typs_sum$sums=rowSums(feat_typs_sum[c(1:4)])

## type probs - 4 most represented inflectional endings
probstyp_tab= rbind.data.frame(probs_a_typs_round, 
                               probs_e_typs_round, 
                               probs_i_typs_round, 
                               probs_o_typs_round)

## type probs - 4 most represented inflectional endings
probstyp_tab=as.data.frame(probstyp_tab)
colnames(probstyp_tab)=colcontent
rownames(probstyp_tab)=rows_char

## H (token)
endings_H_type= c(H_a_typs, H_e_typs, H_i_typs, H_o_typs)



#
#   Tokens
#

#add 1 to frequency in feat_toks
feat_toks0=feat_toks
#feat_toks=feat_toks+1

a_tok=feat_toks[1, ]
names(a_tok)=NULL
a_sums_tok=sum(a_tok)

e_tok=feat_toks[2, ]
names(e_tok)=NULL
e_sums_tok=sum(e_tok)

i_tok=feat_toks[3, ]
names(i_tok)=NULL
i_sums_tok=sum(i_tok)

o_tok=feat_toks[4, ]
names(o_tok)=NULL
o_sums_tok=sum(o_tok)

# suffix: A
probs_a_toks_s=a_tok/a_sums_tok
probs_a_toks=probs_a_toks_s[probs_a_toks_s>0]
H_a_toks= -sum(probs_a_toks * log2(probs_a_toks))
H_a_toks=round(H_a_toks, roundnum)
probs_a_toks_round=round(probs_a_toks_s, roundnum)

# suffix: E
probs_e_toks_s=e_tok/e_sums_tok
probs_e_toks=probs_e_toks_s[probs_e_toks_s>0]
H_e_toks= -sum(probs_e_toks * log2(probs_e_toks))
H_e_toks=round(H_e_toks, roundnum)
probs_e_toks_round=round(probs_e_toks_s, roundnum)


# suffix: I
probs_i_toks_s=i_tok/i_sums_tok
probs_i_toks=probs_i_toks_s[probs_i_toks_s>0]
H_i_toks= -sum(probs_i_toks * log2(probs_i_toks))
H_i_toks=round(H_i_toks, roundnum)
probs_i_toks_round=round(probs_i_toks_s, roundnum)


# suffix: O
probs_o_toks_s=o_tok/o_sums_tok
probs_o_toks=probs_o_toks_s[probs_o_toks_s>0]
H_o_toks= -sum(probs_o_toks * log2(probs_o_toks))
H_o_toks=round(H_o_toks, roundnum)
probs_o_toks_round=round(probs_o_toks_s, roundnum)


## token count - 4 most represented inflectional endings
feat_toks_sum=as.data.frame(feat_toks)
feat_toks_sum$sums=rowSums(feat_toks_sum[c(1:4)])


probstok_tab= rbind(probs_a_toks_round, 
                    probs_e_toks_round, 
                    probs_i_toks_round, 
                    probs_o_toks_round)

## token probs - 4 most represented inflectional endings
probstok_tab=as.data.frame(probstok_tab)
colnames(probstok_tab)=colcontent
rownames(probstok_tab)=rows_char

## H (token)
endings_H_token= c(H_a_toks, H_e_toks, H_i_toks, H_o_toks)


#
#   S U M   U P  
#       
#      T A B L E S
#                     type  tokens - unambiguous nouns 




## type count - 4 most represented inflectional endings
feat_typs_sum

## type probs - 4 most represented inflectional endings
probstyp_tab$typ_entropy=endings_H_type
probstyp_tab

## token count - 4 most represented inflectional endings
feat_toks_sum

## token probs - 4 most represented inflectional endings
probstok_tab$tok_entropy=endings_H_token
probstok_tab


###
#                                   
#
#            I N F L E C T I O N A L   C L A S S E S  
#
#   
#
###


#reassign baseform to invariants 
#amb_nouns_all$baseform=amb_nouns_all$Form
unique(amb_nouns_all$Amb)
amb_nouns_all[amb_nouns_all$Amb=="amb01", ]

# Amb 03 - 10 are not monomorphemic
amb_nouns_all[amb_nouns_all$Amb=="amb03", ]
amb_nouns_all[amb_nouns_all$Amb=="amb06", ]
amb_nouns_all[amb_nouns_all$Amb=="amb07", ]
amb_nouns_all[amb_nouns_all$Amb=="amb10", ]
amb_nouns_all$baseform= ifelse(amb_nouns_all$Amb=="amb01", amb_nouns_all$lemma_morphit, amb_nouns_all$baseform)

# discard amb fp-fs-mp-ms
amb_nouns_all_amb01=amb_nouns_all[amb_nouns_all$Amb=="amb01", ]
amb_nouns_all=amb_nouns_all[amb_nouns_all$Amb!="amb01", ]
head(amb_nouns_all)


# remerge all nouns in a single object
# unambiguous forms, types, lemmas
head(all_nouns_11)
length((all_nouns_11$Form))
length(unique(all_nouns_11$Form))
length(unique(all_nouns_11$lemma))
all_nouns_11$lemma=NULL

# number of ambiguous forms, types, lemmas
head(amb_nouns_all)
length((amb_nouns_all$Form))
length(unique(amb_nouns_all$Form))
length(unique(amb_nouns_all$lemma))
amb_nouns_all$lemma=NULL



# inflectional classes comprising Number + Gender
all_nouns_11_fp=all_nouns_11[all_nouns_11$POS=="fp", ]
all_nouns_11_fs=all_nouns_11[all_nouns_11$POS=="fs", ]
all_nouns_11_mp=all_nouns_11[all_nouns_11$POS=="mp", ]
all_nouns_11_ms=all_nouns_11[all_nouns_11$POS=="ms", ]

# remerge: feminines
all_nouns_lemmas_femflex_1=merge(all_nouns_11_fs, all_nouns_11_fp[ , c("lemma_morphit", "lastchar")], 
                               by.x = "lemma_morphit", by.y = c("lemma_morphit"), all.x = T)
all_nouns_lemmas_femflex_1$s_ending=all_nouns_lemmas_femflex_1$lastchar.x
all_nouns_lemmas_femflex_1$p_ending=all_nouns_lemmas_femflex_1$lastchar.y

summary(all_nouns_lemmas_femflex_1$s_ending==all_nouns_lemmas_femflex_1$p_ending)

all_nouns_lemmas_femflex_2=merge(all_nouns_11_fp, all_nouns_11_fs[ , c("lemma_morphit", "lastchar")], 
                               by.x = "lemma_morphit", by.y = "lemma_morphit", all.x = T)
all_nouns_lemmas_femflex_2$s_ending=all_nouns_lemmas_femflex_2$lastchar.y
all_nouns_lemmas_femflex_2$p_ending=all_nouns_lemmas_femflex_2$lastchar.x

summary(all_nouns_lemmas_femflex_2$s_ending==all_nouns_lemmas_femflex_2$p_ending)


all_nouns_lemmas_femflex=rbind(all_nouns_lemmas_femflex_1, all_nouns_lemmas_femflex_2)
all_nouns_lemmas_femflex$gen_class=paste(all_nouns_lemmas_femflex$s_ending, all_nouns_lemmas_femflex$p_ending, sep="_")

all_nouns_lemmas_femflex_na=all_nouns_lemmas_femflex[is.na(all_nouns_lemmas_femflex$lastchar.y==T)
                                                        |is.na(all_nouns_lemmas_femflex$lastchar.x==T)
                                                        , ]

all_nouns_lemmas_femflex=na.omit(all_nouns_lemmas_femflex)


#remerge: masculines
all_nouns_lemmas_masflex_1=merge(all_nouns_11_ms, all_nouns_11_mp[ , c("lemma_morphit", "lastchar")], 
                               by.x = "lemma_morphit", by.y = "lemma_morphit", all.x = T)
all_nouns_lemmas_masflex_1$s_ending=all_nouns_lemmas_masflex_1$lastchar.x
all_nouns_lemmas_masflex_1$p_ending=all_nouns_lemmas_masflex_1$lastchar.y

summary(all_nouns_lemmas_masflex_1$s_ending==all_nouns_lemmas_masflex_1$p_ending)

all_nouns_lemmas_masflex_2=merge(all_nouns_11_mp, all_nouns_11_ms[ , c("lemma_morphit", "lastchar")], 
                               by.x = "lemma_morphit", by.y = "lemma_morphit", all.x = T)
all_nouns_lemmas_masflex_2$s_ending=all_nouns_lemmas_masflex_2$lastchar.y
all_nouns_lemmas_masflex_2$p_ending=all_nouns_lemmas_masflex_2$lastchar.x

summary(all_nouns_lemmas_masflex_2$s_ending==all_nouns_lemmas_masflex_2$p_ending)

all_nouns_lemmas_masflex=rbind(all_nouns_lemmas_masflex_1, all_nouns_lemmas_masflex_2)
all_nouns_lemmas_masflex$gen_class=paste(all_nouns_lemmas_masflex$s_ending, all_nouns_lemmas_masflex$p_ending, sep="_")
unique(all_nouns_lemmas_masflex$gen_class)

all_nouns_lemmas_masflex_na=all_nouns_lemmas_masflex[is.na(all_nouns_lemmas_masflex$lastchar.y==T)
                                                     |is.na(all_nouns_lemmas_masflex$lastchar.x==T)
                                                     , ]
all_nouns_lemmas_masflex=na.omit(all_nouns_lemmas_masflex)

#remerge: number
all_nouns_lemmas_allflex1= merge(all_nouns_lemmas_femflex, 
                                 all_nouns_lemmas_masflex[ , c("lemma_morphit",# "fs_ending", "fp_ending", 
                                                               "gen_class")],
                                 by.x = "lemma_morphit", 
                                 by.y = "lemma_morphit",
                                 all.x=T)

all_nouns_lemmas_allflex1$class=paste(all_nouns_lemmas_allflex1$gen_class.x, all_nouns_lemmas_allflex1$gen_class.y, sep = " ")
unique(all_nouns_lemmas_allflex1$gen_class.x)

all_nouns_lemmas_allflex2= merge(all_nouns_lemmas_masflex, 
                                 all_nouns_lemmas_femflex[ , c("lemma_morphit", # "fs_ending", "fp_ending", 
                                                               "gen_class")], 
                                 by.x = "lemma_morphit", 
                                 by.y = "lemma_morphit",
                                 all.x=T)


all_nouns_lemmas_allflex2$class=paste(all_nouns_lemmas_allflex2$gen_class.y, all_nouns_lemmas_allflex2$gen_class.x, sep = " ")

unique(all_nouns_lemmas_allflex1$class)
unique(all_nouns_lemmas_allflex2$class)

all_nouns_lemmas_allflex=rbind(all_nouns_lemmas_allflex1, all_nouns_lemmas_allflex2)

unique(all_nouns_lemmas_allflex$class)


all_nouns_lemmas_raw=all_nouns_lemmas_allflex
all_nouns_lemmas_raw$notunique_rows=duplicated(all_nouns_lemmas_raw)
all_nouns_lemmas=all_nouns_lemmas_raw[all_nouns_lemmas_raw$notunique_rows==F, ]


## remerge NAs/uninflected

all_nouns_lemmas_masflex_na$class=paste("NA", all_nouns_lemmas_masflex_na$gen_class, sep = " ")
all_nouns_lemmas_femflex_na$class=paste(all_nouns_lemmas_femflex_na$gen_class, "NA", sep = " ")

head(all_nouns_lemmas)

all_nouns_lemmas_masflex_na=all_nouns_lemmas_masflex_na[, c("Form", "lemma_morphit", "Freq",  "fpmw",  
                                                            "Zipf", "POS", "grade", "baseform", "Gender", "Number",    
                                                            "Amb", "Form_amb", "s_ending", "p_ending", "gen_class",   "class")]

all_nouns_lemmas_femflex_na=all_nouns_lemmas_femflex_na[, c("Form", "lemma_morphit", "Freq",  "fpmw",  
     "Zipf", "POS", "grade", "baseform", "Gender", "Number",    
     "Amb", "Form_amb", "s_ending", "p_ending", "gen_class",   "class")]

all_nouns_lemmas$gen_class=all_nouns_lemmas$gen_class.x
all_nouns_lemmas=all_nouns_lemmas[ , c("Form", "lemma_morphit", "Freq",  "fpmw",  
                                       "Zipf", "POS", "grade", "baseform", "Gender", "Number",    
                                       "Amb", "Form_amb", "s_ending", "p_ending", "gen_class",   "class")]

all_nouns_lemmas_rawinf=rbind(all_nouns_lemmas, all_nouns_lemmas_femflex_na, all_nouns_lemmas_masflex_na)
#remove duplicates 

all_nouns_lemmas_rawinf$notunique_rows=duplicated(all_nouns_lemmas_rawinf)
all_nouns_lemmas_inf=all_nouns_lemmas_rawinf[all_nouns_lemmas_rawinf$notunique_rows==F, ]
all_nouns_lemmas_inf$notunique_rows=NULL


#
# types
#

infclasstypes= as.data.frame.matrix(table(all_nouns_lemmas_inf$class, all_nouns_lemmas_inf$POS))
infclasstypes$tot_types= infclasstypes$fp+infclasstypes$fs+infclasstypes$mp+infclasstypes$ms
infclasstypes=infclasstypes[order(- infclasstypes$tot_types), ]
infclasstypes$class=rownames(infclasstypes)
infclasstypes

# entropy in inflectional classes - types
vec_inf_typs=NULL
for (everyclass in infclasstypes$class){
  class_ord= match(everyclass, infclasstypes$class)
  classfreqs= c(infclasstypes$fp[class_ord], infclasstypes$fs[class_ord], infclasstypes$mp[class_ord], infclasstypes$ms[class_ord])
  classprobs= classfreqs/infclasstypes$tot_types[class_ord]
  classprobs=classprobs[classprobs>0]
  h_inf_typs= -sum((classprobs)*log2(classprobs))
  print(h_inf_typs)
  vec_inf_typs=c(vec_inf_typs, h_inf_typs)
}
infclasstypes$h_typ=round(vec_inf_typs, roundnum)

infclasstypes[is.na(infclasstypes)]=0


#
# tokens 
#

infclasstokens= aggregate(x=all_nouns_lemmas_inf$Freq, by=list(all_nouns_lemmas_inf$class, all_nouns_lemmas_inf$POS), FUN=sum)
infclasstokens_sum= aggregate(x=all_nouns_lemmas_inf$Freq, by=list(all_nouns_lemmas_inf$class), FUN=sum)

infclasstokens_fp= infclasstokens[infclasstokens$Group.2=="fp", ]
infclasstokens_fs= infclasstokens[infclasstokens$Group.2=="fs", ]
infclasstokens_fp$Group.2=NULL
infclasstokens_fs$Group.2=NULL
colnames(infclasstokens_fp)=list("class", "fp_token")
colnames(infclasstokens_fs)=list("class", "fs_token")

infclasstokens_mp= infclasstokens[infclasstokens$Group.2=="mp", ]
infclasstokens_ms= infclasstokens[infclasstokens$Group.2=="ms", ]
infclasstokens_mp$Group.2=NULL
infclasstokens_ms$Group.2=NULL
colnames(infclasstokens_mp)=list("class", "mp_token")
colnames(infclasstokens_ms)=list("class", "ms_token")

infclasstokens_fem=merge(infclasstokens_fp, infclasstokens_fs, by.x = "class", by.y = "class", all.x = T, all.y = T)
infclasstokens_mas=merge(infclasstokens_mp, infclasstokens_ms, by.x = "class", by.y = "class", all.x = T, all.y = T)

infclasstokens_tot=merge(infclasstokens_fem, infclasstokens_mas, by.x = "class", by.y = "class", all.x = T, all.y = T)
infclasstokens_tot[is.na(infclasstokens_tot)] = 0
infclasstokens_tot$tot_tokens=rowSums(infclasstokens_tot[c(2:5)])  
infclasstokens_tot=infclasstokens_tot[order(- infclasstokens_tot$tot_tokens), ]
rownames(infclasstokens_tot)=NULL
infclasstokens_tot


# entropy in inflectional classes - types
vec_inf_toks=NULL
for (everytokclass in infclasstokens_tot$class){
  class_ord_tok= match(everytokclass, infclasstokens_tot$class)
  classfreqs_tok= c(infclasstokens_tot$fp[class_ord_tok], infclasstokens_tot$fs[class_ord_tok],  
                    infclasstokens_tot$mp[class_ord_tok], infclasstokens_tot$ms[class_ord_tok])
  classprobs_tok= classfreqs_tok/infclasstokens_tot$tot_tokens[class_ord_tok]
  classprobs_tok=classprobs_tok[classprobs_tok>0]
  h_inf_toks= -sum((classprobs_tok)*log2(classprobs_tok))
  print(h_inf_toks)
  vec_inf_toks=c(vec_inf_toks, h_inf_toks)
}
infclasstokens_tot$h_tok=round(vec_inf_toks, roundnum)
infclasstokens_tot[is.na(infclasstokens_tot)]=0


# merge types and tokens in a single table 


infclass_toktyp=merge(infclasstypes, infclasstokens_tot, by.x = "class", by.y = "class")
infclass_toktyp=infclass_toktyp[order(- infclass_toktyp$tot_tokens), ]
infclass_toktyp

#
# remerge "amb" nouns (invariant fp-fs-mp-ms excluded)
#

str(all_nouns_lemmas_inf)
all_nouns_lemmas_inf_NA=all_nouns_lemmas_inf[is.na(all_nouns_lemmas_inf$s_ending)|is.na(all_nouns_lemmas_inf$s_ending),  ]

str(all_nouns_lemmas_inf_NA)


all_nouns_lemmas_inf_NA$lastchar=stringr::str_sub(all_nouns_lemmas_inf_NA$Form, -1, -1)
all_nouns_lemmas_inf_NA=all_nouns_lemmas_inf_NA[, colnames(amb_nouns_all)]
                                                
amb_nouns_all_remerge_allgrade=rbind(amb_nouns_all, all_nouns_lemmas_inf_NA)

#remove superlative and comparative grades, keep only pos
amb_nouns_all_remerge=amb_nouns_all_remerge_allgrade[amb_nouns_all_remerge_allgrade$grade=="pos", ]

# split into features 
amb_nouns_11_fp=amb_nouns_all_remerge[amb_nouns_all_remerge$POS=="fp", ]
amb_nouns_11_fs=amb_nouns_all_remerge[amb_nouns_all_remerge$POS=="fs", ]
amb_nouns_11_mp=amb_nouns_all_remerge[amb_nouns_all_remerge$POS=="mp", ]
amb_nouns_11_ms=amb_nouns_all_remerge[amb_nouns_all_remerge$POS=="ms", ]


# remerge: feminine amb ## controlla da qui
amb_nouns_lemmas_femflex_1=merge(amb_nouns_11_fs, amb_nouns_11_fp[ , c("lemma_morphit", "lastchar")], 
                                 by.x = "lemma_morphit", by.y = c("lemma_morphit"), all.x = T)
amb_nouns_lemmas_femflex_1$s_ending=amb_nouns_lemmas_femflex_1$lastchar.x
amb_nouns_lemmas_femflex_1$p_ending=amb_nouns_lemmas_femflex_1$lastchar.y

summary(amb_nouns_lemmas_femflex_1$s_ending==amb_nouns_lemmas_femflex_1$p_ending)
amb_nouns_lemmas_femflex_1[amb_nouns_lemmas_femflex_1$s_ending==amb_nouns_lemmas_femflex_1$p_ending, ]

amb_nouns_lemmas_femflex_2=merge(amb_nouns_11_fp, amb_nouns_11_fs[ , c("lemma_morphit", "lastchar")], 
                                 by.x = "lemma_morphit", by.y = "lemma_morphit", all.x = T)
amb_nouns_lemmas_femflex_2$s_ending=amb_nouns_lemmas_femflex_2$lastchar.y
amb_nouns_lemmas_femflex_2$p_ending=amb_nouns_lemmas_femflex_2$lastchar.x

summary(amb_nouns_lemmas_femflex_2$s_ending==amb_nouns_lemmas_femflex_2$p_ending)
amb_nouns_lemmas_femflex_2[amb_nouns_lemmas_femflex_2$s_ending==amb_nouns_lemmas_femflex_2$p_ending, ]

amb_nouns_lemmas_femflex=rbind(amb_nouns_lemmas_femflex_1, amb_nouns_lemmas_femflex_2)
amb_nouns_lemmas_femflex$gen_class=paste(amb_nouns_lemmas_femflex$s_ending, amb_nouns_lemmas_femflex$p_ending, sep="_")

amb_nouns_lemmas_femflex_na=amb_nouns_lemmas_femflex[is.na(amb_nouns_lemmas_femflex$lastchar.y==T)
                                                     |is.na(amb_nouns_lemmas_femflex$lastchar.x==T)
                                                     , ]

amb_nouns_lemmas_femflex=na.omit(amb_nouns_lemmas_femflex)
unique(amb_nouns_lemmas_femflex$gen_class)


#remerge: masculines amb
amb_nouns_lemmas_masflex_1=merge(amb_nouns_11_ms, amb_nouns_11_mp[ , c("lemma_morphit", "lastchar")], 
                                 by.x = "lemma_morphit", by.y = "lemma_morphit", all.x = T)
amb_nouns_lemmas_masflex_1$s_ending=amb_nouns_lemmas_masflex_1$lastchar.x
amb_nouns_lemmas_masflex_1$p_ending=amb_nouns_lemmas_masflex_1$lastchar.y

amb_nouns_lemmas_masflex_2=merge(amb_nouns_11_mp, amb_nouns_11_ms[ , c("lemma_morphit", "lastchar")], 
                                 by.x = "lemma_morphit", by.y = "lemma_morphit", all.x = T)
amb_nouns_lemmas_masflex_2$s_ending=amb_nouns_lemmas_masflex_2$lastchar.y
amb_nouns_lemmas_masflex_2$p_ending=amb_nouns_lemmas_masflex_2$lastchar.x

amb_nouns_lemmas_masflex=rbind(amb_nouns_lemmas_masflex_1, amb_nouns_lemmas_masflex_2)
amb_nouns_lemmas_masflex$gen_class=paste(amb_nouns_lemmas_masflex$s_ending, amb_nouns_lemmas_masflex$p_ending, sep="_")

amb_nouns_lemmas_masflex_na=amb_nouns_lemmas_masflex[is.na(amb_nouns_lemmas_masflex$lastchar.y==T)
                                                     |is.na(amb_nouns_lemmas_masflex$lastchar.x==T)
                                                     , ]
amb_nouns_lemmas_masflex=na.omit(amb_nouns_lemmas_masflex)

unique(amb_nouns_lemmas_masflex$gen_class)

##

#remerge: number
amb_nouns_lemmas_allflex1= merge(amb_nouns_lemmas_femflex, 
                                 amb_nouns_lemmas_masflex[ , c("lemma_morphit",# "fs_ending", "fp_ending", 
                                                               "gen_class")],
                                 by.x = "lemma_morphit", 
                                 by.y = "lemma_morphit",
                                 all.x=T)

amb_nouns_lemmas_allflex1$class=paste(amb_nouns_lemmas_allflex1$gen_class.x, amb_nouns_lemmas_allflex1$gen_class.y, sep = " ")
unique(amb_nouns_lemmas_allflex1$gen_class.x)

amb_nouns_lemmas_allflex2= merge(amb_nouns_lemmas_masflex, 
                                 amb_nouns_lemmas_femflex[ , c("lemma_morphit", # "fs_ending", "fp_ending", 
                                                               "gen_class")], 
                                 by.x = "lemma_morphit", 
                                 by.y = "lemma_morphit",
                                 all.x=T)
amb_nouns_lemmas_allflex2$class=paste(amb_nouns_lemmas_allflex2$gen_class.y, amb_nouns_lemmas_allflex2$gen_class.x, sep = " ")

unique(amb_nouns_lemmas_allflex2$gen_class.x)

unique(amb_nouns_lemmas_allflex2$class)

amb_nouns_lemmas_allflex=rbind(amb_nouns_lemmas_allflex1, amb_nouns_lemmas_allflex2)

unique(amb_nouns_lemmas_allflex$class)


amb_nouns_lemmas_raw=amb_nouns_lemmas_allflex
amb_nouns_lemmas_raw$notunique_rows=duplicated(amb_nouns_lemmas_raw)
amb_nouns_lemmas=amb_nouns_lemmas_raw[amb_nouns_lemmas_raw$notunique_rows==F, ]


## remerge NAs/uninflected

amb_nouns_lemmas_masflex_na$class=paste("NA", amb_nouns_lemmas_masflex_na$gen_class, sep = " ")
amb_nouns_lemmas_femflex_na$class=paste(amb_nouns_lemmas_femflex_na$gen_class, "NA", sep = " ")

head(amb_nouns_lemmas)

amb_nouns_lemmas_masflex_na=amb_nouns_lemmas_masflex_na[, c("Form", "lemma_morphit", "Freq",  "fpmw",  
                                                            "Zipf", "POS", "grade", "baseform", "Gender", "Number",    
                                                            "Amb", "Form_amb", "s_ending", "p_ending", "gen_class",   "class")]

amb_nouns_lemmas_femflex_na=amb_nouns_lemmas_femflex_na[, c("Form", "lemma_morphit", "Freq",  "fpmw",  
                                                            "Zipf", "POS", "grade", "baseform", "Gender", "Number",    
                                                            "Amb", "Form_amb", "s_ending", "p_ending", "gen_class",   "class")]

amb_nouns_lemmas$gen_class=amb_nouns_lemmas$gen_class.x

amb_nouns_lemmas=amb_nouns_lemmas[ , c("Form", "lemma_morphit", "Freq",  "fpmw",  
                                       "Zipf", "POS", "grade", "baseform", "Gender", "Number",    
                                       "Amb", "Form_amb", "s_ending", "p_ending", "gen_class",   "class")]

amb_nouns_lemmas_rawinf=rbind(amb_nouns_lemmas, amb_nouns_lemmas_femflex_na, amb_nouns_lemmas_masflex_na)




# remerge superlatives and comparatives
amb_nouns_all_remerge_allgrade=amb_nouns_all_remerge_allgrade[, c("Form", "lemma_morphit", "Freq",  "fpmw",  
                                   "Zipf", "POS", "grade", "baseform", "Gender", "Number",    
                                   "Amb", "Form_amb") ] #, "s_ending", "p_ending", "gen_class",   "class")]

# superlatives
amb_nouns_all_remerge_supgrade=amb_nouns_all_remerge_allgrade[amb_nouns_all_remerge_allgrade$grade=="sup", ]
summary(amb_nouns_all_remerge_supgrade$Number)
amb_nouns_all_remerge_supgrade$s_ending=ifelse(amb_nouns_all_remerge_supgrade$Gender=="masc", "o", "a")
amb_nouns_all_remerge_supgrade$p_ending=ifelse(amb_nouns_all_remerge_supgrade$Gender=="masc", "i", "e")
amb_nouns_all_remerge_supgrade$gen_class=paste(amb_nouns_all_remerge_supgrade$s_ending, amb_nouns_all_remerge_supgrade$p_ending, sep="_")
amb_nouns_all_remerge_supgrade$class="a_e o_i"

# comparatives
amb_nouns_all_remerge_comgrade= amb_nouns_all_remerge_allgrade[amb_nouns_all_remerge_allgrade$grade=="com", ]
amb_nouns_all_remerge_comgrade$s_ending="e"
amb_nouns_all_remerge_comgrade$p_ending="i"
amb_nouns_all_remerge_comgrade$gen_class="e_i"
amb_nouns_all_remerge_comgrade$class="e_i e_i"


#remerge invariants (fp-fs-mp-ms)
amb_nouns_all_amb01$s_ending=amb_nouns_all_amb01$lastchar
amb_nouns_all_amb01$p_ending=amb_nouns_all_amb01$lastchar
amb_nouns_all_amb01$gen_class="inv"
amb_nouns_all_amb01$class="inv"

amb_nouns_all_amb01$lemma=NULL
amb_nouns_all_amb01$lastchar=NULL

# bind all amb 
amb_nouns_lemmas_inf= rbind(amb_nouns_all_amb01, amb_nouns_all_remerge_comgrade, amb_nouns_all_remerge_supgrade, amb_nouns_lemmas_rawinf)


#
# merge and order final dataframe
#

summary(all_nouns_lemmas_inf)
str(all_nouns_lemmas_inf)
summary(amb_nouns_lemmas_inf)
str(amb_nouns_lemmas_inf)

#
nouns_database=rbind(all_nouns_lemmas_inf, amb_nouns_lemmas_inf)
nouns_database$Lemma=nouns_database$lemma_morphit
nouns_database$Fpmw=nouns_database$fpmw
nouns_database$Baseform=nouns_database$baseform
nouns_database$Ending=stringr::str_sub(nouns_database$Form, -1, -1)
nouns_database$Inf_class= nouns_database$class
nouns_database$Gen_class=nouns_database$gen_class
nouns_database$Grade=nouns_database$grade

mycol_order <- c("Form", "Lemma", 
                 "Freq", "Fpmw", "Zipf", 
                 "POS", "Baseform", "Grade",
                 "Gender", "Number", "Form_amb",
                 "Ending", "Inf_class")
nouns_database <- nouns_database[, mycol_order]

nouns_database=nouns_database[order((nouns_database$Form), na.last = T), ]

# remove duplicates
nouns_database$notunique_rows=duplicated(nouns_database)
summary(nouns_database$notunique_rows)
nouns_database=nouns_database[nouns_database$notunique_rows==F, ]
nouns_database$notunique_rows=NULL


row.names(nouns_database)=NULL


#
#   O U T P U T  
#       
#       T A B L E S  
#



## type count - 4 most represented inflectional endings
print(xtable::xtable(feat_typs_sum, digits=0))

## type probs - 4 most represented inflectional endings
print(xtable::xtable(probstyp_tab, digits=roundnum))

## token count - 4 most represented inflectional endings
print(xtable::xtable(feat_toks_sum, digits=0))

## token probs - 4 most represented inflectional endings
print(xtable::xtable(probstok_tab, digits=roundnum))

# inflectional classes - types 
print(xtable::xtable(infclasstypes, digits=roundnum))

# inflectional classes - tokens
print(xtable::xtable(infclasstokens_tot, digits=roundnum))

# inflectional classes - types + tokens
print(xtable::xtable(infclass_toktyp, digits=roundnum))


#
#   O U T P U T  
#       
#       D F 
#

setwd(outwd)
#write.csv(nouns_database, "Flex_it_adj.csv" )



#
#   P R I N T 
#       
#       G R A P H I C S
#
#   adapted from Graphresize https://github.com/franfranz/Graphs_and_Pics_Toytools/blob/main/Graph_resize_1_0_2.R

complete_graphlist=ls()[grep(("mygraph"), ls())]

# choose the plots you would like to print/save
graphlist=complete_graphlist#[c(1,3)]


# set directory to save graphics to
setwd(graphwd)


#
# INPUT REQUIRED: choose graphical settings:
#
# size of output images: uncomment your preference 

#imagesize= "small" # gset1, for small, low-quality portable images
imagesize= "medium" # gset2, average 
#imagesize= "big" # gset3 is high-res raster image (for poster printing)

g_type="cairo" 
g_units="px" 

# gset 1 
g1_width=600 
g1_height=600 
g1_pointsize=12 
g1_res=100
rescom1=png
resext_1=".png"

# gset 2 
g2_width=1200 
g2_height=1200 
g2_pointsize=12 
g2_res=200
rescom2=jpeg
resext_2=".jpeg"

# gset 3 
g3_width=2400 
g3_height=2400 
g3_pointsize=10 
g3_res=800
rescom3=tiff
resext_3=".tiff"

if (imagesize=="small"){
  g_width=  g1_width
  g_height= g1_height 
  g_pointsize= g1_pointsize 
  g_res= g1_res
  rescom=rescom1
  resext=resext_1
} else if (imagesize=="medium") {
  g_width=  g2_width
  g_height= g2_height 
  g_pointsize= g2_pointsize 
  g_res= g2_res
  rescom=rescom2
  resext=resext_2
} else if (imagesize=="big") {
  g_width=  g3_width
  g_height= g3_height 
  g_pointsize= g3_pointsize 
  g_res= g3_res
  rescom=rescom3
  resext=resext_3
} else {
  print("please select image size - line 53-55")
}


# save all graphs as images
for (eachgraph in graphlist) {
  thename=as.character(eachgraph)
  thefilename=paste0(thename, resext)
  rescom(filename=thefilename, 
         type=g_type, 
         units=g_units, 
         width=g_width, 
         height=g_height, 
         pointsize=g_pointsize, 
         res=g_res
  )
  eval(str2lang(eachgraph))
  dev.off()
}

# save one of the graphs: "mygraph2"
# thename="mygraph2"
# thefilename=paste0(thename, resext)
# rescom(filename=thefilename, 
#        type=g_type, 
#        units=g_units, 
#        width=g_width, 
#        height=g_height, 
#        pointsize=g_pointsize, 
#        res=g_res
# )
# mygraph2
# dev.off()
# 

