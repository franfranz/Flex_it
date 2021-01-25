
#v3.0.0


#   Database with the forms of inflectional suffixes in Italian nouns 
#   
#   Merge word frequencies collected from Itwac (Baroni et al., 2009) 
#   and a list of morphologically tagged words (Morph-it!, Zanchetta & Baroni, 2005)


# clear ws
rm(list=ls())

# required packages: stringr, pryr, treemap # tidyverse


# ### #
#
#   INPUT REQUIRED: GRAPHICAL SETTINGS 
#


library(pryr)

## input required: graphical parameters 

# palette - different hues for masc and femm, plur are darker than sing
col_fp = "#008B00" #green4       # "#00008B" # "blue4" 
col_fs = "#43CD80" #seagreen3    # "#00B2EE" # "deepskyblue2" 
col_mp = "#CD2990"  #maroon3     # "#CD2626" # "firebrick3"  
col_ms = "#EEAEEE" #plum2        # "#FF8C00" # "darkorange"  
pal_01=c(col_fp, col_fs, col_mp, col_ms)

legendcontent=c("Fem. Plur.","Fem. Sing.","Masc. Plur.", "Masc. Sing.")



# ### #
#
#   INPUT REQUIRED: PATHS
#


# input directory
#inwd <- '/Users/valentinapescuma/Documents/PhD/ItalianMorphophonolgy'


# output directories
#outwd=
#graphwd= whre the graphs are stored 

# where the code is stored
#codewd=


# filename of the itwac list currently in use
  # currently using the "raw" version
#thefilename_itwac="itwac_nouns_lemmas_raw_2_0_0.csv"

thefilename_itwac="itwac_nouns_lemmas_notail_2_0_0.csv"




# ### #
#
#   IMPORT AND PREPROCESS
#

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

nouns_all$lastchar=stringr::str_sub(all_nouns$Form, -1,-1)



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



# ### #
#
#   DETECT INVARIANT/AMBIGUOUS FORMS 
#

# 1)  what nouns are present in the four features ("capolista", "portavoce", reporter")
all_nouns_amb1fp=alln_fp[(alln_fp$Form %in% alln_fs$Form==T)
                       &(alln_fp$Form %in% alln_mp$Form==T)
                       &(alln_fp$Form %in% alln_ms$Form==T), ]

all_nouns_amb1fs=alln_fs[(alln_fs$Form %in% alln_fp$Form==T)
                         &(alln_fs$Form %in% alln_mp$Form==T)
                         &(alln_fs$Form %in% alln_ms$Form==T), ]

all_nouns_amb1mp=alln_mp[(alln_mp$Form %in% alln_fp$Form==T)
                         &(alln_mp$Form %in% alln_fs$Form==T)
                         &(alln_mp$Form %in% alln_ms$Form==T), ]

all_nouns_amb1ms=alln_ms[(alln_ms$Form %in% alln_fp$Form==T)
                       &(alln_ms$Form %in% alln_fs$Form==T)
                       &(alln_ms$Form %in% alln_mp$Form==T), ]

summary((unique(all_nouns_amb1fp$Form)==unique(all_nouns_amb1fs$Form) ) 
        & (unique(all_nouns_amb1fp$Form))== (unique(all_nouns_amb1ms$Form))
        & (unique(all_nouns_amb1fp$Form))== (unique(all_nouns_amb1mp$Form))
)
# total token frequency of all_nouns_amb1
sum(all_nouns_amb1fp$Freq)

# bind all the nouns in a df
all_nouns_amb1=rbind(all_nouns_amb1fp, all_nouns_amb1fs, all_nouns_amb1mp, all_nouns_amb1ms)

# subtract the df from the df with all nouns
all_nouns_1=nouns_all[!(nouns_all$Form %in% all_nouns_amb1$Form), ]

# check whether any of the forms was left out or any form was wrongfully included 
dim(nouns_all) - dim(all_nouns_1)== dim(all_nouns_amb1)

dim(all_nouns_1)


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
