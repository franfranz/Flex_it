# Package
# plot to use instead of cake 

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
        palette=c("#00008B", #femm sing
        "#00B2EE", # fem plur
        "#FF8C00", # masc sing
        "#CD2626" # masc plur
        ), # these colors are random
        
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

