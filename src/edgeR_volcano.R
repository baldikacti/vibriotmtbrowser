library(edgeR)
library(tidyverse)
library(plotly)

#getting CCNA 
df_vibrio <- vibrio_tmt_data[[1]]
x <- df_vibrio[, c(1, 7:15)] %>% column_to_rownames("Accession") %>%
  drop_na()


group <- factor(c(1,1,1,2,2,2,3,3,3)) # Set groups for differential expression
y <- DGEList(counts=x,group=group)

#keep <- rowSums(cpm(y)>30) >= 3 
#y <- y[keep, , keep.lib.sizes=FALSE] # Filters out genes with low counts

y <- calcNormFactors(y)
design <- model.matrix(~group)
y <- estimateDisp(y,design)
fit <- glmQLFit(y,design)

## Differential expression results of each comparison. Using "coef" or "contrast" you 
## set the comparison to be done
t30vt0qlf <- glmQLFTest(fit,coef=2)
E4vWTqlf <- glmQLFTest(fit,coef=3)
XGvWTqlf <- glmQLFTest(fit,coef=4)
XGvDLqlf <- glmQLFTest(fit,contrast=c(0,-1,0,1))


qlffile <- t30vt0qlf

## KEGG pathway analysis
one <- topTags(qlffile,n=Inf)
keg <- kegga(qlffile,species.KEGG="ccs",FDR=0.05)
topKEGG(keg,sort="up") # Prints up regulated pathways
topKEGG(keg,sort="down") # Prints down regulated pathways

#important to change KEGG path here to what you care about
kegpath <- "path:ccs02040"

#make a new data frame and mark those with FDR < 0.05
df <- one$table
df$signifcance <- ifelse(df$FDR<0.05,"FDR < 0.05","FDR > 0.05")
df$genename <- row.names(one$table)
newish <- merge(df,CCNA_description,by="genename")
new <- newish[order(newish$FDR),]
kegdescriptor <- GPN$Description[GPN$PathwayID==kegpath]

# extract the gene names from a certain KEGG path 
i <- row.names(one$table) %in% GK$GeneID[GK$PathwayID==kegpath]
newvec1 <- row.names(one$table[i,])

dfpath1 <- subset(new, (new$genename %in% newvec1))
dfpath1 <- subset(dfpath1,dfpath1$FDR<0.1)

#using ggplot2 to make the volcano plot for the data
volc <- ggplot(df,aes(x=logFC,y=-log10(PValue),text=paste(genename,sep="\n"),color=signifcance)) + 
  geom_point(alpha=0.6) + 
  scale_color_manual(values = c("red","black")) +
  theme_classic() +
  ggtitle(paste("30m v 0m"))
volc
## Interactive Volcano plot
ggplotly(volc, tooltip = "text")


## Recolor the selected genes from the KEGG pathway
volc2 <- volc + geom_point(data=subset(dfpath1),aes(x=logFC,y=-log10(PValue)),color="blue",size=4) +
  ggtitle(paste("Delta Lon v WT", kegdescriptor, sep="\n")) + 
  theme_classic()
ggplotly(volc2, tooltip = "text")


