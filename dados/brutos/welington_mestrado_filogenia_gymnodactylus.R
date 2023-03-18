require(phytools)


## Arvore do genero em notação parentética
tree <- read.tree (text="((((((((G._amarali_Clado_H,G._amarali_Clado_G),G._amarali_Clado_F),G._amarali_Clado_E),
                    (G._amarali_Clado_D,G._amarali_Clado_C)),
                    G._amarali_Clado_B),G._amarali_Clado_A),(Gymnodactylus_sp.4,Gymnodactylus_geckoides)),
                    (((((Gymnodactylus_sp.3,Gymnodactylus_sp.2),Gymnodactylus_sp.1),
                    Gymnodactylus_darwinii),Gymnodactylus_guttulatus),Gymnodactylus_vanzolinii));")

plotTree(tree, offset=0.2, ftype="i", fsize=1, lwd=3, color= "royalblue4", add=FALSE, type="phylogram")
?plotTree.barplot
nodelabels()

### Para salvar a figura
### setwd("~/Dropbox/Wellington_LACV/DISSERTAÇÃO/Imagens")
### png (filename="Filogenia.png", width = 800, height = 800, units = "px", pointsize = 16, bg = "white", res = NA)
### plotTree(tree,offset=0.2,ftype="i", fsize=1, lwd=3)
### dev.off()



setwd("~/Dropbox/Wellington_LACV/DISSERTAÇÃO/Planilhas/Serra da Mesa/R")
pdf ("Filogenia.pdf", width = 12, height = 12,bg = "transparent") 
plotTree(tree, offset=0.2, ftype="i", fsize=1.5, lwd=3, color= "royalblue4", add=F, type="phylogram")
dev.off()

require(ggplot2)
require(ggtree)
ggtree(tree)
ggtree(tree, aes(color=group, linetype=group)) + geom_tiplab(aes(subset=(group==2)))
tree <- groupClade(tree, node=c(21, 17))
ggtree(tree, aes(color=group, linetype=group)) + geom_tiplab(aes(subset=(group==2)))


pdf ("Filogenia Cassimiro.pdf", width = 12, height = 12,bg = "transparent")
tree2 <- read.tree (text="(((Gymnodactylus_amarali),(Gymnodactylus_spn.4,Gymnodactylus_geckoides)),
                    (((((Gymnodactylus_spn.3,Gymnodactylus_spn.2),Gymnodactylus_spn.1),
                    Gymnodactylus_darwinii),Gymnodactylus_guttulatus),Gymnodactylus_vanzolinii));")
plotTree(tree2, offset=0.2, ftype="i", fsize=1.5, lwd=3, color= "royalblue4", add=F, type="phylogram")
dev.off()