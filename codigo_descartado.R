# Codigo descartado manuscrito 1

# Solo caracteres del nectar
#fviz_pca_var(pca1, col.var = factor(c("nectar","nectar","nectar","nectar",
#                                      "nectar","aminoacid","aminoacid",
#                                      "aminoacid","aminoacid","aminoacid",
#                                      "aminoacid","aminoacid","aminoacid",
#                                      "aminoacid","aminoacid","aminoacid",
#                                      "aminoacid","aminoacid","aminoacid",
#                                      "aminoacid","aminoacid","aminoacid")), 
#             palette = c("#0073C2FF", "#EFC000FF"),
#             legend.title = "Cluster")

# Agrupado por condición (SIN elipse)
fviz_pca_ind(pca1,
             geom.ind = "point", # show points only (nbut not "text")
             fill.ind = hem_resu2$condición, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             #addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "euclid",
             legend.title = "Condition",
             title = "",
             pointshape=21,
             pointsize = 2
)+
  labs(x = "PC 1 (67.8%)", y = "PC 2 (13.3%)")

# Agrupado por condición (CON elipse)
fviz_pca_ind(pca1,
             geom.ind = "point", # show points only (nbut not "text")
             fill.ind = hem_resu2$condición, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "euclid",
             legend.title = "Condition",
             title = "",
             pointshape=21,
             pointsize = 2,
             label = "none"
)+
  labs(x = "PC 1 (67.8%)", y = "PC 2 (13.3%)")

display.brewer.all()
brewer.pal(n = 8, name = "Dark2")


### agrupado por condición + las variables
fviz_pca_biplot(pca1,
                # Fill individuals by groups
                geom.ind = "point",
                fill.ind = hem_resu2$condición, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                #addEllipses = TRUE,
                # Variables
                #alpha.var = "contrib",  col.var = "contrib",
                #repel = TRUE,
                legend.title = "Condition",
                title = ""
)+
  scale_color_manual(labels = c("Domesticated", "Wild"),
                     values= c("#00AFBB", "#E7B800"))+
  labs(x = "PC 1 (67.8%)", y = "PC 2 (13.3%)")

### agrupado por especies + las variables
#fviz_pca_biplot(pca1,
#               col.ind = hem_resu2$especies,
#                addEllipses = TRUE, label = "var",
#                col.var = "black", repel = TRUE,
#                legend.title = "Species"
#)

# normalidad de variables
library(MVN)
mvn(hem_resu2[6:17], mvnTest = "hz", univariateTest = "SW", 
    univariatePlot = "histogram")

head(hem_resu2[6:17])

set.seed(0)
dune.div5 <- adonis2(dune5 ~ condición, data = hem_resu2,
                     permutations = 999, method="euclidean")
dune.div5


