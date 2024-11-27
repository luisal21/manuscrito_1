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

# Agrupando por especie
ma_sum %>%
  group_by(especie) %>%
  summarise(CD = mean(CD), n = n())


### Staminate
# Solo caracteres del nectar
#fviz_pca_var(pca2, col.var = factor(c("nectar","nectar","nectar","nectar",
#                                      "nectar","aminoacid","aminoacid",
#                                      "aminoacid","aminoacid","aminoacid",
#                                      "aminoacid","aminoacid","aminoacid",
#                                      "aminoacid","aminoacid","aminoacid",
#                                      "aminoacid","aminoacid","aminoacid",
#                                      "aminoacid","aminoacid","aminoacid")), 
#             palette = c("#0073C2FF", "#EFC000FF"),
#             legend.title = "Cluster")

### agrupado por condición + las variables
fviz_pca_biplot(pca2,
                # Fill individuals by groups
                geom.ind = "point",
                fill.ind = machos2$condicion, col.ind = "black",
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
  labs(x = "PC 1 (53.2%)", y = "PC 2 (21.9%)")

### agrupado por especies + las variables
#fviz_pca_biplot(pca2,
#               col.ind = hem_resu2$especies,
#                addEllipses = TRUE, label = "var",
#                col.var = "black", repel = TRUE,
#                legend.title = "Species"
#)

# normalidad de variables
mvn(machos2[5:14], mvnTest = "hz", univariateTest = "SW", 
    univariatePlot = "histogram")

head(machos2[5:14])


# Agrupado por especie
fviz_pca_ind(pca1,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = hem_resu2$especie, # color by groups
             palette = c("#00AFBB", "#E7B800","#FC4E07", "#00AFDB",
                         "#E7B850", "#FC4E50"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups"
)



# Agrupado por condición (SIN elipse)
fviz_pca_ind(pca2,
             geom.ind = "point", # show points only (nbut not "text")
             fill.ind = machos2$condicion, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             #addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "euclid",
             legend.title = "Condition",
             title = "",
             pointshape=21,
             pointsize = 2
)+
  labs(x = "PC 1 (53.2%)", y = "PC 2 (21.9%)")

# Agrupado por condición (CON elipse)
fviz_pca_ind(pca2,
             geom.ind = "point", # show points only (nbut not "text")
             fill.ind = machos2$condicion, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "euclid",
             legend.title = "Condition",
             title = "",
             pointshape=21,
             pointsize = 2,
             label = "none"
)+
  labs(x = "PC 1 (53.2%)", y = "PC 2 (21.9%)")




scale_color_manual(labels = c("CF","CPF", "CAS",
                              "CPP", "CAA", "CM"),
                   values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",
                              "#66A61E", "#E6AB02"))


scale_color_manual(labels = c("CF","CPF", "CAS",
                              "CPP", "CAA", "CM"),
                   values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",
                              "#66A61E", "#E6AB02"))+



  gt <- grid.arrange(pca_hembras, pca_machos,
                     ncol = 1, nrow = 2)
  
  # Add labels to the arranged plots
  p <- as_ggplot(gt) +                                # transform to a ggplot
  draw_plot_label(label = c("A", "B"), size = 15,
                  x = c(0,0), y = c(1,.5))#Add labels
p


set.seed(0)
# calculando PERMANOVA por condición
dune.div6 <- adonis2(dune6 ~ condicion, data = machos2,
                     permutations = 999, method="euclidean")
dune.div6

# analisis de normalidad del volumen de nectar
# Hembras
hist(hem_nec$VN)
hist(sqrt(hem_nec$VN))
shapiro.test(hem_nec$VN)
shapiro.test(sqrt(hem_nec$VN)) # raiz cuadrada

# Machos
hist(mac_nec$VN)
hist(log(mac_nec$VN))
shapiro.test(mac_nec$VN)
shapiro.test(log(mac_nec$VN)) # logaritmo

### Análisis estadistico del volumen de nectar
# Hembras
aov_hemnec = aov(sqrt(VN) ~ condición, data = hem_nec)
summary(aov_hemnec)
# otra opción
#GLM con distribución gamma
glm_hemnec = glm(VN ~ condición, data = hem_nec, family = Gamma)
summary(glm_hemnec)
anova(glm_hemnec)

# Machos
aov_macnec = aov(log(VN) ~ condicion, data = mac_nec)
summary(aov_macnec)
# otra opción-GLM
glm_macnec = glm(VN ~ condicion, data = mac_nec, family = Gamma)
summary(glm_macnec)
anova(glm_macnec)

