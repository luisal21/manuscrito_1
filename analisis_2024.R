# En este documento se muestran los análisis de los caracteres florales de las 
# diferentes especies de Cucurbita utilizadas en este estudio. También se evalua 
# la concentración de azucares y aminoácidos, además del tamaño, producción y 
# proporción de proteinas y lipidos del polen.

library(usethis)
use_git_config(
  user.name = "luisal21",
  user.email = "lvillanueva@cieco.unam.mx"
)
create_github_token()

library(gitcreds)
gitcreds_set()

#### Funciones que se van a utilizar en el script ####
# Función para obtener el Error Estandar
es <- function(x){
  sd(x) / sqrt(length(x))
}
# Función para obtener el coeficiente de variación
cv <- function(x){
  (sd(x)/mean(x))*100
}
# Función para normalizar valores usando el método min-max
min_max_norm <- function (x) {
  (x - min (x)) / (max (x) - min (x))
}

#### Paquetes a utilizar ####
library(car)
library(MASS)
library(lme4)
#library(nlme)
#library(fitdistrplus)
library(ggplot2)
library(ggsignif)
library(forcats)
library(dplyr)
library(emmeans)
library(multcomp)
#install.packages("multcompView")
library(multcompView)
library(tidyr)

#### Estableciendo directorio en el cual se encuentran las bases de datos ####
setwd("/home/luis/Documents/3_Doctorado_UNAM/manuscrito_1/data_variables")
#### Cargando base de datos de caracteres florales de flores hembra ####
hembras <- read.csv("flores_hembra.csv", header = T)
head(hembras)
str(hembras)
tapply(hembras$CD, list(hembras$especie, hembras$sitio), length)
tapply(hembras$CD, hembras$sitio, length)

### Subset de las flores de Morelia
hembras = subset(hembras, sitio == "morelia")
head(hembras)
str(hembras)
tapply(hembras$CD, hembras$especie, length)


# Agrupando por ID de la planta
hem_resu <- hembras %>%
  group_by(año,condición,especie2,especie,id_planta2) %>%
  dplyr::summarize(CD = mean(CD), TL = mean(TL), CL = mean(CL), TD1 = mean(TD1),
                   TD2 = mean(TD2), TD3 = mean(TD3), NDf = mean(NDf), SD = mean(SD),
                   PL = mean(PL), SL = mean(SL), OL = mean(OL), OD = mean(OD), 
                   VN = mean(vol_nec), n = n())

hem_resu2 <- as.data.frame(hem_resu)
str(hem_resu2)
tapply(hem_resu2$CD, hem_resu2$especie, length)
#write.csv(hem_resu2, file="hem_resu.csv")

#### Estadistica descriptiva de los caracteres de flores hembra ####
# Media
media = hem_resu2 %>%
  group_by(especie) %>%
  dplyr::summarize(CD = mean(CD), TL = mean(TL), CL = mean(CL), TD1 = mean(TD1),
                   TD2 = mean(TD2), TD3 = mean(TD3), NDf = mean(NDf), SD = mean(SD),
                   PL = mean(PL), SL = mean(SL), OL = mean(OL), OD = mean(OD), 
                   VN = mean(VN),n = n())
View(media)
#write.csv(media, file = "hembras_mean.csv")
# Error estandar
erro = hembras %>%
  group_by(especie) %>%
  dplyr::summarize(CD = es(CD), TL = es(TL), CL = es(CL), TD1 = es(TD1), 
                   TD2 = es(TD2), TD3 = es(TD3), NDf = es(NDf), SD = es(SD), 
                   PL = es(PL), SL = es(SL), OL = es(OL), OD = es(OD), 
                   VN = es(vol_nec), n = n())
View(erro)
# Coeficiente de variación
CV = hembras %>%
  group_by(especie) %>%
  dplyr::summarize(CD = cv(CD), TL = cv(TL), CL = cv(CL), TD1 = cv(TD1),
                   TD2 = cv(TD2), TD3 = cv(TD3), NDf = cv(NDf), SD = cv(SD), 
                   PL = cv(PL), SL = cv(SL), OL = cv(OL), OD = cv(OD),
                   VN = cv(vol_nec), n = n())
View(CV)

#### Principal component analysis (PCA) de los caracteres florales ####
# Primeras 6 lineas de base de rasgos de flores hembra
head(hem_resu2)
str(hem_resu2)
# Graficando todas las variables
pairs(hem_resu2[6:17])
#pairs(hem_resu2[5:17]) # considerando nectar
plot(hem_resu2$CD, hem_resu2$VN)
# Realizando el PCA con la función prcomp
pca1 <- prcomp(hem_resu2[, 6:17], scale = T)
# información que genera el PCA
summary(pca1)
pca1$rotation[, 1:2]
plot(pca1)
# Primera visualización del PCA
biplot(pca1, scale=0)

# Cargando librerias para graficar PCAs
library(FactoMineR)
library(factoextra)

# Obteniendo información sobre los eigenvalues del PCA
get_eigenvalue(pca1)
# porcentaje de varianza explicada por cada componente del PCA
fviz_eig(pca1, addlabels = T)
# Información que se obtiene de las variables que usamos para el PCA
var = get_pca_var(pca1); var

# Visualizando las variables del PCA
fviz_pca_var(pca1, col.var = "black")
# Calidad de la representación de las variables en el mapa de factores
head(var$cos2)
# Cargando paqueteria "corrplot" para visualizar correlaciones
library(corrplot)
# Correlaciones entre calidades (cos2)
corrplot(var$cos2, is.corr = F)
# Visualizando la calidad de la representación, que variables estan mejor
# representadas. Grafica de barras y mapa de factores
fviz_cos2(pca1, choice = "var", axes = 1:2)
fviz_pca_var(pca1, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
# Contribución de cada variable
head(var$contrib)
corrplot(var$contrib, is.corr=FALSE)    

# Contributions of variables to PC1
fviz_contrib(pca1, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(pca1, choice = "var", axes = 2, top = 10)
# Contribución de las variables sumando los 2 componentes
fviz_contrib(pca1, choice = "var", axes = 1:2, top = 10)
# Gráfica de PCA de las variables coloreando por contribución 
fviz_pca_var(pca1, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

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

### Gráfica de individuos (filas)
ind = get_pca_ind(pca1)
# Elementos que se pueden sacar de los ind
ind
# Visualizando los individuos en el grafico de PCA
fviz_pca_ind(pca1)
# Visualizando los individuos en el PCA en función de cos2
fviz_pca_ind(pca1, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
# Visualizando los individuos en el PCA en funcións de cos2
fviz_pca_ind(pca1, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)
# Total contribution on PC1 and PC2
fviz_contrib(pca1, choice = "ind", axes = 1:2)
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

# Agrupado por especie
fviz_pca_ind(pca1,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = hem_resu2$especie, # color by groups
             #palette = ,
             #addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "convex",
            legend.title = "Species",
            title = "",
            pointshape = 19,
            pointsize = 2,
            #label = "none"
)+
  labs(x = "PC 1 (67.8%)", y = "PC 2 (13.3%)")+
  scale_color_manual(labels = c("CF","CPF", "CAS",
                                "CPP", "CAA", "CM"),
  values = c("#00AFBB", "#E7B800","#FC4E07", "#00AFDB",
               "#E7B850", "#FC4E50"))

colors()

#palette = c("#00AFBB", "#E7B800","#FC4E07", "#00AFDB",
#            "#E7B850", "#FC4E50"),


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


### PERMANOVA de flores hembra
library(vegan)
head(hem_resu2)
str(hem_resu2)
dune5 = hem_resu2[6:17]
# calculando PERMANOVA por condición
set.seed(0)
dune.div5 <- adonis2(dune5 ~ condición, data = hem_resu2,
                     permutations = 999, method="euclidean")
dune.div5
set.seed(0)
# calculando PERMANOVA por especie
dune.div6 <- adonis2(dune5 ~ especie, data = hem_resu2,
                     permutations = 999, method="euclidean")
dune.div6


# calculando beta dispersión (homogeneidad de varianzas)
bd <- betadisper(dune.dist5, hem_resu2$condición)
anova(bd)
permutest(bd)
plot(bd, hull=FALSE, ellipse=T) ##sd ellipse
# calculando dispersión por especie
dispersion6 <- betadisper(dune.dist5, group=hem_resu2$especie)
permutest(dispersion6)
plot(dispersion6, hull=FALSE, ellipse=T) ##sd ellipse


#### Base de datos de caracteres florales de flores Macho de 6 especies ####
# Cucurbita medidas en 2021 y en 2022
machos <- read.csv("flores_macho.csv", header = T)
head(machos)
str(machos)
tapply(machos$CD, list(machos$especie, machos$sitio), length)
tapply(machos$CD, list(machos$especie, machos$tratamiento), length)
tapply(machos$CD, machos$sitio, length)
# subset de datos de Morelia
machos = subset(machos, sitio == "morelia")
str(machos)

# Agrupando por individuos
machos2 = machos %>%
  group_by(año, condicion, especie, id_planta) %>%
  summarise(CD = mean(CD), TL = mean(TL), CL = mean(CL), TD1 = mean(TD1),
            TD2 = mean(TD2), TD3 = mean(TD3), NDm = mean(NDm), AD = mean(AD),
            StL = mean(StL), AL = mean(AL), VN = mean(vol_nec), n = n())
str(machos2)
tapply(machos2$CD, machos2$especie, length)
#View(machos2)
#write.csv(machos2, file = "machos2_prom.csv")
# Agrupando por especie
ma_sum %>%
  group_by(especie) %>%
  summarise(CD = mean(CD), n = n())

#### Estadistica descriptiva de las flores macho ####
# Media
machos3 = machos2 %>%
  group_by(especie) %>%
  summarise(CD = mean(CD), TL = mean(TL), CL = mean(CL), TD1 = mean(TD1),
            TD2 = mean(TD2), TD3 = mean(TD3), NDm = mean(NDm), AD = mean(AD),
            StL = mean(StL), AL = mean(AL), VN = mean(VN), n = n())
View(machos3)
write.csv(machos3, file = "machos_mean.csv")
# Error estandar
machos %>%
  group_by(especie) %>%
  summarise(CD = es(CD), TL = es(TL), CL = es(CL), TD1 = es(TD1), 
            TD2 = es(TD2), TD3 = es(TD3), NDm = es(NDm), AD = es(AD), 
            StL = es(StL), AL = es(AL), VN = es(vol_nec), n = n())
# Coeficiente de variación
machos %>%
  group_by(especie) %>%
  summarise(CD = cv(CD), TL = cv(TL), CL = cv(CL), TD1 = cv(TD1),
            TD2 = cv(TD2), TD3 = cv(TD3), NDm = cv(NDm), AD = cv(AD), 
            StL = cv(StL), AL = cv(AL), VN = cv(vol_nec), n = n())

#### Principal component analysis (PCA) de las flores macho ####
head(machos2)
machos2 = as.data.frame(machos2)
str(machos2)
pairs(machos2[5:14])
pca2 <- prcomp(machos2[, 5:14], scale = T)
summary(pca2)
pca2$rotation[, 1:2]
#write.csv(pca1$rotation[, 1:2], file = "pca_hembras.csv")
plot(pca2)
biplot(pca2, scale=0)

# Obteniendo información sobre los eigenvalues del PCA
get_eigenvalue(pca2)
# porcentaje de varianza explicada por cada componente del PCA
fviz_eig(pca2, addlabels = T)
# Información que se obtiene de las variables que usamos para el PCA
var = get_pca_var(pca2); var

# Visualizando las variables del PCA
fviz_pca_var(pca2, col.var = "black")
# Calidad de la representación de las variables en el mapa de factores
head(var$cos2)
# Cargando paqueteria "corrplot" para visualizar correlaciones
library(corrplot)
# Correlaciones entre calidades (cos2)
corrplot(var$cos2, is.corr = F)
# Visualizando la calidad de la representación, que variables estan mejor
# representadas. Grafica de barras y mapa de factores
fviz_cos2(pca2, choice = "var", axes = 1:2)
fviz_pca_var(pca2, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
# Contribución de cada variable
head(var$contrib)
corrplot(var$contrib, is.corr=FALSE)    

# Contributions of variables to PC1
fviz_contrib(pca2, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(pca2, choice = "var", axes = 2, top = 10)
# Contribución de las variables sumando los 2 componentes
fviz_contrib(pca2, choice = "var", axes = 1:2, top = 10)
# Gráfica de PCA de las variables coloreando por contribución 
fviz_pca_var(pca2, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

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

### Gráfica de individuos (filas)
ind = get_pca_ind(pca2)
# Elementos que se pueden sacar de los ind
ind
# Visualizando los individuos en el grafico de PCA
fviz_pca_ind(pca2)
# Visualizando los individuos en el PCA en función de cos2
fviz_pca_ind(pca2, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
# Visualizando los individuos en el PCA en funcións de cos2
fviz_pca_ind(pca2, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)
# Total contribution on PC1 and PC2
fviz_contrib(pca2, choice = "ind", axes = 1:2)
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

# Agrupado por especie
#fviz_pca_ind(pca1,
#             geom.ind = "point", # show points only (nbut not "text")
#             col.ind = hem_resu2$especie, # color by groups
#             palette = c("#00AFBB", "#E7B800","#FC4E07", "#00AFDB",
#                         "#E7B850", "#FC4E50"),
#             addEllipses = TRUE, # Concentration ellipses
#ellipse.type = "confidence",
#             legend.title = "Groups"
#)
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

### PERMANOVA de flores macho
head(machos2)
str(machos2)
dune6 = machos2[5:14]
# Calculando distancia euclidiana
set.seed(0)
# calculando PERMANOVA por condición
dune.div6 <- adonis2(dune6 ~ condicion, data = machos2,
                     permutations = 999, method="euclidean")
dune.div6
set.seed(0)
# calculando PERMANOVA por especie
dune.div7 <- adonis2(dune6 ~ especie, data = machos2,
                     permutations = 999, method="euclidean")
dune.div7

# overall tests
#adonis2(dune ~ especie, data = hembras, 
#        permutations = 999, method="euclidean", by = NULL)

# calculando beta dispersión (homogeneidad de varianzas)
dispersion5 <- betadisper(dune.dist5, group=amino$condición)
permutest(dispersion5)
plot(dispersion5, hull=FALSE, ellipse=T) ##sd ellipse
# calculando dispersión por especie
dispersion6 <- betadisper(dune.dist5, group=amino$spp)
permutest(dispersion6)
plot(dispersion6, hull=FALSE, ellipse=T) ##sd ellipse
# calculando dispersión por sexo
dispersion6 <- betadisper(dune.dist5, group=amino$sexo_flor)
permutest(dispersion6)
plot(dispersion6, hull=FALSE, ellipse=T) ##sd ellipse


# Joining PCoA plot from female and male flowers
library(gridExtra)
library(cowplot)
library(ggpubr)

# uniendo graficas
gt <- grid.arrange(calf1, calf3,
                   ncol = 1, nrow = 2)
# Add labels to the arranged plots
p <- as_ggplot(gt) +                                # transform to a ggplot
  draw_plot_label(label = c("A", "B"), size = 15,
                  x = c(0,0), y = c(1,.5))#Add labels
p


#### VOLUMEN DE NECTAR ####
# Volumen de néctar de flores hembra
vol_nec2 = subset(hembras, vol_nec != 0)
vol_nec3 = subset(vol_nec2, vol_nec < 300)
sort(vol_nec3$vol_nec)
boxplot(vol_nec3$vol_nec ~ vol_nec3$especie)
tapply(vol_nec3$vol_nec, vol_nec3$especie, mean)
tapply(vol_nec3$vol_nec, vol_nec3$especie, es)
str(vol_nec3)

# Agrupando el volumen de nectar por individuo
hem_nec <- vol_nec3 %>%
  group_by(año,condición,especie,id_planta2) %>%
  dplyr::summarize(VN = mean(vol_nec), n = n()); hem_nec


# Volumen de néctar de flores macho
machos2 <- subset(machos, vol_nec != 0)
machos3 <- subset(machos2, vol_nec < 150)
str(machos3)
sort(machos3$vol_nec)
# Agrupando volumen de néctar por individuo
mac_nec <- machos3 %>%
  group_by(año,condicion,especie,id_planta2) %>%
  dplyr::summarize(VN = mean(vol_nec), n = n()); mac_nec
# obteniendo media, error standard y numero de individuos por condicion
tapply(mac_nec$VN, mac_nec$condicion, mean)
tapply(mac_nec$VN, mac_nec$condicion, es)
tapply(mac_nec$VN, mac_nec$condicion, length)
boxplot(mac_nec$VN ~ mac_nec$condicion)

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


### Uniendo base de hembras y machos
str(hem_nec)
str(mac_nec)
#write.csv(hem_nec, file = "nectar_female.csv")
#write.csv(mac_nec, file = "nectar_male.csv")

# Cargando base conjunta de nectar 
volumen = read.csv("nectar_volume.csv", header = T)
head(volumen)
str(volumen)
hist(volumen$VN)
shapiro.test(volumen$VN)
shapiro.test(log(volumen$VN))
hist(log(volumen$VN))

volu = glm(log(VN) ~ especie*sexo, data = volumen, family = gaussian)
summary(volu) #, dispersion=1
Anova(volu)
shapiro.test(residuals(volu))
plot(residuals(volu))
volu_compa <- emmeans(volu, 
                      specs = pairwise ~ especie | sexo, 
                      type = "response")
volu_compa
volu_compa.m<-volu_compa$emmean 
volu_compa.m
model_means_cld_volu <- cld(object = volu_compa.m,
                            adjust = "sidak",
                            Letter = letters, 
                            alpha = 0.05)
model_means_cld_volu



volu2 = glm(VN ~ especie*sexo, data = volumen, family = Gamma)
summary(volu2) #, dispersion=1
Anova(volu2)
shapiro.test(residuals(volu2))
plot(residuals(volu2))


#### PCA con los caracteres del néctar ####
# Caracteres del nectar floral (volumen, azucares y aminoacidos) de flores
# hembra y macho
nectar = read.csv("traits_nectar.csv", header = T)
head(nectar)
str(nectar)
tapply(nectar$fructose, nectar$sexo, length)
# Separando machos y hembras
# flores hembra
nectar_h = subset(nectar, sexo =="h")
str(nectar_h)
# flores macho
nectar_m = subset(nectar, sexo =="m")
str(nectar_m)

# PCA de caracteres del néctar de flores hembra
# visualizando todas las combinaciones entre las variables
pairs(nectar_h[8:29], lower.panel = NULL)
# Calculando el PCA para flores hembra
pca4 = prcomp(nectar_h[8:29], scale = T)
summary(pca4)
# Obteniendo información sobre los eigenvalues
get_eigenvalue(pca4)
# calculando la varianza explicada por cada componente
fviz_eig(pca4, addlabels = T)
# información que podemos obtener de las variables analizadas
var = get_pca_var(pca4); var
# Visualización de las variables
fviz_pca_var(pca4, col.var = "black")

# correlaciones entre calidades de variables
corrplot(var$cos2, is.corr = F)
# Calidad de cada variable 
fviz_cos2(pca4, choice = "var", axes = 1:2)
# graficando las variables en el mapa de factores
fviz_pca_var(pca4, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
# Contribución a los componentes
head(var$contrib)
corrplot(var$contrib, is.corr=FALSE)    

# Contributions of variables to PC1
fviz_contrib(pca4, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(pca4, choice = "var", axes = 2, top = 10)
# contribución combinada de los componentes 1  y 2
fviz_contrib(pca4, choice = "var", axes = 1:2, top = 10)
# visualizando las variables coloreadas por contribución
fviz_pca_var(pca4, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# Solo caracteres del nectar
fviz_pca_var(pca4, col.var = factor(c("nectar","nectar","nectar",
                                      "nectar","nectar","aminoacid","aminoacid",
                                      "aminoacid","aminoacid","aminoacid",
                                      "aminoacid","aminoacid","aminoacid",
                                      "aminoacid","aminoacid","aminoacid",
                                      "aminoacid","aminoacid","aminoacid",
                                      "aminoacid","aminoacid","aminoacid")), 
             palette = c("#0073C2FF", "#EFC000FF"),
             legend.title = "Cluster")

### Graph of individuals (rows)
ind = get_pca_ind(pca4)
ind

fviz_pca_ind(pca4)
# Visualizando los individuos por cos2
fviz_pca_ind(pca4, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
#
fviz_pca_ind(pca4, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# Total contribution on PC1 and PC2
fviz_contrib(pca4, choice = "ind", axes = 1:2)

# Agrupado por condición (SIN elipse)
fviz_pca_ind(pca4,
             geom.ind = "point", # show points only (nbut not "text")
             fill.ind = nectar_h$condicion, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             #addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "euclid",
             legend.title = "Condition",
             title = "",
             pointshape=21,
             pointsize = 2
)+
labs(x = "PC 1 (54%)", y = "PC 2 (12.7%)")

# Agrupado por condición (CON elipse)
fviz_pca_ind(pca4,
             geom.ind = "point", # show points only (nbut not "text")
             fill.ind = nectar_h$condicion, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "euclid",
             legend.title = "Condition",
             title = "",
             pointshape=21,
             pointsize = 2,
             label = "none"
)+
  labs(x = "PC 1 (54%)", y = "PC 2 (12.7%)")


# Agrupado por especie
fviz_pca_ind(pca4,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = nectar_h$spp, # color by groups
             palette = c("#00AFBB", "#E7B800","#FC4E07", "#00AFDB",
                         "#E7B850"),
             addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "confidence",
             legend.title = "Groups"
)

### agrupado por condición + las variables
fviz_pca_biplot(pca4,
                col.ind = nectar_h$condicion, palette = "jco",
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Condition"
)

library(MVN)
# Flores hembra
mvn(nectar_h[8:29], mvnTest = "hz", univariateTest = "SW", 
    univariatePlot = "histogram")
mvn(nectar_h[8:12], mvnTest = "hz", univariateTest = "SW", 
    univariatePlot = "histogram")


# Flores macho
mvn(nectar_m[8:29], mvnTest = "hz", univariateTest = "SW", 
    univariatePlot = "histogram")


# PERMANOVA del nectar de flores hembra
str(nectar_h)
nectar_h[8:29]
dune8 = nectar_h[8:29]
# Calculando distancia euclidiana
set.seed(0)
# permanova
dune.div8 <- adonis2(dune8 ~ condicion, data = nectar_h,
                     permutations = 999, method="euclidean")
dune.div8

# calculando PERMANOVA por especie
dune.div8 <- adonis2(dune8 ~ spp, data = nectar_h,
                     permutations = 999, method="euclidean")
dune.div8

# overall tests
#adonis2(dune ~ especie, data = hembras, 
#        permutations = 999, method="euclidean", by = NULL)

# calculando beta dispersión (homogeneidad de varianzas)
dispersion5 <- betadisper(dune.dist5, group=amino$condición)
permutest(dispersion5)
plot(dispersion5, hull=FALSE, ellipse=T) ##sd ellipse
# calculando dispersión por especie
dispersion6 <- betadisper(dune.dist5, group=amino$spp)
permutest(dispersion6)
plot(dispersion6, hull=FALSE, ellipse=T) ##sd ellipse
# calculando dispersión por sexo
dispersion6 <- betadisper(dune.dist5, group=amino$sexo_flor)
permutest(dispersion6)
plot(dispersion6, hull=FALSE, ellipse=T) ##sd ellipse

# MANOVA del nectar
# MANOVA test
mod1<-manova(cbind(fructose, glucose, sucrose) ~ condicion, 
             data = nectar_h)
summary(mod1) #Diferencias significativas



### FLORES MACHO
nectar_m = subset(nectar, sexo =="m")
str(nectar_m)

# PCA de caracteres del néctar de flores hembra
# visualizando todas las combinaciones entre las variables
pairs(nectar_m[8:29], lower.panel = NULL)
# Calculando el PCA para flores machos
pca5 = prcomp(nectar_m[8:29], scale = T)
summary(pca5)
# Obteniendo información sobre los eigenvalues
get_eigenvalue(pca5)
# calculando la varianza explicada por cada componente
fviz_eig(pca5, addlabels = T)
# información que podemos obtener de las variables analizadas
var = get_pca_var(pca5); var
# Visualización de las variables
fviz_pca_var(pca5, col.var = "black")

# correlaciones entre calidades de variables
corrplot(var$cos2, is.corr = F)
# Calidad de cada variable 
fviz_cos2(pca5, choice = "var", axes = 1:2)
# graficando las variables en el mapa de factores
fviz_pca_var(pca5, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
# Contribución a los componentes
head(var$contrib)
corrplot(var$contrib, is.corr=FALSE)    

# Contributions of variables to PC1
fviz_contrib(pca5, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(pca5, choice = "var", axes = 2, top = 10)
# contribución combinada de los componentes 1  y 2
fviz_contrib(pca5, choice = "var", axes = 1:2, top = 10)
# visualizando las variables coloreadas por contribución
fviz_pca_var(pca5, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# Solo caracteres del nectar
fviz_pca_var(pca5, col.var = factor(c("nectar","nectar","nectar",
                                      "nectar","nectar","aminoacid","aminoacid",
                                      "aminoacid","aminoacid","aminoacid",
                                      "aminoacid","aminoacid","aminoacid",
                                      "aminoacid","aminoacid","aminoacid",
                                      "aminoacid","aminoacid","aminoacid",
                                      "aminoacid","aminoacid","aminoacid")), 
             palette = c("#0073C2FF", "#EFC000FF"),
             legend.title = "Cluster")

### Graph of individuals (rows)
ind = get_pca_ind(pca5)
ind

fviz_pca_ind(pca5)
# Visualizando los individuos por cos2
fviz_pca_ind(pca5, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
#
fviz_pca_ind(pca5, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# Total contribution on PC1 and PC2
fviz_contrib(pca5, choice = "ind", axes = 1:2)

# Agrupado por condición (SIN elipse)
fviz_pca_ind(pca5,
             geom.ind = "point", # show points only (nbut not "text")
             fill.ind = nectar_m$condicion, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             #addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "euclid",
             legend.title = "Condition",
             title = "",
             pointshape=21,
             pointsize = 2
)+
  labs(x = "PC 1 (53.4%)", y = "PC 2 (11.6%)")

# Agrupado por condición (CON elipse)
fviz_pca_ind(pca5,
             geom.ind = "point", # show points only (nbut not "text")
             fill.ind = nectar_m$condicion, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "euclid",
             legend.title = "Condition",
             title = "",
             pointshape=21,
             pointsize = 2,
             label = "none"
)+
  labs(x = "PC 1 (53.4%)", y = "PC 2 (11.6%)")

# Agrupado por especie
fviz_pca_ind(pca5,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = nectar_m$spp, # color by groups
             palette = c("#00AFBB", "#E7B800","#FC4E07", "#00AFDB",
                         "#E7B850", "#FF8400"),
             addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "confidence",
             legend.title = "Groups"
)

### agrupado por condición + las variables
fviz_pca_biplot(pca5,
                col.ind = nectar_m$condicion, palette = "jco",
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Condition"
)

# PERMANOVA del nectar de floress macho
str(nectar_m)
nectar_m[8:29]
dune9 = nectar_m[8:29]
# Calculando distancia euclidiana
set.seed(0)
# permanova
dune.div9 <- adonis2(dune9 ~ condicion, data = nectar_m,
                     permutations = 999, method="euclidean")
dune.div9

# calculando PERMANOVA por especie
dune.div9 <- adonis2(dune9 ~ spp, data = nectar_m,
                     permutations = 999, method="euclidean")
dune.div9

# overall tests
#adonis2(dune ~ especie, data = hembras, 
#        permutations = 999, method="euclidean", by = NULL)

# calculando beta dispersión (homogeneidad de varianzas)
dispersion5 <- betadisper(dune.dist5, group=amino$condición)
permutest(dispersion5)
plot(dispersion5, hull=FALSE, ellipse=T) ##sd ellipse
# calculando dispersión por especie
dispersion6 <- betadisper(dune.dist5, group=amino$spp)
permutest(dispersion6)
plot(dispersion6, hull=FALSE, ellipse=T) ##sd ellipse
# calculando dispersión por sexo
dispersion6 <- betadisper(dune.dist5, group=amino$sexo_flor)
permutest(dispersion6)
plot(dispersion6, hull=FALSE, ellipse=T) ##sd ellipse


##### PCA DE RASGOS DEL POLEN ####
setwd("/home/luis/Documents/3_Doctorado_UNAM/a-proyecto/analisis_florales_tesis_2022/rasgos_polen")
# cargando base depurada de todos los rasgos de polen
pollen = read.csv("rasgos_pollen2.csv", header = T)
head(pollen)
str(pollen)

tapply(pollen$produ, pollen$spp, mean)
tapply(pollen$tam, pollen$spp, mean)
tapply(pollen$conc.prot, pollen$spp, mean)
tapply(pollen$conc_lip, pollen$spp, mean)
tapply(pollen$pl, pollen$spp, mean)

# PCA de caracteres del nectar
head(pollen)
str(pollen)
pairs(pollen[6:10])
pca6 <- prcomp(pollen[, 6:10], scale = T)
summary(pca6)
pca6$rotation[, 1:2]
plot(pca6)
#write.csv(pca1$rotation[, 1:2], file = "pca_hembras.csv")
biplot(pca6, scale=0)

# Obteniendo información sobre los eigenvalues
get_eigenvalue(pca6)
# calculando la varianza explicada por cada componente
fviz_eig(pca6, addlabels = T)
# información que podemos obtener de las variables analizadas
var = get_pca_var(pca6); var
# Visualización de las variables
fviz_pca_var(pca6, col.var = "black")

# correlaciones entre calidades de variables
corrplot(var$cos2, is.corr = F)
# Calidad de cada variable 
fviz_cos2(pca6, choice = "var", axes = 1:2)
# graficando las variables en el mapa de factores
fviz_pca_var(pca6, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
# Contribución a los componentes
head(var$contrib)
corrplot(var$contrib, is.corr=FALSE)    

# Contributions of variables to PC1
fviz_contrib(pca6, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(pca6, choice = "var", axes = 2, top = 10)
# contribución combinada de los componentes 1  y 2
fviz_contrib(pca6, choice = "var", axes = 1:2, top = 10)
# visualizando las variables coloreadas por contribución
fviz_pca_var(pca6, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

### Graph of individuals (rows)
ind = get_pca_ind(pca6)
ind

fviz_pca_ind(pca6)
# Visualizando los individuos por cos2
fviz_pca_ind(pca6, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
#
fviz_pca_ind(pca6, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# Total contribution on PC1 and PC2
fviz_contrib(pca6, choice = "ind", axes = 1:2)

# Agrupado por condición (SIN elipse)
fviz_pca_ind(pca6,
             geom.ind = "point", # show points only (nbut not "text")
             fill.ind = pollen$condicion, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             #addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "euclid",
             legend.title = "Condition",
             title = "",
             pointshape=21,
             pointsize = 2
)+
 labs(x = "PC 1 (36.1%)", y = "PC 2 (31.4%)")

# Agrupado por condición (CON elipse)
fviz_pca_ind(pca6,
             geom.ind = "point", # show points only (nbut not "text")
             fill.ind = pollen$condicion, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "euclid",
             legend.title = "Condition",
             title = "",
             pointshape=21,
             pointsize = 2,
             label = "none"
)+
  labs(x = "PC 1 (36.1%)", y = "PC 2 (31.4%)")

# Agrupado por especie
fviz_pca_ind(pca6,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = pollen$spp, # color by groups
             palette = c("#00AFBB", "#E7B800","#FC4E07", "#00AFDB",
                         "#E7B850", "#FF8400"),
             addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "confidence",
             legend.title = "Groups"
)

### agrupado por condición + las variables
fviz_pca_biplot(pca6,
                col.ind = pollen$condicion, palette = "jco",
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Condition"
)

# PERMANOVA del polen
str(pollen)
pollen[6:10]
dune10 = pollen[6:10]
# Calculando distancia euclidiana
set.seed(0)
# permanova
dune.div10 <- adonis2(dune10 ~ condicion, data = pollen,
                     permutations = 999, method="euclidean")
dune.div10

# calculando PERMANOVA por especie
dune.div11 <- adonis2(dune10 ~ spp, data = pollen,
                     permutations = 999, method="euclidean")
dune.div11

# overall tests
#adonis2(dune ~ especie, data = hembras, 
#        permutations = 999, method="euclidean", by = NULL)

# calculando beta dispersión (homogeneidad de varianzas)
dispersion5 <- betadisper(dune.dist5, group=amino$condición)
permutest(dispersion5)
plot(dispersion5, hull=FALSE, ellipse=T) ##sd ellipse
# calculando dispersión por especie
dispersion6 <- betadisper(dune.dist5, group=amino$spp)
permutest(dispersion6)
plot(dispersion6, hull=FALSE, ellipse=T) ##sd ellipse
# calculando dispersión por sexo
dispersion6 <- betadisper(dune.dist5, group=amino$sexo_flor)
permutest(dispersion6)
plot(dispersion6, hull=FALSE, ellipse=T) ##sd ellipse

# Cargando paquete para normalidad de multivarianza
library(mvnormtest)
data("EuStockMarkets")
View(EuStockMarkets)
C <- t(EuStockMarkets[15:29,1:4])
mshapiro.test(C)
# Transponiendo tabla
C = t(pollen[6:10])
# Calculando normalidad de multivarianza
mshapiro.test(C) # no es normal

# Calculando normalidad de cada grupo por variable dependiente
normal<-function(vec){
  shapiro.test(vec)$p
}
aggregate(pollen[,6:10],list(pollen$condicion), normal)
# Calculando normalidad multivariada
library(MVN)
mvn(pollen[, c(6, 7, 8, 9, 10)], mvnTest = "hz", univariateTest = "SW", 
    univariatePlot = "histogram")


# MANOVA test
mod1<-manova(cbind(produ, tam, conc.prot, conc_lip, pl) ~ condicion, 
             data = pollen)
summary(mod1) #Diferencias significativas




#### CALCULANDO SEÑAL FILOGENÉTICA ####
# Cargando base de datos de todos los caracteres de flores hembra (promedios)
traits_hem = read.csv("hembras_mean.csv", header = T, row.names = 1) #, 
head(traits_hem)
str(traits_hem)
traits_hem[2:35]

#### Calculando la señal filogenetica con la función multiPhylosignal del
# paquete "picante"
library(picante)
head(traits_hem)
arbol <- read.nexus("tree7.nex")
class(arbol)
arbol
names(arbol)
arbol$tip.label

# calculando la K de Blomberg. Este valor mide la señal filogenetica o la 
# tendencia de especies relacioanadas a parecerse unas con otras
traits_h <- (traits_hem[2:35])
str(traits_h)
multiPhylosignal(traits_h, arbol)
#multiPhylosignal(traits_h, multi2di(arbol))
# Guardando tabla
write.csv(multiPhylosignal(traits_h, arbol),
          file = "K_blombergs.csv")

#### Calculando señal filogenética con la lambda de Pagel con el ####
# paquete "phytools"
# Load necessary libraries
library(phytools)
library(dplyr)
library(future.apply)  # For cross-platform parallelization

# cargando la base de datos que contiene los promedios de todos los caracteres
# medidos en flores hembra
traits_hem = read.csv("hembras_mean.csv", header = T)
str(traits_hem)
traits_h <- (traits_hem[3:36])
str(traits_h)

# Function to calculate Pagel's lambda and p-value for each trait
calculate_lambda <- function(trait_column, tree) {
  # Combine species and trait column into a temporary data frame
  temp_data <- data.frame(species = traits_hem$especie, trait = trait_column)
  
  # Calculate the mean trait value per species
  #species_means <- temp_data %>%
   # group_by(species) %>%
    #summarise(mean_trait = mean(trait, na.rm = TRUE))
  
  # Create a named vector of trait values for the species
  trait_vector <- temp_data$trait
  names(trait_vector) <- temp_data$species
  
  # Use the phylosig function to calculate Pagel's lambda
  lambda_result <- phylosig(tree, trait_vector, method = "lambda", test = TRUE)
  
  # Return a named vector with lambda and p-value
  return(c(lambda = lambda_result$lambda, p_value = lambda_result$P))  # Corrected p-value reference
}

# Set up parallelization using future.apply (works across platforms)
#plan(multisession)  # Enables parallel processing

# Apply this function to each trait in columns 3 to 12 using future_sapply for parallel execution
trait_results <- future_sapply(traits_hem[, 3:36], calculate_lambda, tree = arbol)


# Convert the result to a data frame
trait_results_df <- as.data.frame(t(trait_results))  # Transpose to get traits in rows
trait_results_df$trait <- colnames(traits_hem)[3:36]  # Add trait names as a column

# View the result
print(trait_results_df)

# Optionally, save the results to a file
write.csv(trait_results_df, "trait_pagels_lambda_results_hembras.csv",
          row.names = FALSE)

# cargando la base de datos que contiene los promedios de todos los caracteres
# medidos en flores macho
traits_mac = read.csv("machos_mean.csv", header = T)
str(traits_mac)

traits_m <- (traits_mac[3:39])
str(traits_m)

# Function to calculate Pagel's lambda and p-value for each trait
calculate_lambda <- function(trait_column, tree) {
  # Combine species and trait column into a temporary data frame
  temp_data <- data.frame(species = traits_mac$especie, trait = trait_column)
  
  # Calculate the mean trait value per species
  #species_means <- temp_data %>%
  # group_by(species) %>%
  #summarise(mean_trait = mean(trait, na.rm = TRUE))
  
  # Create a named vector of trait values for the species
  trait_vector <- temp_data$trait
  names(trait_vector) <- temp_data$species
  
  # Use the phylosig function to calculate Pagel's lambda
  lambda_result <- phylosig(tree, trait_vector, method = "lambda", test = TRUE)
  
  # Return a named vector with lambda and p-value
  return(c(lambda = lambda_result$lambda, p_value = lambda_result$P))  # Corrected p-value reference
}

# Set up parallelization using future.apply (works across platforms)
#plan(multisession)  # Enables parallel processing

# Apply this function to each trait in columns 3 to 12 using future_sapply for parallel execution
trait_results <- future_sapply(traits_mac[, 3:39], calculate_lambda, tree = arbol)


# Convert the result to a data frame
trait_results_df <- as.data.frame(t(trait_results))  # Transpose to get traits in rows
trait_results_df$trait <- colnames(traits_mac)[3:39]  # Add trait names as a column

# View the result
print(trait_results_df)

# Optionally, save the results to a file
write.csv(trait_results_df, "trait_pagels_lambda_results_machos.csv",
          row.names = FALSE)






#### Calculando señal filogenética con la K de Blomberg con el paquete ####
# "phytools"
# cargando la base de datos que contiene los promedios de todos los caracteres
# medidos en flores hembra
traits_hem = read.csv("hembras_mean.csv", header = T)
str(traits_hem)
traits_h <- (traits_hem[3:36])
str(traits_h)

# Function to calculate Blomberg's K and p-value for each trait
calculate_ka <- function(trait_column, tree) {
  # Combine species and trait column into a temporary data frame
  temp_data <- data.frame(species = traits_hem$especie, trait = trait_column)
  
  # Calculate the mean trait value per species
  #species_means <- temp_data %>%
  # group_by(species) %>%
  #summarise(mean_trait = mean(trait, na.rm = TRUE))
  
  # Create a named vector of trait values for the species
  trait_vector <- temp_data$trait
  names(trait_vector) <- temp_data$species
  
  # Use the phylosig function to calculate Pagel's lambda
  ka_result <- phylosig(tree, trait_vector, method = "K", test = TRUE)
  
  # Return a named vector with lambda and p-value
  return(c(ka = ka_result$K, p_value = ka_result$P))  # Corrected p-value reference
}

# Set up parallelization using future.apply (works across platforms)
#plan(multisession)  # Enables parallel processing

# Apply this function to each trait in columns 3 to 12 using future_sapply for parallel execution
trait_results_ka <- future_sapply(traits_hem[, 3:36], calculate_ka, tree = arbol)


# Convert the result to a data frame
trait_results_ka_df <- as.data.frame(t(trait_results))  # Transpose to get traits in rows
trait_results_ka_df$trait <- colnames(traits_hem)[3:36]  # Add trait names as a column

# View the result
print(trait_results_ka_df)

# K de Blomberg
write.csv(trait_results_ka_df, "trait_blomberg_K_results_hembras.csv",
          row.names = FALSE)


# cargando la base de datos que contiene los promedios de todos los caracteres
# medidos en flores MACHO para calcular la K de Blomberg
traits_mac = read.csv("machos_mean.csv", header = T)
str(traits_mac)

traits_m <- (traits_mac[3:39])
str(traits_m)

# Function to calculate Blomberg's K and p-value for each trait
calculate_ka <- function(trait_column, tree) {
  # Combine species and trait column into a temporary data frame
  temp_data <- data.frame(species = traits_mac$especie, trait = trait_column)
  
  # Calculate the mean trait value per species
  #species_means <- temp_data %>%
  # group_by(species) %>%
  #summarise(mean_trait = mean(trait, na.rm = TRUE))
  
  # Create a named vector of trait values for the species
  trait_vector <- temp_data$trait
  names(trait_vector) <- temp_data$species
  
  # Use the phylosig function to calculate Pagel's lambda
  ka_result <- phylosig(tree, trait_vector, method = "K", test = TRUE)
  
  # Return a named vector with lambda and p-value
  return(c(ka = ka_result$K, p_value = ka_result$P))  # Corrected p-value reference
}

# Set up parallelization using future.apply (works across platforms)
#plan(multisession)  # Enables parallel processing

# Apply this function to each trait in columns 3 to 12 using future_sapply for parallel execution
trait_results_ka <- future_sapply(traits_mac[, 3:39], calculate_ka, tree = arbol)


# Convert the result to a data frame
trait_results_ka_df <- as.data.frame(t(trait_results_ka))  # Transpose to get traits in rows
trait_results_ka_df$trait <- colnames(traits_mac)[3:39]  # Add trait names as a column

# View the result
print(trait_results_ka_df)

# K de Blomberg
write.csv(trait_results_ka_df, "trait_blomberg_K_results_machos.csv",
          row.names = FALSE)




#### Calculando señal filogenetica por separado para cada rasgo ####
# cargando base de datos
traits_hem = read.csv("hembras_mean.csv", header = T, row.names = 1) #, 
head(traits_hem)
str(traits_hem)
arbol$tip.label # cas, cm, caa, cpf, cpp, cf
# Poniendo el nombre de cada especie
homeRange <- setNames(traits_hem$TL,
                    rownames(traits_hem))
# Calculando señal filogenétiNDf# Calculando señal filogenética
#hrangeLambda = 
kresult = phylosig(arbol, homeRange, method = "K", test = TRUE)
kresult$K
kresult$P

hrangeLambda

# Renombrando 
arbol$tip.label

relabel.tips <- c("Cucurbita argyrosperma subsp. sororia",
                  "Cucurbita moschata*",
                  "Cucurbita argyrosperma subsp. argyrosperma*",
                  "Cucurbita pepo subsp. fraterna",
                  "Cucurbita pepo subsp. pepo*",
                  "Cucurbita foetidissima")
# renombrar terminales
#arbol$tip.label <- mixedFontLabel(relabel.tips, italic = 1)
arbol$tip.label <- relabel.tips
plotTree(arbol)

library(ape)
genus <- c("Cucurbita", "Cucurbita", "Cucurbita", "Cucurbita", "Cucurbita",
           "Cucurbita")
species <- c("argyrosperma subsp. sororia", "moschata*", 
             "argyrosperma subsp. argyrosperma*", "pepo subsp. fraterna",
             "pepo subsp. pepo*", "foetidissima")

arbol$tip.label <- mixedFontLabel(genus, species, italic = 1:2)
plot(arbol)



layout(matrix(c(1, 2), 2))
plot(tr)


plot(c(0, 2), c(0, 2))
text(1, 1, bquote(
  paste(
    italic("Italic Text:"),
    " Some words with new lines. ",
    italic("More italic text:"),
    "Yet more words divided by new lines. ",
    italic("Italics again:"),
    "And more text with new lines.",
    sep = ""
  )
)
)


##### PCA de aminoacidos de flores hembra #####
amino = read.csv("concentraciones_aminoacidos.csv", header = TRUE)
str(amino)
head(amino)
tapply(amino$Asp, list(amino$condición,amino$sexo_flor), length)
tapply(amino$Asp, list(amino$condición, amino$sexo_flor), mean)
# concentración de aminoacidos en flores hembra
amino_hem = subset(amino, sexo_flor == "H")
str(amino_hem)
# analisis multivariado de normalidad
mvn(amino_hem[8:24], mvnTest = "hz", univariateTest = "SW", 
    univariatePlot = "histogram")

# concentración de aminoacidos en flores hembra
amino_mac = subset(amino, sexo_flor == "M")
str(amino_mac)
# analisis multivariado de normalidad
mvn(amino_mac[8:24], mvnTest = "hz", univariateTest = "SW", 
    univariatePlot = "histogram")

# PCA de caracteres del néctar de flores hembra
# visualizando todas las combinaciones entre las variables
pairs(amino_hem[8:24], lower.panel = NULL)
# Calculando el PCA para flores hembra
pca4 = prcomp(amino_hem[8:24], scale = T)
summary(pca4)
# Obteniendo información sobre los eigenvalues
get_eigenvalue(pca4)
# calculando la varianza explicada por cada componente
fviz_eig(pca4, addlabels = T)
# información que podemos obtener de las variables analizadas
var = get_pca_var(pca4); var
# Visualización de las variables
fviz_pca_var(pca4, col.var = "black")

# correlaciones entre calidades de variables
corrplot(var$cos2, is.corr = F)
# Calidad de cada variable 
fviz_cos2(pca4, choice = "var", axes = 1:2)
# graficando las variables en el mapa de factores
fviz_pca_var(pca4, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
# Contribución a los componentes
head(var$contrib)
corrplot(var$contrib, is.corr=FALSE)    

# Contributions of variables to PC1
fviz_contrib(pca4, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(pca4, choice = "var", axes = 2, top = 10)
# contribución combinada de los componentes 1  y 2
fviz_contrib(pca4, choice = "var", axes = 1:2, top = 10)
# visualizando las variables coloreadas por contribución
fviz_pca_var(pca4, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

### Graph of individuals (rows)
ind = get_pca_ind(pca4)
ind

fviz_pca_ind(pca4)
# Visualizando los individuos por cos2
fviz_pca_ind(pca4, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
#
fviz_pca_ind(pca4, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# Total contribution on PC1 and PC2
fviz_contrib(pca4, choice = "ind", axes = 1:2)

# Agrupado por condición (SIN elipse)
fviz_pca_ind(pca4,
             geom.ind = "point", # show points only (nbut not "text")
             fill.ind = amino_hem$condición, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             #addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "euclid",
             legend.title = "Condition",
             title = "",
             pointshape=21,
             pointsize = 2
)+
 labs(x = "PC 1 (63.5%)", y = "PC 2 (11.8%)")

# Agrupado por condición (CON elipse)
fviz_pca_ind(pca4,
             geom.ind = "point", # show points only (nbut not "text")
             fill.ind = amino_hem$condición, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "euclid",
             legend.title = "Condition",
             title = "",
             pointshape=21,
             pointsize = 2,
             label = "none"
)+
  labs(x = "PC 1 (63.5%)", y = "PC 2 (11.8%)")


# Agrupado por especie
#fviz_pca_ind(pca4,
#             geom.ind = "point", # show points only (nbut not "text")
#             col.ind = nectar_h$spp, # color by groups
#             palette = c("#00AFBB", "#E7B800","#FC4E07", "#00AFDB",
#                         "#E7B850"),
#             addEllipses = TRUE, # Concentration ellipses
#             #ellipse.type = "confidence",
#             legend.title = "Groups"
#)

### agrupado por condición + las variables
#fviz_pca_biplot(pca4,
#                col.ind = nectar_h$condicion, palette = "jco",
#                addEllipses = TRUE, label = "var",
#                col.var = "black", repel = TRUE,
#                legend.title = "Condition"
#)

library(MVN)
# Flores hembra
mvn(amino_hem[8:24], mvnTest = "hz", univariateTest = "SW", 
    univariatePlot = "histogram")



# Flores macho
mvn(amino_mac[8:24], mvnTest = "hz", univariateTest = "SW", 
    univariatePlot = "histogram")


# PERMANOVA de aminoacidos de flores hembra
str(amino_hem)
amino_hem[8:24]
dune8 = amino_hem[8:24]
# Calculando distancia euclidiana
set.seed(0)
# permanova
dune.div8 <- adonis2(dune8 ~ condición, data = amino_hem,
                     permutations = 999, method="euclidean")
dune.div8

# calculando PERMANOVA por especie
dune.div8 <- adonis2(dune8 ~ spp, data = nectar_h,
                     permutations = 999, method="euclidean")
dune.div8

# overall tests
#adonis2(dune ~ especie, data = hembras, 
#        permutations = 999, method="euclidean", by = NULL)

# calculando beta dispersión (homogeneidad de varianzas)
dispersion5 <- betadisper(dune.dist5, group=amino$condición)
permutest(dispersion5)
plot(dispersion5, hull=FALSE, ellipse=T) ##sd ellipse
# calculando dispersión por especie
dispersion6 <- betadisper(dune.dist5, group=amino$spp)
permutest(dispersion6)
plot(dispersion6, hull=FALSE, ellipse=T) ##sd ellipse
# calculando dispersión por sexo
dispersion6 <- betadisper(dune.dist5, group=amino$sexo_flor)
permutest(dispersion6)
plot(dispersion6, hull=FALSE, ellipse=T) ##sd ellipse

#### PCA DE AMINOACIDOS DE FLORES MACHO ####
# concentración de aminoacidos en flores MACHO
amino_mac = subset(amino, sexo_flor == "M")
str(amino_mac)
# analisis multivariado de normalidad
mvn(amino_mac[8:24], mvnTest = "hz", univariateTest = "SW", 
    univariatePlot = "histogram")

# PCA de caracteres del néctar de flores hembra
# visualizando todas las combinaciones entre las variables
pairs(amino_mac[8:24], lower.panel = NULL)
# Calculando el PCA para flores hembra
pca5 = prcomp(amino_mac[8:24], scale = T)
summary(pca5)
# Obteniendo información sobre los eigenvalues
get_eigenvalue(pca5)
# calculando la varianza explicada por cada componente
fviz_eig(pca5, addlabels = T)
# información que podemos obtener de las variables analizadas
var = get_pca_var(pca5); var
# Visualización de las variables
fviz_pca_var(pca5, col.var = "black")

# correlaciones entre calidades de variables
corrplot(var$cos2, is.corr = F)
# Calidad de cada variable 
fviz_cos2(pca5, choice = "var", axes = 1:2)
# graficando las variables en el mapa de factores
fviz_pca_var(pca5, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
# Contribución a los componentes
head(var$contrib)
corrplot(var$contrib, is.corr=FALSE)    

# Contributions of variables to PC1
fviz_contrib(pca5, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(pca5, choice = "var", axes = 2, top = 10)
# contribución combinada de los componentes 1  y 2
fviz_contrib(pca5, choice = "var", axes = 1:2, top = 10)
# visualizando las variables coloreadas por contribución
fviz_pca_var(pca5, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)


### Graph of individuals (rows)
ind = get_pca_ind(pca5)
ind

fviz_pca_ind(pca5)
# Visualizando los individuos por cos2
fviz_pca_ind(pca5, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
#
fviz_pca_ind(pca5, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# Total contribution on PC1 and PC2
fviz_contrib(pca5, choice = "ind", axes = 1:2)

# Agrupado por condición (SIN elipse)
fviz_pca_ind(pca5,
             geom.ind = "point", # show points only (nbut not "text")
             fill.ind = amino_mac$condición, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             #addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "euclid",
             legend.title = "Condition",
             title = "",
             pointshape=21,
             pointsize = 2
)+
 labs(x = "PC 1 (61.7%)", y = "PC 2 (12.3%)")

# Agrupado por condición (CON elipse)
fviz_pca_ind(pca5,
             geom.ind = "point", # show points only (nbut not "text")
             fill.ind = amino_mac$condición, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "euclid",
             legend.title = "Condition",
             title = "",
             pointshape=21,
             pointsize = 2,
             label = "none"
)+
  labs(x = "PC 1 (61.7%)", y = "PC 2 (12.3%)")


# Agrupado por especie
#fviz_pca_ind(pca4,
#             geom.ind = "point", # show points only (nbut not "text")
#             col.ind = nectar_h$spp, # color by groups
#             palette = c("#00AFBB", "#E7B800","#FC4E07", "#00AFDB",
#                         "#E7B850"),
#             addEllipses = TRUE, # Concentration ellipses
#             #ellipse.type = "confidence",
#             legend.title = "Groups"
#)

### agrupado por condición + las variables
#fviz_pca_biplot(pca4,
#                col.ind = nectar_h$condicion, palette = "jco",
#                addEllipses = TRUE, label = "var",
#                col.var = "black", repel = TRUE,
#                legend.title = "Condition"
#)

library(MVN)
# Flores macho
mvn(amino_mac[8:24], mvnTest = "hz", univariateTest = "SW", 
    univariatePlot = "histogram")


# PERMANOVA de aminoacidos de flores hembra
str(amino_mac)
amino_mac[8:24]
dune9 = amino_mac[8:24]
# Calculando distancia euclidiana
set.seed(0)
# permanova
dune.div9 <- adonis2(dune9 ~ condición, data = amino_mac,
                     permutations = 999, method="euclidean")
dune.div9

# calculando PERMANOVA por especie
dune.div8 <- adonis2(dune8 ~ spp, data = nectar_h,
                     permutations = 999, method="euclidean")
dune.div8

# overall tests
#adonis2(dune ~ especie, data = hembras, 
#        permutations = 999, method="euclidean", by = NULL)

# calculando beta dispersión (homogeneidad de varianzas)
dispersion5 <- betadisper(dune.dist5, group=amino$condición)
permutest(dispersion5)
plot(dispersion5, hull=FALSE, ellipse=T) ##sd ellipse
# calculando dispersión por especie
dispersion6 <- betadisper(dune.dist5, group=amino$spp)
permutest(dispersion6)
plot(dispersion6, hull=FALSE, ellipse=T) ##sd ellipse
# calculando dispersión por sexo
dispersion6 <- betadisper(dune.dist5, group=amino$sexo_flor)
permutest(dispersion6)
plot(dispersion6, hull=FALSE, ellipse=T) ##sd ellipse





