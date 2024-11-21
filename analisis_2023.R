# En este documento se muestran los análisis de los caracteres florales de las 
# diferentes especies de Cucurbita utilizadas en este estudio. También se evalua 
# la concentración de azucares y aminoácidos, además del tamaño, producción y 
# proporción de proteinas y lipidos del polen.

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
setwd("/home/luis/Documents/3_Doctorado_UNAM/a-proyecto/analisis_florales_tesis_2022")

#### Cargando base de datos de caracteres florales de flores hembra ####
hembras <- read.csv("flores_hembra.csv", header = T)
head(hembras)
str(hembras)
tapply(hembras$CD, list(hembras$especie, hembras$sitio), length)
tapply(hembras$CD, hembras$sitio, length)
tapply(hembras$CD, list(hembras$especie, hembras$tratamiento), length)
#View(hembras)

### Subset de las flores de Morelia
hembras = subset(hembras, sitio == "morelia")
head(hembras)
str(hembras)
tapply(hembras$CD, hembras$especie, length)

# Agrupando por individuo
hem_resu <- hembras %>%
  group_by(año,condición,especie,id_planta2) %>%
  dplyr::summarize(CD = mean(CD), TL = mean(TL), CL = mean(CL), TD1 = mean(TD1),
                   TD2 = mean(TD2), TD3 = mean(TD3), NDf = mean(NDf), SD = mean(SD),
                   PL = mean(PL), SL = mean(SL), OL = mean(OL), OD = mean(OD), 
                   VN = mean(vol_nec), n = n())
hem_resu
hem_resu2 <- as.data.frame(hem_resu)
str(hem_resu2)
#(hem_resu2, file="hem_resu.csv")

hem_resu3 <- hem_resu %>%
  group_by(especie) %>%
  dplyr::summarize(CD = mean(CD), TL = mean(TL), CL = mean(CL), TD1 = mean(TD1),
                   TD2 = mean(TD2), TD3 = mean(TD3), NDf = mean(NDf), SD = mean(SD),
                   PL = mean(PL), SL = mean(SL), OL = mean(OL), OD = mean(OD), 
                   VN = mean(VN), n = n())
  
#hem_resu3
#View(hem_resu3)
#### Estadistica descriptiva de los caracteres de flores hembra ####
# Media
media = hembras %>%
  group_by(especie) %>%
  dplyr::summarize(CD = mean(CD), TL = mean(TL), CL = mean(CL), TD1 = mean(TD1),
            TD2 = mean(TD2), TD3 = mean(TD3), NDf = mean(NDf), SD = mean(SD),
            PL = mean(PL), SL = mean(SL), OL = mean(OL), OD = mean(OD), 
            VN = mean(vol_nec),n = n())
View(media)
# Error estandar
erro = hem_resu %>%
  group_by(especie) %>%
  dplyr::summarize(CD = es(CD), TL = es(TL), CL = es(CL), TD1 = es(TD1), 
            TD2 = es(TD2), TD3 = es(TD3), NDf = es(NDf), SD = es(SD), 
            PL = es(PL), SL = es(SL), OL = es(OL), OD = es(OD), 
            VN = es(VN), n = n())
View(erro)
tapply(hem_resu$CD, hem_resu$especie, length)
# Coeficiente de variación
CV = hem_resu2 %>%
  group_by(especie) %>%
  dplyr::summarize(CD = cv(CD), TL = cv(TL), CL = cv(CL), TD1 = cv(TD1),
            TD2 = cv(TD2), TD3 = cv(TD3), NDf = cv(NDf), SD = cv(SD), 
            PL = cv(PL), SL = cv(SL), OL = cv(OL), OD = cv(OD),
            VN = cv(VN), n = n())
CV
View(CV)
#### Boxplot de los caracteres por condición en flores hembra ####
boxplot(hembras$CD ~ hembras$especie)
boxplot(hembras$TL ~ hembras$especie)
boxplot(hembras$CL ~ hembras$especie)
boxplot(hembras$TD1 ~ hembras$especie)
boxplot(hembras$TD2 ~ hembras$especie)
boxplot(hembras$TD3 ~ hembras$especie)
boxplot(hembras$NDf ~ hembras$especie)
boxplot(hembras$PL ~ hembras$especie)
boxplot(hembras$SL ~ hembras$especie)
boxplot(hembras$SD ~ hembras$especie)
boxplot(hembras$OD ~ hembras$especie)
boxplot(hembras$OL ~ hembras$especie)
boxplot(hembras$vol_nec ~ hembras$especie)

vol_nec2 = subset(hembras, vol_nec != 0)
vol_nec3 = subset(vol_nec2, vol_nec < 300)
sort(vol_nec3$vol_nec)
boxplot(vol_nec3$vol_nec ~ vol_nec3$especie)
shapiro.test(hembras$vol_nec)
shapiro.test(vol_nec3$vol_nec)
shapiro.test(log(vol_nec3$vol_nec))

tapply(vol_nec3$vol_nec, vol_nec3$especie, mean)
tapply(vol_nec3$vol_nec, vol_nec3$especie, es)

#### Principal component analysis (PCA) de los caracteres florales ####
head(hembras)
str(hembras)
pairs(hembras[11:23])
pca1 <- prcomp(hembras[, 11:23], scale = T)
summary(pca1)
pca1$rotation[, 1:2]
plot(pca1)
#write.csv(pca1$rotation[, 1:2], file = "pca_hembras.csv")
biplot(pca1, scale=0)

#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
ca.species = hembras[, 6]
ca.condicion = hembras[,4]
# gráfica de PCA con ggbiplot por condición
ggbiplot(pca1, obs.scale = 1, var.scale = 1, 
              groups = ca.condicion, ellipse = TRUE, 
              circle = F)+
  scale_color_discrete(name = '')+
  theme(legend.direction = 'horizontal', 
               legend.position = 'bottom')

# gráfica de PCA con ggbiplot por especie
ggbiplot(pca1, obs.scale = 1, var.scale = 1, 
              groups = ca.species, ellipse = TRUE, 
              circle = F) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', 
               legend.position = 'bottom')

#### Principal coordinate analyisis (PCoA) de flores hembra ####
# PCoA con vegan
library(vegan)
cucu_dist = dist(hem_resu2[,c(5:16)])
cucu_PCoA <- wcmdscale(d = cucu_dist, eig = TRUE)
cucu_PCoA
cucu_PCoA$points
# gráfica del PCoA
ggplot(data = data.frame(cucu_PCoA$points),
       aes(x = Dim1, y = Dim2)) +
  geom_point() +
  theme_bw()
#
pcoa <- cmdscale(cucu_dist, eig = TRUE, add = TRUE)
#convert pcoa results into data frame that can be plotted
pcoa_df <- data.frame(pcoa$points)
colnames(pcoa_df) <- c("PCo1", "PCo2")
pcoa_df$Species <- factor(hem_resu2$especie) #add group of interest,
# mine was Morphospecies in the data frame cal_fem_data2
calf <- ggplot(pcoa_df, aes(x = PCo1, y = PCo2, color = Species)) + 
  geom_point(size = 2) +
  xlab("PCo1") +
  ylab("PCo2") + 
  ggtitle("Flores hembra por especie") +
  theme_classic()
calf
#
pcoa1 <- cmdscale(cucu_dist, eig = TRUE, add = TRUE)
#convert pcoa results into data frame that can be plotted
pcoa1_df <- data.frame(pcoa1$points)
colnames(pcoa1_df) <- c("PCo1", "PCo2")
pcoa_df$condicion <- factor(hem_resu2$condición) #add group of interest,
# mine was Morphospecies in the data frame cal_fem_data2
calf <- ggplot(pcoa_df, aes(x = PCo1, y = PCo2, color = condicion)) + 
  geom_point(size = 2) +
  xlab("PCo1") +
  ylab("PCo2") + 
  ggtitle("Flores hembra por condición") +
  theme_classic()+
  scale_color_discrete(labels = c("Domesticadas", "Silvestres"))
calf

### PERMANOVA DE LOS DIFERENTES AMINOACIDOS
library(vegan)
head(hembras)
str(hembras)
dune5 = hembras[12:24]
# Calculando distancia euclidiana
dune.dist5 <- vegdist(dune5, method="euclidean")

# calculando PERMANOVA por condición
dune.div5 <- adonis2(dune5 ~ condición, data = hembras,
                     permutations = 999, method="euclidean")
dune.div5

# calculando PERMANOVA por especie
dune.div6 <- adonis2(dune5 ~ condición+especie, data = hembras,
                     permutations = 999, method="euclidean")
dune.div6

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




## base con los caracteres morfologicos, azucares y aminoacidos de flores hembra
hem_resu3 <- read.csv("hem_resu.csv", header = T)
head(hem_resu3)
str(hem_resu3)
pairs(hem_resu3[6:39])
pca1 <- prcomp(hem_resu3[, 6:39], scale = T)
summary(pca1)
pca1$rotation[, 1:2]
plot(pca1)
#write.csv(pca1$rotation[, 1:2], file = "pca_hembras.csv")
biplot(pca1, scale=0)
#
library(vegan)
cucu_dist = dist(hem_resu3[,c(6:39)])
cucu_PCoA <- wcmdscale(d = cucu_dist, eig = TRUE)
cucu_PCoA
cucu_PCoA$points
# gráfica del PCoA
ggplot(data = data.frame(cucu_PCoA$points),
       aes(x = Dim1, y = Dim2)) +
  geom_point() +
  theme_bw()

#
pcoa <- cmdscale(cucu_dist, eig = TRUE, add = TRUE)
#convert pcoa results into data frame that can be plotted
pcoa_df <- data.frame(pcoa$points)
colnames(pcoa_df) <- c("PCo1", "PCo2")
pcoa_df$condición <- factor(hem_resu3$condición) #add group of interest,
# mine was Morphospecies in the data frame cal_fem_data2
calf <- ggplot(pcoa_df, aes(x = PCo1, y = PCo2, color = condición)) + 
  geom_point(size = 2) +
  xlab("PCo1") +
  ylab("PCo2") + 
  ggtitle("condición") +
  theme_classic()
calf

pcoa <- cmdscale(cucu_dist, eig = TRUE, add = TRUE)
#convert pcoa results into data frame that can be plotted
pcoa_df <- data.frame(pcoa$points)
colnames(pcoa_df) <- c("PCo1", "PCo2")
pcoa_df$Species <- factor(hem_resu3$especie) #add group of interest,
# mine was Morphospecies in the data frame cal_fem_data2
calf <- ggplot(pcoa_df, aes(x = PCo1, y = PCo2, color = Species)) + 
  geom_point(size = 2) +
  xlab("PCo1") +
  ylab("PCo2") + 
  ggtitle("especies") +
  theme_classic()
calf


#### PERMANOVA de flores hembra ####
head(hem_resu3)
dune = hem_resu3[6:39]
# Calculando distancia euclidiana
dune.dist <- vegdist(dune, method="euclidean", na.rm = T)

# calculando PERMANOVA por especie
dune.div <- adonis2(dune ~ especie, data = hem_resu3,
                    permutations = 999, method="euclidean", na.rm = T)
dune.div
# calculando PERMANOVA por condición
dune.div2 <- adonis2(dune ~ condición, data = hem_resu3,
                     permutations = 999, method="euclidean", na.rm = T)
dune.div2
# overall tests
#adonis2(dune ~ especie, data = hembras, 
#        permutations = 999, method="euclidean", by = NULL)

# calculando beta dispersión (homogeneidad de varianzas)
dispersion <- betadisper(dune.dist, group=hembras$condición)
permutest(dispersion)
plot(dispersion, hull=FALSE, ellipse=T) ##sd ellipse

dispersion <- betadisper(dune.dist, group=hembras$especie)
permutest(dispersion)
plot(dispersion, hull=FALSE, ellipse=T) ##sd ellipse


#### GLMMs de flores hembra #### 
# En esta sección se realizaron los modelos lineales generalizados mixtos
# de los diferentes caracteres florales, comparando entre las especies
# tomando el id de la planta como un factor aleatorio:
head(hembras)
## GLMs comparando el CD entre años y entre las diferentes especies ##
shapiro.test(hem_resu2$CD) # gaussian
shapiro.test(hem_resu2$TL) # Gamma
shapiro.test(hem_resu2$CL) # Gamma
shapiro.test(hem_resu2$TD1) # Gamma
shapiro.test(hem_resu2$TD2) # Gamma
shapiro.test(hem_resu2$TD3) # gaussian
shapiro.test(hem_resu2$NDf) # Gamma
#shapiro.test(hem_resu2$VN)
shapiro.test(hem_resu2$PL) # Gamma
shapiro.test(hem_resu2$SL) # Gamma
shapiro.test(hem_resu2$SD) # Gamma
shapiro.test(hem_resu2$OD) # Gamma
shapiro.test(hem_resu2$OL) # Gamma


# CD
model1 <- glm(CD ~ condición, data = hem_resu2, family = gaussian) 
summary(model1)
Anova(model1)
shapiro.test(residuals(model1))
#model1 <- glmer(CD ~ especie + (1|id_planta2), nAGQ = 0, data = hembras, 
#                family = Gamma(link = "inverse"))
#summary(model1)
#Anova(model1)

# Estimated marginal means 
egCD <- emmeans(model1, 
                specs = pairwise ~ especie, 
                type = "response")
egCD
egCD.m<-egCD$emmean 
egCD.m
model_means_cld_CD <- cld(object = egCD.m,
                          adjust = "sidak",
                          Letter = letters, 
                          alpha = 0.05)
model_means_cld_CD
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_CD, file = "model_means_cld_CD.csv")

### GLMs comparando el TL entre años y entre las diferentes especies ###
mod2 <- glm(TL ~ condición, data = hem_resu2, family = "Gamma")
summary(mod2)
shapiro.test(residuals(mod2))
Anova(mod2)

#model2 <- glmer(TL ~ especie + (1|id_planta2), nAGQ = 0, data = hembras, 
#                family = Gamma(link = "inverse"))
#summary(model2)
#Anova(model2)

# Estimated marginal means 
egTL <- emmeans(mod2, 
                   specs = pairwise ~ especie, 
                   type = "response")
egTL
egTL.m<-egTL$emmean 
egTL.m

model_means_cld_TL <- cld(object = egTL.m,
                             adjust = "sidak",
                             Letter = letters, 
                             alpha = 0.05)
model_means_cld_TL
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_TL, file = "model_means_cld_TL.csv")

### GLM comparando el CL entre las diferentes especies ###
mod3 <- glm(CL ~ condición, data = hem_resu2, family = "Gamma") # escogi este modelo
summary(mod3)
Anova(mod3)

#model3 <- glmer(CL ~ especie + (1|id_planta2), nAGQ = 0, data = hembras, 
#                family = Gamma(link = "inverse"))
#summary(model3)
#Anova(model3)

# Estimated marginal means 
egCL <- emmeans(mod3, 
                   specs = pairwise ~ especie, 
                   type = "response")
egCL
egCL.m<-egCL$emmean 
egCL.m
model_means_cld_CL <- cld(object = egCL.m,
                             adjust = "sidak",
                             Letter = letters, 
                             alpha = 0.05)
model_means_cld_CL
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_CL, file = "model_means_cld_CL.csv")

### GLM comparando el TD1 entre años y entre las diferentes especies ###
mod4 <- glm(TD1 ~ condición, data = hem_resu2,
              family = "Gamma") # escogi este modelo
summary(mod4)
Anova(mod4)
# Estimated marginal means 
egTD1 <- emmeans(mod4, 
                   specs = pairwise ~ especie, 
                   type = "response")
egTD1
egTD1.m<-egTD1$emmean 
egTD1.m

model_means_cld_TD1 <- cld(object = egTD1.m,
                             adjust = "sidak",
                             Letter = letters, 
                             alpha = 0.05)
model_means_cld_TD1
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_TD1, file = "model_means_cld_TD1.csv")

### GLM comparando el TD2 entre años y entre las diferentes especies ###
mod5 <- glm(TD2 ~ condición, data = hem_resu2,
              family = "Gamma") # escogi este modelo
summary(mod5)
Anova(mod5)
# Estimated marginal means 
egTD2 <- emmeans(mod5, 
                    specs = pairwise ~ especie, 
                    type = "response")
egTD2
egTD2.m<-egTD2$emmean 
egTD2.m

model_means_cld_TD2 <- cld(object = egTD2.m,
                              adjust = "sidak",
                              Letter = letters, 
                              alpha = 0.05)
model_means_cld_TD2
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_TD2, file = "model_means_cld_TD2.csv")

### GLM comparando el TD3 entre años y entre las diferentes especies ###
mod6 <- glm(TD3 ~ condición, data = hem_resu2,
              family = gaussian) # escogi este modelo
summary(mod6)
Anova(mod6)
# Estimated marginal means 
egTD3 <- emmeans(mod6, 
                    specs = pairwise ~ especie, 
                    type = "response")
egTD3
egTD3.m<-egTD3$emmean 
egTD3.m

model_means_cld_TD3 <- cld(object = egTD3.m,
                              adjust = "sidak",
                              Letter = letters, 
                              alpha = 0.05)
model_means_cld_TD3
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_TD3, file = "model_means_cld_TD3.csv")

### GLM comparando el NDf entre años y entre las diferentes especies ###
mod7 <- glm(NDf ~ condición, data = hem_resu2,
              family = Gamma) # escogi este modelo
summary(mod7)
Anova(mod7)
# Estimated marginal means 
egNDf <- emmeans(mod7, 
                    specs = pairwise ~ especie, 
                    type = "response")
egNDf
egNDf.m<-egNDf$emmean 
egNDf.m
model_means_cld_NDf <- cld(object = egNDf.m,
                              adjust = "sidak",
                              Letter = letters, 
                              alpha = 0.05)
model_means_cld_NDf
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_NDf, file = "model_means_cld_NDf.csv")

### GLM comparando el volumen de néctar entre las diferentes especies ###
str(hembras)
hembras2 = subset(hembras, vol_nec != 0)
sort(hembras2$vol_nec)
mod8 <- glmer(vol_nec ~ especie + (1|id_planta2), nAGQ = 0, data = hembras2,
              family = Gamma(link = "log")) # escogi este modelo
summary(mod8)
Anova(mod8)
(0.1999/(0.1999+0.3613))*100 #35%
# Estimated marginal means 
egvol <- emmeans(mod8, 
                    specs = pairwise ~ especie, 
                    type = "response")
egvol
egvol.m<-egvol$emmean 
egvol.m
model_means_cld_vol <- cld(object = egvol.m,
                              adjust = "sidak",
                              Letter = letters, 
                              alpha = 0.05)
model_means_cld_vol
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_vol, file = "model_means_cld_vol.csv")

### GLM comparando el PL entre años y entre las diferentes especies ###
mod10 <- glm(PL ~ condición, data = hem_resu2,
              family = "Gamma") #parece ser el mejor modelo
summary(mod10)
Anova(mod10)
# Estimated marginal means 
egPL <- emmeans(mod10, 
                specs = pairwise ~ especie, 
                type = "response")
egPL
egPL.m<-egPL$emmean 
egPL.m
model_means_cld_PL <- cld(object = egPL.m,
                          adjust = "sidak",
                          Letter = letters, 
                          alpha = 0.05)
model_means_cld_PL
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_PL, file = "model_means_cld_PL.csv")

### GLM comparando el SL entre años y entre las diferentes especies ###
mod11 <- glm(SL ~ condición, data = hem_resu2,
            family = "Gamma") # mejor modelo
summary(mod11)
Anova(mod11)
# Estimated marginal means 
egSL <- emmeans(mod11, 
                specs = pairwise ~ especie, 
                type = "response")
egSL
egSL.m<-egSL$emmean 
egSL.m
model_means_cld_SL <- cld(object = egSL.m,
                          adjust = "sidak",
                          Letter = letters, 
                          alpha = 0.05)
model_means_cld_SL
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_SL, file = "model_means_cld_SL.csv")

### GLM comparando el SD entre las diferentes especies ###
mod9 <- glm(SD ~ condición, data = hem_resu2,
              family = "Gamma") #parece ser el mejor modelo
summary(mod9)
Anova(mod9)
# Estimated marginal means 
egSD <- emmeans(mod9, 
                specs = pairwise ~ especie, 
                type = "response")
egSD
egSD.m<-egSD$emmean 
egSD.m
model_means_cld_SD <- cld(object = egSD.m,
                          adjust = "sidak",
                          Letter = letters, 
                          alpha = 0.05)
model_means_cld_SD
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_SD, file = "model_means_cld_SD.csv")

### GLM comparando el OD entre años y entre las diferentes especies ###
mod13 <- glm(OD ~ condición, data = hem_resu2,
             family = Gamma)
summary(mod13)
Anova(mod13)
# Estimated marginal means 
egOD <- emmeans(mod13, 
                specs = pairwise ~ especie, 
                type = "response")
egOD
egOD.m<-egOD$emmean 
egOD.m
model_means_cld_OD <- cld(object = egOD.m,
                          adjust = "sidak",
                          Letter = letters, 
                          alpha = 0.05)
model_means_cld_OD
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_OD, file = "model_means_cld_OD.csv")

### GLM comparando el OL entre años y entre las diferentes especies ###
mod12 <- glm(OL ~ condición, data = hem_resu2,
            family = "Gamma")
summary(mod12)
Anova(mod12)
# Estimated marginal means 
egOL <- emmeans(mod12, 
                specs = pairwise ~ especie, 
                type = "response")
egOL
egOL.m<-egOL$emmean 
egOL.m
model_means_cld_OL <- cld(object = egOL.m,
                          adjust = "sidak",
                          Letter = letters, 
                          alpha = 0.05)
model_means_cld_OL
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_OL, file = "model_means_cld_OL.csv")


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

ma_sum = machos %>%
  group_by(especie,id_planta2) %>%
  summarise(CD = mean(CD), n = n())
View(ma_sum)
ma_sum %>%
  group_by(especie) %>%
  summarise(CD = mean(CD), n = n())
#### Estadistica descriptiva de las flores macho ####
# Media
machos2 = machos %>%
  group_by(año, condicion, especie, id_planta2) %>%
  summarise(CD = mean(CD), TL = mean(TL), CL = mean(CL), TD1 = mean(TD1),
            TD2 = mean(TD2), TD3 = mean(TD3), NDm = mean(NDm), AD = mean(AD),
            StL = mean(StL), AL = mean(AL), VN = mean(vol_nec), n = n())
str(machos2)
machos2 <- as.data.frame(machos2)
str(machos2)


#machos3 = machos2 %>%
#  group_by(especie) %>%
#  summarise(CD = mean(CD), TL = mean(TL), CL = mean(CL), TD1 = mean(TD1),
#            TD2 = mean(TD2), TD3 = mean(TD3), NDm = mean(NDm), AD = mean(AD),
#            StL = mean(StL), AL = mean(AL), VN = mean(VN), n = n())
#View(machos3)
# Error estandar
machos4 = machos2 %>%
  group_by(especie) %>%
  summarise(CD = es(CD), TL = es(TL), CL = es(CL), TD1 = es(TD1), 
            TD2 = es(TD2), TD3 = es(TD3), NDm = es(NDm), AD = es(AD), 
            StL = es(StL), AL = es(AL), VN = es(VN), n = n())
View(machos4)
# Coeficiente de variación
machos %>%
  group_by(especie) %>%
  summarise(CD = cv(CD), TL = cv(TL), CL = cv(CL), TD1 = cv(TD1),
            TD2 = cv(TD2), TD3 = cv(TD3), NDm = cv(NDm), AD = cv(AD), 
            StL = cv(StL), AL = cv(AL), VN = cv(vol_nec), n = n())


#### boxplots de los caracteres de flores macho ####
# boxplots de los diferentes caracteres entre especies
boxplot(machos$CD ~ machos$especie)
boxplot(machos$TL ~ machos$especie)
boxplot(machos$CL ~ machos$especie)
boxplot(machos$TD1 ~ machos$especie)
boxplot(machos$TD2 ~ machos$especie)
boxplot(machos$TD3 ~ machos$especie)
boxplot(machos$NDm ~ machos$especie)

boxplot(machos$AD ~ machos$especie)
boxplot(machos$StL ~ machos$especie)
boxplot(machos$AL ~ machos$especie)

boxplot(machos$vol_nec ~ machos$especie)
sort(machos$vol_nec)
machos2 <- subset(machos, vol_nec != 0)
machos3 <- subset(machos2, vol_nec < 150)
sort(machos3$vol_nec)
boxplot(machos3$vol_nec ~ machos3$especie)

tapply(machos3$vol_nec, machos3$especie, mean)
tapply(machos3$vol_nec, machos3$especie, es)
#### Principal component analysis (PCA) de las flores macho ####
head(machos)
str(machos)
pairs(machos[11:21])
pca2 <- prcomp(machos[, 11:21], scale = T)
summary(pca2)
pca2$rotation[, 1:2]
#write.csv(pca1$rotation[, 1:2], file = "pca_hembras.csv")
plot(pca2)
biplot(pca2, scale=0)

#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
ca.condicion2 = machos[,4]
ca.species2 = machos[, 5]

# PCA por condición
ggbiplot(pca2, obs.scale = 1, var.scale = 1, 
         groups = ca.condicion2, ellipse = TRUE, 
         circle = F) + scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom')
# PCA por especie
ggbiplot(pca2, obs.scale = 1, var.scale = 1, 
              groups = ca.species2, ellipse = TRUE, 
              circle = F) + scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', 
               legend.position = 'bottom')


#### Principal coordinate analysis (PCoA) de flores macho ####
library(vegan)
cucu_dist2 = dist(machos2[,c(5:14)])
cucu_PCoA <- wcmdscale(d = cucu_dist2, eig = TRUE)
cucu_PCoA
cucu_PCoA$points
# gráfica del PCoA
ggplot(data = data.frame(cucu_PCoA$points),
       aes(x = Dim1, y = Dim2)) +
  geom_point() +
  theme_bw()
#
pcoa <- cmdscale(cucu_dist2, eig = TRUE, add = TRUE)
#convert pcoa results into data frame that can be plotted
pcoa_df <- data.frame(pcoa$points)
colnames(pcoa_df) <- c("PCo1", "PCo2")
pcoa_df$Species <- factor(machos2$especie) #add group of interest,
# mine was Morphospecies in the data frame cal_fem_data2
calf <- ggplot(pcoa_df, aes(x = PCo1, y = PCo2, color = Species)) + 
  geom_point(size = 2) +
  xlab("PCo1") +
  ylab("PCo2") + 
  ggtitle("Flores macho") +
  theme_classic()
calf
#
pcoa1 <- cmdscale(cucu_dist2, eig = TRUE, add = TRUE)
#convert pcoa results into data frame that can be plotted
pcoa1_df <- data.frame(pcoa1$points)
colnames(pcoa1_df) <- c("PCo1", "PCo2")
pcoa1_df$condicion <- factor(machos2$condicion) #add group of interest,
# mine was Morphospecies in the data frame cal_fem_data2
calf <- ggplot(pcoa1_df, aes(x = PCo1, y = PCo2, color = condicion)) + 
  geom_point(size = 2) +
  xlab("PCo1") +
  ylab("PCo2") + 
  ggtitle("Flores macho por condición") +
  theme_classic()+
  scale_color_discrete(labels = c("Domesticadas", "silvestres"))
calf

### PERMANOVA DE LOS DIFERENTES AMINOACIDOS
library(vegan)
head(machos)
str(machos)
dune5 = machos[11:21]
# Calculando distancia euclidiana
dune.dist5 <- vegdist(dune5, method="euclidean")

# calculando PERMANOVA por condición
dune.div5 <- adonis2(dune5 ~ condicion, data = machos,
                     permutations = 999, method="euclidean")
dune.div5

# calculando PERMANOVA por especie
dune.div6 <- adonis2(dune5 ~ condicion+especie, data = machos,
                     permutations = 999, method="euclidean")
dune.div6

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


#### PERMANOVA de flores macho ####
head(machos)
dune2 = machos[11:21]
# Calculando distancia euclidiana
dune.dist2 <- vegdist(dune2, method="euclidean")

# calculando PERMANOVA por especie
dune.div3 <- adonis2(dune2 ~ especie, data = machos,
                    permutations = 999, method="euclidean")
dune.div3
# calculando PERMANOVA por condición
dune.div4 <- adonis2(dune2 ~ condicion, data = machos,
                     permutations = 999, method="euclidean")
dune.div4
# overall tests
#adonis2(dune ~ especie, data = hembras, 
#        permutations = 999, method="euclidean", by = NULL)

# calculando beta dispersión (homogeneidad de varianzas)
dispersion <- betadisper(dune.dist2, group=machos$condicion)
permutest(dispersion)
plot(dispersion, hull=FALSE, ellipse=T) ##sd ellipse

dispersion4 <- betadisper(dune.dist2, group=machos$especie)
permutest(dispersion4)
plot(dispersion4, hull=FALSE, ellipse=T) ##sd ellipse

#### GLMMs de flores macho ####
str(machos2)
### GLMs comparando el CD entre las diferentes especies ###
mod14 <- glm(CD ~ especie, data = machos2,
            family = "Gamma") #escogí este modelo
summary(mod14)
Anova(mod14)
# Estimated marginal means 
egCD_ma <- emmeans(mod14, 
                specs = pairwise ~ especie, 
                type = "response")
egCD_ma
egCD_ma.m<-egCD_ma$emmean 
egCD_ma.m
model_means_cld_CD_ma <- cld(object = egCD_ma.m,
                          adjust = "sidak",
                          Letter = letters, 
                          alpha = 0.05)
model_means_cld_CD_ma
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_CD_ma, file = "model_means_cld_CD_ma.csv")

### GLMs comparando el TL entre años y entre las diferentes especies ###
mod15 <- glmer(TL ~ especie + (1|id_planta2), nAGQ = 0, data = machos,
               family = "Gamma") #escogí este modelo
summary(mod15)
Anova(mod15)
# Estimated marginal means 
egTL_ma <- emmeans(mod15, 
                   specs = pairwise ~ especie, 
                   type = "response")
egTL_ma
egTL_ma.m<-egTL_ma$emmean 
egTL_ma.m
model_means_cld_TL_ma <- cld(object = egTL_ma.m,
                             adjust = "sidak",
                             Letter = letters, 
                             alpha = 0.05)
model_means_cld_TL_ma
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_TL_ma, file = "model_means_cld_TL_ma.csv")

### GLMs comparando el CL entre años y entre las diferentes especies ###
mod16 <- glmer(CL ~ especie + (1|id_planta2), nAGQ = 0, data = machos,
               family = "Gamma") #escogí este modelo
summary(mod16)
Anova(mod16)
# Estimated marginal means 
egCL_ma <- emmeans(mod16, 
                   specs = pairwise ~ especie, 
                   type = "response")
egCL_ma
egCL_ma.m<-egCL_ma$emmean 
egCL_ma.m
model_means_cld_CL_ma <- cld(object = egCL_ma.m,
                             adjust = "sidak",
                             Letter = letters, 
                             alpha = 0.05)
model_means_cld_CL_ma
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_CL_ma, file = "model_means_cld_CL_ma.csv")

### GLMs comparando el TD1 entre años y entre las diferentes especies ###
mod17 <- glmer(TD1 ~ especie + (1|id_planta2), nAGQ = 0, data = machos,
            family = "Gamma") #escogí este modelo
summary(mod17)
Anova(mod17)
# Estimated marginal means 
egTD1_ma <- emmeans(mod17, 
                   specs = pairwise ~ especie, 
                   type = "response")
egTD1_ma
egTD1_ma.m<-egTD1_ma$emmean 
egTD1_ma.m
model_means_cld_TD1_ma <- cld(object = egTD1_ma.m,
                             adjust = "sidak",
                             Letter = letters, 
                             alpha = 0.05)
model_means_cld_TD1_ma
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_TD1_ma, file = "model_means_cld_TD1_ma.csv")

### GLMs comparando el TD2 entre años y entre las diferentes especies ###
mod18 <- glmer(TD2 ~ especie + (1|id_planta2), nAGQ = 0, data = machos,
               family = "Gamma") #escogí este modelo
summary(mod18)
Anova(mod18)
# Estimated marginal means 
egTD2_ma <- emmeans(mod18, 
                    specs = pairwise ~ especie, 
                    type = "response")
egTD2_ma
egTD2_ma.m<-egTD2_ma$emmean 
egTD2_ma.m
model_means_cld_TD2_ma <- cld(object = egTD2_ma.m,
                              adjust = "sidak",
                              Letter = letters, 
                              alpha = 0.05)
model_means_cld_TD2_ma
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_TD2_ma, file = "model_means_cld_TD2_ma.csv")

### GLMs comparando el TD3 entre años y entre las diferentes especies ###
mod19 <- glmer(TD3 ~ especie + (1|id_planta2), nAGQ = 0, data = machos,
               family = "Gamma") #escogí este modelo
summary(mod19)
Anova(mod19)
# Estimated marginal means 
egTD3_ma <- emmeans(mod19, 
                    specs = pairwise ~ especie, 
                    type = "response")
egTD3_ma
egTD3_ma.m<-egTD3_ma$emmean 
egTD3_ma.m
model_means_cld_TD3_ma <- cld(object = egTD3_ma.m,
                              adjust = "sidak",
                              Letter = letters, 
                              alpha = 0.05)
model_means_cld_TD3_ma
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_TD3_ma, file = "model_means_cld_TD3_ma.csv")

### GLMs comparando el TD3 entre años y entre las diferentes especies ###
mod20 <- glmer(NDm ~ especie + (1|id_planta2), nAGQ = 0, data = machos,
               family = "Gamma") #escogí este modelo
summary(mod20)
Anova(mod20)
# Estimated marginal means 
egNDm_ma <- emmeans(mod20, 
                    specs = pairwise ~ especie, 
                    type = "response")
egNDm_ma
egNDm_ma.m<-egNDm_ma$emmean 
egNDm_ma.m
model_means_cld_NDm_ma <- cld(object = egNDm_ma.m,
                              adjust = "sidak",
                              Letter = letters, 
                              alpha = 0.05)
model_means_cld_NDm_ma
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_NDm_ma, file = "model_means_cld_NDm_ma.csv")

### GLMs comparando el volumen de néctar entre las diferentes especies ###
sort(machos$vol_nec)
machos2 = subset(machos, vol_nec != 0)
str(machos)
str(machos2)

mod21 <- glmer(vol_nec ~ especie + (1|id_planta2), nAGQ = 0, data = machos2,
               family = "Gamma") #escogí este modelo
summary(mod21)
Anova(mod21)
# Estimated marginal means 
egvol_ma <- emmeans(mod21, 
                    specs = pairwise ~ especie, 
                    type = "response")
egvol_ma
egvol_ma.m<-egvol_ma$emmean 
egvol_ma.m
model_means_cld_vol_ma <- cld(object = egvol_ma.m,
                              adjust = "sidak",
                              Letter = letters, 
                              alpha = 0.05)
model_means_cld_vol_ma
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_vol_ma, file = "model_means_cld_vol_ma.csv")

### GLMs comparando el AD entre años y entre las diferentes especies ###
mod22 <- glmer(AD ~ especie + (1|id_planta2), nAGQ = 0, data = machos,
            family = "Gamma") #escogí este modelo
summary(mod22)
Anova(mod22)
# Estimated marginal means 
egAD_ma <- emmeans(mod22, 
                specs = pairwise ~ especie, 
                type = "response")
egAD_ma
egAD_ma.m<-egAD_ma$emmean 
egAD_ma.m

model_means_cld_AD_ma <- cld(object = egAD_ma.m,
                          adjust = "sidak",
                          Letter = letters, 
                          alpha = 0.05)
model_means_cld_AD_ma
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_AD_ma, file = "model_means_cld_AD_ma.csv")

### GLMs comparando el StL entre años y entre las diferentes especies ###
mod23 <- glmer(StL ~ especie + (1|id_planta2), nAGQ = 0, data = machos,
               family = "Gamma") #escogí este modelo
summary(mod23)
Anova(mod23)
# Estimated marginal means 
egStL_ma <- emmeans(mod23, 
                   specs = pairwise ~ especie, 
                   type = "response")
egStL_ma
egStL_ma.m<-egStL_ma$emmean 
egStL_ma.m

model_means_cld_StL_ma <- cld(object = egStL_ma.m,
                             adjust = "sidak",
                             Letter = letters, 
                             alpha = 0.05)
model_means_cld_StL_ma
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_StL_ma, file = "model_means_cld_StL_ma.csv")

### GLMs comparando el AL entre años y entre las diferentes especies ###
mod24 <- glmer(AL ~ especie + (1|id_planta2), nAGQ = 0, data = machos,
               family = "Gamma") #escogí este modelo
summary(mod24)
Anova(mod24)
# Estimated marginal means 
egAL_ma <- emmeans(mod24, 
                    specs = pairwise ~ especie, 
                    type = "response")
egAL_ma
egAL_ma.m<-egAL_ma$emmean 
egAL_ma.m

model_means_cld_AL_ma <- cld(object = egAL_ma.m,
                              adjust = "sidak",
                              Letter = letters, 
                              alpha = 0.05)
model_means_cld_AL_ma
# exportando tabla de la estimated marginal means
write.csv(model_means_cld_AL_ma, file = "model_means_cld_AL_ma.csv")


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
head(hem_nec)
hem_nec$VN
# obteniendo media, error standard y numero de individuos por condicion
tapply(hem_nec$VN, hem_nec$especie, mean)
tapply(hem_nec$VN, hem_nec$especie, es)
tapply(hem_nec$VN, hem_nec$especie, length)
tapply(hem_nec$VN, hem_nec$especie, min)
tapply(hem_nec$VN, hem_nec$especie, max)
# boxplot del volumen de néctar en flores hembra
boxplot(hem_nec$VN ~ hem_nec$especie)

# analisis de normalidad del volumen de nectar
# Hembras
hist(hem_nec$VN)
shapiro.test(hem_nec$VN) #no normal

### Análisis estadistico del volumen de nectar
# Hembras
glm_hemnec = glm(VN ~ especie, data = hem_nec, family = Gamma(link = "inverse"))
summary(glm_hemnec)
Anova(glm_hemnec)
shapiro.test(residuals(glm_hemnec))
hist(residuals(glm_hemnec))

# modelo con poisson y quasipoisson
glm_hemnec_pois = glm(VN ~ especie, data = hem_nec, family = quasipoisson())
summary(glm_hemnec_pois)
Anova(glm_hemnec_pois)
shapiro.test(residuals(glm_hemnec_pois))
hist(residuals(glm_hemnec_pois))


# modelo con binomial negativa
mod_nb = glm.nb(VN ~ especie, data = hem_nec)
summary(mod_nb)
Anova(mod_nb)
shapiro.test(residuals(mod_nb))
hist(residuals(mod_nb))




# modelo con gaussian
glm_hemnec_gaus = glm(VN ~ especie, data = hem_nec,
                      family = gaussian)
summary(glm_hemnec_gaus)
Anova(glm_hemnec_gaus)
shapiro.test(residuals(glm_hemnec_gaus))
plot(residuals(glm_hemnec_gaus))

# Estimated marginal means 

em <- emmeans(mod_nb, "especie")
contrast(em, "pairwise", adjust = "tukey")


volu_compa <- emmeans(mod_nb, 
                      specs = pairwise ~ especie, 
                      type = "response")
volu_compa
volu_compa.m<-volu_compa$emmean 
volu_compa.m
model_means_cld_volu <- cld(object = volu_compa.m,
                            adjust = "sidak",
                            Letter = letters, 
                            alpha = 0.05)
model_means_cld_volu





### Volumen de néctar de flores macho
machos2 <- subset(machos, vol_nec != 0)
machos3 <- subset(machos2, vol_nec < 150)
str(machos3)
sort(machos3$vol_nec)
# Agrupando volumen de néctar por individuo
mac_nec <- machos3 %>%
  group_by(año,condicion,especie,id_planta2) %>%
  dplyr::summarize(VN = mean(vol_nec), n = n()); mac_nec
# obteniendo media, error standard y numero de individuos por condicion
tapply(mac_nec$VN, mac_nec$especie, mean)
tapply(mac_nec$VN, mac_nec$especie, es)
tapply(mac_nec$VN, mac_nec$especie, length)
# boxplot de volumen de néctar en flores macho
boxplot(mac_nec$VN ~ mac_nec$especie)


# Machos
head(mac_nec)
hist(mac_nec$VN)
shapiro.test(mac_nec$VN) # no normal
# Histograma con logaritmmo
hist(log(mac_nec$VN))
shapiro.test(log(mac_nec$VN)) # logaritmo


# Machos
aov_macnec = aov(log(VN) ~ especie, data = mac_nec)
summary(aov_macnec)
TukeyHSD(aov_macnec)

# grafica de hembras
hemb = ggplot(hem_nec, aes(x=especie,y=VN, fill = condición))+
         geom_boxplot()+
  geom_point(size = 2, alpha = .3, #aes(color = especie)
             position = position_jitter(seed = 1, width = .1))+
  labs(x="Especies", y = "Volumen de néctar (μl)")+
  scale_x_discrete(labels=c("1CF" = "CF", "2CPF" = "CPF",
                            "3CPP" = "CPP", "4CAS" = "CAS", "5CAA" = "CAA",
                            "6CM" = "CM"))+
  theme_bw()+
  theme(axis.text = element_text(face = "bold"))+
  scale_color_discrete(name="Species",
                       labels = c("CF", "CPF", "CPP", "CAS", "CAA", "CM"))+
  annotate("text", x=1, y=35, label= "ab", size = 5)+
  annotate("text", x=2, y=50, label= "a", size = 5)+
  annotate("text", x=3, y=205, label= "c", size = 5)+
  annotate("text", x=4, y=65, label= "a", size = 5)+
  annotate("text", x=5, y=177, label= "b", size = 5)+
  annotate("text", x=6, y=165, label= "bc", size = 5)+
  annotate("text", x=1.5, y= 200, label = "F=11.44, gl=5, p < 0.001")+
  #theme(legend.position = "none")+
  #theme(axis.title.x = element_blank())+
  scale_fill_manual(values = c("#DE7862FF", "#58A449FF"))
hemb
# grafica de machos
mach = ggplot(mac_nec, aes(x=especie,y=VN, fill = condicion))+
  geom_boxplot()+
  geom_point(size = 2, alpha = .3, #aes(color = especie)
             position = position_jitter(seed = 1, width = .1))+
  labs(x="Especies", y = "Volumen de néctar (μl)")+
  scale_x_discrete(labels=c("1CF" = "CF", "2CPF" = "CPF",
                            "3CPP" = "CPP", "4CAS" = "CAS", "5CAA" = "CAA",
                            "6CM" = "CM"))+
  theme_bw()+
  theme(axis.text = element_text(face = "bold"))+
  scale_fill_discrete(name = "Condición", labels = c("Domesticadas",
                                                     "Silvestres"))+
  #scale_color_discrete(name="Species",
   #                    labels = c("CF", "CPF", "CPP", "CAS", "CAA", "CM"))+
  annotate("text", x=1, y=21, label= "a", size = 5)+
  annotate("text", x=2, y=24, label= "a", size = 5)+
  annotate("text", x=3, y=88, label= "b", size = 5)+
  annotate("text", x=4, y=44, label= "a", size = 5)+
  annotate("text", x=5, y=72, label= "b", size = 5)+
  annotate("text", x=6, y=61, label= "a", size = 5)+
  annotate("text", x=1.5, y = 80, label = "F=9.451, gl=5, p < 0.001")+
  #theme(legend.position = "none")+
  scale_fill_manual(values = c("#DE7862FF", "#58A449FF"))
mach

# cargando paqueteria gridExtra
library(gridExtra)
library(cowplot)
library(ggpubr)
gt <- grid.arrange(hemb, mach,
                   ncol = 1, nrow = 2)
# Add labels to the arranged plots
p <- as_ggplot(gt) +                                # transform to a ggplot
  draw_plot_label(label = c("A", "B"), size = 15,
                  x = c(0,0), y = c(1,.5))#Add labels
p

ggsave("nectar.jpg", device = "jpg", width = 23, height = 23,
       units = "cm", dpi = 300)



# Codigo para construir gráfica de barras
volumen = read.csv("vol_nec.csv", header = T)
str(volumen)
# gráfica
ggplot(volumen, aes(x = especie, y = med))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_errorbar(aes(ymin=med-error, ymax=med+error), width = .2,
                position = position_dodge(.9))+
  
  

#### Concentración de azúcar (refractometro) ####
# elimine los valores de cas5-h2 y cas15m1 porque eran muy pequeños
azucar <- read.csv("refractometro.csv", header = T)
head(azucar)
str(azucar)
# Filtrando los datos para la parcela de morelia
azucar_m = subset(azucar, sitio == "morelia")
str(azucar_m)
# histograma y prueba de normalidad
hist(azucar_m$concentración)
shapiro.test(azucar_m$concentración)
# Modelado de la distribución de la concentración de azucares
p <- seq(0.05, 0.95, 0.05)
observed_quantiles <- quantile(azucar_m$concentración, p)
theoretical_quantiles <- qnorm(p, 
                               mean = mean(azucar_m$concentración),
                               sd = sd(azucar_m$concentración))
# make QQ-plot
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# gráfica de la concentración por el sexo y la especie
boxplot(azucar_m$concentración ~ azucar_m$spp)
boxplot(azucar_m$concentración ~ azucar_m$sexo+azucar_m$spp)

# GLM de la concentración entre especies y tratamiento
mod_ref <- glm(concentración ~ spp*trat+sexo, data = azucar_m,
               family = gaussian(link = "identity"))
summary(mod_ref)
anova(mod_ref, test = "Chisq")
# AIC: 1958.1
# comparaciones multiples
compa = emmeans(mod_ref, specs = pairwise ~ spp|trat)
compa$contrasts
compa2 = emmeans(mod_ref, specs = pairwise ~ trat|spp)
compa2$contrasts
compa3 = emmeans(mod_ref, specs = pairwise ~ spp)
compa3$contrasts

# GLM de la concentración de azucar por especie y tratamiento, considerando
# el id de la planta como factor aleatorio
mod_ref2 = glmer(concentración ~ spp * trat + (1 | id_planta), data = azucar_m, 
                 family = gaussian(link = "identity"))

#mod_ref2 = aov(concentración ~ spp * trat + Error(id_planta), data = azucar_m)
summary(mod_ref2)
anova(mod_ref2)
Anova(mod_ref2)
extractAIC(mod_ref2)
# comparaciones multiples
compa4 = emmeans(mod_ref2, specs = pairwise ~ spp)
compa4$contrasts
compa5 = emmeans(mod_ref2, specs = pairwise ~ spp|trat)
compa5$contrasts
compa6 = emmeans(mod_ref2, specs = pairwise ~ trat|spp)
compa6$contrasts

# Gráfico de la concentración de azucares
ggplot(data = azucar_m, aes(spp, concentración, color = trat))+
  geom_boxplot()

#### Concentración de azúcar medido con EIR en Iztacala ####
# NOTA: elimine valores de CAA4HT2 y CAA5HT2 porque no encontre el tubo 
# (id 53 y 54), también eliminé un valor de CAS5HT2 que es muy bajo.
azucares = read.csv("azucares_2022.csv", header = T)
head(azucares)
str(azucares)
tapply(azucares$Suma.total, list(azucares$spp, azucares$sexo), length)
tapply(azucares$Suma.total, list(azucares$sitio, azucares$spp), length)
tapply(azucares$Suma.total, list(azucares$sitio, azucares$spp,
                                 azucares$sexo), length)

# Extrayendo los valores de morelia
azucares_more = subset(azucares, sitio == "morelia")
str(azucares_more)
head(azucares_more)
tapply(azucares_more$Suma.total, list(azucares_more$spp,
                                      azucares_more$sexo), length)
# eliminar los 2 datos de CF de flores macho
azucares_more2 = subset(azucares_more, spp != "1CF")
str(azucares_more2)
tapply(azucares_more2$Suma.total, list(azucares_more2$spp,
                                      azucares_more2$sexo), length)


tapply(azucares_more2$Suma.total, list(azucares_more2$spp,
                                      azucares_more2$sexo), mean)
tapply(azucares_more2$Suma.total, list(azucares_more2$spp,
                                       azucares_more2$sexo), es)


tapply(azucares_more2$fructosa, list(azucares_more2$spp,
                                      azucares_more2$sexo), mean)
tapply(azucares_more2$glucosa, list(azucares_more2$spp,
                                      azucares_more2$sexo), mean)
tapply(azucares_more2$sacarosa, list(azucares_more2$spp,
                                      azucares_more2$sexo), mean)

# Separando flores hembra
# histogramas de cada azucar
hist(azucares_more2$fructosa)
hist(azucares_more2$glucosa)
hist(azucares_more2$sacarosa)
hist(azucares_more2$Suma.total)
# prueba de normalidad
shapiro.test(azucares_more2$fructosa) # no normal
shapiro.test(azucares_more2$glucosa) # no normal
shapiro.test(azucares_more2$sacarosa)
shapiro.test(azucares_more2$Suma.total)
# modelado de distribucion
boxplot(azucares_more2$fructosa ~ azucares_more2$spp)#azucares_more2$sexo + 
boxplot(azucares_more2$glucosa ~ azucares_more2$spp)
boxplot(azucares_more2$sacarosa ~ azucares_more2$spp)
boxplot(azucares_more2$Suma.total ~ azucares_more2$spp)
# glm de FRUCTOSA
hem_fruc = glm(fructosa ~ spp*sexo, data = azucares_more2, family = Gamma)
summary(hem_fruc)
Anova(hem_fruc)
shapiro.test(residuals(hem_fruc))
# comparaciones multiples
fru_compa <- emmeans(hem_fruc, 
                     specs = pairwise ~ spp, 
                     type = "response")
fru_compa
fru_compa.m<-fru_compa$emmean 
fru_compa.m
model_means_cld_fru <- cld(object = fru_compa.m,
                           adjust = "tukey",
                           Letter = letters, 
                           alpha = 0.05)
model_means_cld_fru
# glm de GLUCOSA
hem_glu = glm(glucosa ~ spp*sexo, data = azucares_more2, family = Gamma)
summary(hem_glu)
Anova(hem_glu)
shapiro.test(residuals(hem_glu))
# comparaciones multiples
glu_compa <- emmeans(hem_glu, 
                     specs = pairwise ~ spp, 
                     type = "response")
glu_compa
glu_compa.m<-glu_compa$emmean 
glu_compa.m
model_means_cld_glu <- cld(object = glu_compa.m,
                           adjust = "tukey",
                           Letter = letters, 
                           alpha = 0.05)
model_means_cld_glu
### glm de SACAROSA
azucares_more2$sacarosa
hem_sac = glm(sacarosa ~ spp*sexo, data = azucares_more2, family = gaussian)
summary(hem_sac)
Anova(hem_sac)
shapiro.test(residuals(hem_sac))
plot(residuals(hem_sac))
# comparaciones multiples
sac_compa <- emmeans(hem_sac, 
                      specs = pairwise ~ spp, 
                      type = "response")
sac_compa
sac_compa.m<-sac_compa$emmean 
sac_compa.m
model_means_cld_sac <- cld(object = sac_compa.m,
                            adjust = "tukey",
                            Letter = letters, 
                            alpha = 0.05)
model_means_cld_sac
### glm de AZUCAR TOTAL
hem_total = glm(Suma.total ~ spp*sexo, data = azucares_more2, family = gaussian)
summary(hem_total)
Anova(hem_total)
shapiro.test(residuals(hem_total))
plot(residuals(hem_total))
# comparaciones multiples
total_compa <- emmeans(hem_total, 
                     specs = pairwise ~ spp, 
                     type = "response")
total_compa
total_compa.m<-total_compa$emmean 
total_compa.m
model_means_cld_total <- cld(object = total_compa.m,
                           adjust = "tukey",
                           Letter = letters, 
                           alpha = 0.05)
model_means_cld_total




library(MVN)
# concentración de azucares de flores hembra
mvn(azu_hem[18:21], mvnTest = "hz", univariateTest = "SW", 
    univariatePlot = "histogram")

# MANOVA DE LAS CONCENTRACIONES DE AZUCARES DE FLORES HEMBRA
mod1<-manova(cbind(fructosa, glucosa, sacarosa, Suma.total) ~ condicion, 
             data = azu_hem)
summary(mod1) #Diferencias significativas
# PERMANOVA DE LAS CONCENTRACIONES DE AZUCARES DE FLORES HEMBRA
str(azu_hem)
azu_hem[18:21]
dune8 = azu_hem[18:21]
# Calculando distancia euclidiana
set.seed(0)
# permanova
dune.div8 <- adonis2(dune8 ~ condicion, data = azu_hem,
                     permutations = 999, method="euclidean")
dune.div8


# Separando flores macho
azu_mac = subset(azucares_more, sexo == "m")
str(azu_mac)
# histogramas de cada azucar
hist(azu_mac$fructosa)
hist(azu_mac$glucosa)
hist(azu_mac$sacarosa)
hist(azu_mac$Suma.total)
# prueba de normalidad
shapiro.test(azu_mac$fructosa) 
shapiro.test(azu_mac$glucosa) # no normal
shapiro.test(azu_mac$sacarosa)
shapiro.test(azu_mac$Suma.total)
# glm de fructosa
mac_fruc = glm(fructosa ~ condicion, data = azu_mac, family = gaussian)
summary(mac_fruc)
anova(mac_fruc)
Anova(mac_fruc)
# glm de glucosa
mac_glu = glm(glucosa ~ condicion, data = azu_mac, family = Gamma)
summary(mac_glu)
anova(mac_glu)
# glm de sacarosa
mac_sac = glm(sacarosa ~ condicion, data = azu_mac, family = gaussian)
summary(mac_sac)
anova(mac_sac)
# glm de azucar total
mac_total = glm(Suma.total ~ condicion, data = azu_mac, family = gaussian)
summary(mac_total)
anova(mac_total)

library(MVN)
# concentración de azucares de flores hembra
mvn(azu_mac[18:21], mvnTest = "hz", univariateTest = "SW", 
    univariatePlot = "histogram")

# MANOVA DE LAS CONCENTRACIONES DE AZUCARES DE FLORES HEMBRA
mod2<-manova(cbind(fructosa, glucosa, sacarosa, Suma.total) ~ condicion, 
             data = azu_mac)
summary(mod2) #Diferencias significativas
# PERMANOVA DE LAS CONCENTRACIONES DE AZUCARES DE FLORES HEMBRA
str(azu_mac)
azu_mac[18:21]
dune9 = azu_mac[18:21]
# Calculando distancia euclidiana
set.seed(0)
# permanova
dune.div9 <- adonis2(dune9 ~ condicion, data = azu_mac,
                     permutations = 999, method="euclidean")
dune.div9







tapply(azucares_more$Suma.total, list(azucares_more$condicion,
                                      azucares_more$sexo), mean)
tapply(azucares_more$Suma.total, list(azucares_more$condicion,
                                      azucares_more$sexo), es)
# histogramas de los porcentajes de azucar
hist(azucares_more$Suma.total)
hist(azucares_more$fructosa) #log
hist(azucares_more$glucosa)
hist(azucares_more$sacarosa)
boxplot(azucares_more$Suma.total ~ azucares_more$sexo)
boxplot(azucares_more$fructosa ~ azucares_more$sexo)
boxplot(azucares_more$glucosa ~ azucares_more$sexo)
boxplot(azucares_more$sacarosa ~ azucares_more$sexo)

# Prueba de normalidad de cada azucar y el azucar total
shapiro.test(azucares_more$Suma.total) # normal
# prueba de normalidad de fructosa
shapiro.test(azucares_more$fructosa)
shapiro.test(log(azucares_more$fructosa)) # log
# prueba de normalidad de glucosa
shapiro.test(azucares_more$glucosa)
shapiro.test(log(azucares_more$glucosa)) # log
# prueba de normalidad de sacarosa
shapiro.test(azucares_more$sacarosa)

# Media +- error estandar de la concentración de azucar medida con el
# refractometro
azucares_more %>%
  group_by(spp) %>%
  dplyr::summarize(brix2 = mean(brix), brixes = es(brix), n=n())

# Media +- error estandar de la concentración de azucar medida con el 
# Espectrofotometro infrarrojo
azucares_more %>%
  group_by(spp) %>%
  dplyr::summarize(total = mean(Suma.total), totales = es(Suma.total), n=n())

### ANALISIS DEL AZUCAR TOTAL MEDIDO CON EL REFRACTOMETRO ###
#shapiro.test(azucares_more$brix)
#total_refra = aov(brix ~ spp*sexo*trat, data = azucares_more)
#summary(total_refra)

#compa_brix = TukeyHSD(total_refra)
#write.table(compa_brix$spp, file = "compa_brix_spp.csv")

# boxplot de los grados brix con las diferentes variables
ggplot(data = azucares_more, aes(x = spp, y = brix))+
  geom_boxplot()
ggplot(data = azucares_more, aes(spp, brix, color = sexo))+
  geom_boxplot()
ggplot(data = azucares_more, aes(spp, brix, color = trat))+
  geom_boxplot()

### AZUCAR TOTAL MEDIDO CON EL EIR ###
# ANOVA de la concentración total de azucares
shapiro.test(azucares_more$Suma.total)
total_ano = aov(Suma.total ~ condicion*spp, 
                data = azucares_more)
summary(total_ano)
TukeyHSD(total_ano)
# sacarosa
total_ano = aov(sacarosa ~ condicion*sexo, 
                data = azucares_more)
summary(total_ano)
# fructosa
total_ano = aov(fructosa ~ condicion*sexo, 
                data = azucares_more)
summary(total_ano)
# glucosa
total_ano = aov(glucosa ~ condicion*sexo, 
                data = azucares_more)
summary(total_ano)

# comparaciones multiples
compa1 = TukeyHSD(total_ano)
compa1
#compa1$`spp:sexo`
azu1 = multcompLetters4(total_ano, compa1)

#write.table(compa1$spp, file = "compa_spp_aov_concentracion.csv")
#write.table(compa1$`spp:sexo`, file = "compa_sppsexo_aov_concentracion.csv")

# Modelo lineal generalizado de la concentración total de azúcar en el néctar
#total = glmer(Suma.total ~ spp+trat+sexo + (1|id_planta), data = azucares_more,
#            family = gaussian(link = "identity"))
#summary(total)
#anova(total, test = "Chisq")
#Anova(total)
# comparaciones multiples
#compa_total = emmeans(total, specs = pairwise ~ spp)
#compa_total$contrasts
#compa_total2 = emmeans(total, specs = pairwise ~ trat|spp)
#compa_total2$contrasts

### ggplot2 boxplot de la concentración de azúcar entre las especies
ggplot(azucares_more, aes(spp, Suma.total)) +
  geom_boxplot(alpha = 0.3) +
  ## use either geom_point() or geom_jitter()
  geom_point(size = 2, alpha = .3, #
             position = position_jitter(seed = 1, width = .1))+
  #stat_summary(geom = "text", fun.data = n_fun)+
  #coord_flip()+
  labs(x = "Species", y = "Total sugar concentration (w/v)")+
  scale_x_discrete(labels=c("1CF" = "CF", "2CPF" = "CPF",
                            "3CPP" = "CPP", "4CAS" = "CAS", "5CAA" = "CAA",
                            "6CM" = "CM"))+
  theme_bw()+
  theme(axis.text = element_text(face = "bold"))+
  scale_color_discrete(name="Especies",
                       labels = c("CF", "CPF", "CPP", "CAS", "CAA", "CM"))+
  annotate("text", x=1, y=22, label= "n = 2", size = 2)+
  annotate("text", x=2, y=22, label= "n = 9", size = 2)+
  annotate("text", x=3, y=22, label= "n = 9", size = 2)+
  annotate("text", x=4, y=22, label= "n = 22", size = 2)+
  annotate("text", x=5, y=22, label= "n = 19", size = 2)+
  annotate("text", x=6, y=22, label= "n = 22", size = 2)+
  annotate("text", x=1, y=51, label= "abc", size = 3)+
  annotate("text", x=2, y=68, label= "c", size = 3)+
  annotate("text", x=3, y=55, label= "b", size = 3)+
  annotate("text", x=4, y=52, label= "b", size = 3)+
  annotate("text", x=5, y=64, label= "b", size = 3)+
  annotate("text", x=6, y=69, label= "b", size = 3)

ggsave("azucar_total.jpg", device = "jpg", width = 17.3, height = 11.5,
       units = "cm", dpi = 300)

##
dose.labs <- c("Hembras", "Machos")
names(dose.labs) <- c("h", "m")

str(azucares_more)
# AZUCARES TOTALES
ggplot(azucares_more, aes(spp, Suma.total, fill = condicion)) +
  geom_boxplot() +
  ## use either geom_point() or geom_jitter()
  geom_point(size = 2, alpha = .3, #
             position = position_jitter(seed = 1, width = .1))+
  #stat_summary(geom = "text", fun.data = n_fun)+
  #coord_flip()+
  labs(x = "Especies", y = "Concentración total de azúcar (w/v)")+
  scale_x_discrete(labels=c("1CF" = "CF", "2CPF" = "CPF",
                            "3CPP" = "CPP", "4CAS" = "CAS", "5CAA" = "CAA",
                            "6CM" = "CM"))+
  theme_bw()+
  theme(axis.text = element_text(face = "bold"))+
  scale_color_discrete(name="Especies",
                       labels = c("CF", "CPF", "CPP", "CAS", "CAA", "CM"))+
  #facet_wrap(.~sexo, labeller = labeller(sexo = dose.labs))+
  scale_fill_manual(values = c("#DE7862FF", "#58A449FF"), name = "Condición",
                    labels = c("Domesticada", "Silvestre"))+
  annotate("text", x=1, y=51, label= "abc", size = 3)+
  annotate("text", x=2, y=68, label= "c", size = 3)+
  annotate("text", x=3, y=55, label= "b", size = 3)+
  annotate("text", x=4, y=52, label= "b", size = 3)+
  annotate("text", x=5, y=64, label= "b", size = 3)+
  annotate("text", x=6, y=69, label= "b", size = 3)



# SACAROSA
ggplot(azucares_more, aes(condicion, Suma.total, fill = condicion)) +
  geom_boxplot() +
  ## use either geom_point() or geom_jitter()
  geom_point(size = 2, alpha = .3, #
             position = position_jitter(seed = 1, width = .1))+
  #stat_summary(geom = "text", fun.data = n_fun)+
  #coord_flip()+
  labs(x = "Condición", y = "Concentración total de azúcar (w/v)")+
  scale_x_discrete(labels=c("1CF" = "CF", "2CPF" = "CPF",
                            "3CPP" = "CPP", "4CAS" = "CAS", "5CAA" = "CAA",
                            "6CM" = "CM"))+
  theme_bw()+
  theme(axis.text = element_text(face = "bold"))+
  scale_color_discrete(name="Especies",
                       labels = c("CF", "CPF", "CPP", "CAS", "CAA", "CM"))+
  annotate("text", x=1, y=22, label= "n = 2", size = 2)+
  facet_wrap(.~sexo, labeller = labeller(sexo = dose.labs))+
  scale_fill_manual(values = c("#DE7862FF", "#58A449FF"), name = "Condición",
                    labels = c("Domesticada", "Silvestre"))

# FRUCTOSA
ggplot(azucares_more, aes(condicion, Suma.total, fill = condicion)) +
  geom_boxplot() +
  ## use either geom_point() or geom_jitter()
  geom_point(size = 2, alpha = .3, #
             position = position_jitter(seed = 1, width = .1))+
  #stat_summary(geom = "text", fun.data = n_fun)+
  #coord_flip()+
  labs(x = "Condición", y = "Concentración total de azúcar (w/v)")+
  scale_x_discrete(labels=c("1CF" = "CF", "2CPF" = "CPF",
                            "3CPP" = "CPP", "4CAS" = "CAS", "5CAA" = "CAA",
                            "6CM" = "CM"))+
  theme_bw()+
  theme(axis.text = element_text(face = "bold"))+
  scale_color_discrete(name="Especies",
                       labels = c("CF", "CPF", "CPP", "CAS", "CAA", "CM"))+
  annotate("text", x=1, y=22, label= "n = 2", size = 2)+
  facet_wrap(.~sexo, labeller = labeller(sexo = dose.labs))+
  scale_fill_manual(values = c("#DE7862FF", "#58A449FF"), name = "Condición",
                    labels = c("Domesticada", "Silvestre"))

# GLUCOSA
ggplot(azucares_more, aes(condicion, Suma.total, fill = condicion)) +
  geom_boxplot() +
  ## use either geom_point() or geom_jitter()
  geom_point(size = 2, alpha = .3, #
             position = position_jitter(seed = 1, width = .1))+
  #stat_summary(geom = "text", fun.data = n_fun)+
  #coord_flip()+
  labs(x = "Condición", y = "Concentración total de azúcar (w/v)")+
  scale_x_discrete(labels=c("1CF" = "CF", "2CPF" = "CPF",
                            "3CPP" = "CPP", "4CAS" = "CAS", "5CAA" = "CAA",
                            "6CM" = "CM"))+
  theme_bw()+
  theme(axis.text = element_text(face = "bold"))+
  scale_color_discrete(name="Especies",
                       labels = c("CF", "CPF", "CPP", "CAS", "CAA", "CM"))+
  annotate("text", x=1, y=22, label= "n = 2", size = 2)+
  facet_wrap(.~sexo, labeller = labeller(sexo = dose.labs))+
  scale_fill_manual(values = c("#DE7862FF", "#58A449FF"), name = "Condición",
                    labels = c("Domesticada", "Silvestre"))

## ggplto2 boxplot de la concentración de azucar entre tratamientos
#ggplot(azucares_more, aes(trat, Suma.total, color = trat))+
#  geom_boxplot()+
#  geom_jitter(position = position_jitter(0.2))

# correlaciones entre los valores de concentración de azucar obtenidos del
# refractometro y los obtenidos con el espectrometro infrarojo
micor = cor(azucares_more$brix, azucares_more$Suma.total, method = "pearson")
cor.test(azucares_more$brix, azucares_more$Suma.total, method = "pearson")

library("ggpubr")
ggscatter(azucares_more, x = "brix", y = "Suma.total", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Observed concentration (%)",
          ylab = "Estimated concentration (%)")

# guardar grafica
ggsave("cor_refra_inf.jpg", device = "jpg", width = 13, height = 11.5,
       units = "cm", dpi = 300)


#### FRUCTOSA ####
# Media +- error estandar de la concentración de Fructosa por especie
azucares_more %>%
  group_by(condicion, sexo) %>%
  dplyr::summarize(fructosa2 = mean(fructosa), fructosa_es = es(fructosa),
                   n=n())

# ANOVA del porcentaje de fructosa entre las diferentes especies 
fruc = aov(log(fructosa) ~ condicion*sexo, data = azucares_more)
summary(fruc)
# prueba de comparaciones multiples
compa_fru = TukeyHSD(fruc)
compa_fru$spp
azu2 = multcompLetters4(fruc, compa_fru)
#write.table(compa_fru$spp, file = "compa_fru_spp.csv")

# Modelo lineal generalizado del porcentaje de Fructosa entre las especies,
# el sexo y el tratamiento
fru = glm(fructosa ~ spp+sexo+trat, data = azucares_more, 
          family = gaussian(link = "log"))
summary(fru)  
anova(fru, test = "Chisq")
Anova(fru)
# comparaciones multiples
compa_fru = emmeans(fru, specs = pairwise ~ spp)
compa_fru$contrasts
compa_fru2 = emmeans(fru, specs = pairwise ~ spp|sexo)
compa_fru2$contrasts
compa_fru3 = emmeans(fru, specs = pairwise ~ trat|spp)
compa_fru3$contrasts

# Gráfica de fructosa entre especies
ggplot(azucares_more, aes(spp, fructosa)) +
  geom_boxplot(alpha = 0.3) +
  ## use either geom_point() or geom_jitter()
  geom_point(aes(color = spp), size = 2, alpha = .3, #
             position = position_jitter(seed = 1, width = .1))+
  #stat_summary(geom = "text", fun.data = n_fun)+
  #coord_flip()+
  labs(x = "Especies", y = "Concentración de Fructosa")+
  scale_x_discrete(labels=c("1CF" = "CF", "2CPF" = "CPF",
                            "3CPP" = "CPP", "4CAS" = "CAS", "5CAA" = "CAA",
                            "6CM" = "CM"))+
  theme_bw()+
  theme(axis.text = element_text(face = "bold"))+
  scale_color_discrete(name="Especies",
                       labels = c("CF", "CPF", "CPP", "CAS", "CAA", "CM"))+

  annotate("text", x=1, y=1, label= "n = 2", size = 4)+
  annotate("text", x=2, y=1, label= "n = 9", size = 4)+
  annotate("text", x=3, y=1, label= "n = 9", size = 4)+
  annotate("text", x=4, y=1, label= "n = 22", size = 4)+
  annotate("text", x=5, y=1, label= "n = 19", size = 4)+
  annotate("text", x=6, y=1, label= "n = 22", size = 4)+
  annotate("text", x=1, y=15, label= "a", size = 5)+
  annotate("text", x=2, y=6, label= "b", size = 5)+
  annotate("text", x=3, y=11, label= "a", size = 5)+
  annotate("text", x=4, y=15.5, label= "a", size = 5)+
  annotate("text", x=5, y=16, label= "a", size = 5)+
  annotate("text", x=6, y=15, label= "a", size = 5)

#### GLUCOSA #####
# media +- error estandar de la concentración de Glucosa por especie
azucares_more %>%
  group_by(condicion, sexo) %>%
  dplyr::summarize(glucosa2 = mean(glucosa), glucosa_es = es(glucosa),
                   n=n())
# Prueba de normalidad
shapiro.test(azucares_more$glucosa)
shapiro.test(log(azucares_more$glucosa))
# ANOVA del porcentaje de glucosa entre especies
glu = aov(log(glucosa) ~ condicion*sexo, data = azucares_more)
summary(glu)
# comparaciones multiples
compa_glu = TukeyHSD(glu)
compa_glu$spp
azu3 = multcompLetters4(glu, compa_glu)
#write.table(compa_glu$spp, file = "compa_glu_spp.csv")

# Gráfica de glucosa entre especies
ggplot(azucares_more, aes(spp, glucosa)) +
  geom_boxplot(alpha = 0.3) +
  ## use either geom_point() or geom_jitter()
  geom_point(aes(color = spp), size = 2, alpha = .3, #
             position = position_jitter(seed = 1, width = .1))+
  #stat_summary(geom = "text", fun.data = n_fun)+
  #coord_flip()+
  labs(x = "Especies", y = "Concentración de Fructosa")+
  scale_x_discrete(labels=c("1CF" = "CF", "2CPF" = "CPF",
                            "3CPP" = "CPP", "4CAS" = "CAS", "5CAA" = "CAA",
                            "6CM" = "CM"))+
  theme_bw()+
  theme(axis.text = element_text(face = "bold"))+
  scale_color_discrete(name="Especies",
                       labels = c("CF", "CPF", "CPP", "CAS", "CAA", "CM"))+
  
  annotate("text", x=1, y=1, label= "n = 2", size = 4)+
  annotate("text", x=2, y=1, label= "n = 9", size = 4)+
  annotate("text", x=3, y=1, label= "n = 9", size = 4)+
  annotate("text", x=4, y=1, label= "n = 22", size = 4)+
  annotate("text", x=5, y=1, label= "n = 19", size = 4)+
  annotate("text", x=6, y=1, label= "n = 22", size = 4)+
  annotate("text", x=1, y=14.5, label= "ab", size = 5)+
  annotate("text", x=2, y=7, label= "b", size = 5)+
  annotate("text", x=3, y=11, label= "ab", size = 5)+
  annotate("text", x=4, y=12, label= "ab", size = 5)+
  annotate("text", x=5, y=10, label= "a", size = 5)+
  annotate("text", x=6, y=15, label= "ab", size = 5)

# Modelo lineal generalizado del porcentaje de Glucosa entre las especies,
# el sexo y el tratamiento
glu = glm(glucosa ~ spp+sexo+trat, data = azucares_more, 
          family = gaussian(link = "log"))
summary(glu)  
anova(glu, test = "Chisq")
Anova(glu)

#### SACAROSA #######
#### media +- error estandar de la concentración de Glucosa por especie
azucares_more %>%
  group_by(condicion, sexo) %>%
  dplyr::summarize(sacarosa2 = mean(sacarosa), sacarosa_es = es(sacarosa),
                   n=n())
# Prueba de normalidad
shapiro.test(azucares_more$sacarosa)
# ANOVA del porcentaje de sacarosa
saca = aov(sacarosa ~ condicion*sexo, data = azucares_more)
summary(saca)
# comparaciones multiples
compa_saca = TukeyHSD(saca)
compa_saca$spp
azu4 = multcompLetters4(saca, compa_saca)
#write.table(compa_saca$spp, file = "compa_saca_spp.csv")

# Gráfica de sacarosa entre especies
ggplot(azucares_more, aes(spp, sacarosa)) +
  geom_boxplot(alpha = 0.3) +
  ## use either geom_point() or geom_jitter()
  geom_point(aes(color = spp), size = 2, alpha = .3, #
             position = position_jitter(seed = 1, width = .1))+
  #stat_summary(geom = "text", fun.data = n_fun)+
  #coord_flip()+
  labs(x = "Especies", y = "Concentración de Fructosa")+
  scale_x_discrete(labels=c("1CF" = "CF", "2CPF" = "CPF",
                            "3CPP" = "CPP", "4CAS" = "CAS", "5CAA" = "CAA",
                            "6CM" = "CM"))+
  theme_bw()+
  theme(axis.text = element_text(face = "bold"))+
  scale_color_discrete(name="Especies",
                       labels = c("CF", "CPF", "CPP", "CAS", "CAA", "CM"))+
  
  annotate("text", x=1, y=2, label= "n = 2", size = 4)+
  annotate("text", x=2, y=2, label= "n = 9", size = 4)+
  annotate("text", x=3, y=2, label= "n = 9", size = 4)+
  annotate("text", x=4, y=2, label= "n = 22", size = 4)+
  annotate("text", x=5, y=2, label= "n = 19", size = 4)+
  annotate("text", x=6, y=2, label= "n = 22", size = 4)+
  annotate("text", x=1, y=30, label= "bc", size = 5)+
  annotate("text", x=2, y=60, label= "a", size = 5)+
  annotate("text", x=3, y=40, label= "bc", size = 5)+
  anno+tate("text", x=4, y=35, label= "c", size = 5)+
  annotate("text", x=5, y=49, label= "bc", size = 5)+
  annotate("text", x=6, y=61, label= "b", size = 5)

# Modelo lineal generalizado del porcentaje de Sacarosa entre las especies,
# el sexo y el tratamiento
sac = glm(sacarosa ~ spp+sexo+trat, data = azucares_more,
          family = gaussian(link = "identity"))
summary(sac)  
anova(sac, test = "Chisq")
Anova(sac)

compa_sac= emmeans(sac, specs = pairwise ~ spp)
compa_sac$contrasts

#### Tabla de los porcentajes de azucares modificada ####
azucares_2 = read.csv("azucares_2022-2.csv", header = T)
head(azucares_2)
str(azucares_2)
# Extrayendo los valores de morelia
azucares_2_more = subset(azucares_2, sitio == "morelia")
str(azucares_2_more)
head(azucares_2_more)
sort(azucares_2_more$por_azu)

# Gráfica de azucares entre especies
sugars <- c("Fructosa", "Glucosa", "Sacarosa")
names(sugars) <- c("fructosa", "glucosa", "sacarosa")  

ggplot(azucares_2_more, aes(spp, por_azu, fill = condicion)) +
  geom_boxplot() +
  ## use either geom_point() or geom_jitter()
  geom_point(size = 2, alpha = .3, #
             position = position_jitter(seed = 1, width = .1))+
  #stat_summary(geom = "text", fun.data = n_fun)
  #coord_flip()+
    facet_wrap( .~azucares, ncol=3, scales="fixed",
                labeller = labeller(azucares = sugars))+
  labs(x = "Especies", y = "Concentración de azúcar (w/v)")+
  scale_x_discrete(labels=c("1CF" = "CF", "2CPF" = "CPF",
                            "3CPP" = "CPP", "4CAS" = "CAS", "5CAA" = "CAA",
                            "6CM" = "CM"))+
  theme_bw()+
  theme(axis.text = element_text(face = "bold"))+
  scale_color_discrete(name="Species",
                       labels = c("CF", "CPF", "CPP", "CAS", "CAA", "CM"))+
  scale_fill_manual(values = c("#DE7862FF", "#58A449FF"), name = "Condición",
                    labels = c("Domesticadas", "Silvestres"))

  annotate("text", x=1, y=0, label= "n = 2", size = 2)+
  annotate("text", x=2, y=0, label= "n = 9", size = 2)+
  annotate("text", x=3, y=0, label= "n = 9", size = 2)+
  annotate("text", x=4, y=0, label= "n = 22", size = 2)+
  annotate("text", x=5, y=0, label= "n = 19", size = 2)+
  annotate("text", x=6, y=0, label= "n = 22", size = 2)

ggsave("azucares.jpg", device = "jpg", width = 17.3, height = 23,
       units = "cm", dpi = 300)

#  annotate("text", x=1, y=30, label= "bc", size = 5)+
#  annotate("text", x=2, y=60, label= "a", size = 5)+
#  annotate("text", x=3, y=40, label= "bc", size = 5)+
#  annotate("text", x=4, y=35, label= "c", size = 5)+
#  annotate("text", x=5, y=49, label= "bc", size = 5)+
#  annotate("text", x=6, y=61, label= "b", size = 5)+
#  annotate("text", x=1, y = 32, label = "m", size = 5)
  
# Análisis de los tres azucares entre las especies
hist(azucares_2_more$por_azu)
hist(log(azucares_2_more$por_azu))
shapiro.test(azucares_2_more$por_azu)
shapiro.test(log(azucares_2_more$por_azu))

mod_azu1 = glm(por_azu ~ spp*azucares, data = azucares_2_more,
               family = gaussian(link = "identity"))

summary(mod_azu1)
anova(mod_azu1, test = "Chisq") 
# Comparaciones multiples
compa_azu = emmeans(mod_azu1, specs = pairwise ~ azucares|spp)
compa_azu$contrasts

compa_azu2 = emmeans(mod_azu1, specs = pairwise ~ spp|azucares)
compa_azu2$contrasts

#### AMINOACIDOS DEL NECTAR FLORAL DE CUCURBITA ####
amino = read.csv("concentraciones_aminoacidos.csv", header = TRUE)
str(amino)
head(amino)
tapply(amino$Asp, list(amino$spp,amino$sexo_flor), length)
tapply(amino$Asp, list(amino$ssp, amino$sexo_flor), mean)

# media de cada aminoacido por especie
amino2 = amino %>%
  group_by(spp, sexo_flor)  %>%
  dplyr::summarize(Asp2 = mean(Asp), Glu2 = mean(Glu), Ser2 = mean(Ser),
                   His2 = mean(His), Gly2 = mean(Gly), Thr2 = mean(Thr),
                   Arg2 = mean(Arg), Ala2 = mean(Ala), Tyr2 = mean(Tyr),
                   Cys2 = mean(Cys), Val2 = mean(Val), Met2 = mean(Met),
                   Phe2 = mean(Phe), Ile2 = mean(Ile), Leu2 = mean(Leu),
                   Lys2 = mean(Lys), Pro2 = mean(Pro), n = n())
amino2
write.csv(amino2, file = "amino_mean.csv")
# error estandar de cada aminoacido por especie
amino3 = amino %>%
  group_by(condición, sexo_flor)  %>%
  dplyr::summarize(Asp2 = es(Asp), Glu2 = es(Glu), Ser2 = es(Ser),
                   His2 = es(His), Gly2 = es(Gly), Thr2 = es(Thr),
                   Arg2 = es(Arg), Ala2 = es(Ala), Tyr2 = es(Tyr),
                   Cys2 = es(Cys), Val2 = es(Val), Met2 = es(Met),
                   Phe2 = es(Phe), Ile2 = es(Ile), Leu2 = es(Leu),
                   Lys2 = es(Lys), Pro2 = es(Pro), n = n())

amino3
View(amino3)
amino3 = as.data.frame(amino2)
str(amino3)
write.csv(amino3, file = "amino_condicion.csv")

shapiro.test(amino$Asp)
shapiro.test(amino$Glu)
shapiro.test(amino$Ser)
shapiro.test(amino$His)


tapply(amino$Asp, list(amino$spp,amino$sexo_flor), length)
amino2 = subset(amino, spp != "1CF")
str(amino2)


# Cargando libraries
library(FactoMineR)
library(factoextra)

### PCA de los diferentes aminoacidos
head(amino2)
str(amino2)
pairs(amino2[8:24])
# PCA de caracteres del néctar de flores hembra
# visualizando todas las combinaciones entre las variables
pairs(amino2[8:24], lower.panel = NULL)
# Calculando el PCA para flores hembra
pca4 = prcomp(amino2[8:24], scale = T)
summary(pca4)
# Obteniendo información sobre los eigenvalues
get_eigenvalue(pca4)
# calculando la varianza explicada por cada componente
fviz_eig(pca4, addlabels = T)
# información que podemos obtener de las variables analizadas
var = get_pca_var(pca4); var
# Visualización de las variables
fviz_pca_var(pca4, col.var = "black")

library(corrplot)
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
#fviz_pca_var(pca4, col.var = factor(c("nectar","nectar","nectar",
#                                      "nectar","nectar","aminoacid","aminoacid",
#                                      "aminoacid","aminoacid","aminoacid",
#                                      "aminoacid","aminoacid","aminoacid",
#                                      "aminoacid","aminoacid","aminoacid",
#                                      "aminoacid","aminoacid","aminoacid",
#                                      "aminoacid","aminoacid","aminoacid")), 
#             palette = c("#0073C2FF", "#EFC000FF"),
#             legend.title = "Cluster")

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
             fill.ind = amino2$spp, # color by groups
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
             col.ind = amino2$spp, # color by groups
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
library(vegan)
str(amino2)
amino2[8:24]
dune8 = amino[8:24]
# Calculando distancia euclidiana
set.seed(0)
# permanova
dune.div8 <- adonis2(dune8 ~ spp*sexo_flor, by="terms", data = amino,
                     permutations = 999, method="euclidean")
dune.div8

# calculando PERMANOVA por especie
dune.div8 <- adonis2(dune8 ~ sexo_flor, data = amino,
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







# cargando paquete factoextra
library(factoextra)
# Top 10 variables que más contribuyen a PC1
fviz_contrib(pca4, choice = "var", axes = 1, top = 10)
fviz_contrib(pca4, choice = "var", axes = 2, top = 10)
# plot
fviz_pca_biplot(pca4, geom.ind = "point",
                axes = c(1,2), pointsize = 2, title = "", geom.var = c("",""),
)

# cargando paquete para graficar PCA
library(ggfortify)
autoplot(pca4)
pca_ami1 = autoplot(pca4, data = amino, colour = "condición", size = 2)+
  xlab("PC 1 (55.7%)") +
  ylab("PC 2 (11%)") +
  scale_color_discrete(name = "Condición",
                       labels = c("Domesticadas", "Silvestres"))+
  theme_bw()
pca_ami1
# aminoacidos por sexo
pca_ami2 = autoplot(pca4, data = amino, colour = "sexo_flor", size = 2)+
  xlab("PC 1 (55.7%)") +
  ylab("PC 2 (11%)")+
  scale_color_discrete(name = "Sexo floral",
                       labels = c("Hembras", "Machos"))+
  theme_bw()
pca_ami2

# Normalidad multivariada
library(MVN)
# test de normalidad
# test de Mardia en MVN
result <- mvn(data = amino[8:24], mvnTest = "mardia")
result$multivariateNormality
# Henze-Zirkler's MVN test
result <- mvn(data = amino[8:24], mvnTest = "hz")
result$multivariateNormality
# Royston's MVN test
result <- mvn(data = amino[8:24], mvnTest = "royston")
result$multivariateNormality
# Doornik-Hansen's MVN test
result <- mvn(data = amino[8:24], mvnTest = "dh")
result$multivariateNormality
# Energy test
result <- mvn(data = amino[8:24], mvnTest = "energy")
result$multivariateNormality






#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
ca.condicion3 = amino[,3]
ca.species3 = amino[, 4]
ca.sex3 = amino[,6]

#
ggbiplot(pca3)
# PCA por condición
ggbiplot(pca3, obs.scale = 1, var.scale = 1, 
         groups = ca.condicion3, ellipse = F, 
         circle = F) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom')
# PCA por especie
ggbiplot(pca3, obs.scale = 1, var.scale = 1, 
              groups = ca.species3, ellipse = F, 
              circle = F) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', 
               legend.position = 'bottom')
# PCA por sexo
ggbiplot(pca3, obs.scale = 1, var.scale = 1, 
         groups = ca.sex3, ellipse = F, 
         circle = F) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom')

# PCoA con vegan
library(vegan)
cucu_dist = dist(amino[,c(8:24)])
cucu_PCoA <- wcmdscale(d = cucu_dist, eig = TRUE)
cucu_PCoA
cucu_PCoA$points
# gráfica del PCoA
ggplot(data = data.frame(cucu_PCoA$points),
       aes(x = Dim1, y = Dim2)) +
  geom_point() +
  theme_bw()

# PCoA por condición
pcoa <- cmdscale(cucu_dist, eig = TRUE, add = TRUE)
#convert pcoa results into data frame that can be plotted
pcoa_df <- data.frame(pcoa$points)
colnames(pcoa_df) <- c("PCo1", "PCo2")
pcoa_df$condi <- factor(amino$condición) #add group of interest,
# mine was Morphospecies in the data frame cal_fem_data2
calf1 <- ggplot(pcoa_df, aes(x = PCo1, y = PCo2, color = condi)) + 
  geom_point(size = 1) +
  xlab("PCo1") +
  ylab("PCo2") + 
  ggtitle("") +
  theme_classic()+
scale_color_discrete(name="Condition",
                     labels = c("Domesticated", "Wild"))

calf1
# PCoA agrupado por sexo floral
pcoa1 <- cmdscale(cucu_dist, eig = TRUE, add = TRUE)
#convert pcoa results into data frame that can be plotted
pcoa1_df <- data.frame(pcoa1$points)
colnames(pcoa1_df) <- c("PCo1", "PCo2")
pcoa_df$sexo <- factor(amino$sexo_flor) #add group of interest,
# mine was Morphospecies in the data frame cal_fem_data2
calf2 <- ggplot(pcoa_df, aes(x = PCo1, y = PCo2, color = sexo)) + 
  geom_point(size = 1) +
  xlab("PCo1") +
  ylab("PCo2") + 
  ggtitle("") +
  theme_classic()+
  scale_color_discrete(name = "Floral sex", labels = c("Female", "Male"))
calf2

# haciendo panel de las dos gráficas
library(ggpubr)
# juntado calf1 y calf2
ggarrange(calf1, calf2, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)
# exportando plot
ggsave("amino_sex.jpg", device = "jpg", width = 11.5, height = 11.5,
       units = "cm", dpi = 300)





### PERMANOVA DE LOS DIFERENTES AMINOACIDOS
library(vegan)
head(amino)
amino$tratamiento
dune5 = amino[8:24]
# Calculando distancia euclidiana
dune.dist5 <- vegdist(dune5, method="euclidean")

# calculando PERMANOVA por condición
dune.div5 <- adonis2(dune5 ~ condición*sexo_flor, data = amino,
                     permutations = 999, method="euclidean")
dune.div5

# calculando PERMANOVA por especie
dune.div6 <- adonis2(dune5 ~ sexo_flor, data = amino,
                    permutations = 999, method="euclidean")
dune.div6

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

#### NMDS con los datos de aminoacidos ####
library(vegan)
str(amino)
head(amino)

set.seed(0)#Para que los resultados no se brinden aleatorios
nmds1 <- metaMDS(amino[,8:24])
plot(nmds1)
plot(nmds1, type = "t")
stressplot(nmds1)

coordenadas <- as.data.frame(scores(nmds1)$sites)
coordenadas

#Le añadimos a las coordenadas una columna con los tramos:
coordenadas$spp = amino$spp
coordenadas$sexo_flor = amino$sexo_flor
head(coordenadas)
# plot
gr2 <- ggplot(coordenadas, aes(x = NMDS1, y = NMDS2))+ 
  geom_point(size = 4, aes(colour = spp))+
  theme_bw()+
  scale_color_discrete(name = "Species", 
                       labels =c("CF","CPF","CPP","CAS","CAA","CM") );gr2

#+geom_text(hjust=0.5, vjust=1.5, label=datos$sample)
gr3 <- ggplot(coordenadas, aes(x = NMDS1, y = NMDS2))+ 
  geom_point(size = 4, aes(colour = sexo_flor))+
  theme_bw()+
  scale_color_discrete(name = "Floral sex", 
                       labels =c("Female","Male") );gr3
#gr4 <- ggplot(coordenadas, aes(x = NMDS1, y = NMDS2))+ 
#  geom_point(size = 4, aes( shape = sexo_flor, colour = spp)); gr4
# haciendo panel de las dos gráficas
library(ggpubr)
# juntado calf1 y calf2
ggarrange(gr2, gr3, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)
# exportando plot
ggsave("amino_sex4.jpg", device = "jpg", width = 23, height = 23,
       units = "cm", dpi = 300)


# make community matrix - extract columns with abundance information,
# turn data frame into matrix
com = amino[,8:ncol(amino)]
m_com = as.matrix(com)
condicion = c("bosque", "cultivo")

# ANOSIM
ano = anosim(m_com, amino$spp, distance = "bray", permutations = 9999)
ano

ano1 = anosim(m_com, amino$sexo_flor, distance = "bray", permutations = 9999)
ano1

####### GLM de cada aminoacido por separado #####
str(amino)
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

### Asp
tapply(amino$Asp, list(amino$spp, amino$sexo_flor, amino$tratamiento),
       mean)
hist(amino$Asp)
shapiro.test(amino$Asp)
shapiro.test(log(amino$Asp))
# Asp
mod1_aa1 = glm(Asp ~ condición, data = amino,
               family = Gamma)
summary(mod1_aa1)
anova(mod1_aa1)
Anova(mod1_aa1)
# comparaciones multiples
aa1 = emmeans(mod1_aa1, specs = pairwise ~ sexo_flor|spp)
aa1$contrasts
# GLM de Asp
mod2_aa1 = glm(log(Asp) ~ spp+sexo_flor, data = amino, 
               family = gaussian(link = "identity")) # mejor modelo
summary(mod2_aa1)
anova(mod2_aa1, test = "Chisq")
Anova(mod2_aa1, type = )
# comparaciones multiples
aa1_1 = emmeans(mod2_aa1, specs = pairwise ~ sexo_flor | spp)
aa1_1$contrasts
#write.csv(aa1_1$contrasts, file = "Asp_compa.csv")
aa1_2 = emmeans(mod2_aa1, specs = pairwise ~ spp)
aa1_2$contrasts

#### Glu
mod1_aa1 = glm(Glu ~ spp*sexo_flor*tratamiento, data = amino,
               family = Gamma(link = "log"))
summary(mod1_aa1)
anova(mod1_aa1, test = "Chisq")
# comparaciones multiples
aa1 = emmeans(mod1_aa1, specs = pairwise ~ sexo_flor|spp)
aa1$contrasts
#
mod2_aa1 = glm(log(Glu) ~ spp*sexo_flor, data = amino, #*tratamiento
               family = gaussian(link = "identity"))
summary(mod2_aa1)
anova(mod2_aa1, test = "Chisq")
# comparaciones multiples
aa1_2 = emmeans(mod2_aa1, specs = pairwise ~ sexo_flor | spp)
aa1_2$contrasts
write.csv(aa1_2$contrasts, file = "Glu_compa.csv")
aa1_21 = emmeans(mod2_aa1, specs = pairwise ~ spp | sexo_flor)
aa1_21$contrasts
##### Ser
mod1_aa1 = glm(Ser ~ spp*sexo_flor*tratamiento, data = amino,
               family = Gamma(link = "log"))
summary(mod1_aa1)
anova(mod1_aa1, test = "Chisq")
# comparaciones multiples
aa1 = emmeans(mod1_aa1, specs = pairwise ~ sexo_flor|spp)
aa1$contrasts
#
mod2_aa1 = glm(log(Ser) ~ spp*sexo_flor, data = amino, #*tratamiento
               family = gaussian(link = "identity"))
summary(mod2_aa1)
anova(mod2_aa1, test = "Chisq")
# comparaciones multiples
aa1_3 = emmeans(mod2_aa1, specs = pairwise ~ sexo_flor | spp)
aa1_3$contrasts
write.csv(aa1_3$contrasts, file = "Ser_compa.csv")
aa1_31 = emmeans(mod2_aa1, specs = pairwise ~ spp | sexo_flor)
aa1_31$contrasts

### His
str(amino)
amino$His
log(log(amino$His))
his2 = subset(amino, His != 0)
head(his2)
str(his2)
tapply(his2$His, his2$spp, length)
tapply(his2$His, his2$spp, mean)
tapply(his2$His, his2$spp, es)
# His
mod1_aa1 = glm(His ~ spp*sexo_flor*tratamiento, data = amino,
               family = Gamma(link = "log"))
summary(mod1_aa1)
anova(mod1_aa1, test = "Chisq")
# comparaciones multiples
aa1 = emmeans(mod1_aa1, specs = pairwise ~ sexo_flor|spp)
aa1$contrasts
#
mod2_aa1 = glm(log(His) ~ spp*sexo_flor, data = his2, #*tratamiento
               family = gaussian(link = "identity"))
summary(mod2_aa1)
anova(mod2_aa1, test = "Chisq")

#
mod2_aa1 = glm(log(His) ~ spp, data = his2, #*tratamiento
               family = gaussian(link = "identity"))
summary(mod2_aa1)
anova(mod2_aa1, test = "Chisq")

# comparaciones multiples
boxplot(his2$His ~ his2$sexo_flor+his2$spp)
boxplot(log(his2$His) ~ his2$sexo_flor+his2$spp)
#
boxplot(his2$His ~ his2$spp)
boxplot(log(his2$His) ~ his2$spp)
#
aa1_4 = emmeans(mod2_aa1, specs = pairwise ~ sexo_flor | spp)
aa1_4$contrasts
#write.csv(aa1_4$contrasts, file = "His_compa.csv")
# por especie
aa1_4_1 = emmeans(mod2_aa1, specs = pairwise ~ spp) 
aa1_4_1
#write.csv(aa1_4_1$contrasts, file = "his_compa_spp2.csv")
aa1_41 = emmeans(mod2_aa1, specs = pairwise ~ spp | sexo_flor)
aa1_41$contrasts
#write.csv(aa1_41$contrasts, file = "his_compa_sppsex.csv")

### plot de His
ggplot(his2, aes(x = spp, y = His)) +
  geom_boxplot(alpha = 0.3) +
  ## use either geom_point() or geom_jitter()
  geom_point(aes(color = spp), size = 2, alpha = .2, #
             position = position_jitter(seed = 1, width = .1))+
  #stat_summary(geom = "text", fun.data = n_fun)+
  #coord_flip()+
  labs(x = "Especies", y = "Concentración de Histidina (μg/ml)")+
  scale_x_discrete(labels=c("1CF" = "CF", "2CPF" = "CPF",
                            "3CPP" = "CPP", "4CAS" = "CAS", "5CAA" = "CAA",
                            "6CM" = "CM"))+
  theme_bw()+
  theme(axis.text = element_text(face = "bold"))+
  scale_color_discrete(name="Especies",
                       labels = c("CF", "CPF", "CPP", "CAS", "CAA", "CM"))+
  annotate("text", x=1, y=-0.5, label= "n = 2", size = 4)+
  annotate("text", x=2, y=-0.5, label= "n = 10", size = 4)+
  annotate("text", x=3, y=-0.5, label= "n = 9", size = 4)+
  annotate("text", x=4, y=-0.5, label= "n = 16", size = 4)+
  annotate("text", x=5, y=-0.5, label= "n = 17", size = 4)+
  annotate("text", x=6, y=-0.5, label= "n = 13", size = 4)+
  annotate("text", x=1, y=30, label= "ab", size = 5)+
  annotate("text", x=2, y=15, label= "bc", size = 5)+
  annotate("text", x=3, y=16.5, label= "ab", size = 5)+
  annotate("text", x=4, y=46.5, label= "a", size = 5)+
  annotate("text", x=5, y=32, label= "ab", size = 5)+
  annotate("text", x=6, y=19, label= "ab", size = 5)

#### Gly
str(amino)
amino$Gly
mod1_aa1 = glm(Gly ~ spp*sexo_flor*tratamiento, data = amino,
               family = Gamma(link = "log"))
summary(mod1_aa1)
anova(mod1_aa1, test = "Chisq")
# comparaciones multiples
aa1 = emmeans(mod1_aa1, specs = pairwise ~ sexo_flor|spp)
aa1$contrasts
#
mod2_aa1 = glm(log(Gly) ~ spp*sexo_flor, data = amino, #*tratamiento
               family = gaussian(link = "identity"))
summary(mod2_aa1)
anova(mod2_aa1, test = "Chisq")
# comparaciones multiples
aa1_5 = emmeans(mod2_aa1, specs = pairwise ~ sexo_flor | spp)
aa1_5$contrasts
write.csv(aa1_5$contrasts, file = "Gly_compa.csv")
boxplot(log(amino$Gly) ~ amino$sexo_flor+amino$spp)
aa1_51 = emmeans(mod2_aa1, specs = pairwise ~ spp | sexo_flor)
aa1_51$contrasts

#### Thr
str(amino)
amino$Thr
thr2 = subset(amino, Thr != 0);str(thr2)
# Glu
mod1_aa1 = glm(Thr ~ spp*sexo_flor*tratamiento, data = amino,
               family = Gamma(link = "log"))
summary(mod1_aa1)
anova(mod1_aa1, test = "Chisq")
# comparaciones multiples
aa1 = emmeans(mod1_aa1, specs = pairwise ~ sexo_flor|spp)
aa1$contrasts
# modelo 2
mod2_aa1 = glm(log(Thr) ~ spp*sexo_flor, data = thr2, #*tratamiento
               family = gaussian(link = "identity"))
summary(mod2_aa1)
anova(mod2_aa1, test = "Chisq")
# comparaciones multiples
aa1_6 = emmeans(mod2_aa1, specs = pairwise ~ sexo_flor | spp)
aa1_6$contrasts
write.csv(aa1_6$contrasts, file = "Thr_compa.csv")
boxplot(thr2$Thr ~ thr2$sexo_flor+thr2$spp)
aa1_61 = emmeans(mod2_aa1, specs = pairwise ~ spp | sexo_flor)
aa1_61$contrasts

#### Arg
str(amino)
amino$Arg
mod1_aa1 = glm(Arg ~ spp*sexo_flor*tratamiento, data = amino,
               family = Gamma(link = "log"))
summary(mod1_aa1)
anova(mod1_aa1, test = "Chisq")
# comparaciones multiples
aa1 = emmeans(mod1_aa1, specs = pairwise ~ sexo_flor|spp)
aa1$contrasts
#
mod2_aa1 = glm(log(Arg) ~ spp*sexo_flor, data = amino, #*tratamiento
               family = gaussian(link = "identity"))
summary(mod2_aa1)
anova(mod2_aa1, test = "Chisq")
# comparaciones multiples
aa1_7 = emmeans(mod2_aa1, specs = pairwise ~ sexo_flor | spp)
aa1_7$contrasts
write.csv(aa1_7$contrasts, file = "Arg_compa.csv")
aa1_71 = emmeans(mod2_aa1, specs = pairwise ~ spp | sexo_flor)
aa1_71$contrasts
boxplot(amino$Arg[amino$sexo_flor=="M"] ~ amino$spp[amino$sexo_flor=="M"])
write.csv(aa1_71$contrasts, file = "arg_compa_sppsexo.csv")

#### Ala
str(amino)
amino$Ala
mod1_aa1 = glm(Ala ~ spp*sexo_flor*tratamiento, data = amino,
               family = Gamma(link = "log"))
summary(mod1_aa1)
anova(mod1_aa1, test = "Chisq")
# comparaciones multiples
aa1 = emmeans(mod1_aa1, specs = pairwise ~ sexo_flor|spp)
aa1$contrasts
#
mod2_aa1 = glm(log(Ala) ~ spp*sexo_flor, data = amino, #*tratamiento
               family = gaussian(link = "identity"))
summary(mod2_aa1)
anova(mod2_aa1, test = "Chisq")
# comparaciones multiples
aa1_8 = emmeans(mod2_aa1, specs = pairwise ~ sexo_flor | spp)
aa1_8$contrasts
write.csv(aa1_8$contrasts, file = "Ala_compa.csv")
aa1_81 = emmeans(mod2_aa1, specs = pairwise ~ spp | sexo_flor)
aa1_81$contrasts
write.csv(aa1_81$contrasts, file = "ala_compa_sppsexo.csv")

##### Tyr
str(amino)
amino$Tyr
tyr2 = subset(amino, Tyr != 0);str(tyr2)
mod1_aa1 = glm(Tyr ~ spp*sexo_flor*tratamiento, data = amino,
               family = Gamma(link = "log"))
summary(mod1_aa1)
anova(mod1_aa1, test = "Chisq")
# comparaciones multiples
aa1 = emmeans(mod1_aa1, specs = pairwise ~ sexo_flor|spp)
aa1$contrasts
#
mod2_aa1 = glm(log(Tyr) ~ spp*sexo_flor, data = tyr2, #*tratamiento
               family = gaussian(link = "identity"))
summary(mod2_aa1)
anova(mod2_aa1, test = "Chisq")
# comparaciones multiples
aa1_9 = emmeans(mod2_aa1, specs = pairwise ~ sexo_flor | spp)
aa1_9$contrasts
write.csv(aa1_9$contrasts, file = "Tyr_compa.csv")
aa1_91 = emmeans(mod2_aa1, specs = pairwise ~ spp | sexo_flor)
aa1_91$contrasts
boxplot(tyr2$Tyr ~ tyr2$spp)

#### Cys
str(amino)
amino$Cys
mod1_aa1 = glm(Cys ~ spp*sexo_flor*tratamiento, data = amino,
               family = Gamma(link = "log"))
summary(mod1_aa1)
anova(mod1_aa1, test = "Chisq")
# comparaciones multiples
aa1 = emmeans(mod1_aa1, specs = pairwise ~ sexo_flor|spp)
aa1$contrasts
#
mod2_aa1 = glm(log(Cys) ~ spp+sexo_flor, data = amino, #*tratamiento
               family = gaussian(link = "identity"))
summary(mod2_aa1)
anova(mod2_aa1, test = "Chisq")
# comparaciones multiples
aa1_10 = emmeans(mod2_aa1, specs = pairwise ~ sexo_flor | spp)
aa1_10$contrasts
#write.csv(aa1_10$contrasts, file = "Cys_compa.csv")
aa1_10_1 = emmeans(mod2_aa1, specs = pairwise ~ spp)
aa1_10_1$contrasts


#Cld = multcompLetters4(mod2_aa1, aa1_10_1)
#Cld
#write.csv(aa1_10_1$contrasts, file = "Cys_compa_spp.csv")
boxplot(log(amino$Cys) ~ amino$spp)
aa1_101 = emmeans(mod2_aa1, specs = pairwise ~ spp | sexo_flor)
aa1_101$contrasts
#write.csv(aa1_101$contrasts, file = "Cys_compa_sppsexo.csv")

# plot 
ggplot(amino, aes(x = spp, y = Cys)) +
  geom_boxplot(alpha = 0.3) +
  ## use either geom_point() or geom_jitter()
  geom_point(aes(color = spp), size = 2, alpha = .2, #
             position = position_jitter(seed = 1, width = .1))+
  #stat_summary(geom = "text", fun.data = n_fun)+
  #coord_flip()+
  labs(x = "Especies", y = "Concentración de Cisteina (μg/ml)")+
  scale_x_discrete(labels=c("1CF" = "CF", "2CPF" = "CPF",
                            "3CPP" = "CPP", "4CAS" = "CAS", "5CAA" = "CAA",
                            "6CM" = "CM"))+
  theme_bw()+
  theme(axis.text = element_text(face = "bold"))+
  scale_color_discrete(name="Especies",
                       labels = c("CF", "CPF", "CPP", "CAS", "CAA", "CM"))+
  annotate("text", x=1, y=0, label= "n = 2", size = 4)+
  annotate("text", x=2, y=0, label= "n = 10", size = 4)+
  annotate("text", x=3, y=0, label= "n = 9", size = 4)+
  annotate("text", x=4, y=0, label= "n = 16", size = 4)+
  annotate("text", x=5, y=0, label= "n = 17", size = 4)+
  annotate("text", x=6, y=0, label= "n = 13", size = 4)+
  annotate("text", x=1, y=4.4, label= "ab", size = 5)+
  annotate("text", x=2, y=3, label= "ab", size = 5)+
  annotate("text", x=3, y=7.7, label= "a", size = 5)+
  annotate("text", x=4, y=3, label= "bc", size = 5)+
  annotate("text", x=5, y=6, label= "a", size = 5)+
  annotate("text", x=6, y=4, label= "ab", size = 5)

#### Val
str(amino)
amino$Val
mod1_aa1 = glm(Val ~ spp*sexo_flor*tratamiento, data = amino,
               family = Gamma(link = "log"))
summary(mod1_aa1)
anova(mod1_aa1, test = "Chisq")
# comparaciones multiples
aa1 = emmeans(mod1_aa1, specs = pairwise ~ sexo_flor|spp)
aa1$contrasts
#
mod2_aa1 = glm(log(Val) ~ spp*sexo_flor, data = amino, #*tratamiento
               family = gaussian(link = "identity"))
summary(mod2_aa1)
anova(mod2_aa1, test = "Chisq")
# comparaciones multiples
aa1_11 = emmeans(mod2_aa1, specs = pairwise ~ sexo_flor | spp)
aa1_11$contrasts
write.csv(aa1_11$contrasts, file = "Val_compa.csv")
aa1_11 = emmeans(mod2_aa1, specs = pairwise ~ spp)
aa1_11$contrasts
aa1_111 = emmeans(mod2_aa1, specs = pairwise ~ spp | sexo_flor)
aa1_111$contrasts

#### Met
str(amino)
amino$Met
mod1_aa1 = glm(Met ~ spp*sexo_flor*tratamiento, data = amino,
               family = Gamma(link = "log"))
summary(mod1_aa1)
anova(mod1_aa1, test = "Chisq")
# comparaciones multiples
aa1 = emmeans(mod1_aa1, specs = pairwise ~ sexo_flor|spp)
aa1$contrasts
#
mod2_aa1 = glm(log(Met) ~ spp*sexo_flor, data = amino, #*tratamiento
               family = gaussian(link = "identity"))
summary(mod2_aa1)
anova(mod2_aa1, test = "Chisq")
# comparaciones multiples
aa1_12 = emmeans(mod2_aa1, specs = pairwise ~ sexo_flor | spp)
aa1_12$contrasts
write.csv(aa1_12$contrasts, file = "Met_compa.csv")
aa1_121 = emmeans(mod2_aa1, specs = pairwise ~ spp | sexo_flor)
aa1_121$contrasts

#### Phe
str(amino)
amino$Phe
mod1_aa1 = glm(Phe ~ spp*sexo_flor, data = amino,
               family = Gamma(link = "log"))
summary(mod1_aa1)
anova(mod1_aa1, test = "Chisq")
# comparaciones multiples
aa1 = emmeans(mod1_aa1, specs = pairwise ~ sexo_flor|spp)
aa1$contrasts
#
mod2_aa1 = glm(log(Phe) ~ spp+sexo_flor, data = amino, #*tratamiento
               family = gaussian(link = "identity"))
summary(mod2_aa1)
anova(mod2_aa1, test = "Chisq")
# comparaciones multiples
aa1_13 = emmeans(mod2_aa1, specs = pairwise ~ spp)
aa1_13$contrasts
aa1_13 = emmeans(mod2_aa1, specs = pairwise ~ sexo_flor | spp)
aa1_13$contrasts
write.csv(aa1_13$contrasts, file = "Phe_compa.csv")
aa1_131 = emmeans(mod2_aa1, specs = pairwise ~ spp | sexo_flor)
aa1_131$contrasts
## plot
ggplot(amino, aes(x = spp, y = Phe)) +
  geom_boxplot(alpha = 0.3) +
  ## use either geom_point() or geom_jitter()
  geom_point(aes(color = spp), size = 2, alpha = .2, #
             position = position_jitter(seed = 1, width = .1))+
  #stat_summary(geom = "text", fun.data = n_fun)+
  #coord_flip()+
  labs(x = "Especies", y = "Concentración de Fenilalanina (μg/ml)")+
  scale_x_discrete(labels=c("1CF" = "CF", "2CPF" = "CPF",
                            "3CPP" = "CPP", "4CAS" = "CAS", "5CAA" = "CAA",
                            "6CM" = "CM"))+
  theme_bw()+
  theme(axis.text = element_text(face = "bold"))+
  scale_color_discrete(name="Especies",
                       labels = c("CF", "CPF", "CPP", "CAS", "CAA", "CM"))+
  annotate("text", x=1, y=0, label= "n = 2", size = 4)+
  annotate("text", x=2, y=0, label= "n = 10", size = 4)+
  annotate("text", x=3, y=0, label= "n = 9", size = 4)+
  annotate("text", x=4, y=0, label= "n = 16", size = 4)+
  annotate("text", x=5, y=0, label= "n = 17", size = 4)+
  annotate("text", x=6, y=0, label= "n = 13", size = 4)+
  annotate("text", x=1, y=13, label= "a", size = 5)+
  annotate("text", x=2, y=9, label= "bc", size = 5)+
  annotate("text", x=3, y=10, label= "ab", size = 5)+
  annotate("text", x=4, y=15, label= "ab", size = 5)+
  annotate("text", x=5, y=11, label= "ab", size = 5)+
  annotate("text", x=6, y=10, label= "ab", size = 5)

#### Ile
str(amino)
amino$Ile
mod1_aa1 = glm(Ile ~ spp*sexo_flor*tratamiento, data = amino,
               family = Gamma(link = "log"))
summary(mod1_aa1)
anova(mod1_aa1, test = "Chisq")
# comparaciones multiples
aa1 = emmeans(mod1_aa1, specs = pairwise ~ sexo_flor|spp)
aa1$contrasts
#
mod2_aa1 = glm(log(Ile) ~ spp*sexo_flor, data = amino, #*tratamiento
               family = gaussian(link = "identity"))
summary(mod2_aa1)
anova(mod2_aa1, test = "Chisq")
# comparaciones multiples
aa1_14 = emmeans(mod2_aa1, specs = pairwise ~ sexo_flor | spp)
aa1_14$contrasts
write.csv(aa1_14$contrasts, file = "Ile_compa.csv")
aa1_141 = emmeans(mod2_aa1, specs = pairwise ~ spp | sexo_flor)
aa1_141$contrasts

#### Leu
str(amino)
amino$Leu
mod1_aa1 = glm(Leu ~ spp*sexo_flor*tratamiento, data = amino,
               family = Gamma(link = "log"))
summary(mod1_aa1)
anova(mod1_aa1, test = "Chisq")
# comparaciones multiples
aa1 = emmeans(mod1_aa1, specs = pairwise ~ sexo_flor|spp)
aa1$contrasts
#
mod2_aa1 = glm(log(Leu) ~ spp*sexo_flor, data = amino, #*tratamiento
               family = gaussian(link = "identity"))
summary(mod2_aa1)
anova(mod2_aa1, test = "Chisq")
# comparaciones multiples
aa1_15 = emmeans(mod2_aa1, specs = pairwise ~ sexo_flor | spp)
aa1_15$contrasts
write.csv(aa1_15$contrasts, file = "Leu_compa.csv")
aa1_151 = emmeans(mod2_aa1, specs = pairwise ~ spp | sexo_flor)
aa1_151$contrasts

#### Lys
str(amino)
amino$Lys
mod1_aa1 = glm(Lys ~ spp*sexo_flor*tratamiento, data = amino,
               family = Gamma(link = "log"))
summary(mod1_aa1)
anova(mod1_aa1, test = "Chisq")
# comparaciones multiples
aa1 = emmeans(mod1_aa1, specs = pairwise ~ sexo_flor|spp)
aa1$contrasts
#
mod2_aa1 = glm(log(Lys) ~ spp*sexo_flor, data = amino, #*tratamiento
               family = gaussian(link = "identity"))
summary(mod2_aa1)
anova(mod2_aa1, test = "Chisq")
# comparaciones multiples
aa1_16 = emmeans(mod2_aa1, specs = pairwise ~ sexo_flor | spp)
aa1_16$contrasts
write.csv(aa1_16$contrasts, file = "Lys_compa.csv")
aa1_161 = emmeans(mod2_aa1, specs = pairwise ~ spp | sexo_flor)
aa1_161$contrasts

#### Pro
str(amino)
amino$Pro
mod1_aa1 = glm(Pro ~ spp*sexo_flor*tratamiento, data = amino,
               family = Gamma(link = "log"))
summary(mod1_aa1)
anova(mod1_aa1, test = "Chisq")
# comparaciones multiples
aa1 = emmeans(mod1_aa1, specs = pairwise ~ sexo_flor|spp)
aa1$contrasts
#
mod2_aa1 = glm(log(Pro) ~ spp*sexo_flor, data = amino, #*tratamiento
               family = gaussian(link = "identity"))
summary(mod2_aa1)
anova(mod2_aa1, test = "Chisq")
# comparaciones multiples
aa1_17 = emmeans(mod2_aa1, specs = pairwise ~ sexo_flor | spp)
aa1_17$contrasts
write.csv(aa1_17$contrasts, file = "pro_compa.csv")
aa1_17_1 = emmeans(mod2_aa1, specs = pairwise ~ spp)
aa1_17_1$contrasts
write.csv(aa1_17_1$contrasts, file = "pro_compa_spp.csv")
boxplot(log(amino$Pro) ~ amino$spp)
aa1_171 = emmeans(mod2_aa1, specs = pairwise ~ spp | sexo_flor)
aa1_171$contrasts

#### TABLA MODIFICADA DE AMINOACIDOS ####
amino2 = read.csv("aminoacidos_concen.csv", header = TRUE)
str(amino2)
summary(amino2)
#
amino3 = subset(amino2, amino2$concen != 0)
str(amino3)

head(amino2)
sort(amino2$concen)
#
tapply(amino2$concen, list(amino2$sexo_flor,amino2$aa), mean)



library(hrbrthemes)
# heatmap
# Heatmap 
ggplot(amino3, aes(spp, aa, fill= log(concen))) + 
  geom_tile()+
  scale_fill_gradient(low="white", high="blue")+
  theme_ipsum()+
  labs(x = "Especies", y = "aminoácidos")+
  scale_x_discrete(labels=c("1CF" = "CF", "2CPF" = "CPF",
                            "3CPP" = "CPP", "4CAS" = "CAS", "5CAA" = "CAA",
                            "6CM" = "CM"))+
  theme(axis.text = element_text(face = "bold"))+
  scale_color_discrete(name="Especies")
#
ggplot(amino3, aes(spp, aa, fill= concen)) + 
  geom_tile()+
  scale_fill_gradient(low="white", high="blue")+
  theme_ipsum()

#### PRODUCCIÓN DE POLEN DE LAS DIFERENTES ESPECIES DE CUCURBITA ####
contador = read.csv("produccion_polen.csv", header = T)
head(contador)
str(contador)
tapply(contador$polen_total, contador$id_planta, length)

#contador$anoplant <- paste(contador$año, contador$id_planta)

# Agrupando por individuo
conta = contador %>%
  group_by(año, condicion, spp, id_planta) %>%
  dplyr::summarize(total_mean = mean(polen_total), 
                   es_total = es(polen_total),
                   n = n())
conta
# Estadística descriptiva de la producción de polen
tapply(conta$total_mean, conta$spp, mean)
tapply(conta$total_mean, conta$spp, es)
tapply(conta$total_mean, conta$spp, length)
#write.csv(conta, file = "produ.csv")

# promedios por especie
conta2 = conta %>%
  group_by(spp) %>%
  dplyr::summarize(total_mean2 = mean(total_mean),
                   es_total2 = es(total_mean), n = n())

conta2

# boxplot por especie
boxplot(conta$total_mean ~ conta$spp)

# histograma y prueba de normalidad
hist(conta$total_mean)
shapiro.test(conta$total_mean) # no normal
shapiro.test(sqrt(conta$total_mean))
shapiro.test(log(conta$total_mean))

# Modelo GLM de producción entre condición
pro2 = glm(total_mean ~ spp, data = conta, family = gaussian(link = "identity"))
summary(pro2)
Anova(pro2)
shapiro.test(residuals(pro2))

# Estimated marginal means 
conta_compa <- emmeans(pro2, 
                       specs = pairwise ~ spp, 
                       type = "response")
conta_compa
conta_compa.m<-conta_compa$emmean 
conta_compa.m
model_means_cld_conta <- cld(object = conta_compa.m,
                             adjust = "sidak",
                             Letter = letters, 
                             alpha = 0.05)
model_means_cld_conta

# Cargando nueva base con la columna de tamaños por individuo
conta3 = read.csv("produ_tam.csv", header = T)
head(conta3)
str(conta3)

# Estadística descriptiva del tamaño del polen
tapply(conta3$tam, conta3$spp, mean)
tapply(conta3$tam, conta3$spp, es)
tapply(conta3$tam, conta3$spp, length)
# grafico de boxplot del tamaño
boxplot(conta3$tam ~ conta3$spp)

# Histograma y shapiro test
hist(conta3$tam)
shapiro.test(conta3$tam) #no normal
shapiro.test(log(conta3$tam))
shapiro.test(sqrt(conta3$tam))

# normalizando datos del tamaño del polen con el paquete bestNormalize
b1 <- bestNormalize::boxcox(conta3$tam)
str(b1)
shapiro.test(b1$x.t)

arcsinh_obj <-arcsinh_x(conta3$tam)
shapiro.test(arcsinh_obj$x.t)

(BNobject <- bestNormalize(conta3$tam))
# orderNorm Transformation
(orderNorm_obj <- orderNorm(conta3$tam))
str(orderNorm_obj)
shapiro.test(orderNorm_obj$x.t)

# Análisis de varianza de la producción de polen
#conta1 = aov(log(total_mean) ~ spp, data = conta3)
#summary(conta1)


# GLM del tamaño del polen
conta2 = glm(tam ~ condicion, data = conta3, family = gaussian)
summary(conta2)
Anova(conta2)

# análisis estadístico del tamaño del polen con una prueba de kruskal-wallis
kruskal.test(tam ~ spp, data = conta3)

# comparaciones múltiples
pairwise.wilcox.test(conta3$tam, conta3$spp,
                     p.adjust.method = "holm")

# Gráfica de la producción de polen
## function to return median and labels
#n_fun <- function(x){
#  return(data.frame(y = median(x) - 10000, 
#                    label = paste0("n = ",length(x))))
#}
head(conta)
# Gráfica de boxplot de la producción de polen
produ <- ggplot(conta, aes(x = spp, y = total_mean, fill = condicion)) +
  geom_boxplot() +
  ## use either geom_point() or geom_jitter()
  geom_point(size = 2, alpha = .3, #aes(color = especie)
             position = position_jitter(seed = 1, width = .1))+
 #stat_summary(geom = "text", fun.data = n_fun)+
labs(x = "Especies", y = "Producción de polen")+
  scale_x_discrete(labels=c("1CF" = "CF", "2CPF" = "CPF",
                            "3CPP" = "CPP", "4CAS" = "CAS", "5CAA" = "CAA",
                            "6CM" = "CM"))+
  theme_bw()+
  theme(axis.text = element_text(face = "bold"))+
  scale_color_discrete(name="Species",
                       labels = c("CF", "CPF", "CPP", "CAS", "CAA", "CM"))+
  #annotate("text", x=1, y=2000, label= "n = 26", size = 4)+
  #annotate("text", x=2, y=2000, label= "n = 6", size = 4)+
  #annotate("text", x=3, y=2000, label= "n = 28", size = 4)+
  #annotate("text", x=4, y=2000, label= "n = 39", size = 4)+
  #annotate("text", x=5, y=2000, label= "n = 32", size = 4)+
  #annotate("text", x=6, y=2000, label= "n = 31", size = 4)+
  annotate("text", x=1, y=34000, label= "b", size = 5)+
  annotate("text", x=2, y=19000, label= "d", size = 5)+
  annotate("text", x=3, y=21000, label= "d", size = 5)+
  annotate("text", x=4, y=29000, label= "c", size = 5)+
  annotate("text", x=5, y=37000, label= "bc", size = 5)+
  annotate("text", x=6, y=52000, label= "a", size = 5)+
  annotate("text", x=1, y=50000, label= "", size = 5)
  theme(legend.position = "none")+
  scale_fill_manual(values = c("#DE7862FF", "#58A449FF"))
produ

#### TAMAÑO DE POLEN DE LAS DIFENTES ESPECIES ####
polen = read.csv("tamaño_polen.csv", header = TRUE)
head(polen)
str(polen)
tapply(polen$micras, polen$especie, mean)
tapply(polen$micras, polen$especie, sd)
tapply(polen$micras, polen$especie, es)
tapply(polen$micras, polen$especie, length)
boxplot(polen$micras ~ polen$especie)

# Histograma del tamaño de polen 
hist(polen$micras)
shapiro.test(polen$micras)
# Análisis de varianza del tamaño de polen entre especies
mod1_polen = aov(polen$micras ~ polen$especie)
summary(mod1_polen)

# comparaciones multiples
compa1 = TukeyHSD(mod1_polen)
compa1
Cld = multcompLetters4(mod1_polen, compa1)
Cld
#
#em1 = emmeans(mod1_polen, specs = pairwise ~ especie)
#em1$emmeans
#em1$contrasts
#write.csv(em1$contrasts, file = "emmeans_polen_tamaño.csv")

# Gráfica del tamaño del polen entre especies
## function to return median and labels
#n_fun <- function(x){
#  return(data.frame(y = median(x) - 22, 
#                    label = paste0("n = ",length(x))))
#}
# Plot
size <- ggplot(conta3, aes(x = spp, y = tam, fill = condicion)) +
  geom_boxplot() +
  ## use either geom_point() or geom_jitter()
  geom_point(size = 2, alpha = .3, #
             position = position_jitter(seed = 1, width = .1))+
  #stat_summary(geom = "text", fun.data = n_fun)+
  #coord_flip()+
  labs(x = "Especies", y = "Tamaño del polen (μm)")+
scale_x_discrete(labels=c("1CF" = "CF", "2CPF" = "CPF",
                          "3CPP" = "CPP", "4CAS" = "CAS", "5CAA" = "CAA",
                          "6CM" = "CM"))+
  theme_bw()+
  theme(axis.text = element_text(face = "bold"))+
  scale_color_discrete(name="Species",
                    labels = c("CF", "CPF", "CPP", "CAS", "CAA", "CM"))+
  #annotate("text", x=1, y=115, label= "n = 100", size = 4)+
  #annotate("text", x=2, y=115, label= "n = 100", size = 4)+
  #annotate("text", x=3, y=115, label= "n = 100", size = 4)+
  #annotate("text", x=4, y=115, label= "n = 100", size = 4)+
  #annotate("text", x=5, y=115, label= "n = 100", size = 4)+
  #annotate("text", x=6, y=115, label= "n = 100", size = 4)+
  annotate("text", x=1, y=155.5, label= "a", size = 5)+
  annotate("text", x=2, y=142.5, label= "c", size = 5)+
  annotate("text", x=3, y=145, label= "bc", size = 5)+
  annotate("text", x=4, y=147.5, label= "b", size = 5)+
  annotate("text", x=5, y=145, label= "bc", size = 5)+
  annotate("text", x=6, y=146, label= "bc", size = 5)+
  scale_fill_manual(values = c("#DE7862FF", "#58A449FF"))+
  theme(legend.position = "none")
size  
#labels = c("Cucurbita foetidissima (CF)",
#           "Cucurbita pepo subsp. fraterna (CPF)",
#           "Cucurbita pepo subsp. pepo (CPP)",
#           "Cucurbita argyrosperma subsp. sororia (CAS)",
#           "Cucurbita argyrosperma subsp. argyrosperma (CAA)",
#           "Cucurbita moschata (CM)"),
#guide = guide_legend(nrow = 5, 
#                     label.theme = element_text(face = "italic")))
  
#### CONCENTRACIÓN DE PROTEINAS ####
proteinas = read.csv("concen_protein.csv", header = T)
head(proteinas)
str(proteinas)
tapply(proteinas$prot, proteinas$Especie, length)
#proteinas$anoplant  <- paste(proteinas$año, proteinas$PlantID)

# Agrupando por individuos
conce_prot = proteinas %>%
  group_by(año, Especie, PlantID, Estatus) %>%
  dplyr::summarize(total_conc = mean(prot), es_total = es(prot), n = n())
#write.csv(conce_prot, file ="conce_prot.csv")

tapply(conce_prot$total_conc, conce_prot$Especie, mean)
tapply(conce_prot$total_conc, conce_prot$Especie, es)
tapply(conce_prot$total_conc, conce_prot$Especie, length)

tapply(conce_lipi$total_conc, conce_lipi$Estatus, mean)
tapply(conce_lipi$total_conc, conce_lipi$Estatus, es)
tapply(conce_lipi$total_conc, conce_lipi$Estatus, length)

boxplot(conce_prot$total_conc ~ conce_prot$Especie)
boxplot(conce_prot$total_conc ~ conce_prot$Estatus)
shapiro.test(conce_prot$total_conc)
hist(conce_prot$total_conc)

# GLM de proteinas modelo bueno
modpro = glm(total_conc ~ Especie, data = conce_prot, family = gaussian)
summary(modpro)
Anova(modpro)
shapiro.test(residuals(modpro))


modpro1 = aov(total_conc ~ Estatus, data = conce_prot)
summary(modpro1)

#mod_prot = glm(prot ~ Especie, data = proteinas, family = "Gamma")
#summary(mod_prot)
#Anova(mod_prot)
# comparaciones multiples
#em_prot = emmeans(modpro1, specs = pairwise ~ Especie)
#em_prot$emmeans
#em_prot$contrasts

#mod_prot2 = glmer(prot ~ Especie + (1|PlantID), nAGQ = 0, data = proteinas,
#                  family = Gamma)
#summary(mod_prot2)
#Anova(mod_prot2)
# comparaciones multiples

# Plot
prot <- ggplot(conce_prot, aes(x = Especie, y = total_conc, fill = Estatus)) +
  geom_boxplot() +
  ## use either geom_point() or geom_jitter()
  geom_point(size = 2, alpha = .5, #
             position = position_jitter(seed = 1, width = .1))+
  #stat_summary(geom = "text", fun.data = n_fun)+
  #coord_flip()+
  labs(x = "Especies", y = "Concentración de proteínas (µg/mg)")+
  scale_x_discrete(labels=c("1CF" = "CF", "2CPF" = "CPF",
                            "3CPP" = "CPP", "4CAS" = "CAS", "5CAA" = "CAA",
                            "6CM" = "CM"))+
  theme_bw()+
  theme(axis.text = element_text(face = "bold"))+
  scale_color_discrete(name="Species",
                       labels = c("CF", "CPF", "CPP", "CAS", "CAA", "CM"))+
  #annotate("text", x=1, y=60, label= "n = 26", size = 4)+
  #annotate("text", x=2, y=60, label= "n = 21", size = 4)+
  #annotate("text", x=3, y=60, label= "n = 16", size = 4)+
  #annotate("text", x=4, y=60, label= "n = 33", size = 4)+
  #annotate("text", x=5, y=60, label= "n = 33", size = 4)+
  #annotate("text", x=6, y=60, label= "n = 43", size = 4)+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("#DE7862FF", "#58A449FF"))
prot

#### CONCENTRACIÓN DE LIPIDOS EN EL POLEN ####
lipi = read.csv("lipidos.csv", header = T)
head(lipi)
str(lipi)

# Agrupando por individuos
#lipi$anoplant = paste(lipi$año, lipi$PlantID)

# Agrupando por especie
conce_lipi = lipi %>%
  group_by(año, Especie, PlantID, Estatus) %>%
  dplyr::summarize(total_conc = mean(lipidos), es_total = es(lipidos), n = n())
conce_lipi
#write.csv(conce_lipi, file = "conce_lipi.csv")

# EStadistica descriptiva
tapply(conce_lipi$total_conc, conce_lipi$Especie, mean)
tapply(conce_lipi$total_conc, conce_lipi$Especie, es)
tapply(conce_lipi$total_conc, conce_lipi$Especie, length)


boxplot(conce_lipi$total_conc ~ conce_lipi$Especie)
shapiro.test(conce_lipi$total_conc)
hist(conce_lipi$total_conc)

# modelo
m1 = glm(total_conc ~ Especie, data = conce_lipi, family = gaussian)
summary(m1)
Anova(m1)
shapiro.test(residuals(m1))
# Estimated marginal means 
lip_compa <- emmeans(m1, 
                       specs = pairwise ~ Especie, 
                       type = "response")
lip_compa
lip_compa.m<-lip_compa$emmean 
lip_compa.m
model_means_cld_lip <- cld(object = lip_compa.m,
                             adjust = "sidak",
                             Letter = letters, 
                             alpha = 0.05)
model_means_cld_lip






m1 = aov(total_conc ~ Estatus, data = conce_lipi)
compa6 = TukeyHSD(m1)
compa6
Cld3 = multcompLetters4(m1, compa6)
Cld3

# modelo estadistico
mod_lip = glm(total_conc ~ Especie, data = conce_lipi, family = "gaussian")
#summary(mod_lip)
#Anova(mod_lip)
# comparaciones multiples
em_lip = emmeans(m1, specs = pairwise ~ Especie)
em_lip$emmeans
em_lip$contrasts


#library(lme4)
#mod_lip2 = glmer(lipidos ~ Especie + (1|PlantID), data = lipi,
#                  family = "gaussian")
#summary(mod_lip2)
#Anova(mod_lip2)
# comparaciones multiples
#em_prot2 = emmeans(mod_prot2, specs = pairwise ~ Especie)

# Plot
lipid <- conce_lipi %>%
ggplot(aes(x = Especie, y = total_conc, fill = Estatus)) +
  geom_boxplot() +
  ## use either geom_point() or geom_jitter()
  geom_point(size = 2, alpha = .5, #
             position = position_jitter(seed = 1, width = .1))+
  #stat_summary(geom = "text", fun.data = n_fun)+
  #coord_flip()+
  labs(x = "Especies", y = "Concentración de lípidos (µg/mg)")+
  scale_x_discrete(labels=c("1CF" = "CF", "2CPF" = "CPF",
                           "3CPP" = "CPP", "4CAS" = "CAS", "5CAA" = "CAA",
                            "6CM" = "CM"))+
  theme_bw()+
  theme(axis.text = element_text(face = "bold"))+
  scale_color_discrete(name="Species")+
  annotate("text", x=1, y=172, label= "ab", size = 5)+
  annotate("text", x=2, y=176, label= "ab", size = 5)+
  annotate("text", x=3, y=151, label= "b", size = 5)+
  annotate("text", x=4, y=155, label= "b", size = 5)+
  annotate("text", x=5, y=140, label= "b", size = 5)+
  annotate("text", x=6, y=189, label= "a", size = 5)+
  #annotate("text", x=1, y=58, label= "n = 24", size = 4)+
  #annotate("text", x=2, y=58, label= "n = 21", size = 4)+
  #annotate("text", x=3, y=58, label= "n = 16", size = 4)+
  #annotate("text", x=4, y=58, label= "n = 34", size = 4)+
  #annotate("text", x=5, y=58, label= "n = 32", size = 4)+
  #annotate("text", x=6, y=58, label= "n = 41", size = 4)+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("#DE7862FF", "#58A449FF"))
lipid  
  

#### relación proteinas lipidos P:L ####
pl = read.csv("PL.csv", header = T)  
head(pl)  
str(pl)

# Agrupando por individuo
pl_res = pl %>%
  group_by(año, Especie, PlantID, Estatus) %>%
  dplyr::summarize(total_pl = mean(pl), es_pl = es(pl), n = n())
pl_res

tapply(pl_res$total_pl, pl_res$Especie, mean)
tapply(pl_res$total_pl, pl_res$Especie, es)
tapply(pl_res$total_pl, pl_res$Especie, length)
#write.csv(pl_res, file = "pl.csv")
# Agrupando por especie
pl_res2 = pl_res %>%
  group_by(Especie) %>%
  dplyr::summarize(total_pl2 = mean(total_pl), 
                   es_pl2 = es(total_pl), n = n())
pl_res2
View(pl_res2)

#
boxplot(pl_res$total_pl ~ pl_res$Especie)
#boxplot(pl_res$total_pl ~ pl_res$Estatus)
shapiro.test(pl_res$total_pl)
hist(pl_res$total_pl)
# GLM
modpl = glm(total_pl ~ Especie, data = pl_res, family = gaussian)
summary(modpl)
Anova(modpl)
# ANOVA
modpl = aov(total_pl ~ Estatus, data = pl_res)
summary(modpl)

## grafica de boxplot
# Plot
prot_lip <- ggplot(aes(x = Especie, y = total_pl, fill = Estatus),
                   data = pl_res) +
  geom_boxplot() +
  ## use either geom_point() or geom_jitter()
  geom_point(size = 2, alpha = .5, #
             position = position_jitter(seed = 1, width = .1))+
  #stat_summary(geom = "text", fun.data = n_fun)+
  #coord_flip()+
  labs(x = "Especies", y = "P : L")+
  scale_x_discrete(labels=c("1CF" = "CF", "2CPF" = "CPF",
                           "3CPP" = "CPP", "4CAS" = "CAS", "5CAA" = "CAA",
                            "6CM" = "CM"))+
  theme_bw()+
  theme(axis.text = element_text(face = "bold"))+
  scale_color_discrete(name="Species",
                       labels = c("CF", "CPF", "CPP", "CAS", "CAA", "CM"))+
  #annotate("text", x=1, y=187, label= "a", size = 5)+
  #annotate("text", x=2, y=208, label= "a", size = 5)+
  #annotate("text", x=3, y=165, label= "a", size = 5)+
  #annotate("text", x=4, y=182, label= "a", size = 5)+
  #annotate("text", x=5, y=165, label= "a", size = 5)+
  #annotate("text", x=6, y=210, label= "a", size = 5)+
  #annotate("text", x=1, y=0.4, label= "n = 24", size = 4)+
  #annotate("text", x=2, y=0.4, label= "n = 20", size = 4)+
  #annotate("text", x=3, y=0.4, label= "n = 13", size = 4)+
  #annotate("text", x=4, y=0.4, label= "n = 33", size = 4)+
  #annotate("text", x=5, y=0.4, label= "n = 32", size = 4)+
  #annotate("text", x=6, y=0.4, label= "n = 41", size = 4)+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("#DE7862FF", "#58A449FF"))
prot_lip

# grafica de puntos
ggplot(pl, aes(x = prot, y = lipidos, color = Especie))+
  geom_point()#+
#  geom_smooth(method="lm", se=FALSE)


# haciendo panel de las dos gráficas
library(ggpubr)
# juntado grafica de proteinas, lipidos y P:L
ggarrange(produ, size, prot, lipid, prot_lip, 
          labels = c("A", "B", "C", "D", "E"),
          ncol =3, nrow = 2)

# cargando paqueteria gridExtra
library(gridExtra)
library(cowplot)
gt <- grid.arrange(produ, size, 
             prot, lipid, prot_lip,
             ncol = 6, nrow = 2, 
             layout_matrix = rbind(c(1,1,1,2,2,2),
                                   c(3,3,4,4,5,5)))
# Add labels to the arranged plots
p <- as_ggplot(gt) +                                # transform to a ggplot
  draw_plot_label(label = c("A", "B", "C", "D", "E"), size = 15,
                  x = c(0,0.5,0,0.333,0.666), y = c(1,1,0.5,0.5,0.5))#Add labels
p



##### Rasgos del polen ####
setwd("/home/luis/Documents/3_Doctorado_UNAM/a-proyecto/analisis_florales_tesis_2022/rasgos_polen")
# producción de polen
produccion = read.csv("produ.csv", header = T)
tamaño = read.csv("produ_tam.csv", header = T) #ya tiene la variable
proteinas = read.csv("conce_prot.csv", header = T)
lipidos = read.csv("conce_lipi.csv", header = T)
pl = read.csv("pl.csv", header = T)

# Agrupando por individuos
produccion$anoplant = paste(produccion$año, produccion$id_planta)
proteinas$anoplant = paste(proteinas$año, proteinas$PlantID)
lipidos$anoplant = paste(lipidos$año, lipidos$PlantID)
pl$anoplant = paste(pl$año, pl$PlantID)

# Uniendo tablas
joined_tibble <- full_join(produccion, tamaño,
                           by = join_by(anoplant == anoplant))
joined_tibble2 <- full_join(joined_tibble, proteinas,
                           by = join_by(anoplant == anoplant))
joined_tibble3 <- full_join(joined_tibble2, lipidos,
                            by = join_by(anoplant == anoplant))
joined_tibble4 <- full_join(joined_tibble3, pl,
                            by = join_by(anoplant == anoplant))
# Exportar tabla a csv
write.csv(joined_tibble4, file = "rasgos_pollen.csv")

### análisis multivariado con todos los rasgos evaluados del polen
library(vegan)

# cargando base depurada de todos los rasgos de polen
pollen = read.csv("rasgos_pollen2.csv", header = T)
head(pollen)
str(pollen)

tapply(pollen$produ, pollen$spp, mean)
tapply(pollen$tam, pollen$spp, mean)
tapply(pollen$conc.prot, pollen$spp, mean)
tapply(pollen$conc_lip, pollen$spp, mean)
tapply(pollen$pl, pollen$spp, mean)


pollen2 = pollen[,6:10]
head(pollen2)

# Normalidad multivariada
library(MVN)
# test de normalidad
# test de Mardia en MVN
result <- mvn(data = pollen2, mvnTest = "mardia")
result$multivariateNormality
# Henze-Zirkler's MVN test
result <- mvn(data = pollen2, mvnTest = "hz")
result$multivariateNormality
# Royston's MVN test
result <- mvn(data = pollen2, mvnTest = "royston")
result$multivariateNormality
# Doornik-Hansen's MVN test
result <- mvn(data = pollen2, mvnTest = "dh")
result$multivariateNormality
# Energy test
result <- mvn(data = pollen2, mvnTest = "energy")
result$multivariateNormality


# PCA de caracteres del nectar
head(pollen)
str(pollen)
pairs(pollen[6:10])
pca3 <- prcomp(pollen[, 6:10], scale = T)
summary(pca3)
pca3$rotation[, 1:2]
plot(pca3)
str(pca3)
#write.csv(pca1$rotation[, 1:2], file = "pca_hembras.csv")
biplot(pca3, scale=0)

# cargando paquete factoextra
library(factoextra)
# Top 10 variables que más contribuyen a PC1
fviz_contrib(pca3, choice = "var", axes = 1, top = 10)
fviz_contrib(pca3, choice = "var", axes = 2, top = 10)
# plot
fviz_pca_biplot(pca3, geom.ind = "point",
                axes = c(1,2), pointsize = 2, title = "", geom.var = c("",""),
)

# Variables
fviz_pca_var(pca3, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
# Contribución
fviz_pca_var(pca3, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

### Graph of individuals (rows)
ind = get_pca_ind(pca3)
ind

fviz_pca_ind(pca3)
fviz_pca_ind(pca3, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_pca_ind(pca3, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# Total contribution on PC1 and PC2
fviz_contrib(pca3, choice = "ind", axes = 1:2)

# Agrupado por condición
fviz_pca_ind(pca3,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = pollen$condicion, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "euclid",
             legend.title = "Groups"
)

# Agrupado por especie
fviz_pca_ind(pca3,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = pollen$spp, # color by groups
             palette = c("#00AFBB", "#E7B800","#FC4E07", "#00AFDB",
                         "#E7B850", "#FC4E50"),
             addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "confidence",
             legend.title = "Groups"
)

### agrupado por condición + las variables
fviz_pca_biplot(pca3,
                col.ind = pollen$condicion, palette = "jco",
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Condition"
)

### agrupado por especies + las variables
fviz_pca_biplot(pca1,
                col.ind = hem_resu2$especies,
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species"
)



# cargando paquete para graficar PCA
library(ggfortify)
autoplot(pca3)
pca_hem = autoplot(pca3, data = pollen, colour = "condicion", size = 2)+
  xlab("PC 1 (36.1%)") +
  ylab("PC 2 (31.35%)") +
  scale_color_discrete(name = "Condition",
                       labels = c("Domesticated", "Wild"))+
  theme_bw()
pca_hem


set.seed(0)#Para que los resultados no se brinden aleatorios
nmds1 <- metaMDS(pollen[,6:10])
plot(nmds1)
plot(nmds1, type = "t")
stressplot(nmds1)

coordenadas <- as.data.frame(scores(nmds1)$sites)
coordenadas

#Le añadimos a las coordenadas una columna con los tramos:
coordenadas$spp = pollen$spp
coordenadas$condicion = pollen$condicion
head(coordenadas)
# plot
gr2 <- ggplot(coordenadas, aes(x = NMDS1, y = NMDS2))+ 
  geom_point(size = 4, aes(colour = spp))+
  theme_bw()+
  scale_color_discrete(name = "Species", 
                       labels =c("CF","CPF","CPP","CAS","CAA","CM") );gr2

#+geom_text(hjust=0.5, vjust=1.5, label=datos$sample)
gr3 <- ggplot(coordenadas, aes(x = NMDS1, y = NMDS2))+ 
  geom_point(size = 4, aes(colour = condicion))+
  theme_bw()+
  scale_color_discrete(name = "condicion", 
                       labels =c("Domesticada","Silvestre") );gr3
#gr4 <- ggplot(coordenadas, aes(x = NMDS1, y = NMDS2))+ 
#  geom_point(size = 4, aes( shape = sexo_flor, colour = spp)); gr4
# haciendo panel de las dos gráficas

# PCoA
pol_dist2 = dist(pollen[,c(6:10)])
pol_PCoA <- wcmdscale(d = pol_dist2, eig = TRUE)
pol_PCoA
pol_PCoA$points
# gráfica del PCoA
ggplot(data = data.frame(pol_PCoA$points),
       aes(x = Dim1, y = Dim2)) +
  geom_point() +
  theme_bw()
#
pcoa <- cmdscale(pol_dist2, eig = TRUE, add = TRUE)
#convert pcoa results into data frame that can be plotted
pcoa_df <- data.frame(pcoa$points)
colnames(pcoa_df) <- c("PCo1", "PCo2")
pcoa_df$Species <- factor(pollen$spp) #add group of interest,
# mine was Morphospecies in the data frame cal_fem_data2
calf <- ggplot(pcoa_df, aes(x = PCo1, y = PCo2, color = Species)) + 
  geom_point(size = 2) +
  xlab("PCo1") +
  ylab("PCo2") + 
  ggtitle("Flores macho") +
  theme_classic()
calf
#
pcoa1 <- cmdscale(pol_dist2, eig = TRUE, add = TRUE)
#convert pcoa results into data frame that can be plotted
pcoa1_df <- data.frame(pcoa1$points)
colnames(pcoa1_df) <- c("PCo1", "PCo2")
pcoa1_df$condicion <- factor(pollen$condicion) #add group of interest,
# mine was Morphospecies in the data frame cal_fem_data2
calf <- ggplot(pcoa1_df, aes(x = PCo1, y = PCo2, color = condicion)) + 
  geom_point(size = 2) +
  xlab("PCo1") +
  ylab("PCo2") + 
  #ggtitle("Flores macho por condición") +
  theme_classic()+
  scale_color_discrete(name = "Condition", labels = c("Domesticated", "Wild"))
calf

 # PERMANOVA
library(vegan)
head(pollen)
str(pollen)
dune8 = pollen[6:10]
# Calculando distancia euclidiana
dune.dist5 <- vegdist(dune5, method="euclidean")

set.seed(0)
# calculando PERMANOVA por condición
dune.div8 <- adonis2(dune8 ~ condicion, data = pollen,
                     permutations = 999, method="euclidean")
dune.div8

# calculando PERMANOVA por especie
dune.div9 <- adonis2(dune8 ~ condicion+spp, data = pollen,
                    permutations = 999, method="euclidean")
dune.div9





#### VISITAS DE POLINIZADORES EN PARCELA DE MORELIA ####
more2021 <- read.csv("visitas_morelia_2021.csv", header = T)
head(more2021)
str(more2021)

### Horas totales de grabación entre 2021 y 2022: 128:52:28
# Horas de grabación por especie
# CPF: 7:31:30
# CPP: 30:41:42
# CAS: 31:25:02
# CAA: 22:50:56
# CM: 13:18:00

#To calculate the visitation rate for each floral visitor group we divided the
# number of visits by the total duration of the video recording 
# (decimal time = time x 1440) for each flower.


# numero de visitas por especie
tapply(more2021$especie, more2021$Especie.planta, length)

#
visitas <- more2021 %>%
  group_by(Especie.planta, Sexo.flor, especie) %>%
  dplyr::summarize(num_visitas = length(especie))

## frecuencia de visitas
frec <- more2021 %>%
  group_by(año, fecha, Especie.planta, ID.planta, Sexo.flor, especie,
           duración.decimal) %>%
  dplyr::summarize(num_visitas = length(especie)) %>% 
  mutate(frecu = num_visitas/duración.decimal)
  
frec2 <- frec %>%
  group_by(Especie.planta, Sexo.flor, especie) %>%
  dplyr::summarize(mean_frec = mean(frecu))


View(frec2)
#
ggplot(frec, aes(Especie.planta, fill = especie))+
  geom_bar()
#
ggplot(more2021, aes(Especie.planta, fill = especie))+
  geom_bar(position = "fill")
#
ggplot(more2021, aes(Especie.planta, fill = especie))+
  geom_bar()+
  facet_grid(.~ Sexo.flor)
#
ggplot(more2021, aes(Especie.planta, fill = especie))+
  geom_bar(position = "fill")+
  facet_grid(.~ Sexo.flor)

#### Realizando redes de interacción ####
library(fossil)

visitas2 = visitas %>%
  unite("plant_sex", Especie.planta:Sexo.flor)
df_vis <- create.matrix(as.data.frame(visitas2), tax.name = "especie",
                    locality = "plant_sex",
                    abund.col = "num_visitas",
                    abund = TRUE)
df1_vis <- t(df_vis)

# frecuencias de visita
frec3 = frec2 %>%
  unite("plant_sex", Especie.planta:Sexo.flor)

df <- create.matrix(as.data.frame(frec3), tax.name = "especie",
                    locality = "plant_sex",
                    abund.col = "mean_frec",
                    abund = TRUE)
df1 <- t(df)

#cargar la libreria bipartite
library (bipartite)

#Dibujar la red de interacciones
# numero de visita
plotweb(df1_vis,method="normal", y.lim=c(-1,2.5), labsize=1.2, text.rot=90,
        col.high = "brown4", col.low = "#D9A404")

# frecuencia de visita
plotweb(df1,method="normal", y.lim=c(-1,2.5), labsize=1.2, text.rot=90,
        col.high = "brown4", col.low = "#D9A404")

specieslevel(df1)
beeR<-specieslevel(df1)
summary(beeR)
str(beeR)
class(beeR)

colors()

#Calcular la m?tricas de red y guardarlos en un archivo de texto
bee_strength<-beeR$'higher level'
write.table(bee_strength, "bee_strength")

plant_strength<-beeR$'lower level'
write.table(plant_strength, "plant_strength")

visweb(df1) 
visweb(df1, type = "diagonal")
networklevel(df1) 










######
tam = read.csv("Tamaño_IND.csv", header = T)
head(tam)
str(tam)
hist(tam$Incremento)

tapply(tam$Incremento, tam$Size_Particle, length)
plot(tam$Size_Particle, tam$Incremento)
plot(tam$Incremento)

plo1 = ggplot(tam, aes(x=Size_Particle, y=Incremento)) + 
  geom_bar(stat = "identity")+
  facet_wrap(~Especie)
str(plo1)
summary(plo1)

ggplot(tam, aes(x=Size_Particle)) + geom_histogram()
# Change the width of bins
ggplot(df, aes(x=weight)) + 
  geom_histogram(binwidth=1)
# Change colors
p<-ggplot(df, aes(x=weight)) + 
  geom_histogram(color="black", fill="white")
p










#######
# Analizando por sitio
head(hembras)
CAS = subset(hembras, especie == "4CAS")
CAS
str(CAS)
boxplot(CAS$CD ~ CAS$sitio)
boxplot(CAS$TL ~ CAS$sitio)
boxplot(CAS$CL ~ CAS$sitio)
boxplot(CAS$TD1 ~ CAS$sitio)
boxplot(CAS$TD2 ~ CAS$sitio)
boxplot(CAS$TD3 ~ CAS$sitio)
boxplot(CAS$NDf ~ CAS$sitio)
boxplot(CAS$PL ~ CAS$sitio)
boxplot(CAS$SL ~ CAS$sitio)
boxplot(CAS$OL ~ CAS$sitio)
boxplot(CAS$OD ~ CAS$sitio)
boxplot(CAS$vol_nec ~ CAS$sitio)

# PCoa de flores hembra entre sitios
library(vegan)
cas_dist = dist(CAS[,c(11:23)])
cas_PCoA <- wcmdscale(d = cas_dist, eig = TRUE)
cas_PCoA
cas_PCoA$points
# gráfica del PCoA
ggplot(data = data.frame(cas_PCoA$points),
       aes(x = Dim1, y = Dim2)) +
  geom_point() +
  theme_bw()
#
pcoa <- cmdscale(cas_dist, eig = TRUE, add = TRUE)
#convert pcoa results into data frame that can be plotted
pcoa_df <- data.frame(pcoa$points)
colnames(pcoa_df) <- c("PCo1", "PCo2")
pcoa_df$sitios <- factor(CAS$sitio) #add group of interest,
# mine was Morphospecies in the data frame cal_fem_data2
calf <- ggplot(pcoa_df, aes(x = PCo1, y = PCo2, color = sitios)) + 
  geom_point(size = 2) +
  xlab("PCo1") +
  ylab("PCo2") + 
  ggtitle("Flores hembra de CAS por sitio") +
  theme_classic()
calf
#
simple.result.adonis2 <- adonis2(
  cas_dist ~ sitios,
  data = pcoa_df,
  method = "euc"
)


##### MACHOS ####
machos

#### CPF ####
mcpf = subset(machos, especie == "2CPF")
str(mcpf)
head(mcpf)

# cpf graficas
boxplot(mcpf$CD ~ mcpf$sitio)
boxplot(mcpf$TL ~ mcpf$sitio)
boxplot(mcpf$CL ~ mcpf$sitio)
boxplot(mcpf$TD1 ~ mcpf$sitio)
boxplot(mcpf$TD2 ~ mcpf$sitio)
boxplot(mcpf$TD3 ~ mcpf$sitio)
boxplot(mcpf$NDm ~ mcpf$sitio)
boxplot(mcpf$AD ~ mcpf$sitio)
boxplot(mcpf$StL ~ mcpf$sitio)
boxplot(mcpf$AL ~ mcpf$sitio)
boxplot(mcpf$vol_nec ~ mcpf$sitio)
# normalidad test
shapiro.test(mcpf$CD)
shapiro.test(mcpf$TL)
shapiro.test(mcpf$CL) # NO
shapiro.test(mcpf$TD1) # NO
shapiro.test(mcpf$TD2) # NO
shapiro.test(mcpf$TD3) # NO
shapiro.test(mcpf$NDm)
shapiro.test(mcpf$AD)
shapiro.test(mcpf$StL)
shapiro.test(mcpf$AL)
shapiro.test(mcpf$vol_nec) # NO

cpfm_dist = dist(mcpf[,c(11:21)])
cpfm_PCoA <- wcmdscale(d = cpfm_dist, eig = TRUE)
cpfm_PCoA
cpfm_PCoA$points
# gráfica del PCoA
ggplot(data = data.frame(cpfm_PCoA$points),
       aes(x = Dim1, y = Dim2)) +
  geom_point() +
  theme_bw()
#
pcoa_cpf <- cmdscale(cpfm_dist, eig = TRUE, add = TRUE)
#convert pcoa results into data frame that can be plotted
pcoa_cpf <- data.frame(pcoa_cpf$points)
colnames(pcoa_cpf) <- c("PCo1", "PCo2")
pcoa_cpf$sitios <- factor(mcpf$sitio) #add group of interest,
# mine was Morphospecies in the data frame cal_fem_data2
calf_cpf <- ggplot(pcoa_cpf, aes(x = PCo1, y = PCo2, color = sitios)) + 
  geom_point(size = 2) +
  xlab("PCo1") +
  ylab("PCo2") + 
  ggtitle("Flores macho de Cpf por sitio") +
  theme_classic()
calf_cpf
#
simple.result.adonis2 <- adonis2(
  cas_dist ~ sitios,
  data = pcoa_df,
  method = "euc"
)

#### CAS ####
mcas = subset(machos, especie == "4CAS")
str(mcas)
# cas
boxplot(mcas$CD ~ mcas$sitio)
boxplot(mcas$TL ~ mcas$sitio)
boxplot(mcas$CL ~ mcas$sitio)
boxplot(mcas$TD1 ~ mcas$sitio)
boxplot(mcas$TD2 ~ mcas$sitio)
boxplot(mcas$TD3 ~ mcas$sitio)
boxplot(mcas$NDm ~ mcas$sitio)
boxplot(mcas$AD ~ mcas$sitio)
boxplot(mcas$StL ~ mcas$sitio)
boxplot(mcas$AL ~ mcas$sitio)
boxplot(mcas$vol_nec ~ mcas$sitio)
# normalidad test
shapiro.test(mcas$CD)
shapiro.test(mcas$TL)
shapiro.test(mcas$CL)
shapiro.test(mcas$TD1) # NO
shapiro.test(mcas$TD2) # NO
shapiro.test(mcas$TD3)
shapiro.test(mcas$NDm) #NO
shapiro.test(mcas$AD) # NO
shapiro.test(mcas$StL)
shapiro.test(mcas$AL) # NO
shapiro.test(mcas$vol_nec) # NO
cas_dist = dist(CAS[,c(11:23)])
cas_PCoA <- wcmdscale(d = cas_dist, eig = TRUE)
cas_PCoA
cas_PCoA$points
# gráfica del PCoA
ggplot(data = data.frame(cas_PCoA$points),
       aes(x = Dim1, y = Dim2)) +
  geom_point() +
  theme_bw()
#
pcoa <- cmdscale(cas_dist, eig = TRUE, add = TRUE)
#convert pcoa results into data frame that can be plotted
pcoa_df <- data.frame(pcoa$points)
colnames(pcoa_df) <- c("PCo1", "PCo2")
pcoa_df$sitios <- factor(CAS$sitio) #add group of interest,
# mine was Morphospecies in the data frame cal_fem_data2
calf <- ggplot(pcoa_df, aes(x = PCo1, y = PCo2, color = sitios)) + 
  geom_point(size = 2) +
  xlab("PCo1") +
  ylab("PCo2") + 
  ggtitle("Flores hembra de CAS por sitio") +
  theme_classic()
calf
#
simple.result.adonis2 <- adonis2(
  cas_dist ~ sitios,
  data = pcoa_df,
  method = "euc"
)

#### CAA ####
mcaa = subset(machos, especie == "5CAA")
str(mcaa)
# CAA
boxplot(mcaa$CD ~ mcaa$sitio)
boxplot(mcaa$TL ~ mcaa$sitio)
boxplot(mcaa$CL ~ mcaa$sitio)
boxplot(mcaa$TD1 ~ mcaa$sitio)
boxplot(mcaa$TD2 ~ mcaa$sitio)
boxplot(mcaa$TD3 ~ mcaa$sitio)
boxplot(mcaa$NDm ~ mcaa$sitio)
boxplot(mcaa$AD ~ mcaa$sitio)
boxplot(mcaa$StL ~ mcaa$sitio)
boxplot(mcaa$AL ~ mcaa$sitio)
boxplot(mcaa$vol_nec ~ mcaa$sitio)
cas_dist = dist(CAS[,c(11:23)])
cas_PCoA <- wcmdscale(d = cas_dist, eig = TRUE)
cas_PCoA
cas_PCoA$points
# gráfica del PCoA
ggplot(data = data.frame(cas_PCoA$points),
       aes(x = Dim1, y = Dim2)) +
  geom_point() +
  theme_bw()
#
pcoa <- cmdscale(cas_dist, eig = TRUE, add = TRUE)
#convert pcoa results into data frame that can be plotted
pcoa_df <- data.frame(pcoa$points)
colnames(pcoa_df) <- c("PCo1", "PCo2")
pcoa_df$sitios <- factor(CAS$sitio) #add group of interest,
# mine was Morphospecies in the data frame cal_fem_data2
calf <- ggplot(pcoa_df, aes(x = PCo1, y = PCo2, color = sitios)) + 
  geom_point(size = 2) +
  xlab("PCo1") +
  ylab("PCo2") + 
  ggtitle("Flores hembra de CAS por sitio") +
  theme_classic()
calf
#
simple.result.adonis2 <- adonis2(
  cas_dist ~ sitios,
  data = pcoa_df,
  method = "euc"
)

#### CM ####
mcm = subset(machos, especie == "6CM")
str(mcm)
# CM
boxplot(mcm$CD ~ mcm$sitio)
boxplot(mcm$TL ~ mcm$sitio)
boxplot(mcm$CL ~ mcm$sitio)
boxplot(mcm$TD1 ~ mcm$sitio)
boxplot(mcm$TD2 ~ mcm$sitio)
boxplot(mcm$TD3 ~ mcm$sitio)
boxplot(mcm$NDm ~ mcm$sitio)
boxplot(mcm$AD ~ mcm$sitio)
boxplot(mcm$StL ~ mcm$sitio)
boxplot(mcm$AL ~ mcm$sitio)
boxplot(mcm$vol_nec ~ mcm$sitio)

cas_dist = dist(CAS[,c(11:23)])
cas_PCoA <- wcmdscale(d = cas_dist, eig = TRUE)
cas_PCoA
cas_PCoA$points
# gráfica del PCoA
ggplot(data = data.frame(cas_PCoA$points),
       aes(x = Dim1, y = Dim2)) +
  geom_point() +
  theme_bw()
#
pcoa <- cmdscale(cas_dist, eig = TRUE, add = TRUE)
#convert pcoa results into data frame that can be plotted
pcoa_df <- data.frame(pcoa$points)
colnames(pcoa_df) <- c("PCo1", "PCo2")
pcoa_df$sitios <- factor(CAS$sitio) #add group of interest,
# mine was Morphospecies in the data frame cal_fem_data2
calf <- ggplot(pcoa_df, aes(x = PCo1, y = PCo2, color = sitios)) + 
  geom_point(size = 2) +
  xlab("PCo1") +
  ylab("PCo2") + 
  ggtitle("Flores hembra de CAS por sitio") +
  theme_classic()
calf
#
simple.result.adonis2 <- adonis2(
  cas_dist ~ sitios,
  data = pcoa_df,
  method = "euc"
)



###########33
citation("lme4")
citation("emmeans")
citation()
citation("vegan")
citation("phyr")
citation("phytools")
citation("jpeg")


citation("phytools")
citation("jpeg")


###############################################################################
semillas <- read.csv("semillas_lesly.csv", header = T)
head(semillas)
str(semillas)


sem_ran = sample(semillas$id2, 300, replace = F)
View(sem_ran)
as.data.frame(sem_ran)
#as.matrix(sem_ran)


###########################################################################
# datos edgar
edgar = read.csv("feno2.csv")
head(edgar)
View(edgar)
boxplot(edgar$concentración ~ edgar$sexo)

library(ggplot2)
# Grafica de fenoles por especie
ggplot(edgar, aes(x = spp, y = concentración, fill = spp))+
  geom_boxplot()+
  labs(x = "Especies", y = "Concentración de Fenoles (mg/ml)", fill = "Especies")

# Gráfica de fenoles por especie y sexo
ggplot(edgar, aes(x = spp, y = concentración, fill = sexo))+
  geom_boxplot()+
  labs(x = "Especies", y = "Concentración de Fenoles (mg/ml)", 
       fill = "Sexos florales")+
  scale_fill_discrete(labels = c("Hembras", "Machos"))


# cargando paquete para graficar PCA
library(ggfortify)

# PCA por condición de flores hembra
autoplot(pca1)
pca_hem = autoplot(pca1, data = hem_resu2, colour = "condición", size = 2)+
  xlab("PC 1 (67.8%)") + #,frame = TRUE, frame.type = 'norm'
  ylab("PC 2 (13.3%)") +
  scale_color_discrete(name = "Condición",
                       labels = c("Domesticadas", "Silvestres"))+
  theme_bw()
pca_hem

# PCA por especie de flores hembra
autoplot(pca1)
pca_hem_spp = autoplot(pca1, data = hem_resu2, colour = "especie", size = 2)+
  xlab("PC 1 (67.8%)") + #,frame = TRUE, frame.type = 'norm'
  ylab("PC 2 (13.3%)") +
  scale_color_discrete(name = "Especies",
                       labels = c("CF", "CPF", "CPP", "CAS", "CAA", "CM"))+
  theme_bw()
pca_hem_spp

#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
ca.species = hem_resu2[, 3]
ca.condicion = hem_resu2[,2]
# gráfica de PCA con ggbiplot por condición
ggbiplot(pca1, obs.scale = 1, var.scale = 1, 
         groups = ca.condicion, ellipse = F, 
         circle = F, var.axes = F)+
  scale_color_discrete(name = '')+
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom')+
  theme_classic()

# gráfica de PCA con ggbiplot por especie
ggbiplot(pca1, obs.scale = 1, var.scale = 1, 
         groups = ca.species, ellipse = TRUE, 
         circle = F) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom')


## Otra forma de graficar PCA
# Getting proportion of variance explained by PC1 and PC2
prop_var <- pca1$sdev^2 / sum(pca1$sdev^2)

scores <- as.data.frame(pca1$x) 
scores$condicion <- hem_resu2$condición

loadings <- as.data.frame(pca1$rotation)
loadings$condicion <- rownames(loadings)


# Create biplot
biplot <- ggplot(data = scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = condicion), shape = 19)+
  scale_color_discrete(name = "Condition", labels = c("Domesticated", "Wild"))+
  scale_x_continuous(name = paste0("PC1 (",round(prop_var[1]*100, digits = 2),
                                   " %)")) +
  scale_y_continuous(name = paste0("PC2 (",round(prop_var[2]*100, digits = 2),
                                   " %)"))+
  theme_bw()
biplot


# Normalidad multivariada
library(MVN)
# test de normalidad
# test de Mardia en MVN
result <- mvn(data = hem3, mvnTest = "mardia")
result$multivariateNormality
# Henze-Zirkler's MVN test
result <- mvn(data = hem3, mvnTest = "hz")
result$multivariateNormality
# Royston's MVN test
result <- mvn(data = hem3, mvnTest = "royston")
result$multivariateNormality
# Doornik-Hansen's MVN test
result <- mvn(data = hem3, mvnTest = "dh")
result$multivariateNormality
# Energy test
result <- mvn(data = hem3, mvnTest = "energy")
result$multivariateNormality

# cargando paquete factoextra
library(factoextra)
# Top 10 variables que más contribuyen a PC1
fviz_contrib(pca2, choice = "var", axes = 1, top = 10)
fviz_contrib(pca2, choice = "var", axes = 2, top = 10)
# plot
fviz_pca_ind(pca2, geom.ind = "point",
             axes = c(1,2), pointsize = 2)

# cargando paquete para graficar PCA
library(ggfortify)
autoplot(pca2)
pca_mac = autoplot(pca2, data = machos2, colour = "condicion", size = 2)+
  xlab("PC 1 (53.2%)") +
  ylab("PC 2 (21.9%)") +
  scale_color_discrete(name = "Condición",
                       labels = c("Domesticadas", "Silvestres"))+
  theme_bw()
pca_mac


## machos por especie
pca_mac_spp = autoplot(pca2, data = machos2, colour = "especie", size = 2)+
  xlab("PC 1 (53.2%)") +
  ylab("PC 2 (21.9%)") +
  scale_color_discrete(name = "Especies",
                       labels = c("CF", "CPF", "CPP", "CAS", "CAA", "CM"))+
  theme_bw()
pca_mac_spp

# Joining PCoA plot from female and male flowers
library(gridExtra)
library(cowplot)
library(ggpubr)

# uniendo graficas
gt <- grid.arrange(pca_hem, pca_mac,
                   ncol = 1, nrow = 2)
# Add labels to the arranged plots
p <- as_ggplot(gt) +                                # transform to a ggplot
  draw_plot_label(label = c("A", "B"), size = 15,
                  x = c(0,0), y = c(1,.5))#Add labels
p

#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
ca.condicion2 = machos[,4]
ca.species2 = machos[, 5]

# PCA por condición
ggbiplot(pca2, obs.scale = 1, var.scale = 1, 
         groups = ca.condicion2, ellipse = TRUE, 
         circle = F) + scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom')
# PCA por especie
ggbiplot(pca2, obs.scale = 1, var.scale = 1, 
         groups = ca.species2, ellipse = TRUE, 
         circle = F) + scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom')
