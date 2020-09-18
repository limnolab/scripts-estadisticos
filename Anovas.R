#########  CARGAR EL ARCHIVO DESDE UN CSV ######### 
datos <- read.csv(choose.files(),sep=";")

# CAMBIAR COMAS POR PUNTOS - no funca despues
datos <- data.frame(lapply(datos, function(x) gsub(",", ".", x, fixed = TRUE)))

#########  SEPARAR FACTORES Y DATOS ######### 
# asume q la variable esta en la columna 3, el factor 1 en la columna 1 y factor 2 en columna 2
VARIABLE <- as.vector(datos[,4])
FACTOR1 <- as.vector(datos[,1])
FACTOR2 <- as.vector(datos[,3])

#VER NIVELES
levels(datos[,1])
levels(datos[,2])

######### GRAFICOS PARA VER DATOS######### 
# los nombres de los factores y variables son explicitos, son los nombres de las 
# columnas en la matriz de datos (no los nombres de los factores como vectores)
install.packages("ggpubr")
library("ggpubr")
ggline(datos, x = "CANAL", y = "PEPTONA", 
       add = c("mean_se", "jitter"), 
       order = c("C", "A", "B"),
       ylab = "PEPTONA", xlab = "CANAL")

ggboxplot(datos, x = "SITIO", y = "LENGTH", 
          color = "SITIO", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("C", "T"),
          ylab = "LENGTH", xlab = "SITIO")

######### CHEQUEO DE SUPUESTOS #######################
# Homogeneidad de varianza con test de Levene
install.packages("car")
library(car)
leveneTest(datos[,3] ~ datos[,1], data = datos)
#Si el test da p>0.05, las varianzas son homog?neas

library(rstatix)
#IDENTIFICAR OUTLIERS EN UNA VARIABLE DEPENDIENDO DE DOS FACTORES (SUSTRATO; TIEMPO)
matriz%>% group_by(SUBSTRATE, TIME) %>% identify_outliers(BACT)

#SHAPIRO WILKS PARA NORMALIDAD DEPENDIENDO DE DOS FACTORES (SUSTRATO; TIEMPO)
matriz %>% group_by(SUBSTRATE, TIME) %>% shapiro_test(BACT)





######### ANOVAS #######################
# ++++++++++++++++++++

# ANOVA A UNA VIA, FIJO
result <- aov(VARIABLE ~ FACTOR1, data=datos)
summary(result)

# KRUSKAL WALLIS, UNA VIA, FIJO
result <- kruskal.test((datos[,3]) ~ (datos[,1]), data=datos)
result

# ANOVA A DOS VIAS, FIXED FACTORS
# esta version es corta, pero menos explicativa: result <- aov(VARIABLE ~ FACTOR1*FACTOR2, data=datos)
result <- aov(VARIABLE ~ FACTOR1 + FACTOR2 + FACTOR1:FACTOR2, data=datos)
summary(result)

# ANOVA A DOS VIAS, CANAL NESTED IN FECHA
# esta version es corta, pero menos explicativa: result <- aov(VARIABLE ~ FACTOR1/FACTOR2, data=datos)
result <- aov(VARIABLE ~ FACTOR1 + FACTOR1 %in% FACTOR2, data=datos)
summary(result)

#########  RANDOM EFFECTS   #######################
#ES MAS FACIL CON LA FUNCION LMER, YA QUE LA FUNCION AOV PIDE TRATAMIENTOS BALANCEADOS

# HAY QUE USAR EL PAQUETE LME4 y CAR
install.packages("lme4")
install.packages("lmerTest")
library(lme4)
library(lmerTest)
library(car)

# ANOVA A UNA VIA, FACTOR RANDOM
result <- lmer(VARIABLE ~ 1 + (1|FACTOR1), data=datos)
#fit <- aov(VARIABLE ~ Error(FACTOR1), data=datos)
summary(result)
anova(result)


# ANOVA A DOS VIAS, CANAL ES FIJO, FECHA RANDOM
# Hay dos maneras:
result <- lmer(VARIABLE ~ FACTOR1 + (1|FACTOR2), data=datos, REML=TRUE)
anova(result)
rand(result)

#O 
result <- aov(VARIABLE ~ FACTOR1 + Error(FACTOR2), data=datos)
summary(result)

######### CHEQUEO DE SUPUESTOS A POSTERIORI #######################
# HOMOGENEIDAD: Plotear los residuales (post ANOVA!)
plot(result, 1)
#Si no hay relaci?n evidente entre residuals y fitted, hay homogeneidad d varianza

# NORMALIDAD: Plotear los residuales estandarizados (post ANOVA!)
plot(result, 2)
#Si los valores siguen mas o menos una recta a 45?, asumimos normalidad

# NORMALIDAD: TEST DE SHAPIRO-WILKS 
aov_residuals <- residuals(object = result)
shapiro.test(x = aov_residuals )
#Si el p>0.05, hay normalidad de datos


#########  TESTS A POSTERIORI   #######################
#TUKEY A POSTERIORI
TukeyHSD(result)

#########  MANOVAS   #######################
#hay que unir todas las variables dependientes (y1,y2,y3) en una matriz, y despues hacer anovas normales
Y <- cbind(y1,y2,y3)
fit <- manova(Y ~ A*B)
summary(fit, test="Pillai")