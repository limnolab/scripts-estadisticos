
# ------ Busca a ver si existen estas librer�as. Si no, las instala
list.of.packages <- c("ggplot2", "DescTools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#----- Cargar un archivo en CSV a trav�s de un cuadro de di�logo 
df <- read.csv(choose.files(),sep=",")