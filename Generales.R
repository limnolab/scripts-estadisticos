
# ------ Busca a ver si existen estas librerías. Si no, las instala
list.of.packages <- c("ggplot2", "DescTools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#----- Cargar un archivo en CSV a través de un cuadro de diálogo 
df <- read.csv(choose.files(),sep=",")