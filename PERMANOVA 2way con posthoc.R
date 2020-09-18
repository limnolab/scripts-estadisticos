install.packages("vegan")
library(vegan)

#ABRE DAToS
datos1<-read.csv(file.choose(),  header=TRUE, dec=",",sep=";")

#ASIGNA FACTORES
spp1<-datos1[,c(5:31)]
planta<-datos1[,1]
sitio<-datos1[,2]

#corre funcion
pairwise_adonis(spp1, planta:sitio, sim_method = "euclidean",
                p_adjust_m = "bonferroni", reduce = NULL)


#FUNCION PERMANOVA A DOS VIAS CON POSTHOC
pairwise_adonis <- function(x, factors, sim_method = "euclidean",
                            p_adjust_m = "bonferroni", reduce = NULL) {
  
  co <- utils::combn(unique(as.character(factors)), 2)
  pairs <- c()
  F.Model <- c()
  R2 <- c()
  p.value <- c()
  
  
  for (elem in 1:ncol(co)) {
    x1 <- vegan::vegdist(x[factors %in% c(co[1, elem], co[2, elem]), ],
                         method = sim_method)
    
    ad <- vegan::adonis(x1 ~ factors[factors %in% c(co[1, elem], co[2, elem])])
    pairs <- c(pairs, paste(co[1, elem], "vs", co[2, elem]))
    F.Model <- c(F.Model, ad$aov.tab[1, 4])
    R2 <- c(R2, ad$aov.tab[1, 5])
    p.value <- c(p.value, ad$aov.tab[1, 6])
  }
  p.adjusted <- stats::p.adjust(p.value, method = p_adjust_m)
  
  sig <- c(rep("", length(p.adjusted)))
  sig[p.adjusted <= 0.05] <- "."
  sig[p.adjusted <= 0.01] <- "*"
  sig[p.adjusted <= 0.001] <- "**"
  sig[p.adjusted <= 1e-04] <- "***"
  pairw.res <- data.frame(pairs, F.Model, R2, p.value, p.adjusted, sig)
  
  if (!is.null(reduce)) {
    pairw.res <- subset(pairw.res, grepl(reduce, pairs))
    pairw.res$p.adjusted <- stats::p.adjust(pairw.res$p.value, method = p_adjust_m)
    
    sig <- c(rep("", length(pairw.res$p.adjusted)))
    sig[pairw.res$p.adjusted <= 0.05] <- "."
    sig[pairw.res$p.adjusted <= 0.01] <- "*"
    sig[pairw.res$p.adjusted <= 0.001] <- "**"
    sig[pairw.res$p.adjusted <= 1e-04] <- "***"
    pairw.res <- data.frame(pairw.res[, 1:5], sig)
  }
  class(pairw.res) <- c("pwadonis", "data.frame")
  return(pairw.res)
}
