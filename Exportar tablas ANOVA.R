### ESTE PAQUETE EXPORTA TABLAS DE ANOVA DIRECAMENTE A FORMATO BONITO


install.packages("rtf")
library(rtf)
export.aov.table <- function(lm_output, table.number=NA){
  table_values <- summary(pruebaANOVA)[[1]]
  table_values <- broom::tidy(table_values)
  names(table_values) <- c("Predictor","df", "SS","MS","Fvalue","pvalue")
  
  table_out <- table_values
  table_out$SS     <- round(table_out$SS,2)
  table_out$MS     <- round(table_out$MS,2)
  table_out$Fvalue <- round(table_out$Fvalue,2)
  table_out$pvalue <- round(table_out$pvalue,3)
  
  rtffile <- RTF("rtf.doc")  # this can be an .rtf or a .doc
  addParagraph(rtffile, paste ("Table ", table.number))
  addTable(rtffile, as.data.frame(table_out))
  done(rtffile)
  
}

export.aov.table(pruebaANOVA)


