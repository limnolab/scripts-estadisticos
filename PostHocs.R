
#POST HOCS -----------------------

#-----------LS MEANS
difflsmeans(fit1, test.effs="CANAL")

##-----------POST HOCS TUKEY CON GRAFICO SIMPLE
install.packages("agricolae")
library(agricolae)

#para efectos principales
posthoc <- (HSD.test(fit, "TREATMENT"))
plot(posthoc, cex.main=0.8, cex.axis=0.8)

#para interacciones hay q hacer una nueva variable con la interacción (tx)
tx <- with(df, interaction(TREATMENT, CHANNEL))
amod <- aov(N2O ~ tx, data=df)
posthoc <- HSD.test(amod, "tx", group=TRUE)
plot(posthoc, cex.main=0.8, cex.axis=0.8)

#-----------MULTIPLE COMPARISONS OF MEANS - TUKEY
library(multcomp)
posthoc = glht(fit1, linfct = mcp(CANAL="Tukey"))
mcs = summary(posthoc, test=adjusted("single-step"))
mcs
cld(mcs, level=0.05, decreasing=TRUE)

#-----------SNK con GAD
install.packages("GAD")
library(GAD)
#EL SNK TRABAJA CON UN LM
model <- lm(df$RRU ~ CHANNEL + TREATMENT + CHANNEL:TREATMENT, df)
anova(model)
print(snk.test(model,term= "TREATMENT", among = 'CHANNEL', within = 'TREATMENT'))

#-----------SNK con AGRICOLAE
library(agricolae)
#factores principales
print(SNK.test(fit,"CHANNEL", group=TRUE))


#-----------PAIRWISE PARA EFECTOS PRINCIPALES EN UN CUADRO BONITO
install.packages("factorplot")
library(factorplot)
fp <- factorplot(fit, factor.variable="TREATMENT", pval = 0.05, order="alph")
plot(fp)

#-----------POSTHOC GRAFICO DE BOXPLOT
library(multcomp)
# para factores principales
amod <- aov(RRU ~ TREATMENT, data=df)
#para interacciones
tx <- with(df, interaction(TREATMENT, CHANNEL))
amod <- aov(RRU ~ tx, data=df)
tuk <- glht(amod, linfct = mcp(tx = "Tukey"))
tuk.cld <- cld(tuk)   # letter-based display
opar <- par(mai=c(1,1,1.5,1)) #etiquetas
plot(tuk.cld, col=c("black", "red", "blue", "green"))
par(opar)

#------------ EFFECT SIZE MEASURES
#ETA SQUARED
install.packages("DescTools")
library("DescTools")
EtaSq(fit, type = 2, anova = TRUE)