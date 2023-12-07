library(Matrix)
library(lme4)
library(OddsPlotty)

pros <- read.csv("/home/ins-bio/programas/orson_Rscripts/GLM_tareas/pros.tsv", sep = "\t")

pros$capsula <- factor(pros$capsula, levels = c(0,1), labels= c("No penetro","Si Penetro"))
pros$dpros <- factor(pros$dpros, levels = c(1,2,3,4), labels= c("No nodulo","nodulo unilobar izquierdo", "nodulo unilobar derecho", "nodulo bilobar"))
pros$dcaps <- factor(pros$dcaps, levels = c(1,2), labels= c("No dcaps","Si dcaps"))
pros$raza <- factor(pros$raza, levels = c(1,2), labels= c("Blancos","Negros"))
pros$vol <- as.double(pros$vol)

#pros <- pros[complete.cases(pros),]
summary(pros)
attach(pros)

# ANALISIS UNIVARIADO

par(mfrow = c(3, 4))

plot(capsula, main = "Tumor penetró la capsula prostática")
plot(raza, main = "raza")
plot(dpros, main = "Examen digital rectal")
plot(dcaps, main = "Detección de afectación capsular")
hist(psa, main = "Valor del antígeno prostático")
boxplot(psa, main = "Valor del antígeno prostático")
hist(gleason, main = "score total de gleason")
boxplot(gleason, main = "score total de gleason")
hist(edad)
boxplot(edad, main = "edad")
hist(vol, main = "Volumen del tumor")
boxplot(vol, main = "Volumen del tumor")


# edad y psa a categoricas 

pros$edad_set <- ifelse(pros$edad <= 60, 'menor igual de 60', 'mayor de 60')
pros$psa_set <- ifelse(pros$psa <= 7, 'normal', 'anormal')

# ANALISIS BIVARIADO

table_1 <- table(capsula,raza)
table_1 <- addmargins(table_1)
table_1

modelo_1<- glm(capsula ~ dpros, family = "binomial" ,data = pros)
summary(modelo_1)
confint(modelo_1)
coef(modelo_1)
exp(cbind(coef(modelo_1),confint(modelo_1)))


# Analisis GLM para datos binarios. 

modelo_2 <- glm(capsula ~ edad + raza + dpros + dcaps + psa + vol + gleason, 
                family = binomial(link = "logit") ,data = pros)

summary(modelo_2)

exp(cbind(coef(modelo_2),confint(modelo_2)))


OR <- odds_plot(modelo_2, 
                title = "Odds Ratios",
                subtitle = "Factores Asociados a la penetracion capsular",
                point_col = "#5f00db", 
                error_bar_colour = "#5f00db",
                point_size = 1, 
                error_bar_width = 0.2,
                h_line_color = "#5f00db")
## Waiting for profiling to be done...
OR$odds_plot

# BONDAD DE AJUSTE 


p_val = pchisq(residual_deviande, gl_residual_deviance, lower.tail=FALSE)

# Prediccion del modelo binomial

prediccion <- predict.glm(modelo_3,type = "response")
clasificado <- 1*(prediccion>=0.5)

tabla <- table(capsula,clasificado)

round(100*sum(diag(tabla))/sum(tabla),2)

plot(modelo_2)
