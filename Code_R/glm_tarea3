# Author Orson Mestanza

library(dplyr)
library(nnet)

datos <- read.csv("data_multinomial.tsv", sep = "\t")
str(datos)

datos$ME <- factor(datos$ME, levels = c(0,1,2), labels= c("Nunca","dentro del año","mas de un año"))
datos$SYMPT <- factor(datos$SYMPT, levels = c(1,2,3,4), labels= c("totalmente de acuerdo","de acuerdo","en desacuerdo","totalmente desacuerdo"))
datos$HIST<- factor(datos$HIST, levels = c(0,1), labels= c("No","Si"))
datos$BSE<- factor(datos$BSE, levels = c(0,1), labels= c("No","Si"))
datos$DETC<- factor(datos$DETC, levels = c(1,2,3), labels= c("No es probable","algo probable","Muy probable"))


# ANALISIS UNIVARIADO
par(mfrow = c(2, 4))

plot(datos$ME, main = "Experiencia con mamografía", )
plot(datos$SYMPT, main = "No necesita una mamografía hasta los sintomas")
hist(datos$PB, main = "Beneficio percibido")
boxplot(datos$PB, main = "Beneficio percibido")
plot(datos$HIST, main = "antecedentes de cáncer de mama")
plot(datos$BSE, main = "Alguien le enseñó a examinarse los senos")
plot(datos$DETC, main = "probabilidad de encontrar un caso nuevo")

attach(datos)

# ANALISIS BIVARIADO

table_1 <- table(ME, DETC)
table_1 <- addmargins(table_1)
table_1


# ANALISIS BIVARIADO

chisq.test(datos$ME, datos$SYMPT, correct = FALSE)
chisq.test(datos$ME, datos$PB, correct = FALSE)
chisq.test(datos$ME, datos$HIST, correct = FALSE)
chisq.test(datos$ME, datos$BSE, correct = FALSE)
chisq.test(datos$ME, datos$DETC, correct = FALSE)


# ANALISIS MULTIVARIADO


datos$ME <- relevel(datos$ME,ref= "Nunca") # De

multinorm.fit <- multinom(ME ~ SYMPT + PB + HIST + BSE + DETC, data = datos)

summary(multinorm.fit)

z <- summary(multinorm.fit)$coefficients/summary(multinorm.fit)$standard.errors
z

# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# CALCULO DE LOS OR
exp(coef(multinorm.fit))

# INTERVALO DE CONFIANZA 95% LOS OR
exp(confint(multinorm.fit))

# prediccion del modelo

head(pp <- fitted(multinorm.fit))
