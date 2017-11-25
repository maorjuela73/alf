library("tidyverse", lib.loc="~/R/win-library/3.4")
library("readxl", lib.loc="~/R/win-library/3.4")
library("lubridate", lib.loc="~/R/win-library/3.4")

pretest_eval1 <- read_excel("../Datos/PRE_TEST_LENGUAJE FIGURADO_EVALUACION 1_Vf.xlsx")
pretest_eval1 <- pretest_eval1 %>% 
  unite("Mandragora disfrutaba de una vida sin dolores de cabeza:","Mandragora disfrutaba de una vida sin dolores de cabeza:","INTERPRETACION", sep = "%", remove = TRUE) %>%
  unite("Mandragora ordeno su habitacion en un abrir y cerrar de ojos:","Mandragora ordeno su habitacion en un abrir y cerrar de ojos:","INTERPRETACION__1", sep = "%", remove = TRUE) %>%
  unite("Circe pierde la cabeza por un buen postre:","Circe pierde la cabeza por un buen postre:","INTERPRETACION__2", sep = "%", remove = TRUE) %>%
  unite("Mandragora se quedo con la boca abierta:","Mandragora se quedo con la boca abierta:","INTERPRETACION__3", sep = "%", remove = TRUE) %>%
  unite("Abriendo los ojos como platos:","Abriendo los ojos como platos:","INTERPRETACION__4", sep = "%", remove = TRUE) %>%
  unite("Le habian tomado el pelo:","Le habian tomado el pelo:","INTERPRETACION__5", sep = "%", remove = TRUE) %>%
  unite("Se habia dado con la puerta en las narices:","Se habia dado con la puerta en las narices:","INTERPRETACION__6", sep = "%", remove = TRUE) %>%
  unite("Gritando a todo pulmon:","Gritando a todo pulmon:","INTERPRETACION__7", sep = "%", remove = TRUE) %>%
  unite("Las aguas del rio rugian como leones:","Las aguas del rio rugian como leones:","INTERPRETACION__8", sep = "%", remove = TRUE) %>%
  unite("La bruja con el corazon en la boca:","La bruja con el corazon en la boca:","INTERPRETACION__9", sep = "%", remove = TRUE) %>%
  unite("Mandragora con los nervios a flor de piel:","Mandragora con los nervios a flor de piel:","INTERPRETACION__10", sep = "%", remove = TRUE) %>%
  unite("Y asi como por arte de magia:","Y asi como por arte de magia:","INTERPRETACION__11", sep = "%", remove = TRUE) %>%
  select(-contains("TIPO DE LENGUAJE FIGURADO")) %>% 
  select(-contains("Marca temporal")) %>% 
  gather(pregunta,respuesta,c("Mandragora disfrutaba de una vida sin dolores de cabeza:",
                                                               "Mandragora ordeno su habitacion en un abrir y cerrar de ojos:",
                                                               "Circe pierde la cabeza por un buen postre:",
                                                               "Mandragora se quedo con la boca abierta:",
                                                               "Abriendo los ojos como platos:",
                                                               "Le habian tomado el pelo:",
                                                               "Se habia dado con la puerta en las narices:",
                                                               "Gritando a todo pulmon:",
                                                               "Las aguas del rio rugian como leones:",
                                                               "La bruja con el corazon en la boca:",
                                                               "Mandragora con los nervios a flor de piel:",
                                                               "Y asi como por arte de magia:")) %>%
  separate(respuesta, c("respuesta","interpretacion"), sep = "%", remove = TRUE)
names(pretest_eval1) <- c("nombre","sexo","fecha_nacimiento","grado","pregunta","respuesta","interpretacion")
pretest_eval1$fecha_nacimiento <- ymd(pretest_eval1$fecha_nacimiento)

postest_eval1 <- read_excel("../Datos/POST_TEST_LENGUAJE FIGURADO_EVALUACION 1_Vf.xlsx")
postest_eval1 <- postest_eval1 %>% 
  unite("Mandragora disfrutaba de una vida sin dolores de cabeza:","Mandragora disfrutaba de una vida sin dolores de cabeza:","INTERPRETACION", sep = "%", remove = TRUE) %>%
  unite("Mandragora ordeno su habitacion en un abrir y cerrar de ojos:","Mandragora ordeno su habitacion en un abrir y cerrar de ojos:","INTERPRETACION__1", sep = "%", remove = TRUE) %>%
  unite("Circe pierde la cabeza por un buen postre:","Circe pierde la cabeza por un buen postre:","INTERPRETACION__2", sep = "%", remove = TRUE) %>%
  unite("Mandragora se quedo con la boca abierta:","Mandragora se quedo con la boca abierta:","INTERPRETACION__3", sep = "%", remove = TRUE) %>%
  unite("Abriendo los ojos como platos:","Abriendo los ojos como platos:","INTERPRETACION__4", sep = "%", remove = TRUE) %>%
  unite("Le habian tomado el pelo:","Le habian tomado el pelo:","INTERPRETACION__5", sep = "%", remove = TRUE) %>%
  unite("Se habia dado con la puerta en las narices:","Se habia dado con la puerta en las narices:","INTERPRETACION__6", sep = "%", remove = TRUE) %>%
  unite("Gritando a todo pulmon:","Gritando a todo pulmon:","INTERPRETACION__7", sep = "%", remove = TRUE) %>%
  unite("Las aguas del rio rugian como leones:","Las aguas del rio rugian como leones:","INTERPRETACION__8", sep = "%", remove = TRUE) %>%
  unite("La bruja con el corazon en la boca:","La bruja con el corazon en la boca:","INTERPRETACION__9", sep = "%", remove = TRUE) %>%
  unite("Mandragora con los nervios a flor de piel:","Mandragora con los nervios a flor de piel:","INTERPRETACION__10", sep = "%", remove = TRUE) %>%
  unite("Y asi como por arte de magia:","Y asi como por arte de magia:","INTERPRETACION__11", sep = "%", remove = TRUE) %>%
  select(-contains("TIPO DE LENGUAJE FIGURADO")) %>% 
  select(-contains("Marca temporal")) %>% 
  gather(pregunta,respuesta,c("Mandragora disfrutaba de una vida sin dolores de cabeza:",
                              "Mandragora ordeno su habitacion en un abrir y cerrar de ojos:",
                              "Circe pierde la cabeza por un buen postre:",
                              "Mandragora se quedo con la boca abierta:",
                              "Abriendo los ojos como platos:",
                              "Le habian tomado el pelo:",
                              "Se habia dado con la puerta en las narices:",
                              "Gritando a todo pulmon:",
                              "Las aguas del rio rugian como leones:",
                              "La bruja con el corazon en la boca:",
                              "Mandragora con los nervios a flor de piel:",
                              "Y asi como por arte de magia:")) %>%
  separate(respuesta, c("respuesta","interpretacion"), sep = "%", remove = TRUE)
names(postest_eval1) <- c("nombre","sexo","fecha_nacimiento","grado","pregunta","respuesta","interpretacion")
postest_eval1$fecha_nacimiento <- ymd(postest_eval1$fecha_nacimiento)
as.duration(interval(now(),postest_eval1$fecha_nacimiento))
interval(ymd(20090201), ymd(20090101))

pretest_eval2 <- read_excel("../Datos/PRE_TEST_LENGUAJE FIGURADO EVALUACION 2_Vf.xlsx")
pretest_eval2 <- pretest_eval2 %>% 
  unite("Si yo le digo a un niño la expresion \"no seas payaso\" le estoy queriendo decir que:","Si yo le digo a un niño la expresion \"no seas payaso\" le estoy queriendo decir que:","INTERPRETACION", sep = "%", remove = TRUE) %>%
  unite("Si yo le digo a un niño: \"pareces un loro\" le estoy queriendo decir que:","Si yo le digo a un niño: \"pareces un loro\" le estoy queriendo decir que:","INTERPRETACION__1", sep = "%", remove = TRUE) %>%
  unite("Si yo te digo que: \"quiero un abrazo de oso\" te estoy queriendo decir que:","Si yo te digo que: \"quiero un abrazo de oso\" te estoy queriendo decir que:","INTERPRETACION__2", sep = "%", remove = TRUE) %>%
  unite("Si yo digo que \"Felipe se puso rojo como un tomate\" estoy queriendo decir que:","Si yo digo que \"Felipe se puso rojo como un tomate\" estoy queriendo decir que:","INTERPRETACION__3", sep = "%", remove = TRUE) %>%
  unite("Si yo digo que \"Juan camina como una tortuga\" estoy queriendo decir que:","Si yo digo que \"Juan camina como una tortuga\" estoy queriendo decir que:","INTERPRETACION__4", sep = "%", remove = TRUE) %>%
  unite("Si yo digo que \"esos dos niños mantienen como perros y gatos\" quiero decir que:","Si yo digo que \"esos dos niños mantienen como perros y gatos\" quiero decir que:","INTERPRETACION__5", sep = "%", remove = TRUE) %>%
  unite("Si yo digo que \"si no te portas bien te voy a dar un jalar de orejas\" estoy queriendo decir que:","Si yo digo que \"si no te portas bien te voy a dar un jalar de orejas\" estoy queriendo decir que:","INTERPRETACION__6", sep = "%", remove = TRUE) %>%
  unite("Si yo digo \"mi mama esta que echa fuego por la boca\" quiero decir que:","Si yo digo \"mi mama esta que echa fuego por la boca\" quiero decir que:","INTERPRETACION__7", sep = "%", remove = TRUE) %>%
  unite("Si yo digo que \"mi vecina tiene la lengua larga\" quiero decir que:","Si yo digo que \"mi vecina tiene la lengua larga\" quiero decir que:","INTERPRETACION__8", sep = "%", remove = TRUE) %>%
  select(-contains("TIPO DE LENGUAJE FIGURADO")) %>% 
  select(-contains("Marca temporal")) %>% 
  gather(pregunta,respuesta,c("Si yo le digo a un niño la expresion \"no seas payaso\" le estoy queriendo decir que:",
                              "Si yo le digo a un niño: \"pareces un loro\" le estoy queriendo decir que:",
                              "Si yo te digo que: \"quiero un abrazo de oso\" te estoy queriendo decir que:",
                              "Si yo digo que \"Felipe se puso rojo como un tomate\" estoy queriendo decir que:",
                              "Si yo digo que \"Juan camina como una tortuga\" estoy queriendo decir que:",
                              "Si yo digo que \"esos dos niños mantienen como perros y gatos\" quiero decir que:",
                              "Si yo digo que \"si no te portas bien te voy a dar un jalar de orejas\" estoy queriendo decir que:",
                              "Si yo digo \"mi mama esta que echa fuego por la boca\" quiero decir que:",
                              "Si yo digo que \"mi vecina tiene la lengua larga\" quiero decir que:")) %>%
  separate(respuesta, c("respuesta","interpretacion"), sep = "%", remove = TRUE)
names(pretest_eval2) <- c("nombre","sexo","fecha_nacimiento","grado","pregunta","respuesta","interpretacion")
pretest_eval2$fecha_nacimiento <- ymd(pretest_eval2$fecha_nacimiento)

postest_eval2 <- read_excel("../Datos/POST_TEST_LENGUAJE FIGURADO EVALUACION 2_Vf.xlsx")
postest_eval2 <- postest_eval2 %>% 
  unite("Si yo le digo a un niño la expresion \"no seas payaso\" le estoy queriendo decir que:","Si yo le digo a un niño la expresion \"no seas payaso\" le estoy queriendo decir que:","INTERPRETACION", sep = "%", remove = TRUE) %>%
  unite("Si yo le digo a un niño: \"pareces un loro\" le estoy queriendo decir que:","Si yo le digo a un niño: \"pareces un loro\" le estoy queriendo decir que:","INTERPRETACION__1", sep = "%", remove = TRUE) %>%
  unite("Si yo te digo que: \"quiero un abrazo de oso\" te estoy queriendo decir que:","Si yo te digo que: \"quiero un abrazo de oso\" te estoy queriendo decir que:","INTERPRETACION__2", sep = "%", remove = TRUE) %>%
  unite("Si yo digo que \"Felipe se puso rojo como un tomate\" estoy queriendo decir que:","Si yo digo que \"Felipe se puso rojo como un tomate\" estoy queriendo decir que:","INTERPRETACION__3", sep = "%", remove = TRUE) %>%
  unite("Si yo digo que \"Juan camina como una tortuga\" estoy queriendo decir que:","Si yo digo que \"Juan camina como una tortuga\" estoy queriendo decir que:","INTERPRETACION__4", sep = "%", remove = TRUE) %>%
  unite("Si yo digo que \"esos dos niños mantienen como perros y gatos\" quiero decir que:","Si yo digo que \"esos dos niños mantienen como perros y gatos\" quiero decir que:","INTERPRETACION__5", sep = "%", remove = TRUE) %>%
  unite("Si yo digo que \"si no te portas bien te voy a dar un jalar de orejas\" estoy queriendo decir que:","Si yo digo que \"si no te portas bien te voy a dar un jalar de orejas\" estoy queriendo decir que:","INTERPRETACION__6", sep = "%", remove = TRUE) %>%
  unite("Si yo digo \"mi mama esta que echa fuego por la boca\" quiero decir que:","Si yo digo \"mi mama esta que echa fuego por la boca\" quiero decir que:","INTERPRETACION__7", sep = "%", remove = TRUE) %>%
  unite("Si yo digo que \"mi vecina tiene la lengua larga\" quiero decir que:","Si yo digo que \"mi vecina tiene la lengua larga\" quiero decir que:","INTERPRETACION__8", sep = "%", remove = TRUE) %>%
  select(-contains("TIPO DE LENGUAJE FIGURADO")) %>% 
  select(-contains("Marca temporal")) %>% 
  gather(pregunta,respuesta,c("Si yo le digo a un niño la expresion \"no seas payaso\" le estoy queriendo decir que:",
                              "Si yo le digo a un niño: \"pareces un loro\" le estoy queriendo decir que:",
                              "Si yo te digo que: \"quiero un abrazo de oso\" te estoy queriendo decir que:",
                              "Si yo digo que \"Felipe se puso rojo como un tomate\" estoy queriendo decir que:",
                              "Si yo digo que \"Juan camina como una tortuga\" estoy queriendo decir que:",
                              "Si yo digo que \"esos dos niños mantienen como perros y gatos\" quiero decir que:",
                              "Si yo digo que \"si no te portas bien te voy a dar un jalar de orejas\" estoy queriendo decir que:",
                              "Si yo digo \"mi mama esta que echa fuego por la boca\" quiero decir que:",
                              "Si yo digo que \"mi vecina tiene la lengua larga\" quiero decir que:")) %>%
  separate(respuesta, c("respuesta","interpretacion"), sep = "%", remove = TRUE)
names(postest_eval2) <- c("nombre","sexo","fecha_nacimiento","grado","pregunta","respuesta","interpretacion")
postest_eval2$fecha_nacimiento <- ymd(postest_eval2$fecha_nacimiento)


# Relacion entre genero y respuestas --------------------------------------

names(postest_eval1) <- c("nombre","sexo","fecha_nacimiento","grado","pregunta","respuesta","interpretacion")

p <- ggplot(pretest_eval1, aes(x = pregunta, fill = interpretacion))
p + geom_bar(position = "dodge") + coord_flip() + ylab("Pregunta") + xlab("Frecuencia") + facet_grid(~sexo)

p <- ggplot(pretest_eval1, aes(x = pregunta, fill = sexo))
p + geom_bar(position = "dodge") + coord_flip() + ylab("Pregunta") + xlab("Frecuencia") + facet_grid(~interpretacion)


# Postest -----------------------------------------------------------------


p <- ggplot(postest_eval1, aes(x = pregunta, fill = interpretacion))
p + geom_bar(position = "dodge") + coord_flip() + ylab("Pregunta") + xlab("Frecuencia") + facet_grid(~sexo)

p <- ggplot(postest_eval1, aes(x = pregunta, fill = sexo))
p + geom_bar(position = "dodge") + coord_flip() + ylab("Pregunta") + xlab("Frecuencia") + facet_grid(~interpretacion)


# Comparacion post y pre --------------------------------------------------

base1 <- rbind(data.frame(resptesta = c(pretest_eval1$interpretacion), tipo = "Pretest", genero = pretest_eval1$sexo), data.frame(resptesta = c(postest_eval1$interpretacion), tipo = "Postest", genero = postest_eval1$sexo))

p <- ggplot(base1, aes(x = resptesta, fill = tipo))
p + geom_bar(position = "dodge") + coord_flip() + ylab("Pregunta") + xlab("Frecuencia") + facet_grid(~genero)


p <- ggplot(postest_eval1, aes(x = pregunta, fill = interpretacion))
p + geom_bar() + coord_flip() + ylab("Pregunta") + xlab("Frecuencia")

ggplot(pretest_eval1,aes(x =pretest_eval1$fecha_nacimiento))+geom_bar()

table(pretest_eval1$interpretacion , pretest_eval1$pregunta)
table(postest_eval1$interpretacion , postest_eval1$pregunta)

chisq.test(base1$resptesta, base1$tipo)


ggplot(base1, aes(resptesta, fill = tipo)) + geom_bar(position = "dodge")

p <- ggplot(postest_eval1, aes(x = pregunta, fill = interpretacion))
p + geom_bar(position = "dodge") + coord_flip() + ylab("Pregunta") + xlab("Frecuencia") + facet_grid(~sexo)



