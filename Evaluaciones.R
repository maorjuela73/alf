# Librerías ---------------------------------------------------------------

library("tidyverse", lib.loc="~/R/win-library/3.4")
library("readxl", lib.loc="~/R/win-library/3.4")
library("lubridate", lib.loc="~/R/win-library/3.4")
library("scales", lib.loc="~/R/win-library/3.4")

# Carga de datos ----------------------------------------------------------

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
pretest_eval1[,"edad"] <- interval(pretest_eval1$fecha_nacimiento,now()) %>% 
  as.period() %>% # Puede ser as.duration?
  year() %>% 
  as.numeric("years")
pretest_eval1[,"evaluacion"] <- "primera"
pretest_eval1[,"tratamiento"] <- "pretest"

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
postest_eval1[,"edad"] <- interval(postest_eval1$fecha_nacimiento,now()) %>% 
  as.period() %>% # Puede ser as.duration?
  year() %>% 
  as.numeric("years")
postest_eval1[,"evaluacion"] <- "primera"
postest_eval1[,"tratamiento"] <- "postest"

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
pretest_eval2[,"edad"] <- interval(pretest_eval2$fecha_nacimiento,now()) %>% 
  as.period() %>% # Puede ser as.duration?
  year() %>% 
  as.numeric("years")
pretest_eval2[,"evaluacion"] <- "segunda"
pretest_eval2[,"tratamiento"] <- "pretest"

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
postest_eval2[,"edad"] <- interval(postest_eval2$fecha_nacimiento,now()) %>% 
  as.period() %>% # Puede ser as.duration?
  year() %>% 
  as.numeric("years")
postest_eval2[,"evaluacion"] <- "segunda"
postest_eval2[,"tratamiento"] <- "postest"

preguntas <- read_excel("../Datos/preguntas.xlsx",sheet = "preguntas")

# Tabla de datos consolidada
base_alf <- pretest_eval1 %>% 
  bind_rows(postest_eval1) %>% 
  bind_rows(pretest_eval2) %>% 
  bind_rows(postest_eval2)

base_alf$tratamiento <-  ordered(base_alf$tratamiento, levels = c("pretest","postest"))

# Análisis de información personal----------------------------------------------------------------

#write_excel_csv(distinct(base_alf[,"nombre"]),"../Datos/correcion_nombres.xlsx")
distinct(base_alf[,"nombre"]) # Nombres de niños
nombres <- distinct(pretest_eval1[,"nombre"]) %>% 
  full_join(distinct(postest_eval1[,"nombre"]), copy = T) %>% 
  full_join(distinct(pretest_eval2[,"nombre"]), copy = T) %>%
  full_join(distinct(postest_eval2[,"nombre"]), copy = T) 

ip_pretest <- base_alf[,c("nombre","sexo","fecha_nacimiento","edad","grado")] %>% 
  distinct() %>% 
  count(grado,edad,sexo) 
ip_pretest

gruposexo <- ip_pretest %>% group_by(sexo) %>% summarise(n=sum(n))
gruposexo
grupoedad <- ip_pretest %>% group_by(edad) %>% summarise(n=sum(n))
grupoedad
grupogrado <- ip_pretest %>% group_by(grado) %>% summarise(n=sum(n))
grupogrado

grupoedadgrado <- ip_pretest %>% group_by(edad,grado) %>% summarise(n=sum(n))
grupoedadgrado

ggplot(grupoedadgrado, aes(x=edad,y=n,fill=grado)) +
  geom_bar(stat = "identity",position="dodge") +
  geom_text(aes(label=n), position=position_dodge(width = 1), vjust=-0.4)
  
ggplot(ip_pretest, aes(x=edad,y=n,fill=grado)) + 
  geom_bar(stat = "identity",position="dodge") + 
  geom_text(aes(label=n), position=position_dodge(width = 1), vjust=-0.4) +
  facet_grid(.~sexo,labeller = label_both)

grupoedadsexo <-  ip_pretest %>% group_by(edad,sexo) %>% summarise(n=sum(n))
grupoedadsexo

ggplot(grupoedadsexo, aes(x=edad,y=factor(n),fill=sexo)) + 
  geom_bar(stat = "identity",position="dodge") +
  geom_text(aes(label=n), position=position_dodge(width = 1), vjust=-0.4)

ggplot(ip_pretest, aes(x=edad,y=n,fill=sexo)) + 
  geom_bar(stat = "identity",position="dodge") + 
  geom_text(aes(label=n), position=position_dodge(width = 1),vjust=-0.4) +
  facet_grid(.~grado,labeller = label_both)


# Análisis general de interpretación de respuestas POR EVALUACIÓN ---------

# Primera evaluación
gruporespuestas_1 <-  base_alf %>%
  left_join(select(preguntas,-evaluacion), by = "pregunta") %>% 
  group_by(tratamiento,evaluacion,pregunta,interpretacion) %>% 
  summarise(n=n()) %>% 
  filter(evaluacion=="primera")
gruporespuestas_1

# ggplot(gruporespuestas_1, aes(x = pregunta, y = n, fill = interpretacion )) +
#   geom_bar(stat = "identity",position = "fill") +
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   coord_flip() +
#   facet_grid(~tratamiento)

gruporespuestas_1tally <- gruporespuestas_1 %>% group_by(tratamiento,evaluacion,pregunta) %>% add_tally()
gruporespuestas_1tally

ggplot(gruporespuestas_1tally, aes(x = pregunta, y = n, fill = interpretacion )) +
  geom_bar(stat = "identity",position = "fill") +
  geom_text(aes(label=percent(signif(n/nn,2))),position = position_fill(vjust = .5))+
  coord_flip() +
  facet_grid(~tratamiento,labeller = label_both)

# Segunda evaluación
gruporespuestas_2 <-  base_alf %>%
  left_join(select(preguntas,-evaluacion), by = "pregunta") %>% 
  group_by(tratamiento,evaluacion,pregunta,interpretacion) %>% 
  summarise(n=n()) %>% 
  filter(evaluacion=="segunda")
gruporespuestas_2

# ggplot(gruporespuestas_2, aes(x = pregunta, y = n, fill = interpretacion )) +
#   geom_bar(stat = "identity",position = "fill") +
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   coord_flip() +
#   facet_grid(~tratamiento)

gruporespuestas_2tally <- gruporespuestas_2 %>% group_by(tratamiento,evaluacion,pregunta) %>% add_tally()
gruporespuestas_2tally

ggplot(gruporespuestas_2tally, aes(x = pregunta, y = n, fill = interpretacion )) +
  geom_bar(stat = "identity",position = "fill") +
  geom_text(aes(label=percent(signif(n/nn,2))),position = position_fill(vjust = .5))+
  coord_flip() +
  facet_grid(~tratamiento,labeller = label_both)

# Análisis por Edad  ------------------------------------------------------

grupoedadinterpretacion <- base_alf %>% 
  group_by(tratamiento,evaluacion,edad,interpretacion) %>% 
  summarise(n=n())
grupoedadinterpretacion

# ggplot(grupoedadinterpretacion,aes(x = edad, y=n, fill = interpretacion))+
#   geom_bar(stat = "identity",position = "fill")+ 
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   facet_grid(tratamiento~evaluacion)

grupoedadinterpretaciontally <- grupoedadinterpretacion %>% group_by(tratamiento,evaluacion,edad) %>% add_tally()
grupoedadinterpretaciontally

ggplot(grupoedadinterpretaciontally,aes(x = edad, y=n, fill = interpretacion))+
  geom_bar(stat = "identity",position = "fill")+ 
  geom_text(aes(label=percent(signif(n/nn,2))),position = position_fill(vjust = .5))+
  facet_grid(tratamiento~evaluacion,labeller = label_both)

# Edad y respuestas por pregunta

# Primera evaluación
grupoedadrespuestas_1 <-  base_alf %>%
  filter(evaluacion=="primera") %>% 
  left_join(select(preguntas,-evaluacion), by = "pregunta") %>% 
  group_by(tratamiento,edad,pregunta,interpretacion) %>% 
  summarise(n=n()) 
grupoedadrespuestas_1

# ggplot(grupoedadrespuestas_1, aes(x = pregunta, y = n, fill = interpretacion )) +
#   geom_bar(stat = "identity",position = "fill") +
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   coord_flip() +
#   facet_grid(edad~tratamiento)

grupoedadrespuestas_1tally <- grupoedadrespuestas_1 %>% group_by(tratamiento,edad,pregunta) %>% add_tally()

ggplot(grupoedadrespuestas_1tally, aes(x = pregunta, y = n, fill = interpretacion )) +
  geom_bar(stat = "identity",position = "fill") +
  geom_text(aes(label=percent(signif(n/nn,2))),position = position_fill(vjust = .5))+
  coord_flip() +
  facet_grid(edad~tratamiento,labeller = label_both)

# Segunda evaluación
grupoedadrespuestas_2 <-  base_alf %>%
  filter(evaluacion=="segunda") %>% 
  left_join(select(preguntas,-evaluacion), by = "pregunta") %>% 
  group_by(tratamiento,edad,pregunta,interpretacion) %>% 
  summarise(n=n()) 

# ggplot(grupoedadrespuestas_2, aes(x = pregunta, y = n, fill = interpretacion )) +
#   geom_bar(stat = "identity",position = "fill") +
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   coord_flip() +
#   facet_grid(edad~tratamiento)

grupoedadrespuestas_2tally <- grupoedadrespuestas_2 %>% group_by(tratamiento,edad,pregunta) %>% add_tally()

ggplot(grupoedadrespuestas_2tally, aes(x = pregunta, y = n, fill = interpretacion )) +
  geom_bar(stat = "identity",position = "fill") +
  geom_text(aes(label=percent(signif(n/nn,2))),position = position_fill(vjust = .5))+
  coord_flip() +
  facet_grid(edad~tratamiento,labeller = label_both)


# Análisis por Grado  ------------------------------------------------------

grupogradointerpretacion_1 <- base_alf %>% 
  group_by(tratamiento,evaluacion,grado,interpretacion) %>% 
  summarise(n=n())
grupogradointerpretacion_1

# ggplot(grupogradointerpretacion_1,aes(x = grado, y=n, fill = interpretacion))+
#   geom_bar(stat = "identity",position = "fill")+ 
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   facet_grid(tratamiento~evaluacion)

grupogradointerpretacion_1tally <- grupogradointerpretacion_1 %>% group_by(tratamiento,evaluacion,grado) %>% add_tally()
grupogradointerpretacion_1tally

ggplot(grupogradointerpretacion_1tally,aes(x = grado, y=n, fill = interpretacion))+
  geom_bar(stat = "identity",position = "fill")+ 
  geom_text(aes(label=percent(signif(n/nn,2))),position = position_fill(vjust = .5))+
  facet_grid(tratamiento~evaluacion,labeller = label_both)

# Grado y respuestas por pregunta

# Primera evaluación
grupogradorespuestas_1 <-  base_alf %>%
  filter(evaluacion=="primera") %>% 
  left_join(select(preguntas,-evaluacion), by = "pregunta") %>% 
  group_by(tratamiento,grado,pregunta,interpretacion) %>% 
  summarise(n=n()) 
grupogradorespuestas_1

# ggplot(grupogradorespuestas_1, aes(x = pregunta, y = n, fill = interpretacion )) +
#   geom_bar(stat = "identity",position = "fill") +
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   coord_flip() +
#   facet_grid(grado~tratamiento)

grupogradorespuestas_1tally <- grupogradorespuestas_1 %>% group_by(tratamiento,grado,pregunta) %>% add_tally()

ggplot(grupogradorespuestas_1tally, aes(x = pregunta, y = n, fill = interpretacion )) +
  geom_bar(stat = "identity",position = "fill") +
  geom_text(aes(label=percent(signif(n/nn,2))),position = position_fill(vjust = .5))+
  coord_flip() +
  facet_grid(grado~tratamiento,labeller = label_both)

# Segunda evaluación
grupogradorespuestas_2 <-  base_alf %>%
  filter(evaluacion=="segunda") %>% 
  left_join(select(preguntas,-evaluacion), by = "pregunta") %>% 
  group_by(tratamiento,grado,pregunta,interpretacion) %>% 
  summarise(n=n()) 

# ggplot(grupogradorespuestas_2, aes(x = pregunta, y = n, fill = interpretacion )) +
#   geom_bar(stat = "identity",position = "fill") +
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   coord_flip() +
#   facet_grid(grado~tratamiento)

grupogradorespuestas_2tally <- grupogradorespuestas_2 %>% group_by(tratamiento,grado,pregunta) %>% add_tally()

ggplot(grupogradorespuestas_2tally, aes(x = pregunta, y = n, fill = interpretacion )) +
  geom_bar(stat = "identity",position = "fill") +
  geom_text(aes(label=percent(signif(n/nn,2))),position = position_fill(vjust = .5))+
  coord_flip() +
  facet_grid(grado~tratamiento,labeller = label_both)

# Análisis por Sexo -----------------------------------------------------

gruposexointerpretacion <- base_alf %>% 
  group_by(tratamiento,evaluacion,sexo,interpretacion) %>% 
  summarise(n=n())

# ggplot(gruposexointerpretacion,aes(x = sexo, y=n, fill = interpretacion))+
#   geom_bar(stat = "identity",position = "fill")+ 
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   facet_grid(tratamiento~evaluacion)

gruposexointerpretaciontally <- gruposexointerpretacion %>% group_by(tratamiento,evaluacion,sexo) %>% add_tally()

ggplot(gruposexointerpretaciontally,aes(x = sexo, y=n, fill = interpretacion))+
  geom_bar(stat = "identity",position = "fill")+ 
  geom_text(aes(label=percent(signif(n/nn,2))),position = position_fill(vjust = .5))+
  facet_grid(tratamiento~evaluacion,labeller = label_both)

# Sexo y respuestas por pregunta

# Primera evaluación
gruposexorespuestas_1 <-  base_alf %>%
  filter(evaluacion=="primera") %>% 
  left_join(select(preguntas,-evaluacion), by = "pregunta") %>% 
  group_by(tratamiento,sexo,pregunta,interpretacion) %>% 
  summarise(n=n()) 

# ggplot(gruposexorespuestas_1, aes(x = pregunta, y = n, fill = interpretacion )) +
#   geom_bar(stat = "identity",position = "fill") +
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   coord_flip() +
#   facet_grid(sexo~tratamiento)

gruposexorespuestas_1tally <- gruposexorespuestas_1 %>% group_by(tratamiento,sexo,pregunta) %>% add_tally()

ggplot(gruposexorespuestas_1tally, aes(x = pregunta, y = n, fill = interpretacion )) +
  geom_bar(stat = "identity",position = "fill") +
  geom_text(aes(label=percent(signif(n/nn,2))),position = position_fill(vjust = .5))+
  coord_flip() +
  facet_grid(sexo~tratamiento,labeller = label_both)

# Segunda evaluación
gruposexorespuestas_2 <-  base_alf %>%
  filter(evaluacion=="segunda") %>% 
  left_join(select(preguntas,-evaluacion), by = "pregunta") %>% 
  group_by(tratamiento,sexo,pregunta,interpretacion) %>% 
  summarise(n=n()) 

# ggplot(gruposexorespuestas_2, aes(x = pregunta, y = n, fill = interpretacion )) +
#   geom_bar(stat = "identity",position = "fill") +
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   coord_flip() +
#   facet_grid(sexo~tratamiento)

gruposexorespuestas_2tally <- gruposexorespuestas_2 %>% group_by(tratamiento,sexo,pregunta) %>% add_tally()

ggplot(gruposexorespuestas_2tally, aes(x = pregunta, y = n, fill = interpretacion )) +
  geom_bar(stat = "identity",position = "fill") +
  geom_text(aes(label=percent(signif(n/nn,2))),position = position_fill(vjust = .5))+
  coord_flip() +
  facet_grid(sexo~tratamiento,labeller = label_both)

# Análisis por Contexto ---------------------------------------------------

grupocontexto <- base_alf %>% 
  left_join(select(preguntas,-evaluacion), by = "pregunta") %>% 
  filter(evaluacion=="segunda") %>% 
  group_by(tratamiento,contexto,interpretacion) %>% 
  summarise(n=n())

# ggplot(grupocontexto,aes(x = contexto, y=n, fill = interpretacion))+
#   geom_bar(stat = "identity",position = "fill")+ 
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   coord_flip() +
#   facet_grid(~tratamiento)

grupocontextotally <- grupocontexto %>% group_by(tratamiento,contexto) %>% add_tally()

ggplot(grupocontextotally,aes(x = contexto, y=n, fill = interpretacion))+
  geom_bar(stat = "identity",position = "fill")+ 
  geom_text(aes(label=percent(signif(n/nn,2))),position = position_fill(vjust = .5))+
  coord_flip() +
  facet_grid(~tratamiento,labeller = label_both)

# Contexto y respuestas por pregunta

# Segunda evaluación
grupocontextorespuestas_2 <-  base_alf %>%
  left_join(select(preguntas,-evaluacion), by = "pregunta") %>% 
  filter(evaluacion=="segunda") %>% 
  group_by(tratamiento,contexto,pregunta,interpretacion) %>% 
  summarise(n=n()) 

# ggplot(grupocontextorespuestas_2, aes(x = pregunta, y = n, fill = interpretacion )) +
#   geom_bar(stat = "identity",position = "fill") +
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   coord_flip() +
#   facet_grid(contexto~tratamiento, scales = "free", space = "free")

grupocontextorespuestas_2tally <- grupocontextorespuestas_2 %>% group_by(tratamiento,contexto,pregunta) %>% add_tally()

ggplot(grupocontextorespuestas_2tally, aes(x = pregunta, y = n, fill = interpretacion )) +
  geom_bar(stat = "identity",position = "fill") +
  geom_text(aes(label=percent(signif(n/nn,2))),position = position_fill(vjust = .5))+
  coord_flip() +
  facet_grid(contexto~tratamiento, scales = "free", space = "free",labeller = label_both)








# PRIMER INTENTO Relacion entre genero y respuestas --------------------------------------

# TODO Separarlo a nivel general
names(postest_eval1) <- c("nombre","sexo","fecha_nacimiento","grado","pregunta","respuesta","interpretacion")

p <- ggplot(pretest_eval1, aes(x = pregunta, fill = interpretacion))
p + geom_bar(position = "dodge") + coord_flip() + ylab("Pregunta") + xlab("Frecuencia") #+ facet_grid(~sexo)
p + geom_bar(position = "dodge") + coord_flip() + ylab("Pregunta") + xlab("Frecuencia") + facet_grid(~sexo)

# TODO hacerla en general, poner frecuencias relativas
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


# TODO respecto a genero, sacar boxplots sobre grados y edades TODO es hacer

# En evaluación 2 se desar mirar si hay diferencias por el tipo de pregunta (simil, metáfora, metonimia)

base_alf1 <- filter(base_alf, tratamiento == "pretest")
salida <- list(prop.table(table(base_alf1$interpretacion, base_alf1$sexo)),prop.table(table(base_alf1$interpretacion, base_alf1$sexo),1), prop.table(table(base_alf1$interpretacion, base_alf1$sexo),2), (table(base_alf1$interpretacion, base_alf1$sexo)))
salida <- do.call(rbind, salida)
write.table(salida, "salida.txt")

salida1 <- table(base_alf1$interpretacion, base_alf1$sexo) %>% prop.table(.,1) %>% data.table() %>% setnames(., names(.), c("Interpretacion", "Sexo", "Porcentaje")) 
ggplot(salida1, aes(Interpretacion, Porcentaje, fill = Sexo)) + geom_bar(stat = "identity", position = "fill")+
  geom_text(aes(label=paste0(round(Porcentaje*100), "%")),position = position_fill(vjust = .5))

chisq.test(table(base_alf1$interpretacion, base_alf1$sexo))


