# Librerías ---------------------------------------------------------------

library("tidyverse", lib.loc = "~/R/win-library/3.4")
library("readxl", lib.loc = "~/R/win-library/3.4")
library("lubridate", lib.loc = "~/R/win-library/3.4")
library("scales", lib.loc = "~/R/win-library/3.4")

# Carga de datos ----------------------------------------------------------

Inicial_eval1 <-
  read_excel("../Datos/PRE_TEST_LENGUAJE FIGURADO_EVALUACION 1_Vf.xlsx")
Inicial_eval1 <- Inicial_eval1 %>%
  unite(
    "Mandragora disfrutaba de una vida sin dolores de cabeza:",
    "Mandragora disfrutaba de una vida sin dolores de cabeza:",
    "INTERPRETACION",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Mandragora ordeno su habitacion en un abrir y cerrar de ojos:",
    "Mandragora ordeno su habitacion en un abrir y cerrar de ojos:",
    "INTERPRETACION__1",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Circe pierde la cabeza por un buen postre:",
    "Circe pierde la cabeza por un buen postre:",
    "INTERPRETACION__2",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Mandragora se quedo con la boca abierta:",
    "Mandragora se quedo con la boca abierta:",
    "INTERPRETACION__3",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Abriendo los ojos como platos:",
    "Abriendo los ojos como platos:",
    "INTERPRETACION__4",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Le habian tomado el pelo:",
    "Le habian tomado el pelo:",
    "INTERPRETACION__5",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Se habia dado con la puerta en las narices:",
    "Se habia dado con la puerta en las narices:",
    "INTERPRETACION__6",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Gritando a todo pulmon:",
    "Gritando a todo pulmon:",
    "INTERPRETACION__7",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Las aguas del rio rugian como leones:",
    "Las aguas del rio rugian como leones:",
    "INTERPRETACION__8",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "La bruja con el corazon en la boca:",
    "La bruja con el corazon en la boca:",
    "INTERPRETACION__9",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Mandragora con los nervios a flor de piel:",
    "Mandragora con los nervios a flor de piel:",
    "INTERPRETACION__10",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Y asi como por arte de magia:",
    "Y asi como por arte de magia:",
    "INTERPRETACION__11",
    sep = "%",
    remove = TRUE
  ) %>%
  select(-contains("TIPO DE LENGUAJE FIGURADO")) %>%
  select(-contains("Marca temporal")) %>%
  gather(
    pregunta,
    respuesta,
    c(
      "Mandragora disfrutaba de una vida sin dolores de cabeza:",
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
      "Y asi como por arte de magia:"
    )
  ) %>%
  separate(respuesta,
           c("respuesta", "interpretacion"),
           sep = "%",
           remove = TRUE)
names(Inicial_eval1) <-
  c(
    "nombre",
    "sexo",
    "fecha_nacimiento",
    "grado",
    "pregunta",
    "respuesta",
    "interpretacion"
  )
Inicial_eval1$fecha_nacimiento <-
  ymd(Inicial_eval1$fecha_nacimiento)
Inicial_eval1[, "edad"] <-
  interval(Inicial_eval1$fecha_nacimiento, now()) %>%
  as.period() %>% # Puede ser as.duration?
  year() %>%
  as.numeric("years")
Inicial_eval1[, "caracterizacion"] <- "Caracterización 1"
Inicial_eval1[, "tratamiento"] <- "Inicial"

Final_eval1 <-
  read_excel("../Datos/POST_TEST_LENGUAJE FIGURADO_EVALUACION 1_Vf.xlsx")
Final_eval1 <- Final_eval1 %>%
  unite(
    "Mandragora disfrutaba de una vida sin dolores de cabeza:",
    "Mandragora disfrutaba de una vida sin dolores de cabeza:",
    "INTERPRETACION",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Mandragora ordeno su habitacion en un abrir y cerrar de ojos:",
    "Mandragora ordeno su habitacion en un abrir y cerrar de ojos:",
    "INTERPRETACION__1",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Circe pierde la cabeza por un buen postre:",
    "Circe pierde la cabeza por un buen postre:",
    "INTERPRETACION__2",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Mandragora se quedo con la boca abierta:",
    "Mandragora se quedo con la boca abierta:",
    "INTERPRETACION__3",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Abriendo los ojos como platos:",
    "Abriendo los ojos como platos:",
    "INTERPRETACION__4",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Le habian tomado el pelo:",
    "Le habian tomado el pelo:",
    "INTERPRETACION__5",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Se habia dado con la puerta en las narices:",
    "Se habia dado con la puerta en las narices:",
    "INTERPRETACION__6",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Gritando a todo pulmon:",
    "Gritando a todo pulmon:",
    "INTERPRETACION__7",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Las aguas del rio rugian como leones:",
    "Las aguas del rio rugian como leones:",
    "INTERPRETACION__8",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "La bruja con el corazon en la boca:",
    "La bruja con el corazon en la boca:",
    "INTERPRETACION__9",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Mandragora con los nervios a flor de piel:",
    "Mandragora con los nervios a flor de piel:",
    "INTERPRETACION__10",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Y asi como por arte de magia:",
    "Y asi como por arte de magia:",
    "INTERPRETACION__11",
    sep = "%",
    remove = TRUE
  ) %>%
  select(-contains("TIPO DE LENGUAJE FIGURADO")) %>%
  select(-contains("Marca temporal")) %>%
  gather(
    pregunta,
    respuesta,
    c(
      "Mandragora disfrutaba de una vida sin dolores de cabeza:",
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
      "Y asi como por arte de magia:"
    )
  ) %>%
  separate(respuesta,
           c("respuesta", "interpretacion"),
           sep = "%",
           remove = TRUE)
names(Final_eval1) <-
  c(
    "nombre",
    "sexo",
    "fecha_nacimiento",
    "grado",
    "pregunta",
    "respuesta",
    "interpretacion"
  )
Final_eval1$fecha_nacimiento <- ymd(Final_eval1$fecha_nacimiento)
Final_eval1[, "edad"] <-
  interval(Final_eval1$fecha_nacimiento, now()) %>%
  as.period() %>% # Puede ser as.duration?
  year() %>%
  as.numeric("years")
Final_eval1[, "caracterizacion"] <- "Caracterización 1"
Final_eval1[, "tratamiento"] <- "Final"

Inicial_eval2 <-
  read_excel("../Datos/PRE_TEST_LENGUAJE FIGURADO EVALUACION 2_Vf.xlsx")
Inicial_eval2 <- Inicial_eval2 %>%
  unite(
    "Si yo le digo a un niño la expresion \"no seas payaso\" le estoy queriendo decir que:",
    "Si yo le digo a un niño la expresion \"no seas payaso\" le estoy queriendo decir que:",
    "INTERPRETACION",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Si yo le digo a un niño: \"pareces un loro\" le estoy queriendo decir que:",
    "Si yo le digo a un niño: \"pareces un loro\" le estoy queriendo decir que:",
    "INTERPRETACION__1",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Si yo te digo que: \"quiero un abrazo de oso\" te estoy queriendo decir que:",
    "Si yo te digo que: \"quiero un abrazo de oso\" te estoy queriendo decir que:",
    "INTERPRETACION__2",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Si yo digo que \"Felipe se puso rojo como un tomate\" estoy queriendo decir que:",
    "Si yo digo que \"Felipe se puso rojo como un tomate\" estoy queriendo decir que:",
    "INTERPRETACION__3",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Si yo digo que \"Juan camina como una tortuga\" estoy queriendo decir que:",
    "Si yo digo que \"Juan camina como una tortuga\" estoy queriendo decir que:",
    "INTERPRETACION__4",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Si yo digo que \"esos dos niños mantienen como perros y gatos\" quiero decir que:",
    "Si yo digo que \"esos dos niños mantienen como perros y gatos\" quiero decir que:",
    "INTERPRETACION__5",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Si yo digo que \"si no te portas bien te voy a dar un jalar de orejas\" estoy queriendo decir que:",
    "Si yo digo que \"si no te portas bien te voy a dar un jalar de orejas\" estoy queriendo decir que:",
    "INTERPRETACION__6",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Si yo digo \"mi mama esta que echa fuego por la boca\" quiero decir que:",
    "Si yo digo \"mi mama esta que echa fuego por la boca\" quiero decir que:",
    "INTERPRETACION__7",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Si yo digo que \"mi vecina tiene la lengua larga\" quiero decir que:",
    "Si yo digo que \"mi vecina tiene la lengua larga\" quiero decir que:",
    "INTERPRETACION__8",
    sep = "%",
    remove = TRUE
  ) %>%
  select(-contains("TIPO DE LENGUAJE FIGURADO")) %>%
  select(-contains("Marca temporal")) %>%
  gather(
    pregunta,
    respuesta,
    c(
      "Si yo le digo a un niño la expresion \"no seas payaso\" le estoy queriendo decir que:",
      "Si yo le digo a un niño: \"pareces un loro\" le estoy queriendo decir que:",
      "Si yo te digo que: \"quiero un abrazo de oso\" te estoy queriendo decir que:",
      "Si yo digo que \"Felipe se puso rojo como un tomate\" estoy queriendo decir que:",
      "Si yo digo que \"Juan camina como una tortuga\" estoy queriendo decir que:",
      "Si yo digo que \"esos dos niños mantienen como perros y gatos\" quiero decir que:",
      "Si yo digo que \"si no te portas bien te voy a dar un jalar de orejas\" estoy queriendo decir que:",
      "Si yo digo \"mi mama esta que echa fuego por la boca\" quiero decir que:",
      "Si yo digo que \"mi vecina tiene la lengua larga\" quiero decir que:"
    )
  ) %>%
  separate(respuesta,
           c("respuesta", "interpretacion"),
           sep = "%",
           remove = TRUE)
names(Inicial_eval2) <-
  c(
    "nombre",
    "sexo",
    "fecha_nacimiento",
    "grado",
    "pregunta",
    "respuesta",
    "interpretacion"
  )
Inicial_eval2$fecha_nacimiento <-
  ymd(Inicial_eval2$fecha_nacimiento)
Inicial_eval2[, "edad"] <-
  interval(Inicial_eval2$fecha_nacimiento, now()) %>%
  as.period() %>% # Puede ser as.duration?
  year() %>%
  as.numeric("years")
Inicial_eval2[, "caracterizacion"] <- "Caracterización 2"
Inicial_eval2[, "tratamiento"] <- "Inicial"

Final_eval2 <-
  read_excel("../Datos/POST_TEST_LENGUAJE FIGURADO EVALUACION 2_Vf.xlsx")
Final_eval2 <- Final_eval2 %>%
  unite(
    "Si yo le digo a un niño la expresion \"no seas payaso\" le estoy queriendo decir que:",
    "Si yo le digo a un niño la expresion \"no seas payaso\" le estoy queriendo decir que:",
    "INTERPRETACION",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Si yo le digo a un niño: \"pareces un loro\" le estoy queriendo decir que:",
    "Si yo le digo a un niño: \"pareces un loro\" le estoy queriendo decir que:",
    "INTERPRETACION__1",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Si yo te digo que: \"quiero un abrazo de oso\" te estoy queriendo decir que:",
    "Si yo te digo que: \"quiero un abrazo de oso\" te estoy queriendo decir que:",
    "INTERPRETACION__2",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Si yo digo que \"Felipe se puso rojo como un tomate\" estoy queriendo decir que:",
    "Si yo digo que \"Felipe se puso rojo como un tomate\" estoy queriendo decir que:",
    "INTERPRETACION__3",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Si yo digo que \"Juan camina como una tortuga\" estoy queriendo decir que:",
    "Si yo digo que \"Juan camina como una tortuga\" estoy queriendo decir que:",
    "INTERPRETACION__4",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Si yo digo que \"esos dos niños mantienen como perros y gatos\" quiero decir que:",
    "Si yo digo que \"esos dos niños mantienen como perros y gatos\" quiero decir que:",
    "INTERPRETACION__5",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Si yo digo que \"si no te portas bien te voy a dar un jalar de orejas\" estoy queriendo decir que:",
    "Si yo digo que \"si no te portas bien te voy a dar un jalar de orejas\" estoy queriendo decir que:",
    "INTERPRETACION__6",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Si yo digo \"mi mama esta que echa fuego por la boca\" quiero decir que:",
    "Si yo digo \"mi mama esta que echa fuego por la boca\" quiero decir que:",
    "INTERPRETACION__7",
    sep = "%",
    remove = TRUE
  ) %>%
  unite(
    "Si yo digo que \"mi vecina tiene la lengua larga\" quiero decir que:",
    "Si yo digo que \"mi vecina tiene la lengua larga\" quiero decir que:",
    "INTERPRETACION__8",
    sep = "%",
    remove = TRUE
  ) %>%
  select(-contains("TIPO DE LENGUAJE FIGURADO")) %>%
  select(-contains("Marca temporal")) %>%
  gather(
    pregunta,
    respuesta,
    c(
      "Si yo le digo a un niño la expresion \"no seas payaso\" le estoy queriendo decir que:",
      "Si yo le digo a un niño: \"pareces un loro\" le estoy queriendo decir que:",
      "Si yo te digo que: \"quiero un abrazo de oso\" te estoy queriendo decir que:",
      "Si yo digo que \"Felipe se puso rojo como un tomate\" estoy queriendo decir que:",
      "Si yo digo que \"Juan camina como una tortuga\" estoy queriendo decir que:",
      "Si yo digo que \"esos dos niños mantienen como perros y gatos\" quiero decir que:",
      "Si yo digo que \"si no te portas bien te voy a dar un jalar de orejas\" estoy queriendo decir que:",
      "Si yo digo \"mi mama esta que echa fuego por la boca\" quiero decir que:",
      "Si yo digo que \"mi vecina tiene la lengua larga\" quiero decir que:"
    )
  ) %>%
  separate(respuesta,
           c("respuesta", "interpretacion"),
           sep = "%",
           remove = TRUE)
names(Final_eval2) <-
  c(
    "nombre",
    "sexo",
    "fecha_nacimiento",
    "grado",
    "pregunta",
    "respuesta",
    "interpretacion"
  )
Final_eval2$fecha_nacimiento <- ymd(Final_eval2$fecha_nacimiento)
Final_eval2[, "edad"] <-
  interval(Final_eval2$fecha_nacimiento, now()) %>%
  as.period() %>% # Puede ser as.duration?
  year() %>%
  as.numeric("years")
Final_eval2[, "caracterizacion"] <- "Caracterización 2"
Final_eval2[, "tratamiento"] <- "Final"


# Datos de preguntas
preguntas <-
  read_excel("../Datos/preguntas.xlsx", sheet = "preguntas")
preguntas$pregunta <- 
  ordered(
    preguntas$pregunta,
    levels = c(
      "Mandragora disfrutaba de una vida sin dolores de cabeza:",
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
      "Y asi como por arte de magia:",
      "Si yo le digo a un niño la expresion \"no seas payaso\" le estoy queriendo decir que:",
      "Si yo le digo a un niño: \"pareces un loro\" le estoy queriendo decir que:",
      "Si yo te digo que: \"quiero un abrazo de oso\" te estoy queriendo decir que:",
      "Si yo digo que \"Felipe se puso rojo como un tomate\" estoy queriendo decir que:",
      "Si yo digo que \"Juan camina como una tortuga\" estoy queriendo decir que:",
      "Si yo digo que \"esos dos niños mantienen como perros y gatos\" quiero decir que:",
      "Si yo digo que \"si no te portas bien te voy a dar un jalar de orejas\" estoy queriendo decir que:",
      "Si yo digo \"mi mama esta que echa fuego por la boca\" quiero decir que:",
      "Si yo digo que \"mi vecina tiene la lengua larga\" quiero decir que:"
    )
  )
preguntas$pregunta_corta <- 
  ordered(
    preguntas$pregunta_corta,
    levels = c(
      "Mandragora disfrutaba de una vida sin dolores de cabeza",
      "Mandragora ordeno su habitacion en un abrir y cerrar de ojos",
      "Circe pierde la cabeza por un buen postre",
      "Mandragora se quedo con la boca abierta",
      "Abriendo los ojos como platos",
      "Le habian tomado el pelo",
      "Se habia dado con la puerta en las narices",
      "Gritando a todo pulmon",
      "Las aguas del rio rugian como leones",
      "La bruja con el corazon en la boca",
      "Mandragora con los nervios a flor de piel",
      "Y asi como por arte de magia",
      "No seas payaso",
      "Pareces un loro",
      "Quiero un abrazo de oso",
      "Se puso rojo como un tomate",
      "Camina como una tortuga",
      "Mantienen como perros y gatos",
      "Te voy a dar un jalón de orejas",
      "Está que echa fuego por la boca",
      "Tiene la lengua larga"
    )
  )
names(preguntas) <- c("pregunta","figura","caracterizacion","pregunta_corta")
preguntas <- preguntas %>% 
  mutate(
    figura = case_when(
      figura == "SIMIL" ~ "Símil",
      figura == "METAFORA" ~ "Metáfora",
      figura == "METONIMIA" ~ "Metonimia"
    )
  )

# Tabla de datos consolidada
base_alf <- Inicial_eval1 %>%
  bind_rows(Final_eval1) %>%
  bind_rows(Inicial_eval2) %>%
  bind_rows(Final_eval2)

base_alf <- base_alf %>% 
  mutate(
    grado = case_when(
      grado == "TRANSICION" ~ "Transición",
      grado == "JARDIN" ~ "Jardín"
    )
  )

base_alf <- base_alf %>% 
  mutate(
    interpretacion = case_when(
      interpretacion == "LITERAL" ~ "Literal",
      interpretacion == "FIGURADO" ~ "Figurativa",
      interpretacion == "DISTRACTOR" ~ "Distractor"
    )
  )

base_alf$tratamiento <-
  ordered(base_alf$tratamiento, levels = c("Inicial", "Final"))
base_alf$pregunta <-
  ordered(
    base_alf$pregunta,
    levels = c(
      "Mandragora disfrutaba de una vida sin dolores de cabeza:",
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
      "Y asi como por arte de magia:",
      "Si yo le digo a un niño la expresion \"no seas payaso\" le estoy queriendo decir que:",
      "Si yo le digo a un niño: \"pareces un loro\" le estoy queriendo decir que:",
      "Si yo te digo que: \"quiero un abrazo de oso\" te estoy queriendo decir que:",
      "Si yo digo que \"Felipe se puso rojo como un tomate\" estoy queriendo decir que:",
      "Si yo digo que \"Juan camina como una tortuga\" estoy queriendo decir que:",
      "Si yo digo que \"esos dos niños mantienen como perros y gatos\" quiero decir que:",
      "Si yo digo que \"si no te portas bien te voy a dar un jalar de orejas\" estoy queriendo decir que:",
      "Si yo digo \"mi mama esta que echa fuego por la boca\" quiero decir que:",
      "Si yo digo que \"mi vecina tiene la lengua larga\" quiero decir que:"
    )
  )


# Análisis de información personal----------------------------------------------------------------

#write_excel_csv(distinct(base_alf[,"nombre"]),"../Datos/correcion_nombres.xlsx")
distinct(base_alf[, "nombre"]) # Nombres de niños
nombres <- distinct(Inicial_eval1[, "nombre"]) %>%
  full_join(distinct(Final_eval1[, "nombre"]), copy = T) %>%
  full_join(distinct(Inicial_eval2[, "nombre"]), copy = T) %>%
  full_join(distinct(Final_eval2[, "nombre"]), copy = T)

ip_Inicial <-
  base_alf[, c("nombre", "sexo", "fecha_nacimiento", "edad", "grado")] %>%
  distinct() %>%
  count(grado, edad, sexo)
write_excel_csv(ip_Inicial, "../Datos/caracterizacion_encuestados.csv")

gruposexo <- ip_Inicial %>% group_by(sexo) %>% summarise(n = sum(n))
gruposexo
grupoedad <- ip_Inicial %>% group_by(edad) %>% summarise(n = sum(n))
grupoedad
grupogrado <-
  ip_Inicial %>% group_by(grado) %>% summarise(n = sum(n))
grupogrado

grupoedadgrado <-
  ip_Inicial %>% group_by(edad, grado) %>% summarise(n = sum(n))
grupoedadgrado

grupoedadsexo <-
  ip_Inicial %>% group_by(edad, sexo) %>% summarise(n = sum(n))
grupoedadsexo

# ggplot(grupoedadsexo, aes(x=edad,y=factor(n),fill=sexo)) +
#   geom_bar(stat = "identity",position="dodge") +
#   geom_text(aes(label=n), position=position_dodge(width = 1), vjust=-0.4)
#
# ggplot(ip_Inicial, aes(x=edad,y=n,fill=sexo)) +
#   geom_bar(stat = "identity",position="dodge") +
#   geom_text(aes(label=n), position=position_dodge(width = 1),vjust=-0.4) +
#   facet_grid(.~grado,labeller = label_both)


# Análisis general de interpretación de respuestas POR EVALUACIÓN ---------

gruporespuestas_solo <-  base_alf %>%
  left_join(select(preguntas,-caracterizacion), by = "pregunta") %>%
  group_by(tratamiento, caracterizacion, interpretacion) %>%
  summarise(n = n())
gruporespuestas_solo
gruporespuestas_solo$interpretacion <-
  ordered(gruporespuestas_solo$interpretacion,
          c("Figurativa", "Literal", "Distractor"))

gruporespuestas_solotally <-
  gruporespuestas_solo %>% group_by(tratamiento, caracterizacion) %>% add_tally()

cbPalette <- c("#56B4E9",  "#009E73", "#CC79A7", "#999999", "#F0E442", "#0072B2", "#D55E00", "#E69F00")
cbbPalette <- c("#56B4E9", "#D55E00", "#CC79A7", "#999999", "#F0E442", "#0072B2",  "#009E73", "#E69F00")

ggplot(gruporespuestas_solotally,
       aes(x = tratamiento, y = n, fill = interpretacion)) +
  #scale_fill_manual("Interpretación",values=cbPalette)+
  scale_fill_manual("Interpretación",values=c("#479FFF","#FB7377","#00B826"))+
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = percent(signif(n / nn, 2))), position = position_fill(vjust = .5)) +
  scale_y_continuous(labels = scales::percent)+
  #coord_flip() +
  facet_grid(~ caracterizacion) +
  labs(x = "", y = "Porcentaje")+
  theme(text=element_text(size=11))

ungroup(gruporespuestas_solotally)

gruporespuestas_solotallyd <- gruporespuestas_solotally %>%
  ungroup() %>%
  mutate(interpretacion = case_when(interpretacion == "Figurativa" ~ "Figurativa",
                                    TRUE ~ "No Figurativa")) %>%
  select(-nn) %>%
  group_by(tratamiento, caracterizacion, interpretacion) %>%
  summarize(sum = sum(n)) %>%
  group_by(tratamiento, caracterizacion) %>% add_tally(sum)

ggplot(gruporespuestas_solotallyd,
       aes(x = tratamiento, y = sum, fill = interpretacion)) +
  scale_fill_hue("Interpretación",h.start = 236)+
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = percent(signif(sum / n, 2))), position = position_fill(vjust = .5)) +
  scale_y_continuous(labels = scales::percent)+
  #coord_flip() +
  facet_grid(~ caracterizacion) +
  labs(x = "", y = "Porcentaje")

# Caracterizacion Primera evaluación
gruporespuestas_1 <-  base_alf %>%
  left_join(select(preguntas,-caracterizacion), by = "pregunta") %>%
  group_by(tratamiento, caracterizacion, pregunta_corta, interpretacion) %>%
  summarise(n = n()) %>%
  filter(caracterizacion == "Caracterización 1")
gruporespuestas_1

gruporespuestas_1tally <-
  gruporespuestas_1 %>% 
  group_by(tratamiento, caracterizacion, pregunta_corta) %>% 
  add_tally()
gruporespuestas_1tally$interpretacion <-
  ordered(gruporespuestas_1tally$interpretacion,
          c("Figurativa", "Literal", "Distractor"))
gruporespuestas_1tally$pregunta_corta <- droplevels(gruporespuestas_1tally$pregunta_corta)

ggplot(gruporespuestas_1tally,
  aes(x = forcats::fct_rev(pregunta_corta), y = n, fill = interpretacion)) +
  scale_fill_hue("Interpretación",h.start = 236)+
  geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
  geom_text(aes(label = percent(signif(n / nn, 2))), position = position_fill(reverse = TRUE,vjust = .5)) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent)+
  facet_grid(~ tratamiento,scales = "free",space = "free")+
  scale_x_discrete(position="top")+
  theme(legend.position="left")+
  labs(x="Pregunta",y="Porcentaje")
  
# Caracterización 2 evaluación
gruporespuestas_2 <-  base_alf %>%
  left_join(select(preguntas,-caracterizacion), by = "pregunta") %>%
  group_by(tratamiento, caracterizacion, pregunta_corta, interpretacion) %>%
  summarise(n = n()) %>%
  filter(caracterizacion == "Caracterización 2")
gruporespuestas_2

gruporespuestas_2tally <-
  gruporespuestas_2 %>% 
  group_by(tratamiento, caracterizacion, pregunta_corta) %>% 
  add_tally()
gruporespuestas_2tally$interpretacion <-
  ordered(gruporespuestas_2tally$interpretacion,
          c("Figurativa", "Literal", "Distractor"))
gruporespuestas_2tally$pregunta <- droplevels(gruporespuestas_2tally$pregunta_corta)

ggplot(gruporespuestas_2tally,
       aes(x = forcats::fct_rev(pregunta), y = n, fill = interpretacion)) +
  scale_fill_manual("Interpretación",values=c("#479FFF","#FB7377","#00B826"))+
  geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
  geom_text(aes(label = percent(signif(n / nn, 2))), position = position_fill(reverse = TRUE,vjust = .5)) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent)+
  facet_grid(~ tratamiento,scales = "free",space = "free")+
  scale_x_discrete(position="top")+
  theme(legend.position="left", text=element_text(family="Times"))+
  labs(x="Pregunta",y="Porcentaje")

# Análisis por Edad  ------------------------------------------------------

grupoedadinterpretacion <- base_alf %>%
  group_by(tratamiento, caracterizacion, edad, interpretacion) %>%
  summarise(n = n()) 
grupoedadinterpretacion$interpretacion <-
  ordered(grupoedadinterpretacion$interpretacion,
          c("Figurativa", "Literal", "Distractor"))
grupoedadinterpretacion

grupoedadinterpretaciontally <-
  grupoedadinterpretacion %>% group_by(tratamiento, caracterizacion, edad) %>% add_tally()
grupoedadinterpretaciontally

ggplot(grupoedadinterpretaciontally,
       aes(x = edad, y = n, fill = interpretacion)) +
  #scale_fill_manual("Interpretación",values=cbPalette)+
  scale_fill_hue("Interpretación",h.start = 236)+
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = percent(signif(n / nn, 2))), position = position_fill(vjust = .5)) +
  scale_y_continuous(labels = scales::percent)+
  #coord_flip() +
  facet_grid(caracterizacion ~ tratamiento) +
  labs(x = "Edad (Años)", y = "Porcentaje")


#Clasificando entre figurado y no figurado
grupoedadinterpretaciontally2 <- grupoedadinterpretacion %>%
  mutate(interpretacion = case_when(interpretacion == "Figurativa" ~ "Figurativa",
                                    TRUE ~ "No Figurativa")) %>%
  group_by(tratamiento, caracterizacion, edad, interpretacion) %>%
  summarise(sum = sum(n)) %>%
  add_tally(sum)
grupoedadinterpretaciontally2

ggplot(grupoedadinterpretaciontally2,
       aes(x = edad, y = sum, fill = interpretacion)) +
  scale_fill_hue("Interpretación",h.start = 236)+
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = percent(signif(sum / n, 2))), position = position_fill(vjust = .5)) +
  #coord_flip() +
  scale_y_continuous(labels = scales::percent)+
  facet_grid(caracterizacion~ tratamiento,scales = "free",space = "free")+
  labs(x="Edad (Años)",y="Porcentaje")


ggplot(grupoedadinterpretaciontally2,
       aes(x = edad, y = sum, fill = interpretacion)) +
  #scale_fill_manual("Interpretación",values=cbPalette)+
  scale_fill_hue("Interpretación",h.start = 236)+
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = percent(signif(sum / n, 2))), position = position_fill(vjust = .5)) +
  scale_y_continuous(labels = scales::percent)+
  #coord_flip() +
  facet_grid(caracterizacion ~ tratamiento) +
  labs(x = "Edad (Años)", y = "Porcentaje")

# Edad y respuestas por pregunta

# Primera evaluación
grupoedadrespuestas_1 <-  base_alf %>%
  filter(caracterizacion == "Caracterización 1") %>%
  left_join(select(preguntas,-caracterizacion), by = "pregunta") %>%
  group_by(tratamiento, edad, pregunta, interpretacion) %>%
  summarise(n = n())
grupoedadrespuestas_1

# ggplot(grupoedadrespuestas_1, aes(x = pregunta, y = n, fill = interpretacion )) +
#   geom_bar(stat = "identity",position = "fill") +
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   coord_flip() +
#   facet_grid(edad~tratamiento)

grupoedadrespuestas_1tally <-
  grupoedadrespuestas_1 %>% group_by(tratamiento, edad, pregunta) %>% add_tally()

ggplot(grupoedadrespuestas_1tally,
       aes(x = pregunta, y = n, fill = interpretacion)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = percent(signif(n / nn, 2))), position = position_fill(vjust = .5)) +
  coord_flip() +
  facet_grid(edad ~ tratamiento, labeller = label_both)

# Caracterización 2 evaluación
grupoedadrespuestas_2 <-  base_alf %>%
  filter(caracterizacion == "Caracterización 2") %>%
  left_join(select(preguntas,-caracterizacion), by = "pregunta") %>%
  group_by(tratamiento, edad, pregunta, interpretacion) %>%
  summarise(n = n())

# ggplot(grupoedadrespuestas_2, aes(x = pregunta, y = n, fill = interpretacion )) +
#   geom_bar(stat = "identity",position = "fill") +
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   coord_flip() +
#   facet_grid(edad~tratamiento)

grupoedadrespuestas_2tally <-
  grupoedadrespuestas_2 %>% group_by(tratamiento, edad, pregunta) %>% add_tally()

ggplot(grupoedadrespuestas_2tally,
       aes(x = pregunta, y = n, fill = interpretacion)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = percent(signif(n / nn, 2))), position = position_fill(vjust = .5)) +
  coord_flip() +
  facet_grid(edad ~ tratamiento, labeller = label_both)

ggplot(grupoedadrespuestas_2tally,
       aes(x = edad, y = n, fill = interpretacion)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = percent(signif(n / nn, 2))), position = position_fill(vjust = .5)) +
  #coord_flip() +
  facet_grid(tratamiento ~ pregunta, labeller = label_both)

# Análisis por Grado  ------------------------------------------------------

grupogradointerpretacion_1 <- base_alf %>%
  group_by(tratamiento, caracterizacion, grado, interpretacion) %>%
  summarise(n = n())

grupogradointerpretacion_1$interpretacion <-grupogradointerpretacion_1$interpretacion %>% 
  ordered(c("Figurativa","Literal", "Distractor"))


# ggplot(grupogradointerpretacion_1,aes(x = grado, y=n, fill = interpretacion))+
#   geom_bar(stat = "identity",position = "fill")+
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   facet_grid(tratamiento~caracterizacion)

grupogradointerpretacion_1tally <-
  grupogradointerpretacion_1 %>% group_by(tratamiento, caracterizacion, grado) %>% add_tally()
grupogradointerpretacion_1tally

ggplot(grupogradointerpretacion_1tally,
       aes(x = grado, y = n, fill = interpretacion)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = percent(signif(n / nn, 2))), position = position_fill(vjust = .5)) +
  facet_grid(caracterizacion ~ tratamiento, labeller = label_both)

ggplot(grupogradointerpretacion_1tally,
       aes(x = grado, y = n, fill = interpretacion)) +
  #scale_fill_manual("Interpretación",values=cbPalette)+
  scale_fill_hue("Interpretación",h.start = 236)+
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = percent(signif(n / nn, 2))), position = position_fill(vjust = .5)) +
  scale_y_continuous(labels = scales::percent)+
  #coord_flip() +
  facet_grid(caracterizacion ~ tratamiento) +
  labs(x = "Grado", y = "Porcentaje")

#Clasificando entre figurado y no figurado
grupogradointerpretaciontally2 <- grupogradointerpretacion_1 %>%
  mutate(interpretacion = case_when(interpretacion == "Figurativa" ~ "Figurativa",
                                    TRUE ~ "No Figurativa")) %>%
  group_by(tratamiento, caracterizacion, grado, interpretacion) %>%
  summarise(sum = sum(n)) %>%
  add_tally(sum)
grupogradointerpretaciontally2

ggplot(grupogradointerpretaciontally2,
       aes(x = grado, y = sum, fill = interpretacion)) +
  #scale_fill_manual("Interpretación",values=cbPalette)+
  scale_fill_hue("Interpretación",h.start = 236)+
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = percent(signif(sum / n, 2))), position = position_fill(vjust = .5)) +
  scale_y_continuous(labels = scales::percent)+
  #coord_flip() +
  facet_grid(caracterizacion ~ tratamiento) +
  labs(x = "Grado", y = "Porcentaje")

ggplot(grupogradointerpretaciontally2,
       aes(x = grado, y = sum, fill = interpretacion)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = percent(signif(sum / n, 2))), position = position_fill(vjust = .5)) +
  facet_grid(caracterizacion ~ tratamiento, labeller = label_both)

# Grado y respuestas por pregunta

# Primera evaluación
grupogradorespuestas_1 <-  base_alf %>%
  filter(caracterizacion == "Caracterización 1") %>%
  left_join(select(preguntas,-caracterizacion), by = "pregunta") %>%
  group_by(tratamiento, grado, pregunta, interpretacion) %>%
  summarise(n = n())
grupogradorespuestas_1

# ggplot(grupogradorespuestas_1, aes(x = pregunta, y = n, fill = interpretacion )) +
#   geom_bar(stat = "identity",position = "fill") +
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   coord_flip() +
#   facet_grid(grado~tratamiento)

grupogradorespuestas_1tally <-
  grupogradorespuestas_1 %>% group_by(tratamiento, grado, pregunta) %>% add_tally()

ggplot(grupogradorespuestas_1tally,
       aes(x = pregunta, y = n, fill = interpretacion)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = percent(signif(n / nn, 2))), position = position_fill(vjust = .5)) +
  coord_flip() +
  facet_grid(grado ~ tratamiento, labeller = label_both)

# Caracterización 2 evaluación
grupogradorespuestas_2 <-  base_alf %>%
  filter(caracterizacion == "Caracterización 2") %>%
  left_join(select(preguntas,-caracterizacion), by = "pregunta") %>%
  group_by(tratamiento, grado, pregunta, interpretacion) %>%
  summarise(n = n())

# ggplot(grupogradorespuestas_2, aes(x = pregunta, y = n, fill = interpretacion )) +
#   geom_bar(stat = "identity",position = "fill") +
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   coord_flip() +
#   facet_grid(grado~tratamiento)

grupogradorespuestas_2tally <-
  grupogradorespuestas_2 %>% group_by(tratamiento, grado, pregunta) %>% add_tally()

ggplot(grupogradorespuestas_2tally,
       aes(x = pregunta, y = n, fill = interpretacion)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = percent(signif(n / nn, 2))), position = position_fill(vjust = .5)) +
  coord_flip() +
  facet_grid(grado ~ tratamiento, labeller = label_both)

# Análisis por Sexo -----------------------------------------------------

gruposexointerpretacion <- base_alf %>%
  group_by(tratamiento, caracterizacion, sexo, interpretacion) %>%
  summarise(n = n())
gruposexointerpretacion$interpretacion <-
  ordered(gruposexointerpretacion$interpretacion,
          c("Figurativa", "Literal", "Distractor"))
gruposexointerpretacion

# ggplot(gruposexointerpretacion,aes(x = sexo, y=n, fill = interpretacion))+
#   geom_bar(stat = "identity",position = "fill")+
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   facet_grid(tratamiento~caracterizacion)

gruposexointerpretaciontally <-
  gruposexointerpretacion %>% group_by(tratamiento, caracterizacion, sexo) %>% add_tally()

ggplot(gruposexointerpretaciontally,
       aes(x = sexo, y = n, fill = interpretacion)) +
  #scale_fill_manual("Interpretación",values=cbPalette)+
  scale_fill_hue("Interpretación",h.start = 236)+
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = percent(signif(n / nn, 2))), position = position_fill(vjust = .5)) +
  scale_y_continuous(labels = scales::percent)+
  #coord_flip() +
  facet_grid(caracterizacion ~ tratamiento) +
  labs(x = "Sexo", y = "Porcentaje")

#Clasificando entre figurado y no figurado
gruposexointerpretaciontally2 <- gruposexointerpretacion %>%
  mutate(interpretacion = case_when(interpretacion == "Figurativa" ~ "Figurativa",
                                    TRUE ~ "No Figurativa")) %>%
  group_by(tratamiento, caracterizacion, sexo, interpretacion) %>%
  summarise(sum = sum(n)) %>%
  add_tally(sum)
gruposexointerpretaciontally2

ggplot(gruposexointerpretaciontally2,
       aes(x = sexo, y = sum, fill = interpretacion)) +
  #scale_fill_manual("Interpretación",values=cbPalette)+
  scale_fill_hue("Interpretación",h.start = 236)+
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = percent(signif(sum / n, 2))), position = position_fill(vjust = .5)) +
  scale_y_continuous(labels = scales::percent)+
  #coord_flip() +
  facet_grid(caracterizacion ~ tratamiento) +
  labs(x = "Sexo", y = "Porcentaje")

# Sexo y respuestas por pregunta

# Primera evaluación
gruposexorespuestas_1 <-  base_alf %>%
  filter(caracterizacion == "Caracterización 1") %>%
  left_join(select(preguntas,-caracterizacion), by = "pregunta") %>%
  group_by(tratamiento, sexo, pregunta, interpretacion) %>%
  summarise(n = n())

# ggplot(gruposexorespuestas_1, aes(x = pregunta, y = n, fill = interpretacion )) +
#   geom_bar(stat = "identity",position = "fill") +
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   coord_flip() +
#   facet_grid(sexo~tratamiento)

gruposexorespuestas_1tally <-
  gruposexorespuestas_1 %>% group_by(tratamiento, sexo, pregunta) %>% add_tally()

ggplot(gruposexorespuestas_1tally,
       aes(x = pregunta, y = n, fill = interpretacion)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = percent(signif(n / nn, 2))), position = position_fill(vjust = .5)) +
  coord_flip() +
  facet_grid(sexo ~ tratamiento, labeller = label_both)

# Caracterización 2 evaluación
gruposexorespuestas_2 <-  base_alf %>%
  filter(caracterizacion == "Caracterización 2") %>%
  left_join(select(preguntas,-caracterizacion), by = "pregunta") %>%
  group_by(tratamiento, sexo, pregunta, interpretacion) %>%
  summarise(n = n())

# ggplot(gruposexorespuestas_2, aes(x = pregunta, y = n, fill = interpretacion )) +
#   geom_bar(stat = "identity",position = "fill") +
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   coord_flip() +
#   facet_grid(sexo~tratamiento)

gruposexorespuestas_2tally <-
  gruposexorespuestas_2 %>% group_by(tratamiento, sexo, pregunta) %>% add_tally()

ggplot(gruposexorespuestas_2tally,
       aes(x = pregunta, y = n, fill = interpretacion)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = percent(signif(n / nn, 2))), position = position_fill(vjust = .5)) +
  coord_flip() +
  facet_grid(sexo ~ tratamiento, labeller = label_both)

# Análisis por figura ---------------------------------------------------

grupofigura <- base_alf %>%
  left_join(select(preguntas,-caracterizacion), by = "pregunta") %>%
  filter(caracterizacion == "Caracterización 2") %>%
  group_by(tratamiento, figura, interpretacion) %>%
  summarise(n = n())

# ggplot(grupofigura,aes(x = figura, y=n, fill = interpretacion))+
#   geom_bar(stat = "identity",position = "fill")+
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   coord_flip() +
#   facet_grid(~tratamiento)

grupofiguratally <-
  grupofigura %>% group_by(tratamiento, figura) %>% add_tally()

ggplot(grupofiguratally, aes(x = figura, y = n, fill = interpretacion)) +
  scale_fill_manual("Interpretación",values=c("#479FFF","#FB7377","#00B826"))+
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = percent(signif(n / nn, 2))), position = position_fill(vjust = .5)) +
  scale_y_continuous(labels = scales::percent)+
  coord_flip() +
  labs(x = "Figura Literaria", y = "Porcentaje")+
  facet_grid(~ tratamiento,scales = "free",space = "free")
  
ggplot(grupofiguratally, aes(x = figura, y = n, fill = interpretacion)) +
  scale_fill_manual("Interpretación",values=c("#479FFF","#FB7377","#00B826"))+
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = percent(signif(n / nn, 2))), position = position_fill(vjust = .5)) +
  scale_y_continuous(labels = scales::percent)+
  #coord_flip() +
  labs(x = "Tipo de lenguaje figurado", y = "Porcentaje")+
  facet_grid(~ tratamiento,scales = "free",space = "free")
  
# figura y respuestas por pregunta

# Caracterización 2 evaluación
grupofigurarespuestas_2 <-  base_alf %>%
  left_join(select(preguntas,-caracterizacion), by = "pregunta") %>%
  filter(caracterizacion == "Caracterización 2") %>%
  group_by(tratamiento, figura, pregunta, interpretacion) %>%
  summarise(n = n())

# ggplot(grupofigurarespuestas_2, aes(x = pregunta, y = n, fill = interpretacion )) +
#   geom_bar(stat = "identity",position = "fill") +
#   geom_text(aes(label=n),position = position_fill(vjust = .5))+
#   coord_flip() +
#   facet_grid(figura~tratamiento, scales = "free", space = "free")

grupofigurarespuestas_2tally <-
  grupofigurarespuestas_2 %>% group_by(tratamiento, figura, pregunta) %>% add_tally()

ggplot(grupofigurarespuestas_2tally,
       aes(x = pregunta, y = n, fill = interpretacion)) +
  scale_fill_manual("Interpretación",values=c("#479FFF","#FB7377","#00B826"))+
  geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
  geom_text(aes(label = percent(signif(n / nn, 2))), position = position_fill(reverse = TRUE,vjust = .5)) +
  scale_y_continuous(labels = scales::percent)+
  coord_flip() +
  labs(x = "Pregunta", y = "Porcentaje")+
  facet_grid(
    figura ~ tratamiento,
    scales = "free",
    space = "free"
  )

ggplot(grupofigurarespuestas_2tally,
       aes(x = pregunta, y = n, fill = interpretacion)) +
  scale_fill_brewer()+
  geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
  geom_text(aes(label = percent(signif(n / nn, 2))), position = position_fill(reverse = TRUE,vjust = .5)) +
  scale_y_continuous(labels = scales::percent)+
  coord_flip() +
  labs(x = "Pregunta", y = "Porcentaje")+
  facet_grid(
    figura ~ tratamiento,
    scales = "free",
    space = "free"
  )+
  theme_light()
# PRIMER INTENTO Relacion entre genero y respuestas --------------------------------------

# TODO Separarlo a nivel general
names(Final_eval1) <-
  c(
    "nombre",
    "sexo",
    "fecha_nacimiento",
    "grado",
    "pregunta",
    "respuesta",
    "interpretacion"
  )

p <- ggplot(Inicial_eval1, aes(x = pregunta, fill = interpretacion))
p + geom_bar(position = "dodge") + coord_flip() + ylab("Pregunta") + xlab("Frecuencia") #+ facet_grid(~sexo)
p + geom_bar(position = "dodge") + coord_flip() + ylab("Pregunta") + xlab("Frecuencia") + facet_grid(~
                                                                                                       sexo)

# TODO hacerla en general, poner frecuencias relativas
p <- ggplot(Inicial_eval1, aes(x = pregunta, fill = sexo))
p + geom_bar(position = "dodge") + coord_flip() + ylab("Pregunta") + xlab("Frecuencia") + facet_grid(~
                                                                                                       interpretacion)


# Final -----------------------------------------------------------------


p <- ggplot(Final_eval1, aes(x = pregunta, fill = interpretacion))
p + geom_bar(position = "dodge") + coord_flip() + ylab("Pregunta") + xlab("Frecuencia") + facet_grid(~
                                                                                                       sexo)

p <- ggplot(Final_eval1, aes(x = pregunta, fill = sexo))
p + geom_bar(position = "dodge") + coord_flip() + ylab("Pregunta") + xlab("Frecuencia") + facet_grid(~
                                                                                                       interpretacion)


# Comparacion post y pre --------------------------------------------------

base1 <-
  rbind(
    data.frame(
      resptesta = c(Inicial_eval1$interpretacion),
      tipo = "Inicial",
      genero = Inicial_eval1$sexo
    ),
    data.frame(
      resptesta = c(Final_eval1$interpretacion),
      tipo = "Final",
      genero = Final_eval1$sexo
    )
  )

p <- ggplot(base1, aes(x = resptesta, fill = tipo))
p + geom_bar(position = "dodge") + coord_flip() + ylab("Pregunta") + xlab("Frecuencia") + facet_grid(~
                                                                                                       genero)


p <- ggplot(Final_eval1, aes(x = pregunta, fill = interpretacion))
p + geom_bar() + coord_flip() + ylab("Pregunta") + xlab("Frecuencia")

ggplot(Inicial_eval1, aes(x = Inicial_eval1$fecha_nacimiento)) + geom_bar()

table(Inicial_eval1$interpretacion , Inicial_eval1$pregunta)
table(Final_eval1$interpretacion , Final_eval1$pregunta)

chisq.test(base1$resptesta, base1$tipo)


ggplot(base1, aes(resptesta, fill = tipo)) + geom_bar(position = "dodge")

p <- ggplot(Final_eval1, aes(x = pregunta, fill = interpretacion))
p + geom_bar(position = "dodge") + coord_flip() + ylab("Pregunta") + xlab("Frecuencia") + facet_grid(~
                                                                                                       sexo)


# TODO respecto a genero, sacar boxplots sobre grados y edades TODO es hacer

# En evaluación 2 se desar mirar si hay diferencias por el tipo de pregunta (simil, metáfora, metonimia)

base_alf1 <- filter(base_alf, tratamiento == "Inicial")
salida <-
  list(prop.table(table(
    base_alf1$interpretacion, base_alf1$sexo
  )),
  prop.table(table(
    base_alf1$interpretacion, base_alf1$sexo
  ), 1),
  prop.table(table(
    base_alf1$interpretacion, base_alf1$sexo
  ), 2),
  (table(
    base_alf1$interpretacion, base_alf1$sexo
  )))
salida <- do.call(rbind, salida)
write.table(salida, "salida.txt")

salida1 <-
  table(base_alf1$interpretacion, base_alf1$sexo) %>% prop.table(., 1) %>% data.table() %>% setnames(., names(.), c("Interpretacion", "Sexo", "Porcentaje"))
ggplot(salida1, aes(Interpretacion, Porcentaje, fill = Sexo)) + geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = paste0(round(Porcentaje * 100), "%")), position = position_fill(vjust = .5))

chisq.test(table(base_alf1$interpretacion, base_alf1$sexo))
