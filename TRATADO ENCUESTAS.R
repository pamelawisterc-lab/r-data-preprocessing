# ==============================================================================
# PROYECTO DE INVESTIGACIÓN: SALUD VISUAL Y HÁBITOS DIGITALES
# ==============================================================================

# ==============================================================================
# BLOQUE 1: CONFIGURACIÓN Y CARGA DE DATOS
# ==============================================================================
# install.packages("dplyr")
# install.packages("DataExplorer")
library(dplyr)
library(readxl)
library(DataExplorer)
library(ggplot2)
library(tidyr)
library(stringr)
library(gtsummary)
library(corrplot)
library(reshape2)

# Importar base de datos y quitar fila de metadatos
DS_SV <- read_excel("Downloads/ULT_ACT_CUEST.xlsx")
DS_SV <- DS_SV[-1,]
#View(DS_SV)

# data.frame(Numero = 1:ncol(DS_SV), Variable = names(DS_SV))

# ==============================================================================
# BLOQUE 2: LIMPIEZA Y TRANSFORMACIÓN DE VARIABLES
# ==============================================================================

# 2.1 Renombrar columnas masivamente
DS_SV <- DS_SV %>%
  rename( Aviso = `He leído el aviso de privacidad y acepto participar en esta investigación.`,
          Licenciatura = `¿Qué licenciatura de DACyTi estás cursando?`,
          Semestre = `¿Qué semestre está cursando?`,
          Otra_division = `Si no estudias en DACyTI, ¿Qué estudias y en dónde? Por favor escribe "LICENCIATURA/POSGRADO, DIVISIÓN". Por ejemplo: "ACT, DACB"`,
          Posgrado = `¿Qué posgrado de DACyTi estas cursando?`,
          Trabajo = `¿Trabajas? Selecciona el área.`,
          Tiempo_Lentes = `¿Desde cuándo usas lentes?`,
          Lentes =`¿Usas lentes con alguna(s) de estas caractarísticas?`,
          Oftalmologo= `¿Has visitado a un oftalmólogo? ¿Con qué frencuencia?`,
          Examen_vista= `¿Te has hecho algún examen de graduación o examen de salud ocular? ¿Cuál?`,
          Frecuencia_examen_vista= `¿Con qué frecuencia te realizas exámenes de la vista? `,
          Oft_Opt= `¿Has sido atendido por un oftalmólogo (especialista que puede tratar problemas médicos complejos relacionados con la vista, llevar a cabo procedimientos correctivos y cirugías) o un optometrista (médi`,
          Enf_ocular= `¿Has sido diagnosticado con alguna enfermedad, afecciones o emergencia ocular? ¿Cuál?`,
          Condicion= `¿Has sido diagnosticado médicamente con alguna de las siguientes condiciones?`,
          Enf_fam= `Selecciona todas las enfermedades oculares diagnosticadas en tu familia directa (papás, hermanos o abuelos) que apliquen:`,
          Condicion_fam= `¿Algún miembro de tu familia directa (papás, hermanos o abuelos) padece alguna de las siguientes enfermedades sistémicas?`,
          Lectura= `¿Cuánto tiempo lees (libros, folletos o revistas físicas) diariamente?`,
          Carpintería= `¿Cuál de las siguientes actividades realizas?.Carpintería`,
          Herrería= `¿Cuál de las siguientes actividades realizas?.Herrería`,
          Manicure_pedicure= `¿Cuál de las siguientes actividades realizas?.Actividades relacionadas al manicure/pedicure`,
          Accesorios_pequeños= `¿Cuál de las siguientes actividades realizas?.Trabajas con accesorios, herramientas u objetos muy pequeños`,
          Act_sol= `¿Cuál de las siguientes actividades realizas?.Actividades directamente expuesto a los rayos del sol`,
          Laboratorios= `¿Cuál de las siguientes actividades realizas?.Laboratorios` ,
          Costura= `¿Cuál de las siguientes actividades realizas?.Costurar o tejer`,
          Jardinería= `¿Cuál de las siguientes actividades realizas?.Jardinería`,
          Pintura= `¿Cuál de las siguientes actividades realizas?.Pintura de cualquier tipo` ,
          Sensaciones_conduccion= `Cuando conduces, ¿normalmente presentas alguno de los siguientes sensaciones?`,
          Sueño= `En promedio, ¿Cuántas horas duermes al día?`,
          Vitamina_A= `Frecuencia de consumo de alimentos protectores para la salud visual.Zanahoria, Calabaza, Camote (Vitamina A / Betacarotenos)`,
          Luteina= `Frecuencia de consumo de alimentos protectores para la salud visual.Espinacas, Brócoli, Acelgas (Luteína y Zeaxantina - Filtro natural UV)`,
          Vitamina_C= `Frecuencia de consumo de alimentos protectores para la salud visual.Naranja, Kiwi, Fresas, Guayaba (Vitamina C - Antioxidante)`,
          Omega_3= `Frecuencia de consumo de alimentos protectores para la salud visual.Pescado, Atún, Salmón, Nueces (Omega 3 - Evita ojo seco)`,
          Zinc= `Frecuencia de consumo de alimentos protectores para la salud visual.Huevo, Lácteos (Zinc y Vitamina A)`,
          Suplementos= `¿Consumes suplementos? Marca todos los necesarios.`,
          Prom_celular= 38,
          App_semana= 39,
          Tiempo_app_semana= 40,
          Tamaño_texto= 41 ,
          Descanso_visual= `Cuando estás estudiando o trabajando frente a una pantalla por largo tiempo, ¿cada cuánto tomas un descanso visual (dejar de mirar cualquier dispositivo) de por los menos 30 segundos?`,
          Oscuridad= `¿Con qué frecuencia utilizas tus dispositivos (celular, tablet, laptop) en completa oscuridad o con muy poca luz ambiental?`,
          Distancia= `¿Aproximadamente a qué distancia mantienes el monitor, celular, tablet, pantalla de laptop o consola de videojuegos portatil de tus ojos?`,
          Tamaño_tv= `¿Sabe cuánto mide aproximadamente la televisión que usa normalmente?`,
          Distancia_tv= `¿Aproximadamente a qué distancia mira la televisión?`,
          Tiempo_computadora= `¿Cuánto tiempo pasa frente a la computadora o tablet diariamenete? (Aproximadamente)`,
          Tiempo_tv= `¿Cuánto tiempo mira el televisor?`,
          Celular_mañana= `Al despertar por la mañana, ¿cuánto tiempo pasa hasta que mira la pantalla de tu celular?`,
          Dispositivos_noche= `¿Cuánto tiempo antes de dormir deja de usar sus dispositivos (celular, tablet, laptop)?`,
          Consolas= `En promedio, ¿Cuánto tiempo usa consolas de videojuegos diario?`,
          Conf_celular= 52,
          Dispositivo_mas_usado= `De los siguientes dispositivos electrónicos inteligentes, ¿cuál usa más?`,
          Act_dispositivo= `Marque las actividades que realiza diario en el dispositivo electrónico que más usa, según su respuesta a la pregunta anterior.`,
          EntrecerrarM= `Actividades de visión lejana (Ver el pizarrón, proyecciones, conducir, ver TV). Al realizar estas actividades (sin lentes, en caso de que uses):.¿Necesitas entrecerrar los ojos (hacerlos "chinitos") p`,
          Objetos_lejanos_borrososM= `Actividades de visión lejana (Ver el pizarrón, proyecciones, conducir, ver TV). Al realizar estas actividades (sin lentes, en caso de que uses):.¿Sientes que los objetos lejanos se ven borrosos o con`,
          Acercar_objetoM= `Actividades de visión lejana (Ver el pizarrón, proyecciones, conducir, ver TV). Al realizar estas actividades (sin lentes, en caso de que uses):.¿Te ves obligado a acercarte físicamente al objeto (piz`,
          Dolor_cabezaM= `Actividades de visión lejana (Ver el pizarrón, proyecciones, conducir, ver TV). Al realizar estas actividades (sin lentes, en caso de que uses):.¿Sientes dolor de cabeza al intentar descifrar letreros`,
          Dolor_cabezaH= `Actividades de visión cercana (Leer libros impresos, costura, manualidades, escritura a mano). Al realizar estas actividades por más de 40 minutos (sin lentes, en caso de que uses):.¿Sientes dolor de`,
          DesenfoqueH= `Actividades de visión cercana (Leer libros impresos, costura, manualidades, escritura a mano). Al realizar estas actividades por más de 40 minutos (sin lentes, en caso de que uses):.¿Las letras parece`,
          Alejar_objetoH= `Actividades de visión cercana (Leer libros impresos, costura, manualidades, escritura a mano). Al realizar estas actividades por más de 40 minutos (sin lentes, en caso de que uses):.¿Sientes alivio si`,
          Esfuerzo_enfoqueH= `Actividades de visión cercana (Leer libros impresos, costura, manualidades, escritura a mano). Al realizar estas actividades por más de 40 minutos (sin lentes, en caso de que uses):.¿Te lloran los ojo`,
          LetrasA= `Visión general (tanto de lejos como de cerca, especialmente con luces o textos). Al fijar la vista en objetos o luces (sin lentes, en caso de que uses):.¿Confundes letras similares (como la H, N, M o`,
          LucesA= `Visión general (tanto de lejos como de cerca, especialmente con luces o textos). Al fijar la vista en objetos o luces (sin lentes, en caso de que uses):.¿Ves las luces (semáforos, faros de autos, LEDs`,
          SombrasA= `Visión general (tanto de lejos como de cerca, especialmente con luces o textos). Al fijar la vista en objetos o luces (sin lentes, en caso de que uses):.¿Ves "sombras" duplicadas en las letras (como s`,
          EnfocarA= `Visión general (tanto de lejos como de cerca, especialmente con luces o textos). Al fijar la vista en objetos o luces (sin lentes, en caso de que uses):.¿Te resulta difícil enfocar rápidamente al camb`,
          Lineas_coloresD= `Interpretación de gráficos, mapas, códigos de colores o edición. Al interactuar con colores (sin lentes, en caso de que uses):.¿Te cuesta distinguir líneas de diferentes colores en gráficas de Excel o`,
          Estado_LEDD= `Interpretación de gráficos, mapas, códigos de colores o edición. Al interactuar con colores (sin lentes, en caso de que uses):.¿Confundes el estado de LEDs indicadores (ej. cargando vs. cargado / rojo`,
          Colores_no_coincidenD= `Interpretación de gráficos, mapas, códigos de colores o edición. Al interactuar con colores (sin lentes, en caso de que uses):.¿Tus compañeros te han señalado que los colores que ves no coinciden con`,
          Ropa_coloresD= `Interpretación de gráficos, mapas, códigos de colores o edición. Al interactuar con colores (sin lentes, en caso de que uses):.¿Tienes dificultad para combinar ropa o identificar colores en mapas de c`,
          Parpados_pesadosFV= `Cansancio físico del ojo (no necesariamente por luz, sino por enfoque sostenido). Después de 40 minutos de esfuerzo visual intenso:.¿Sientes los párpados pesados o con ganas de cerrar los ojos?`,
          Dificultad_enfocarFV= `Cansancio físico del ojo (no necesariamente por luz, sino por enfoque sostenido). Después de 40 minutos de esfuerzo visual intenso:.¿Sientes dificultad para volver a enfocar a lo lejos inmediatamente`,
          Dolor_punzanteFV= `Cansancio físico del ojo (no necesariamente por luz, sino por enfoque sostenido). Después de 40 minutos de esfuerzo visual intenso:.¿Sientes dolor punzante en la cuenca del ojo (detrás del ojo)?`,
          FrotarFV= `Cansancio físico del ojo (no necesariamente por luz, sino por enfoque sostenido). Después de 40 minutos de esfuerzo visual intenso:.¿Te frotas los ojos inconscientemente para "despertarlos"?`,
          Doble_momentaneoFV= `Cansancio físico del ojo (no necesariamente por luz, sino por enfoque sostenido). Después de 40 minutos de esfuerzo visual intenso:.¿Ves doble momentáneamente?`,
          Ardor_ojosSVI= `Exclusivo de pantallas digitales (PC, Laptop, Celular, Tablet). Después de 40 minutos frente a una pantalla:.¿Sientes sensación de "arenilla", sequedad o ardor intenso en la superficie del ojo?`,
          Dolor_cuelloSVI= `Exclusivo de pantallas digitales (PC, Laptop, Celular, Tablet). Después de 40 minutos frente a una pantalla:.¿Sientes dolor o rigidez en el cuello, hombros o espalda alta?`,
          FotofobiaSVI= `Exclusivo de pantallas digitales (PC, Laptop, Celular, Tablet). Después de 40 minutos frente a una pantalla:.¿Te molesta la luz de la pantalla o la luz ambiental (fotofobia) al terminar?`,
          Ojos_rojosSVI= `Exclusivo de pantallas digitales (PC, Laptop, Celular, Tablet). Después de 40 minutos frente a una pantalla:.¿Notas los ojos rojos o irritados al mirarte en un espejo?`,
          LagrimeoSVI= `Exclusivo de pantallas digitales (PC, Laptop, Celular, Tablet). Después de 40 minutos frente a una pantalla:.¿Notas lagrimeo excesivo o que los ojos se te humedecen repentinamente después de sentir ar`,
          Vision_dobleSVI= `Exclusivo de pantallas digitales (PC, Laptop, Celular, Tablet). Después de 40 minutos frente a una pantalla:.¿Presentas visión doble al intentar enfocar?`,
          Seis= 82,
          Seis_= 83,
          Siete= 84,
          Cuarenta_y_cinco= 85,
          Dieciseis= 86,
          Cinco= 87,
          Setenta_y_tres= 88,
          Quince= 89,
          Veintiseis= 90,
          Doce= 91,
          Veintinueve= 92,
          Ocho= 93
  )

# 2.2 Verificación de tipos de variables
tabla_variables <- data.frame(
  Tipo_Variable = sapply(DS_SV, function(x) class(x)[1])) 
#View(tabla_variables)
#glimpse(DS_SV)

DS_SV <- DS_SV %>%
  mutate(
    Division_DACB  = ifelse(str_detect(Otra_division,"DACB"), 1, 0),
    Division_DACEA = ifelse(str_detect(Otra_division,"DACEA"), 1, 0),
    Division_DAEA  = ifelse(str_detect(Otra_division,"DAEA"), 1, 0)
  ) %>%
  mutate(across(starts_with("Division_"), ~replace(., is.na(.), 0)))

DS_SV <- DS_SV %>%
  filter(Division_DACB != 1,
         Division_DACEA != 1,
         Division_DAEA != 1)

DS_SV <- DS_SV %>%
  select(-Division_DACB,-Division_DACEA,-Division_DAEA,-Otra_division)

# 2.3 Transformación de variable Sexo a numérico (0 y 1)
DS_SV_L <- DS_SV %>%
  mutate(Sexo = case_when(
    Sexo == "Masculino" ~ 0,
    Sexo == "Femenino"  ~ 1,
    TRUE ~ NA_real_
  )) %>%
  mutate(Sexo = as.numeric(Sexo))

DS_SV_L <- DS_SV_L %>%
  mutate(
    Seis  = ifelse(Seis  == "6",  1, 0),
    Seis_  = ifelse(Seis_  == "6",  1, 0),
    Siete  = ifelse(Siete  == "7",  1, 0),
    Cuarenta_y_cinco  = ifelse(Cuarenta_y_cinco  == "45",  1, 0),
    Dieciseis  = ifelse(Dieciseis  == "16",  1, 0),
    Cinco  = ifelse(Cinco == "5",  1, 0),
    Setenta_y_tres  = ifelse(Setenta_y_tres  == "73",  1, 0),
    Quince  = ifelse(Quince  == "15",  1, 0),
    Veintiseis  = ifelse(Veintiseis  == "26",  1, 0),
    Doce = ifelse(Doce == "12", 1, 0),
    Veintinueve = ifelse(Veintinueve == "29", 1, 0),
    Ocho = ifelse(Ocho == "8", 1, 0)
  ) %>%
  rowwise() %>% 
  
  mutate(
    Puntaje_Agudeza_Cromatica = sum(c_across(Seis:Ocho), na.rm = TRUE)
  ) %>%
  
  ungroup()

DS_SV_L <- DS_SV_L %>%
  mutate(
    
    Frecuencia_examen_vista = case_when(
      Frecuencia_examen_vista == "Nunca me hecho un examen de la vista" ~ 1,
      Frecuencia_examen_vista == "Cada de 3 años o más" ~ 2,
      Frecuencia_examen_vista == "Cada dos años" ~ 3,
      Frecuencia_examen_vista == "Anualmente" ~ 4,
      Frecuencia_examen_vista == "Cada 6 meses" ~ 5,
      TRUE ~ NA_real_
    ),
    
   Tiempo_Lentes = case_when(
     Tiempo_Lentes == "No uso lentes" ~ 0,
     Tiempo_Lentes == "Desde antes de 6 meses de edad" ~ 1,
     Tiempo_Lentes == "Entre los 6 meses y 10 años de edad" ~ 2,
     Tiempo_Lentes == "Entre los 10 y 15 años de edad" ~ 3,
     Tiempo_Lentes == "Entre 15 y 20 años de edad" ~ 4,
     Tiempo_Lentes == "Entre 20 y 35 años de edad" ~ 5,
     Tiempo_Lentes == "Entre 35 años a 50 años de edad" ~ 6,
     Tiempo_Lentes == "De 50 años en adelante" ~ 7,
     TRUE ~ NA_real_
   ),
   
   Oftalmologo = case_when(
     Oftalmologo == "Nunca he ido al oftalmólogo" ~ 0,
     Oftalmologo == "Solo ante una emergencia o dolor" ~ 1,
     Oftalmologo == "Cada tres años" ~ 2,
     Oftalmologo == "Una vez cada dos años" ~ 3,
     Oftalmologo == "1-2 veces al año" ~ 4,
     TRUE ~ NA_real_
   ),
       
    Lectura = case_when(
      Lectura == "No leo" ~ 0,
      Lectura == "Menos de 30 minutos" ~ 1,
      Lectura == "Entre 30 y 40 minutos" ~ 2,
      Lectura == "40 minutos o más" ~ 3,
      TRUE ~ NA_real_
    ),
    
    # 3. Sueño
    Sueño = case_when(
      Sueño == "Menos de 5 horas" ~ 1,
      Sueño == "Entre 6 y 8 horas" ~ 2,
      Sueño == "Más de 8 horas" ~ 3,
      TRUE ~ NA_real_
    ),
    
    # 4. Promedio del uso diario del celular
    Prom_celular = case_when(
      Prom_celular == "Menos de 2 horas" ~ 1,
      Prom_celular == "Entre 2 y 4 horas" ~ 2,
      Prom_celular == "Entre 4 y 6 horas" ~ 3,
      Prom_celular == "Entre 6 y 8 horas" ~ 4,
      Prom_celular == "Más de 8 horas" ~ 5,
      TRUE ~ NA_real_
    ),
    
    # 5. Promedio de tiempo semanal en la app
    Tiempo_app_semana = case_when(
      Tiempo_app_semana == "Menos de 3 horas" ~ 1,
      Tiempo_app_semana == "De 3 a 6 horas" ~ 2,
      Tiempo_app_semana == "Entre 6 horas a 9 horas" ~ 3,
      Tiempo_app_semana == "Más de 9 horas" ~ 4,
      TRUE ~ NA_real_
    ),
    
    # 6. Descanso visual
    Descanso_visual = case_when(
      Descanso_visual == "No hago pausas activas" ~ 1,
      Descanso_visual == "Solo cuando me arden los ojos o me duele la cabeza." ~ 2,
      Descanso_visual == "Cada 2 o 3 horas" ~ 3,
      Descanso_visual == "Cada hora" ~ 4,
      grepl("20-30", Descanso_visual) ~ 5,
      TRUE ~ NA_real_
    ),
    
    # 7. Uso en la oscuridad
    Oscuridad = case_when(
      Oscuridad == "Nunca" ~ 1,
      Oscuridad == "A veces" ~ 2,
      Oscuridad == "Siempre" ~ 3,
      TRUE ~ NA_real_
    ),
    
    # 8. Distancia a dispositivos
    Distancia = case_when(
      Distancia == "Menos de 30 cm" ~ 1,
      Distancia == "30-50 cm" ~ 2,
      Distancia == "Mas de 50 cn" ~ 3,
      TRUE ~ NA_real_
    ),
    
    # 9. Tamaño de TV
    Tamaño_tv = case_when(
      Tamaño_tv == "No uso televisión" ~ 0,
      Tamaño_tv == "32 pulgadas" ~ 1,
      Tamaño_tv == "40- 43 pulgadas" ~ 2,
      Tamaño_tv == "50-55 pugadas" ~ 3,
      Tamaño_tv == "65 pulgadas o más" ~ 4,
      Tamaño_tv == "No sé cuánto mide" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    # 10. Distancia de TV
    Distancia_tv = case_when(
      Distancia_tv == "No uso televisión" ~ 0,
      Distancia_tv == "Menos de 1 metro - Como si fuera monitor de PC." ~ 1,
      Distancia_tv == "1 a 1.5 metros - Puedo tocar la TV estirando el pie o la mano." ~ 2,
      Distancia_tv == "2 a 3 metros - Hay una mesa de centro o espacio libre entre yo y la tele." ~ 3,
      Distancia_tv == "Más de 3 metros - La veo desde el otro extremo de la habitación." ~ 4,
      TRUE ~ NA_real_
    ),
    
    # 11. Tiempo en computadora o tablet
    Tiempo_computadora = case_when(
      Tiempo_computadora == "No uso computadora o tablet" ~ 0,
      Tiempo_computadora == "Menos de 60 minutos" ~ 1,
      Tiempo_computadora == "De 1 hora a 3 horas" ~ 2,
      Tiempo_computadora == "De 3 horas a 5 horas" ~ 3,
      Tiempo_computadora == "De 5 horas a 8 horas" ~ 4,
      Tiempo_computadora == "Más de 8 horas" ~ 5,
      TRUE ~ NA_real_
    ),
    
    # 12. Tiempo en TV
    Tiempo_tv = case_when(
      Tiempo_tv == "No uso televisión" ~ 0,
      Tiempo_tv == "Menos de 60 minutos" ~ 1,
      Tiempo_tv == "De 1 hora a 2 horas" ~ 2,
      Tiempo_tv == "De 2 a 3 horas" ~ 3,
      Tiempo_tv == "Más de 3 horas" ~ 4,
      TRUE ~ NA_real_
    ),
    
    # 13. Tiempo celular mañana (1 = Menos tiempo de espera, mayor exposición temprana)
    Celular_mañana = case_when(
      Celular_mañana == "0 - 5 minutos" ~ 1,
      Celular_mañana == "5 - 15 minutos" ~ 2,
      Celular_mañana == "15 - 30 minutos" ~ 3,
      Celular_mañana == "Más de 30 minutos" ~ 4,
      TRUE ~ NA_real_
    ),
    
    # 14. Dispositivos noche (1 = Menos tiempo de desconexión, mayor exposición tardía)
    Dispositivos_noche = case_when(
      Dispositivos_noche == "Lo uso hasta que se me cierran los ojos" ~ 1,
      Dispositivos_noche == "5 a 10 minutos antes" ~ 2,
      Dispositivos_noche == "30 minutos antes" ~ 3,
      Dispositivos_noche == "1 hora antes o más" ~ 4,
      TRUE ~ NA_real_
    ),
    
    # 15. Consolas de videojuegos
    Consolas = case_when(
      Consolas == "No uso consola de videojuegos" ~ 0,
      Consolas == "Menos de 1 hora" ~ 1,
      Consolas == "De 1 horas a 2 horas" ~ 2,
      Consolas == "Más de 2 horas" ~ 3,
      TRUE ~ NA_real_
    )
  )

# Transformación variables en matriz
DS_SV_L <- DS_SV_L %>%
  mutate(
    across(
      .cols = c(EntrecerrarM:Vision_dobleSVI), 
      .fns = ~ case_when(
        . == "Nunca" ~ 0,
        grepl("Casi nunca", .) ~ 1,
        . == "Regular" ~ 2,
        . == "Casi siempre" ~ 3,
        . == "Siempre" ~ 4,
        TRUE ~ NA_real_
      )
    ),
    across(
      .cols = c(Vitamina_A:Zinc),
      .fns = ~ case_when(
        . == "Diario" ~ 4,
        . == "3-4 veces a la semana" ~ 3,
        . == "1-2 veces a la semana" ~ 2,
        . == "1-2 veces al mes" ~ 1,
        . == "Nunca" ~ 0,
        TRUE ~ NA_real_
      )
    ),
    across(
      .cols = c(Carpintería:Pintura),
      .fns = ~ case_when(
        . == "Diario" ~ 4,
        . == "1 vez a la semana" ~ 3,
        . == "1-2 veces al mes" ~ 2,
        . == "1-2 veces al año" ~ 1,
        . == "Nunca" ~ 0,
        TRUE ~ NA_real_
      )
    )
  )

# Transformación multiple checkbox
DS_SV_L <- DS_SV_L %>%
  mutate(
    Trabajo_CallCenter  = ifelse(str_detect(Trabajo, "Call Center"), 1, 0),
    Trabajo_Chofer      = ifelse(str_detect(Trabajo, "Chofer"), 1, 0),
    Trabajo_Clases      = ifelse(str_detect(Trabajo, "Clases"), 1, 0),
    Trabajo_Mesero      = ifelse(str_detect(Trabajo, "Mesero"), 1, 0),
    Trabajo_Ventas      = ifelse(str_detect(Trabajo, "Ventas"), 1, 0),
    Trabajo_Diseno      = ifelse(str_detect(Trabajo, "Diseño"), 1, 0),
    Trabajo_Soporte     = ifelse(str_detect(Trabajo, "Soporte Técnico"), 1, 0),
    Trabajo_Oficina     = ifelse(str_detect(Trabajo, "Oficina"), 1, 0),
    Trabajo_Manuales    = ifelse(str_detect(Trabajo, "Trabajos Manuales"), 1, 0),
    Trabajo_Salud       = ifelse(str_detect(Trabajo, "Área de la Salud"), 1, 0),
    Trabajo_Ninguno     = ifelse(str_detect(Trabajo, "No trabajo"), 1, 0),
    Lentes_Ninguno       = ifelse(str_detect(Lentes, "No uso lentes"), 1, 0),
    Lentes_Graduacion    = ifelse(str_detect(Lentes, "Graduación médica"), 1, 0),
    Lentes_FiltroAzul    = ifelse(str_detect(Lentes, "Filtro de Luz Azul"), 1, 0),
    Lentes_Antirreflejo  = ifelse(str_detect(Lentes, "Antirreflejante básico"), 1, 0),
    Lentes_Bifocales     = ifelse(str_detect(Lentes, "Bifocales o Progresivos"), 1, 0),
    Lentes_Fotocromaticos= ifelse(str_detect(Lentes, "Fotocromáticos"), 1, 0),
    Lentes_SolBasicos    = ifelse(str_detect(Lentes, "Lentes de sol básicos"), 1, 0),
    Lentes_Polarizados   = ifelse(str_detect(Lentes, "Lentes polarizados"), 1, 0),
    Lentes_VistaCansada  = ifelse(str_detect(Lentes, "Vista cansada"), 1, 0),
    Examen_Autorefractometro = ifelse(str_detect(Examen_vista, "Autorefractómetro"), 1, 0),
    Examen_Snellen           = ifelse(str_detect(Examen_vista, "Snellen"), 1, 0),
    Examen_Jaeger            = ifelse(str_detect(Examen_vista, "Jaeger"), 1, 0),
    Examen_Foroptero         = ifelse(str_detect(Examen_vista, "Foróptero"), 1, 0),
    Examen_Tonometria        = ifelse(str_detect(Examen_vista, "Tonometría"), 1, 0),
    Examen_Hendidura         = ifelse(str_detect(Examen_vista, "Hendidura"), 1, 0),
    Examen_FondoOjo          = ifelse(str_detect(Examen_vista, "Fondo de Ojo"), 1, 0),
    Examen_Campimetria       = ifelse(str_detect(Examen_vista, "Campimetría"), 1, 0),
    Examen_Ninguno           = ifelse(str_detect(Examen_vista, "No me he hecho"), 1, 0),
    Enf_Astigmatismo  = ifelse(str_detect(Enf_ocular, "Astigmatismo"), 1, 0),
    Enf_Miopia        = ifelse(str_detect(Enf_ocular, "Miopía"), 1, 0),
    Enf_Hipermetropia = ifelse(str_detect(Enf_ocular, "Hipermetropía"), 1, 0),
    Enf_Fatiga        = ifelse(str_detect(Enf_ocular, "Fatiga Visual"), 1, 0),
    Enf_SindromeComp  = ifelse(str_detect(Enf_ocular, "Síndrome de Visión por Computadora"), 1, 0),
    Enf_Conjuntivitis = ifelse(str_detect(Enf_ocular, "Conjuntivitis"), 1, 0),
    Enf_Estrabismo    = ifelse(str_detect(Enf_ocular, "Estrabismo"), 1, 0),
    Enf_Ambliopia     = ifelse(str_detect(Enf_ocular, "Ambliopía"), 1, 0),
    Enf_Ceguera       = ifelse(str_detect(Enf_ocular, "Ceguera"), 1, 0),
    Enf_Ninguna       = ifelse(str_detect(Enf_ocular, "No he sido diagnosticado"), 1, 0),
    Enf_Daltonismo   = ifelse(str_detect(Enf_ocular, "Daltonismo"), 1, 0),
    Enf_Queratocono   = ifelse(str_detect(Enf_ocular, "Queratocono"), 1, 0),
    Enf_Carnosidad2   = ifelse(str_detect(Enf_ocular, "Queratocono"), 1, 0),
    Enf_Glaucoma2   = ifelse(str_detect(Enf_ocular, "Queratocono"), 1, 0),
    Enf_Cataratas2   = ifelse(str_detect(Enf_ocular, "Queratocono"), 1, 0),
    Enf_Degeneracion2   = ifelse(str_detect(Enf_ocular, "Queratocono"), 1, 0),
    Enf_Desprendimiento2   = ifelse(str_detect(Enf_ocular, "Queratocono"), 1, 0),
    Enf_Retinopatia   = ifelse(str_detect(Enf_ocular, "Queratocono"), 1, 0),
    Fam_Diabetes     = ifelse(str_detect(Condicion_fam, "Diabetes"), 1, 0),
    Fam_Hipertension = ifelse(str_detect(Condicion_fam, "Hipertensión"), 1, 0),
    Fam_Ninguna = ifelse(str_detect(Condicion_fam, "Ninguna"), 1, 0),
    Sup_Multivitaminico = ifelse(str_detect(Suplementos, "Multivitamínico"), 1, 0),
    Sup_Omega3          = ifelse(str_detect(Suplementos, "Omega 3"), 1, 0),
    Sup_Vista           = ifelse(str_detect(Suplementos, "Luteína/Zeaxantina"), 1, 0),
    Sup_NoConsumo       = ifelse(str_detect(Suplementos, "No consumo"), 1, 0),
    Conf_BrilloAdaptable = ifelse(str_detect(Conf_celular, "Brillo adaptable"), 1, 0),
    Conf_ColoresCalidos  = ifelse(str_detect(Conf_celular, "Colores más cálidos"), 1, 0),
    Conf_ModoOscuro      = ifelse(str_detect(Conf_celular, "Modo oscuro"), 1, 0),
    Conf_Distancia       = ifelse(str_detect(Conf_celular, "Distancia de la pantalla"), 1, 0),
    Conf_ProtectorVista  = ifelse(str_detect(Conf_celular, "Protector de la vista"), 1, 0),
    Conf_ModoTenue       = ifelse(str_detect(Conf_celular, "Modo Tenue"), 1, 0),
    Act_Academicas   = ifelse(str_detect(Act_dispositivo, "Actividades académicas"), 1, 0),
    Act_Entretenimiento = ifelse(str_detect(Act_dispositivo, "Entretenimiento visual"), 1, 0),
    Act_Social       = ifelse(str_detect(Act_dispositivo, "Interacción social"), 1, 0),
    Act_Videojuegos  = ifelse(str_detect(Act_dispositivo, "Videojuegos"), 1, 0),
    Act_Noticias     = ifelse(str_detect(Act_dispositivo, "Leer noticias"), 1, 0)
    
  )
DS_SV_L <- DS_SV_L %>%
  mutate(
    Condicion_Colesterol   = ifelse(str_detect(Condicion, "Colesterol"), 1, 0),
    Condicion_Diabetes     = ifelse(str_detect(Condicion, "Diabetes"), 1, 0),
    Condicion_Hipertension = ifelse(str_detect(Condicion, "Hipertensión"), 1, 0),
    Condicion_Migrana      = ifelse(str_detect(Condicion, "Migraña"), 1, 0),
    Tiene_Condicion = Condicion_Colesterol + Condicion_Diabetes + Condicion_Hipertension + Condicion_Migrana,
    
    Condicion_Ninguna = ifelse(Tiene_Condicion > 0, 0, ifelse(str_detect(Condicion, "Ninguna"), 1, 0))
  ) %>%
  select(-Tiene_Condicion)

DS_SV_L <- DS_SV_L %>%
  mutate(
    Enf_Cataratas         = ifelse(str_detect(Enf_fam, "Cataratas"), 1, 0),
    Enf_Ceguera           = ifelse(str_detect(Enf_fam, "Ceguera"), 1, 0),
    Enf_DegeneracionMac   = ifelse(str_detect(Enf_fam, "Degeneración macular"), 1, 0),
    Enf_Desprendimiento   = ifelse(str_detect(Enf_fam, "Desprendimiento de retina"), 1, 0),
    Enf_Glaucoma          = ifelse(str_detect(Enf_fam, "Glaucoma"), 1, 0),
    Enf_Nose              = ifelse(str_detect(Enf_fam, "No sé"), 1, 0),
    Total_Enfermedades = Enf_Cataratas + Enf_Ceguera + Enf_DegeneracionMac + Enf_Desprendimiento + Enf_Glaucoma,
    Enf_Ninguna2 = ifelse(Total_Enfermedades > 0, 0, ifelse(str_detect(Enf_fam, "Ninguna"), 1, 0))
  ) %>%
  select(-Total_Enfermedades)

DS_SV_L <- DS_SV_L %>%
  mutate(
    Conduce = ifelse(str_detect(Sensaciones_conduccion, "No conduzco"), 0, 1),
    Sens_CegueraNocturna   = ifelse(str_detect(Sensaciones_conduccion, "Ceguera Nocturna"), 1, 0),
    Sens_LucesMolestas     = ifelse(str_detect(Sensaciones_conduccion, "luces de los otros autos"), 1, 0),
    Sens_EntrecerrarOjos   = ifelse(str_detect(Sensaciones_conduccion, "entrecerrar los ojos"), 1, 0),
    Sens_OjoSeco           = ifelse(str_detect(Sensaciones_conduccion, "Ojo Seco"), 1, 0),
    Sens_Destellos         = ifelse(str_detect(Sensaciones_conduccion, "destellos, rayos"), 1, 0),
    Sens_VisionBorrosa     = ifelse(str_detect(Sensaciones_conduccion, "Visión Borrosa Lejana"), 1, 0),
    Sens_Ninguna           = ifelse(str_detect(Sensaciones_conduccion, "Ninguna"), 1, 0)
  ) %>%
  mutate(
    Sens_CegueraNocturna   = Sens_CegueraNocturna * Conduce,
    Sens_LucesMolestas     = Sens_LucesMolestas * Conduce,
    Sens_EntrecerrarOjos   = Sens_EntrecerrarOjos * Conduce,
    Sens_OjoSeco           = Sens_OjoSeco * Conduce,
    Sens_Destellos         = Sens_Destellos * Conduce,
    Sens_VisionBorrosa     = Sens_VisionBorrosa * Conduce,

    Total_Sensaciones = Sens_CegueraNocturna + Sens_LucesMolestas + Sens_EntrecerrarOjos + 
      Sens_OjoSeco + Sens_Destellos + Sens_VisionBorrosa,
    Sens_Ninguna = ifelse(Conduce == 1 & Total_Sensaciones == 0, 1, 0)
  ) %>%
  select(-Total_Sensaciones)

DS_SV_L <- DS_SV_L %>%
  mutate(
    Oft_Opt_Limpia = str_remove(Oft_Opt, ";$"),
    Oft_Oftalmologo = ifelse(str_detect(Oft_Opt_Limpia, "Oftalmólogo"), 1, 0),
    Oft_Optometrista = ifelse(str_detect(Oft_Opt_Limpia, "Optometrista"), 1, 0),
    Total_Profesionales = Oft_Oftalmologo + Oft_Optometrista,
    Oft_Ninguno = ifelse(Total_Profesionales > 0, 0, ifelse(str_detect(Oft_Opt_Limpia, "Ninguno"), 1, 0))
  ) %>%
  select(-Oft_Opt_Limpia,-Total_Profesionales)

DS_SV_L <- DS_SV_L %>%
  select(-Aviso, -Id, -Lentes,-Trabajo, -Examen_vista, -Enf_ocular, 
         -Condicion, -Enf_fam, -Condicion_fam, -Sensaciones_conduccion, 
         -Suplementos, -Conf_celular, -Act_dispositivo)

# ==============================================================================
# BLOQUE 3: ANÁLISIS UNIVARIADO (ESTADÍSTICA DESCRIPTIVA Y TABLAS)
# ==============================================================================
tbl_summary(DS_SV_L)
dfSummary(DS_SV_L)

EDA_1 <- DS_SV_L %>%
  mutate(Sexo = case_when(
    Sexo == 0 ~ "Hombres",
    Sexo == 1 ~ "Mujeres",
    TRUE ~ "NA"
  )) %>%
  select(-Aviso, -Id, -Lentes,-Otra_division, -Trabajo, -Examen_vista, -Enf_ocular, 
         -Condicion, -Enf_fam, -Condicion_fam, -Sensaciones_conduccion, 
         -Suplementos, -Conf_celular, -Act_dispositivo) %>%
  tbl_summary(
    by=Sexo,
    statistic = list(all_continuous() ~ "{mean} ({sd})", 
                     all_categorical() ~ "{n} ({p}%)"),
    missing_text = "(Datos faltantes)"
  ) %>%
  
  add_overall() %>% 
  modify_caption("**Tabla 1. Perfil sociodemográfico y de salud visual por sexo**")

table(DS_SV_L$Trabajo_Soporte)
EDA_1
write(as.character(as_kable(EDA_1, format = "markdown")), "resumen_tesis.txt")


# ==============================================================================
# BLOQUE 4: ANÁLISIS UNIVARIADO (VISUALIZACIONES)
# ==============================================================================

# 4.1 Histogramas para variables numéricas
ggplot(DS_SV_L, aes(x = Edad)) +
  geom_histogram(fill = "coral", color = "black", bins = 20) +
  theme_minimal() +
  labs(title = "Distribución de Edad",
       x = "Edad",
       y = "Frecuencia")

ggplot(DS_SV_L, aes(x = Tamaño_texto)) +
  geom_histogram(fill = "coral", color = "black", bins = 20) +
  theme_minimal() +
  labs(title = "Distribución de Tamaño del texto",
       x = "Tamaño del texto",
       y = "Frecuencia")

# 4.2 Gráfico individual de Sexo (variable binaria)
ggplot(DS_SV_L, aes(x = Sexo)) +
  geom_bar(fill = "coral", color = "black") +
  theme_minimal() +
  labs(title = "Distribución de Sexo",
       subtitle = "0 = Masculino | 1 = Femenino",
       x = "Sexo",
       y = "Número de Respuestas") +
  
  scale_x_continuous(breaks = c(0, 1))

# 4.3 Gráficos de barras masivos para variables categóricas (opción única)
 update_geom_defaults("bar", list(fill = "coral", color = "black"))

mis_graficos2 <- DS_SV %>%
  #select(Licenciatura, Otra_division) %>%
 select(-Id, -Aviso, -Suplementos, -Lentes, -Sensaciones_conduccion, -Enf_ocular,
               -Conf_celular, -Act_dispositivo, -Condicion,-Oft_Opt,
                -Enf_fam, -Condicion_fam, -Examen_vista, -Trabajo) %>%

  plot_bar(nrow = 2,
          ncol = 1,
           theme_config = list(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  )
for (i in seq_along(mis_graficos2)) {
  ggsave(
    filename = paste0("graf_cat", i, ".png"), 
    plot = mis_graficos2[[i]], 
    width = 8, 
    height = 10, 
    dpi = 300
  )
}

# 4.4 FUNCIÓN PERSONALIZADA ACTUALIZADA CON TU RUTA

Carpeta_Guardado <- "/Users/pame/Documents/tesis_oficial/IMG"
if (!dir.exists(Carpeta_Guardado)) dir.create(Carpeta_Guardado, recursive = TRUE)

graficar_multiple <- function(nombre_columna, titulo_grafico) {
  
  p <- DS_SV_L %>%  
    rename(opcion = all_of(nombre_columna)) %>%
    select(opcion) %>%
    separate_rows(opcion, sep = ";") %>%
    filter(opcion != "", !is.na(opcion)) %>%
    mutate(opcion = str_wrap(opcion, width = 50)) %>%
    count(opcion) %>%
    ggplot(aes(x = reorder(opcion, n), y = n)) +
    geom_bar(stat = "identity", fill = "coral", color = "black") +
    coord_flip() +
    theme_minimal() +
    labs(title = titulo_grafico,
         subtitle = "Los encuestados podían elegir más de una opción",
         x = "Respuesta",
         y = "Total de Menciones")

  print(p)

  ruta_archivo <- paste0(Carpeta_Guardado, "/grafico_", nombre_columna, ".png")
  
  ggsave(filename = ruta_archivo, 
         plot = p, 
         width = 8, 
         height = 6, 
         dpi = 300)
}
# 4.5 Ejecución de gráficos de Opción Múltiple
graficar_multiple("Suplementos", "Total de menciones de consumo de Suplementos")
graficar_multiple("Sensaciones_conduccion", "Total de menciones de Sensaciones al conducir")
graficar_multiple("Lentes", "Total de menciones de uso de lentes")
graficar_multiple("Enf_ocular", "Total de menciones de enfermedades oculares")
graficar_multiple("Conf_celular", "Total de menciones de configuración del celular")
graficar_multiple("Act_dispositivo", "Total de menciones de actividades en
                  el dispositivo más usado")
graficar_multiple("Condicion", "Total de menciones de condiciones médicas")
graficar_multiple("Enf_fam", "Total de menciones de enfermedades oculares en la familia")
graficar_multiple("Condicion_fam", "Total de menciones de condiciones médicas en la familia")
graficar_multiple("Examen_vista", "Total de menciones de frecuencia de examen de la vista")
graficar_multiple("Trabajo", "Total de menciones de trabajo")
graficar_multiple("Oft_Opt", "Total de menciones de visitas al oftalmólogo u optometrista")

# ==============================================================================
# BLOQUE 5: ANÁLISIS BIVARIADO (CRUZANDO VARIABLES POR SEXO)
# ==============================================================================

#################################
## 5.1 Análisis de correlación ##
#################################
BASE_CORR2 <- DS_SV_L %>% 
  select(-Id,-Aviso,-Act_dispositivo,-Conf_celular,
         -Sensaciones_conduccion,-Suplementos,-Conf_celular,-Act_dispositivo,
         -Condicion,-Condicion_fam,-Enf_ocular,-Enf_fam,-Oft_Opt,-Examen_vista,
         -Lentes,-Trabajo)

BASE_CORR2 <- BASE_CORR2 %>%
  mutate(
    # Creamos las dummies para cada una de las 7 opciones
    Lic_NoDACyTI       = ifelse(Licenciatura == "No estudio una licenciatura en DACyTI", 1, 0),
    Lic_IngInfoAdm     = ifelse(Licenciatura == "Ingeniería en Informática Administrativa (DACyTI)", 1, 0),
    Lic_IngSistemas    = ifelse(Licenciatura == "Ingeniería en Sistemas Computacionales (DACyTI)", 1, 0),
    Lic_InfoAdm        = ifelse(Licenciatura == "Licenciatura en Informática Administrativa (DACyTI)", 1, 0),
    Lic_Sistemas       = ifelse(Licenciatura == "Licenciatura en Sistemas Computacionales (DACyTI)", 1, 0),
    Lic_Tecnologias    = ifelse(Licenciatura == "Licenciatura en Tecnologías de la Información (DACyTI)", 1, 0),
    Lic_Telematica     = ifelse(Licenciatura == "Licenciatura en Telemática (DACyTI)", 1, 0),
    Posg_NoEstudio         = ifelse(Posgrado == "No estudio un posgrado en DACyTI.", 1, 0),
    Posg_MaestriaATI       = ifelse(Posgrado == "Maestría en Administración de Tecnologías de la Información (DACyTI)", 1, 0),
    Posg_MaestriaCC        = ifelse(Posgrado == "Maestría en Ciencias de la Computación (DACyTI)", 1, 0),
    Posg_MaestriaTAC       = ifelse(Posgrado == "Maestría en Tecnologías para el Aprendizaje y el Conocimiento (DACyTI)", 1, 0),
    Posg_DoctoradoCC       = ifelse(Posgrado == "Doctorado en Ciencias de la Computación (DACyTI)", 1, 0),
    Posg_DoctoradoGTI      = ifelse(Posgrado == "Doctorado en Gestión de Tecnologías de la Información (DACyTI)", 1, 0),
    Semestre_Num = as.numeric(str_extract(Semestre, "\\d+")),
    App_Youtube      = ifelse(App_semana == "Youtube", 1, 0),
    App_Facebook     = ifelse(App_semana == "Facebook", 1, 0),
    App_WhatsApp     = ifelse(App_semana == "WhatsApp", 1, 0),
    App_Instagram    = ifelse(App_semana == "Instagram", 1, 0),
    App_LinkedIn     = ifelse(App_semana == "LinkedIn", 1, 0),
    App_Netflix      = ifelse(App_semana == "Netflix", 1, 0),
    App_Safari       = ifelse(App_semana == "Safari", 1, 0),
    App_X            = ifelse(App_semana == "X", 1, 0),
    App_Gmail        = ifelse(App_semana == "Gmail", 1, 0),
    App_Tiktok       = ifelse(App_semana == "Tiktok", 1, 0),
    App_Pinterest    = ifelse(App_semana == "Pinterest", 1, 0),
    App_Telegram     = ifelse(App_semana == "Telegram", 1, 0),
    App_Videojuegos  = ifelse(App_semana == "Videojuego(s)", 1, 0),
    App_Google       = ifelse(App_semana == "Google", 1, 0),
    App_Chrome       = ifelse(App_semana == "Google Chrome", 1, 0),
    App_Mihon        = ifelse(App_semana == "Mihon", 1, 0),
    App_Teams        = ifelse(App_semana == "Teams", 1, 0),
    Disp_Celular     = ifelse(Dispositivo_mas_usado == "Celular", 1, 0),
    Disp_Television  = ifelse(Dispositivo_mas_usado == "Televisión", 1, 0),
    Disp_Computadora = ifelse(Dispositivo_mas_usado == "Computadora", 1, 0),
    Disp_Consola     = ifelse(Dispositivo_mas_usado == "Consola de videojuegos", 1, 0)
    ) %>%
  select(-Licenciatura,-Posgrado,-Semestre,-App_semana,-Dispositivo_mas_usado)

BASE_CORR2 <- BASE_CORR2 %>%
  mutate(
    Division_DACB  = ifelse(str_detect(Otra_division,"DACB"), 1, 0),
    Division_DACEA = ifelse(str_detect(Otra_division,"DACEA"), 1, 0),
    Division_DAEA  = ifelse(str_detect(Otra_division,"DAEA"), 1, 0)
  ) %>%
  mutate(across(starts_with("Division_"), ~replace(., is.na(.), 0)))

BASE_CORR2 <- BASE_CORR2 %>%
  select(-Otra_division,-Enf_Carnosidad2,-Enf_Cataratas2,-Enf_Degeneracion2,-Enf_Glaucoma2,
         -Enf_Desprendimiento2,-Enf_Daltonismo,-Enf_Queratocono, -Enf_Retinopatia,
         -App_Pinterest,-App_Gmail,-App_Safari,-App_LinkedIn,-Lic_NoDACyTI,
         -Posg_MaestriaTAC,-Seis,-Seis_,-Siete,-Cuarenta_y_cinco, -Posg_MaestriaATI,
         -Dieciseis,-Cinco,-Setenta_y_tres,-Quince,-Veintiseis,-Doce,-Veintinueve,
         -Ocho)
BASE_CORR2 <- BASE_CORR2 %>%
filter(Division_DACB != 1,
       Division_DACEA != 1,
       Division_DAEA != 1)

BASE_CORR2 <- BASE_CORR2 %>%
  select(-Division_DACB,-Division_DACEA,-Division_DAEA)

orden_tesis <- c(
  "Sexo", "Edad", "Lic_IngInfoAdm", "Lic_IngSistemas", "Lic_InfoAdm", "Lic_Sistemas", 
  "Lic_Tecnologias", "Lic_Telematica", "Posg_NoEstudio", "Posg_MaestriaCC", 
  "Posg_DoctoradoCC", "Posg_DoctoradoGTI", "Semestre_Num", "Trabajo_CallCenter", 
  "Trabajo_Chofer", "Trabajo_Clases", "Trabajo_Mesero", "Trabajo_Ventas", 
  "Trabajo_Diseno", "Trabajo_Soporte", "Trabajo_Oficina", "Trabajo_Manuales", 
  "Trabajo_Salud", "Trabajo_Ninguno",
  "Sueño", "Lectura", "Carpintería", "Herrería", "Manicure_pedicure", 
  "Accesorios_pequeños", "Act_sol", "Laboratorios", "Costura", "Jardinería", 
  "Pintura", "Zinc", "Vitamina_A", "Vitamina_C", "Omega_3", "Luteina", 
  "Prom_celular", "Sup_Multivitaminico", "Sup_Omega3", "Sup_Vista", "Sup_NoConsumo",
  "Disp_Celular", "Disp_Television", "Disp_Computadora", "Disp_Consola",
  "Tiempo_app_semana", "Tiempo_computadora", "Tiempo_tv", "Celular_mañana", 
  "Dispositivos_noche", "Consolas", 
  "App_Youtube", "App_Facebook", "App_WhatsApp", "App_Instagram", "App_Netflix", 
  "App_X", "App_Tiktok", "App_Telegram", "App_Videojuegos", "App_Google", 
  "App_Chrome", "App_Mihon", "App_Teams", 
  "Act_Academicas", "Act_Entretenimiento", "Act_Social", "Act_Videojuegos", "Act_Noticias",
  "Oscuridad", "Distancia", "Tamaño_tv", "Distancia_tv", "Descanso_visual", 
  "Conf_BrilloAdaptable", "Conf_ColoresCalidos", "Conf_ModoOscuro", "Conf_Distancia", 
  "Conf_ProtectorVista", "Conf_ModoTenue", "Tamaño_texto",
  "Conduce", "Sens_CegueraNocturna", "Sens_LucesMolestas", "Sens_EntrecerrarOjos", 
  "Sens_OjoSeco", "Sens_Destellos", "Sens_VisionBorrosa", "Sens_Ninguna",
  "Parpados_pesadosFV", "Dificultad_enfocarFV", "Dolor_punzanteFV", "FrotarFV", 
  "Doble_momentaneoFV", "Ardor_ojosSVI", "Dolor_cuelloSVI", "FotofobiaSVI", 
  "Ojos_rojosSVI", "LagrimeoSVI", "Vision_dobleSVI",
  "EntrecerrarM", "Objetos_lejanos_borrososM", "Acercar_objetoM", "Dolor_cabezaM",
  "DesenfoqueH", "Alejar_objetoH", "Esfuerzo_enfoqueH", "Dolor_cabezaH",
  "LetrasA", "LucesA", "SombrasA", "EnfocarA",
  "Tiempo_Lentes", "Oftalmologo", "Lentes_Ninguno", "Lentes_Graduacion", 
  "Lentes_FiltroAzul", "Lentes_Antirreflejo", "Lentes_Bifocales", 
  "Lentes_Fotocromaticos", "Lentes_SolBasicos", "Lentes_Polarizados", "Lentes_VistaCansada",
  "Oft_Oftalmologo", "Oft_Optometrista", "Oft_Ninguno", "Frecuencia_examen_vista",
  "Examen_Autorefractometro", "Examen_Snellen", "Examen_Jaeger", "Examen_Foroptero", 
  "Examen_Tonometria", "Examen_Hendidura", "Examen_FondoOjo", "Examen_Campimetria", 
  "Examen_Ninguno",
  "Enf_Astigmatismo", "Enf_Miopia", "Enf_Hipermetropia", "Enf_Fatiga", 
  "Enf_SindromeComp", "Enf_Conjuntivitis", "Enf_Estrabismo", "Enf_Ambliopia", 
  "Enf_Ceguera", "Enf_Ninguna", "Enf_Cataratas", "Enf_DegeneracionMac", 
  "Enf_Desprendimiento", "Enf_Glaucoma", "Enf_Nose", "Enf_Ninguna2",
  "Fam_Diabetes", "Fam_Hipertension", "Fam_Ninguna", 
  "Condicion_Colesterol", "Condicion_Diabetes", "Condicion_Hipertension", 
  "Condicion_Migrana", "Condicion_Ninguna",
  "Lineas_coloresD", "Estado_LEDD", "Colores_no_coincidenD", "Ropa_coloresD", 
  "Puntaje_Agudeza_Cromatica"
)

matriz_cor <- cor(BASE_CORR2, method = "spearman", use = "complete.obs")
round(matriz_cor, 2)
#View(round(matriz_cor, 2))

variables_a_usar <- intersect(orden_tesis, colnames(BASE_CORR2))
matriz_ordenada <- matriz_cor[variables_a_usar, variables_a_usar]

pdf("Mapa_Calor_Tesis.pdf", width = 10, height = 10)

# 3. Generar el gráfico dentro del archivo
corrplot(matriz_ordenada, 
         method = "color", 
         type = "lower", 
         order = "original", 
         tl.col = "black", 
         tl.cex = 0.3, 
         diag = FALSE)

# 4. Cerrar el archivo (esto es obligatorio para que se guarde)
dev.off()

matriz_larga <- melt(matriz_ordenada)
hallazgos <- subset(matriz_larga, abs(value) >= 0.5 & abs(value) < 1)
hallazgos <- hallazgos[order(-abs(hallazgos$value)), ]

############################
# 5.3 PRUEBAS DE HIPÓTESIS #
############################

BASE_CORR2$Indice_Fatiga_Total <- rowSums(BASE_CORR2[, c("Parpados_pesadosFV", "Dificultad_enfocarFV", "Dolor_punzanteFV", 
                                                         "FrotarFV", "Doble_momentaneoFV", "Ardor_ojosSVI", "FotofobiaSVI", 
                                                         "Dolor_cuelloSVI", "Ojos_rojosSVI", "Vision_dobleSVI", "LagrimeoSVI", 
                                                         "EntrecerrarM", "Objetos_lejanos_borrososM", "Acercar_objetoM", 
                                                         "DesenfoqueH", "Dolor_cabezaM", "Alejar_objetoH", "Esfuerzo_enfoqueH", 
                                                         "Dolor_cabezaH", "LucesA", "LetrasA", "SombrasA", "EnfocarA")])
resultado_distancia <- kruskal.test(Indice_Fatiga_Total ~ as.factor(Distancia), data = BASE_CORR2)
print(resultado_distancia)

resultado_celular <- kruskal.test(Indice_Fatiga_Total ~ as.factor(Prom_celular), data = BASE_CORR2)
print(resultado_celular)

resultado_computadora <- kruskal.test(Indice_Fatiga_Total ~ as.factor(Tiempo_computadora), data = BASE_CORR2)
print(resultado_computadora)

resultado_tv <- kruskal.test(Indice_Fatiga_Total ~ as.factor(Tiempo_tv), data = BASE_CORR2)
print(resultado_tv)

resultado_consolas <- kruskal.test(Indice_Fatiga_Total ~ as.factor(Consolas), data = BASE_CORR2)
print(resultado_consolas)

perfil_celular <- BASE_CORR2 %>% filter(Disp_Celular == 1)
perfil_pc <- BASE_CORR2 %>% filter(Disp_Computadora == 1)
# perfil_tv <- BASE_CORR2 %>% filter(Disp_Television == 1)
# perfil_consola <- BASE_CORR2 %>% filter(Disp_Consola == 1)

resultado_perfil <- wilcox.test(perfil_celular$Indice_Fatiga_Total, 
                                perfil_pc$Indice_Fatiga_Total)
print(resultado_perfil)

resultado_fisher_1 <- fisher.test(table(BASE_CORR2$Frecuencia_examen_vista, BASE_CORR2$Oftalmologo), simulate.p.value = TRUE)
print(resultado_fisher_1)

resultado_fisher_2 <- fisher.test(table(BASE_CORR2$Frecuencia_examen_vista, BASE_CORR2$Lentes_Graduacion), simulate.p.value = TRUE)
print(resultado_fisher_2)

resultado_fisher_3 <- fisher.test(table(BASE_CORR2$Oftalmologo, BASE_CORR2$Lentes_Graduacion), simulate.p.value = TRUE)
print(resultado_fisher_3)

BASE_CORR2$Uso_Lentes_Proteccion <- rowSums(BASE_CORR2[, c("Lentes_Antirreflejo", "Lentes_FiltroAzul", 
                                                           "Lentes_Polarizados", "Lentes_VistaCansada", 
                                                           "Lentes_Fotocromaticos")], na.rm = TRUE)

# Convertimos a una variable categórica: "Si" (usa) o "No" (no usa)
BASE_CORR2$Uso_Lentes_Proteccion <- ifelse(BASE_CORR2$Uso_Lentes_Proteccion > 0, "Si", "No")
BASE_CORR2$Uso_Lentes_Proteccion <- as.factor(BASE_CORR2$Uso_Lentes_Proteccion)

resultado_brecha <- fisher.test(table(BASE_CORR2$Uso_Lentes_Proteccion, BASE_CORR2$Oftalmologo), simulate.p.value = TRUE)
print(resultado_brecha)

shapiro.test(BASE_CORR2$Indice_Fatiga_Total)


##############################################
### ANÁLISIS POR VARIABLE SOCIODEMOGRÁFICA ###
##############################################

# Asegurarnos de que las variables de nivel académico y edad existan
BASE_CORR2 <- BASE_CORR2 %>%
  mutate(
    # Asumimos que los que tienen Posg_NoEstudio == 0 están en algún posgrado
    Nivel = ifelse(Posg_NoEstudio == 1, "Licenciatura", "Posgrado"),
    
    # Crear grupos de edad (ajusta los cortes según tu distribución real)
    Grupo_Edad = cut(Edad, breaks = c(17, 22, 27, 60), 
                     labels = c("18-22", "23-27", "28+"))
  )
# Tabla por Nivel Académico
EDA_Nivel <- BASE_CORR2 %>%
  mutate(Sexo = factor(Sexo, levels = c(0, 1), labels = c("Hombres", "Mujeres"))) %>%
  mutate(
    Prom_celular = factor(Prom_celular, 
                          levels = 1:5, 
                          labels = c("Menos de 2 horas", "Entre 2 y 4 horas", "Entre 4 y 6 horas", "Entre 6 y 8 horas", "Más de 8 horas")),
    
    Tiempo_computadora = factor(Tiempo_computadora, 
                                levels = 0:5, 
                                labels = c("No uso computadora o tablet", "Menos de 60 minutos", "De 1 hora a 3 horas", "De 3 horas a 5 horas", "De 5 horas a 8 horas", "Más de 8 horas")),
    
    Tiempo_tv = factor(Tiempo_tv, 
                       levels = 0:4, 
                       labels = c("No uso televisión", "Menos de 60 minutos", "De 1 hora a 2 horas", "De 2 a 3 horas", "Más de 3 horas")),
    
    Consolas = factor(Consolas, 
                      levels = 0:3, 
                      labels = c("No uso consola de videojuegos", "Menos de 1 hora", "De 1 horas a 2 horas", "Más de 2 horas"))
  )%>%
  select(Nivel, Sexo, Indice_Fatiga_Total, Tiempo_computadora, Prom_celular,
         Tiempo_tv, Consolas, App_Youtube, App_Facebook, App_WhatsApp, App_Instagram,
         App_Netflix, App_X, App_Tiktok, App_Telegram, App_Videojuegos, App_Google,
         App_Chrome, App_Mihon, App_Teams) %>% 
  tbl_summary(
    by = Nivel,
    statistic = list(all_continuous() ~ "{mean} ({sd})", 
                     all_categorical() ~ "{n} ({p}%)")
  ) %>%
  add_overall() %>%
  modify_caption("**Perfil de salud visual por nivel académico**")

EDA_Nivel
cat(as_gt(EDA_Nivel) %>% gt::as_latex())


# Tabla por Edad
EDA_Edad <- BASE_CORR2 %>%
  mutate(Sexo = factor(Sexo, levels = c(0, 1), labels = c("Hombres", "Mujeres"))) %>%
  mutate(
    Prom_celular = factor(Prom_celular, 
                          levels = 1:5, 
                          labels = c("Menos de 2 horas", "Entre 2 y 4 horas", "Entre 4 y 6 horas", "Entre 6 y 8 horas", "Más de 8 horas")),
    
    Tiempo_computadora = factor(Tiempo_computadora, 
                                levels = 0:5, 
                                labels = c("No uso computadora o tablet", "Menos de 60 minutos", "De 1 hora a 3 horas", "De 3 horas a 5 horas", "De 5 horas a 8 horas", "Más de 8 horas")),
    
    Tiempo_tv = factor(Tiempo_tv, 
                       levels = 0:4, 
                       labels = c("No uso televisión", "Menos de 60 minutos", "De 1 hora a 2 horas", "De 2 a 3 horas", "Más de 3 horas")),
    
    Consolas = factor(Consolas, 
                      levels = 0:3, 
                      labels = c("No uso consola de videojuegos", "Menos de 1 hora", "De 1 horas a 2 horas", "Más de 2 horas"))
  )%>%
  select(Grupo_Edad, Indice_Fatiga_Total, Sexo, Tiempo_computadora, Prom_celular, Tiempo_tv, Consolas,
         App_Youtube,App_Facebook, App_WhatsApp, App_Instagram,
         App_Netflix, App_X, App_Tiktok, App_Telegram, App_Videojuegos, App_Google,
         App_Chrome, App_Mihon, App_Teams) %>%
  tbl_summary(
    by = Grupo_Edad,
    statistic = list(all_continuous() ~ "{mean} ({sd})", 
                     all_categorical() ~ "{n} ({p}%)")
  ) %>%
  add_overall() %>%
  modify_caption("**Perfil de salud visual por grupo de edad**")

EDA_Edad
cat(as_gt(EDA_Edad) %>% gt::as_latex())

#Tabla por SExo
EDA_Sexo <- BASE_CORR2 %>%
  mutate(Sexo = factor(Sexo, levels = c(0, 1), labels = c("Hombres", "Mujeres"))) %>%
  mutate(
    Prom_celular = factor(Prom_celular, 
                          levels = 1:5, 
                          labels = c("Menos de 2 horas", "Entre 2 y 4 horas", "Entre 4 y 6 horas", "Entre 6 y 8 horas", "Más de 8 horas")),
    
    Tiempo_computadora = factor(Tiempo_computadora, 
                                levels = 0:5, 
                                labels = c("No uso computadora o tablet", "Menos de 60 minutos", "De 1 hora a 3 horas", "De 3 horas a 5 horas", "De 5 horas a 8 horas", "Más de 8 horas")),
    
    Tiempo_tv = factor(Tiempo_tv, 
                       levels = 0:4, 
                       labels = c("No uso televisión", "Menos de 60 minutos", "De 1 hora a 2 horas", "De 2 a 3 horas", "Más de 3 horas")),
    
    Consolas = factor(Consolas, 
                      levels = 0:3, 
                      labels = c("No uso consola de videojuegos", "Menos de 1 hora", "De 1 horas a 2 horas", "Más de 2 horas"))
  )%>%
  select(Sexo, Indice_Fatiga_Total, Tiempo_computadora, Prom_celular, Tiempo_tv, Consolas,
         App_Youtube,App_Facebook, App_WhatsApp, App_Instagram,
         App_Netflix, App_X, App_Tiktok, App_Telegram, App_Videojuegos, App_Google,
         App_Chrome, App_Mihon, App_Teams) %>%
  tbl_summary(
    by = Sexo,
    statistic = list(all_continuous() ~ "{mean} ({sd})", 
                     all_categorical() ~ "{n} ({p}%)")
  ) %>%
  add_overall() %>%
  modify_caption("**Perfil de salud visual por sexo**")

EDA_Sexo
cat(as_gt(EDA_Sexo) %>% gt::as_latex())
