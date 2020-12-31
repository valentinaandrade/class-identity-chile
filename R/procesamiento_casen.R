# 0. Procesamiento de datos : CASEN
# Valentina Andrade
# 13 de Noviembre

# A. Cargar librerías -------------------
pacman::p_load(haven, dplyr, survey, srvyr)

# B. Cargar datos--------------------
casen <- read_spss("data/Casen 2017.sav")

#C. Seleccion -------------------
names(casen)
casen1 <- casen %>%  select(starts_with("o"), edad, sexo, s12,
                            oficio4, rama1, rama4, region, expr)
# D. Filtros relevantes ------------
# Mayor a 18 años
# Trabajadores publicos  (o15)
casen1 <- casen1 %>%  filter(edad > 18 & !is.na(o24a))

# E. Recodificar ------------
# 5. Edad ----
# Crear objeto suvrye
casen1<-casen1 %>% mutate(edad_cat= case_when(edad<40 ~ 1,
                                              edad>49 & edad<60 ~ 2,
                                              edad>59 & edad<80 ~ 3,                                            TRUE ~ NA_real_))
casen1$edad_cat<-as.factor(casen1$edad_cat)

# 6. Sexo
casen1$sexo<-as.factor(casen1$sexo)

# 7. Region
casen1$region<-as.factor(casen1$region)

#8.  Estamento ----------
casen1$oficio1<-as.factor(casen1$oficio1)
casen1$oficio1 <- car::recode(casen1$oficio1, recodes = c('999 = NA'))

#9. Rama-------------
table(casen1$rama1)
# "Administracion Central" 
# "Educacion Municipal"    
# "Educacion Universitaria"
# "FFAA y Orden"           
# "Judicial"               
# "Municipal"              
#  "Obras Sanitarias"       
#  "Otros"                  
#  "Salud Hospitalaria"     
#  "Salud Municipal"        
#  "SII"
casen1$rama1<-as.factor(casen1$rama1)

# 10.  Jornada ---------------------
table(casen1$o18)
casen1$o18 <- as.factor(casen1$o18)
casen1$jornada <- car::recode(casen1$o18, recodes = c("1='Completa';2='Parcial';3:9=NA"), as.factor = T,
                              levels = c('Parcial', 'Completa'))

#11. Tipo de contrato----------------
table(isp$b6)
table(casen1$o14) # 1 es honorario y 3 No, 2 Y 9 NA
table(casen1$o16) # 1 indefinido y 2 es fijo 9NA
table(casen1$o20) # 2 y 3 subcontratado
table(casen1$o16, casen1$o14)
casen1$o16 <- as.factor(casen1$o16)
casen1$contrato <- car::recode(casen1$o16, recodes = c("1='Indefinido';2='Fijo';9=NA"), as.factor = T,
                               levels = c('Indefinido', 'Fijo'))

#12. Sindicato-------------
casen1$sindicato <- as.factor(casen1$o24a)
casen1$sindicato <- car::recode(casen1$sindicato, recodes = c("1='Sindicalizado';2='No sindicalizado'"), as.factor = T,
                               levels = c('Sindicalizado', 'No sindicalizado'))
table(casen1$sindicato)
#13.  Sistema previsional--------------------
casen1$s12 <- as.factor(casen1$s12)
casen1$salud <- car::recode(casen1$s12, recodes = c("1:5='FONASA';6='CAPREDENA/DIPRECA';7='ISAPRE';8:hi=NA "), as.factor = T,
                            levels = c('FONASA', 'ISAPRE', 'CAPREDENA/DIPRECA'))

#14.  Tamano--------------------
casen1$o23 <- as.factor(casen1$o23)
casen1$tamano <- car::recode(casen1$o23, recodes = c("'A'='Solo 1';c('B','C')='2 a 9';c('D','E')='10 a 199';'F'='20 a mas'; 'X'=NA"), as.factor = T,
                            levels = c('Solo 1', '2 a 9', '10 a 199', '20 a mas'))

table(casen1$tamano)
# F.  Expandir ########----------
casen_regional <- casen1 %>% as_survey_design(ids = 1, weights = expr)
options(survey.lonely.psu = "certainty" )

## Región --------
region<-casen_regional %>% group_by(sindicato,region) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                          total = survey_total(vartype = "ci",na.rm = TRUE))
## Sexo ---------
sexo <-casen_regional %>% group_by(sindicato,sexo) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                       total = survey_total(vartype = "ci",na.rm = TRUE))
## Edad -----------
edad <-casen_regional %>% group_by(sindicato,edad_cat) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                           total = survey_total(vartype = "ci",na.rm = TRUE))
## Rama --------
rama <-casen_regional %>% group_by(sindicato,rama1) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                       total = survey_total(vartype = "ci",na.rm = TRUE))
## Ocupación -----------
ocup <-casen_regional %>% group_by(sindicato,oficio1) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                          total = survey_total(vartype = "ci",na.rm = TRUE))
## Jornada -----------
jor <-casen_regional %>% group_by(sindicato,jornada) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                         total = survey_total(vartype = "ci",na.rm = TRUE))
## Tipo de contrato
contrato <-casen_regional %>% group_by(sindicato,contrato) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                               total = survey_total(vartype = "ci",na.rm = TRUE))
## Salud
salud <-casen_regional %>% group_by(sindicato,salud) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                         total = survey_total(vartype = "ci",na.rm = TRUE))

## Tamano
tamano <-casen_regional %>% group_by(sindicato,tamano) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                                   total = survey_total(vartype = "ci",na.rm = TRUE))

# X. Guardar----------
save(region,sexo,edad, ocup, rama, jor, contrato, tamano, salud, file = "data/casen-expandida.RData")
writexl::write_xlsx(list(region,sexo,edad, ocup, rama, jor, contrato, tamano, salud),  "data/tablas-casen.xlsx", col_names = TRUE,format_headers = TRUE)
