# Code 2: Estructura de Clases de E. O Wright ---------------
# Profesor Pablo Pérez y ayudante de investigacion Valentina Andrade

# 1. Cargar librarias
pacman::p_load(haven, dplyr, car, tidyverse, ggplot2, summarytools, magrittr)

# Esquema 2019 ----------------------
# 2. Cargar base de datos
rm(list = ls())
load("data/issp19.RData")

# 3.Seleccionar variables
# Esquema E.O Wright 
names(issp19)
issp <- select(issp19,
               SEXO=DS_P1,
               EDAD=DS_P2_EXACTA,
               UNION=DS_P27,
               WORK=DS_P5, # Employee/Unemployee
               EMPREL=DS_P7, # Asalariado/No Asalariado 
               WRKSUP=DS_P9, #Supervision
               ISCO08=ciuo_cod2, #Skills 
               EDUCYRS=DS_P4, # years of schooling
               TYPEWRK=DS_P11, #Empleo publico/privado
               region=REGION,
               identity_b=DS_P30,
               FACTOR=FACTOR) 

# 4. Recodificar variables -------------
# 4.1 Employment relation --------------------
# Proxy: (A) ¿WORRK o EMPREL? (# https://zacat.gesis.org/webview/index.jsp?object=http://zacat.gesis.org/obj/fStudy/ZA6770)
# (A) WORK: Are you currently working for pay, did you work for pay in the past, or have you never been in paid work?
# (B) EMPREL: Are/ were you an employee, self-employed, or working for your own family's business?

# A.1 Filtrar inactivos
issp <- filter(issp, WORK != 3) # 8 y 9 NS/NR

# A.2 Crear variable asalariados
issp$EMPREL <- as.numeric(issp$EMPREL) 
issp$prop_salaried <- car::recode(issp$EMPREL,recodes = "1='Salaried';c(5,2)='3.Petite bourgeoisie';3='2.Small employers';4='1.Capitalist';c(8,9)=NA", as.factor = T,
levels = c("Salaried", "3.Petite bourgeoisie", "2.Small employers","1.Capitalist"))

# Verificar
issp %>% count(prop_salaried) %>% mutate(prop = prop.table(n))
issp %>% count(EMPREL) %>% mutate(prop = prop.table(n))

# 4.3 Asalariados ------------------
# A.1 Supervisan --------------------
# Proxy: WRKSUP #Do/ did you supervise other employees? Yes/No
issp$WRKSUP <- as.numeric(issp$WRKSUP)
table(issp$WRKSUP)
issp$control <- car::recode(issp$WRKSUP,recodes = "1='Control';2='No control';99=NA",as.factor =T, 
                            levels = c("Control", "No control"))

# Verificar
issp %>% count(control) %>% mutate(prop = prop.table(n)) 

# A.2 Skills--------------------
# Proxy: ISCO08 #What is/ was your occupation - i.e., what is/ was the name or title of your main job?
# In your main job, what kind of activities do/ did you do most of the time?
# (https://zacat.gesis.org/webview/index.jsp?object=http://zacat.gesis.org/obj/fStudy/ZA6770)

## Educacion (Para controlar)
issp$EDUCYRS <- as.numeric(issp$EDUCYRS)
issp$educ <- car::recode(issp$EDUCYRS, recodes = c("0:5='No';6:9='Yes';c(88,99)=NA"))

table(issp$EDUCYRS)
table(issp$educ)

# A.2.1 Transformar a numérica
issp$ISCO08 <- as.numeric(issp$ISCO08)

#A.2.2 Eliminar FFAA
issp <- issp %>% filter(ISCO08!=1) # Preguntar JP 1, 
table(issp$ISCO08)

#A.2.3 Quedarme solo con los primeros dos digitos
issp$ISCO08_2 <- substr(issp$ISCO08, start = 1, stop = 2) 
table(issp$ISCO08_2)

#A.2.4 Agrupar por "skills"
# Analizar años de escolaridad EDUCYRS
issp %$% ctable(EDUCYRS, ISCO08_2==51, prop = "c")
# Analizas solo los TRUE

#Grupo 1: Experts (del 10 al 26)
#Grupo 2: Skilled.  Desde el código 30 hasta el 35. Sumar 60 61 y 72. 
#Grupo 3: Unskilled. Son 40, 41, 42, 43, 44,50, 51, 52, 53, 54, 62, 63, 71. Luego del 73 al 96.
#Grupo 4: NA
## Revisar grupo 36
issp$skillsA <- car::recode(issp$ISCO08_2, 
                           recodes = "10:26='Experts';c(30,31,32,33,34,35,36,60,61,72)='Skilled'
                               ;c(40,41,42,43, 44,50,51,52,53,54,62,63,70,71,73,74,75,76,76,77,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96)='Unskilled'")

issp <- issp %>% mutate(skills = if_else(skillsA =="Experts" & educ=="Yes", "Experts",
                               if_else(skillsA == "Experts" & educ=="No", "Skilled", skillsA)),
                        qual = case_when(ISCO08_2 %in% c(11:44) ~ 1,
                                         ISCO08_2 %in% c(51:96) ~ 2, TRUE ~ NA_real_))


# A.2.5 Verificar
issp %>% count(skills) %>% mutate(prop = prop.table(n))
issp %>% count(skillsA) %>% mutate(prop = prop.table(n))
issp %>% count(qual) %>% mutate(prop = prop.table(n))

# Se corrigen los expertos y los calificados (crecen calicifcados)

ctable(issp$prop_salaried, issp$skills)

#######5. FINAL CLASS VARIABLE#####

#.1 Se genera variable class
issp$class<- NA #crear variable vacia
issp$class <- with(issp, ifelse(prop_salaried=="1.Capitalist", 1, class))
issp$class <- with(issp, ifelse(prop_salaried=="2.Small employers", 2, class))
issp$class <- with(issp, ifelse(prop_salaried=="3.Petite bourgeoisie" & qual == 1, 3, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="Control" & skills=="Experts", 4, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="No control" & skills=="Experts", 5, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="Control" & skills=="Skilled", 6, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="Control" & skills=="Unskilled", 7, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="No control" & skills=="Skilled", 8, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="No control" & skills=="Unskilled", 9, class))
issp$class <- with(issp, ifelse(prop_salaried=="3.Petite bourgeoisie" & qual==2, 10, class))


#3.2 Etiquetar class
issp$class <- factor(issp$class,levels = c(1:10),
                     labels = c("1.Capitalists","2.Small employers","3.Petite bourgeoisie",
                                "4.Expert managers","5.Nonmanagerial experts",
                                "6.Skilled supervisors","7.Unskilled supervisors",
                                "8.Skilled workers","9.Unskilled workers", "10. Informal self-employed"))

table(issp$class)

# 3.3 Verificar
issp %>% 
  filter(!is.na(class)) %>% 
  count(class) %>% #n de casos
  mutate(proporcion = prop.table(n))#proporción

# 4. Identidadde clases
issp$identity_b <- as.numeric(issp$identity_b)
table(issp$identity_b)
issp$identity_b <- car::recode(issp$identity_b, recodes = c("c(88,99)=NA"))
issp19 <- issp %>%  mutate(year = as.Date("2019-1-1"),
                           identity_w = case_when(identity_b %in% c(2) ~ 1, identity_b %in% c(1,3,4,5,6) ~ 0, TRUE ~NA_real_ ),
                           identity_l = case_when(identity_b %in% c(1) ~ 1, identity_b %in% c(2,3,4,5,6) ~ 0, TRUE ~NA_real_ ),
                           identity_r = case_when(identity_b %in% c(1,2) ~ 1, identity_b %in% c(3,4,5,6) ~ 0, TRUE ~NA_real_ ),
                           identity_d = case_when(identity_b %in% c(2) ~ 2, identity_b %in% c(1) ~ 1,identity_b %in% c(3,4,5,6) ~ 0, TRUE ~NA_real_ )) 

issp19 %>%  select(contains("identity"), FACTOR) %$%  
sjmisc::frq(., weights = .$FACTOR)


# 5. Variables independientes 2019 ----

# Sexo
issp19$SEXO <- as.numeric(issp19$SEXO)
issp19$SEXO <- car::recode(issp19$SEXO, recodes =c("1='Hombre';2='Mujer'"), as.factor = T,
                           levels = c('Hombre', 'Mujer'))

# Edad
issp19$EDAD <- as.numeric(issp19$EDAD)

#Sindicato
issp19$UNION <- as.factor(issp19$UNION)
issp19$UNION <- car::recode(issp19$UNION, recodes = c("3='No';c(1,2)='Si';c(88,99)=NA"))

table(issp19$UNION)

# Empleo publico privado 
issp19$TYPEWRK <- as.numeric(issp19$TYPEWRK)
issp19$TYPEWRK <- car::recode(issp19$TYPEWRK, recodes = c("1='Público';2='Privado';c(8,9)=NA"), as.factor = T,
                              levels = c("Público", "Privado"))
issp19$typewrk <- as_factor(issp19$TYPEWRK)
table(issp19$typewrk)

# Region
table(issp19$region)
issp19$region <- as.numeric(issp19$region)
issp19$region <- ifelse(issp19$region == 13, 'Metropolitana','No metropolitana')
issp19$region <- as_factor(issp19$region)
table(issp19$region)

# 6. Guardar 
save(issp19, file = "data/issp19-proc.RData")

# Esquema 2009 ----------------------

# 2. Cargar base de datos
issp <- read_stata("data/ISSP2009.dta")

# 3.Seleccionar variables
# Otras: pregunta 15 percepcion desigualdad espacial
# 1. Propiedad; 2. Autoridad; 3. Calificaciones
names(issp)
issp <- select(issp,
               SEXO=SEX,
               EDAD=AGE,
               UNION=UNION,
               NEMPLOY, # Capitalist
               WRKTYPE, # Asalariado/No Asalariado 
               WRKSUP, #Supervision
               ISCO88, #Skills 
               EDUCYRS, # years of schooling
               identity_b=V66,
               region=CL_REG,
               FACTOR=WEIGHT) 

# 4. Recodificar variables -------------
# 4.1 Employment relation --------------------

# A.1 Filtrar inactivos
#Variable WRKTYPE: R: Working for private or public sector or self employed 
issp <- filter(issp, WRKTYPE != 0)
# --> 0 Never had paid work

# A.2 Crear variable asalariados
issp$WRKTYPE <- as.numeric(issp$WRKTYPE) 
issp$prop_salariedA <- car::recode(issp$WRKTYPE,recodes = "c(1,2,3)='Salaried';4='Self-employed'")

# Verificar
str(issp$prop_salariedA)
issp %>% count(prop_salariedA) %>% mutate(prop = prop.table(n))
issp %>% count(WRKTYPE) %>% mutate(prop = prop.table(n))


# 4.2 Propietarios ----------------
# Proxy: NEMPLOY (How many employees do/ did you have, not counting yourself?)
# A.1 Crear variable "owners":
#1.Pequeña burguesia: 0 a 1 empleados
#2. Pequeños empleadores: de 2 a 9 empleados
#3. Capitalist: de 10 a más empleados
issp$owners <- as.numeric(issp$NEMPLOY)
issp <- issp %>% mutate(owners = case_when(owners %in% c(1, 9995, NA_character_)& prop_salariedA=="Self-employed"~ "3.Petite bourgeoisie",
                                           owners %in% c(2:9) ~ "2.Small employers",
                                           owners %in% c(10:4800)~ "1.Capitalist",
                                           TRUE ~ NA_character_))

# Importante --> hay 1 que indican no tener ni 1 a 9995, indican NA asi que son incorporados en petite bourgoise
# A.2 Verificar
issp %>% count(owners) %>% mutate(prop = prop.table(n))
issp %>% count(NEMPLOY) %>% mutate(prop = prop.table(n))

# A.3 Var final salaried ----
issp <- issp %>% mutate(prop_salaried = if_else(is.na(owners), prop_salariedA, owners ))

# A.2 Verificar
issp %>% count(prop_salaried) %>% mutate(prop = prop.table(n))
issp %>% count(prop_salariedA) %>% mutate(prop = prop.table(n))


# 4.3 Asalariados ------------------
# A.1 Supervisan --------------------
# Proxy: WRKSUP #Do/ did you supervise other employees? Yes/No
issp$WRKSUP <- as.numeric(issp$WRKSUP)
table(issp$WRKSUP)
issp$control <- car::recode(issp$WRKSUP,recodes = "1='Control';2='No control';99=NA",as.factor =T, 
                            levels = c("Control", "No control"))

# Verificar
issp %>% count(control) %>% mutate(prop = prop.table(n)) 

# A.2 Skills--------------------
# Proxy: ISCO08 #What is/ was your occupation - i.e., what is/ was the name or title of your main job?
# In your main job, what kind of activities do/ did you do most of the time?
# (https://zacat.gesis.org/webview/index.jsp?object=http://zacat.gesis.org/obj/fStudy/ZA6770)

## Educacion (Para controlar)
issp$EDUCYRS <- as.numeric(issp$EDUCYRS)
issp$educ <- car::recode(issp$EDUCYRS, recodes = c("1:15='No';16:23='Yes'"))

table(issp$EDUCYRS)
table(issp$educ, useNA =  "ifany")
# A.2.1 Transformar a numérica
issp$ISCO88 <- as.numeric(issp$ISCO88)

#A.2.2 Eliminar FFAA
issp <- issp %>% filter(ISCO88!=110)
table(issp$ISCO88)

#A.2.3 Quedarme solo con los primeros dos digitos
issp$ISCO88_2 <- substr(issp$ISCO88, start = 1, stop = 2) 
table(issp$ISCO88_2)


#A.2.4 Agrupar por "skills"
# Analizar años de escolaridad EDUCYRS
issp %$% ctable(EDUCYRS, ISCO88_2==51, prop = "c")
# Analizas solo los TRUE

issp$skillsA <- car::recode(issp$ISCO88_2, 
                            recodes = "12:24='Experts';c(31,32,33,34,71,72,73,74)='Skilled'
                               ;c(41,42,51,52,61,62,81,82,83,91,92,93)='Unskilled'")

table(issp$skillsA, useNA = "ifany")
issp <- issp %>% mutate(skills = if_else(skillsA =="Experts" & educ=="Yes", "Experts",
                                         if_else(skillsA == "Experts" & educ=="No", "Skilled", skillsA)),
                        qual = case_when(ISCO88_2 %in% c(12:44) ~ 1,
                                         ISCO88_2 %in% c(51:96) ~ 2, TRUE ~ NA_real_)) %>% mutate(skills = if_else(is.na(skills),skillsA, skills))

# Los NA se dejan como estaban antes

# A.2.5 Verificar
issp %>% count(skills) %>% mutate(prop = prop.table(n))
issp %>% count(skillsA) %>% mutate(prop = prop.table(n))
issp %>% count(qual) %>% mutate(prop = prop.table(n))

# Se corrigen los expertos y los calificados (crecen calicifcados)
ctable(issp$prop_salaried, issp$skills)


#######5. FINAL CLASS VARIABLE 2009 #####
#5.1 Se genera variable class
issp$class<- NA #crear variable vacia
issp$class <- with(issp, ifelse(prop_salaried=="1.Capitalist", 1, class))
issp$class <- with(issp, ifelse(prop_salaried=="2.Small employers", 2, class))
issp$class <- with(issp, ifelse(prop_salaried=="3.Petite bourgeoisie" & qual == 1, 3, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="Control" & skills=="Experts", 4, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="No control" & skills=="Experts", 5, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="Control" & skills=="Skilled", 6, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="Control" & skills=="Unskilled", 7, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="No control" & skills=="Skilled", 8, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="No control" & skills=="Unskilled", 9, class))
issp$class <- with(issp, ifelse(prop_salaried=="3.Petite bourgeoisie"  & qual == 2, 10, class))

#3.2 Etiquetar class
issp$class <- factor(issp$class,levels = c(1:10),
                     labels = c("1.Capitalists","2.Small employers","3.Petite bourgeoisie",
                                "4.Expert managers","5.Nonmanagerial experts",
                                "6.Skilled supervisors","7.Unskilled supervisors",
                                "8.Skilled workers","9.Unskilled workers", "10. Informal self-employed"))


# 3.3 Verificar
issp %>% 
  filter(!is.na(class)) %>% 
  count(class) %>% #n de casos
  mutate(proporcion = prop.table(n))#proporción

# 4. Identidad de clases ---------------
issp$identity_b <- as.numeric(issp$identity_b)

issp09 <- issp %>%  mutate(year = as.Date("2009-1-1"),
                           identity_w = case_when(identity_b %in% c(2) ~ 1, identity_b %in% c(1,3,4,5,6) ~ 0, TRUE ~NA_real_ ),
                           identity_l = case_when(identity_b %in% c(1) ~ 1, identity_b %in% c(2,3,4,5,6) ~ 0, TRUE ~NA_real_ ),
                           identity_r = case_when(identity_b %in% c(1,2) ~ 1, identity_b %in% c(3,4,5,6) ~ 0, TRUE ~NA_real_ ),
                           identity_d = case_when(identity_b %in% c(2) ~ 2, identity_b %in% c(1) ~ 1,identity_b %in% c(3,4,5,6) ~ 0, TRUE ~NA_real_ )) 

issp09 %>%  select(contains("identity"), FACTOR) %$%  
  sjmisc::frq(., weights = .$FACTOR)

# 5. Variables independientes 2009 ----
# Sexo
issp09$SEXO <- as.numeric(issp09$SEXO)
issp09$SEXO <- car::recode(issp09$SEXO, recodes =c("1='Hombre';2='Mujer'"), as.factor = T,
                           levels = c('Hombre', 'Mujer'))

table(issp09$SEXO)
# Edad
issp09$EDAD <- as.numeric(issp09$EDAD)

#Sindicato
issp09$UNION <- as.factor(issp09$UNION)
issp09$UNION <- car::recode(issp09$UNION, recodes = c("3='No';c(1,2)='Si';c(88,99)=NA"))

table(issp09$UNION)

#Publico/Privado
table(issp09$WRKTYPE)
issp09$TYPEWRK <- car::recode(issp09$WRKTYPE, recodes = c("c(1,2)='Público';c(4,3)='Privado'"), as.factor = T,
                              levels = c("Público", "Privado"))
issp09$typewrk <- as_factor(issp09$TYPEWRK)
table(issp09$TYPEWRK)
table(issp19$TYPEWRK)
levels(issp09$TYPEWRK)
levels(issp19$TYPEWRK)

# Region
table(issp09$region)
issp09$region <- as.numeric(issp09$region)
issp09$region <- ifelse(issp09$region == 13, 'Metropolitana','No metropolitana')
issp09$region <- as_factor(issp09$region)
table(issp09$region)

# 6. Guardar  ----------
save(issp, file = "data/issp09-proc.RData")

# Esquema 1999 ----------------------

# 2. Cargar base de datos
issp <- read_stata("data/ISSP1999.dta")

# 2.1 Filtrar base para chile
issp <- issp %>% filter(v3 == 30)

# 3.Seleccionar variables
issp <- select(issp,
               SEXO=sex,
               EDAD = age,
               UNION=union,
               NEMPLOY=nemploy, # Capitalist
               WRKTYPE=wrkgovt, # Asalariado/No Asalariado 
               WRKSUP=wrksup, #Supervision
               ISCO88=isco88_4, #Skills 
               EDUCYRS=educyrs, # years of schooling
               identity_b=class,
               selfemp,
               region=x_reg,
               FACTOR=weight) 

# 4. Recodificar variables -------------
# 4.1 Employment relation --------------------
# A.1 Filtrar inactivos
issp <- filter(issp, WRKTYPE != 0)
# issp<- filter(issp, selfemp != 0)
#0. Never had paid work

# A.2 Crear variable asalariados
issp$WRKTYPE <- as.numeric(issp$WRKTYPE) 
issp$prop_salariedA <- car::recode(issp$WRKTYPE,recodes = "c(1,2,3,6)='Salaried';8='Self-employed'; 9=NA")

# Verificar
issp %>% count(prop_salariedA) %>% mutate(prop = prop.table(n))
issp %>% count(WRKTYPE) %>% mutate(prop = prop.table(n))

# # A.2 Crear variable asalariados
# issp$selfemp <- as.numeric(issp$selfemp) 
# issp$selfemp <- car::recode(issp$selfemp,recodes = "2='Salaried';1='Self-employed'", as.factor = T)
## Verificar
# issp %>% count(selfemp) %>% mutate(prop = prop.table(n))

# 4.2 Propietarios ----------------
# Proxy: NEMPLOY (How many employees do/ did you have, not counting yourself?)

# A.1 Crear variable "owners":
#1.Pequeña burguesia: 0 a 1 empleados
#2. Pequeños empleadores: de 2 a 9 empleados
#3. Capitalist: de 10 a más empleados
issp$owners <- as.numeric(issp$NEMPLOY)
issp <- issp %>% mutate(owners = case_when(owners %in% c(0,1, 9995, 9999, 9997)& prop_salariedA=="Self-employed"~ "3.Petite bourgeoisie",
                                           owners %in% c(2:9) ~ "2.Small employers",
                                           owners %in% c(10:9990)~ "1.Capitalist",
                                           TRUE ~ NA_character_))

# 14 self-employed que indican 9997(refused) y 9999 (no anwser)y 0 Not available/ NAP: Not in labour force; not self-employed

# A.2 Verificar
issp %>% count(owners) %>% mutate(prop = prop.table(n))

# A.3 Var final salaried ----
issp <- issp %>% mutate(prop_salaried = if_else(is.na(owners), prop_salariedA, owners ))

# A.2 Verificar
issp %>% count(prop_salaried) %>% mutate(prop = prop.table(n))
issp %>% count(prop_salariedA) %>% mutate(prop = prop.table(n))

# 4.3 Asalariados ------------------
# A.1 Supervisan --------------------
# Proxy: WRKSUP #Do/ did you supervise other employees? Yes/No
issp$WRKSUP <- as.numeric(issp$WRKSUP)
table(issp$WRKSUP)
issp$control <- car::recode(issp$WRKSUP,recodes = "1='Control';2='No control';c(0,7, 8,9)=NA",as.factor =T, 
                            levels = c("Control", "No control"))

# Verificar
issp %>% count(control) %>% mutate(prop = prop.table(n)) 
# Mucha perdida de NA

# A.2 Skills--------------------
# Proxy: ISCO08 #What is/ was your occupation - i.e., what is/ was the name or title of your main job?
# In your main job, what kind of activities do/ did you do most of the time?
# (https://zacat.gesis.org/webview/index.jsp?object=http://zacat.gesis.org/obj/fStudy/ZA6770)

## Educacion (Para controlar)
issp$EDUCYRS <- as.numeric(issp$EDUCYRS)
issp$educ <- car::recode(issp$EDUCYRS, recodes = c("1:15='No';16:23='Yes'"))
table(issp$EDUCYRS)
table(issp$educ, useNA =  "ifany")
# A.2.1 Transformar a numérica
issp$ISCO88 <- as.numeric(issp$ISCO88)

#A.2.2 Eliminar FFAA
issp <- issp %>% filter(ISCO88!= 0)  
table(issp$ISCO88)

#A.2.3 Quedarme solo con los primeros dos digitos
issp$ISCO88_2 <- substr(issp$ISCO88, start = 1, stop = 2) 
table(issp$ISCO88_2)


#A.2.4 Agrupar por "skills"

# Analizar años de escolaridad EDUCYRS
issp %$% ctable(EDUCYRS, ISCO88_2==51, prop = "c")
# Analizas solo los TRUE

#Grupos no especficados a 2 d no estaban; 25,75,79 [Master craftsman, supervisor] tampoco
issp$skillsA <- car::recode(issp$ISCO88_2, 
                            recodes = "1:25='Experts';c(30, 31,32,33,34,70, 71,72,73,74, 75,79)='Skilled'
                               ;c(40, 41,42,51,52,61,62,80, 81,82,83,84, 90,91,92,93)='Unskilled'; 99='99'")


table(issp$skillsA, useNA = "ifany")
issp <- issp %>% mutate(skills = if_else(skillsA =="Experts" & educ=="Yes", "Experts",
                                         if_else(skillsA == "Experts" & educ=="No", "Skilled",
                                                 if_else(skillsA == "99", NA_character_, "Unskilled"))),
                        qual = case_when(ISCO88_2 %in% c(12:44) ~ 1,
                                         ISCO88_2 %in% c(51:96) ~ 2, TRUE ~ NA_real_))


# A.2.5 Verificar
issp %>% count(skills) %>% mutate(prop = prop.table(n))
issp %>% count(skillsA) %>% mutate(prop = prop.table(n))
issp %>% count(qual) %>% mutate(prop = prop.table(n))

# Se corrigen los expertos y los calificados (crecen calicifcados)
# Grupo de personas que no reporta educ (puse unskileed)

#######5. FINAL CLASS VARIABLE#####

#.1 Se genera variable class
issp$class<- NA #crear variable vacia
issp$class <- with(issp, ifelse(prop_salaried=="1.Capitalist", 1, class))
issp$class <- with(issp, ifelse(prop_salaried=="2.Small employers", 2, class))
issp$class <- with(issp, ifelse(prop_salaried=="3.Petite bourgeoisie" & qual == 1, 3, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="Control" & skills=="Experts", 4, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="No control" & skills=="Experts", 5, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="Control" & skills=="Skilled", 6, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="Control" & skills=="Unskilled", 7, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="No control" & skills=="Skilled", 8, class))
issp$class <- with(issp, ifelse(prop_salaried=="Salaried" & control=="No control" & skills=="Unskilled", 9, class))
issp$class <- with(issp, ifelse(prop_salaried=="3.Petite bourgeoisie" & qual==2, 10, class))


#3.2 Etiquetar class
issp$class <- factor(issp$class,levels = c(1:10),
                     labels = c("1.Capitalists","2.Small employers","3.Petite bourgeoisie",
                                "4.Expert managers","5.Nonmanagerial experts",
                                "6.Skilled supervisors","7.Unskilled supervisors",
                                "8.Skilled workers","9.Unskilled workers", "10. Informal self-employed"))

# 3.3 Verificar
issp %>% 
  filter(!is.na(class)) %>% 
  count(class) %>% #n de casos
  mutate(proporcion = prop.table(n))#proporción
# 4. Identidad de clases 1999 ---------
issp$identity_b <- as.numeric(issp$identity_b)
table(issp$identity_b)
issp$identity_b <- car::recode(issp$identity_b, recodes = c("c(97,98,99)=NA"))

issp99 <- issp %>%  mutate(year = as.Date("1999-1-1"),
                           identity_w = case_when(identity_b %in% c(2) ~ 1, identity_b %in% c(1,3,4,5,6) ~ 0, TRUE ~NA_real_ ),
                           identity_l = case_when(identity_b %in% c(1) ~ 1, identity_b %in% c(2,3,4,5,6) ~ 0, TRUE ~NA_real_ ),
                           identity_r = case_when(identity_b %in% c(1,2) ~ 1, identity_b %in% c(3,4,5,6) ~ 0, TRUE ~NA_real_ ),
                           identity_d = case_when(identity_b %in% c(2) ~ 2, identity_b %in% c(1) ~ 1,identity_b %in% c(3,4,5,6) ~ 0, TRUE ~NA_real_ )) 

issp99 %>%  select(contains("identity"), FACTOR) %$%  
  sjmisc::frq(., weights = .$FACTOR)

# 5. Variables independientes 1999 ----
# Sexo
issp99$SEXO <- as.numeric(issp99$SEXO)
issp99$SEXO <- car::recode(issp99$SEXO, recodes =c("1='Hombre';2='Mujer'"), as.factor = T,
                         levels = c('Hombre', 'Mujer'))

table(issp99$SEXO)
str
# Edad
issp99$EDAD <- as.numeric(issp99$EDAD)

#Sindicato
issp99$UNION <- as.factor(issp99$UNION)
issp99$UNION <- car::recode(issp99$UNION, recodes = c("2='No';1='Si';c(88,99)=NA"))

table(issp99$UNION)

# Publico/Privado
table(issp99$WRKTYPE)
issp99$TYPEWRK <- car::recode(issp99$WRKTYPE, recodes = c("c(1,2)='Público';c(8,3)='Privado';9=NA"), as.factor = T,
                              levels = c("Público", "Privado"))
issp99$typewrk <- as_factor(issp99$TYPEWRK)
table(issp09$TYPEWRK)
table(issp19$TYPEWRK)
levels(issp99$TYPEWRK)
levels(issp09$TYPEWRK)
levels(issp19$typewrk)

# Region
table(issp99$region)
issp99$region <- as.numeric(issp99$region)
issp99$region <- ifelse(issp99$region == 3013, 'Metropolitana','No metropolitana')
issp99$region <- as_factor(issp99$region)
table(issp99$region)

# Guardar 
save(issp99, file = "data/issp99-proc.RData")

# Unir tres bases ----------
issp <- bind_rows(issp19,issp09,issp99)
issp19_09_99 <- issp %>% select(year, class, sex=SEXO,region, age=EDAD, educ, union=UNION,typewrk=TYPEWRK, starts_with("identity"), FACTOR)

# Guardar 
save(issp19_09_99,issp19, issp09,issp99, file = "data/issp-proc2.RData") # Cambiar issp19-09-99
foreign::write.dta(issp, "data/issp-proc.dta") 
