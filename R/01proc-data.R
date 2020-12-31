# 0. Procesamiento Articulo 3 
#1. Packages 
pacman::p_load(dplyr, haven, tidyverse, stringr, sjmisc)
#2. Load data base
issp19 <- read_stata(file = "data/issp019.dta")
issp <- read_sav("data/encuesta_cep83_jun2019.zip")

# 3. Explore data  base
# Pérez and Elbert (p.11)
## in the models we also include soci-odemographic controls such as gender (1 = female, 0 = male),
# age (in years),
# marital status (1 = married or live-in partner, 0 = other),
# and employment sector (1 = private, 0 = public)
# Class origin (ref. unskilled working class)
str(issp19) # labelled data
find_var(issp,  "regió")
find_var(issp, "P11")
issp <- select(issp19,ID_UNICO, starts_with("DS_P12"), DS_P27, DS_P2_EXACTA, DS_P1, FACTOR)
issp <- select(issp,ID_UNICO, REGION, DS_P27, DS_P2_EXACTA, DS_P1, FACTOR, DS_P5, DS_P7,DS_P9, DS_P4, DS_P30, DS_P11)

# 4. F(x) to clean an simplify strings
clean_strings <- function( s ) {
  clz <- class( s )
  s %<>%
    gsub( "^\\s+", "" , . ) %>% # leading whitepace
    gsub( "\\s+$", "" , . ) %>% # trailing whitespace
    gsub( "\"+", "\"", . )  %>% # embedded "'s
    gsub("[[:punct:]]", " ") %>% #extras
    as( clz ) # u don't mess with the type
  return( s )
}
simplify_strings <- function( s ) {
  s %<>%
    clean_strings() %>%
    gsub( "á", "a", . ) %>%
    gsub( "é", "e", . ) %>%
    gsub( "í", "i", . ) %>%
    gsub( "ó", "o", . ) %>%
    gsub( "ú", "u", . ) %>%
    gsub( "Ã³", "o", . ) %>%
    gsub( "Ã©", "e", . ) %>%
    gsub( "Ã±", "ñ", . ) %>% 
    gsub( "Ã¡", "a", . ) %>%
    gsub( "Ã", "i", . ) %>%
    tolower()
  return( s )
}


#5. Explorar variable ocupacion-----
tibble(issp)
sum(is.na(issp$DS_P12A_1)) #1373
sum(is.na(issp$DS_P12A_1)) #0
issp[issp == "-"] <- NA
issp1 <- issp %>% filter(!is.na(DS_P12A_1))
sum(is.na(issp$DS_P12A_1)) #349
# 5.1 Limpiar
issp$DS_P12A_1 <- issp$DS_P12A_1 %>% simplify_strings()
issp$DS_P12B_1 <- issp$DS_P12B_1 %>% simplify_strings()
issp$DS_P12C_1 <- issp$DS_P12C_1 %>% simplify_strings()

#5.2 Ver valores unicos
length(unique(issp$DS_P12A_1)) #662 ocupaciones distintas

#5.3 Cambios
issp$DS_P12A_1 <- issp$DS_P12A_1 %>% clean_strings()
issp$DS_P12B_1 <- issp$DS_P12B_1 %>% clean_strings()
issp$DS_P12C_1 <- issp$DS_P12C_1 %>% clean_strings()

issp19$DS_P12A_1 <- issp19$DS_P12A_1 %>% clean_strings()
issp19$DS_P12B_1 <- issp19$DS_P12B_1 %>% clean_strings()
issp19$DS_P12C_1 <- issp19$DS_P12C_1 %>% clean_strings()

length(unique(issp$DS_P12A_1)) #662 ocupaciones distintas


# 4. Imputar segun CIUO88

str_count(issp$DS_P12A_1, "director")
issp %>% 
  mutate(DS_P12A_1_cod = str_replace_all(DS_P12A_1, "jardinero", "61")) %>%
  select(c(DS_P12A_1,DS_P12B_1, DS_P12C_1,DS_P12A_1_cod))
issp %>% 
  mutate(DS_P12A_1_cod = ifelse(grepl("direct", DS_P12A_1, ignore.case = TRUE), "11", DS_P12A_1),
         ifelse(grepl("jefe", DS_P12A_1, ignore.case = TRUE), "12", DS_P12A_1),
         ifelse(grepl("jefe", DS_P12A_1, ignore.case = TRUE), "12", DS_P12A_1),
         ifelse(grepl("jefe|opera|producc", DS_P12A_1, ignore.case = TRUE), "13", DS_P12A_1),
         ifelse(grepl("administra", DS_P12A_1, ignore.case = TRUE), "23", DS_P12A_1)) %>%
  select(c(DS_P12A_1,DS_P12A_1_cod))

#4. Exportar e importar -----
#write.csv(issp, "data/iss2p-ocupacion.csv")
issp_ocupacion <- readxl::read_xlsx("data/issp-ciuo08-cod.xlsx")
issp_ocupacion[issp_ocupacion=="NA"] <- NA
# 5. Merge
issp19 <- merge(issp, issp_ocupacion, by.x = "ID_UNICO", by.y = "...1" )

# 6. Export
save(issp19, file= "data/issp19.RData")
