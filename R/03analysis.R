# Code 3: Analisis

# 1. Cargar librarias
pacman::p_load(tidyverse, sjPlot, summarytools,
               magrittr, ggmosaic, texreg, kableExtra)

# 2. Cargar base de datos
rm(list = ls())
load("data/issp-proc.RData")
issp <- issp19_09_99; remove(issp19_09_99)

# 3. Analisis descriptivo 
issp <- issp %>% group_by(year) 
print(summarytools::dfSummary(issp),  method="viewer")

# Conclusiones
## identity_b: 2019 está más marcada hacia 2

## Figura 1

issp$identity_b <- car::recode(issp$identity_b, recodes = c("1='Clase baja'; 2='Clase trabajadora';
                                                            3='Clase media-baja'; 4='Clase media'; 5='Clase media-alta';
                                                            6='Clase alta'"), as.factor = T,
                               levels = c('Clase baja', 'Clase trabajadora', 'Clase media-baja', 'Clase media',
                                          'Clase media-alta', 'Clase alta')) 

issp %>% 
  filter(!is.na(class), !is.na(identity_b)) %>% group_by(year) %>% 
  count(identity_b) %>% #n de casos
  mutate(p= prop.table(n)) %>% 
  ggplot(aes(x = year, y = p*100, color = identity_b)) +
  geom_line(size = 1) + geom_point () + facet_wrap(.~ identity_b) + 
  scale_x_date(name = "",breaks = as.Date(c("1999-1-1", "2009-1-1", "2019-1-1")), date_labels = "%Y") +
  scale_y_continuous(name = "%  ", labels = function(x) paste0(x, "%")) + guides(color = F) + theme_sjplot() + scale_color_grey()

ggsave(plot = last_plot(),
       filename = "images/figura1.png",
       device = "png",
       dpi = 500,
       units = "cm",
       width = 32,
       height = 15)


# Descriptivos modelo
tab_xtab(var.row = issp$identity_d,issp$year,
         show.cell.prc = T,show.summary = F)

tab_xtab(var.row = issp$identity_d,issp$class,
         show.cell.prc = T,show.summary = F, encoding = " ")

ctable(issp$identity_r, issp$year, prop = "c")

ctable(issp$class, issp$identity_d, prop = "r")


# 4. Estimar modelo
# Predictores categóricos
issp <- issp %>%  filter(!is.na(class))

issp$class<- as_factor(issp$class) # Clase
issp$year<- as.character(issp$year) # Year
issp$year<-  as_factor(issp$year)
issp$year<-  fct_rev(issp$year)
issp$sex<-  as_factor(issp$sex) # Sex
issp$union<-  as_factor(issp$union) # Union


# Modelos
# Modelos
m00 <- glm(identity_r ~1,data = issp,family = "binomial", weights = FACTOR)
m01 <- glm(identity_r~class,data = issp,family = "binomial", weights = FACTOR)
m02 <- glm(identity_r~ class + year,data = issp,family = "binomial", weights = FACTOR)
m03 <- glm(identity_r~ class + year + union,data = issp,family = "binomial", weights = FACTOR)
m04 <- glm(identity_r~ class + year + union + sex + age,data = issp,family = "binomial", weights = FACTOR)
m05 <- glm(identity_r~ class*year,data = issp,family = "binomial", weights = FACTOR)


# 5. Presentacion
# Tabla
screenreg(l = list(m00,m01,m02,m03,m04),
        custom.model.names = c("Modelo 0","Modelo 1","Modelo 2","Modelo 3", "Modelo 4"))

# 5.2 Plots
# Probabilidades predichas  segun clase
plot_model(m01,type = "pred",
           terms = "class",
           title = "Probabilidades predichas para identidad de clase según clase social") + geom_line()


# Plot de Odds Ratio
plot_model(m04,vline.color = "grey")


# 6. Ajuste 

# 6.1 Pseudo R
mfr2.00 <- DescTools::PseudoR2(m00)
mfr2.01 <- DescTools::PseudoR2(m01)
mfr2.02 <- DescTools::PseudoR2(m02)

r2<- as.data.frame(cbind(c(mfr2.00,mfr2.01,mfr2.02)))
rownames(r2) <- c("Modelo nulo",
                  "Modelo 1",
                  "Modelo 2")
knitr::kable(r2,digits = 3, col.names = c("McFadden R2"))


#6.2 Test devianza
test01 <- anova(m00,m01, test = "Chisq")
test02 <- anova(m00,m02, test = "Chisq")
lrt01 <- rbind (test01, test02) %>%  unique()
row.names(lrt01) <- c("Modelo nulo",
                      "Modelo 1",
                      "Modelo 2")

knitr::kable(lrt01,digits = 3, col.names = c("McFadden R2"))


## Pendiente
# Controles sociodemográficos: edad, genero, region (metropolitana y otra)
# Sindicalización 

#7. Re especificar modelos
table(issp$class)
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}
issp <- issp %>% mutate(class2 = case_when(class == "1.Capitalists"~ "1.Capitalists and Directors",
                                           class == "4.Expert managers"~ "1.Capitalists and Directors",
                                           class == "2.Small employers"~ "2.Small employers",
                                           class == "3.Petite bourgeoisie"~ "3.Petite bourgeoisie",
                                           class == "5.Nonmanagerial experts"~ "4.Nonmanagerial experts",
                                           class == "6.Skilled supervisors"~ "5.Skilled supervisors",
                                           class == "7.Unskilled supervisors"~ "6.Unskilled supervisors",
                                           class == "8.Skilled workers"~ "7.Skilled workers",
                                           class == "9.Unskilled workers"~ "8.Unskilled workers",
                                           class == "10. Informal self-employed"~ "9. Informal self-employed",
                                           TRUE ~ NA_character_))

table(issp$class2)
issp$class2 <- factor(issp$class2, levels = c("1.Capitalists and Directors", "2.Small employers","3.Petite bourgeoisie",
                                                 "4.Nonmanagerial experts","5.Skilled supervisors", "6.Unskilled supervisors",
                                                 "7.Skilled workers", "8.Unskilled workers", "9. Informal self-employed"))
issp$class2 <- as_factor(issp$class2)

## Models ---------
# Modelos
issp$year<- as.character(issp$year) # Year
issp$year<-  as_factor(issp$year)
issp$year<-  fct_rev(issp$year)
issp$sex<-  as_factor(issp$sex) # Sex
issp$union<-  as_factor(issp$union) # Union

m10 <- glm(identity_r ~1,data = issp,family = "binomial", weights = FACTOR)
m11 <- glm(identity_r~class2,data = issp,family = "binomial", weights = FACTOR)
m12 <- glm(identity_r~ class2 + year,data = issp,family = "binomial", weights = FACTOR)
m13 <- glm(identity_r~ class2 + year + union,data = issp,family = "binomial", weights = FACTOR)
m14 <- glm(identity_r~ class2 + year + union + sex + age,data = issp,family = "binomial", weights = FACTOR)
m15 <- glm(identity_r~ class2*year,data = issp,family = "binomial", weights = FACTOR)

