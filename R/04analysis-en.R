# Code 04: Analysis -------------------------------------------------------
# 1. Cargar librarias
pacman::p_load(tidyverse, sjPlot, summarytools,
               magrittr, ggmosaic, texreg, kableExtra,
               ggeffects)

## Setup
theme_set(theme_sjplot2())

# 2. Cargar base de datos
rm(list = ls())
load("data/issp-proc2.RData")
issp <- issp19_09_99; remove(issp19_09_99)
issp <- issp %>%
  filter(year != '1999-01-01', !is.na(class)&!is.na(identity_r)&!is.na(union)& !is.na(typewrk)&!is.na(region)&!is.na(sex)&!is.na(age), !is.na(class))


# 4. Estimar modelo
# Predictores categóricos
issp <- issp %>%  filter(!is.na(class), !is.na(age), !is.na(sex), !is.na(union), !is.na(typewrk))

issp$class<- as_factor(issp$class) # Clase
issp <- within(issp, class <- relevel(class, ref = "9.Unskilled workers"))

issp$year<- as.character(issp$year) # Year
issp$year<-  as_factor(issp$year)
issp$year<-  fct_rev(issp$year)
issp$sex<-  as_factor(issp$sex) # Sex
issp$union<-  as_factor(issp$union) # Union
issp <- within(issp, union <- relevel(union, ref = "No"))

#issp$typewrk<-  as_factor(issp$typewrk) # Union

# Modelos sustantivos
m00 <- glm(identity_r~ 1,data = issp,family = "binomial", weights = FACTOR)
m01 <- glm(identity_r~ class + union +year  + typewrk + sex + age + region,data = issp,family = "binomial", weights = FACTOR)
m02 <- glm(identity_r~ class +  union+ year  + typewrk + sex + age + region + year*class,data = issp,family = "binomial", weights = FACTOR)
m03 <- glm(identity_r~ class  + union + year + typewrk + sex + age + region + year*union,data = issp,family = "binomial", weights = FACTOR)

# 5. Presentacion
labs01 <- c("Intercepto", "1. Empresarios", "2.Pequeños empleadores",
            "3.Pequeña burguesía formal", "4.Expertos Directivos",
            "5.Expertos sin autoridad", "6.Supervisores calificados",
            "7.Supervisores no calificados", "8.Trabajadores calificados",
            "10.Autoempleados informales", "Sindicalizado", "2019", "Empleado sector privado (ref: Público)",
            "Mujer (ref:Hombre)", "Edad (en años)", "Region No RM (ref: Metropolitana)",
            "1. Empresarios- 2019", "2.Pequeños empleadores-2019", "3.Pequeña burguesía formal-2019", "4.Expertos managers-2019", "5.Expertos sin autoridad-2019", "6.Supervisores calificados-2019", "7.Supervisores no calificados-2019", "8.Trabajadores calificados-2019", "10.Autoempleados informales-2019",
            "Sindicalización-2019")

# Tabla
htmlreg(l = list(m01,m02,m03),
        groups = list(" " = 1,"<b>Posición de clase</b> (ref: 9.Trabajadores no calificados)" = 2:10,
                      "<b>Sindicalización</b> (ref: No)" = 11,
                      "<b>Años</b> (ref: 2009)" = 12,
                      "<b>Controles sociodemográficos</b>" = 13:16,
                      "<b>Interacciones</b>" = 17:26),
        custom.model.names = c("Modelo 1","Modelo 2","Modelo 3"),
        caption= "",
        caption.above = "Tabla 3. Determinantes de la identidad de clase trabajadora en Chile, 1999 – 2019",
        include.aic = F,
        include.bic = F,
        include.pseudors = TRUE,
        custom.gof.rows = list("Pseudo R$^{2}$" = c("00.946","0.1066","00.953")), 
        custom.note = "$^{***}$ p < 0.001; $^{**}$ p < 0.01; $^{*}$ p < 0.05 <br> Errores estándar entre paréntesis \n**Nota**: Para identidad de clases se utilizó una recodificación restrictiva, esto es, considerando solo de 'clase trabajadora' a quienes indicador ser de 'Clase Trabajadora' y 'Clase baja'")

htmlreg(l = list(m01,m02,m03),
        custom.coef.names =labs01, file = "output/modelo-2.doc",
        groups = list(" " = 1,"<b>Posición de clase</b> (ref: 9.Trabajadores no calificados)" = 2:10,
                      "<b>Sindicalización</b> (ref: No)" = 11,
                      "<b>Años</b> (ref: 2009)" = 12,
                      "<b>Controles sociodemográficos</b>" = 13:16,
                      "<b>Interacciones</b>" = 17:26),
        custom.model.names = c("Modelo 1","Modelo 2","Modelo 3"),
        caption= "",
        caption.above = "Tabla 3. Determinantes de la identidad de clase trabajadora en Chile, 1999 – 2019",
        include.aic = F,
        include.bic = F,
        include.pseudors = TRUE,
        custom.gof.rows = list("Pseudo R$^{2}$" = c("00.946","0.1066","00.953")), 
        custom.note = "$^{***}$ p < 0.001; $^{**}$ p < 0.01; $^{*}$ p < 0.05 <br> Errores estándar entre paréntesis \n**Nota**: Para identidad de clases se utilizó una recodificación restrictiva, esto es, considerando solo de 'clase trabajadora' a quienes indicador ser de 'Clase Trabajadora' y 'Clase baja'")

# Figura 3 ----------------------------------------------------------------
# Probabilidades predichas para identidad de clase según clase social
fig2class <-  plot_model(m01,type = "pred",
                         terms = "class [1.Capitalists, 2.Small employers,3.Petite bourgeoisie,4.Expert managers, 5.Nonmanagerial experts, 6.Skilled supervisors, 7.Unskilled supervisors, 8.Skilled workers, 9.Unskilled workers, 10. Informal self-employed]",
                         title = "Social class", axis.title = "") +
  scale_x_discrete(name = "",limits = c("1.Capitalists", "2.Small employers","3.Petite bourgeoisie","4.Expert managers", "5.Nonmanagerial experts", "6.Skilled supervisors", "7.Unskilled supervisors", "8.Skilled workers", "9.Unskilled workers", "10. Informal self-employed"),
                   labels = c("1.Employers", "2.Small\nemployers", "3.Petite\nbourgeoisie", "4.Expert\nmanagers", "5.Experts", "6.Skilled\nsupervisors", "7.Unskilled\nsupervisors", "8.Skilled\nworkers","9.Unskilled\nworkers", "10.Informal\nself-employed")) +
  scale_y_continuous(limits = c(0.0,0.8), labels = scales::percent_format()) +
  geom_line() +
  theme(axis.text.x = element_text(size = 6))

# Probabilidades predichas para identidad de clase según sindicalización
fig2union <- plot_model(m01,type = "pred",
                        terms = c("union"), 
                        title = "Union member status",
                        axis.title = "") + 
  scale_x_discrete(name = "",limits =c("No", "Si"),labels = c("Non-union member", "Union member"))  +
  scale_y_continuous(limits = c(0.1,0.8), labels = scales::percent_format()) + 
  geom_line()

fig2year <- plot_model(m01,type = "pred",
                       terms = c("year"), 
                       title = "Year", axis.title = "") +
  scale_x_discrete(name = "",limits = c("2009-01-01", "2019-01-01"),
                   labels = c( "2009", "2019")) + 
  geom_line() + scale_y_continuous(limits = c(0.1,0.8),
                                   labels = scales::percent_format())

layout <- rbind(c(1, 1),
                c(2, 3))

fig <- gridExtra::grid.arrange(fig2class, fig2union, fig2year, layout_matrix=layout)
fig <- gridExtra::grid.arrange(fig2class, fig2union)

### Guardar
ggsave(fig, filename = "output/images/figure3-en.png",
       device = "png",dpi = "retina", units = "cm",
       width = 27,height = 15)


# Figura 4 ----------------------------------------------------------------

dat <- ggpredict(m02, terms = c("class", "year"))
#plot(dat, colors = "bw", ci = T, connect.lines = TRUE)   + labs(title = "", x= "", y ="Identidad de clase (ref:9. Trabajadores no calificados)", fill = "")

ggplot(dat, aes(x, predicted, shape = group, color = group)) +
  geom_point(size = 3,position = position_dodge(.1))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(.1))  + 
  scale_shape_discrete(name = "",limits = c( "2009-01-01", "2019-01-01"),
                       labels = c( "2009", "2019")) +
  scale_x_discrete(name = "",
                   limits = c("1.Capitalists", "2.Small employers","3.Petite bourgeoisie","4.Expert managers", "5.Nonmanagerial experts", "6.Skilled supervisors", "7.Unskilled supervisors", "8.Skilled workers", "9.Unskilled workers", "10. Informal self-employed"),
                   labels = c("1.Employers", "2.Small employers","3.Petite bourgeoisie","4.Expert managers", "5.Experts", "6.Skilled supervisors", "7.Unskilled supervisors", "8.Skilled workers", "9.Unskilled workers", "10. Informal self-employed")) +
  scale_color_grey(name = "",limits = c("2009-01-01", "2019-01-01"),
                   labels = c( "2009", "2019")) +
  scale_y_continuous(limits = c(0.0,0.8),
                     labels = scales::percent_format()) +
  labs(y = "") + 
  theme(axis.text.x = element_text(angle = -90,vjust = 0.5, hjust=1)) 


ggsave(plot = last_plot(), filename = "output/images/figure4-en.png",
       device = "png",dpi = "retina", units = "cm",
       width = 27,height = 15)


# Figura 5 ----------------------------------------------------------------
dat <- ggpredict(m03, terms = c("union", "year"))
ggplot(dat, aes(x, predicted, shape = group, color =group)) +
  geom_point(size = 3,position = position_dodge(.1))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(.1))  + 
  scale_shape_discrete(name = "",
                       limits = c("2009-01-01", "2019-01-01"),
                       labels = c("2009", "2019")) +
  scale_x_discrete(name = "",limits =c("No", "Si"),labels = c("Non-union member", "Union member")) +
  scale_color_grey(name = "",limits = c("2009-01-01", "2019-01-01"),
                   labels = c("2009", "2019"))+ 
  scale_y_continuous(limits = c(0.0,0.8),
                                                                   labels = scales::percent_format())+
  labs(y = "")

ggsave(plot = last_plot(), filename = "output/images/figure5-en.png",
       device = "png",dpi = "retina", units = "cm",
       width = 27,height = 15)

