#################################################################
##             Proyecto: Análisis de datos del IPS             ##
#################################################################
##
## Descripción:    En este script se analizan los resultados del
##                 IPS 2020.
##
## Autor:          Javier Mtz.  
##
## Fecha creac.:   2022-02-12
##
## Email:          javier.mtz.rd@gamil.com
##
## ---------------------------
## Notas:          
## ---------------------------

# Setup ----
## Paquetes a utilizar ----
pacman::p_load(tidyverse, janitor, writexl, readxl, scales,
               mytidyfunctions, viridis, ggrepel)

## Especificar locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Desabilitar notación científica.----
options(scipen = 999)

# Cargar datos ----
## IPS----
ips <- read_excel("01_datos_brutos/00_IPS_COMPLETE_WIDE.xlsx",
                  sheet = 1) %>% 
  full_join(read_excel("01_datos_brutos/00_IPS_COMPLETE_WIDE.xlsx",
                       sheet = 2),
            by = c("cve_ent", "anio", "entidad_abr_m")) %>% 
  pivot_longer(-c(cve_ent, anio, entidad_abr_m),
               names_to = "id",
               values_to = "values") %>% 
  mutate(tipo = case_when(id == "00" ~ "idx",
                          nchar(id) == 2 ~ "comp",
                          str_detect(id, "comp") ~ "subcomp")) %>% 
  mutate(id = str_remove_all(id, "_|comp"),
         id = factor(id, levels = c("00",
                                    "01", "0101", "0102", "0103", "0104",
                                    "02", "0205", "0206", "0207", "0208",
                                    "03", "0309", "0310", "0311", "0312"))) %>% 
  left_join(read_excel("01_datos_brutos/08_ips_ranking.xlsx") %>% 
              count(id, name) %>% 
              select(-n),
            by = "id") %>% 
  mutate(name = factor(name, 
                       levels = c("Índice de Progreso Social",
                                  "Necesidades Humanas Básicas",
                                  "Nutrición y cuidados médicos básicos",
                                  "Agua y saneamiento",
                                  "Vivienda",
                                  "Seguridad personal",
                                  "Fundamentos del Bienestar",
                                  "Acceso a conocimientos básicos",
                                  "Acceso a información y comunicaciones",
                                  "Salud y bienestar",
                                  "Calidad medioambiental",
                                  "Oportunidades",
                                  "Derechos personales",
                                  "Libertad personal y de elección",
                                  "Inclusión",
                                  "Acceso a educación superior"))) %>% 
  arrange(name)

## IPS detalle ----
sheets <- excel_sheets("01_datos_brutos/IPS_bd.xlsx")

ips_detalle <- map_df(sheets[3:length(sheets)],
       ~read_excel("01_datos_brutos/IPS_bd.xlsx",
                   guess_max = 100000,
                  sheet = .x) %>% 
         mutate(id_indicador = as.character(id_indicador),
                anio = as.numeric(anio),
                indicador_value = as.numeric(indicador_value),
                dato_name = .x) %>% 
         select(-starts_with("drop"))) %>% 
  full_join(read_excel("01_datos_brutos/IPS_bd.xlsx",
                       sheet = "00_directorio"),
            by = c("id_dimension", "id_indicador")) %>% 
  group_by(entidad_abr_m, dato_name) %>% 
  arrange(entidad_abr_m, dato_name, anio) %>% 
  mutate(indicador_value_var = indicador_value - lag(indicador_value),
         indicador_value_por = indicador_value_var/lag(indicador_value),
         indicador_value_comp = percent(indicador_value_por*direccion))


  

## Población ----

pob <- read_excel("01_datos_brutos/IPS_bd.xlsx",
                      sheet = "99_pobtot") %>% 
  select(anio, cve_ent, pob_tot)

## PIB ----

pib <- read_excel("01_datos_brutos/PIBE_2.xlsx",
                      range = "A121:S154") %>% 
  rename_at(., 
            vars(names(.)),
            function(x) c("entidad", "2003",	"2004",	"2005",	
                          "2006",	"2007",	"2008",	"2009",	"2010",	
                          "2011",	"2012",	"2013",	"2014",	"2015",	
                          "2016",	"2017",	"2018",	"2019",	"2020")) %>% 
  mutate(entidad_abr_m = mexicoR::entidad_to_abr(entidad),
         entidad_abr_m = recode(entidad_abr_m, 
                          "Estados Unidos Mexicanos" = "Nacional"),) %>% 
  select(-`2003`, -entidad) %>% 
  pivot_longer(-c(entidad_abr_m),
               names_to = "anio",
               values_to = "cambio_pib") %>% 
  left_join(read_excel("01_datos_brutos/PIBE_2.xlsx",
                       range = "A10:S43") %>% 
              rename_at(., 
                        vars(names(.)),
                        function(x) c("entidad", "2003",	"2004",	"2005",	
                                      "2006",	"2007",	"2008",	"2009",	"2010",	
                                      "2011",	"2012",	"2013",	"2014",	"2015",	
                                      "2016",	"2017",	"2018",	"2019",	"2020")) %>% 
              mutate(entidad_abr_m = mexicoR::entidad_to_abr(entidad),
                     entidad_abr_m = recode(entidad_abr_m, 
                                            "Estados Unidos Mexicanos" = "Nacional"),) %>% 
              select(-entidad) %>% 
              pivot_longer(-c(entidad_abr_m),
                           names_to = "anio",
                           values_to = "pib_2013"),
            by = c("entidad_abr_m", "anio")) %>% 
  left_join(read_excel("01_datos_brutos/PIBE_8.xlsx",
                       range = "A8:S41") %>% 
              rename_at(., 
                        vars(names(.)),
                        function(x) c("entidad", "2003",	"2004",	"2005",	
                                      "2006",	"2007",	"2008",	"2009",	"2010",	
                                      "2011",	"2012",	"2013",	"2014",	"2015",	
                                      "2016",	"2017",	"2018",	"2019",	"2020")) %>% 
              mutate(entidad_abr_m = mexicoR::entidad_to_abr(entidad),
                     entidad_abr_m = recode(entidad_abr_m, 
                                            "Estados Unidos Mexicanos" = "Nacional"),) %>% 
              select(-entidad) %>% 
              pivot_longer(-c(entidad_abr_m),
                           names_to = "anio",
                           values_to = "pib_mp_2013"),
            by = c("entidad_abr_m", "anio")) %>% 
  group_by(entidad_abr_m) %>% 
  mutate(pib_var = pib_2013 - lag(pib_2013),
         pib_2013_np = pib_2013 - pib_mp_2013,
         pib_np_var = pib_2013_np - lag(pib_2013_np),
         anio = as.numeric(anio),
         entidad = mexicoR::abr_to_entidad(entidad_abr_m),
         entidad = recode(entidad,
                          "CHPS" = "Chiapas")) %>% 
  ungroup() %>% 
  select(-entidad_abr_m) 

## cruce de informacion ----
ips_con <- ips %>% 
  mutate(entidad = mexicoR::abr_to_entidad(entidad_abr_m),
         entidad = recode(entidad,
                          "CHPS" = "Chiapas")) %>% 
  left_join(pob, 
            by = c("cve_ent", "anio")) %>% 
  left_join(pib, 
            by = c("entidad", "anio")) %>% 
  group_by(entidad_abr_m, name) %>% 
  arrange(entidad_abr_m, cve_ent, name, anio) %>% 
  mutate(pib_capita = (pib_2013/pob_tot)*1000000,
         pib_capita_var = pib_capita - lag(pib_capita),
         pib_np_capita = (pib_2013_np/pob_tot)*1000000,
         pib_np_capita_var = pib_np_capita - lag(pib_np_capita),
         pib_np_capita_por = pib_np_capita_var/lag(pib_np_capita),
         values_var = values - lag(values)) %>% 
  ungroup()

# Hatmap inicial de datos -----

ips %>% 
  filter(entidad_abr_m %in% c("OAX",
                              "GRO",
                              "CHPS",
                              "Nacional")) %>% 
  mutate(name = str_wrap(name,
                         22),
         values = as.numeric(values),
         entidad = case_when(entidad_abr_m == "OAX" ~ "Oaxaca",
                             entidad_abr_m == "GRO" ~ "Guerrero",
                             entidad_abr_m == "CHPS" ~ "Chiapas",
                             T ~ entidad_abr_m),
         entidad = factor(entidad, levels = c("Nacional",
                                              "Oaxaca", "Chiapas",
                                              "Guerrero"))) %>%
  ggplot(aes(x = fct_inorder(name), 
             y = anio, 
             fill = values)) + 
  geom_tile() +
  geom_tile(aes(size = tipo,
                linetype = tipo),
            color = "white",
            fill = "transparent") +
  geom_text(aes(label = round(values, 1)), 
            color = "white", 
            family = "EB Garamond",
            size = 2) +
  facet_wrap(~ entidad,
             ncol = 1) +
  scale_fill_gradient2(low = c("#9b2226",
                               "#ae2012",
                               # "#bb3e03",
                               "#e3a14b"),
                       mid = c("#ee9b00"),
                       high = c("#94d2bd",
                                "#0a9396",
                                "#005f73",
                                "#001219"),
                       breaks = seq(0, 100, 5),
                       midpoint = 50,
                       guide = guide_legend(
                         direction = "horizontal",
                         keyheight = unit(2, units = "mm"),
                         keywidth = unit(6/length(labels), units = "mm"),
                         title.position = 'left',
                         title.hjust = 0.5,
                         label.hjust = 0.5,
                         nrow = 1,
                         byrow = T,
                         reverse = T,
                         label.position = "bottom"
                       )) +
  scale_size_manual(values = c("idx" = 0.6,
                               "comp" = 0.3,
                               "subcomp" = 0.2),
                     guide = "none") +
  scale_linetype_manual(values = c("idx" = "solid",
                                   "comp" ="solid",
                                   "subcomp" ="dotted"),
                        guide = "none") +
  scale_x_discrete(#position="top",
    expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(breaks = 2015:2020,
                     expand = expansion(mult = c(0.005, 0.005))) +
  labs(title = "Índice de Progreso Social por componente \nde la región sureste", 
       # subtitle = "Miles de millones de pesos a precios de 2021",
       y = element_blank(),
       x = element_blank(),
       color = element_blank(),
       fill = element_blank(),
       caption = "Elaboración con datos México ¿cómo vamos? | @javiermtzrd") +
  mi_tema(text = element_text(family = "EB Garamond"),
          legend.position = "top",
          axis.line = element_line(size = 0),
          axis.text.x = element_text(angle = 90,
                                     face = c("bold", 
                                              "bold", "plain", "plain", 
                                              "plain", "plain",
                                              "bold", "plain", "plain", 
                                              "plain", "plain",
                                              "bold", "plain", "plain", 
                                              "plain", "plain"),
                                     color = c("black", 
                                               "grey30", "grey50", "grey50", 
                                               "grey50", "grey50",
                                               "grey30", "grey50", "grey50", 
                                               "grey50", "grey50",
                                               "grey30", "grey50", "grey50", 
                                               "grey50", "grey50"),
                                     size = c(10, 
                                              9, 8, 8, 8, 8, 
                                              9, 8, 8, 8, 8, 
                                              9, 8, 8, 8, 8),
                                     vjust = 0.5,
                                     hjust = 1))

ggsave_mult(format = c(".png", ".jpg", ".svg"),
            path_name = "02_graficas/comp-ips",
            width = 120,
            height = 200)


# Gráfica de cambio PIB vs IPS ----

comp1 <- ips_con %>% 
  filter(tipo %in% c("idx"),
         anio == 2020) %>% 
  mutate(color = case_when(entidad_abr_m == "Nacional" ~ "Nacional",
                           entidad_abr_m %in% c("OAX",
                                                "GRO",
                                                "CHPS") ~ "P.Entidades",
                           T ~ "Otras entidads"),
         entidad_abr_m = recode(entidad_abr_m,
                                "Nacional" = "NAC.")) %>% 
  ggplot(aes(x = pib_np_capita,
             y = values,
             color = color)) +
  geom_point() +
  scale_color_manual(values = c("Nacional" = "#001219",
                                "P.Entidades" = "#005f73",
                                "Otras entidads" = "#0a9396"),
                     guide = "none") +
  geom_smooth(color = "grey50",
              method = "lm", 
              formula = (y ~ log(x)),
              size = 0.5,
              linetype = "dashed",
              se = FALSE) +
  labs(y = "Índice de Progreso Social",
       x = "PIB per cápita (no petrolero)") +
  geom_text_repel(aes(label = entidad_abr_m),
                  color = "grey50",
                  segment.color = "grey40",
                  family = "EB Garamond",
                  segment.size = 0.25,
                  bg.color = "white", # shadow color
                  bg.r = 0.05,
                  size = 2.5) +
  scale_x_continuous(labels = comma) +
  mi_tema_html(text = element_text(family = "EB Garamond")) 

comp2 <- ips_con %>% 
  filter(tipo %in% c("idx"),
         anio == 2020) %>% 
  mutate(color = case_when(entidad_abr_m == "Nacional" ~ "Nacional",
                           entidad_abr_m %in% c("OAX",
                                                "GRO",
                                                "CHPS") ~ "P.Entidades",
                           T ~ "Otras entidads"),
         entidad_abr_m = recode(entidad_abr_m,
                                "Nacional" = "NAC.")) %>% 
  ggplot(aes(x = pib_np_capita_var,
             y = values_var,
             color = color)) +
  geom_point() +
  scale_color_manual(values = c("Nacional" = "#001219",
                                "P.Entidades" = "#005f73",
                                "Otras entidads" = "#0a9396"),
                     guide = "none") +
  geom_smooth(color = "grey50",
              method = "lm", 
              formula = (y ~ log(x)),
              size = 0.5,
              linetype = "dashed",
              se = FALSE) +
  labs(y = "Variación anual del Índice de Progreso Social",
       x = "Variación anual del PIB per cápita (no petrolero)") +
  geom_text_repel(aes(label = entidad_abr_m),
                  color = "grey50",
                  segment.color = "grey40",
                  segment.size = 0.25,
                  bg.color = "white", # shadow color
                  family = "EB Garamond",
                  bg.r = 0.05,
                  size = 2.5) +
  scale_x_continuous(labels = comma) +
  mi_tema_html(text = element_text(family = "EB Garamond")) 



pacman::p_load(patchwork)


(comp1 + comp2) +
  plot_annotation(title = 'Índice de Progreso Social vs. PIB per cápita (no petrolero), 2020',
                  caption  = str_replace_all(paste0("Elaboración con datos de México ¿cómo vamos? e INEGI | @javiermtzrd\nNota: ",
                                                    str_wrap("<sup>1/</sup>Se descuenta el PIB por actividades de minería petrolera.", 120),
                                                    "\n"),
                                             "\n", "<br>"),
                  theme = theme(axis.line = element_line(size = 0.3),
                                text = element_text(family = "EB Garamond"),
                                plot.title = ggtext::element_markdown(hjust = 0.5, 
                                                                      size = 14, face = "bold", 
                                                                      color = "grey20"),
                                plot.subtitle = ggtext::element_markdown(hjust = 0.5,
                                                                         size = 12,
                                                                         color = "gray50"),
                                plot.caption =  ggtext::element_markdown(color = "gray50",
                                                                         size = 9, 
                                                                         hjust = 0),
                                panel.grid = element_line(linetype = 2,
                                                          size = 0.3,
                                                          color = "gray90"),
                                # panel.grid = element_blank(),
                                panel.grid.minor = element_blank(),
                                strip.background = element_rect(fill="gray95", 
                                                                linetype="blank"),
                                panel.border = element_rect(color = "gray95",
                                                            fill=NA),
                                
                                rect = element_rect(fill = "transparent"))) 

ggsave_mult(format = c(".png", ".jpg", ".svg"),
            path_name = "02_graficas/ips-vs-pib-capita")

# Gráfica de PIB per cápita ----

ips_con %>% 
  filter(tipo %in% c("idx"),
         anio == 2020) %>% 
  mutate(color = case_when(entidad_abr_m == "Nacional" ~ "Nacional",
                           entidad_abr_m %in% c("OAX",
                                                "GRO",
                                                "CHPS") ~ "P.Entidades",
                           T ~ "Otras entidads")) %>% 
  ggplot(aes(x = reorder(entidad_abr_m,
                         pib_np_capita),
             y = pib_np_capita/1000)) +
  coord_flip() +
  geom_col(aes(fill = color),
           # fill = "#005f73",
           width = 0.8) +
  geom_text_bilateral(lab_position = pib_np_capita/1000,
                      lab_hor = paste0(dollar(pib_np_capita/1000,
                                              accuracy = 0.1),
                                       " (",
                                       ifelse(pib_np_capita == max(pib_np_capita),
                                              paste0(percent(pib_np_capita_por,
                                                      accuracy = 0.1),
                                                     " vs. 2019"),
                                              percent(pib_np_capita_por,
                                               accuracy = 0.1)),
                                       ")"),
                      percent_change = 0,
                      size = 3,
                      family = "EB Garamond",
                      fontface = "bold") +
  geom_col(data = ips_con %>% 
             filter(tipo %in% c("idx"),
                    anio == 2019),
           width = 0.8,
           size = 0.2,
           linetype = "dashed",
           color  = "grey30",
           fill = "transparent") +
  geom_hline(yintercept = 0) +
  annotate("text", x = 30.3, y = 243,
           colour = "grey40",
           family = "EB Garamond",
           label = "Nivel de 2020") +
  geom_curve(aes(x = 30.5, xend = 32.5, y = 270, yend = 307),
             curvature = 0.2,
             size = 0.15,
             arrow = arrow(length = unit(0.015, "npc")),
             colour = "grey60") +
  annotate("text", x = 21, y = 242,
           colour = "grey40",
           family = "EB Garamond",
           label = "Nivel de 2019") +
  geom_curve(aes(x = 21, xend = 18.5, 
                 y = 215, yend = 173),
             curvature = 0.2,
             size = 0.15,
             arrow = arrow(length = unit(0.015, "npc")),
             colour = "grey60") +
  labs(title = "PIB per cápita (no petrolero) por entidad, 2019-2020", 
       subtitle = "Miles de pesos a precios de 2013",
       y = element_blank(),
       x = element_blank(),
       color = element_blank(),
       fill = element_blank(),
       caption = str_replace_all(paste0("Elaboración con datos de INEGI | @javiermtzrd\nNota: ",
                                        str_wrap("<sup>1/</sup>Se descuenta el PIB por actividades de minería petrolera.", 120),
                                        "\n"),
                                 "\n", "<br>")) +
  scale_fill_manual(values = c("Nacional" = "#001219",
                               "P.Entidades" = "#005f73",
                               "Otras entidads" = "#0a9396"),
                    guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.005, 0.04))) +
  mi_tema_html(text = element_text(family = "EB Garamond")) 

ggsave_mult(format = c(".png", ".jpg", ".svg"),
            path_name = "02_graficas/pib-capita",
            width = 200,
            height = 180)


ips_con %>% 
  filter(tipo %in% c("idx"),
         anio == 2020) %>% 
  mutate(color = case_when(entidad_abr_m == "Nacional" ~ "Nacional",
                           entidad_abr_m %in% c("OAX",
                                                "GRO",
                                                "CHPS") ~ "P.Entidades",
                           T ~ "Otras entidads"),
         entidad_abr_m = recode(entidad_abr_m,
                                "Nacional" = "NAC."))

ips %>% 
  # filter(entidad_abr_m %in% c("OAX",
  #                             "GRO",
  #                             "CHPS",
  #                             "Nacional")) %>% 
  group_by(entidad_abr_m) %>% 
  mutate(value_var = values - lag(values)) %>% 
  filter(anio == 2020) %>% 
  arrange(value_var) %>% View

