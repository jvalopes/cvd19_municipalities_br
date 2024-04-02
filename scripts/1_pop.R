
### Joao Victor Antunes Lopes ###

rm(list = ls())
dev.off()

# pacotes
library(tidyverse) # pipe etc
library(readxl) # read_xlsx

dir <- "C:/Users/User/Documents/GitHub/cvd19_municipalities_br/raw/ProjMunic-2010_2030.xlsx"

df <- read_excel(dir) # projecoes populacionais para pequenas areas, Freire et al (2019)

# construindo o df para a pop do municipio de sao paulo
# https://www.ibge.gov.br/explica/codigos-dos-municipios.php

# sao paulo (3550308), guarulhos (3518800) e campinas (3509502) 
# codigos de acordo com ibge. Por algum motivo eh necessario remover o ultimo digito...

df_pop <- df |> filter(Armenor %in% c("3550308", "3518800", "3509502") & Ano <= 2022)

length(unique(df_pop$Armenor)) 

# pivoteando
df_pop_ts <- df_pop |>
  pivot_longer(cols = starts_with("0 a 5"):ends_with("Total"),
               names_to = "GrupoEtario",
               values_to = "Populacao")

# removendo os totais
df_pop2 <- df_pop_ts |>
  filter(GrupoEtario != "Total")

df_pop2 <- df_pop2 |>
  select(Armenor, Sexo, Ano, GrupoEtario, Populacao)

names(df_pop2)

# sera util para piramide etaria
df_pop3 <- df_pop2 |>
  group_by(Ano, Armenor) |>
  mutate(populacao_total = sum(Populacao),
         proporcoes = Populacao / populacao_total * 100)

df_pop3 <- df_pop3 |>
  mutate(GrupoEtario = case_when(
    GrupoEtario == "0 a 5" ~ "0-5",
    GrupoEtario == "5 a 10" ~ "5-10",
    GrupoEtario == "10 a 15" ~ "10-15",
    GrupoEtario == "15 a 20" ~ "15-20",
    GrupoEtario == "20 a 25" ~ "20-25",
    GrupoEtario == "25 a 30" ~ "25-30",
    GrupoEtario == "30 a 35" ~ "30-35",
    GrupoEtario == "35 a 40" ~ "35-40",
    GrupoEtario == "40 a 45" ~ "40-45",
    GrupoEtario == "45 a 50" ~ "45-50",
    GrupoEtario == "50 a 55" ~ "50-55",
    GrupoEtario == "55 a 60" ~ "55-60",
    GrupoEtario == "60 a 65" ~ "60-65",
    GrupoEtario == "65 a 70" ~ "65-70",
    GrupoEtario == "70 a 75" ~ "70-75",
    GrupoEtario == "75 a 80" ~ "75-80",
    GrupoEtario == "80 a 85" ~ "80-85",
    GrupoEtario == "85 a 90" ~ "85-90",
    GrupoEtario == "90+" ~ "90+",
    TRUE ~ GrupoEtario))

ordem_grupos <- c("0-5", "5-10", "10-15", 
                  "15-20", "20-25", "25-30", 
                  "30-35", "35-40", "40-45", 
                  "45-50", "50-55", "55-60", 
                  "60-65", "65-70", "70-75", 
                  "75-80", "80-85", "85-90", "90+")

df_pop3$GrupoEtario <- factor(df_pop3$GrupoEtario,
                         levels = ordem_grupos,
                         ordered = TRUE)

df_pop3$Ano <- as.numeric(df_pop3$Ano)

df_pop3$GrupoEtario <- factor(df_pop3$GrupoEtario)

df_pop3$Sexo <- factor(df_pop3$Sexo)

df_pop3 <- df_pop3 |> mutate(Armenor = recode(Armenor,
                                              "3550308" = "São Paulo",
                                              "3518800" = "Guarulhos",
                                              "3509502" = "Campinas"))

ordem_mun <- c("São Paulo", "Guarulhos", "Campinas")

df_pop3$Armenor <- factor(df_pop3$Armenor, levels = ordem_mun)

# obtendo a populacao total
dft <- df_pop3 |> 
  group_by(Armenor, Sexo, Ano) |> 
  summarise(populacao_total = sum(Populacao))

# adiciona labels apenas em sp
df_first_facet <- subset(df_pop3, Armenor == unique(df_pop3$Armenor)[3])

pir_mun <- ggplot(df_pop3, aes(x = GrupoEtario, group = interaction(Sexo, Ano),
                         y = ifelse(test = Sexo == "m",
                                    yes = -proporcoes, no = proporcoes),
                         color = Ano)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = abs, 
                     limits = max(df_pop3$proporcoes) * c(-1, 1), expand = c(0, 0)) +
  labs(title = "", 
       x = "Grupos etários", 
       y = "Percentual da população") +
  labs(color = "") +
  coord_flip() +
  facet_wrap(~ Armenor) +
  geom_vline(xintercept = 4, 
             size = 0.5,
             color = 'grey') +
  geom_vline(xintercept = 13,
             size = 0.5,
             color = 'grey') +
  geom_hline(yintercept = 0, 
             size = 1, 
             color = 'grey') +
  scale_color_viridis_c(option = "H") +
  geom_text(data = df_first_facet, 
            aes(x = 17, 
                y = 2.75, 
                label = "Mulheres"),
            size = 4, 
            #fontface = "bold", 
            color = "black") + 
  geom_text(data = df_first_facet, 
            aes(x = 17, 
                y = -2.75, 
                label = "Homens"), 
            size = 4, 
            #fontface = "bold", 
            color = "black") +
  theme_gray(base_size = 14) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, 
                                 linetype = "solid"))

setwd("../imgs")

ggsave(filename = 'imgs/pop_sp2.pdf',
       plot = pir_mun,
       width = 12,
       height = 5)

# filtra as informacoes necessarias para as mxs
df_pop4 <- df_pop3[, 1:5]

# criando a categoria 80+
df_wide_pop <- pivot_wider(df_pop4, 
                           id_cols = c(Armenor, Sexo, Ano),
                           names_from = GrupoEtario,
                           values_from = Populacao)

df_wide_pop <- df_wide_pop %>%
  group_by(Armenor, Sexo, Ano) %>%
  mutate(`80+` = `80-85` + `85-90` + `90+`)

df_wide_pop <- df_wide_pop[, -c(20:22)]

# voltando para o formato longer
df_pop5 <- df_wide_pop %>%
  pivot_longer(cols = starts_with("0-5"):ends_with("80+"),
               names_to = "GrupoEtario",
               values_to = "Populacao")

# ajustando a forma que as idades aparecem no df para serem compativeis com a notacao da idade nos registros de obitos
df_pop5 <- df_pop5 |>
  mutate(GrupoEtario = case_when(
    GrupoEtario == "0-5" ~ "0-4",
    GrupoEtario == "5-10" ~ "5-9",
    GrupoEtario == "10-15" ~ "10-14",
    GrupoEtario == "15-20" ~ "15-19",
    GrupoEtario == "20-25" ~ "20-24",
    GrupoEtario == "25-30" ~ "25-29",
    GrupoEtario == "30-35" ~ "30-34",
    GrupoEtario == "35-40" ~ "35-39",
    GrupoEtario == "40-45" ~ "40-44",
    GrupoEtario == "45-50" ~ "45-49",
    GrupoEtario == "50-55" ~ "50-54",
    GrupoEtario == "55-60" ~ "55-59",
    GrupoEtario == "60-65" ~ "60-64",
    GrupoEtario == "65-70" ~ "65-69",
    GrupoEtario == "70-75" ~ "70-74",
    GrupoEtario == "75-80" ~ "75-79",
    GrupoEtario == "80+" ~ "80+",
    TRUE ~ GrupoEtario))

ordem_grupos <- c("0-4", "5-9", "10-14", 
                  "15-19", "20-24", "25-29", 
                  "30-34", "35-39", "40-44", 
                  "45-49", "50-54", "55-59", 
                  "60-64", "65-69", "70-74", 
                  "75-79", "80+")

df_pop5$GrupoEtario <- factor(df_pop5$GrupoEtario,
                              levels = ordem_grupos,
                              ordered = TRUE)

write.csv(df_pop5, "data/df_pop.csv", row.names = FALSE)

# fim populacao