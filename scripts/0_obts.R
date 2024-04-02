
### Joao Victor Antunes Lopes ###

rm(list = ls())
dev.off()

## Instalaçao do pacote microdatasus
## Fonte: https://github.com/rfsaldanha/microdatasus

# e necessario instalar o R tools previamente: https://cran.rstudio.com/bin/windows/Rtools/rtools43/rtools.html
# em seguida:
# install.packages("devtools")
# devtools::install_github("danicat/read.dbc")
# devtools::install_github("rfsaldanha/microdatasus")

# pacotes
library(microdatasus) # fetch_datasus, process_sim etc
library(tidyverse) # dplyr, ggplot
library(ggrepel) # personaliza legendas
library(data.table) # f read

# obitos por residencia, estado de SP
df <- microdatasus::fetch_datasus(year_start = 2010,
                                  month_start = 1,
                                  year_end = 2023,
                                  month_end = 12,
                                  uf = "SP",
                                  information_system = "SIM-DO")

# categoriza a partir do dicionario dos microdados
df2 <- microdatasus::process_sim(df)

# tres municipios mais populoso do estado de sp de acordo com o ultimo censo (2022)

# sao paulo (355030), guarulhos (3518800) e campinas (3509502) 
# codigos de acordo com ibge. Por algum motivo eh necessario remover o ultimo digito...

# filtra apenas os municipios de interesse,
df2_mun <- df2 |> filter(CODMUNRES == "355030" | CODMUNRES == "351880" | CODMUNRES == "350950")

# conta as categorias de CODMUNRES
# length(unique(df2_mun$CODMUNRES)) 

# classe 'date' para variavel data
df2_mun$DTOBITO <- as.Date(df2_mun$DTOBITO)

# extraindo o ano da data do obito
df2_mun$ANO_OBITO <- year(df2_mun$DTOBITO)

# agrupando as categorias de idade
df3 <- df2_mun |>
  group_by(ANO_OBITO,
           CODMUNRES,
           IDADEminutos, 
           IDADEhoras, 
           IDADEdias, 
           IDADEmeses, # todas categorias de idade
           IDADEanos, 
           SEXO, 
           CAUSABAS, 
           RACACOR) |>
  summarise(count = n()) |> # obitos
  as.data.frame()

# atribui 0 as categorias de idade minutos, horas ... meses
df4 <- df3 |>
  mutate(across(starts_with("IDADE"), as.numeric)) |>
  mutate(across(c(IDADEminutos, 
                  IDADEhoras, 
                  IDADEdias, 
                  IDADEmeses),
                ~if_else(!is.na(.), 0, .))) |>
  mutate(IDADE_agrupada = coalesce(IDADEanos,  # coalesce function: https://www.statology.org/dplyr-coalesce/
                                   IDADEmeses, 
                                   IDADEdias, 
                                   IDADEhoras, 
                                   IDADEminutos)) 

### codigo validado com os resultados do tabnet ###

# filtra as variaveis de interesse 
df5 <- df4 |>
  select(ANO_OBITO, # ano
         CODMUNRES, # municipio
         IDADE_agrupada, # idade apos tratamento
         CAUSABAS, # causa basica
         RACACOR, # raca cor
         SEXO,
         count) |> # total de obitos pela causa
  rename(
    ano = ANO_OBITO,
    mun = CODMUNRES,
    idade = IDADE_agrupada,
    causabas = CAUSABAS,
    racacor = RACACOR,
    sexo = SEXO,
    obitos = count)

# sumarisa
df5 <- df5 |>
  group_by(ano, mun, idade, causabas, sexo) |> 
  summarise(obitos = sum(obitos))

#df5 <- df5 |>
  #mutate(mun = recode(mun,
                     #"355030" = "sp",
                     #"351880" = "grlhs",
                     #"350950" = "cps"))

# avaliando nas
# obitos com idade ignorada
na_idade <- df5 |>
  filter(is.na(idade)) |>
  group_by(ano, mun) |> 
  summarise(obitos_ign = sum(obitos))

# obitos com sexo ignorado
na_sexo <- df5 |>
  filter(is.na(sexo)) |>
  group_by(ano, mun) |>
  summarise(sexo_ign = sum(obitos))

# apenas covid
causas <- c("B342")

# filtra as causas basicas inseridas no vetor causas
dfcovid <- df5 |> filter(causabas %in% causas)

sum(dfcovid$obitos)

# anos de pandemia para plotar apenas covid
dfcovid <- dfcovid |>
  filter(ano >= 2020)

# remove nas para idade e sexo (puco significativas nos municipios de interese)
dfcovid <- dfcovid |>
  filter(!is.na(sexo) & !is.na(idade))

any(is.na(dfcovid$obitos))

any(is.na(dfcovid$idade))

dfcovid <- dfcovid |> 
  mutate(mun = recode(mun,
                      "355030" = "São Paulo",
                      "351880" = "Guarulhos",
                      "350950" = "Campinas"))

ordem_mun <- c("São Paulo", "Guarulhos", "Campinas")

dfcovid$mun <- factor(dfcovid$mun, levels = ordem_mun)

# ggplot(subset(dfcovid, mun == "São Paulo") # para salvar apenas sp

covid <- ggplot(dfcovid,
                aes(x = idade,
                    y = obitos,
                    color = as.factor(ano))) +
  geom_point(size = 3, 
             alpha = .6) +
  labs(x = "Idades",
       y = "Óbitos",
       color = "Anos",
       shape = "Sexo") +
  facet_grid(mun ~ sexo, scales = "free_y") +
  scale_color_manual(values = c("darkblue", 
                                "darkred", 
                                "darkgreen")) + 
  theme_grey(base_size = 16) +
  theme(legend.position = "top",
        axis.line = element_line(colour = "black", 
                                       size = 1, 
                                       linetype = "solid"),
              plot.background = element_rect(fill = "white")) 

# fim covid

### causas selecionadas
# causas basicas selecionadas

causas <- c("U04", 
            "J09",
            "J10",
            "J11",
            "I20",
            "I23",
            "I24",
            "I25",
            "E10",
            "E11",
            "E12",
            "E13",
            "E14",
            "E66",
            "J45",
            "J46",
            "J40",
            "J41",
            "J42",
            "J43",
            "J44",
            "D65",
            "D66",
            "D67",
            "D68",
            "D69",
            "D70",
            "D71",
            "D72",
            "D73",
            "D74",
            "D75",
            "D76",
            "D77",
            "G10",
            "G11",
            "G12",
            "G13",
            "G21",
            "G22",
            "G23",
            "G24",
            "G25",
            "G26",
            "G31",
            "G32",
            "G36",
            "G37",
            "G46",
            "G47",
            "G60",
            "G61",
            "G62",
            "G63",
            "G64",
            "G70",
            "G71",
            "G72",
            "G73",
            "G90",
            "G91",
            "G92",
            "G93",
            "G94",
            "G95",
            "G96",
            "G97",
            "G98",
            "G99",
            "N17",
            "N18",
            "N19",
            "D80",
            "D81",
            "D82",
            "D83",
            "D84",
            "D86",
            "D89",
            "I80",
            "I81",
            "I82",
            "Outros")

# armazena de acordo com o padrao no vetor 'causas'
# nesse contexto e util porque ha subcategorias das causas basicas acima
# que devem ser selecionadas. Ou seja, a partir da função grepl, evita-se ter que 
# especificar todas as categorias

dft$causabas <- ifelse(dft$causabas %in% causas | grepl(paste(causas, 
                                                              collapse = "|"), 
                                                        dft$causabas), dft$causabas, "Outros")

mapeamento_causas <- c(
  "U04" = "(U04)",
  "J09|J10|J11" = "(J09-J11)",
  "I20|I23|I24|I25" = "(I20, I23-I25)",
  "E10|E11|E12|E13|E14" = "(E10-E14)",
  "E66" = "(E66)",
  "J45|J46" = "(J45-J46)",
  "J40|J41|J42|J43|J44" = "(J40-J44)",
  "G10|G11|G12|G13|G21|G22|G23|G24|G25|G26|G31|G32|G36|G37|G46|G47|G60|G61|G62|G63|G64|G65|G66|G67|G68|G69|G70|G71|G72|G73|G90|G91|G92|G93|G94|G95|G96|G97|G98|G99" = "(G10-G99)",
  "N17|N18|N19" = "(N17-N19)",
  "D80|D81|D82|D83|D84|D85|D86|D87|D88|D89" = "(D80-D89)",
  "D65|D66|D67|D68|D69|D70|D71|D72|D73|D74|D75|D76|D77" = "(D65-D77)",
  "I80|I81|I82" = "(I80-I82)",
  "Outros" = "(Outros)"
)

# adiciona mapeamento para cada prefixo de 3 caracteres
for (prefixo in substr(causas, 1, 3)) {
  mapeamento_causas[prefixo] <- paste0("(", prefixo, ")")
}

# cria funcao para mapear
mapear_causa <- function(codigo) {
  for (padrao in names(mapeamento_causas)) {
    if (any(str_detect(codigo, padrao))) {
      return(mapeamento_causas[padrao])
    }
  }
  return(codigo)
}

# aplica o mapeamento conjunto de dados
df_soma <- dft |>
  mutate(causabas = sapply(causabas, mapear_causa)) |>
  group_by(ano, mun, idade, sexo, causabas) |>
  summarise(obitos = sum(as.numeric(obitos)))

colSums(is.na(df_soma))

# esse df possui os obitos em idade simples
df_soma <- df_soma |> 
  mutate(mun = recode(mun,
                      "355030" = "São Paulo",
                      "351880" = "Guarulhos",
                      "350950" = "Campinas"))

# cria grupos quinquenais, se preferivel, e possivel ajusta-los para decenais etc
df_soma_grp <- df_soma |>
  mutate(idade_grp = case_when(
    idade >= 80 ~ "80+",
    TRUE ~ as.character(cut(idade, breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80),
                            labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79"),
                            include.lowest = TRUE, 
                            right = FALSE))
  ))


df_final <- df_soma_grp |>
  group_by(ano, mun, idade_grp, sexo, causabas) |> # opcionalmente, pode incluir racacor
  summarize(obitos = sum(obitos))

# total por causa
causa_tot <- df_final |>
  group_by(causabas, sexo, ano, mun) |>
  summarize(total_obitos = sum(obitos))

# validando os totais
sum(df_soma$obitos) # idade simples
sum(df_final$obitos) # apos agrupamento
sum(causa_tot$total_obitos) # total

ordem <- c(
  "(U04)", 
  "(J09-J11)",
  "(I20, I23-I25)",
  "(E10-E14)",
  "(E66)",
  "(J45-J46)",
  "(J40-J44)",
  "(G10-G99)",
  "(N17-N19)",
  "(D80-D89)",
  "(D65-D77)",
  "(I80-I82)",
  "(Outros)"
)

causa_tot$causabas <- factor(causa_tot$causabas, levels = ordem)

cores_manual <- c("Masculino" = "darkgreen", "Feminino" = "purple")
shapes_manual <- c("Masculino" = 1, "Feminino" = 19)

ordem_mun <- c("São Paulo", "Guarulhos", "Campinas")

causa_tot$mun <- factor(causa_tot$mun, levels = ordem_mun)

causa_tot <- na.omit(causa_tot) # pouco significativas nas regioes...

sum(causa_tot$total_obitos)

any(is.na(causa_tot$sexo))

# any(is.na(causa_tot$mun))

# any(is.na(causa_tot$total_obitos))

# any(is.na(causa_tot$causabas))

explr_causas2 <- ggplot(causa_tot) +
  aes(x = ano, 
      y = log(total_obitos), 
      colour = sexo) +
  #scale_color_manual(values = cores_manual) +
  #scale_shape_manual(values = shapes_manual) +
  geom_point(size = 1.5, alpha = .7, color = "black", stroke = .7) +
  #geom_line() +
  geom_smooth(colour = "darkred", fill = "darkred", se = T) +
  labs(x = "Anos", 
       y = "Total de óbitos, escala log", 
       colour = "", 
       shape = "") +
  theme_grey(base_size = 14) +
  geom_vline(xintercept = 2020, 
             linetype = "dotted", 
             color = "red", 
             size = 1) +
  facet_grid(mun ~ causabas) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.line = element_line(colour = "black", 
                                 size = 1, 
                                 linetype = "solid"),
        plot.background = element_rect(fill = "white")) +
  scale_x_continuous(breaks = seq(min(causa_tot$ano), 
                                  max(causa_tot$ano), 
                                  by = 5))


setwd("C:/Users/User/Documents/GitHub/cvd19_municipalities_br")

ggsave(filename = 'imgs/covid.pdf',
       plot = covid,
       width = 10,
       height = 8)

ggsave(filename = 'imgs/covid_sp.pdf',
       plot = covid_sp,
       width = 10,
       height = 8)

ggsave(filename = 'imgs/explr_causas.png',
       plot = explr_causas,
       width = 14,
       height = 8)

ggsave(filename = 'imgs/explr_causas2.png',
       plot = explr_causas2,
       width = 14,
       height = 8)

write.csv(df_soma, "data/df_obitos_s.csv", row.names = FALSE)

write.csv(df_final, "data/df_obitos_g.csv", row.names = FALSE)

write.csv(df5, "data/df_g.csv", row.names = FALSE)

sum(df_soma$obitos)
sum(df_final$obitos)

####### capitulos #######

df_c <- df5 |> 
  mutate(cid_grupos = case_when(
    substr(causabas, 1,1) == "I" ~ 'Doenças do aparelho circulatório',
    substr(causabas, 1,1) == "C" ~ 'Neoplasias',
    substr(causabas, 1,1) == "D" & as.numeric(substr(causabas,2,4) < 49) ~ 'Neoplasias',
    substr(causabas, 1,1) == "J" ~ 'Doenças do aparelho respiratório',
    substr(causabas, 1,1) == "R" ~ 'Sintomas, sinais e achados anormais de exames clínicos e de laboratório',
    substr(causabas, 1,1) == "E" ~ 'Doenças endócrinas, nutricionais e metabólicas',
    substr(causabas, 1,1) == "K" ~ 'Doenças do aparelho digestivo',
    substr(causabas, 1,1) == "N" ~ 'Doenças do aparelho geniturinário',
    substr(causabas, 1,1) == "G" ~ 'Doenças do sistema nervoso',
    substr(causabas, 1,1) == "V" ~ 'Causas externas de morbidade e de mortalidade',
    substr(causabas, 1,1) == "W" ~ 'Causas externas de morbidade e de mortalidade',
    substr(causabas, 1,1) == "X" ~ 'Causas externas de morbidade e de mortalidade',
    substr(causabas, 1,1) == "Y" ~ 'Causas externas de morbidade e de mortalidade',
    substr(causabas, 1,1) == "A" ~ 'Algumas doenças infecciosas e parasitárias',
    substr(causabas, 1,1) == "B" ~ 'Algumas doenças infecciosas e parasitárias',
    TRUE ~ 'Outros'))

# criando um df com as cids agrupadas
df_c2 <- select(df_c, ano, mun, idade, sexo, obitos, cid_grupos)

df_c3 <- df_c2 |>
  mutate(idade_grp = case_when(
    idade >= 80 ~ "80+",
    TRUE ~ as.character(cut(idade, breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80),
                            labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79"),
                            include.lowest = TRUE, 
                            right = FALSE))
  ))

df_c4 <- df_c3 |>
  group_by(cid_grupos, idade_grp, sexo, mun, ano) |>
  summarise(obitos = sum(obitos))

ordem <- c("Doenças do aparelho circulatório",
           "Neoplasias",
           "Doenças do aparelho respiratório",
           "Sintomas, sinais e achados anormais de exames clínicos e de laboratório",
           "Doenças endócrinas, nutricionais e metabólicas",
           "Doenças do aparelho digestivo",
           "Doenças do aparelho geniturinário",
           "Doenças do sistema nervoso",
           "Causas externas de morbidade e de mortalidade",
           "Algumas doenças infecciosas e parasitárias",
           "Outros")

df_c4$cid_grupos <- factor(df_c4$cid_grupos, levels = ordem)

df_c4 <- df_c4 |> 
  mutate(mun = recode(mun,
                      "355030" = "São Paulo",
                      "351880" = "Guarulhos",
                      "350950" = "Campinas"))

na_anos <- df_c4 |>
  filter(is.na(idade_grp)) |>
  group_by(ano, mun) |> 
  summarise(obitos_ign = sum(obitos))

na_sexo <- df_c4 |>
  filter(is.na(sexo)) |>
  group_by(ano, mun) |>
  summarise(sexo_ign = sum(obitos))

df_c5 <- df_c4 |> 
  filter(idade_grp %in% c("60-64",
                          "65-69", 
                          "70-74",
                          "75-79", 
                          "80+")) |> 
  group_by(idade_grp, mun, ano) |> 
  mutate(prop = obitos / sum(obitos))


df_c6 <- na.omit(df_c5)

# looping para plotar e salvar a motalidade proporconal para cada municipio
setwd("C:/Users/User/Documents/GitHub/cvd19_municipalities_br/imgs")

munv <- unique(df_c6$mun)

sgm <- function(i) {
  df_mun <- subset(df_c6, mun == munv[i])

p <- ggplot(df_mun,
               aes(
                 x = idade_grp, 
                 y = prop, 
                 fill = cid_grupos)) +
  geom_col(position = "fill") +
  scale_fill_brewer(palette = "Spectral") +
  labs(x = "Grupos etários",
       y = "Óbitos, %",
       fill = "Capítulos\n(CID-10)",
       title = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = "bottom",
        axis.line = element_line(colour = "black", 
                                 size = 0.7, 
                                 linetype = "solid"),
        panel.spacing.y = unit(1, "lines")) +
  scale_y_continuous(breaks = seq(0, 1, .1),
                     name = "Óbitos, %",
                     sec.axis = dup_axis(name = ""),
                     expand = c(0, 0),
                     labels = scales::percent_format(scale = 100)) +
  scale_x_discrete(expand = c(0, 0)) + 
  facet_grid(sexo ~ ano) +
  #coord_flip() +
  guides(fill = guide_legend
         (ncol = 2, bycol = TRUE)) 


ggsave(filename = paste0(munv[i], ".png"), 
       plot = p, 
       width = 10, 
       height = 6,)
}


for (i in seq_along(munv)) {
  sgm(i)
}  

# fim capítulos