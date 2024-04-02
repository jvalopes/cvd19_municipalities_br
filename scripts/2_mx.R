### Joao Victor Antunes Lopes ###

rm(list = ls())
dev.off()

# pacotes
library(tidyverse) # dyplr, ggplot2 etc
library(tidyrates) # rate_adj_direct
library(readr) # # read_csv
library(ggh4x) # facet_nested

# diretorio 
setwd("C:/Users/User/Desktop/covid_mx/data")

# obitos
dfo <- read_csv("df_obitos_g.csv")

sum(dfo$obitos)

anyNA(dfo$obitos)

anyNA(dfo$sexo)

sum(is.na(dfo$sexo))


cb <- expand.grid(
  ano = 2010:2022,
  mun = c("São Paulo", "Guarulhos", "Campinas"),
  idade_grp = unique(dfo$idade_grp),
  sexo = c("Feminino", "Masculino"),
  causabas = c(
    "(U04)", "(J09-J11)", "(I20, I23-I25)", "(E10-E14)", "(E66)",
    "(J45-J46)", "(J40-J44)", "(G10-G99)", "(N17-N19)", "(D80-D89)",
    "(D65-D77)", "(I80-I82)", "(Outros)"
  )
)

dfo_cb <- cb |> 
  left_join(dfo, by = c("ano", "mun", "idade_grp", "sexo", "causabas")) %>%
  mutate(obitos = ifelse(is.na(obitos), 0, obitos))

sum(dfo_cb$obitos)

sum(dfo$obitos)

sum(dfo$obitos[is.na(dfo$sexo)]) + sum(dfo_cb$obitos) # valida se a diferenca eh no sexo ignorado

# pop
dfp <- read.csv("df_pop.csv")

names(dfp) <- c("mun", "sexo", "ano", "idade_grp", "populacao")

dfp$sexo <- ifelse(dfp$sexo == "m", "Masculino", "Feminino")

# right join
dfo2 <- right_join(dfo_cb, 
                dfp,
                by = c("mun",
                       "sexo", 
                       "ano",
                       "idade_grp")) |>
  pivot_longer(cols = c("obitos", 
                        "populacao"))

# populacao padrao, estado de sp 2010 (censo)
setwd("C:/Users/User/Desktop/covid_mx/raw")

### sao paulo
df <- read_delim("saopaulo_pop_2010_f_m.csv",
                 delim = ";", 
                 escape_double = FALSE, 
                 col_names = FALSE,
                 trim_ws = TRUE, skip = 4)

df <- df[-c(34:nrow(df)), ]

colnames(df) <- c('idade', 'masc', 'fem', 'total')

df <- df |> 
  mutate(idade = recode(idade,
                        "Menor 1 ano" = "<1",
                        "1 ano" = "1",
                        "2 anos" = "2",
                        "3 anos" = "3",
                        "4 anos" = "4",
                        "5 anos" = "5",
                        "6 anos" = "6",
                        "7 anos" = "7",
                        "8 anos" = "8",
                        "9 anos" = "9",
                        "10 anos" = "10",
                        "11 anos" = "11",
                        "12 anos" = "12",
                        "13 anos" = "13",
                        "14 anos" = "14",
                        "15 anos" = "15",
                        "16 anos" = "16",
                        "17 anos" = "17",
                        "18 anos" = "18",
                        "19 anos" = "19",
                        "20 a 24 anos" = "20-24",
                        "25 a 29 anos" = "25-29",
                        "30 a 34 anos" = "30-34",
                        "35 a 39 anos" = "35-39",
                        "40 a 44 anos" = "40-44",
                        "45 a 49 anos" = "45-49",
                        "50 a 54 anos" = "50-54",
                        "55 a 59 anos" = "55-59",
                        "60 a 64 anos" = "60-64",
                        "65 a 69 anos" = "65-69",
                        "70 a 74 anos" = "70-74",
                        "75 a 79 anos" = "75-79",
                        "80 anos e mais" = "80+"))

df_t <- as.data.frame(t(df))

colnames(df_t) <- df_t[1, ]

df_t <- df_t[-1, ]

# convertendo para numerico
df_t <- df_t |>
  mutate_all(as.numeric)

# por algum motivo aqui e necessario manter o pipe antigo 
# agrupamentos
df_t <- df_t %>%
  mutate('0-4' = rowSums(select(., 1:5)))

df_t <- df_t %>%
  mutate('5-9' = rowSums(select(., 6:10)))

df_t <- df_t %>% 
  mutate('10-14' = rowSums(select(., 11:15)))

df_t <- df_t %>% 
  mutate('15-19' = rowSums(select(., 16:20)))

# removendo as idades simples
df_t <- df_t[ ,-c(1:20)]

# criando ordem
col_order <- c("0-4", "5-9", "10-14",
               "15-19", "20-24", "25-29", "30-34", 
               "35-39", "40-44", "45-49", "50-54", 
               "55-59", "60-64", "65-69", "70-74", 
               "75-79", "80+")

# reordenar as colunas
df_t <- df_t |> 
  select(all_of(col_order))

df2 <- as.data.frame(t(df_t)) # transpondo novamente

# ajustando o nome das colunas
df_pop_10 <- df2 |> 
  rownames_to_column(var = "idade_grp")

# populacao referencia feminina
pop_reff <- df_pop_10[, -c(2, 4)] |> as.tibble()

# populacao referencia masculina
pop_refm <- df_pop_10[, -c(3:4)] |> as.tibble()

# criando um df com cada causa, para obter uma taxa bruta para cada causa e sexo
ordem <- c(
  "(U04)", #1
  "(J09-J11)" , #2
  "(I20, I23-I25)", #3
  "(E10-E14)", #4
  "(E66)", #5
  "(J45-J46)", #6
  "(J40-J44)", #7
  "(G10-G99)", #8
  "(N17-N19)", #9
  "(D80-D89)", #10
  "(D65-D77)", #11
  "(I80-I82)", #12
  "(Outros)" #13
)

### mulheres ###

# nota: inserir isso num looping depois e otimizar o codigo

# rates1 (zero)
df_o1 <- dfo2 |>
  filter(causabas == "(U04)", sexo == "Feminino")

rates1 <- tidyrates::rate_adj_direct(
  .data = df_o1, 
  .std = pop_reff, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "fem",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates1$cid <- "(U04)"
rates1$sexo <- "Feminino"

# rates 2
df_o2 <- dfo2 |>
  filter(causabas == "(J09-J11)", sexo == "Feminino")

rates2 <- tidyrates::rate_adj_direct(
  .data = df_o2, 
  .std = pop_reff, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "fem",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates2$cid <- "(J09-J11)"
rates2$sexo <- "Feminino"

# rates 3
df_o3 <- dfo2 |>
  filter(causabas == "(I20, I23-I25)", sexo == "Feminino")

rates3 <- tidyrates::rate_adj_direct(
  .data = df_o3, 
  .std = pop_reff, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "fem",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates3$cid <- "(I20, I23-I25)"
rates3$sexo <- "Feminino"

# rates 4
df_o4 <- dfo2 |>
  filter(causabas == "(E10-E14)", sexo == "Feminino")

rates4 <- tidyrates::rate_adj_direct(
  .data = df_o4, 
  .std = pop_reff, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "fem",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates4$cid <- "(E10-E14)"
rates4$sexo <- "Feminino"

# rates5
df_o5 <- dfo2 |>
  filter(causabas == "(E66)", sexo == "Feminino")

rates5 <- tidyrates::rate_adj_direct(
  .data = df_o5, 
  .std = pop_reff, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "fem",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates5$cid <- "(E66)"
rates5$sexo <- "Feminino"

# rates 6
df_o6 <- dfo2 |>
  filter(causabas == "(J45-J46)" , sexo == "Feminino")

rates6 <- tidyrates::rate_adj_direct(
  .data = df_o6, 
  .std = pop_reff, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "fem",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates6$cid <- "(J45-J46)"
rates6$sexo <- "Feminino"

# rates 7
df_o7 <- dfo2 |>
  filter(causabas == "(J40-J44)", sexo == "Feminino")

rates7 <- tidyrates::rate_adj_direct(
  .data = df_o7, 
  .std = pop_reff, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "fem",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates7$cid <- "(J40-J44)"
rates7$sexo <- "Feminino"

# rates 8
df_o8 <- dfo2 |>
  filter(causabas == "(G10-G99)", sexo == "Feminino")

rates8 <- tidyrates::rate_adj_direct(
  .data = df_o8, 
  .std = pop_reff, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "fem",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates8$cid <- "(G10-G99)"
rates8$sexo <- "Feminino"

# rates 9
df_o9 <- dfo2 |>
  filter(causabas == "(N17-N19)", sexo == "Feminino")

rates9 <- tidyrates::rate_adj_direct(
  .data = df_o9, 
  .std = pop_reff, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "fem",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates9$cid <- "(N17-N19)"
rates9$sexo <- "Feminino"

# rates 10
df_o10 <- dfo2 |>
  filter(causabas == "(D80-D89)", sexo == "Feminino")

rates10 <- tidyrates::rate_adj_direct(
  .data = df_o10, 
  .std = pop_reff, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "fem",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates10$cid <- "(D80-D89)"
rates10$sexo <- "Feminino"

# rates 11
df_o11 <- dfo2 |>
  filter(causabas == "(D65-D77)", sexo == "Feminino")

rates11 <- tidyrates::rate_adj_direct(
  .data = df_o11, 
  .std = pop_reff, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "fem",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates11$cid <- "(D65-D77)"
rates11$sexo <- "Feminino"

# rates 12
df_o12 <- dfo2 |>
  filter(causabas == "(I80-I82)", sexo == "Feminino")

rates12 <- tidyrates::rate_adj_direct(
  .data = df_o12, 
  .std = pop_reff, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "fem",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates12$cid <- "(I80-I82)"
rates12$sexo <- "Feminino"

# rates 13
df_o13 <- dfo2 |>
  filter(causabas == "(Outros)", sexo == "Feminino")

rates13 <- tidyrates::rate_adj_direct(
  .data = df_o13, 
  .std = pop_reff, 
  .keys = c("mun", "ano"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "fem",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates13$cid <- "(Outros)"
rates13$sexo <- "Feminino"

ratesf <- rbind(rates1, rates2, rates3, rates4, rates5, rates6, rates7, rates8,
                rates9, rates10, rates11, rates12, rates13)

### homens ###
# rates1 (zero)
df_o1 <- dfo2 |>
  filter(causabas == "(U04)", sexo == "Masculino")

rates1 <- tidyrates::rate_adj_direct(
  .data = df_o1, 
  .std = pop_refm, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "masc",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates1$cid <- "(U04)"
rates1$sexo <- "Masculino"

# rates 2
df_o2 <- dfo2 |>
  filter(causabas == "(J09-J11)", sexo == "Masculino")

rates2 <- tidyrates::rate_adj_direct(
  .data = df_o2, 
  .std = pop_refm, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "masc",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates2$cid <- "(J09-J11)"
rates2$sexo <- "Masculino"

# rates 3
df_o3 <- dfo2 |>
  filter(causabas == "(I20, I23-I25)", sexo == "Masculino")

rates3 <- tidyrates::rate_adj_direct(
  .data = df_o3, 
  .std = pop_refm, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "masc",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates3$cid <- "(I20, I23-I25)"
rates3$sexo <- "Masculino"

# rates 4
df_o4 <- dfo2 |>
  filter(causabas == "(E10-E14)", sexo == "Masculino")

rates4 <- tidyrates::rate_adj_direct(
  .data = df_o4, 
  .std = pop_refm, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "masc",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates4$cid <- "(E10-E14)"
rates4$sexo <- "Masculino"

# rates5
df_o5 <- dfo2 |>
  filter(causabas == "(E66)", sexo == "Masculino")

rates5 <- tidyrates::rate_adj_direct(
  .data = df_o5, 
  .std = pop_refm, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "masc",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates5$cid <- "(E66)"
rates5$sexo <- "Masculino"

# rates 6
df_o6 <- dfo2 |>
  filter(causabas == "(J45-J46)" , sexo == "Masculino")

rates6 <- tidyrates::rate_adj_direct(
  .data = df_o6, 
  .std = pop_refm, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "masc",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates6$cid <- "(J45-J46)"
rates6$sexo <- "Masculino"

# rates 7
df_o7 <- dfo2 |>
  filter(causabas == "(J40-J44)", sexo == "Masculino")

rates7 <- tidyrates::rate_adj_direct(
  .data = df_o7, 
  .std = pop_refm, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "masc",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates7$cid <- "(J40-J44)"
rates7$sexo <- "Masculino"

# rates 8
df_o8 <- dfo2 |>
  filter(causabas == "(G10-G99)", sexo == "Masculino")

rates8 <- tidyrates::rate_adj_direct(
  .data = df_o8, 
  .std = pop_refm, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "masc",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates8$cid <- "(G10-G99)"
rates8$sexo <- "Masculino"

# rates 9
df_o9 <- dfo2 |>
  filter(causabas == "(N17-N19)", sexo == "Masculino")

rates9 <- tidyrates::rate_adj_direct(
  .data = df_o9, 
  .std = pop_refm, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "masc",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates9$cid <- "(N17-N19)"
rates9$sexo <- "Masculino"

# rates 10
df_o10 <- dfo2 |>
  filter(causabas == "(D80-D89)", sexo == "Masculino")

rates10 <- tidyrates::rate_adj_direct(
  .data = df_o10, 
  .std = pop_refm, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "masc",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates10$cid <- "(D80-D89)"
rates10$sexo <- "Masculino"

# rates 11
df_o11 <- dfo2 |>
  filter(causabas == "(D65-D77)", sexo == "Masculino")

rates11 <- tidyrates::rate_adj_direct(
  .data = df_o11, 
  .std = pop_refm, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "masc",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates11$cid <- "(D65-D77)"
rates11$sexo <- "Masculino"

# rates 12
df_o12 <- dfo2 |>
  filter(causabas == "(I80-I82)", sexo == "Masculino")

rates12 <- tidyrates::rate_adj_direct(
  .data = df_o12, 
  .std = pop_refm, 
  .keys = c("ano", "mun"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "masc",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates12$cid <- "(I80-I82)"
rates12$sexo <- "Masculino"

# rates 13
df_o13 <- dfo2 |>
  filter(causabas == "(Outros)", sexo == "Masculino")

rates13 <- tidyrates::rate_adj_direct(
  .data = df_o13, 
  .std = pop_refm, 
  .keys = c("mun", "ano"),
  .age_group_var = "idade_grp",
  .age_group_pop_var = "masc",
  .events_label = "obitos",
  .population_label = "populacao",
  .progress = T
) %>%
  mutate(
    crude.rate = crude.rate * 100000,
    adj.rate = adj.rate * 100000,
    lci = lci * 100000,
    uci = uci * 100000,
  )

rates13$cid <- "(Outros)"
rates13$sexo <- "Masculino"


ratesm <- rbind(rates1, rates2, rates3, rates4, rates5, rates6, rates7, rates8,
                rates9, rates10, rates11, rates12, rates13)

ratest <- rbind(ratesf, ratesm)

ratest <- ratest |>
  filter(cid != "(U04)")

ordem <- c(
 # "(U04)", #1
  "(J09-J11)" , #2
  "(I20, I23-I25)", #3
  "(E10-E14)", #4
  "(E66)", #5
  "(J45-J46)", #6
  "(J40-J44)", #7
  "(G10-G99)", #8
  "(N17-N19)", #9
  "(D80-D89)", #10
  "(D65-D77)", #11
  "(I80-I82)", #12
  "(Outros)" #13
)

ratest$cid <- factor(ratest$cid, levels = ordem)

logtbmp <- ggplot(ratest) +
  aes(x = ano, 
      y = log(adj.rate), 
      color = sexo) +
  geom_point(size = 2, show.legend = FALSE) + 
  facet_nested(mun + sexo ~ cid) +
  scale_x_continuous(breaks = seq(min(ratest$ano), 
                                  max(ratest$ano), 
                                  by = 5)) +
  scale_color_manual(values = c("black", "darkred")) +
  labs(x = "", y = expression(logTBM[p])) +
  annotate("rect", 
           xmin = 2020 - .5, xmax = 2022 + .5, 
           ymin = -Inf, ymax = Inf, 
           fill = "red", 
           alpha = 0.25) +
  theme_grey(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        axis.line = element_line(colour = "black", 
                                 size = 1, 
                                 linetype = "solid"),
        plot.background = element_rect(fill = "white"))



# looping para plotar os graficos separadamente para as causas
for (i in seq_along(ordem)) {
  nome_causa <- ordem[i]
  
  dados_causa <- subset(ratestf, cid == nome_causa)
  
  grafico <- ggplot(dados_causa) +
    aes(x = ano, 
        y = adj.rate, 
        color = sexo) +
    geom_point(size = 1.5) + 
    geom_errorbar(aes(ymin = adj.rate - lci, 
                      ymax = adj.rate + uci), 
                  width = 0, 
                  size = 1,
                  alpha = 1) +
    annotate("rect", 
             xmin = 2020 - .5, xmax = 2022 + .5, 
             ymin = -Inf, ymax = Inf, 
             fill = "red", 
             alpha = 0.25) +
    facet_nested(cid ~ mun + sexo) +
    labs(x = "", y = expression(TBM[p])) +
    scale_x_continuous(breaks = seq(min(ratest$ano), 
                                    max(ratest$ano), 
                                    by = 2)) +
    #scale_y_continuous(breaks = seq(min(ratest$adj.rate), max(ratest$adj.rate), by = 1)) +
    theme_grey(base_size = 14) +
    guides(color = "none") +
    scale_color_manual(values = c("black", "darkred")) +
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 90, vjust = .5),
          axis.line = element_line(colour = "black", 
                                   size = 1, 
                                   linetype = "solid"),
          plot.background = element_rect(fill = "white"))
  
  ggsave(paste0("fig_", i, ".png"), plot = grafico)
}

# fim graficos