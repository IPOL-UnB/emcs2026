# ==============================================================================
# Script de Apoio — Curso de R para Ciência Política
# Escola de Métodos em Ciências Sociais 2026
# Frederico Bertholini — IPOL/UnB
# ==============================================================================
# Este script acompanha os slides do curso.
# Abra-o no RStudio e vá rodando linha a linha com Ctrl+Enter (Cmd+Enter no Mac).
# ==============================================================================
a

# AULA 1 — Conhecendo o R ===========

# --- R como calculadora ---

5 + 5 
5 - 3
4 * 9
16 / 2

(5 + 6) * 3
5 + 6 * 3

2 ^ 2
sqrt(36)

# --- Operadores lógicos ---

5 == 5
5 <= 5 / 5
5 * 4 > 5
3 != 6

TRUE == TRUE
TRUE <= FALSE

"Python" == "python"
"Stata" != "Sasta"

# --- Operadores & (E) e | (OU) ---

(3 == 3) & (4 != 5)
(3 != 3) | (4 != 5)

# --- Criação de objetos ---
sorte = 5
sorte <- 5
8 -> azar
sorte + 2

class(sorte)

# --- Numérico ---
# Separador de decimais = ponto
decimal <- 3.5

# --- Lógico ---

vdd <- TRUE
class(vdd)
T + F

# --- Caracteres e fatores ---

nome <- "Frederico"
class(nome)

partido <- factor(c("PT", "PL", "MDB", "PT", "PL"))
partido <- c("PT", "PL", "MDB", "PT", "PL")
partido_fator <- factor(partido)
levels(partido)

# --- Vetores ---

c(2, 4, 6, 8)
c("Pedro", "Paula", "Pietro", "Paloma")
c(TRUE, FALSE, TRUE, FALSE)

n.pares <- c(2, 4, 6, 8)
nomes.com.p <- c("Pedro", "Paula", "Pietro", "Paloma")
valores.log <- c(TRUE, FALSE, TRUE, FALSE)

class(n.pares)
class(nomes.com.p)
length(n.pares)

# --- Somatório ---

sum(n.pares)
sum(valores.log)
sum(nomes.com.p == "Pedro")

# --- Seleção de elementos ---

nomes.com.p[2]
nomes.com.p[nomes.com.p == "Paula"]
n.pares[n.pares > median(n.pares)]

"Paula" %in% nomes.com.p

n.pares[n.pares >= 5] * 2

# --- data.frame ---

# Carrega o ecossistema tidyverse
pacman::p_load(tidyverse)

# Cria o conjunto de dados com a sintaxe corrigida
df <- tibble(
  nome = c("SP", "RJ", "MG"),
  populacao = c(46, 17, 21),
  partido = factor(c("REP", "PL", "NOVO")),
  regiao = c("Sudeste", "Sudeste", "Sudeste")
)
# rm(df)

min(df$populacao)
"DF" %in% df$nome

dim(df)
str(df)

# --- Pacotes ---

# install.packages("pacman")   # instala
# library(foreign)               # ativa

# Nossa abordagem:
pacman::p_load(tidyverse)

# --- Pipe |> ---

x <- c(1, 2, 3, 4)

# Sem pipe:
sqrt(sum(x))

# Com pipe:
x |> 
  sum() |> 
  sqrt()


# AULA 2 — Conhecendo seus Dados ============

# --- Setup ---

pacman::p_load(
  tidyverse, scales,
  sf, geobr,
  patchwork, leaflet,
  broom, modelsummary,
  jsonlite, httr2,
  janitor, readxl, haven,
  electionsBR, ipeadatar
)

# --- Diretório de trabalho ---

getwd()
# setwd("~/Documents/emcs2026")  # evite! Use RProjects

# --- Importação: nosso dataset ---

municipios <- read_csv("dados/municipios_br.csv")

# --- Explorando ---

glimpse(municipios)
dim(municipios)
names(municipios)
n_distinct(municipios$uf)
summary(municipios$idhm)

# head(municipios)
# tail(municipios)
# View(municipios)

# --- Outros formatos ---

# dados_excel <- read_xlsx("dados/planilha.xlsx", sheet = 2)
# dados_txt <- read_delim("dados/base.txt", delim = "\t")
# dados_stata <- read_dta("dados/base.dta")
# dados_rds <- read_rds("dados/base.rds")

# --- Salvando dados ---

# write_csv(municipios, "dados/municipios_processados.csv")
# write_rds(municipios, "dados/municipios_processados.rds")

# --- electionsBR ---

# candidatos_df <- candidate_fed(year = 2022, uf = "DF")
# glimpse(candidatos_df)

# --- ipeadatar ---

# search_series(terms = "IDHM", fields = "name")
# idhm <- ipeadata("ADH_IDHM")
# idhm_mun <- idhm |>
#   filter(uname == "Municipality") |>
#   filter(date == max(date))

# --- API da Câmara dos Deputados ---

# url <- "https://dadosabertos.camara.leg.br/api/v2/votacoes"
# resp <- request(url) |>
#   req_url_query(dataInicio = "2024-08-01", dataFim = "2024-08-31") |>
#   req_perform()
# votacoes <- resp |> resp_body_json() |> pluck("dados")


# AULA 3 — Preparando e Manipulando ===========

# --- select ---

municipios |>
  select(municipio, uf, regiao, idhm)

municipios |>
  select(starts_with("perc"))

municipios |>
  select(-populacao, -gini)

# --- filter ---

municipios |>
  filter(regiao == "Nordeste", idhm > 0.7)

municipios |>
  filter(uf %in% c("SP", "RJ", "MG")) |>
  select(municipio, uf, perc_votos_gov) |>
  head(8)

# --- mutate ---

municipios |>
  mutate(
    margem_vitoria = abs(perc_votos_gov - 50),
    competitivo = margem_vitoria < 10,
    faixa_idh = case_when(
      idhm < 0.55 ~ "Muito Baixo",
      idhm < 0.70 ~ "Baixo",
      idhm < 0.80 ~ "Médio",
      TRUE ~ "Alto"
    )
  ) |>
  select(municipio, margem_vitoria, competitivo, faixa_idh) |>
  head(6)

municipios |>
  mutate(
    log_pop = log(populacao),
    idhm_padronizado = (idhm - mean(idhm)) / sd(idhm),
    ranking_idhm = min_rank(desc(idhm))
  ) |>
  select(municipio, log_pop, idhm_padronizado, ranking_idhm) |>
  head(6)

# --- summarise + group_by ---

municipios |>
  group_by(regiao) |>
  summarise(
    n = n(),
    idhm_medio = mean(idhm) |> round(3),
    voto_gov_medio = mean(perc_votos_gov) |> round(1),
    comparecimento = mean(taxa_comparecimento) |> round(2)
  )

# --- count ---

municipios |>
  count(regiao, sort = TRUE) |>
  mutate(prop = n / sum(n),
         prop = scales::percent(prop))

# --- arrange ---

municipios |>
  select(municipio, uf, idhm) |>
  arrange(desc(idhm)) |>
  head(5)

# --- EXERCÍCIO: análise regional ---
# 1. Crie uma variável faixa_idh (Baixo < 0.6, Médio 0.6-0.75, Alto > 0.75)
# 2. Agrupe por regiao e faixa_idh
# 3. Calcule o voto médio no governo e o número de municípios
# 4. Ordene por voto médio decrescente



# --- tidyr: pivot_wider ---

votos_wide <- municipios |>
  mutate(faixa_idh = cut(idhm, breaks = c(0, 0.6, 0.75, 1),
                          labels = c("Baixo","Médio","Alto"))) |>
  group_by(regiao, faixa_idh) |>
  summarise(voto_medio = mean(perc_votos_gov) |> round(1), .groups = "drop") |>
  pivot_wider(names_from = faixa_idh, values_from = voto_medio)

votos_wide

# --- tidyr: pivot_longer ---

votos_wide |>
  pivot_longer(-regiao, names_to = "faixa_idh", values_to = "voto_medio")

# --- separate e unite ---

mun_cod <- municipios |>
  mutate(uf_regiao = paste(uf, regiao, sep = "-")) |>
  select(municipio, uf_regiao) |>
  head(6)

mun_cod

mun_cod |>
  separate(uf_regiao, into = c("uf", "regiao"), sep = "-")

# --- duplicatas e limpeza ---

municipios |>
  distinct(regiao)

# dados |> janitor::clean_names()
# dados |> janitor::get_dupes(municipio)

# --- valores ausentes ---

municipios |>
  summarise(across(everything(), ~ sum(is.na(.x))))

# --- joins ---

indicadores_uf <- municipios |>
  group_by(uf) |>
  summarise(idhm_medio = mean(idhm) |> round(3), .groups = "drop") |>
  head(6)

gov_uf <- tibble(
  uf = c("SP", "RJ", "MG", "BA", "RS", "CE"),
  governador = c("Tarcísio", "Cláudio", "Zema",
                 "Jerônimo", "Leite", "Elmano")
)

# left join
indicadores_uf |>
  left_join(gov_uf, by = "uf")

# inner join
indicadores_uf |>
  inner_join(gov_uf, by = "uf")

# anti join
indicadores_uf |>
  anti_join(gov_uf, by = "uf")


# AULA 4 — Visualizando e Mapeando ==========

# --- Gráfico de barras ---

municipios |>
  group_by(regiao) |>
  summarise(voto_medio = mean(perc_votos_gov)) |>
  ggplot(aes(x = reorder(regiao, voto_medio), y = voto_medio, fill = regiao)) +
  geom_col() +
  coord_flip() +
  labs(title = "Votação Média no Governo por Região",
       x = "", y = "% Votos no Governo") +
  theme_minimal() +
  theme(legend.position = "none")

# --- Dispersão: IDH x Voto (Lipset, 1959) ---

municipios |>
  ggplot(aes(x = idhm, y = perc_votos_gov, color = regiao)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(title = "Modernização e Voto: a Tese de Lipset",
       x = "IDHM", y = "% Votos no Governo", color = "Região") +
  theme_minimal()

# --- Histograma ---

municipios |>
  ggplot(aes(x = idhm, fill = regiao)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(title = "Distribuição do IDHM por Região",
       x = "IDHM", y = "Frequência", fill = "Região") +
  theme_minimal()

# --- Boxplot + Facets ---

municipios |>
  mutate(faixa_idh = cut(idhm, breaks = c(0, 0.6, 0.75, 1),
                          labels = c("Baixo","Médio","Alto"))) |>
  ggplot(aes(x = faixa_idh, y = perc_votos_gov, fill = faixa_idh)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~regiao) +
  labs(title = "Distribuição do Voto por IDH e Região",
       x = "Faixa de IDH", y = "% Votos no Governo") +
  theme_minimal() +
  theme(legend.position = "none")

# --- Heatmap ---

municipios |>
  mutate(faixa_idh = cut(idhm, breaks = c(0, 0.6, 0.75, 1),
                          labels = c("Baixo","Médio","Alto"))) |>
  group_by(regiao, faixa_idh) |>
  summarise(voto_medio = mean(perc_votos_gov), .groups = "drop") |>
  ggplot(aes(x = faixa_idh, y = regiao, fill = voto_medio)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(voto_medio, 1)), color = "white", fontface = "bold") +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick",
                       midpoint = 50) +
  labs(title = "Votação Média: Região × IDH", x = "Faixa IDH", y = "") +
  theme_minimal()

# --- Escalas divergentes ---

municipios |>
  ggplot(aes(x = idhm, y = pib_per_capita, color = perc_votos_gov)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_gradient2(low = "steelblue", mid = "white", high = "firebrick",
                        midpoint = 50, name = "% Voto Gov") +
  scale_y_log10(labels = scales::dollar_format(prefix = "R$ ")) +
  labs(title = "PIB per Capita, IDHM e Voto",
       x = "IDHM", y = "PIB per Capita (escala log)") +
  theme_minimal()

# --- Patchwork ---

p1 <- municipios |>
  ggplot(aes(x = idhm, y = perc_votos_gov)) +
  geom_point(alpha = 0.3) + geom_smooth(method = "lm") +
  labs(title = "IDHM × Voto", x = "IDHM", y = "% Gov") + theme_minimal()

p2 <- municipios |>
  ggplot(aes(x = regiao, fill = regiao)) +
  geom_bar() + coord_flip() +
  labs(title = "Municípios por Região", x = "", y = "N") +
  theme_minimal() + theme(legend.position = "none")

p1 + p2 + plot_annotation(title = "Dashboard Eleitoral")

# --- Mapas: sf + geobr ---

estados_br <- read_state(year = 2020, showProgress = FALSE)

# Mapa coroplético
dados_uf <- municipios |>
  group_by(uf) |>
  summarise(voto_medio = mean(perc_votos_gov), .groups = "drop")

mapa <- estados_br |>
  left_join(dados_uf, by = c("abbrev_state" = "uf"))

ggplot(mapa) +
  geom_sf(aes(fill = voto_medio), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick",
                       midpoint = 50, name = "% Gov") +
  theme_void() +
  labs(title = "Votação Média no Governo por Estado")

# Mapa com capitais
capitais_geo <- read_municipal_seat(showProgress = FALSE)

ggplot() +
  geom_sf(data = estados_br, fill = "grey95", color = "grey60") +
  geom_sf(data = capitais_geo, color = "firebrick", size = 1.5, alpha = 0.7) +
  theme_void() +
  labs(title = "Capitais Brasileiras")


# --- Pontos Ideais ---

set.seed(2025)
n_parl <- 40
n_vot <- 80

# Posições ideológicas "verdadeiras"
posicao_ideal <- c(rnorm(10, -1.5, 0.3), rnorm(10, -0.3, 0.4),
             rnorm(10, 0.5, 0.4), rnorm(10, 1.5, 0.3))
bloco <- rep(c("Esquerda","Centro-Esq","Centro-Dir","Direita"), each = 10)

# Simular votos
pontos_corte <- runif(n_vot, -2, 2)
votos_mat <- matrix(NA, n_parl, n_vot)
for(j in 1:n_vot) {
  votos_mat[, j] <- rbinom(n_parl, 1, plogis(2 * (posicao_ideal - pontos_corte[j])))
}

# PCA
pca <- prcomp(votos_mat, center = TRUE, scale. = TRUE)

pontos <- tibble(
  bloco = bloco,
  posicao_real = posicao_ideal,
  pc1 = pca$x[, 1],
  pc2 = pca$x[, 2]
)

# Mapa do Plenário
ggplot(pontos, aes(x = pc1, y = pc2, color = bloco)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c("Esquerda" = "red", "Centro-Esq" = "salmon",
                                 "Centro-Dir" = "lightblue", "Direita" = "darkblue")) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  labs(title = "Mapa Espacial do Plenário",
       subtitle = "Pontos ideais estimados via PCA sobre votações simuladas",
       x = "1ª Dimensão (Gov-Oposição)", y = "2ª Dimensão", color = "Bloco") +
  theme_minimal()

# Validação
ggplot(pontos, aes(x = posicao_real, y = pc1, color = bloco)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "grey30", linetype = "dashed") +
  scale_color_manual(values = c("Esquerda" = "red", "Centro-Esq" = "salmon",
                                 "Centro-Dir" = "lightblue", "Direita" = "darkblue")) +
  labs(title = "Posição Real vs. Estimada",
       x = "Posição Ideológica Real", y = "PC1 (estimada)", color = "Bloco") +
  theme_minimal()

# Na prática: API da Câmara
# id_votacao <- "2372912"
# url_votos <- paste0(
#   "https://dadosabertos.camara.leg.br/api/v2/votacoes/",
#   id_votacao, "/votos")
# votos <- request(url_votos) |>
#   req_perform() |>
#   resp_body_json() |>
#   pluck("dados") |>
#   map_dfr(~ tibble(
#     nome = .x$deputado_$nome,
#     partido = .x$deputado_$siglaPartido,
#     voto = .x$tipoVoto))


# AULA 5 — Comunicando seus Dados ===============

# --- Tabelas com knitr::kable ---

municipios |>
  group_by(regiao) |>
  summarise(
    n = n(),
    idhm_medio = mean(idhm) |> round(3),
    voto_medio = mean(perc_votos_gov) |> round(1)
  ) |>
  knitr::kable(col.names = c("Região", "N", "IDHM Médio", "% Voto Gov"))

# --- Regressão com modelsummary ---

modelo1 <- lm(perc_votos_gov ~ idhm, data = municipios)
modelo2 <- lm(perc_votos_gov ~ idhm + log(populacao), data = municipios)
modelo3 <- lm(perc_votos_gov ~ idhm + log(populacao) + pib_per_capita + regiao,
              data = municipios)

modelsummary(
  list("Bivariado" = modelo1, "Controle Pop." = modelo2, "Completo" = modelo3),
  stars = TRUE, gof_omit = "AIC|BIC|Log|RMSE"
)

# ==============================================================================
# FIM DO SCRIPT
# ==============================================================================
