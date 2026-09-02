# ---------------------------------------------------------------------------- #
# Script R: Modelo de Dinâmica de Sistemas da Degradação - v3.9i
# (AMPLITUDE AUMENTADA: Forçando drivers a ter maior impacto para capturar picos)
# (Fator clima aumentado, k_lin com lower_bounds significativamente maiores)
# ---------------------------------------------------------------------------- #

library(FME)
library(dplyr)
library(ggplot2)
library(readxl)
library(stringr)
library(tidyr)
library(patchwork)

# --- Configurações do usuário ---
excel_producao_path       <- "C:/Users/Argemiro/Documents/sos_amazon_degradation/serie_extrativismo_vegetal_total_legal_ilegal.xlsx"
excel_degradacao_path     <- "C:/Users/Argemiro/Documents/sos_amazon_degradation/serie_historica_degradacao_media.xlsx"
excel_dias_secos_path     <- "C:/Users/Argemiro/Documents/sos_amazon_degradation/serie_historica_dias_secos.xlsx"
excel_desmatamento_path   <- "C:/Users/Argemiro/Documents/sos_amazon_degradation/terrabrasilis_amazon_deforestation.xlsx"
excel_precos_path         <- "C:/Users/Argemiro/Documents/sos_amazon_degradation/serie_historica_precos_madeira_tropical.xlsx"
excel_pastagem_path       <- "C:/Users/Argemiro/Documents/sos_amazon_degradation/serie_historica_area_pastagem.xlsx"

initial_simulation_year <- 2007

# --- 0. Dados históricos ---

cat("Carregando dados...\n")

# Produção de madeira (apenas para contexto/gráficos)
producao_data <- read_excel(excel_producao_path) %>%
  setNames(tolower(names(.))) %>%
  dplyr::select(ano, producao) %>%
  arrange(ano)

# Degradação histórica (KM²)
degradacao_historica_data <- read_excel(excel_degradacao_path) %>%
  setNames(tolower(names(.))) %>%
  dplyr::select(ano, media_degradacao) %>%
  arrange(ano)

# Dias secos (clima)
dias_secos_historica_data <- read_excel(excel_dias_secos_path) %>%
  setNames(tolower(names(.))) %>%
  dplyr::select(ano, media_dias_secos) %>%
  arrange(ano)
interp_chuva_func <- approxfun(
  x    = dias_secos_historica_data$ano,
  y    = dias_secos_historica_data$media_dias_secos,
  rule = 2
)

# Desmatamento
desmatamento_data <- read_excel(excel_desmatamento_path) %>%
  setNames(tolower(names(.))) %>%
  dplyr::select(ano, area_desmatada_km2) %>%
  arrange(ano)
interp_desmatamento_func <- approxfun(
  x    = desmatamento_data$ano,
  y    = desmatamento_data$area_desmatada_km2,
  rule = 2
)

# Preços da madeira tropical
precos_data <- read_excel(excel_precos_path) %>%
  setNames(tolower(names(.))) %>%
  dplyr::select(ano, media_preco) %>%
  arrange(ano)
interp_precos_func <- approxfun(
  x    = precos_data$ano,
  y    = precos_data$media_preco,
  rule = 2
)

# Área de pastagem
pastagem_data <- read_excel(excel_pastagem_path) %>%
  setNames(tolower(names(.))) %>%
  dplyr::select(ano, pastagem) %>%
  arrange(ano)
interp_pastagem_func <- approxfun(
  x    = pastagem_data$ano,
  y    = pastagem_data$pastagem,
  rule = 2
)

# Condição inicial de degradação
initial_degradation_area <- degradacao_historica_data %>%
  filter(ano == initial_simulation_year) %>%
  pull(media_degradacao)

if (length(initial_degradation_area) == 0 || is.na(initial_degradation_area)) {
  stop("Erro: condição inicial de degradação não encontrada.")
}
cat("Degradação inicial:", round(initial_degradation_area, 2), "km²\n")

# Máximos globais para normalização
max_dias_secos_global   <- max(dias_secos_historica_data$media_dias_secos, na.rm = TRUE)
max_desmatamento_global <- max(desmatamento_data$area_desmatada_km2, na.rm = TRUE)
max_precos_global       <- max(precos_data$media_preco, na.rm = TRUE)
max_pastagem_global     <- max(pastagem_data$pastagem, na.rm = TRUE)

# Média histórica do clima normalizado (para anomalia)
media_clima_norm <- mean(
  (dias_secos_historica_data$media_dias_secos / max_dias_secos_global) * 100,
  na.rm = TRUE
)

state_0 <- initial_degradation_area

simulation_end_year <- 2024
times <- seq(from = initial_simulation_year, to = simulation_end_year, by = 1)

# --- Controle: peso de clima (AUMENTADO para dar mais potência ao clima) ---
fator_clima_driver <- 20 # antes 10

# ------------------------------------------------------------------------------
# 4. Calibração (modelo discreto em km²)
# ------------------------------------------------------------------------------

cat("\nPreparando calibração (v3.9i - Aumentando Amplitude)...\n")

# Lags FIXOS (Clima imediato costuma ser melhor para picos de fogo)
lag_desmat_fix   <- 1
lag_chuva_fix    <- 0 
lag_preco_fix    <- 0
lag_pastagem_fix <- 0

# 4.1. Parâmetros calibráveis (Chutes iniciais mais fortes e dentro dos novos limites)
par_fit <- c(
  k_degrad_base      = 500,  # Base de degradação (para manter um "chão")
  k_recuperacao_base = 0.50, # Alta recuperação inicial
  k_desmat_lin       = 100,  # Chute inicial mais alto para desmatamento
  k_chuva_lin        = 50,   # Chute inicial mais alto para clima
  k_preco_lin        = 100,  # Chute inicial mais alto para preço
  k_pastagem_lin     = 50    # Chute inicial mais alto para pastagem
)

# 4.2. Parâmetros fixos
par_fixed <- list(
  interp_chuva_func        = interp_chuva_func,
  interp_desmatamento_func = interp_desmatamento_func,
  interp_precos_func       = interp_precos_func,
  interp_pastagem_func     = interp_pastagem_func,
  max_dias_secos_global    = max_dias_secos_global,
  max_desmatamento_global  = max_desmatamento_global,
  max_precos_global        = max_precos_global,
  max_pastagem_global      = max_pastagem_global,
  media_clima_norm         = media_clima_norm,
  fator_clima_driver       = fator_clima_driver,
  lag_desmat               = lag_desmat_fix,
  lag_chuva                = lag_chuva_fix,
  lag_preco                = lag_preco_fix,
  lag_pastagem             = lag_pastagem_fix
)

# 4.3. Dados usados na calibração
data_calib <- degradacao_historica_data %>%
  filter(ano >= initial_simulation_year, ano <= simulation_end_year)

anos_calib <- data_calib$ano
n_obs      <- length(anos_calib)

# 4.4. Modelo discreto para calibração
run_model <- function(par_vec) {
  parms <- c(as.list(par_vec), par_fixed)
  anos  <- anos_calib
  n     <- length(anos)
  
  degr <- numeric(n)
  degr[1] <- state_0
  
  for (i in 1:(n - 1)) {
    ano_atual <- anos[i]
    
    # Lags
    t_chuva_lagged        <- ano_atual - parms$lag_chuva
    t_desmatamento_lagged <- ano_atual - parms$lag_desmat
    t_precos_lagged       <- ano_atual - parms$lag_preco
    t_pastagem_lagged     <- ano_atual - parms$lag_pastagem
    
    # Interpolação
    dias_sem_chuva_raw <- parms$interp_chuva_func(t_chuva_lagged)
    desmatamento_raw   <- parms$interp_desmatamento_func(t_desmatamento_lagged)
    preco_madeira_raw  <- parms$interp_precos_func(t_precos_lagged)
    pastagem_raw       <- parms$interp_pastagem_func(t_pastagem_lagged)
    
    # Normalização
    dias_sem_chuva_norm <- (dias_sem_chuva_raw / parms$max_dias_secos_global)   * 100
    desmatamento_norm   <- (desmatamento_raw   / parms$max_desmatamento_global) * 100
    preco_madeira_norm  <- (preco_madeira_raw  / parms$max_precos_global)       * 100
    pastagem_norm       <- (pastagem_raw       / parms$max_pastagem_global)     * 100
    
    # Anomalia Climática
    clima_anomalia     <- dias_sem_chuva_norm - parms$media_clima_norm
    clima_anomalia_eff <- clima_anomalia * parms$fator_clima_driver
    
    # Impactos
    impact_desmat   <- parms$k_desmat_lin   * desmatamento_norm
    impact_chuva    <- parms$k_chuva_lin    * clima_anomalia_eff
    impact_preco    <- parms$k_preco_lin    * preco_madeira_norm
    impact_pastagem <- parms$k_pastagem_lin * pastagem_norm
    
    Taxa_Nova_Degradacao <- parms$k_degrad_base +
      impact_desmat +
      impact_chuva +
      impact_preco +
      impact_pastagem
    
    # Recuperação: proporcional à área
    Taxa_Recuperacao <- parms$k_recuperacao_base * degr[i]
    
    dDegr <- Taxa_Nova_Degradacao - Taxa_Recuperacao
    degr[i + 1] <- degr[i] + dDegr
    
    # Trava no zero
    degr[i + 1] <- max(degr[i + 1], 0)
  }
  
  data.frame(
    ano = anos,
    Degradacao_Area_km2 = degr
  )
}

# 4.5. Resíduos com PESOS ESTRATÉGICOS
residuals_fun <- function(par_vec) {
  sim <- try(run_model(par_vec), silent = TRUE)
  if (inherits(sim, "try-error") || nrow(sim) < n_obs) {
    return(rep(1e9, n_obs)) 
  }
  
  sim_degrad <- as.numeric(sim$Degradacao_Area_km2[1:n_obs])
  obs_degrad <- as.numeric(data_calib$media_degradacao[1:n_obs])
  if (any(!is.finite(sim_degrad))) {
    return(rep(1e9, n_obs))
  }
  
  res <- sim_degrad - obs_degrad
  
  # PESOS FOCADOS EM PICOS DE VOLATILIDADE
  pesos <- rep(1, n_obs)
  
  # Pico de 2010 (Seca histórica)
  pesos[anos_calib %in% 2009:2010] <- 10 
  
  # Pico de 2015-2016 (El Niño forte)
  pesos[anos_calib %in% 2015:2016] <- 10 
  
  # Período recente (Tendência atual)
  pesos[anos_calib >= 2019]        <- 10 
  
  res * pesos
}

# 4.6. Limites dos parâmetros (CRÍTICO: LOWER_BOUNDS MAIS ALTOS para K_LIN)
lower_bounds <- c(
  k_degrad_base      = 0,
  k_recuperacao_base = 0.1,   # Mínimo 10% de turnover anual
  k_desmat_lin       = 50,    # Aumentado para forçar impacto significativo
  k_chuva_lin        = 20,    # Aumentado para forçar impacto significativo
  k_preco_lin        = 50,    # Aumentado para forçar impacto significativo
  k_pastagem_lin     = 20     # Aumentado para forçar impacto significativo
)

upper_bounds <- c(
  k_degrad_base      = 30000, 
  k_recuperacao_base = 0.999, # Permite limpar quase toda a degradação de um ano pro outro
  k_desmat_lin       = 30000, # Teto para impacto de desmatamento
  k_chuva_lin        = 30000, # Teto para impacto de clima
  k_preco_lin        = 30000, # Teto para impacto de preço
  k_pastagem_lin     = 30000  # Teto para impacto de pastagem
)

cat("\nCalibrando (v3.9i)...\n")
fit <- modFit(
  f      = residuals_fun,
  p      = par_fit,
  lower  = lower_bounds,
  upper  = upper_bounds,
  method = "Port"
)

cat("\nParâmetros calibrados:\n")
print(fit$par)

best_par <- fit$par

# ------------------------------------------------------------------------------
# 5. Simulação final histórica (até 2024)
# ------------------------------------------------------------------------------

simula_discreto <- function(parms) {
  anos <- times
  n    <- length(anos)
  degr <- numeric(n)
  degr[1] <- state_0
  
  for (i in 1:(n - 1)) {
    ano_atual <- anos[i]
    
    t_chuva_lagged        <- ano_atual - parms$lag_chuva
    t_desmatamento_lagged <- ano_atual - parms$lag_desmat
    t_precos_lagged       <- ano_atual - parms$lag_preco
    t_pastagem_lagged     <- ano_atual - parms$lag_pastagem
    
    dias_sem_chuva_raw <- parms$interp_chuva_func(t_chuva_lagged)
    desmatamento_raw   <- parms$interp_desmatamento_func(t_desmatamento_lagged)
    preco_madeira_raw  <- parms$interp_precos_func(t_precos_lagged)
    pastagem_raw       <- parms$interp_pastagem_func(t_pastagem_lagged)
    
    dias_sem_chuva_norm <- (dias_sem_chuva_raw / parms$max_dias_secos_global)   * 100
    desmatamento_norm   <- (desmatamento_raw   / parms$max_desmatamento_global) * 100
    preco_madeira_norm  <- (preco_madeira_raw  / parms$max_precos_global)       * 100
    pastagem_norm       <- (pastagem_raw       / parms$max_pastagem_global)     * 100
    
    clima_anomalia     <- dias_sem_chuva_norm - parms$media_clima_norm
    clima_anomalia_eff <- clima_anomalia * parms$fator_clima_driver
    
    impact_desmat   <- parms$k_desmat_lin   * desmatamento_norm
    impact_chuva    <- parms$k_chuva_lin    * clima_anomalia_eff
    impact_preco    <- parms$k_preco_lin    * preco_madeira_norm
    impact_pastagem <- parms$k_pastagem_lin * pastagem_norm
    
    Taxa_Nova_Degradacao <- parms$k_degrad_base +
      impact_desmat +
      impact_chuva +
      impact_preco +
      impact_pastagem
    
    Taxa_Recuperacao <- parms$k_recuperacao_base * degr[i]
    
    dDegr <- Taxa_Nova_Degradacao - Taxa_Recuperacao
    degr[i + 1] <- degr[i] + dDegr
    
    degr[i + 1] <- max(degr[i + 1], 0)
  }
  
  data.frame(
    ano = anos,
    Degradacao_Area_km2 = degr
  )
}

cat("\nRodando simulação final (v3.9i)...\n")
parameters_list <- c(as.list(best_par), par_fixed)
output_df <- simula_discreto(parameters_list)

cat("Simulação concluída. Resultados parciais:\n")
print(head(output_df))
print(tail(output_df))

# --- 6. Visualizar (nível + drivers) ---------------------------------------- #

plot_df_full <- output_df %>%
  left_join(producao_data,             by = "ano") %>%
  left_join(degradacao_historica_data, by = "ano") %>%
  left_join(dias_secos_historica_data, by = "ano") %>%
  left_join(desmatamento_data,         by = "ano") %>%
  left_join(precos_data,               by = "ano") %>%
  left_join(pastagem_data,             by = "ano")

# 6.1. Degradação (Simulado vs. Histórico) - Eixo Y agora em km²
plot_degradation <- ggplot(plot_df_full, aes(x = ano)) +
  geom_line(aes(y = Degradacao_Area_km2, color = "Simulado (km²)"), size = 1) +
  geom_point(aes(y = Degradacao_Area_km2, color = "Simulado (km²)"), size = 2) +
  geom_line(aes(y = media_degradacao, color = "Histórico (km²)"), size = 1, na.rm = TRUE) +
  geom_point(aes(y = media_degradacao, color = "Histórico (km²)"), size = 2, na.rm = TRUE) +
  scale_y_continuous(name = "Área de Degradação (km²)", limits = c(0, NA)) +
  labs(title = "1. Degradação da Vegetação Natural",
       subtitle = "Simulado vs. Histórico - Modelo de Alta Volatilidade (km²)",
       x = "Ano") +
  scale_color_manual(values = c("Simulado (km²)" = "blue", "Histórico (km²)" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "bottom")

# 6.2. Drivers (Normalizados)
max_dias_secos_plot   <- max(dias_secos_historica_data$media_dias_secos, na.rm = TRUE)
max_desmatamento_plot <- max(desmatamento_data$area_desmatada_km2, na.rm = TRUE)
max_precos_plot       <- max(precos_data$media_preco, na.rm = TRUE)
max_pastagem_plot     <- max(pastagem_data$pastagem, na.rm = TRUE)

plot_df_drivers_normalized <- plot_df_full %>%
  mutate(
    Dias_Sem_Chuva_Driver = media_dias_secos   / max_dias_secos_plot    * 100,
    Desmatamento_Driver   = area_desmatada_km2 / max_desmatamento_plot  * 100,
    Preco_Madeira_Driver  = media_preco        / max_precos_plot        * 100,
    Pastagem_Driver       = pastagem           / max_pastagem_plot      * 100
  ) %>%
  dplyr::select(ano,
                Dias_Sem_Chuva_Driver,
                Desmatamento_Driver,
                Preco_Madeira_Driver,
                Pastagem_Driver) %>%
  pivot_longer(
    cols      = c(Dias_Sem_Chuva_Driver,
                  Desmatamento_Driver,
                  Preco_Madeira_Driver,
                  Pastagem_Driver),
    names_to  = "Driver",
    values_to = "Valor_Normalizado"
  )

plot_drivers <- ggplot(plot_df_drivers_normalized, aes(x = ano, y = Valor_Normalizado, color = Driver)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Driver, ncol = 1, scales = "free_y") +
  labs(title = "2. Drivers Históricos (Normalizados)",
       subtitle = "Clima, Desmatamento, Preços e Pastagem (Impacto escalado para km²)",
       y = "Valor Normalizado (0-100%)",
       x = "Ano") +
  scale_color_manual(values = c(
    "Dias_Sem_Chuva_Driver" = "purple",
    "Desmatamento_Driver"   = "orange",
    "Preco_Madeira_Driver"  = "brown",
    "Pastagem_Driver"       = "darkgreen"
  ),
  labels = c("Dias_Sem_Chuva_Driver" = "Dias Sem Chuva",
             "Desmatamento_Driver"   = "Desmatamento",
             "Preco_Madeira_Driver"  = "Preço Madeira Tropical",
             "Pastagem_Driver"       = "Área de Pastagem")) +
  theme_minimal() +
  theme(legend.position = "none")

cat("\nGerando gráficos combinados...\n")
combined_plot <- plot_degradation / plot_drivers

print(combined_plot)

# 1. Juntar os dados observados e simulados no mesmo dataframe para os anos correspondentes
# CORREÇÃO APLICADA: A coluna histórica se chama media_degradacao
dados_comparacao <- data_calib %>%
  left_join(output_df, by = "ano") %>%
  select(ano, Observado = media_degradacao, Simulado = Degradacao_Area_km2)

# 2. Calcular a Soma dos Quadrados dos Resíduos (RSS) e a Soma Total dos Quadrados (TSS)
rss <- sum((dados_comparacao$Observado - dados_comparacao$Simulado)^2, na.rm = TRUE)
tss <- sum((dados_comparacao$Observado - mean(dados_comparacao$Observado, na.rm = TRUE))^2, na.rm = TRUE)

# 3. Calcular o R-quadrado (R²)
r_squared <- 1 - (rss / tss)
variancia_explicada_pct <- r_squared * 100

# 4. Imprimir o resultado
cat(sprintf("\nVariância explicada (R²): %.2f%%\n", variancia_explicada_pct))
