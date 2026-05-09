# ==============================================================================
# SPRINT 1 - STATISTICAL COMPUTING WITH R AND PYTHON
# Tutorial de Analise de Dados Seguros Agrícolas - Sompo
# ==============================================================================

# 1. Importação de Bibliotecas
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(knitr)

# 2. Importação da Base de Dados
# Nota: Certifique-se de que o arquivo esta na pasta 'dados/'
df_sompo <- read_excel("dados/dados_abertos_psr_2016a2024.xlsx", na = c("", "NA", "-"))

# 3. Análise Exploratória
glimpse(df_sompo)

# 4. Configuração de Reprodutibilidade
set.seed(42)

# 5. Processamento e Criação da Nova Base de Dados (Amostra de 150 linhas)
new_df <- df_sompo %>%
    drop_na(NM_CULTURA_GLOBAL, NR_AREA_TOTAL, NivelDeCobertura, ANO_APOLICE, 
            NR_PRODUTIVIDADE_ESTIMADA, VL_PREMIO_LIQUIDO) %>%
    mutate(
        valor_indenizacao_brl = as.numeric(ifelse(is.na(VALOR_INDENIZAÇÃO), 0, VALOR_INDENIZAÇÃO)),
        causa_sinistro = ifelse(is.na(EVENTO_PREPONDERANTE), "Sem Sinistro", trimws(EVENTO_PREPONDERANTE)),
        porte_propriedade = factor(
            ifelse(NR_AREA_TOTAL <= 100, "Pequeno",
                ifelse(NR_AREA_TOTAL <= 500, "Médio", "Grande")
            ),
            levels = c("Pequeno", "Médio", "Grande"),
            ordered = TRUE
        ),
        nivel_cobertura = paste0(round(NivelDeCobertura * 100, 2), "%")
    ) %>%
    select(
        # Qualitativas Nominais
        Cultura_Agricola = NM_CULTURA_GLOBAL, 
        Causa_Sinistro = causa_sinistro,
        
        # Qualitativas Ordinais
        porte_propriedade = porte_propriedade,
        Nivel_Cobertura = nivel_cobertura,
        
        # Quantitativas Discretas
        ano_police = ANO_APOLICE,
        produtividade_estimada = NR_PRODUTIVIDADE_ESTIMADA,
        
        # Quantitativas Contínuas
        valor_premio_brl = VL_PREMIO_LIQUIDO,
        valor_indenizacao_brl = valor_indenizacao_brl
    ) %>%
    # Sorteia 150 linhas
    sample_n(150)

# Exibição das 5 primeiras observações
kable(head(new_df, 5), caption = "Amostra das 5 primeiras observações da base final")

# 6. Exportando a base tratada para Excel
write_xlsx(new_df, path = "base_dados_sompo.xlsx")

# ==============================================================================
# 7. TABELAS DE DISTRIBUIÇÃO DE FREQUÊNCIAS
# ==============================================================================

### a) Variável Discreta: Produtividade Estimada
tabela_discreta <- new_df %>%
    count(produtividade_estimada, name = "frequencia_absoluta") %>%
    mutate(
        frequencia_relativa = frequencia_absoluta / sum(frequencia_absoluta),
        frequencia_percentual = paste0(round(frequencia_relativa * 100, 2), "%")
    ) %>%
    arrange(desc(frequencia_absoluta))

kable(head(tabela_discreta, 10), caption = "Top 10 Frequências - Produtividade Estimada")

# INSIGHTS DE NEGÓCIO - VARIÁVEL DISCRETA
# Insight 1: Arredondamento e Comportamento de Contratação.
# Valores redondos sugerem uso de médias regionais/comerciais na contratação.
# Insight 2: Altíssima Dispersão e Pulverização de Risco.
# O risco não está concentrado, garantindo saúde financeira à carteira.

### b) Variável Contínua: Valor da Indenização (Regra de Sturges)

# Filtro: Apenas sinistros reais (valor > 0)
df_sinistros <- new_df %>% 
    filter(valor_indenizacao_brl > 0)

# Regra de Sturges
n_obs <- nrow(df_sinistros)
k_sturges <- round(1 + 3.322 * log10(n_obs))

# Construção da Tabela Contínua
tabela_continua <- df_sinistros %>%
    mutate(
        faixa_indenizacao = cut(
            valor_indenizacao_brl, 
            breaks = k_sturges, 
            include.lowest = TRUE,
            dig.lab = 10
        )
    ) %>%
    count(faixa_indenizacao, name = "frequencia_absoluta") %>%
    mutate(
        frequencia_relativa = frequencia_absoluta / sum(frequencia_absoluta),
        frequencia_percentual = paste0(round(frequencia_relativa * 100, 2), "%"),
        frequencia_acumulada  = cumsum(frequencia_relativa)
    )

# Exibição da Tabela
kable(tabela_continua, caption = "Distribuição de Frequências - Valor de Indenização (Regra de Sturges)")

# INSIGHTS DE NEGÓCIO - VARIÁVEL CONTÍNUA
# Insight 1: Concentração em Sinistros de Menor Severidade (80% até R$ 115 mil).
# Insight 2: Risco de Cauda Longa e Eventos Catastróficos (raros, mas de alto valor).
