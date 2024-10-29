# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(stringr)
library(hrbrthemes)
# Importar o arquivo Excel (substitua "caminho_do_arquivo.xlsx" pelo caminho real)
dados <- read_excel("Planilha_BCB_Bhub.xlsx")

colnames(dados) <- c("Data", "Microempreendedores", "MEI")
  
# Converter colunas para numéricas, substituindo "-" por NA
  dados <- dados %>%
    mutate(
      MEI = as.numeric(MEI),
      Microempreendedores = as.numeric(Microempreendedores)
    )

# Transformar os dados para o formato longo
  dados_long <- dados %>%
    pivot_longer(cols = c("MEI", "Microempreendedores"), 
                 names_to = "tipo", 
                 values_to = "taxa_inadimplencia")
  
# Remover as duas últimas linhas do data frame (linhas com fontes da base de dados)
  dados_long <- dados_long %>% 
    slice(1:(n() - 2))
  
# Extraindo ano e trimestre e formatando para o início de cada trimestre
  dados_long <- dados_long %>%
    mutate(
      Ano = as.numeric(str_extract(Data, "\\d{4}")), # Extrai números que sejam uma sequência de quatro dígitos de 0 a 9, ou seja, os dígitos que indicam o ano.                       # Extrai o ano
      Trimestre = as.numeric(str_extract(Data, "^\\d")), # Extrai o primeiro dígito do início da string, ou seja, o dígito que indica o trimestre.
      Data_formatada = make_date(Ano, (Trimestre - 1) * 3 + 1), # Define a data como início do trimestre (Ano/Mês/Dia)
    )

# Removendo colunas desnecessárias (Data original, Ano, Trimestre)
  dados_long <- select(dados_long, -Data, -Ano, -Trimestre)

# Plotar o gráfico com tema escuro e cores personalizadas
  grafico <- ggplot(dados_long, aes(x = Data_formatada, y = taxa_inadimplencia, color = tipo)) +
    geom_line(size = 1.2) +  # Aumentando um pouco a espessura da linha
    labs(
      title = "Taxa de Inadimplência de MEI e Microempreendedores (2012-2023)",
      x = "Data (Mês/Ano)",
      y = "Taxa de Inadimplência (%)",
      color = "Tipo de Empreendedor"
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(limits = c(0, 20)) +
    # Escolha manual das cores das linhas
    scale_color_manual(values = c(
      "MEI" = "#FF0066",              # Laranja
      "Microempreendedores" = "#56B4E9"  # Azul claro
    )) +
    theme(
      panel.background = element_rect(fill = "#1C1C1C", color = NA),   # Fundo do gráfico preto
      plot.background = element_rect(fill = "#1C1C1C", color = NA),    # Fundo da área total preto
      panel.grid.major = element_line(color = "gray50"),             # Grades maiores em cinza
      panel.grid.minor = element_line(color = "gray30"),             # Grades menores em cinza
      axis.text = element_text(color = "white"),                     # Texto dos eixos em branco
      axis.title = element_text(color = "white", size = 14),         # Títulos dos eixos em branco
      plot.title = element_text(color = "white", size = 16, face = "bold", hjust = 0.5),  # Título centralizado
      legend.background = element_rect(fill = "#1C1C1C"),              # Fundo da legenda preto
      legend.text = element_text(color = "white"),                   # Texto da legenda em branco
      axis.text.x = element_text(angle = 45, hjust = 1)              # Rotaciona os rótulos do eixo X
    )
  
  # Exibir o gráfico
  print(grafico)
