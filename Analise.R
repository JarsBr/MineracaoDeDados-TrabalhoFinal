# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("caret")
# install.packages("lattice")
# install.packages("randomForest")
# install.packages("xgboost")
# install.packages("lightgbm")

# Função de preprocessamento
preprocessar_dados <- function(dados) {
  library(dplyr)
  library(caret)
  
  # Codificar variáveis categóricas
  dados$Month <- factor(dados$Month)
  dados$VisitorType <- factor(dados$VisitorType)
  dados$Weekend <- as.factor(dados$Weekend)
  dados$Revenue <- as.factor(dados$Revenue)
  
  # Escalar variáveis contínuas
  variaveis_continuas <- c("Administrative_Duration", "Informational_Duration", 
                           "ProductRelated_Duration", "BounceRates", "ExitRates")
  dados[variaveis_continuas] <- scale(dados[variaveis_continuas])
  
  # Dividir dados em treino e teste
  set.seed(123)  # Para resultados reprodutíveis
  indice_treino <- createDataPartition(dados$Revenue, p = 0.7, list = FALSE)
  dados_treino <- dados[indice_treino, ]
  dados_teste <- dados[-indice_treino, ]
  
  # Retornar conjunto de treino e teste
  return(list(treino = dados_treino, teste = dados_teste))
}

library(ROSE)

# Carregar os dados
dados <- read.csv("dataset/online_shoppers_intention.csv")

# Converter Revenue para fator
dados$Revenue <- as.factor(dados$Revenue)

# Preprocessamento
bases <- preprocessar_dados(dados)

# Verificar se o retorno da função está correto
if (is.null(bases$treino) || is.null(bases$teste)) {
  stop("Erro: dados de treino ou teste não retornados corretamente.")
} else {
  print("Dados de treino e teste carregados corretamente.")
}

# # Acessando dados de treino e teste

# Definir um novo tamanho balanceado
N_novo <- 2 * min(table(dados_treino$Revenue))

# Aplicar oversampling nos dados de treino
dados_treino <- ovun.sample(Revenue ~ ., data = bases$treino, method = "over", N = N_novo, seed = 42)$data
dados_teste <- bases$teste

# Confirme que os dados estão corretos
print(dim(dados_treino))  # Imprime o tamanho do conjunto de treino
print(dim(dados_teste))   # Imprime o tamanho do conjunto de teste
print(table(dados_treino$Revenue))
print(table(dados_teste$Revenue))


# Treinando e avaliando os modelos:

# # # SVM - Parece OK
# print("Treinando Modelo SVM:")
# resultado_svm <- treinar_svm(dados_treino, dados_teste)
# print("Resultados SVM:")
# print(resultado_svm$avaliacao)

# # # Random Forest - Parece OK
# print("Treinando Modelo Random Forest:")
# resultado_rf <- treinar_random_forest(dados_treino, dados_teste)
# print("Resultados Random Forest:")
# print(resultado_rf$avaliacao)
# print(resultado_rf$importancia)

# # # Regressão Logística - Dados muitos desbalanceados 
# O aviso "glm.fit: fitted probabilities numerically 0 or 1 occurred" indica separação completa nos dados, ou seja, algumas combinações de variáveis explicativas predizem perfeitamente o target (Revenue). Isso faz com que os coeficientes fiquem muito grandes e as probabilidades fiquem muito próximas de 0 ou 1, causando instabilidade no modelo de regressão logística. Sei oque fazer
# print("Treinando Modelo Regressão Logística:")
# resultado_rl <- treinar_regressao(dados_treino, dados_teste)
# print("Resultados Regressão Logística:")
# print(resultado_rl$avaliacao)
# print(resultado_rl$coeficientes)

# # # LightGBM - Parece OK
# print("Treinando Modelo LightGBM:")
# resultado_lgb <- treinar_lightgbm(dados_treino, dados_teste)
# print("Resultados LightGBM:")
# print(resultado_lgb$avaliacao)