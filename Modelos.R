# Função SVM
treinar_svm <- function(dados_treino, dados_teste) {
  library(e1071)
  # Treinar o modelo SVM
  modelo_svm <- svm(Revenue ~ ., data = dados_treino, kernel = "linear",
                    cost = 1, scale = FALSE)
  # Fazer previsões
  previsoes <- predict(modelo_svm, newdata = dados_teste)
  # Avaliar o desempenho
  matriz_confusao <- confusionMatrix(previsoes, dados_teste$Revenue)
  # Retornar modelo e avaliação
  return(list(modelo = modelo_svm, avaliacao = matriz_confusao))
}

# Função Random Forest
treinar_random_forest <- function(dados_treino, dados_teste) {
  library(randomForest)
  # Treinar o modelo Random Forest
  modelo_rf <- randomForest(Revenue ~ ., data = dados_treino, ntree = 100,
                            mtry = sqrt(ncol(dados_treino) - 1),
                            importance = TRUE)
  # Fazer previsões
  previsoes <- predict(modelo_rf, newdata = dados_teste)
  # Avaliar o desempenho
  matriz_confusao <- confusionMatrix(previsoes, dados_teste$Revenue)
  # Obter as variáveis mais importantes
  importancia_variaveis <- importance(modelo_rf)
  # Retornar modelo, avaliação e importância das variáveis
  return(list(modelo = modelo_rf, avaliacao = matriz_confusao,
              importancia = importancia_variaveis))
}

# Função Regressão Logística
treinar_regressao <- function(dados_treino, dados_teste) {
  # Treinar o modelo de Regressão Logística
  modelo_rl <- glm(Revenue ~ ., data = dados_treino, family = binomial)
  
  # Fazer previsões (probabilidades)
  prob_previsoes <- predict(modelo_rl, newdata = dados_teste, type = "response")
  
  # Converter probabilidades para classes (0 ou 1)
  previsoes <- ifelse(prob_previsoes > 0.5, 1, 0)
  previsoes <- factor(previsoes, levels = c(0, 1))  # Garantir os mesmos níveis da variável original
  
  # Garantir que a variável de teste também tenha os mesmos níveis
  dados_teste$Revenue <- factor(dados_teste$Revenue, levels = c(0, 1))
  
  # Avaliar o desempenho
  matriz_confusao <- confusionMatrix(previsoes, dados_teste$Revenue)
  
  # Retornar o modelo, a matriz de confusão e os coeficientes
  return(list(modelo = modelo_rl, avaliacao = matriz_confusao, coeficientes = coef(modelo_rl)))
}

# Função para treinar XGBoost
treinar_xgboost <- function(dados_treino, dados_teste) {
  library(xgboost)
  library(caret)
  
  # Certifique-se de que os dados de treino e teste sejam numéricos
  dados_treino_numeric <- dados_treino[, -which(names(dados_treino) == "Revenue")]
  dados_teste_numeric <- dados_teste[, -which(names(dados_teste) == "Revenue")]
  
  # Converter as colunas para o formato numérico (se necessário)
  dados_treino_numeric <- as.data.frame(lapply(dados_treino_numeric, as.numeric))
  dados_teste_numeric <- as.data.frame(lapply(dados_teste_numeric, as.numeric))
  
  # Converter os dados para o formato adequado para o XGBoost
  dtrain <- xgb.DMatrix(data = as.matrix(dados_treino_numeric), label = as.numeric(dados_treino$Revenue) - 1)
  dtest <- xgb.DMatrix(data = as.matrix(dados_teste_numeric), label = as.numeric(dados_teste$Revenue) - 1)
  
  # Definir os parâmetros do modelo (remover nrounds de dentro de 'params')
  params <- list(
    objective = "binary:logistic", 
    eval_metric = "logloss", 
    eta = 0.1, 
    max_depth = 6
  )
  
  # Treinar o modelo
  modelo_xgb <- xgboost(params = params, data = dtrain, nrounds = 100, verbose = 0)
  
  # Fazer previsões
  previsoes_prob <- predict(modelo_xgb, dtest)
  
  # Converter probabilidades para classes
  previsoes <- ifelse(previsoes_prob > 0.5, 1, 0)
  previsoes <- factor(previsoes, levels = c(0, 1))  # Garantir os mesmos níveis da variável original
  
  # Garantir que a variável de teste também tenha os mesmos níveis
  dados_teste$Revenue <- factor(dados_teste$Revenue, levels = c(0, 1))  # Ajuste os níveis de 'Revenue'
  
  # Avaliar o desempenho
  matriz_confusao <- confusionMatrix(previsoes, dados_teste$Revenue)
  
  # Retornar o modelo e a avaliação
  return(list(modelo = modelo_xgb, avaliacao = matriz_confusao))
}


# Função para treinar LightGBM
treinar_lightgbm <- function(dados_treino, dados_teste) {
  library(lightgbm)
  
  # Converter os dados para o formato adequado para o LightGBM
  dtrain <- lgb.Dataset(data = as.matrix(dados_treino[, -which(names(dados_treino) == "Revenue")]), label = as.numeric(dados_treino$Revenue) - 1)
  dtest <- as.matrix(dados_teste[, -which(names(dados_teste) == "Revenue")])
  label_test <- as.numeric(dados_teste$Revenue) - 1
  
  # Definir os parâmetros do modelo
  params <- list(
    objective = "binary", 
    metric = "binary_error", 
    learning_rate = 0.1, 
    max_depth = 6, 
    num_leaves = 31,
    num_iterations = 100
  )
  
  # Treinar o modelo
  modelo_lgb <- lgb.train(params = params, data = dtrain, nrounds = 100)
  
  # Fazer previsões
  previsoes_prob <- predict(modelo_lgb, dtest)
  
  # Converter probabilidades para classes
  previsoes <- ifelse(previsoes_prob > 0.5, 1, 0)
  previsoes <- factor(previsoes, levels = c(0, 1))  # Garantir os mesmos níveis da variável original
  
  # Avaliar o desempenho
  matriz_confusao <- confusionMatrix(previsoes, factor(label_test, levels = c(0, 1)))
  
  # Retornar o modelo e a avaliação
  return(list(modelo = modelo_lgb, avaliacao = matriz_confusao))
}
