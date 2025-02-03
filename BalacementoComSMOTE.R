smote_data_balancer <- function(data) {
  if (!require(smotefamily)) install.packages("smotefamily", dependencies=TRUE)
  if (!require(dplyr)) install.packages("dplyr", dependencies=TRUE)
  library(smotefamily)
  library(dplyr)
  
  # Converter Revenue para numérico (0 e 1)
  data$Revenue <- ifelse(data$Revenue == "TRUE", 1, 0)
  
  # Converter variáveis categóricas para numéricas (One-Hot Encoding)
  data_numeric <- model.matrix(~ . -1, data = data)  # Remove intercepto automático
  
  # Aplicar SMOTE
  smote_result <- SMOTE(X = data_numeric[, -ncol(data_numeric)],  # Apenas atributos
                        target = data_numeric[, ncol(data_numeric)],  # Revenue
                        K = 5, dup_size = 2)
  
  # Criar novo dataframe balanceado
  data_balanced <- as.data.frame(smote_result$data)
  
  # Reverter Revenue para fator ("TRUE"/"FALSE")
  data_balanced$Revenue <- as.factor(ifelse(data_balanced$Revenue > 0.5, "TRUE", "FALSE"))
  
  return(data_balanced)
}
