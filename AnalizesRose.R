# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("caret")

# Treinando e avaliando os modelos:

# # # SVM - 
print("Treinando Modelo SVM:")
resultado_svm <- treinar_svm(dados_treino, dados_teste)
print("Resultados SVM:")
print(resultado_svm$avaliacao)

# # # Random Forest - 
print("Treinando Modelo Random Forest:")
resultado_rf <- treinar_random_forest(dados_treino, dados_teste)
print("Resultados Random Forest:")
print(resultado_rf$avaliacao)
print(resultado_rf$importancia)

# # # LightGBM - 
print("Treinando Modelo LightGBM:")
resultado_lgb <- treinar_lightgbm(dados_treino, dados_teste)
print("Resultados LightGBM:")
print(resultado_lgb$avaliacao)