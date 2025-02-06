input_path = "D:/FCDados/[18] - Mini Projeto 2/[01] - InputData/"
output_path = "D:/FCDados/[18] - Mini Projeto 2/[02] - OutputData/"
projeto_path = "D:/FCDados/[18] - Mini Projeto 2/[03] - Projetos/"
setwd(projeto_path)
getwd

# Pacote para calcular moda
#install.packages("DescTools")
#library(DescTools)
#Mode(dados2$AGE)
library("dplyr")

setwd(input_path)
dataset = read.csv("dataset.csv")
View(dataset)

?rename_with
renomear <- c("Idade", "Mulher", "TempoInternacao", "Raca", "Custo", "Grupo")
colnames(dataset) <- renomear

summary(dataset)
dataset_na_filter <- na.omit(dataset)

#### ETAPA 1 

# Exercicio 1 - Quantas raças estão representadas no dataset?
length(unique(dataset_na_filter$Raca))

Exerc1 <- n_distinct(dataset_na_filter$Raca)
Exerc1

# Exercicio 2 - Qual a idade média dos pacientes?
mean(dataset_na_filter$Idade)

Exerc2 <- dataset_na_filter %>%
          select(Idade) %>%
          summarise(Media = mean(Idade))
Exerc2

# Exercicio 3 - Qual a moda da idade dos pacientes?
Exerc3 <- dataset_na_filter %>%
          group_by(Idade) %>%
          summarise(count = n()) %>%
          arrange(desc(count)) %>%
          first
          
Exerc3[1]

# Exercicio 4 - Qual a variância da coluna idade?
Exerc4 <- dataset_na_filter %>%
          select(Idade) %>%
          var()
Exerc4

# Exercicio 5 - Qual o gasto total com internações hospitalares por idade?
Exerc5 <- dataset_na_filter %>%
          select(Custo, Idade) %>%
          group_by(Idade) %>%
          summarise(Total = sum(Custo))
Exerc5

# Exercicio 6 - Qual idade gera o maior gasto total com internações hospitalares?
Exerc6 <- dataset_na_filter %>%
          select(Custo, Idade) %>%
          group_by(Idade) %>%
          summarise(Total = sum(Custo)) %>%
          arrange(desc(Total)) %>%
          first
Exerc6

# Exercicio 7 - Qual o gasto total com internações hospitalares por gênero?
Exerc7 <- dataset_na_filter %>%
          select(Custo, Mulher) %>%
          group_by(Mulher) %>%
          summarise(Total = sum(Custo))
Exerc7
 
# Exercicio 8 - Qual a média de gasto com internações hospitalares por raça do paciente?
Exerc8 <- dataset_na_filter %>%
          select(Custo, Raca) %>%
          group_by(Raca) %>%
          summarise(Media = mean(Custo))
Exerc8

# Exercicio 9 - Para  pacientes  acima  de  10  anos,  qual  a  média  de  gasto  total  com  internações hospitalares?
Exerc9 <- dataset_na_filter %>%
          select(Custo, Idade) %>%
          filter(Idade > 10) %>%
          group_by(Idade) %>%
          summarise(Media = mean(Custo))
Exerc9

# Exercicio 10 - Considerando o item anterior, qual idade tem média de gastos superior a 3000?
Exerc10 <- dataset_na_filter %>%
            select(Custo, Idade) %>%
            filter(Idade > 10) %>%
            group_by(Idade) %>%
            summarise(Media = mean(Custo)) %>%
            filter(Media > 3000)
Exerc10

### ETAPA 2

# Exercicio 1 - Qual a distribuição da idade dos pacientes que frequentam o hospital?

hist(dataset_na_filter$Idade, main="Histograma de Idade", xlab="Idade", ylab="Frequência", col="blue")


# Exercicio 2 - Qual faixa etária tem o maior gasto total no hospital?

# Calcular o custo médio por faixa etária
custo_total_por_faixa <- tapply(dataset_na_filter$Custo, dataset_na_filter$Idade, sum)
custo_total_por_faixa

# Encontrar a faixa etária com o maior custo médio
faixa_max_custo <- which.max(custo_total_por_faixa)

# Imprimir o resultado
custo_total_por_faixa
faixa_max_custo


# Exercicio 3 - Qual grupo baseado em diagnóstico (Aprdrg) tem o maior gasto total no hospital?

# Calcular o custo médio por faixa etária
custo_total_por_Grupo <- tapply(dataset_na_filter$Custo, dataset_na_filter$Grupo, sum)
custo_total_por_Grupo
# Encontrar a faixa etária com o maior custo médio
grupo_max_custo <- which.max(custo_total_por_Grupo)

# Imprimir o resultado
custo_total_por_Grupo
grupo_max_custo


# Exercicio 4 - A raça do paciente tem relação com o total gasto em internações no hospital?

custo_total_por_Raca <- tapply(dataset_na_filter$Custo, dataset_na_filter$Raca, sum)
custo_total_por_Raca

plot(Custo ~ Raca, data=dataset_na_filter, pch=19, col=c("red", "blue", "green")[dataset_na_filter$Raca])

# Calcular estatísticas descritivas por grupo
summary_por_grupo <- by(dataset_na_filter$Custo, dataset_na_filter$Raca, summary)

# Imprimir as estatísticas descritivas
print(summary_por_grupo)

cor(dataset_na_filter$Custo, dataset_na_filter$Raca)


# Exercicio 5 - A combinação de idade e gênero dos pacientes influencia no gasto total em internações no hospital?

modelo <- lm(Custo ~ Idade + Mulher, data = dataset_na_filter)

# Visualizar os resultados do modelo
summary(modelo)

# Fortemente por Idade e Pouco por Genero


# Exercicio 6 - Como o tempo de permanência é o fator crucial para pacientes internados, 
                # desejamos descobrir se o tempo de permanência pode ser previsto 
                # a partir de idade, gênero e raça.

library("caret")

set.seed(123)

# Amostrar aleatoriamente os dados para o conjunto de teste
dados_modelos <- dataset_na_filter
dados_modelos$TempoInternacao <- as.factor(dados_modelos$TempoInternacao)
summary(dados_modelos)
str(dados_modelos)
dados_teste <- dados_modelos %>% sample_frac(0.2)

# Obter o conjunto de treinamento excluindo as observações do conjunto de teste
dados_treinamento <- dados_modelos %>% anti_join(dados_teste)


library(e1071)
# Treinar um modelo SVM
modelo_svm <- svm(TempoInternacao ~ Idade + Mulher + Raca, data = dados_treinamento, kernel = "linear")

# Visualizar o modelo treinado
print(modelo_svm)

# Fazer previsões
previsoes <- predict(modelo_svm, newdata = dados_teste)

# Exibir as previsões
print(previsoes)


previsoes <- data.frame(observado = dados_teste$TempoInternacao,
                        previsto = predict(modelo_svm, newdata = dados_teste))
previsoes

x <- confusionMatrix(previsoes, dados_teste$TempoInternacao)
x

# Exercicio 7 - Quais variável têm maior impacto nos custos de internação hospitalar?

library("corrplot")
matriz_correlacao <- cor(dataset_na_filter)
corrplot(matriz_correlacao, method = "color")

# Tempo e Grupo
