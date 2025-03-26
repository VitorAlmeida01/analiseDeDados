# Convênio de Saúde “Fique em Paz”
# cerca de 32% atendimento na rede de pronto socorro por problemas respiratórios.
# previsão de cerca de 4 semanas para uma onda de calor na cidade de são paulo.
# faixa etária desses pacientes estão entre 7 a 17 e 55 a 82 anos.
# elabore a simulação, criando um modelo de distribuição normal para este cenário,
# com o objetivo de responder as perguntas dos executivos que administram o hospital.

# 1- Qual a média de idade com maior número de atendimentos médicos?

# 2- Quais faixas etárias necessitam de maior número de atendimentos médicos?

# 3- Qual ala, os hospital deve priorizar o atendimento, jovens ou idosos?

# 4- Qual o percentual da ala que precisa ser priorizada?

# 5– Todas as perguntas acima devem ser justificadas por meio de gráficos.

# 6– Elabore uma apresentação para que os executivos do Convênio, possam compreender os insights elaborados

#A Simulação deve ser feita com a linguagem R. 

#Para realizar esta atividade a dupla deve entrar em acordo e defender sua simulação.

#Troque sua apresentação com a de outro grupo depois que estiver finalizada. 
# O grupo que defendeu o projeto deverá contra argumentar o projeto do outro grupo,
# e o grupo que recebeu o contra argumento, além de defender o seu projeto, irá contra argumentar o grupo.

#Boa sorte!

p <- 189000
atendimentos_percent <- 0.32

set.seed(123)
n <- round(p * atendimentos_percent)  # 32% dos conveniados sendo atendidos

# Gera idades medias de 12 anos, com desvio padrão de 3.
# deixando os valores entre 7 a 17 anos.
idade_jovens <- round(rnorm(n * 0.5, mean = 12, sd = 3))
idade_jovens <- pmax(7, pmin(17, idade_jovens))# De 7 a 17 anos

# Gera idades medias de 68 anos, com desvio padrão de 6.
# deixando os valores entre 55 a 82 anos.
idade_idosos <- round(rnorm(n * 0.5, mean = 68, sd = 6))  # De 55 a 82 anos
idade_idosos <- pmax(55, pmin(82, idade_idosos))

# Unindo todos os dados
idade <- c(idade_jovens, idade_idosos)

# Criando um dataframe
df <- data.frame(idade)

# 1. média de idade com maior número de atendimentos?
media_idade <- mean(df$idade)
moda_idade <- as.numeric(names(which.max(table(df$idade))))
cat("Média de idade dos atendimentos:", media_idade, "\n")
cat("Idade mais frequente:", moda_idade, "\n")

# 2. Quais faixas etárias necessitam de maior número de atendimentos médicos?
hist(df$idade,
     main="Distribuição de Idades dos Atendidos",
     xlab="Idade", 
     ylab="Frequência", 
     col="lightblue", 
     breaks=20)

# 3. Qual ala, jovens ou idosos, deve ser priorizada?
tabela_faixa <- table(cut(df$idade, breaks = c(6, 17, 54, 82),
                          labels = c("Jovens (7-17)", 
                                     "Adultos (18-54)", 
                                     "Idosos (55-82)")))

print(tabela_faixa)
hist(tabela_faixa,
     main="Atendimentos por Faixa Etária",
     col=c("blue", "gray", "red"), 
     ylab="Número de Atendimentos")

# 4. Qual o percentual da ala que precisa ser priorizada?
percent_jovens <- tabela_faixa[1] / sum(tabela_faixa) * 100
percent_idosos <- tabela_faixa[3] / sum(tabela_faixa) * 100
cat("Percentual de jovens atendidos:", percent_jovens, "%\n")
cat("Percentual de idosos atendidos:", percent_idosos, "%\n")

# 5. Todas as perguntas acima devem ser justificadas por meio de gráficos.
# (Os gráficos já foram gerados acima)
