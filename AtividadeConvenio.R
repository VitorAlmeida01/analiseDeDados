p <- 189000
n <- 60480

n17 <- n / 2
n55 <- n / 2

set.seed(1245)

# idadepop <- abs(round(rnorm(n, 37, 7), 0))
# idadepop

idade7a17 <- sample(7:17, n17, replace = TRUE)
idade7a17

idade55a82 <- sample(55:82, n55, replace = TRUE)
idade55a82

idades <- c(idade7a17, idade55a82)
idades

ala_temp <- rbinom(n, 1, 0.30)
ala_temp

ala <- factor(ala_temp,
              levels = c(0,1),
              labels = c("Geral", "Socorro"),
              ordered = TRUE
)
ala

atendimento_temp <- rbinom(n, 1, 0.30)
atendimento_temp

atendimento <- factor(atendimento_temp,
                  levels = c(0,1),
                  labels = c("Não", "Sim"),
                  ordered = TRUE
)
atendimento
# Data frame
df = data.frame(idades,atendimento, atendimento_temp, ala)

View(df)

mediaAtendida <- round(mean(df$atendimento_temp), 2)
# Qual a média de idade com maior número de atendimentos médicos?
mediaAtendida

df_atendidos <- df[df$atendimento_temp == 1, ]
media_idade_a <- round(mean(df_atendidos$idades), 0)
# Quais faixas etárias necessitam de maior número de atendimentos médicos?
media_idade_a

ala_counts <- table(df_atendidos$ala)
ala_counts


# Qual ala, os hospital deve priorizar o atendimento, jovens ou idosos?
df$idades <- as.numeric(df$idades)
tabela_faixa <- table(cut(df$idades, breaks = c(6, 17, 54, 82),
                          labels = c("Jovens (7-17)", 
                                     "Adultos (18-54)", 
                                     "Idosos (55-82)")))
print(tabela_faixa)


# Qual o percentual da ala que precisa ser priorizada?
barplot(as.numeric(ala_counts),
        col = c("blue", "red"),
        main = "Atendimentos por Ala",
        xlab = "Ala",
        ylab = "Quantidade de Atendimentos",
        names.arg = names(ala_counts))

