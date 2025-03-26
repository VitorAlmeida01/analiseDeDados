p <- 189000
n <- 60480

n17 <- n / 2
n55 <- n / 2

# idadepop <- abs(round(rnorm(n, 37, 7), 0))
# idadepop

idade7a17 <- sample(7:17, n17, replace = TRUE)
idade7a17

idade55a82 <- sample(55:82, n55, replace = TRUE)
idade55a82

idades <- c(idade7a17, idade55a82)

idades

atendimento_temp <- rbinom(n, 1, 0.30)
atendimento_temp

atendimento <- factor(atendimento_temp,
                  levels = c(0,1),
                  labels = c("Não", "Sim"),
                  ordered = TRUE
)
atendimento

df = data.frame(idades,atendimento, atendimento_temp)

View(df)

mediaAtendida <- round(mean(df$atendimento_temp), 2)
mediaAtendida
barplot(table(df$atendimento), 
        col = c("red", "blue"), 
        main = "Distribuição de Atendimentos", 
        xlab = "Atendimento", 
        ylab = "Quantidade de Pessoas")


df_atendidos <- df[df$atendimento_temp == 1, ]
media_idade_a <- round(mean(df_atendidos$idades), 0)
media_idade_a

hist(df_atendidos$idades, 
     breaks = 15, 
     col = "lightblue", 
     main = "Distribuição de Idades dos Atendidos",
     xlab = "Idade",
     ylab = "Frequência")

