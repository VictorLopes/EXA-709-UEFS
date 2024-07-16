install.packages('readxl') #importar banco em excel
install.packages('descr') #crosstable e teste qui-quadrado
install.packages('scales')
install.packages('Hmisc') # Para cálculos estatísticos ponderados
install.packages('DescTools')
install.packages('ggplot2')

library(readxl) #importar banco em excel
library(descr)  #crosstable
library(scales) #para formatar porcentagens
library(Hmisc)
library(DescTools)
library(ggplot2)

# Busca os dados
current_directory <- getwd()
caminho_arquivo <- paste(current_directory, "/Docs/dados.csv", sep = "")
dados <- read.csv(caminho_arquivo, header = TRUE, sep = ",", stringsAsFactors = FALSE)

View(dados) # ver o banco de dados
names(dados) # ver o nome das variaveis que estao no arquivo


#------------------------- Uso do computador  ------------------------------
# Criar a tabela de frequências

names(tabela_computador) <-c("menos de 1 ano", "1-3 anos", "4-6 anos", "7-9 anos", "10-12 anos", "mais de 12 anos")
tabela_computador <- table(dados$Há.quanto.tempo.utiliza.computador.)
# Definir uma paleta de cores suficientemente grande
# Definir uma paleta de cores suficientemente grande
cores <- rainbow(length(tabela_computador))

# Ajustar tamanho do dispositivo gráfico
par(pin = c(8, 6))  # pin define o tamanho do gráfico em polegadas (largura, altura)

# Criar o gráfico de barras
barplot(tabela_computador,
              main = "Há quanto tempo o participante utiliza o computador",
              col = cores,
              ylim = c(0, max(tabela_computador) * 1.2),  # Ajustar o limite superior do eixo y para espaço suficiente para os rótulos
              names.arg = names(tabela_computador),  # Incluir os nomes das categorias no eixo x
              xlab = "Tempo de uso do computador",
              ylab = "Frequência",
              cex.names=0.9)

# Adicionar legenda
legend("topright", legend = names(tabela_computador), fill = cores)


#Definir os limites das classes
limites_inf <- c(0, 1, 4, 7, 10, 12)
limites_sup <- c(1,3, 6, 9, 12, 14)


# Calcular os pontos médios das classes
pontos_medios <- (limites_inf + limites_sup) / 2

# soma das frequencias
total <- sum(tabela_computador)

# Calcular a moda
moda <- pontos_medios[which.max(tabela_computador)]
cat("Moda:", moda)


# Calcular a média ponderada
media <- sum(pontos_medios * tabela_computador) / total
cat("Média ponderada:", media)

# Calcular a mediana
freq_acum <- cumsum(tabela_computador)
amplitudes <- limites_sup - limites_inf
emd <- total/2 #elemento mediano
classe_mediana <- which.max(freq_acum >= emd)  # Encontrar a classe mediana
x = (emd - freq_acum[classe_mediana-1])/tabela_computador[classe_mediana]
mediana <- limites_inf[classe_mediana] + (amplitudes[classe_mediana] * x) 
cat("Mediana ponderada:", mediana, "\n")

# Calcular o desvio padrão e variância
somatorio <- sum(tabela_computador * (pontos_medios - media)^2)
variancia <- somatorio/(total - 1)
desvio_padrao <- sqrt(variancia)
cat("Variância:", variancia, "\n")
cat("Desvio padrão:", desvio_padrao, "\n")
#Coeficiente de variacao
coef_variacao <- (desvio_padrao / media) * 100
cat("Coeficiente de variação", coef_variacao, "\n")

#------------------------- Sexo ------------------------------
tabela_sexo <- table(dados$Sexo)
porcentagens <- round(tabela_sexo / sum(tabela_sexo) * 100, 1)

# Plotar gráfico de pizza com porcentagens formatadas
pie(tabela_sexo, main = "Distribuição por Sexo dos participantes", labels = paste(names(tabela_sexo), "\n", porcentagens, "%"), col = c("lightblue", "pink"))
legend("topright", legend = names(tabela_sexo))

#------------------------- Idade -----------------------------
attach(dados)
Idade

# Criar uma tabela de frequências
freq <- table(Idade)

# Substituir os nomes dos valores para versões mais curtas
names(freq) <- c("17-20", "21-30", "31-40", "41-50")  # Ajuste conforme necessário

# Criar histograma
par(mar = c(7, 4, 4, 2) + 0.1)
barplot(freq, main = "Histograma da Idade dos participantes", col = "lightblue", 
        xlab = "Faixas Etárias", ylab = "Frequência", las = 2, cex.names = 0.8)

# Definir os limites das classes
limites_inf <- c(17, 21, 31, 41)
limites_sup <- c(20, 30, 40, 50)

# Calcular os pontos médios das classes
pontos_medios <- (limites_inf + limites_sup) / 2

# soma das frequencias
total <- sum(freq)

# Calcular a moda
moda <- pontos_medios[which.max(freq)]
cat("Moda:", moda)

# Calcular a média ponderada
media <- sum(pontos_medios * freq) / total
cat("Média ponderada:", media)

# Calcular a mediana
freq_acum <- cumsum(freq)
amplitudes <- limites_sup - limites_inf
emd <- total/2 #elemento mediano
classe_mediana <- which.max(freq_acum >= emd)  # Encontrar a classe mediana
x = (emd - freq_acum[classe_mediana-1])/freq[classe_mediana]
mediana <- limites_inf[classe_mediana] + (amplitudes[classe_mediana] * x) 
cat("Mediana ponderada:", mediana, "\n")

# Calcular o desvio padrão e variância
somatorio <- sum(freq * (pontos_medios - media)^2)
variancia <- somatorio/(total - 1)
desvio_padrao <- sqrt(variancia)
cat("Variância:", variancia, "\n")
cat("Desvio padrão:", desvio_padrao, "\n")

#Coeficiente de variacao
coef_variacao <- (desvio_padrao / media) * 100
cat("Coeficiente de variação", coef_variacao, "\n")

#------------------------- Semestre -----------------------------
attach(dados)
Semestre

# Criar uma tabela de frequências
freq <- table(Semestre)
freq

names(freq) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "14")

# Criar histograma
par(mar = c(7, 4, 4, 2) + 0.1)
barplot(freq, main = "Semestre dos participantes", col = "lightblue", 
        xlab = "Semestres", ylab = "Frequência", las = 3, cex.names = 0.8)

# soma das frequencias
total <- sum(freq)
freq
total

# Calcular a moda
moda <- which.max(freq)
cat("Moda:", moda)

perc_maioria = max(freq) / total
cat("% maioria:", perc_maioria)

# Calcular a média ponderada
media <- mean(freq)
cat("Média:", media)

# Calcular a mediana
mediana <- median(freq)
cat("Mediana:", mediana, "\n")

# Calcular o desvio padrão e variância
sd (freq) # Desvio padrão
var(freq) # Variância


cv=sd(freq)/mean(freq)*100 # coeficiente de variação
cv


somatorio <- sum(freq * (pontos_medios - media)^2)
variancia <- somatorio/(total - 1)
desvio_padrao <- sqrt(variancia)
cat("Variância:", variancia, "\n")
cat("Desvio padrão:", desvio_padrao, "\n")

#Coeficiente de variacao
coef_variacao <- (desvio_padrao / media) * 100
cat("Coeficiente de variação", coef_variacao, "\n")

#----------------- Relacao entre idade X Dispositivo Usado------------------------------------------
attach(dados)
Idade
Qual.o.dispositivo.móvel.que.você.mais.acessa.

tabela_idade <- table(Idade)
names(tabela_idade) <- c("17-20", "21-30", "31-40", "41-50")

tabela_dispositivo <- table(Qual.o.dispositivo.móvel.que.você.mais.acessa.)
names(tabela_dispositivo) <- c("celular", "tablet","Computador/notebook")

tablea_evento <- table(Idade, Qual.o.dispositivo.móvel.que.você.mais.acessa.)
def_tabela_evento <- as.data.frame(tablea_evento)
colnames(def_tabela_evento) <- c("Idade", "Dispositivo", "Frequencia")

ggplot(def_tabela_evento, aes(x=Idade, y =Frequencia, fill= Dispositivo)) + 
  geom_bar(stat= "identity", position="dodge")+
  labs(title = "Relação entre idade x Dispositivo Usado",
       x="Faixa Etária",
       y="Frequência",
       fill="Dispositivo")+ theme_minimal()

detach(dados)

#----------------- Relacao entre idade X tempo conexao internet------------------------------------------
attach(dados)
Idade
tempo_conectado_internet_diario

freqIdade <- table(Idade)
names(freqIdade) <- c("17-20", "21-30", "31-40", "41-50")  # Ajuste conforme necessário

freqTempoConexao <- table(tempo_conectado_internet_diario)
names(freqTempoConexao) <- c("1-3 horas", "3-6 horas", "Acima de 6 horas")

tabela_contingencia <- table(Idade, tempo_conectado_internet_diario)
df_tabela_contingencia <- as.data.frame(tabela_contingencia)
colnames(df_tabela_contingencia) <- c("Idade", "TempoConexao", "Frequencia")

ggplot(df_tabela_contingencia, aes(x = Idade, y = Frequencia, fill = TempoConexao)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Relação idade x tempo de conexão diária com a Internet",
       x = "Faixa Etária",
       y = "Frequência",
       fill = "Tempo de Conexão") +
  theme_minimal()
detach(dados)

#---------------------------------------------------------------------------------------

#----------------- Relacao usa internet para trabalho X Trabalha ou não ------------------------------------------
attach(dados)
Semestre
Você.utiliza.a.internet.para.trabalho.


freqSemestre <- table(Semestre)
freqSemestre
names(freqSemestre) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "14")  # Ajuste conforme necessário

freqInternetParaTrabalho <- table(Você.utiliza.a.internet.para.trabalho.)
freqInternetParaTrabalho
names(freqInternetParaTrabalho) <- c("Sim", "Não")

tabela_contingencia <- table(Semestre, Você.utiliza.a.internet.para.trabalho.)
df_tabela_contingencia <- as.data.frame(tabela_contingencia)
colnames(df_tabela_contingencia) <- c("Semestre", "Você.utiliza.a.internet.para.trabalho.", "Frequencia")

ggplot(df_tabela_contingencia, aes(x = Semestre, y = Frequencia, fill = Você.utiliza.a.internet.para.trabalho.)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Relação entre usar a internet para trabalho x Semestre",
       x = "Semestre",
       y = "Frequencia",
       fill = "Você utiliza internet para trabalho?") +
  theme_minimal()
detach(dados)

#---------------------------------------------------------------------------------------
#------------------------- Quanto tempo usa a internet diariamente (horas) -----------------------------
attach(dados)
tempo_conectado_internet_diario

# Calcular a frequência do tempo de conexão
names(freqTempoConexao) <- c("1-3 horas", "3-6 horas", "Acima de 6 horas")
freqTempoConexao <- table(tempo_conectado_internet_diario)

# Transformar a tabela de frequências em um data frame
df_freqTempoConexao <- as.data.frame(freqTempoConexao)
colnames(df_freqTempoConexao) <- c("TempoConexao", "Frequencia")

# Criar o gráfico de barras
ggplot(df_freqTempoConexao, aes(x = TempoConexao, y = Frequencia, fill = TempoConexao)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribuição do Tempo de Conexão Diário com a Internet",
       x = "Tempo de Conexão",
       y = "Frequência") +
  theme_minimal() +
  theme(legend.position = "none")

  # Definir os limites das classes
limites_inf <- c(1, 3, 6)
limites_sup <- c(3, 6, 12)

# Calcular os pontos médios das classes
pontos_medios <- (limites_inf + limites_sup) / 2
cat("Pontos medios:", pontos_medios)

# Calcular a moda
moda <- names(which.max(freqTempoConexao))
cat("Moda:", moda, "\n")

# soma das frequencias
tempoTotal <- sum(freqTempoConexao)

# Calcular a média ponderada
media <- sum(pontos_medios * freqTempoConexao) / tempoTotal
cat("Média ponderada:", media)

# Calcular a mediana
freq_acum <- cumsum(freqTempoConexao)
amplitudes <- limites_sup - limites_inf
emd <- tempoTotal/2 #elemento mediano
classe_mediana <- which.max(freq_acum >= emd)  # Encontrar a classe mediana
x = (emd - freq_acum[classe_mediana-1])/freqTempoConexao[classe_mediana]
mediana <- limites_inf[classe_mediana] + (amplitudes[classe_mediana] * x) 
cat("Mediana ponderada:", mediana, "\n")

# Calcular o desvio padrão e variância
somatorio <- sum(freqTempoConexao * (pontos_medios - media)^2)
variancia <- somatorio/(tempoTotal - 1)
desvio_padrao <- sqrt(variancia)
cat("Variância:", variancia, "\n")
cat("Desvio padrão:", desvio_padrao, "\n")

#Coeficiente de variacao
coef_variacao <- (desvio_padrao / media) * 100
cat("Coeficiente de variação", coef_variacao, "\n")


#----------------- Relação Idade x Ambiente tóxico ------------------------------------------#
attach(dados)
Idade
Você.considera.as.redes.sociais.um.ambiente.tóxico.

freqIdade <- table(Idade)
names(freqIdade) <- c("17-20", "21-30", "31-40", "41-50")  # Ajuste conforme necessário

freqAmbienteToxico <- table(Você.considera.as.redes.sociais.um.ambiente.tóxico.)
freqAmbienteToxico
names(freqAmbienteToxico) <- c("Sim", "Não")

tabela_contingencia <- table(Idade, Você.considera.as.redes.sociais.um.ambiente.tóxico.)
df_tabela_contingencia <- as.data.frame(tabela_contingencia)
colnames(df_tabela_contingencia) <- c("Idade", "Você.considera.as.redes.sociais.um.ambiente.tóxico.", "Frequencia")

ggplot(df_tabela_contingencia, aes(x = Idade, y = Frequencia, fill = Você.considera.as.redes.sociais.um.ambiente.tóxico.)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Relação entre Idade e visão das redes sociais como ambiente tóxico",
       x = "Idade",
       y = "Frequencia",
       fill = "Você considera as redes sociais\num ambiente tóxico?") +
  theme_minimal()
detach(dados)

#----------------- Voce acredita que a internet atrapalha a sua formacao? ------------------------------------------

attach(dados)
tabela_acredita <- table(dados$Você.acredita.que.a.internet.atrapalha.a.sua.formação.)

porcento_acredita <- round(tabela_acredita / sum(tabela_acredita) * 100, 1)

# Plotar gráfico de pizza com porcentagens formatadas
pie(tabela_acredita, main = "Acreditam que a internet atrapalha a formação", labels = paste(names(tabela_acredita), "\n", porcento_acredita, "%"), col = c("lightgreen", "red"))
detach(dados)


# ----------------------------------------------------------------------------------

#----------------- Tempo de uso da internet x Dispositivo utilizado ------------------------------------------

attach(dados)
#--------------------------------------------------------------
tempo_conectado_internet_diario
Qual.o.dispositivo.móvel.que.você.mais.acessa.

tabela_dispositivo <- table(Qual.o.dispositivo.móvel.que.você.mais.acessa.) #FreqIdade
names(tabela_dispositivo) <- c("celular", "tablet","Computador/notebook") 

freqTempoConexao <- table(tempo_conectado_internet_diario)
names(freqTempoConexao) <- c("1-3 horas", "3-6 horas", "Acima de 6 horas")

tabela_contingencia <- table(Qual.o.dispositivo.móvel.que.você.mais.acessa., tempo_conectado_internet_diario)
df_tabela_contingencia <- as.data.frame(tabela_contingencia)
colnames(df_tabela_contingencia) <- c("Dispositivo", "TempoConexao","Frequencia")

ggplot(df_tabela_contingencia, aes(x = TempoConexao, y = Frequencia, fill = Dispositivo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Dispositivo utilizado x tempo de conexão diária com a Internet",
       x = "Tempo de conexão",
       y = "Frequência",
       fill = "Dispositivo") +
  theme_minimal()
#--------------------------------------------------------------
detach(dados)
# ------------------------------------------------------------------------------------------------