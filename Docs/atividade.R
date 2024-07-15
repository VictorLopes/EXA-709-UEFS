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
cores <- rainbow(length(tabela_computador))

# Criar o gráfico de barras
barplot(tabela_computador,
              main = "Há quanto tempo o participante utiliza o computador",
              col = cores,
              ylim = c(0, max(tabela_computador) * 1.2),  # Ajustar o limite superior do eixo y para espaço suficiente para os rótulos
              names.arg = names(tabela_computador),  # Incluir os nomes das categorias no eixo x
              xlab = "Tempo de uso do computador",
              ylab = "Frequência")
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
freqTempoConexao <- table(tempo_conectado_internet_diario)
names(freqTempoConexao) <- c("1-3 horas", "3-6 horas", "Acima de 6 horas")

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

#----------------- How much time uses internet x Which device uses ------------------------------------------

attach(dados)

tabela_disp_movel <- table(dados$Qual.o.dispositivo.móvel.que.você.mais.acessa.)
tabela_tempo_conectado <- table(dados$tempo_conectado_internet_diario)

detach(dados)

# ----------------------------------------------------------------------------------
#CODIGO ABAIXO É O ANTIGO, ADICIONAR ACIMA TODOS OS NOSSOS E DIVIDIR EM SEÇÃO
#------------------------------------------------------------------------------------


########################
#Introdu??o ao R Studio#
########################

#######DICAS GERAIS#######

#1-As fun??es em R sempre acompanhada de ().
#2-H? diferen?a entre letras MAI?SCULA e min?scula.
#3-Para visualizar o comando anterior use a tecla da seta para cima (?).
#4-No caso das casas d?cimais, o R usa ponto (.) em vez de virgula (,).

##############################
#Usando o R como calculadora #
##############################

#Soma

3+5

#Subtração 

22-7

#Multiplica??o 

3*8

#Divis?o

246/8

#Pot?ncia

2**3

#Realiza c?lculos mais complexos como uma calculadora (log, seno, cosseno....)

log(22)
2+ 2**3-1


########################
#Criando objetos no  R#
########################

#Para criar o objeto usamos os s?mbolos <- ou  =
  
resultado <-2+ 2**3
resultado

resultado1=2+ 2**3-1
resultado1

##vetor das observa??es

idade <- c(22,10,33,32)
idade
mean(idade)


############################################
#Instala??o de pacotes no ambiente R#
############################################
install.packages('readxl') #importar banco em excel
install.packages('descr') #crosstable e teste qui-quadrado

                 
##################
#Chamadar pacote #
##################                
library(readxl) #importar banco em excel
library(descr)#crosstable

#########################
#Chamar o banco de dados#
#########################                 
#1) Import Dataset

#2) Escolha a extens?o do seu arquivo (Excel, spss, stata, csv, dta...)

#3) V? em Browse

#OBS: Para abrir banco com extens?o csv ou dta n?o ? necess?rio baixar pacote
#library(readxl) #biblioteca necess?ria para abrir o banco em excel
#library(haven) #biblioteca necess?ria para abrir o banco em stata, spss e sas


#######################
#Ver o banco de dados #
#######################

View(dados) # ver o banco de dados
fix(dados)  #alterar valores 
names(dados) # ver o nome das vari?veis que est?o no arquivo

#####################
#Manipular vari?vel #
#####################

dim(dados) #vendo a dimens?o do banco de dados
length(dados)

#dados$nome da vari?vel ver a vari?vel separadamente
dados$DIETA

attach(dados) #Separar a vari?vel do banco de dados
DIETA

#detach(dados) # Tirar a fun??o attach



######################
#Medidas descritivas #
######################

range(PESO) #Valor m?nimo e m?ximo
diff(range(PESO, na.rm=T)) #Amplitude
mean(PESO) #M?dia
median(PESO) #M?diana

sd (PESO) #Desvio padr?o
var(PESO)#Vari?ncia


cv=sd(PESO)/mean(PESO)*100 #coeficiente de varia??o
cv


summary(PESO)

###########################
#TROCAR O NOME DA VARIAVEL#
###########################

names(dados)[8] <- c("Diferenca")
names(dados)[3:4] <- c("PESOINICIAL", "PESOFINAL")
names(dados)
#####################################
# Transformar uma variavel em fator #
#####################################

factor(DIETA)

##Codificando vari?veis num?ricas como categ?ricas.


Social=factor(SOCIAL, levels = c(1,2,3),labels = c("baixa", "media", "alta")) # Colocando labels
Social
###########incluir no banco de dados 
social.new <- matrix(Social,ncol=1)
social.new
dados["social.new"]<-social.new


#################
# Fun??o tapply #
#################

## Agrega os valores de um vetor num?rico segundo os valores de alguma vari?vel categ?rica.

tapply(IDADE, Social,summary) #sumario da idade por tipo de dieta

tapply(IDADE, DIETA,summary) #sumario da idade por tipo de dieta

tapply(IDADE, DIETA ,mean)

tapply(dados$PESOINICIAL, list(IDADE>=28),summary)

attach(dados)
tapply(PESOINICIAL, list(IDADE>=28),summary)

###########################
# Frqu?ncia das vari?veis #
###########################

####Vari?veis  qualitativa####

table(DIETA)
table(Social)
prop.table(table (DIETA))

##cruzando duas vari?veis

table (DIETA,Social)
prop.table(table(DIETA,Social))

crosstab(DIETA,Social, prop.r = TRUE, plot = FALSE)# precisa do pacote descr
crosstab (DIETA,Social,prop.c = TRUE, plot = FALSE)
crosstab (DIETA,Social, prop.t = TRUE,plot = FALSE)

table(DIETA,Social, IDADE)
table(DIETA,Social, IDADE>=28) #GRUPO COM IDADE MAIOR QUE 28
crosstab (DIETA,Social,IDADE>=28, prop.r = TRUE, plot = FALSE)

summary(DIETA,IDADE)



###########################
# Teste qui-quadrado     #
###########################

crosstab (DIETA,Social,expected = FALSE, prop.r = TRUE, 
          chisq = TRUE, fisher = TRUE, plot = FALSE)

crosstab (DIETA,Social,expected = TRUE, prop.r = TRUE,
          chisq = TRUE, fisher = TRUE, plot = FALSE)

chisq.test(DIETA,Social) # N?o sai a tabela, s? o resultado de teste qui-quadrado



############
# Gráficos #
############


pie(table(DIETA), main = "Dieta dos pacientes",labels = c("Dieta 1","Dieta 2"))
x11()
pie(table(SOCIAL), main = "Classe social",labels = c("Baixa","Média","Alta"))
plot(IDADE,Diferenca, main = "Distribuição da idade por diferença do peso.")
hist(IDADE, main = "Histograma da Idade do Aluno.")
    
boxplot(PESOINICIAL~social.new,main = "Classe social pelo peso inicial", xlab="Classe social",ylab="Peso inicial")
boxplot(PESOFINAL~social.new,main = "Classe social pelo peso final", xlab="Classe social",ylab="Peso Final")
    
x11()# visualizando graficos fora da tela
par(mfrow=c(1, 2)) # visualizando graficos em pares
boxplot(PESOINICIAL~DIETA,main = "Dietas pelo peso inicial",names= c("Dieta 1","Dieta 2"), xlab="Dietas",ylab="Peso inicial")
boxplot(PESOFINAL~DIETA,main = "Dietas pelo peso final",names= c("Dieta 1","Dieta 2"), xlab="Dietas",ylab="Peso inicial")
    
    
    
#####################
#   Colocando Cores #
#####################

pie(table(DIETA), main = "Dieta dos pacientes",labels = c("Dieta 1","Dieta 2"),col = c("blue", "red"))
pie(table(SOCIAL), main = "Classe social dos pacientes",labels = c("Baixa","MéDIA","Alta"),col = c("palevioletred1", "sienna","blue"))
plot(IDADE,Diferenca, main = "Distribui??o da idade por diferen?a do peso.", col="blue")
    
    

pie(table(DIETA), main = "DIETAS DOS PACIENTES",labels = c("Dieta 1","Dieta 2"),col = c("blue", "red"))

plot(IDADE,DIFF, main = "Distribui??o da por diferen?a do peso.", col="blue")

hist(IDADE, main = "Histograma da Idade do Aluno.", col="pink")



barplot (table (DIETA,SOCIAL), ylab="Freq.",xlab="Faixa et?ria",ylim=c(0,40),names=c("baixa", "media", "alta"), col=c(4,7,3),beside=TRUE)
legend (6,40, c("Dieta1", "Dieta2"),fill=c(4,7))



x11()
par(mfrow=c(1, 2)) # visualizando graficos em pares
boxplot(PESOINICIAL~DIETA,main = "Dietas pelo peso inicial",names= c("Dieta 1","Dieta 2"), xlab="Dietas",ylab="Peso inicial",col = c("blue", "red"))
boxplot(PESOFINAL~DIETA,main = "Dietas pelo peso final",names= c("Dieta 1","Dieta 2"), xlab="Dietas",ylab="Peso inicial",col = c("blue", "red"))

