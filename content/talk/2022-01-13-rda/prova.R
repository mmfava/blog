

# Universidade Estadual do Oeste do Paraná
# Prova 1 - Análise multivariada
# Alunas: Marília Melo Favalesso e Gabriela Medeiros

# Pacotes para as análises
library("grid")
library("tidyr")
library("gridExtra")
library("ggfortify")
library("reshape2")
library("colorspace")
library("plyr")
library("dplyr")
library("ggplot2")
library("corrgram")
library("Rcmdr")
library("vegan")
library("mvnormtest")
library("psych") 

# Os dados no banco de dados "prova" representam uma pesquisa sobre o peso (em kg) de depósitos de 
# de cascas de 28 árvores (árvore de cortiça) em 4 direções: Norte (N), Sul (S), Leste (L) e Oeste
# (O). 

prova=read.csv2(file.choose(),dec=",",header=TRUE,strip.white=TRUE)
prova
attach(prova)
prova1=prova[,-1]
prova1
attach(prova1)

# Estatística descritiva
summary(prova1)
boxplot(prova1, ylab="Peso (Kg)", names=c("Norte", "Leste", "Sul", "Oeste"))
sd(prova1$N)
sd(prova1$S)
sd(prova1$L)
sd(prova1$O)

summary(t(prova1))
boxplot(t(prova1))

par(mfrow=c(2,2))
boxplot(t(prova1$N), main="Norte",xname=c(prova$indivíduo))
boxplot(t(prova1$S), main="Sul", xname=c(prova$indivíduo))
boxplot(t(prova1$L), main="Leste", xname=c(prova$indivíduo))
boxplot(t(prova1$O), main="Oeste", xname=c(prova$indivíduo))
dev.off()

# a. Faça a análise desses dados pela técnica de componentes principais. Interprete os resultados 
# obtidos.

# Idealmente fazer com a matriz de correlação 
cor=cor(prova1)
cor

# Pressupostos

# 1. Correlação 
cor # elementos com correlação > 0.7 (correlação forte)
cor.test(prova1$N, prova1$L) # P < 0,001
cor.test(prova1$N, prova1$S) # P < 0,001
cor.test(prova1$N, prova1$O) # P < 0,001
cor.test(prova1$L, prova1$S) # P < 0,001
cor.test(prova1$L, prova1$O) # P < 0,001
cor.test(prova1$S, prova1$O) # P < 0,001
print(prova1,digits = 2)
pairs(cbind(prova1), pch="+", col="blue")
# As variáveis apresentarão correlação > 0.7 (correlação forte) e significativa entre sí

# 2. Número de observações segundo autores 


# 3. Avaliação do índice KMO
# Critério de Kayser-Meyer-Olkin
# Adequação: > 0,9 = excelente, 0,8-0,9 = Meritória, 0,7-0,8 = Intermediária... <0,5 = Inaceitável
KMO(cor) 
# A adequação amostral é aceitável e meritória (KMO = 0,82). 

# 4. Teste de Bartlett
# Há dependência entre as variáveis (colunas)?
n <- nrow(prova1)
n
p <- ncol(prova1)
p
chi2 <- -(n-1-((2*p+5)/6))*log(det(R))
ddl <- p*(p-1)/2
print(chi2)
print(ddl)
print(pchisq(chi2,ddl,lower.tail=F))
cortest.bartlett(R,n) 
# Resposta: Segundo o teste de Bartlett a aplicação da análise de componentes principais (bem 
#           a análise fatorial) é adequada (qui-quadrado: 131,3517, df = 6, P < 0,001).

# PCA

# componentes principais para o peso das cascas de árvores (kg)
pca=princomp(prova1, cor=T) 
pca
cor(prova1)
# Resultado da análise: Visualização da proporção da variância total explicativa de cada 
# componente principal. 
summary(pca)
# onde:
# standard deviation = autovalor
# proportion of variance = o quanto cada componente explica a variação total dos dados
# cumulative proportion = % acumulada de explicabilidade de todos os fatores

pca$loadings # Coeficientres das combinações lineares das variáveis contínuas

pca$scores # escores padronizados

scores=as.matrix(pca$scores)
scores
plot(pca) # variâncias (y) vs. componentes principais (x)
plot(pca, type="lines", main=NULL) # gráfico de cotovelo - desconsiderar componente 2

# Resultado gráfico da PCA
autoplot(pca, data=prova, label=TRUE,
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 5, xlab="Componente 1",
         ylab="Componente 2")

# RESULTADO: 

# No presente estudo foram definidos 2 componentes principais, sendo o primeiro (Componente 1)
# denominado como "Norte e Sul" (autovalor = 1,90, explicabilidade = 90%) e o segundo componen-
# te denominado como "Leste e Oeste" (autovalor = 0,50, explicabilidade = 0,06%). 
# !! COMPLEMENTAR RESPOSTA- FALAR QUE SOBRE O VALOR NÃO SER ACEITO DO COMPONENTE 2 PELO
#    GRÁFICO DE COTOVELO.

# PCA na bruta - como a prof. passou na aula
eigen(cor(prova1)) # auto-valores e auto-vetores da matriz

cp <- prcomp(prova1, scale = T) # cria os componentes principais usando R de Pearson
cp 
summary(cp)
screeplot(pca)
screeplot(pca, type="lines")
biplot(cp) # Gráfico igual, mas esse saiu um pouco mais feinho
score <- t(cp$rotation[,1]) %*% t(prova1)
cor(as.vector(score), prova1)
dev.off()

# b. Faça a análises desses dados pela técnica de análise fatorial. Interprete os resultados 
# obtidos. 

# Matriz de correlação
cor

# Análise fatorial utilizando a análise de componentes principais
cor
cor=as.matrix(cor)
acpcor <- prcomp(prova1, scale = TRUE)
summary(acpcor)
plot(acpcor, type="lines", main=NULL)
acpcor$rotation
acpcor$sdev

auto=eigen(cor)
auto
auto$vectors

prop_acu=sum(auto$values[1]) / sum(auto$values)
prop_acu

k <- 1
auto$values
auto$vectors
carfat <- sqrt(auto$values[1]) * auto$vectors[,1]
carfat
comum <- carfat^2
comum
vespec <- diag(cor) - comum
estimat <- cbind(comum, vespec, diag(cor))
rownames(estimat) <- colnames(prova1)
colnames(estimat) <- c("Comunalidade", "Variância única", "Variância")
estimat

resid <- cor - (carfat %*% t(carfat) + diag(vespec))
resid

acpcor$loadings # Coeficientres das combinações lineares das variáveis contínuas

acpcor$scores # escores padronizados

k <- 2
carfat <- acpcor$rotation[, 1:k] %*% diag(acpcor$sdev[1:k])
colnames(carfat) <- paste("Fator", 1:k, sep = " ")
carfat
comum <- rowSums(carfat^2)
comum
vespec <- diag(cor) - comum
estimat <- cbind(comum, vespec, diag(cor))
rownames(estimat) <- colnames(prova1)
colnames(estimat) <- c("Comunalidade", "Variância única", "Variância")
estimat

# Rotação Varimax
carfatr=varimax(carfat, normalize = F)
carfatr

# Plot padrão e plot varimax
plot(carfat, pch = 20, col = "red", xlab = "Fator 1", ylab = "Fator 2")
text(carfat, rownames(carfat), adj = 1)
plot(carfatr$loadings, pch = 20, col = "red", xlab = "Fator 1", ylab = "Fator 2")
text(carfatr$loadings, rownames(carfat), adj = 1)

# Script prof Luciana com um único fator
auto=eigen(cor)
auto

prop_acu=sum(auto$values[1]) / sum(auto$values)
prop_acu

carga1 <- sqrt(auto$values[1]) * auto$vectors[,1]
carga1

L <- cbind(carga1)
L

com <- L^2
com

plot(carga1, carga2)

var=varimax(L, normalize = F)
var


# c. Faça a análise de agrupamento das árvores, considerando como medida de dissimilaridade a dis-
# tância euclidiana e o método de ligação do vizinho mais próximo. Interprete os resultados.

# Valores em distância euclidiana
euclidiana=vegdist(prova1, method="euclidian", binary=F)
euclidiana

# plot dos valores
plot(hclust(euclidiana, method="single"))

# nomear cluster
cluster=hclust(euclidiana, "single")
plot(cluster, subtitle=NULL)
range(euclidiana)
plot(cluster, xlab = "Parcelas", ylab = "Similaridade", main = "", 
     hang = -1, sub="")


# coef. cofenetico
cor(euclidiana, cophenetic(cluster)) # 0,78
cor.test(euclidiana, cophenetic(cluster)) # t = 14,09, P < 0,001

# Dendograma com grupos númerados
plot(cluster, xlab = "Árvores (indivíduos)", ylab = "Similaridade", main = "", sub="")
r=rect.hclust(cluster, 4)
dev.off()

# d. Faça análise de agrupamentos das variáveis (direções), considerando como medida de similari-
# dade a correlação linear de Pearson (e de dissimilaridade uma função do coeficiente linear
# de Pearson) e o método de ligação do vizinho mais próximo. Interprete os resultados obtidos
# e compare-os com os resultados obtidos nos itens (a) e (b).

# função do coeficiente linear de Pearson para dissimilaridade
R=cor(prova1) # Medida de similaridade
R
r=as.dist((1-(R^2))) # medida de dissimilaridade (o hclust so utiliza medidas de dissimilaridade)
r
R_=as.dist(R)
R_

# Cluster para r
par(mfrow=c(1,2))
rcluster=hclust(r,"single")
plot(rcluster, xlab = "a)", ylab = "1-R^2", main = NULL, sub="")
boxplot(prova1)
cor(r, cophenetic(rcluster)) # 0,75
cor.test(r, cophenetic(rcluster)) # t = 2,2716, P = 0,08 ## !! dúvida = p não sign.

# Cluster para R 
Rcluster=hclust(R_,"single")
plot(Rcluster, xlab = "b)", ylab = "Coeficiente de Pearson", main = NULL, sub="")

cor(R_, cophenetic(Rcluster)) # 
cor.test(R_, cophenetic(Rcluster)) # r = 0,74, p = 0,0952




