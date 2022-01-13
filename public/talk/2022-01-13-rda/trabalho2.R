

# Questão 1.

# Q1.a)

# Construção da tabela de dupla entrada
Q1=matrix(c(114,47,83,32,148,92,128,49,59,165,52,47),nrow=3,byrow=4)
Q1
colnames(Q1)=c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4")
rownames(Q1)=c("Feminino/não trabalha", "Feminino/trabalha", "Masculino/trabalha")
Q1

freq1=prop.table(Q1)*100
freq1 # fazer uma tabela com a frequência absoluta (n - tab. Q1) e a % (tab. freq1)

# Interpretação:
# 


# Q1.b) Construa a matriz P e os vetores r e c.

# soma n total
n=sum(Q1)
n # Foram avaliados 1016 indivíduos 

# construção da matriz P
P=Q1/n 
P # proporção de indivíduos em relação ao total

# Construção do vetor r
r1= sum(Q1[1,])/n 
r2= sum(Q1[2,])/n 
r3= sum(Q1[3,])/n 

r= c(r1,r2,r3)      
r # vetor coluna - proporção de cada categoria automobilistica

# construção do vetor c
c1 = sum(Q1[,1])/n 
c2 = sum(Q1[,2])/n 
c3 = sum(Q1[,3])/n 
c4 = sum(Q1[,4])/n 


c <- c(c1,c2,c3,c4)     
c # vetor linha - proporção de cada categoria de gênero/trabalho

# Q1.c) Qui-quadrado e análise de correspondência: 

# Teste qui-quadrado de independência (alfa = 5%):

# Hipótese do teste:
# H0: Não existe associação entre as variáveis em estudo, ou seja, a frequência observada (O) é 
#     igual a frequência esperada (E) (O=E).
# H1: Existe associação entre as variável em estudo (O é diferente do E).

Q1 # tabela 1 - Tabela de contingência relativa a preferência de indivíduos, por gênero e se
   #            trabalha ou não, por quatro diferentes modelos automobilisticos.
X2=chisq.test(Q1)
X2
X2$residuals # tabela 2 - Tabela com os resíduos ajustados do teste qui-quadrado de independên-
             #            cia (alfa = 5%).

# Houve diferença estatística significativa entre a distribuição das proporções de preferência
# de modelos automobilisticos e os gêneros-trabalho (X²= 119,67;GL = 6; p < 0,001). Houve maior
# proporção de indivíduos que preferem o modelo automobilistico 1 dentro da categoria 
# 'feminino/não trabalha' e maior proporção de indivíduos que preferem o modelo 2 
# dentro da categoria 'masculino/trabalha' (tabela 2).

# Análise de correspondência
require(FactoMineR)
corresp = CA(Q1)      
summary(corresp)
par (cex = 0.7, mar = c (2.5 , 3 , 2 , 0.8 ) + 0.1 )
plot(corresp)
corresp$call
corresp$eig
corresp$col
corresp$row

library(MASS)
r.corresp=corresp(Q1)
r.corresp
biplot(corresp(Q1, nf=2)) # análise gráfica mais bonita *.*

library(ca)
library(vegan)
ca=ca(Q1)
ca
plot(ca)
plot(ca(Q1), mass = TRUE, contrib = "absolute", arrows = c(FALSE, TRUE)) 
round(0.116879+0.000905,4)

# inércia:
# Uma medida relativa de qui-quadrado usado em análise de correspondência. A inércia total de 
# uma tabela de tabulação cruzada é calculada como qui-quadrado total dividido pela frequência
# total (soma de linhas ou colunas). Inércia pode então ser calculado para cada categoria
# de linha ou coluna para representar sua contribuição ao total da inércia. 

# Escalas de similaridade:
# Escala arbitrária, por exemplo, de -5 a +5, que permite a representação de uma relação ordenada
# entre objetos que vai do mais semelhante (mais próximo) ao menos similar (mais distante).
# Esse tipo de escala é adequado apenas para representar uma única dimensão. 

# Massa:
# Uma medida relativa de frequência usada em análise de correspondência para descrever o tama-
# nhode qualquer célula ou categoria em uma tabulação cruzada. É definida como o valor (total
# da celula ou categoria) dividido pela frequência total, gerando o percentual de frequência
# total representado pelo valor. Como tal, a massa total ao longo das linhas, colunas ou todas
# as entradas é 1,0. 

##  Resposta:
# Utilizamos a análise de correspondência (CA) para examinar a relação conjunta entre o gênero-
# trabalho com quatro modelos automobilisticos (apresentados inicialmente na tabela 1). 
# A inércia total dos dados é 0,1178, e as contribuições das inércias por dimensão são 99,23% e 
# 0,77% (total = 100% ) respectivamente (tabela 3). Verifica-se, portanto, que o componente 
# principal 1 (CP 1) contribui com grande parte da inécia total, sendo a única CP aceita e 
# influente (CITAR ALGUÉM),dessa forma exluís-e o CP2 (tabela 3). Também é possível verifi-
# car que a magnitude da associação entre as variáveis é alta (X²=119,6686; GL=6; p<0,001).

# tabela 3 - Inércia (autovalor) de cada componente principal resultante da análise de 
#            correspondência, com respectiva frequência relativa % (FR%) da variância de 
#            cada CP e frequência acumulada (FA%). 
corresp$eig 

# figura 1 - Mapa de correspondência, com as categorias de 'gênero-trabalho' e de 
#            'modelos automobilisticos (inércia total = 0,118).
par (cex = 0.7, mar = c (2.5 , 3 , 2 , 0.8 ) + 0.1 )
plot(corresp)

# Para 'gênero-trabalho', as categorias que mais contribuem com a inércia, são, 
# respectivamente, masculino/trabalha (0,0782), feminino/não trabalha (0,0253) e feminino/
# trabalha (0,0144) (tabela 4). Para 'modelo automobilistico' a contribuição das categorias,
# do maior para o menor são, respectivamente, modelo 2 (0,0609), modelo 1 (0,0236), modelo
# 3 (0,0143) e modelo 4 (0,0012) (tabela 4). Todas as categorias aqui apresentadas possuem um
# um Coseno² > 0,96, indicando qualidade de ajuste da CA (EXPLICAR PQ) (tabela 4).
# Segundo os escores do CP1, há associação entre o modelo automobilistico 1 e a categoria 
# 'feminino/não trabalha' e modelo 2 com a categoria 'masculino/trabalha' (tabelas 4). 
# O modelo 3 esteve próximamente associado com 'femino/não trabalha' e 'feminino trabalha 
# (tabela 4). O modelo 4 não apresentou associação com nenhuma categoria de 'gêneto-trabalho'
# (tabela 4).

# tabela 4 -
corresp$row # tabela 4
corresp$col # tabela 4

# Questão 2. GABI

# Questão 3. 

Q3=read.csv2(file.choose(),h=T,dec=".",sep=",")
Q3
attach(Q3)
names(Q3)

cor(Q3) # Tabela 1 - Matriz de correlação entre as variáveis amostradas.
plot(Q3) # Figura 1 - Diagrama de dispersão entre as variáveis.

shapiro.test(Temp) # P > 0,05
shapiro.test(pH) # P < 0,05
shapiro.test(Condut) # P < 0,05
shapiro.test(Mat_inorg) # P > 0,05
shapiro.test(Mat_org) # P < 0,05
shapiro.test(Dureza) # P < 0,05
shapiro.test(Mat_susp) # P > 0,05

# TESTAR A NORMALIDADE MULTIVARIADA!!!!!1 --> pre-requisito da CCA
library(MVN)
mardiaTest(Q3)

# As variáveis foram separadas em dois grupos

# Grupo 1:
x=cbind(Temp, pH, Mat_inorg, Mat_org, Mat_susp)
x

# Grupo 2:
y=cbind(Condut, Dureza)
y


# As variáveis foram separadas tendo como base as medidas de associação da matriz
# de correlação de Pearson realizada (tabela ). O primeiro grupo foi comsp=

canon=cancor(x,y)
canon
canon$cor # correlação canônica
canon$xcoef # coeficiente estimado para as x variáveis
canon$ycoef # coeficientes estimados para as y variáveis

library(yacca)
cca=cca(x,y)
cca
summary(cca)
cca$corr # Correlação canonica 
cca$corrsq # correlação canonica ao quadrado (R²)
cca$xcoef # coeficientes das variáveis de x para cada variável canônica
cca$ycoef # coeficientes das variáveis de y para cada variável 

# Metodologia:
# Foi realizada a análise de correlação canônica, considerando os dados originais observados,
# para verificar as associações existentes entre um primeiro grupo x com um segugundo grupo y.
# O primeiro grupo representa as variáveis independêntes (x) e o segundo grupo as variáveis
# dependentes (ou resposta y). Dessa forma, foi possível determinar 2 funções canônicas ou 
# 5 pares de variáveis estatísticas canônicas. Foram estimadas as cargas canônicas, ou seja, 
# as correlações entre as variáveis originaise suas respectivas variáveis estatísticas canôni-
# cas, e as cargas canônicas cruzadas que representam a correlação entre a variável original
# de um grupo e a variável estatística canônica do outro grupo. 
# A quantidade de variância explicada, ou seja, o percentual de variância na variável estatís-
# tica canônica dependente que pode ser explicada pela variável estatística canônica independe-
# nte, e vice-versa, foi detertminada elevando-se ao quadrado a correlação canônica (R²). 
# Determinou-se ainda o índice de redundância como sendo a carga canônica quadrada média
# vezes o R² canônico. O índice de redundância expressa a quantidade de variância em uma variá-
# vel estatística canônica (dependente ou independênte) explicada pela outra variável estatís-
# tica canônica.

# Resultados:
# Na tabela 3 encontram-se as correlações canônicas obtidas, o R² canônico e o teste de 
# significância realizado pelo qui-quadrado de Bartlett. Pela tabela é possível observar
# que apenas a função canônica 1 mostrou-se significativa (X²=33,40; P < 0,001) (tabela 3). O
# R² canônico encontrado foi satisfatório (!!), ou seja, a quantidade da variância explicada
# (73%) entre as variáveis estatísticas canônicas x e y da função 1 foi expressiva para os 
# grupos de características analisadas. Isto é um indicativo da influência das características
# de y sobre x.

# Tabela 3 - Correlações canônicas e pares canônicos entre as variáveis dos grupos 
#            x e y.
cca$corr # Correlação canonica 
cca$corrsq # correlação canonica ao quadrado (R²)
cca$xcoef # coeficientes das variáveis de x para cada variável canônica
cca$ycoef # coeficientes das variáveis de y para cada variável 
summary(cca) # --> valores de chi-quadrado do teste de Bartlett
             #     incluir valor de 'chisq', 'df' e 'Pr(>X)'. 
cca$xstructcorr # estrutura de correlação (loadings) das variáveis de x em cada
                # variável canônica.

cca$ystructcorr # estrutura de correlação (loadings) das variáveis de y em cada
                # variável canônica.

cca$xcancom # comunalidades canonicas de x
cca$ycancom # comunalidades canonicas de y




## Questão 3.


shapiro.test(pH2) # P > 0,05
shapiro.test(Condut2) # P < 0,05
shapiro.test(Mat_inorg2) # P > 0,05
shapiro.test(Mat_org2) # P < 0,05
shapiro.test(Dureza2) # P < 0,05
shapiro.test(Mat_susp2)
## TESTE DE SHAPIRO WILKS MULTIVARIDO ##
## cada elemento amostral deve ser uma coluna ##
mshapiro.test(t(tr))

## TESTE DE NORMALIDADE MULTIVARIADO DE SHAPIRO-FRANCIA##
mvsf(t(tr)) 


## Em razão de nenhuma das transformações terem resultado em normalidade dos dados, será realizada
## a análise de correlação canonica utilizando dados de uma matriz de correlação de Spearman. 
cor=cor(Q3, method='spearman') # Tabela 1 - Matriz de correlação não-parámetrica entre as variáveis
cor
c=data.frame(cor)
plot(Q3) # Figura 1 - Diagrama de dispersão entre as variáveis.
pairs(cbind(Q3), pch="+", col="blue")

## usando o pacote CCAP
library(ccaPP)
cc1=ccaGrid(x, y, method="spearman")
cc1$cor # Vetor número contendo as medidas de correlação canonica
cc1$A # Matriz númerica contendo os vetores canonicos para x
cc1$B # Matriz númerica contendo os vetores canonicos para y

ccaProj(x, y, method='spearman', k=2)

maxCorGrid(x,y, method='spearman')


## Análise de correspondência
library(mvnormtest)

## Banco de dados
Q3=read.csv2(file.choose(),h=T,dec=".",sep=",")
attach(Q3)
names(Q3)

## Testando os pressupostos:

## Os dados estão em normalidade?

## Dados brutos
par(mfrow=c(2,4))
shapiro.test(Temp) # P > 0,05
qqnorm(Temp)
qqline(Temp)

shapiro.test(pH) # P < 0,05
qqnorm(pH)
qqline(pH)

shapiro.test(Condut) # P < 0,05
qqnorm(Condut)
qqline(Condut)

shapiro.test(Mat_inorg) # P > 0,05
qqnorm(Mat_inorg)
qqline(Mat_inorg)

shapiro.test(Mat_org) # P < 0,05
qqnorm(Mat_org)
qqline(Mat_org)

shapiro.test(Dureza) # P < 0,05
qqnorm(Dureza)
qqline(Dureza)

shapiro.test(Mat_susp) # P > 0,05
qqnorm(Mat_susp)
qqline(Mat_susp)
dev.off()


# normalidade multivariada
mshapiro.test(t(Q3))

## Tentativas de transformações nos dados para aplicação da correlação canonica
##Transformação logarítmica##
Qlog<-(log10(Q3))
Qlog
shapiro.test(Qlog[,1]) # P < 0,05
shapiro.test(Qlog[,2]) # P < 0,05
shapiro.test(Qlog[,3]) # P < 0,05
shapiro.test(Qlog[,4]) # P < 0,05
shapiro.test(Qlog[,5]) # P < 0,05
shapiro.test(Qlog[,6]) # P < 0,05
shapiro.test(Qlog[,7])# P < 0,05

##Transformação logarítmica log+1##
Qlog1<-(log10(Q3+1))
Qlog1
shapiro.test(Qlog1[,1]) # P < 0,05
shapiro.test(Qlog1[,2]) # P < 0,05
shapiro.test(Qlog1[,3]) # P < 0,05
shapiro.test(Qlog1[,4]) # P < 0,05
shapiro.test(Qlog1[,5]) # P < 0,05
shapiro.test(Qlog1[,6]) # P < 0,05
shapiro.test(Qlog1[,7])# P < 0,05

##Transformação raiz quadrada##
Qraiz<-(sqrt(Q3))
Qraiz
shapiro.test(Qraiz[,1]) # P < 0,05
shapiro.test(Qraiz[,2]) # P < 0,05
shapiro.test(Qraiz[,3]) # P < 0,05
shapiro.test(Qraiz[,4]) # P > 0,05
shapiro.test(Qraiz[,5]) # P > 0,05
shapiro.test(Qraiz[,6]) # P < 0,05
shapiro.test(Qraiz[,7])# P >0,05

##Transformação raiz quadrada Dados + 1##
Qraiz1<-(sqrt(Q3+1))
Qraiz1
shapiro.test(Qraiz1[,1]) # P > 0,05
shapiro.test(Qraiz1[,2]) # P < 0,05
shapiro.test(Qraiz1[,3]) # P < 0,05
shapiro.test(Qraiz1[,4]) # P > 0,05
shapiro.test(Qraiz1[,5]) # P > 0,05
shapiro.test(Qraiz1[,6]) # P < 0,05
shapiro.test(Qraiz1[,7])# P >0,05

##Transformação raiz cubica##
Qraiz3<-(Q3^(1/3))
Qraiz3
shapiro.test(Qraiz3[,1]) # P < 0,05
shapiro.test(Qraiz3[,2]) # P < 0,05
shapiro.test(Qraiz3[,3]) # P < 0,05
shapiro.test(Qraiz3[,4]) # P < 0,05
shapiro.test(Qraiz3[,5]) # P < 0,05
shapiro.test(Qraiz3[,6]) # P < 0,05
shapiro.test(Qraiz3[,7])# P <0,05

##Transformação angular Seno##
Qseno<-(sin(Q3))
Qseno
shapiro.test(Qseno[,1]) # P < 0,05
shapiro.test(Qseno[,2]) # P < 0,05
shapiro.test(Qseno[,3]) # P < 0,05
shapiro.test(Qseno[,4]) # P < 0,05
shapiro.test(Qseno[,5]) # P < 0,05
shapiro.test(Qseno[,6]) # P < 0,05
shapiro.test(Qseno[,7])# P <0,05

##Transformação angular cosseno##
Qco<-(cos(Q3))
Qco
shapiro.test(Qco[,1]) # P < 0,05
shapiro.test(Qco[,2]) # P < 0,05
shapiro.test(Qco[,3]) # P < 0,05
shapiro.test(Qco[,4]) # P < 0,05
shapiro.test(Qco[,5]) # P < 0,05
shapiro.test(Qco[,6]) # P < 0,05
shapiro.test(Qco[,7])# P <0,05 

##Transformação hiperbólica##
Qsenoh<-(sinh(Q3))
Qsenoh
shapiro.test(Qsenoh[,1]) # P < 0,05
shapiro.test(Qsenoh[,2]) # P < 0,05
shapiro.test(Qsenoh[,3]) # P < 0,05
shapiro.test(Qsenoh[,4]) # P < 0,05
shapiro.test(Qsenoh[,5]) # P < 0,05
shapiro.test(Qsenoh[,6]) # P < 0,05
shapiro.test(Qsenoh[,7])# P <0,05


##transformação Box-cox##
library(MASS)
boxcox(Q3[,1]~1, lam = seq(2.2,2.4,1/50)) #transformação dos dados#
boxcox(Q3[,2]~1, lam = seq(8.5,9,1/50))
boxcox(Q3[,3]~1, lam = seq(0.78,0.82,1/50))
boxcox(Q3[,4]~1, lam = seq(0,1,1/50))
boxcox(Q3[,5]~1, lam = seq(0,1,1/50))
boxcox(Q3[,6]~1, lam = seq(1.2,1.4,1/50))
boxcox(Q3[,7]~1, lam = seq(0,1,1/50))
temp2 <- ((Q3[,1]^2.23)-1)/2.23
pH2 <- ((Q3[,2]^8.78)-1)/8.78
Condut2 <- ((Q3[,3]^0.795)-1)/0.795
Mat_inorg2 <- ((Q3[,4]^0.58)-1)/0.58
Mat_org2 <- ((Q3[,5]^0.6)-1)/0.6
Dureza2<- ((Q3[,6]^1.255)-1)/1.255
Mat_susp2<- ((Q3[,7]^0.6)-1)/0.6

tr=c(temp2,pH2, Condut2,Mat_inorg2,Mat_org2,Dureza2,Mat_susp2)
tr=data.frame(tr)

plot(ecdf(temp2))
plot(ecdf(pH2))
plot(ecdf(Condut2))
plot(ecdf(Mat_inorg2))
plot(ecdf(Mat_org2))
plot(ecdf(Dureza2))
plot(ecdf(Mat_susp2))


par(mfrow=c(2,4))
shapiro.test(temp2) # P > 0,05
qqnorm(temp2, main="Temperatura p>0,05")
qqline(temp2)

shapiro.test(pH2) # P > 0,05
qqnorm(pH2, main="pH P>0,05")
qqline(pH2)

shapiro.test(Condut2) # P < 0,05
qqnorm(Condut2, main="Condutividade p<0,05")
qqline(Condut2)

shapiro.test(Mat_inorg2) # P > 0,05
qqnorm(Mat_inorg2,  main="Mat inorg P>0,05")
qqline(Mat_inorg2)

shapiro.test(Mat_org2) # P < 0,05
qqnorm(Mat_org2, main="Matéria orgânica P <0,05")
qqline(Mat_org2)

shapiro.test(Dureza2) # P < 0,05
qqnorm(Dureza2, main= "Dureza P<0,05")
qqline(Dureza2)

shapiro.test(Mat_susp2) # P > 0,05
qqnorm(Mat_susp2, main="Matéria em suspensão P>0,05")
qqline(Mat_susp2)

dev.off()

# Grupo 1:
x1=cbind(Mat_inorg2, Mat_org2, Mat_susp2)
x1

# Grupo 2:
y1=cbind(temp2, pH2, Condut2, Dureza2)
y1

box=c(x1,y1)
## TESTE DE SHAPIRO WILKS MULTIVARIDO ##
## cada elemento amostral deve ser uma coluna ##
mshapiro.test(t(box))
qqplot(x1, y1)

## Em razão de nenhuma das transformações terem resultado em normalidade dos dados, será realizada
## a análise de correlação canonica utilizando dados de uma matriz de correlação de pearson. 

# Matriz de correlação
cor=cor(Q3, method='spearman') # Tabela 1 - Matriz de correlação não-parámetrica entre as variáveis
cor
c=data.frame(cor)
plot(Q3) # Figura 1 - Diagrama de dispersão entre as variáveis.
pairs(cbind(Q3), pch="+", col="blue")

# Grupo 1:
x=cbind(Mat_inorg, Mat_org)
x

# Grupo 2:
y=cbind(Temp, pH, Condut, Dureza,  Mat_susp)
y

Q3=read.csv2(file.choose(),h=T,dec=".",sep=",")
attach(Q3)

boxplot(Mat_inorg)
boxplot(Mat_org)
boxplot(Temp)
boxplot(pH)
boxplot(Condut)
boxplot(Dureza)
boxplot(Mat_susp)

plot(Mat_susp~Mat_inorg)
plot(Mat_susp~Mat_org)


# Grupo 1:
x=cbind(Mat_inorg, Mat_org)
x

# Grupo 2:
y=cbind(Temp, pH, Condut, Dureza, Mat_susp)
y

matriz=data.frame(x,y)
matriz

cor=cor(matriz)

## fazendo a correlação canonica a mão 
cor
r11=cor[1:2, 1:2]
r11

r22=cor[3:7, 3:7]
r22

r12=cor[1:2, 3:7]
r12

# calculando os autovalores e autovetores
A <- (solve(r11)) %*% r12 %*% (solve(r22)) %*% (t(r12))
A

B <- (solve(r22)) %*% (t(r12)) %*% (solve(r11)) %*% r12
B

eigen1 <- eigen(A)
eigen2 <- eigen(B)
eigen1
eigen2

# Autovalores
sqrt(eigen1$values) ## autovalores (valor de correlação canônica)
sqrt(eigen2$values)*100
sum(eigen1$values)

vetores1=eigen1$vectors
rownames(vetores1)=c("Matéria inorgânica", "Matéria orgânica")
vetores1

vetores2=eigen2$vectors
rownames(vetores2)=c("Temperatura", "pH", "Condut", "Dureza", "Matéria suspensão")
vetores2

## Estimativas das variáveis canônicas

## Com dados transformados em Z


Temp.z <- (Temp - mean(Temp))/(sd(Temp))
Temp.z

ph.z <- (pH - mean(pH))/(sd(pH))
ph.z

Condut.z=(Condut- mean(Condut))/(sd(Condut))
Condut.z

Mat_inorg.z=(Mat_inorg- mean(Mat_inorg))/(sd(Mat_inorg))
Mat_inorg.z

Mat_org.z=(Mat_org- mean(Mat_org))/(sd(Mat_org))
Mat_org.z

Dureza.z=(Dureza- mean(Dureza))/(sd(Dureza))
Dureza.z

Mat_susp.z=(Mat_susp- mean(Mat_susp))/(sd(Mat_susp))
Mat_susp.z

names(Q3)
z1 <- as.matrix(cbind(Mat_inorg, Mat_org))
z1

a1 <- as.matrix(eigen1$vectors[,1]) 
rownames(a1)=c("Matéria inorgânica", "Matéria orgânica")
a1 # autovetores 1

u1 <- z1 %*% a1
u1 # 


a2 <- as.matrix(eigen1$vectors[,2]) 
rownames(a2)=c("Matéria inorgânica", "Matéria orgânica")
a2 # autovetores 2

u2 <- z1 %*% a2
u2

z2 <- as.matrix(cbind(Temp, ph, Condut, Dureza, Mat_susp))
z2

b1 <- as.matrix(eigen2$vectors[,1]) 
rownames(b1)=c("Temperatura", "pH", "Condut", "Dureza", "Matéria suspensão")
b1

v1 <- z2 %*% b1
v1

b2 <- as.matrix(eigen2$vectors[,2]) 
rownames(b2)=c("Temperatura", "pH", "Condut", "Dureza", "Matéria suspensão")
b2

v2 <- z2 %*% b2
v2
cor(u1,v1, method='spearman')    
cor(u2,v2, method='spearman')    

a1
b1

a2
b2
par(mfrow = c(1,2))
plot(u1,v1, col="blue")
plot(u2,v2, col="blue")
dev.off()

cor(u1,v1, method='spearman')
cor(u1,v2, method='spearman')
cor(u2,v1, method='spearman')
cor(u2,v2, method='spearman')


## cargas canônicas e cargas canônicas cruzadas

a1
cargas1=matrix(c(cor(u1,Mat_inorg.z, method='spearman'), cor(u1,Mat_org.z, method='spearman')))
cargas1
cargas12=matrix(c(cor(u1,Temp.z, method='spearman'), cor(u1,ph.z, method='spearman'), cor(u1,Condut.z, method='spearman'), cor(u1,Dureza.z, method='spearman'), cor(u1,Mat_susp.z, method='spearman')))
cargas12

b1
cargas2=matrix(c(cor(u2,Mat_inorg, method='spearman'), cor(u2,Mat_org, method='spearman')))
cargas2
cargas21=matrix(c(cor(u2,Temp.z, method='spearman'), cor(u2,ph.z, method='spearman'), cor(u2,Condut.z, method='spearman'), cor(u2,Dureza.z, method='spearman'), cor(u2,Mat_susp.z, method='spearman')))
cargas21


matrixv1=matrix(c(cor(v1,Temp.z, method='spearman'), cor(v1,ph.z, method='spearman'), 
                  cor(v1,Condut.z, method='spearman'), cor(v1,Dureza.z, method='spearman'),
                  cor(v1,Mat_susp.z, method='spearman'), cor(v1,Mat_inorg.z, method='spearman'), 
                  cor(v1,Mat_org.z, method='spearman')))

cargas2=matrix(c(cor(v2,Mat_inorg.z, method='spearman'), cor(v2,Mat_org.z, method='spearman')))
cargas2

cargas21=matrix(c(cor(v2,Temp.z, method='spearman'), cor(v2,ph.z, method='spearman'), cor(v2,Condut.z, method='spearman'), cor(v2,Dureza.z, method='spearman'), cor(v2,Mat_susp.z, method='spearman')))
cargas21

## medida de qualidade do modelo 
p <- 2  ## nº de variáveis do primeiro grupo
q <- 5  ## nº de variáveis do segundo grupo

a1
cor_u1 = (cor(u1, Mat_org.z, method='spearman')^2) + (cor(u1,Mat_inorg.z, method='spearman')^2)
prop_u1 <- 100 * (cor_u1/p)
prop_u1

b1
cor_v1 <- (cor(v1,Temp.z, method='spearman')^2) + (cor(v1,ph.z, method='spearman')^2) + (cor(v1, Condut.z, method='spearman')^2) + (cor(v1, Dureza.z, method='spearman')^2) + (cor(v1, Mat_susp.z, method='spearman')^2)
prop_v1 <- 100 * (cor_v1/p)
prop_v1

a2
cor_u2 = (cor(u2,Mat_inorg.z, method='spearman')^2) + (cor(u2,Mat_org.z, method='spearman')^2) 
prop_u2 <- 100 * (cor_u2/p)
prop_u2

b2
cor_v2 <- (cor(v2,Temp.z, method='spearman')^2) + (cor(v2,ph.z, method='spearman')^2) + (cor(v2,Condut.z, method='spearman')^2) + (cor(v2, Dureza.z, method='spearman')^2)+ (cor(v2, Mat_susp.z, method='spearman')^2)
prop_v2 <- 100 * (cor_v2/p)
prop_v2 <- 100 * (cor_v2/p)
prop_v2

