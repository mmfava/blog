
### Estatística Multivariada - Trabalho 1 
### Alunas: Gabriela Medeiros e Marília Favalesso

# Questão 1.
# Incluindo a matriz 
matriz=matrix(c(9,-1,-1,5), nrow=2, byrow=2) 
matriz


# a) Matriz transposta (A^t)
transposta=t(matriz)
transposta

# b)

# Matriz em modulo |A|
modulo=abs(matriz)
modulo

# Matriz inversa (A^-1)
inversa=solve(matriz)
inversa
round(inversa,2)

# c)
determinante=det(matriz)
determinante # ou seja, >0

# d) 
vec=eigen(matriz, symmetric=T, only.values=F) # autovalores ($values) e autovetores ($vectors)
vec
round(vec$values,2) # auto-valores
round(vec$vectors,2) # auto-vetores
(-0.9732490)^2 # 0.9472136
(0.2297529)^2 # 0.0527864
0.9472136 + 0.0527864
sqrt(1) # 1 --> Módulo do autovetor 1, ou norma do autovetor 1

# e)
autovalor1=9.236068
autovalor2=4.763932
autovetor1=matrix(c(-0.97, 0.23), nrow=2, byrow=1)
autovetor1
autovetor2=matrix(c(-0.23, -0.97), nrow=2, byrow=1)
autovetor2
t1=t(autovetor1)
t2=t(autovetor2)
decomp=((autovalor1*autovetor1%*%t1)+(autovalor2*autovetor2%*%t2))
round(decomp,2)

# f)
matrizi2=matrix(c(1,0,0,1), nrow=2, byrow=2)
matrizi2  
kronecker(matriz,matrizi2)

# Questão 2.
Q2=read.csv2(file.choose(),dec=".",header=TRUE,strip.white=TRUE)
Q2
Q21=Q2[,-1]
attach(Q21)

# a)
# a.1) Vetor média amostral
summary(Q2)
vetormedia=c(128.03, 17.75, 78.21, 1259, 25.32)
vetormedia  

# a.2) Matriz de variância e covariância
cov(Q21)
var(Q21)
cor(Q21)
cov=data.frame(cov(Q21))

# b)
cor=cor(Q21, method="pearson")

cor.test(x1, x2) # P > 0,05
cor.test(x1, x3) # P > 0,05
cor.test(x1, x4) # P > 0,05
cor.test(x1, x5) # P > 0,05
cor.test(x2, x3) # P > 0,05
cor.test(x2, x4) # P < 0,05**
cor.test(x2, x5) # P < 0,05**
cor.test(x3, x4) # P < 0,05**
cor.test(x3, x5) # P < 0,05**
cor.test(x4, x5) # P < 0,001**

# c)
par(mfrow=c(2,3))
boxplot(x1, xlab="x1", ylab=NULL)
boxplot(x2, xlab="x2", ylab=NULL)
boxplot(x3, xlab="x3", ylab=NULL)
boxplot(x4, xlab="x4", ylab=NULL)
boxplot(x5, xlab="x5", ylab=NULL)
dev.off()

par(mfrow=c(2,3))
hist(x1, xlab="x1", ylab="FR%", main=NULL)
hist(x2, xlab="x2", ylab="FR%", main=NULL)
hist(x3, xlab="x3", ylab="FR%", main=NULL)
hist(x4, xlab="x4", ylab="FR%", main=NULL)
hist(x5, xlab="x5", ylab="FR%", main=NULL)
dev.off()

# d)
plot(Q21)

# e)
shapiro.test(x1) # P > 0,05
shapiro.test(x2) # P > 0,05
shapiro.test(x3) # P > 0,05
shapiro.test(x4) # P > 0,05
shapiro.test(x5) # P > 0,05

# f)
require(mvnormtest)
Q21=as.matrix(Q21)
mshapiro.test(t(Q21)) # P < 0,001**

# 3.
require(MASS)
Q3=read.csv2(file.choose(),dec=".",header=TRUE,strip.white=TRUE)
Q3
attach(Q3)
Q31=Q3[,-1]
Q31
Q31=as.matrix(Q31)
Q31

mshapiro.test(t(Q31))
mshapiro.test(t(log10(Q31)))
mshapiro.test(t(log(Q31)))
mshapiro.test(t(sqrt(Q31)))
mvsf(t(Q31))                  
mvsf(t(log10(Q31)))
mvsf(t(log(Q31)))
mvsf(t(sqrt(Q31)))

fit <- manova(Q31 ~ sexo)
fit
summary(fit)
summary.aov(fit)
summary.manova(fit)
shapiro.test(resid(fit))

library(vegan)
adonis(Q31 ~ sexo, permutations=999)

par(mfrow=c(1,3))
boxplot(comp~sexo, xlab="Comprimento")
boxplot(larg~sexo, xlab="Largura")
boxplot(peso~sexo, xlab="Peso")
dev.off()

