---
 title: "Distribuição Normal"
 subtitle: "Aula 2"
 author: "Prof. André Luiz Cunha"
 date: "21/05/2021"
 header-includes: 
 - \usepackage{caption}
 - \usepackage[portuguese]{babel}
 output:
   pdf_document:
     number_sections: true
 ---



#+ setup, include=FALSE
knitr::opts_chunk$set(echo=TRUE, fig.height = 4)

 # Transformação de escala
 Um passo importante de qualquer análise de dados é a uniformização do intervalo de dados,
 de modo que todas as variáveis do banco de dados tenham o mesmo intervalo de variação. 
 
 Seja o exemplo do conjunto de dados aleatório abaixo, são apresentados dois tipos de 
 transformação encontrados na literatura.

# Conjunto de valores aleatórios com média 50 e desvio 15.
(x <- rnorm(100, 50, 15))
summary(x)

 ## Normalização [0,1]
 A normalização transforma os dados no intervalo entre 0 e 1.
 
x_norm <- (x - min(x)) / (max(x) - min(x))
summary(x_norm)

 ## Padronizar [-3,3]
 A padronização dos dados nada mais é do que a transformação para a escala da curva
 normal padrão (z-padrão). Vide Figura 1 a tabela z-padrão.
 
x_pad <- (x - mean(x)) / sd(x)
summary(x_pad)

#+ ztable, echo=FALSE, fig.cap='Tabela z-padrão', fig.align='center', out.width = '60%'
knitr::include_graphics("normal-table.png")

#+ echo=TRUE
## OLHANDO A TABELA
#z = 1,0 ----> p(z) = 0,1587
1 - 2 * 0.1587
#z = 2,0 ----> p(z) = 0,0228
1 - 2 * 0.0228

# p(z) = 95% -----> z(p = 0,025) = ?
1.96

# p(z) = 99% -----> z =??
2.575

 # Funções do R
 ## Números aleatórios
## Uniformemente distribuídos
 Função: `runif(n, min, max)`

runif(10)
runif(10, 100, 150)

hist(runif(10000))

## Normalmente distribuídos
 Função: `rnorm(n, mean, sd)`

rnorm(10)
rnorm(10, 100, 15)

hist(rnorm(10000), breaks = seq(-5,5,.1),
     freq = FALSE)


 ## Distribuição Normal
 Encontrando o valor z-padrão com a função `qnorm(area da curva, mean=0, sd=1)`
 
 - Unicaudal a esquerda: `z_alpha = qnorm(alpha)`
 - Unicaudal a direita: `z_alpha = qnorm(1 - alpha)`
 - Bicaudal: `z_alpha/2 = qnorm(1 - alpha/2)`

qnorm(.90)
qnorm(.5)

 Encontrando o p-valor com a função `pnorm(valor z, mean=0, sd=1)`
 
 - Unicaudal a esquerda: `p-value = pnorm(z, lower.tail=TRUE)`
 - Unicaudal a direita: `p-value = pnorm(z, lower.tail=FALSE)`
 - Bicaudal: `p-value = 2 * pnorm(abs(z), lower.tail=FALSE)`

pnorm(1.96)
pnorm(1.96, lower.tail = FALSE)
pnorm(0)

 Encontrando a densidade do valor com a função `dnorm(valor z, mean=0, sd=1)`
 

dnorm(1.96)
dnorm(-1.96)
dnorm(0)

 **EXEMPLO 1**
## P(z > 1,65)
pnorm(1.65, lower.tail = FALSE)

## P(z < 1,65)
pnorm(1.65)

## P(1,40 < z < 1,70)
pnorm(1.7) - pnorm(1.4)


 **EXEMPLO 2**
x = c(58,78,84,90,97,70,
      90,86,82,59,90,70,
      74,83,90,75,88,84,
      68,96,70,94,70,110,
      67,68,75,80,68,82,
      104,92,112,84,98,80)


## Análise descritiva
summary(x)

hdados <- hist(x, 
               breaks = seq(40,140,10))

hdados$breaks
hdados$counts
hdados$density

 O parâmetro `density` traz a razão entre a porcentagem de elementos e o intervalo de bins, tanto
 que a soma das porcentagens `density` é igual a  
{{ sum(hdados$density) }}
#. 

sum(hdados$density)

Ao multiplicar cada densidade pelo intervalo do bin, a porcentagem total será de 100%.

sum(hdados$density) * 10

 # Testes do R
x_pad <- (x - mean(x))/sd(x)

 ## Qui-quadrado
 Teste de aderência de Qui-quadrado é usado para compara distribuições observadas com 
 distribuições esperadas em dados discretos (histogramas de frequências). 

## Densidade dos valores X com a curva normal teórica
bin <- 10
hist.real <- hist(x, breaks = seq(40,140,bin), freq=FALSE)
curve(dnorm(x, mean(x), sd(x)), col='darkblue', lw=2, add=TRUE)

hist.real$density
hist.real$counts

## Frequências das distribuições observadas e esperadas
barplot(rbind(hist.real$counts, dnorm(hist.real$mids, mean(x), sd(x))*bin*length(x) ),
        names.arg = hist.real$mids,
        col = c("darkblue", "red"),
        beside = TRUE
)

chisq.test(hist.real$counts, # Frequência observada - dados originais
           p = dnorm(hist.real$mids, mean(x), sd(x))*bin, # Frequência teórica - distribuição normal
           rescale.p = TRUE ) 

 ## Kolmogorv-Smirnov (KS)
 
ks.test(x, "pnorm", mean(x), sd(x))
ks.test(x_pad, "pnorm")

 ## Shapiro-Wilk
 
shapiro.test(x)
