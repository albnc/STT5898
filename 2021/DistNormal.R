#' ---
#' title: "Distribuição Normal"
#' subtitle: "Aula 2"
#' author: "Prof. André Luiz Cunha"
#' date: "21/05/2021"
#' output:
#'   pdf_document:
#'     keep_tex: true
#'     number_sections: true
#' ---
#'
#' # Transformação de escala
#' Um passo importante de qualquer análise de dados é a uniformização do intervalo de dados,
#' de modo que todas as variáveis do banco de dados tenham o mesmo intervalo de variação. 
#' Na literatura temos dois tipos de transformação
#' 

# Conjunto de valores aleatórios com média 50 e desvio 15.
(x <- rnorm(100, 50, 15))
summary(x)

#' ## Normalização [0,1]
x_norm <- (x - min(x)) / (max(x) - min(x))
summary(x_norm)

#' ## Padronizar [-3,3]
x_pad <- (x - mean(x)) / sd(x)
summary(x_pad)

#' A padronização dos dados nada mais é do que a transformação para a escala da curva
#' normal padrão (z-padrão). Vide Figura 1 a tabela z-padrão.
#' 
#' ![Tabela Z-padrão](normal-table.png)
#' 
## OLHANDO A TABELA
#z = 1,0 ----> p(z) = 0,1587
1 - 2 * 0.1587
#z = 2,0 ----> p(z) = 0,0228
1 - 2 * 0.0228

# p(z) = 95% -----> z(p = 0,025) = ?
1.96

# p(z) = 99% -----> z =??
2.575

#' # Funções do R
#' ## Números aleatórios
## Uniformemente distribuídos
#' Função: `runif(n, min, max)`

runif(10)
runif(10, 100, 150)


## Normalmente distribuídos
#' Função: `rnorm(n, mean, sd)`

rnorm(10)
rnorm(10, 100, 15)

#' ## Distribuição Normal
#' Encontrando o valor z-padrão com a função `qnorm(area da curva, mean=0, sd=1)`
#' 
#' - Unicaudal a esquerda: $z_\alpha = qnorm(\alpha)$
#' - Unicaudal a direita: $z_\alpha = qnorm(1 - \alpha)$
#' - Bicaudal: $z_frac{\alpha}{2} = qnorm(1 - \alpha/2)$

qnorm(.90)
qnorm(.5)

#' Encontrando o p-valor com a função `pnorm(valor z, mean=0, sd=1)`
#' 
#' - Unicaudal a esquerda: $p-value = pnorm(z, lower.tail=TRUE)$
#' - Unicaudal a direita: $p-value = pnorm(z, lower.tail=FALSE)$
#' - Bicaudal: $p-value = 2 * pnorm(abs(z), lower.tail=FALSE)$

pnorm(1.96)
pnorm(1.96, lower.tail = FALSE)
pnorm(0)

#' Encontrando a densidade do valor com a função `dnorm(valor z, mean=0, sd=1)`
#' 

dnorm(1.96)
dnorm(-1.96)
dnorm(0)

#' **EXEMPLO 1**
## P(z > 1,65)
pnorm(1.65, lower.tail = FALSE)

## P(z < 1,65)
pnorm(1.65)

## P(1,40 < z < 1,70)
pnorm(1.7) - pnorm(1.4)


#' ## Histogramas
#+ fig.width=5
hist(runif(10000))

#+ fig.width=5
hist(rnorm(10000), breaks = seq(-5,5,.1),
     freq = FALSE)

#' **EXEMPLO 2**
x = c(58,78,84,90,97,70,
      90,86,82,59,90,70,
      74,83,90,75,88,84,
      68,96,70,94,70,110,
      67,68,75,80,68,82,
      104,92,112,84,98,80)


## Análise descritiva
summary(x)

#+ fig.width=5
hdados <- hist(x, 
               breaks = seq(40,140,10))

hdados$breaks
hdados$counts
hdados$density

#' O parâmetro `density` traz a razão entre a porcentagem de elementos e o intervalo de bins, tanto
#' que a soma das porcentagens `density` é igual a  
{{ sum(hdados$density) }}
#. 

sum(hdados$density)

#'Ao multiplicar cada densidade pelo intervalo do bin, a porcentagem total será de 100%.

sum(hdados$density) * 10

#' # Testes do R
x_pad <- (x - mean(x))/sd(x)

#' ## Qui-quadrado
#' 
chisq.test(x, rnorm(36, mean(x), sd(x)) )

#' ## Kolmogorv-Smirnov (KS)
#' 
ks.test(x, "pnorm", mean(x), sd(x))
ks.test(x_pad, "pnorm")

#' ## Shapiro-Wilk
#' 
shapiro.test(x)
