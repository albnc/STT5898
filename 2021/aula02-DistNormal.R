# Normalização e Padronização -----------------------------------------------------------------

x <- rnorm(100, 50, 15)
x

# Normalização [0,1]
x_norm <- (x - min(x)) / (max(x) - min(x))
summary(x_norm)

# Padronizar [-3,3]
x_pad <- (x - mean(x)) / sd(x)
summary(x_pad)

## TABELA
#z = 1,0 ----> p(z) = 0,1587
1 - 2 * 0.1587
#z = 2,0 ----> p(z) = 0,0228
1 - 2 * 0.0228

# p(z) = 95% -----> z(p = 0,025) = ?
1.96

# p(z) = 99% -----> z =??
2.575


# FUNÇÕES DO R --------------------------------------------------------------------------------
# Números aleatórios
rnorm(5, 70, 20)

# Valor z dado a probabilidade da área
qnorm(.90)

# Probabilidade de x
dnorm(1)

# Probabilidade da área dado o valor x
pnorm(1.96)



# EXEMPLO 1: ----------------------------------------------------------------------------------

## P(z > 1,65)
1 - pnorm(1.65)

## P(z < 1,65)
pnorm(1.65)

## P(1,40 < z < 1,70)
pnorm(1.7) - pnorm(1.4)


# DISTRIBUIÇÕES -------------------------------------------------------------------------------
hist(runif(10000))

hist(rnorm(10000), breaks = seq(-5,5,.1),
     freq = FALSE)



# EXEMPLO 2 -----------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)

x = c(58,78,84,90,97,70,
         90,86,82,59,90,70,
         74,83,90,75,88,84,
         68,96,70,94,70,110,
         67,68,75,80,68,82,
         104,92,112,84,98,80)


## Visual
summary(x)

hdados <- hist(x, 
     breaks = seq(40,140,10))

hdados$breaks
hdados$counts
hdados$density


# TESTES DO R ---------------------------------------------------------------------------------
x_pad <- (x - mean(x))/sd(x)


chisq.test(x, rnorm(36, mean(x), sd(x)) )


ks.test(x, "pnorm", mean(x), sd(x))
ks.test(x_pad, "pnorm")

shapiro.test(x)
