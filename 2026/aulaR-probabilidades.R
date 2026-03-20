# 
#  AULA 4 - PROBABILIDADES
# 


# INTRODUCTION ------------------------------------------------------------------------------------------------------
sample(1:7)
set.seed(1)
sample(1:7)
sample(1:7, 5)
sample(1:7, 5, replace = TRUE)
sample(1:7, 5, prob = (1:7)/28)
globe = c("water", "land")
sample(globe, 10, replace = TRUE, prob = c(0.3, 0.7))



# EXAMPLE 1 v.1 ---------------------------------------------------------------------------------------------------
## Create data frame
bagagem <- data.frame(entrega = c(rep('Normal', 6500), rep('Defeito', 350)), 
                      empresa = c(rep('X', 500), rep('Y', 4500), rep('Z', 1500),
                                  rep('X', 30), rep('Y', 270), rep('Z', 50)))
length(bagagem)
dim(bagagem)
nrow(bagagem)
ncol(bagagem)

## Frequencies
(tbl <- table(bagagem))
table(bagagem$empresa)
table(bagagem$entrega)

## Proportions
prop.table(tbl)
prop.table(tbl, 1)
prop.table(tbl, 2)

proportions(tbl)
proportions(tbl,1)
proportions(tbl,2)

# EXAMPLE 1 v.2 ---------------------------------------------------------------------------------------------------
#df <- data.frame(empresa = rep(c('X', 'Y', 'Z'),2),
#                 entrega = c(rep('Normal', 3), rep('Defeito',3)),
#                 freq    = c(500,4500,1500,30,270,50)
#)
df <- as.data.frame.table(tbl)
df

tbl_df <- xtabs(Freq ~ entrega + empresa, df)
tbl_df

proportions(tbl_df)
## P( entrega | empresa )
proportions(tbl_df, 1)
df_prob <- as.data.frame.table(proportions(tbl_df, 1))
df_prob[df_prob$entrega == 'Defeito',]
df_prob[df_prob$entrega == 'Normal',]

## P( empresa | entrega )
proportions(tbl_df, 2)
df_prob <- as.data.frame.table(proportions(tbl_df, 2))
df_prob[df_prob$empresa == 'X',]
df_prob[df_prob$empresa == 'Y',]
df_prob[df_prob$empresa == 'Z',]


# EXAMPLE 2 -------------------------------------------------------------------------------------------------------
## Emissão de poluentes:
## CARRO : regular, excesso
## TESTE : negativo (não excesso), positivo (excesso)
##
## P(carro = 'excesso') = 25%
## P(teste = 'positivo' | carro = 'excesso') = 99%
## P(carro = 'regular') = 75%
## P(teste = 'positivo' | carro = 'regular') = 17% 
##
## P(carro = 'excesso' | teste = 'positivo' ) = ???
## P(carro = E | teste = +) = (P(teste = + | carro = E) * P(carro = E)) / P(teste = +)

## P(teste = +) = P(teste = + | carro = E) * P(carro = E) + P(teste = + | carro = R) * P(carro = R)
.25 * .99 + .17 * .75
0.99 * 0.25 / 0.375

df_tbl <- data.frame(veiculo_emissao = rep(c('excesso', 'regular'),1, each=2),
                     teste_emissao = rep(c('positivo', 'negativo'),2),
                     Freq = c(0.99, 0.01, 0.17, 0.83)
                     )
df_tbl$Freq <- df_tbl$Freq * c(0.25, .25, .75, .75)
proportions(xtabs(Freq ~ teste_emissao + veiculo_emissao, df_tbl),2)



# EXEMPLO 3 -------------------------------------------------------------------------
ex3 <- data.frame(rodovia = rep(1:4, 2),
                  trafego = c(rep('congestionado',4), rep('normal',4)),
                  Freq    = c(0.3, 0.2, 0.60, 0.35, 
                              0.7, 0.8, 0.4, 0.65))
ex3$Freq <- ex3$Freq * rep(0.25, 8)
ex3_tbl <- xtabs(Freq ~ trafego + rodovia, ex3)
ex3_tbl
prop.table(ex3_tbl)
prop.table(ex3_tbl,1)
prop.table(ex3_tbl,2)

# EXAMPLE 4 -------------------------------------------------------------------------
df4_prior <- data.frame(local = c('L', 'W'),
                        Freq = c(0.50, 0.50))

df4 <- data.frame(local = c('W', 'W', 'L', 'W', 'W', 'L', 'L'))
df4_tbl <- table(df4)
df4_likelihood <- as.data.frame.table(proportions(df4_tbl))

df4_posterior <- data.frame(local = c('L', 'W'),
                            Freq = (df4_prior$Freq * df4_likelihood$Freq)/sum(df4_prior$Freq * df4_likelihood$Freq))
df4_prior
df4_likelihood
df4_posterior

