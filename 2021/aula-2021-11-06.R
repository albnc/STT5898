## EXEMPLO 1
mu = 100 #km/h
var = 64 #(km/h)^2
n = 12
alpha = .05

# H0: mu = 100
# H1: mu > 100

## Usando a equação
#zp = (x-mu)/sd
zp = qnorm(.05, lower.tail = FALSE)
zp * sqrt(var/n) + mu

# Usando a função do R
qnorm(.05, mu, sqrt(var/n), lower.tail = FALSE)

hist(rnorm(n, mu, sqrt(var/n)))




## POTÊNCIA DO TESTE
# POPULAÇÃO / H1 [literatura/trabalhos anteriores]
mu = 100
sd = 4

# AMOSTRA / H0 [levantamento em campo/laboratório]
n = 12
avg = 105
avgerr = sd / sqrt(n)


# TESTE (manualmente)
alpha = 0.05 
tails = 1 #2
zp = qnorm(alpha/tails, mean = mu, sd = sd, lower.tail = FALSE)

beta = pnorm(zp, avg, avgerr)
(power = 1 - beta)

# TESTE R
power.t.test(n = n, delta = avgerr, sd = sd, sig.level = alpha, alternative = "one.sided" )



## TAMANHO AMOSTRAL
# Amostra aleatória
E0 = 0.01
N = 200

n0 = 1/(E0 ^2)
(n = (N * n0) / (N + n0))



## Seguindo distribuições
alpha = 0.05
tails = 2
sd = 4
E0 = .4
df = 2

# Distribuição Normal
(n_norm = (qnorm(alpha/tails) * sd / E0)^2)

# Distribuição t-Student
(n_student = (qt(alpha/tails, df) * sd / E0)^2)





