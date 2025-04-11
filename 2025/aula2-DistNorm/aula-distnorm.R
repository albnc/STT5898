set.seed(123)
velocidades <- rnorm(1000, mean=50, sd = 8)

mean(velocidades)
sd(velocidades)

hist(velocidades, 
     probability = TRUE, 
     col = 'lightblue', 
     main="Distribuição de Velocidades na Rua X")
curve(dnorm(x, mean=60, sd=12),
      col='red',
      lwd=2,
      add=TRUE)

shapiro.test(velocidades)

qqnorm(velocidades)
qqline(velocidades, col='red')

t.ks <- ks.test(velocidades, pnorm, mean=mean(velocidades), sd=sd(velocidades))
ks.test(velocidades, pnorm, mean=60, sd=12)


h <- hist(velocidades, probability = TRUE)
h$counts
h$mids
h$breaks

chsq <- chisq.test(h$counts)
chsq$observed
chsq$expected


chsq <- chisq.test(h$counts, p = dnorm(h$mids, mean=mean(velocidades), sd=sd(velocidades)), rescale.p = TRUE)
chsq$observed
chsq$expected


