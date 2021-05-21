library(ggplot2)
library(dplyr)

# Frequencies -----------------------------------------------------------------------
x=c(1,1,2,3,4,5)
hist(x, breaks = c(1,2,3,4,5))
h <- hist(x, breaks = c(1,2,3,4,5), freq = FALSE)

h$counts
h$density
h$mids


# Random variables ------------------------------------------------------------------
runif(10)
hist(runif(10000), freq = FALSE)

rnorm(10)
hist(rnorm(10000), freq = FALSE)


# Normal distribution ---------------------------------------------------------------

## PI value
sample <- 1000
df <- tibble(
  x = runif(sample),
  y = runif(sample),
  dist = x^2+y^2
)

ggplot(df, aes(x,y)) +
  geom_point() +
  geom_point(aes(color=dist<1))

prob <- df %>% 
  filter(dist<1) %>% 
  summarise(n = n())

prob$n * 4 / sample

## Normal table

# quantile
qnorm(.5)
# distribution function
pnorm(0)
# density
dnorm(0)

