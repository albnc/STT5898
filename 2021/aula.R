library(tibble)
library(dplyr)

bd <- tibble(
  empresas = c(rep("X", 500), rep("X", 30), rep("Y", 4500), rep("Y", 270), 
               rep("Z", 1500), rep("Z", 50)),
  entregas = c(rep("normal", 500), rep("defeito", 30), rep("normal", 4500), 
               rep("defeito", 270), rep("normal", 1500), rep("defeito", 50))
)

# Probabilidade Total
# CTRL + SHIFT + M (then == ENTÃO FAÇA)

bd %>% 
  group_by(empresas, entregas) %>% 
  summarise(freq = n(), .groups = "drop") %>% 
  mutate(prob = freq / sum(freq))


# P(entrega | empresa = X/Y/Z)
bd %>% 
  group_by(empresas, entregas) %>% 
  summarise(freq = n()) %>% 
  mutate(prob = freq / sum(freq))


# P(empresa | entrega = defeito/normal)
bd %>% 
  group_by(entregas, empresas) %>% 
  summarise(freq = n()) %>% 
  mutate(prob = freq / sum(freq))

