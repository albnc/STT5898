# TABLE ---------------------------------------------------------------------------------------
## Using Table
tbl <- as.table(matrix(c(500, 30, 4500, 270, 1500, 50), nrow = 2, byrow = FALSE,
                       dimnames = list(entrega = c("normal", " defeito"),
                                       empresa = c("X", "Y", "Z"))))
tbl
# Proportion Total
prop.table(tbl)
# Proportion by row
prop.table(tbl, 1)
# Proportion by column
prop.table(tbl, 2)



# TIBBLE --------------------------------------------------------------------------------------
library(dplyr)
library(tibble)


data <- tibble(
  empresa = c(rep("X", 500), rep("X", 30), rep("Y", 4500), rep("Y", 270), rep("Z", 1500), rep("Z", 50)),
  entrega = c(rep("normal", 500), rep("defeito", 30), rep("normal", 4500), rep("defeito", 270), rep("normal", 1500), rep("defeito", 50))
)

# Total probability
data %>% 
  group_by(empresa, entrega) %>% 
  summarise(freq = n(), .groups = "drop") %>%  # drop all the groups specified before
  mutate(prop = freq/sum(freq))

# Row probability
data %>% 
  group_by(empresa, entrega) %>% 
  summarise(freq = n()) %>%  # drop all the groups specified before
  mutate(prop = freq/sum(freq))

# Column probability
data %>% 
  group_by(entrega, empresa) %>% 
  summarise(freq = n()) %>%  # drop all the groups specified before
  mutate(prop = freq/sum(freq))



# SAMPLE --------------------------------------------------------------------------------------

x = seq(1,5,.5)
weights = c(1, 4, 2, 6, 1, 4, 2, 1, 1)
?barplot
barplot(weights, names.arg = x)

?hist
sim <-sample(x, 10000, replace = TRUE,prob = weights)
hist(sim , breaks = x, freq = FALSE)
