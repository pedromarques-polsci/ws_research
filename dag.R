# Packages -----------------------------------------------------------
if(require(dagitty) == F) install.packages('dagitty'); require(dagitty)
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(ggdag) == F) install.packages('ggdag'); require(ggdag)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(latex2exp) == F) install.packages('latex2exp'); require(latex2exp)

theme_set(theme_dag())

# GRAPHICAL REPRESENTATION ------------------------------------------------
mydag <- function(data) {
  data %>% 
    tidy_dagitty() %>% 
    mutate(linetype = ifelse(name %in% c("unitconfounder", "mob"), 
                             "dashed", "solid")) %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
    geom_dag_point() + 
    geom_dag_edges() +
    geom_dag_label_repel(aes(label = label)) +
    geom_dag_text()
}

## Naive GGDAG -----------------------------------------------------------

### Naive 1: Simple DAG --------------------------------------------------
ngg_label <- c(W = "Gasto Social",
           X = "Receita de Commodities",
           P = "Preços de Commodities")

ngg_coord <- list(
  x = c(P = 1, X = 2, W = 3),
  y = c(P = 0, X = 0, W = 0))

ngg <- dagify(W ~ X,
              X ~ P,
              labels = ngg_label,
              coords = ngg_coord)

mydag(ngg)

### Naive 2: Openness --------------------------------------------------
ngg2_label <- c(W = "Gasto Social",
                X = "Receita de Commodities",
                P = "Preços de Commodities",
                O = "Abertura Econômica")

ngg2_coord <- list(
  x = c(P = 1, X = 2, W = 3,
        O = 2),
  y = c(P = 0, X = 0, W = 0,
        O = 1))

ngg2 <- dagify(W ~ X,
              X ~ P + O,
              labels = ngg2_label,
              coords = ngg2_coord)

mydag(ngg2)

## Confounder GGDAG --------------------------------------------------------

### Openess Confounder -----------------------------------------------------
zgg1_label <- c(W = "Gasto Social",
                X = "Receita de Commodities",
                P = "Preços de Commodities",
                O = "Abertura Econômica")

zgg1_coord <- list(
  x = c(P = 1, X = 2, W = 3,
        O = 2),
  y = c(P = 0, X = 0, W = 0,
        O = 1))

zgg1 <- dagify(W ~ X + O,
              X ~ P + O,
              labels = zgg1_label,
              coords = zgg1_coord)

mydag(zgg1)

### Violence Confounder -----------------------------------------------------
zgg1b_label <- c(W = "Gasto Social",
                X = "Receita de Commodities",
                P = "Preços de Commodities",
                O = "Abertura Econômica",
                V = "Violência Política",
                Wt = "Gasto Social (t-2)")

zgg1b_coord <- list(
  x = c(P = 1, X = 2, W = 3,
        O = 2, V = 2, Wt = 2),
  y = c(P = 0, X = 0, W = 0,
        O = 1, V = 2, Wt = 3))

zgg1b <- dagify(W ~ X + V + Wt,
               X ~ P + O,
               O ~ V,
               V ~ Wt,
               labels = zgg2_label,
               coords = zgg2_coord)

zgg1b %>%
  tidy_dagitty() %>% 
  mutate(linetype = ifelse(name %in% c("unitconfounder", "mob"), 
                           "dashed", "solid")) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_dag_point() + 
  geom_dag_edges() +
  geom_dag_label_repel(aes(label = label)) +
  geom_dag_text(parse = TRUE, label = c(
    "O","P", expression(V[t-1]), "W", expression(W[t-2]), "X"))

### Two Confounders --------------------------------------------------------
zgg2_label <- c(W = "Gasto Social",
                X = "Receita de Commodities",
                P = "Preços de Commodities",
                O = "Abertura Econômica",
                V = "Violência Política",
                Wt = "Gasto Social")

zgg2_coord <- list(
  x = c(P = 1, X = 2, W = 3,
        O = 2, V = 2, Wt = 2),
  y = c(P = 0, X = 0, W = 0,
        O = 1, V = 2, Wt = 3))

zgg2 <- dagify(W ~ X + O + V + Wt,
               X ~ P + O,
               O ~ V,
               V ~ Wt,
               labels = zgg2_label,
               coords = zgg2_coord)

zgg2 %>%
  tidy_dagitty() %>% 
  mutate(linetype = ifelse(name %in% c("unitconfounder", "mob"), 
                           "dashed", "solid")) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_dag_point() + 
  geom_dag_edges() +
  geom_dag_label_repel(aes(label = label)) +
  geom_dag_text(parse = TRUE, label = c(
    "O","P", expression(V[t-1]), "W", expression(W[t-2]), "X"))