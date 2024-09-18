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
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
    geom_dag_point() + 
    geom_dag_edges() +
    geom_dag_label_repel(aes(label = label)) +
    geom_dag_text()
}

## Naive DAG --------------------------------------------------
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

ggsave('product/dag/ngg.jpeg', dpi = 300, height = 5, width = 10, 
       unit = 'in', mydag(ngg))

## Confounder GGDAG --------------------------------------------------------
zgg1_label <- c(W = "Gasto Social",
                X = "Receita de Commodities",
                P = "Preços de Commodities",
                O = "Abertura Econômica",
                U = "Confusor Não-Observável",
                Wt1 = "Gasto Social Anterior",
                R = "Arrecadação",
                H = "Humor Político")

zgg1_coord <- list(
  x = c(P = 1, X = 2, R = 3, W = 4,
        O = 3, Wt1 = 4,
        H = 3, U = 3),
  y = c(P = 0, X = 0, R = 0.25, W = 0,
        O = 1, Wt1 = 1,
        H = -0.25, U = -1))

zgg1 <- dagify(W ~ R + H + O + Wt1 + U,
               R ~ X, H ~ X + R,
               X ~ P + U,
              labels = zgg1_label,
              coords = zgg1_coord)

zgg1 <- zgg1 %>%
  tidy_dagitty() %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_dag_point() + 
  geom_dag_edges() +
  geom_dag_label_repel(aes(label = label)) +
  geom_dag_text(parse = TRUE, label = c(
    expression(H[t-1]),"O",expression(P[t-1]), expression(R[t-1]), "U","W", 
    expression(W[t-1]), expression(X[t-1])
    ))

zgg1

ggsave('product/dag/zgg1.jpeg', dpi = 300, height = 5, width = 10, 
       unit = 'in', zgg1)
