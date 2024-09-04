# Packages -----------------------------------------------------------
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(plm) == F) install.packages('plm'); require(plm)
# if(require(tidyr) == F) install.packages('tidyr'); require(tidyr)
# if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(rlang) == F) install.packages('rlang'); require(rlang)
if(require(zoo) == F) install.packages('zoo'); require(zoo)

ws_dataset <- readRDS("final_data/ws_dataset.RDS")

ws_dataset <- ws_dataset %>% 
  group_by(country) %>% 
  arrange(year) %>% 
  mutate(cg_pcp_sexp = na.approx(cg_pcp_sexp, na.rm=FALSE),
         d_cg_pcp_sexp = cg_pcp_sexp - dplyr::lag(cg_pcp_sexp, n = 1),
         d_all_cmd_pcp = all_cmd_pcp - dplyr::lag(all_cmd_pcp, n = 1)) %>% 
  ungroup()

# Visualizing data --------------------------------------------------------
ws_visualizer <- function(data, var, varlabel){
  var <- ensym(var)
  data %>% 
    filter(year %in% 1990:2019) %>% 
    filter(n() != sum(is.na(!!var)), .by = country) %>% 
    ggplot(aes(x=year, y=!!var, group = 1)) +
    facet_wrap(
      ~factor(country), ncol = 5, scales = "free_y") +
    xlab("Ano") + ylab(varlabel) +
    theme_minimal() +
    geom_line(linewidth = 0.8) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

ws_dataset %>% 
  ws_visualizer(var = "cg_pcp_sexp", varlabel = "Gastos Sociais Per Capita")

ws_dataset %>% 
  ws_visualizer(var = "d_cg_pcp_sexp", 
                varlabel = "Gastos Sociais Per Capita (Primeira Diferença)")

ws_dataset %>% 
  ws_visualizer(var = "cg_prop_sexp", varlabel = "Proporção de Gastos Sociais")

ws_dataset %>% 
  ws_visualizer(var = "cg_gdp_sexp", 
                varlabel = "Gastos Sociais (%PIB)")

ws_dataset %>% 
  ws_visualizer(var = "all_cmd_pcp", 
                varlabel = "Receita de Exportação de Commodities")

ws_dataset %>% 
  ws_visualizer(var = "unemp", 
                varlabel = "Desemprego")

ws_dataset %>% 
  ws_visualizer(var = "kof_trade_df", 
                varlabel = "")

ws_dataset %>% 
  ws_visualizer(var = "kof_trade_dj", 
                varlabel = "")
# Unit-Root Test ----------------------------------------------------------
# Social Spending ---------------------------------------------------------
pws_dataset <- ws_dataset %>% 
  filter(!is.na(cg_pcp_sexp),
         !country %in% c("Haiti"),
         year %in% c(1990:2019)) %>% 
  pdata.frame(index = c("country", "year"))

purtest(cg_pcp_sexp ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

pws_dataset <- ws_dataset %>% 
  filter(!is.na(d_cg_pcp_sexp),
         !country %in% c("Haiti", "Trinidad & Tobago"),
         year %in% c(1990:2019)) %>% 
  pdata.frame(index = c("country", "year"))

purtest(d_cg_pcp_sexp ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

# Commodity Trade ---------------------------------------------------------
pws_dataset <- ws_dataset %>% 
  filter(!is.na(all_cmd_pcp),
         !country %in% c("Haiti")) %>% 
  pdata.frame(index = c("country", "year"))

purtest(all_cmd_pcp ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()
