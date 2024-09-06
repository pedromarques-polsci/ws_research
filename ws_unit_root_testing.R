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
         #cg_prop_def = na.approx(cg_prop_def, na.rm=FALSE),
         d_cg_pcp_sexp = cg_pcp_sexp - dplyr::lag(cg_pcp_sexp, n = 1),
         d_cg_gdp_sexp = cg_gdp_sexp - dplyr::lag(cg_gdp_sexp, n = 1),
         d_all_cmd_pcp = all_cmd_pcp - dplyr::lag(all_cmd_pcp, n = 1),
         d_gdp_pcp_ppp = gdp_pcp_ppp - dplyr::lag(gdp_pcp_ppp, n = 1),
         d_dp_ratio_old = 
           dp_ratio_old - (dplyr::lag(dp_ratio_old, n = 1) + 
                             dplyr::lag(dp_ratio_old, n = 2))) %>% 
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

rare_visualizer <- function(data, var, varlabel){
  var <- ensym(var)
  data %>%
    filter(year %in% 1990:2019) %>% 
    filter(n() != sum(is.na(!!var)), .by = country,
           sum(!!var) != 0) %>% 
    ggplot(aes(x = year, y = !!var)) + 
    facet_wrap(
      ~factor(country), ncol = 5, scales = "free_y") +
    xlab("Ano") + ylab(varlabel) +
    geom_col()
}

ws_dataset %>% 
rare_visualizer(var = "n_ncp",
                varlabel = "Non-Contributory Policy")

ws_dataset %>% 
  rare_visualizer(var = "n_sp",
                  varlabel = "Non-Contributory Social Pension")

ws_dataset %>% 
  rare_visualizer(var = "n_lpi",
                  varlabel = "Non-Contributory Labour Programme")

ws_dataset %>% 
  rare_visualizer(var = "n_cct",
                  varlabel = "Non-Contributory Cash Transfer Programme")

ws_dataset %>% 
  ws_visualizer(var = "v2pariglef_ord", 
                varlabel = "Ideologia")

ws_dataset %>%
  filter(year %in% 1990:2019) %>% 
  filter(n() != sum(is.na(civtot)), .by = country) %>% 
  ggplot(aes(x = year, y = civtot)) + 
  facet_wrap(
    ~factor(country), ncol = 5, scales = "free_y") +
  xlab("Ano") + ylab("Civil Violence") +
  geom_col()

# Unit-Root Test ----------------------------------------------------------
## Social Spending --------------------------------------------------------

### Social Spending Per Capita  -------------------------------------------
# Stationary at first difference
ws_dataset %>% 
  ws_visualizer(var = "cg_pcp_sexp", varlabel = "Gastos Sociais Per Capita")

pws_dataset <- ws_dataset %>% 
  filter(!is.na(cg_pcp_sexp),
         year %in% c(1990:2019)) %>% 
  filter(n() > 3, .by = country) %>% 
  pdata.frame(index = c("country", "year"))

purtest(cg_pcp_sexp ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

ws_dataset %>% 
  ws_visualizer(var = "d_cg_pcp_sexp", 
                varlabel = "Gastos Sociais Per Capita (Primeira Diferença)")

pws_dataset <- ws_dataset %>% 
  filter(!is.na(d_cg_pcp_sexp),
         year %in% c(1990:2019)) %>%
  filter(n() > 11, .by = country) %>% 
  pdata.frame(index = c("country", "year"))

purtest(d_cg_pcp_sexp ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

### Social Spending (% Overall) ---------------------------------------------
# Stationary at levels
ws_dataset %>% 
  ws_visualizer(var = "cg_prop_sexp", varlabel = "Proporção de Gastos Sociais")

pws_dataset <- ws_dataset %>% 
  filter(!is.na(cg_prop_sexp),
         year %in% c(1990:2019)) %>% 
  filter(n() > 8, .by = country) %>% 
  pdata.frame(index = c("country", "year"))

purtest(cg_prop_sexp ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

### Social Spending (%GDP) ------------------------------------------------
# Stationary at first difference
ws_dataset %>% 
  ws_visualizer(var = "cg_gdp_sexp", 
                varlabel = "Gastos Sociais (%PIB)")

pws_dataset <- ws_dataset %>% 
  filter(!is.na(cg_gdp_sexp),
         year %in% c(1990:2019)) %>% 
  filter(n() > 3, .by = country) %>%
  pdata.frame(index = c("country", "year"))

purtest(cg_gdp_sexp ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

ws_dataset %>% 
  ws_visualizer(var = "d_cg_gdp_sexp", 
                varlabel = "Gastos Sociais (%PIB) (Primeira Diferença)")

pws_dataset <- ws_dataset %>% 
  filter(!is.na(d_cg_gdp_sexp),
         year %in% c(1990:2019)) %>% 
  filter(n() > 11, .by = country) %>% 
  pdata.frame(index = c("country", "year"))

purtest(d_cg_gdp_sexp ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

## Commodity Trade ---------------------------------------------------------
# Stationary at levels
# Non-Stationary if I force 4 lags to all unities
ws_dataset %>% 
  ws_visualizer(var = "all_cmd_pcp", 
                varlabel = "Receita de Exportação de Commodities")

pws_dataset <- ws_dataset %>% 
  filter(!is.na(all_cmd_pcp),
         !country %in% c("")) %>% 
  filter(n() > 0, .by = country) %>% 
  pdata.frame(index = c("country", "year"))

purtest(all_cmd_pcp ~ 1, data = pws_dataset, lags = 'AIC', 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

## Covariates ---------------------------------------------------------------
### Defense expenditures (% Overall) ----------------------------------------
# Stationary at levels
ws_dataset %>% 
  ws_visualizer(var = "cg_prop_def", 
                varlabel = "Gastos Militares (%Total)")

pws_dataset <- ws_dataset %>% 
  filter(!is.na(cg_prop_def),
         year %in% c(1990:2019)) %>% 
  filter(n() > 7, .by = country) %>% 
  pdata.frame(index = c("country", "year"))

purtest(cg_prop_def ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

### Margin of Majority (% Overall) ----------------------------------------
# Stationary at levels
ws_dataset %>% 
  ws_visualizer(var = "maj", 
                varlabel = "Margem de Maioria Parlamentar")

pws_dataset <- ws_dataset %>% 
  filter(!is.na(maj),
         year %in% c(1990:2019),
         country != "Cuba") %>% 
  filter(n() > 1, .by = country) %>% 
  pdata.frame(index = c("country", "year"))

purtest(maj ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

### GDP per capita at PPP ----------------------------------------
# Stationary at first difference
ws_dataset %>% 
  ws_visualizer(var = "gdp_pcp_ppp", 
                varlabel = "PIB Per Capita em Paridade de Compra")

pws_dataset <- ws_dataset %>% 
  filter(!is.na(gdp_pcp_ppp),
         year %in% c(1990:2019)) %>% 
  filter(n() > 1, .by = country) %>% 
  pdata.frame(index = c("country", "year"))

purtest(gdp_pcp_ppp ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

ws_dataset %>% 
  ws_visualizer(var = "d_gdp_pcp_ppp", 
                varlabel = "PIB Per Capita em PPP (Primeira Diferença)")

pws_dataset <- ws_dataset %>% 
  filter(!is.na(d_gdp_pcp_ppp),
         year %in% c(1990:2019)) %>% 
  filter(n() > 1, .by = country) %>% 
  pdata.frame(index = c("country", "year"))

purtest(d_gdp_pcp_ppp ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

### Inflation Annual Growth ----------------------------------------
# Stationary at levels
ws_dataset %>% 
  ws_visualizer(var = "inf_eop_g", 
                varlabel = "Crescimento Anual da Inflação (%)")

pws_dataset <- ws_dataset %>% 
  filter(!is.na(inf_eop_g),
         year %in% c(1990:2019)) %>% 
  filter(n() > 1, .by = country) %>% 
  pdata.frame(index = c("country", "year"))

purtest(inf_eop_g ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

### Unemployment ----------------------------------------------
# Stationary at levels
ws_dataset %>% 
  ws_visualizer(var = "unemp", 
                varlabel = "Taxa de Desemprego (%)")

pws_dataset <- ws_dataset %>% 
  filter(!is.na(unemp),
         year %in% c(1990:2019)) %>% 
  filter(n() > 1, .by = country) %>% 
  pdata.frame(index = c("country", "year"))

purtest(unemp ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

### Central Government Debt ----------------------------------------------
# Stationary at levels
ws_dataset %>% 
  ws_visualizer(var = "cgov_debt", 
                varlabel = "Dívida do Governo Central")

pws_dataset <- ws_dataset %>% 
  filter(!is.na(cgov_debt),
         year %in% c(1990:2019)) %>% 
  filter(n() > 1, .by = country) %>% 
  pdata.frame(index = c("country", "year"))

purtest(cgov_debt ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

### Economic Opennesss (%GDP) ----------------------------------------------
# Stationary at levels
ws_dataset %>% 
  ws_visualizer(var = "kof_trade_df", 
                varlabel = "Abertura Econômica De Fato")

pws_dataset <- ws_dataset %>% 
  filter(!is.na(kof_trade_df),
         year %in% c(1990:2019)) %>% 
  filter(n() > 1, .by = country) %>% 
  pdata.frame(index = c("country", "year"))

purtest(kof_trade_df ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

### Economic Opennesss Index ----------------------------------------------
# Stationary at levels
ws_dataset %>% 
  ws_visualizer(var = "kof_trade_dj", 
                varlabel = "Abertura Econômica De Jure")

pws_dataset <- ws_dataset %>% 
  filter(!is.na(kof_trade_dj),
         year %in% c(1990:2019)) %>% 
  filter(n() > 1, .by = country) %>% 
  pdata.frame(index = c("country", "year"))

purtest(kof_trade_dj ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

### Urban Population ----------------------------------------------
# Stationary at levels
ws_dataset %>% 
  ws_visualizer(var = "urban_pop", 
                varlabel = "População Urbana")

pws_dataset <- ws_dataset %>% 
  filter(!is.na(urban_pop),
         year %in% c(1990:2019)) %>% 
  filter(n() > 1, .by = country) %>% 
  pdata.frame(index = c("country", "year"))

purtest(urban_pop ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

### Dependency Ratio (Old People) ----------------------------------------------
# Stationary at second difference at 1%
ws_dataset %>% 
  ws_visualizer(var = "dp_ratio_old", 
                varlabel = "Razão de Dependência (Idosos)")

pws_dataset <- ws_dataset %>% 
  filter(!is.na(dp_ratio_old),
         year %in% c(1990:2019)) %>% 
  filter(n() > 1, .by = country) %>% 
  pdata.frame(index = c("country", "year"))

purtest(dp_ratio_old ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

ws_dataset %>% 
  ws_visualizer(var = "d_dp_ratio_old", 
                varlabel = "Razão de Dependência (Idosos) (Segunda Diferença)")

pws_dataset <- ws_dataset %>% 
  filter(!is.na(d_dp_ratio_old),
         year %in% c(1990:2019)) %>% 
  filter(n() > 1, .by = country) %>% 
  pdata.frame(index = c("country", "year"))

purtest(d_dp_ratio_old ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

### Dependency Ratio (Young People) ----------------------------------------------
# Stationary at levels
ws_dataset %>% 
  ws_visualizer(var = "dp_ratio_yg", 
                varlabel = "Razão de Dependência (Jovens)")

pws_dataset <- ws_dataset %>% 
  filter(!is.na(dp_ratio_yg),
         year %in% c(1990:2019)) %>% 
  filter(n() > 1, .by = country) %>% 
  pdata.frame(index = c("country", "year"))

purtest(dp_ratio_yg ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

### Dependency Ratio (Young People) ----------------------------------------------
# Stationary at levels
ws_dataset %>% 
  ws_visualizer(var = "dp_ratio_yg", 
                varlabel = "Razão de Dependência (Jovens)")

pws_dataset <- ws_dataset %>% 
  filter(!is.na(dp_ratio_yg),
         year %in% c(1990:2019)) %>% 
  filter(n() > 1, .by = country) %>% 
  pdata.frame(index = c("country", "year"))

purtest(dp_ratio_yg ~ 1, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "madwu", pmax = 4) %>% summary()

# Jokes apart -------------------------------------------------------------
ws_dataset %>% 
  filter(year %in% 1990:2019,
         region == "South America") %>% 
  filter(n() != sum(is.na(maj)), .by = country) %>% 
  ggplot(aes(x=year, y=maj, group = 1)) +
  facet_wrap(
    ~factor(country), ncol = 3, scales = "free_y") +
  xlab("Ano") + 
  ylab("Proporção de Parlamentares do Partido do Governo") +
  labs(title = "Base Partidária dos Chefes de Governo na América do Sul") +
  theme_minimal() +
  geom_line(linewidth = 0.8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

ws_dataset %>% 
  filter(year %in% 1990:2019,
         country == "Brazil") %>% 
  ggplot(aes(x=year, y=maj, group = 1)) +
  xlab("Ano") + 
  ylab("Proporção de Parlamentares do Partido do Governo") +
  theme_minimal() +
  geom_line(linewidth = 0.8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
