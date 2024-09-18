# Packages -----------------------------------------------------------
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(plm) == F) install.packages('plm'); require(plm)
if(require(rlang) == F) install.packages('rlang'); require(rlang)
if(require(zoo) == F) install.packages('zoo'); require(zoo)

ws_dataset <- readRDS("final_data/ws_dataset.RDS")

ws_dataset <- ws_dataset %>% 
  group_by(iso3c) %>% 
  arrange(year) %>% 
  mutate(cg_pcp_sexp = na.approx(cg_pcp_sexp, na.rm=FALSE),
         #cg_prop_def = na.approx(cg_prop_def, na.rm=FALSE),
         d_cg_pcp_sexp = cg_pcp_sexp - dplyr::lag(cg_pcp_sexp, n = 1),
         d_cg_gdp_sexp = cg_gdp_sexp - dplyr::lag(cg_gdp_sexp, n = 1),
         d_cg_prop_sexp = cg_prop_sexp - dplyr::lag(cg_prop_sexp, n = 1)) %>%
  ungroup()

# Visualizing functions ------------------------------------------------------
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

# UNIT ROOT TESTS / VISUALIZATION ------------------------------------------
## Social Spending Per Capita ----------------------------------------------
# Stationary at first difference
ws_dataset %>% 
  ws_visualizer(var = "cg_pcp_sexp", 
                varlabel = "Gastos Sociais Per Capita")
ws_dataset %>% 
  ws_visualizer(var = "d_cg_pcp_sexp", 
                varlabel = "Gastos Sociais Per Capita (Primeira Diferença)")

myvar <- c("cg_pcp_sexp", "all_cmd_pcp", "kof_trade_df")

ws_dataset_a <- ws_dataset %>% 
  group_by(iso3c) %>% 
  summarise(across(all_of(myvar), ~ mean(!is.na(.)))) %>%
  filter(across(all_of(myvar), ~ . >= 0.7)) %>%
  select(iso3c) %>%
  inner_join(ws_dataset, by = "iso3c") %>% 
  ungroup()

pws_dataset <- ws_dataset_a %>% 
  filter(!is.na(cg_pcp_sexp),
         year %in% c(1990:2019)) %>% 
  pdata.frame(index = c("country", "year"))

purtest(pws_dataset$cg_pcp_sexp, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "ips", pmax = 5)

pws_dataset <- ws_dataset_a %>% 
  filter(!is.na(d_cg_pcp_sexp),
         year %in% c(1990:2019)) %>% 
  pdata.frame(index = c("country", "year"))

purtest(pws_dataset$d_cg_pcp_sexp, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "ips", pmax = 6)

## Social Spending (% Overall) ---------------------------------------------
# Stationary at levels
ws_dataset %>% 
  ws_visualizer(var = "cg_prop_sexp", 
                varlabel = "Gastos Sociais (% Gastos Públicos)")

myvar <- c("cg_prop_sexp", "all_cmd_pcp", "kof_trade_df")

ws_dataset_b <- ws_dataset %>% 
  group_by(iso3c) %>% 
  summarise(across(all_of(myvar), ~ mean(!is.na(.)))) %>%
  filter(across(all_of(myvar), ~ . >= 0.7)) %>%
  select(iso3c) %>%
  inner_join(ws_dataset, by = "iso3c") %>% 
  ungroup()

pws_dataset <- ws_dataset_b %>% 
  filter(!is.na(cg_prop_sexp),
         year %in% c(1990:2019)) %>% 
  pdata.frame(index = c("country", "year"))

purtest(pws_dataset$cg_prop_sexp, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "ips", pmax = 5)

## Social Spending (%GDP) ------------------------------------------------
# Stationary at first difference
ws_dataset %>% 
  ws_visualizer(var = "cg_gdp_sexp", 
                varlabel = "Gastos Sociais (%PIB)")

ws_dataset %>% 
  ws_visualizer(var = "d_cg_gdp_sexp", 
                varlabel = "Gastos Sociais (%PIB) (Primeira Diferença)")

myvar <- c("cg_gdp_sexp", "all_cmd_pcp", "kof_trade_df")

ws_dataset_c <- ws_dataset %>% 
  group_by(iso3c) %>% 
  summarise(across(all_of(myvar), ~ mean(!is.na(.)))) %>%
  filter(across(all_of(myvar), ~ . >= 0.7)) %>%
  select(iso3c) %>%
  inner_join(ws_dataset, by = "iso3c") %>% 
  ungroup()

pws_dataset <- ws_dataset_c %>% 
  filter(!is.na(cg_gdp_sexp),
         year %in% c(1990:2019)) %>% 
  pdata.frame(index = c("country", "year"))

purtest(pws_dataset$cg_gdp_sexp, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "ips", pmax = 6)

pws_dataset <- ws_dataset_c %>% 
  filter(!is.na(d_cg_gdp_sexp),
         year %in% c(1990:2019)) %>% 
  pdata.frame(index = c("country", "year"))

purtest(pws_dataset$d_cg_gdp_sexp, data = pws_dataset, lags = "AIC", 
        exo = "trend", test = "ips", pmax = 5)

## Commodity Trade ---------------------------------------------------------
ws_dataset %>% 
  ws_visualizer(var = "all_cmd_pcp", 
                varlabel = "Receita de Exportação de Commodities per capita")


# PLOTTING TIME SERIES ------------------------------------------------------
## Defense expenditures (% Overall) ----------------------------------------
ws_dataset %>% 
  ws_visualizer(var = "cg_prop_def", 
                varlabel = "Gastos Militares (%Total)")

## Margin of Majority (% Overall) ----------------------------------------
ws_dataset %>% 
  ws_visualizer(var = "maj", 
                varlabel = "Margem de Maioria Parlamentar")

## GDP per capita at PPP ----------------------------------------
ws_dataset %>% 
  ws_visualizer(var = "gdp_pcp_ppp", 
                varlabel = "PIB Per Capita em Paridade de Compra")

## Inflation Annual Growth ----------------------------------------
ws_dataset %>% 
  ws_visualizer(var = "inf_eop_g", 
                varlabel = "Crescimento Anual da Inflação (%)")

## Unemployment ----------------------------------------------
ws_dataset %>% 
  ws_visualizer(var = "unemp", 
                varlabel = "Taxa de Desemprego (%)")

## Central Government Debt ----------------------------------------------
ws_dataset %>% 
  ws_visualizer(var = "cgov_debt", 
                varlabel = "Dívida do Governo Central")

## Economic Opennesss (%GDP) ----------------------------------------------
ws_dataset %>% 
  ws_visualizer(var = "kof_trade_df", 
                varlabel = "Abertura Econômica De Fato")

## Economic Opennesss Index ----------------------------------------------
ws_dataset %>% 
  ws_visualizer(var = "kof_trade_dj", 
                varlabel = "Abertura Econômica De Jure")

## Urban Population ----------------------------------------------
ws_dataset %>% 
  ws_visualizer(var = "urban_pop", 
                varlabel = "População Urbana")

## Dependency Ratio (Old People) ----------------------------------------------
ws_dataset %>% 
  ws_visualizer(var = "dp_ratio_old", 
                varlabel = "Razão de Dependência (Idosos)")

## Dependency Ratio (Young People) -------------------------------------------
ws_dataset %>% 
  ws_visualizer(var = "dp_ratio_yg", 
                varlabel = "Razão de Dependência (Jovens)")

## Dependency Ratio (Young People) -------------------------------------------
ws_dataset %>% 
  ws_visualizer(var = "dp_ratio_yg", 
                varlabel = "Razão de Dependência (Jovens)")

## Non-Contributory Social Policy Adoption -----------------------------------
ws_dataset %>% 
rare_visualizer(var = "n_ncp",
                varlabel = "Adoção de Programas Sociais Não-Contributivos")

ws_dataset %>% 
  rare_visualizer(var = "n_sp",
                  varlabel = "Adoção de Programas de Aposentadoria
                  Não-Contributivos")

ws_dataset %>% 
  rare_visualizer(var = "n_lpi",
                  varlabel = "Adoção de Programas de Inclusão do Trabalho
                  Não-Contributivos")

ws_dataset %>% 
  rare_visualizer(var = "n_cct",
                  varlabel = "Adoção de Programas de Transferência
                  Condicionada de Renda")

ws_dataset %>% 
  ws_visualizer(var = "v2pariglef_ord", 
                varlabel = "Ideologia")

ws_dataset %>%
  group_by(iso3c) %>% 
  mutate(soma_valor = sum(civtot, na.rm = T)) %>% 
  ungroup() %>% 
  filter(year %in% 1990:2019, soma_valor > 0) %>% 
  filter(n() != sum(is.na(civtot)), .by = country) %>% 
  ggplot(aes(x = year, y = civtot)) + 
  facet_wrap(
    ~factor(country), ncol = 5, scales = "free_y") +
  xlab("Ano") + ylab("Magnitude de Episódios de Violência Política") +
  geom_col()