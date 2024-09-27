# Packages -----------------------------------------------------------
if(require(car) == F) install.packages('car'); require(car)
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(ggpubr) == F) install.packages('ggpubr'); require(ggpubr)
if(require(lmtest) == F) install.packages('lmtest'); require(lmtest)
if(require(plm) == F) install.packages('plm'); require(plm)
if(require(rlang) == F) install.packages('rlang'); require(rlang)
if(require(zoo) == F) install.packages('zoo'); require(zoo)

dataset <- readRDS("final_data/ws_dataset.RDS")


# Loading data ------------------------------------------------------------
ws_dataset <- dataset %>% 
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

# 1. UNIT ROOT TESTS / VISUALIZATION ---------------------------------------
## Social Spending Per Capita ----------------------------------------------
# Stationary at first difference

ws_dataset %>% 
  ws_visualizer(var = "cg_pcp_sexp", 
                varlabel = "Gastos Sociais Per Capita")
ws_dataset %>% 
  ws_visualizer(var = "d_cg_pcp_sexp", 
                varlabel = "Gastos Sociais Per Capita (Primeira Diferença)")

myvar <- c("cg_pcp_sexp", "all_cmd_pcp", 
           ws_dataset %>% select(gdp_pcp_ppp:kof_trade_df, 
                                 urban_pop:dp_ratio_yg, v2pariglef_ord) %>% 
             colnames())

ws_dataset_a <- ws_dataset %>% 
  group_by(iso3c) %>% 
  summarise(across(all_of(myvar), ~ mean(!is.na(.)))) %>%
  filter(if_all(all_of(myvar), ~ . >= 0.7)) %>%
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

myvar <- c("cg_prop_sexp", "all_cmd_pcp", 
           ws_dataset %>% select(gdp_pcp_ppp:kof_trade_df, 
                                 urban_pop:dp_ratio_yg, v2pariglef_ord) %>% 
             colnames())

ws_dataset_b <- ws_dataset %>% 
  group_by(iso3c) %>% 
  summarise(across(all_of(myvar), ~ mean(!is.na(.)))) %>%
  filter(if_all(all_of(myvar), ~ . >= 0.7)) %>%
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

myvar <- c("cg_gdp_sexp", "all_cmd_pcp", 
           ws_dataset %>% select(gdp_pcp_ppp:kof_trade_df, 
                                 urban_pop:dp_ratio_yg, v2pariglef_ord) %>% 
             colnames())

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

# 2. NORMALITY -------------------------------------------------------------
ggplot(data = ws_dataset_a, aes(x = cg_pcp_sexp))+
  geom_histogram(fill = "#FF6885", color = "#FF6885", alpha = 0.6)+
  ylab("Frequência")+
  xlab("Gasto social")

ggdensity(ws_dataset_a$cg_pcp_sexp, 
          main = "Densidade",
          xlab = "Gasto social")

shapiro.test(ws_dataset_a$cg_pcp_sexp)

ggplot(data = ws_dataset_b, aes(x = cg_prop_sexp))+
  geom_histogram(fill = "#FF6885", color = "#FF6885", alpha = 0.6)+
  ylab("Frequência")+
  xlab("Gasto social")

ggdensity(ws_dataset_b$cg_prop_sexp, 
          main = "Densidade",
          xlab = "Gasto social")

shapiro.test(ws_dataset_b$cg_prop_sexp)

ggplot(data = ws_dataset_c, aes(x = cg_gdp_sexp))+
  geom_histogram(fill = "#FF6885", color = "#FF6885", alpha = 0.6)+
  ylab("Frequência")+
  xlab("Gasto social")

ggdensity(ws_dataset_c$cg_gdp_sexp, 
          main = "Densidade",
          xlab = "Gasto social")

shapiro.test(ws_dataset_c$cg_gdp_sexp)

# 3. PANEL DATA -------------------------------------------------------------

## 3.1 FIXED EFFECTS --------------------------------------------------------

### SOCIAL SPENDING PCP -----------------------------------------------------

### Two variables
myvar <- c("d_cg_pcp_sexp", "all_cmd_pcp")

a_test <- ws_dataset %>% 
  group_by(iso3c) %>% 
  summarise(across(all_of(myvar), ~ mean(!is.na(.)))) %>%
  filter(if_all(all_of(myvar), ~ . >= 0.7)) %>%
  select(iso3c) %>%
  inner_join(ws_dataset, by = "iso3c") %>% 
  ungroup() %>% 
  pdata.frame(index = c("iso3c", "year"))

a_test %>% plm(formula = d_cg_pcp_sexp ~ all_cmd_pcp,
                     data = .,
                     index = c("iso3c", "year"),
                     model = "within", effect = "individual") %>%
  coeftest(., vcov = vcovHC(., type="HC0", cluster="group"))

###
myvar <- c("cg_pcp_sexp", "all_cmd_pcp", "unemp", 
             "cgov_debt", "kof_trade_df", "dp_ratio_old", 'dp_ratio_yg',
             "v2pariglef_ord", "urban_pop", "gdp_pcp_ppp")

a_test <- ws_dataset %>% 
  group_by(iso3c) %>% 
  summarise(across(all_of(myvar), ~ mean(!is.na(.)))) %>%
  filter(if_all(all_of(myvar), ~ . >= 0.7)) %>%
  select(iso3c) %>%
  inner_join(ws_dataset, by = "iso3c") %>% 
  ungroup() %>% 
  pdata.frame(index = c("iso3c", "year"))

a_test %>% plm(formula = d_cg_pcp_sexp ~ all_cmd_pcp + unemp +
               cgov_debt + kof_trade_df + dp_ratio_old + dp_ratio_yg + 
                 v2pariglef_ord + urban_pop + gdp_pcp_ppp,
               data = .,
               index = c("iso3c", "year"),
               model = "within", effect = "individual") %>%
  coeftest(., vcov = vcovHC(., type="HC0", cluster="group"))

vif_test <- lm(formula = d_cg_pcp_sexp ~ all_cmd_pcp + unemp +
             cgov_debt + kof_trade_df + dp_ratio_old + dp_ratio_yg + 
             v2pariglef_ord + urban_pop + gdp_pcp_ppp, data = a_test)

vif(vif_test)

###
myvar <- c("cg_pcp_sexp", "all_cmd_pcp", "unemp", 
           "cgov_debt", "kof_trade_df", "dp_ratio_old", 'dp_ratio_yg',
           "v2pariglef_ord", "gdp_pcp_ppp")

a_test <- ws_dataset %>% 
  group_by(iso3c) %>% 
  summarise(across(all_of(myvar), ~ mean(!is.na(.)))) %>%
  filter(if_all(all_of(myvar), ~ . >= 0.7)) %>%
  select(iso3c) %>%
  inner_join(ws_dataset, by = "iso3c") %>% 
  ungroup() %>% 
  pdata.frame(index = c("iso3c", "year"))

a_test %>% plm(formula = d_cg_pcp_sexp ~ all_cmd_pcp + unemp +
                 cgov_debt + kof_trade_df + dp_ratio_old + dp_ratio_yg + 
                 v2pariglef_ord + gdp_pcp_ppp,
               data = .,
               index = c("iso3c", "year"),
               model = "within", effect = "individual") %>%
  coeftest(., vcov = vcovHC(., type="HC0", cluster="group"))

vif_test <- lm(formula = d_cg_pcp_sexp ~ all_cmd_pcp + unemp +
                 cgov_debt + kof_trade_df + dp_ratio_old + dp_ratio_yg + 
                 v2pariglef_ord + gdp_pcp_ppp, data = a_test)

vif(vif_test)

###
myvar <- c("cg_pcp_sexp", "all_cmd_pcp", "kof_trade_df", "dp_ratio_old", 
           "dp_ratio_yg", "v2pariglef_ord", "cg_prop_def")

a_test <- ws_dataset %>% 
  group_by(iso3c) %>% 
  summarise(across(all_of(myvar), ~ mean(!is.na(.)))) %>%
  filter(if_all(all_of(myvar), ~ . >= 0.7)) %>%
  select(iso3c) %>%
  inner_join(ws_dataset, by = "iso3c") %>% 
  ungroup() %>% 
  pdata.frame(index = c("iso3c", "year"))

a_test %>% plm(formula = d_cg_pcp_sexp ~ all_cmd_pcp + 
                 kof_trade_df + dp_ratio_old + dp_ratio_yg + 
                 + cg_prop_def + v2pariglef_ord,
               data = .,
               index = c("iso3c", "year"),
               model = "within", effect = "individual") %>%
  coeftest(., vcov = vcovHC(., type="HC0", cluster="group"))

vif_test <- lm(formula = d_cg_pcp_sexp ~ all_cmd_pcp + 
                 kof_trade_df + dp_ratio_old + dp_ratio_yg + 
                 v2pariglef_ord + cg_prop_def, data = a_test)

vif(vif_test)