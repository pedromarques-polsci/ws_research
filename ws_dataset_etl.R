# Packages -----------------------------------------------------------
if(require(countrycode) == F) install.packages('countrycode'); require(countrycode)
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(gridExtra) == F) install.packages('gridExtra'); require(gridExtra)
if(require(haven) == F) install.packages('haven'); require(haven)
if(require(Hmisc) == F) install.packages('Hmisc'); require(Hmisc)
if(require(readxl) == F) install.packages('readxl'); require(readxl)
if(require(rvest) == F) install.packages('rvest'); require(rvest)
#if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(tidyr) == F) install.packages('tidyr'); require(tidyr)
if(require(wbstats) == F) install.packages('wbstats'); require(wbstats)

# Functions ---------------------------------------------------------------
## Summing values while skipping NAs and not imputing zeroes
calc <- function(df, cols, x) {
  new_column <- ensym(x)
  df %>%
    mutate(
      !!x := ifelse(
        rowSums(is.na(select(., all_of(cols)))) == ncol(select(., all_of(cols))),
        NA,
        rowSums(select(., all_of(cols)), na.rm = TRUE)
      )
    )
}

## Extraction of information directly from Data Bank
wb_etl <- function(y, w, z){
  wb_data(country = "countries_only", indicator = as.character(y), start_date = w, 
          end_date = z) %>% 
#    left_join(correct_names, join_by(country)) %>% 
    rename(year = date) %>% 
    mutate(iso3c = case_when(country == "Kosovo" ~ "KSV", .default = iso3c)) %>% 
    select(2:5)
}

# Country Name Correction -------------------------------------------------
correct_names <- data.frame(
  country = c('Czechoslovakia', 'Netherlands Antilles', 'Serbia and Montenegro',
              "Yugoslavia, Soc. Fed. Rep. of", "Yugoslavia",
              "Pacific Islands, Trust Territory",
              "GDR", "German Democratic Republic", "Germany East",
              "Turk Cyprus", "P. N. Guinea", "eSwatini",
              "Kosovo", "Somaliland", "Zanzibar",
              "Palestine/British Mandate", "Palestine/Gaza", "Channel Islands",
              "Yemen, Democratic", "Yemen (PDR)", "South Yemen", "Yemen South",
              "Yemen North", "Yemen (AR)", "Yemen",
              "Vietnam, South", "Vietnam South",
              "Vietnam North", "Republic of Vietnam"
  ),
  iso3c = c("CSK", "ANT", "SCG",
            "YUG", "YUG",
            "PCI",
            "DDR", "DDR", "DDR",
            0, "PNG", "SSW",
            "KSV", "SML", "ZZB",
            "PSB", "PSG", "CHI",
            "YMD", "YMD", "YMD", "YMD",
            "YAR", "YAR", "YEM",
            "VDR", "VDR",
            "VNR", "VNR"
            )
)

# 1. DEPENDENT VARIABLE ------------------------------------------------
## 1.1 Social Spending per capita (Constant Prices) --------------------
social_spending <- read_xlsx("raw_data/cepal_social_spending_pcp_constant.xlsx", sheet = 1, col_types = c(
  "text", "text", "text", "text", "numeric", "numeric", "text", "text", "numeric")
) %>%
  select(c(-1, -7, -8, -9)) %>%
  tidyr::pivot_wider(names_from = 3,
              values_from = value,
              values_fill = NA) %>%
  select(-4, -8, -9) %>% 
  tidyr::pivot_wider(names_from = 1,
              values_from = c(4:6),
              values_fill = NA) %>% 
  rename(country = 1, year = 2, cg_pcp_edu = 3, gg_pcp_edu = 4, nfps_pcp_edu = 5,
         cg_pcp_security = 6, gg_pcp_security = 7, nfps_pcp_security = 8,
         cg_pcp_health = 9, gg_pcp_health = 10, nfps_pcp_health = 11) %>% 
  calc(c("cg_pcp_edu", "cg_pcp_security", "cg_pcp_health"), "cg_pcp_sexp") %>% 
  calc(c("gg_pcp_edu", "gg_pcp_security", "gg_pcp_health"), "gg_pcp_sexp") %>% 
  calc(c("nfps_pcp_edu", "nfps_pcp_security", "nfps_pcp_health"), "nfps_pcp_sexp") %>% 
  arrange(country, year)

# Warnings are referred to coercion and should be ignored

## 1.2 Public Spending by Function -----------------------------------
# Public spending by function
public_spending <- read_xlsx("raw_data/cepal_public_spending_current.xlsx") %>%
  select(c(-1, -7, -8, -9)) %>%
  pivot_wider(names_from = 3,
              values_from = value,
              values_fill = NA) %>%
  pivot_wider(names_from = 1,
              values_from = c(4:8),
              values_fill = NA) %>% 
  rename(country = 1, year = 2, 
         cg_curr = 3, gg_curr = 4, nfps_curr = 5,
         cg_curr_def = 6, gg_curr_def = 7, nfps_curr_def = 8,
         cg_curr_health = 9, gg_curr_health = 10, nfps_curr_health = 11,
         cg_curr_edu = 12, gg_curr_edu = 13, nfps_curr_edu = 14,
         cg_curr_security = 15, gg_curr_security = 16, nfps_curr_security = 17) %>%
  
  # Central government
  mutate(year = as.numeric(year),
         cg_prop_def = cg_curr_def * 100 / cg_curr,
         cg_prop_health = cg_curr_health * 100 / cg_curr,
         cg_prop_edu = cg_curr_edu * 100 / cg_curr,
         cg_prop_security = cg_curr_security * 100 / cg_curr,
  
  # General government
         gg_prop_def = gg_curr_def * 100 / gg_curr,
         gg_prop_health = gg_curr_health * 100 / gg_curr,
         gg_prop_edu = gg_curr_edu * 100 / gg_curr,
         gg_prop_security = gg_curr_security * 100 / gg_curr,
         
  # Non-Financial Public Sector
         nfps_prop_def = nfps_curr_def * 100 / nfps_curr,
         nfps_prop_health = nfps_curr_health * 100 / nfps_curr,
         nfps_prop_edu = nfps_curr_edu * 100 / nfps_curr,
         nfps_prop_security = nfps_curr_security * 100 / nfps_curr) %>% 
    calc(c("cg_prop_health", "cg_prop_edu", "cg_prop_security"), "cg_prop_sexp") %>% 
    calc(c("gg_prop_health", "gg_prop_edu", "gg_prop_security"), "gg_prop_sexp") %>% 
    calc(c("nfps_prop_health", "nfps_prop_edu", "nfps_prop_security"), "nfps_prop_sexp") %>% 
  arrange(country, year)

## 1.3 Social Spending (% GPD) ---------------------------------------------
social_spending_gdp <- read_xlsx("raw_data/cepal_social_spending_gdp.xlsx",
                                 col_types = c(
  "text", "text", "text", "text", "numeric", "numeric", "text", "text", "numeric")
) %>% 
  select(-1, -7, -8, -9) %>% 
  pivot_wider(names_from = 3,
              values_from = value,
              values_fill = NA) %>%
  select(-4, -5, -6) %>% 
  pivot_wider(names_from = 2,
              values_from = c(4:6),
              values_fill = NA) %>% 
  rename(country = 1, year = 2, 
         cg_gdp_health = 3, gg_gdp_health = 4, nfps_gdp_health = 5,
         cg_gdp_edu = 6, gg_gdp_edu = 7, nfps_gdp_edu = 8,
         cg_gdp_security = 9, gg_gdp_security = 10, nfps_gdp_security = 11) %>% 
  calc(c("cg_gdp_edu", "cg_gdp_security", "cg_gdp_health"), "cg_gdp_sexp") %>% 
  calc(c("gg_gdp_edu", "gg_gdp_security", "gg_gdp_health"), "gg_gdp_sexp") %>% 
  calc(c("nfps_gdp_edu", "nfps_gdp_security", "nfps_gdp_health"), "nfps_gdp_sexp") %>% 
  arrange(country, year)

# Correcting Bolivia's country name
social_spending$country[social_spending$country == 
                          "Bolivia (Plurinational State of)"] <- 'Bolivia'
public_spending$country[social_spending$country == 
                          "Bolivia (Plurinational State of)"] <- 'Bolivia'
social_spending_gdp$country[social_spending$country == 
                          "Bolivia (Plurinational State of)"] <- 'Bolivia'


social_spending<- social_spending %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name",
                             destination = "iso3c"))

social_spending_gdp <- social_spending_gdp %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name",
                             destination = "iso3c"))
public_spending <- public_spending %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name",
                             destination = "iso3c"))

lac <- codelist_panel %>% filter(continent == "Americas") %>% 
  select(country.name.en,year,iso3c) %>% 
  filter(year >= 1990, !country.name.en %in% c("United States", "Canada")) %>% 
  complete(year = 1990:2022) %>% drop_na() %>% 
  arrange(country.name.en, year)

lac_expenditure <- lac %>% 
  left_join(social_spending %>% select(-country), join_by(iso3c, year)) %>% 
  left_join(social_spending_gdp%>% select(-country), join_by(iso3c, year)) %>% 
  left_join(public_spending %>% select(-country), join_by(iso3c, year)) %>% 
  rename(country = country.name.en)

lac_expenditure %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

## Exporting full spending data-set
saveRDS(lac_expenditure, "final_data/lac_expenditure.RDS")

# 2. INDEPENDENT VARIABLE -------------------------------------------------
## 2.1 Deflator -----------------------------------------------------------
cpi <- wb_etl(y = "FP.CPI.TOTL", w = 1980, z = 2022) %>% 
  filter(iso3c == "USA") %>% 
  rename(us_cpi = FP.CPI.TOTL) %>% 
  select(iso3c, year, us_cpi)

## 2.2 Population ---------------------------------------------------------
population <- read.csv2("raw_data/unctad_population.csv", sep = ",", 
                        na.strings = c(NA, "")) %>% 
  rename(country = 1, pop = 3, year = Year) %>% 
  filter(!country %in% c('Individual economies')) %>% 
  left_join(correct_names, join_by(country))

population <- population %>% 
  filter(!country %in% intersect(population$country, correct_names$country)) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", 
                             destination = "iso3c")) %>% 
  rbind(population %>% filter(country %in% intersect(population$country, 
                                                 correct_names$country))) %>% 
  drop_na(pop, iso3c)

population %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

## 2.3 Commodity Trade Revenue --------------------------------------------
cmd_trade <- read.csv2("raw_data/unctad_trade.csv", sep = ",", 
                       na.strings = c(NA, "")) %>% 
  rename(country = 2, cmd_stone = 3, cmd_stone_prop = 4, ore_metal = 5, 
         ore_metal_prop = 6, iron_steel = 7, iron_steel_prop = 8,
         year = Year) %>% 
  filter(!country %in% 
           c("Sudan (...2011)", "Indonesia (...2002)")) %>% 
  mutate(cmd_stone_prop = as.numeric(cmd_stone_prop),
         ore_metal_prop = as.numeric(ore_metal_prop),
         iron_steel_prop = as.numeric(iron_steel_prop)) %>% 
  calc(c("cmd_stone", "ore_metal", "iron_steel"), "all_cmd") %>% 
  calc(c("cmd_stone_prop", "ore_metal_prop", "iron_steel_prop"), "all_cmd_prop") %>% 
  left_join(correct_names, join_by(country))


cmd_trade <- cmd_trade %>% 
  filter(!country %in% intersect(cmd_trade$country, correct_names$country)) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", 
                             destination = "iso3c")) %>% 
  rbind(cmd_trade %>% filter(country %in% intersect(cmd_trade$country, 
                                                     correct_names$country))) %>% 
  drop_na(iso3c)

cmd_trade %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

cmd_trade <- cmd_trade %>% 
  left_join(population %>% select(-country), join_by(year, iso3c)) %>% 
  left_join(cpi %>% select(-iso3c), join_by(year)) %>% 
  mutate(all_cmd_pcp = (all_cmd/pop) * (100/us_cpi)) %>% # USA CPI for 2010 is "100"
  filter(year < 2021)

attr(cmd_trade$us_cpi, "label") <- ""
attr(cmd_trade$all_cmd_pcp, "label") <- ""

# 3. POLITICAL COVARIATES-----------------------------------------
## 3.1 Database of Political Institutions ----------------------------
dpi <- read.csv2("raw_data/dpi.csv", sep = ",") %>%
  rename(country = countryname) %>%
  mutate(year = as.double(year),
         maj = as.numeric(maj)) %>%
  left_join(correct_names, join_by(country))

dpi <- dpi %>% 
  filter(!country %in% intersect(dpi$country, correct_names$country)) %>% 
  mutate(country = ifelse(country == 'Cent. Af. Rep.', 'Central African Republic', country),
         country = ifelse(country == 'Dom. Rep.', 'Dominican Republic', country),
         country = ifelse(country == 'PRC', "People's Republic of China", country),
         country = ifelse(country == 'PRK', "Korea (the Democratic People's Republic of)", country),
         country = ifelse(country == 'ROK', 'Republic of Korea', 
                          country),
         country = ifelse(country == 'S. Africa', 'South Africa', country),
  )%>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>% 
  rbind(dpi %>% filter(country %in% intersect(dpi$country, 
                                                    correct_names$country)))

dpi %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

## 3.2 Leaders Global  ------------------------------------------------
leadglob <- read.csv2("raw_data/leadglob.csv", sep = ",") %>%
  mutate(year = as.double(year)) %>%
  left_join(correct_names, join_by(country))

leadglob <- leadglob %>% 
  filter(!country %in% intersect(leadglob$country, correct_names$country)) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", 
                             destination = "iso3c")) %>% 
  rbind(leadglob %>% filter(country %in% intersect(leadglob$country, 
                                                 correct_names$country)))
  
leadglob %>% 
  count(iso3c, year) %>% 
  filter(n > 1) 

# Panama is duplicated

leadglob <- leadglob[!duplicated(leadglob[c("country","year")]),] # Removing duplicates

## 3.3 V-Party ------------------------------------------------------------
vparty <- readRDS("raw_data/v_party.rds") %>%
  rename(country = country_name) %>%
  left_join(correct_names, join_by(country))

vparty <- vparty %>% 
  filter(!country %in% intersect(vparty$country, correct_names$country)) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", 
                             destination = "iso3c")) %>% 
  rbind(vparty %>% filter(country %in% intersect(vparty$country, 
                                              correct_names$country)))

vparty %>% 
  count(iso3c, year, v2paid) %>% 
  filter(n > 1)

# 4. ECONOMIC COVARIATES-------------------------------------------------
## 4.1 World Economic Outlook ---------------------------------------------
weo <- read.csv2("raw_data/weo_data.csv", na = c("n/a", "", "--"), dec = ".") %>% 
  slice(1:2170)

names(weo) <- substr(names(weo), 2, 5)

weo <- weo %>%  
  pivot_longer(cols = c(6:48))

weo$value <- gsub(",", "", weo$value)

weo <- weo %>%
  rename(country = 2, iso3c = 1, year = "name") %>% 
  mutate(value = as.numeric(value),
         year = as.numeric(year)) %>% 
  unite(variable, 3:4, sep = "_", remove = TRUE, na.rm = FALSE) %>%
  pivot_wider(id_cols = c("country", "iso3c", "year"), names_from = "variable",
              values_from = 'value') %>% 
  rename(gdp_nc = 4, # GDP, Constant Prices, National Currency
         gdp_g = 5, # GDP, Constant prices, Percent Change
         gdp_pcp_nc = 6, # GDP Per Capita, Constant prices, National Currency
         gdp_pcp_ppp = 7, # GDP Per Capita, PPP, International Dollar
         inf_avg_idx = 8, # Inflation, Average Prices Index
         inf_avg_g = 9, # Inflation, Average Prices Percent Change
         inf_eop_idx = 10, # Inflation, End of Period, Index
         inf_eop_g = 11, # Inflation, End of Period, Percent Change
         unemp = 12, # Unemployment Rate % Total Labor Force
         population = 13, # Population
         gg_rev = 14, # General Government Revenue % GDP
         gg_exp = 15, # General Government Total Expenditure % GDP
         gg_debt = 16, # General Government Net Debt % GDP
         acc_balance = 17 # Current Account Balance % GDP
  ) %>% 
  mutate(iso3c = case_when(country == "Kosovo" ~ "KSV", .default = iso3c)) %>% 
  select(country, iso3c, year, gdp_pcp_ppp, inf_eop_g, unemp, unemp, gg_rev)

weo %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

## 4.2 Central Government Debt -------------------------------------------
cg_debt <- read_xls("raw_data/imf_central_debt.xls", sheet = 1, na = 'no data') %>%
  slice(-c(1, 176:177)) %>%
  pivot_longer(cols = c(2:73)) %>%
  rename(country = 1, year = 2, cgov_debt = 3) %>%
  mutate(year = as.numeric(year)) %>% 
  left_join(correct_names, join_by(country))

cg_debt <- cg_debt %>% 
  filter(!country %in% intersect(cg_debt$country, correct_names$country)) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
         year = as.double(year)) %>%
  rbind(cg_debt %>% filter(country %in% intersect(cg_debt$country, 
                                                 correct_names$country))) %>% 
  relocate(1, 4, 2, 3)

cg_debt %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

## 4.3 Economic Openness and Globalization ---------------------------
### 4.3.1 Economic Openness ------------------------------------------
ecopen <- wb_etl(y = 'NE.TRD.GNFS.ZS', w = 1980, z = 2024) %>% 
  rename(ecopen = 4)

intersect(ecopen$country, correct_names$country)

ecopen %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

### 4.3.2 Globalization ----------------------------------------------
globalization <- read_dta('raw_data/kof_globalization.dta') %>% 
  filter(!country %in% c("East Asia and Pacific", "Europe and Central Asia", 
                         "High income", "Latin America and Caribbean", 
                         "Low income", "Lower middle income", 
                         "Middle East and North Africa", "North America", 
                         "South Asia", "Sub-Saharan Africa", 
                         "Upper middle income", "World")) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>% 
  rename(kof_trade_df = KOFTrGIdf, # Trade globalization idx de facto
         kof_trade_dj = KOFTrGIdj, # Trade globalization idx de jure
         kof_finance_df = KOFFiGIdf, # Financial globaliztion idx de facto
         kof_finance_dj = KOFFiGIdj, # Financial globaliztion idx de jure
         kof_personal_df = KOFIpGIdf, # Interpersonal glob. idx de facto
         kof_personal_dj = KOFIpGIdj, # Interpersonal glob. idx de jure
         kof_info_df = KOFInGIdf, # Informational globalization idx de facto
         kof_info_dj = KOFInGIdj, # Informational globalization idx de jure
         kof_pol_df = KOFPoGIdf, # Political globalization idx de facto
         kof_pol_dj = KOFPoGIdj, # Political globalization idx de jure
         kof_idx = KOFGI, # Globalization Index
         kof_idx_df = KOFGIdf, # Globalization Index de facto
         kof_idx_dj = KOFGIdj) %>% # Globalization Index de jure
  select(-c(KOFCuGIdf, KOFCuGIdj, KOFEcGI, KOFEcGIdf, KOFEcGIdj,
            KOFTrGI, KOFFiGI, KOFSoGI, KOFSoGIdf, KOFSoGIdj,
            KOFIpGI, KOFCuGI, KOFPoGI,code))

globalization %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

intersect(globalization$country, correct_names$country)

## 4.4 Urban population ------------------------------------------------
urban_pop <- wb_etl(y = 'SP.URB.TOTL.IN.ZS', w = 1980, z = 2024) %>% 
  rename(urban_pop = 4)

urban_pop %>% 
  count(country, year) %>% 
  filter(n > 1)

## 4.5 Natural Resources Depletion -------------------------------------
res_depletion <- wb_etl(y = 'NY.ADJ.DRES.GN.ZS', w = 1980, z = 2024) %>% 
  rename(res_depletion = 4)

res_depletion %>% 
  count(country, year) %>% 
  filter(n > 1)

# 5. SOCIETAL COVARIATES -------------------------------------------------
## 5.1 Conflict -------------------------------------------------------
warfare <- read_xlsx("raw_data/csp_political_violence.xlsx") %>% 
  select(-scode) %>% 
  left_join(correct_names, join_by(country))

warfare <- warfare %>% 
  filter(!country %in% intersect(warfare$country, correct_names$country)) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", 
                             destination = "iso3c")) %>% 
  rbind(warfare %>% filter(country %in% intersect(warfare$country, 
                                                 correct_names$country)))

warfare %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

intersect(warfare$country, correct_names$country)

## 5.2 Dependency Ratio ---------------------------------------------------
dp_ratio <- wb_etl(y = 'SP.POP.DPND', w = 1980, z = 2024) %>% 
  rename(dp_ratio = 4)

dp_ratio %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

# 6. DATA ENRICHMENT ---------------------------------------------------
## 6.1 Easy bind -----------------------------------------------------
ws_dataset <- lac %>%
  rename(country = country.name.en) %>% 
  
  # Dependent Variables
  left_join(lac_expenditure %>% select(year, iso3c, cg_pcp_sexp, cg_prop_sexp, 
                                       cg_prop_def, cg_gdp_sexp, -country)) %>%
  
  # Independent Variables
  left_join(cmd_trade %>% select(year, iso3c, all_cmd_pcp)) %>% 
  
  # Political Covariates
  left_join(dpi %>% select(iso3c, year, system, maj)) %>% 
  
  # Political Economy
  left_join(weo %>% select(iso3c, year, gdp_pcp_ppp, inf_eop_g, unemp, 
                           -country)) %>%
  left_join(cg_debt %>% select(-country)) %>% 
  #left_join(ecopen %>% select(-country)) %>% 
  left_join(globalization %>% select(iso3c, year, kof_trade_df, kof_trade_dj,
                                     -country)) %>% 
  #left_join(res_depletion %>% select(year, iso3c, res_depletion)) %>%
  left_join(urban_pop %>% select(year, iso3c, urban_pop)) %>% 
  
  # Societal Covariates
  left_join(dp_ratio %>% select(year, iso3c, dp_ratio)) %>% 
  left_join(warfare %>% select(year, iso3c, 
                               # civviol, # Civil violence
                               # civwar, # Civil war
                               # ethviol, # Ethnic violence
                               # ethwar, # Ethnic war
                               civtot))

## 6.3 Hard Bind -----------------------------------------------------------
### 6.3.1 Non-Contributory Policies ----------------------------------------
ncp_all <- read.csv2("final_data/ncp_programmes.csv")

ws_dataset <- ws_dataset %>% 
  left_join(ncp_all %>% group_by(iso3c, year, ptype) %>%
              dplyr::summarise(ncp_count=n()
              ) %>% 
              ungroup() %>% 
              pivot_wider(names_from = ptype, values_from = ncp_count)) %>%  
  rename(n_cct = cct, n_sp = sp, n_lpi = lpi) %>% 
  mutate(n_cct = coalesce(n_cct, 0),
         n_sp = coalesce(n_sp, 0),
         n_lpi = coalesce(n_lpi, 0),
         n_ncp = n_cct + n_sp + n_lpi)

### 6.3.2 Elections --------------------------------------------------------
# Government party
ws_dataset <- leadglob %>%
  mutate(year = as.double(year)) %>%
  select(iso3c, year, HoS_name, HoS_party_short, HoS_party_english, 
         HoS_party_id, HoG_name, HoG_party_short, HoG_party_english, 
         HoG_party_id) %>%
  right_join(ws_dataset, join_by(iso3c, year)) %>%
  arrange(country, iso3c, year)

ws_dataset <- ws_dataset %>%
  mutate(leader = ifelse(system == 'Presidential', HoS_name, NA),
         leader = ifelse(system == 'Assembly-Elected President', HoS_name, leader),
         leader = ifelse(system == 'Parliamentary', HoG_name, leader),
         party_name = ifelse(system == 'Presidential', HoS_party_english, NA),
         party_name = ifelse(system == 'Assembly-Elected President', HoS_party_english, party_name),
         party_name = ifelse(system == 'Parliamentary', HoG_party_english, party_name),
         pf_party_id = ifelse(system == 'Presidential', HoS_party_id, NA),
         pf_party_id = ifelse(system == 'Assembly-Elected President', HoS_party_id, pf_party_id),
         pf_party_id = ifelse(system == 'Parliamentary', HoG_party_id, pf_party_id),
         party_short = ifelse(system == 'Presidential', HoS_party_short, NA),
         party_short = ifelse(system == 'Assembly-Elected President', HoS_party_short, party_short),
         party_short = ifelse(system == 'Parliamentary', HoG_party_short, party_short)) 

ws_dataset <- ws_dataset %>% 
  select(-HoS_name, -HoS_party_short, -HoS_party_english, -HoS_party_id, 
         -HoG_name, -HoG_party_short, -HoG_party_english, -HoG_party_id)

### 6.3.3 Party Ideology -----------------------------------------------------
ws_dataset <- ws_dataset %>%
  mutate(year1 = year)

vparty_sel <- vparty %>%
  select(iso3c, country, year, pf_party_id, v2pariglef_ord) %>%
  mutate(year2 = year)

ws_dataset <- ws_dataset %>%
  left_join(vparty_sel,
            by = join_by(country, pf_party_id, closest(year1 >= year2))) %>%
  mutate(region = countrycode(country, origin = "country.name", 
                              destination = "region23"),
         iso3c = countrycode(country, origin = "country.name", 
                             destination = "iso3c"))

ws_dataset$region[ws_dataset$country == "Mexico"] <- 'North America'

ws_dataset <- ws_dataset %>%
  select(-c(iso3c.x, iso3c.y, year.x, iso3c.y, year.y, year2)) %>%
  rename(year = year1)

ws_dataset <- ws_dataset %>%
  relocate(region, iso3c, country, year, system,
           leader, party_name, pf_party_id, party_short) %>%
  arrange(region, country, year)

# 7. DATASET EXPORT ------------------------------------------------------
saveRDS(ws_dataset, "final_data/ws_dataset.RDS")
write.csv2(ws_dataset, "final_data/ws_dataset.csv", na = '')
write_dta(ws_dataset, "final_data/ws_dataset.dta")