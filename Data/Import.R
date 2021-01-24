##-----------------------------------------------------------------------------##
## Project: RW Arbitrage and Bank Lending                                      ##
## Author: Lucas Avezum, Tilburg University                                    ##
## Description: this file opens, merges and saves in .Rda format               ##
##-----------------------------------------------------------------------------##
  
##=============================================================================##
## Code setup                                                                  ##
##=============================================================================##


library(tidyverse)    ## Data manipulation, pipe operator                                                             
library(readxl)       ## Command to open xlsx files                                                     
library(data.table)   ## Command to bind lists
library(stringr)      ## Command to clean variables names in Orbis
library(zoo)          ## Command to replace missing values by other group's mean value and rolling average

rm(list = ls()) 

##============================================================================##
## Auxiliar data                                                              ##
##============================================================================##

# Matching datasets
parent.lender.link <- read_excel("Data/Raw/Links/DealScan_Parent_Lender_Link.xlsx", 
                                 sheet = "BankScope") %>%
  mutate(lender_parent_name = tolower(str_replace_all(lender_parent_name,"[[:space:]]",""))) %>%
  distinct(lender_parent_name, .keep_all = TRUE)

ifs.country.code <- read_excel("Data/Raw/IFS/ifs_code.xlsx", skip = 1) %>%
  select(contains("Code")) %>%
  rename_all(tolower)%>%
  rename_all(~ str_replace_all(.,"[:blank:]", "\\_"))%>%
  mutate(country_code = as.numeric(imf_code),
         country_code = ifelse(iso_code %in% c("BEL", "DEU", "ESP", "FRA", "FIN", "IRL", "ITA"), 163, country_code),
         long_country_code = iso_code)

country.code <- read.csv("Data/Raw/WDI/WDICountry.csv", fileEncoding="UTF-8-BOM") %>%
  rename_all(tolower)%>%
  rename(long_country_code = country.code,
         short_country_code = x2.alpha.code,
         country_name = short.name)%>%
  rename_all(~ str_replace_all(.,"\\.", "\\_"))%>%
  mutate(currency_unit = ifelse(long_country_code=="EMU", "Euro", as.character(currency_unit)))

# IRB adoption year
IRB.indicator <- read_xlsx("Data/Raw/Pillar3/Auxiliar/Auxiliar.xlsx", sheet = 1)  %>%
  pivot_longer(cols      = where(is.numeric),
               names_to  = "year",
               values_to = "IRB")%>%
  select(bank_name = name, country_code_lender = Country, bvdid_new, bvdid_old, year, IRB)%>%
  mutate(year = as.numeric(year))

# Basel II introduction
basel.indicator <- read_xlsx("Data/Raw/Pillar3/Auxiliar/Auxiliar.xlsx", sheet = 2) %>%
  select(country_code_lender = Country, basel = year)

# SSM introduction
SSM.indicator <- read_xlsx("Data/Raw/Pillar3/Auxiliar/Auxiliar.xlsx", sheet = 4)  %>%
  pivot_longer(cols      = where(is.numeric),
               names_to  = "year",
               values_to = "SSM")%>%
  select(bvdid, year, SSM)%>%
  mutate(year = as.numeric(year))

# World Development Indicators
WDI <- read.csv("Data/Raw/WDI/WDIData.csv", fileEncoding = "UTF-8-BOM", check.names = FALSE) %>%
  select(!(""))%>%
  rename_all(tolower)%>%
  rename_all(~ str_replace_all(.,"[:blank:]", "\\_"))%>%
  filter(indicator_code %in% c("NY.GDP.DEFL.ZS","NY.GDP.PCAP.KD","NY.GDP.MKTP.KD.ZG","PA.NUS.FCRF", "NY.GDP.MKTP.CN",
                               "PX.REX.REER","FS.AST.DOMS.GD.ZS","FS.AST.PRVT.GD.ZS","FD.AST.PRVT.GD.ZS","FP.CPI.TOTL")) %>%
  pivot_longer(cols      = where(is.numeric),
               names_to  = "year") %>%
  select(long_country_code = country_code, indicator_name, year, value) %>%
  pivot_wider(names_from = indicator_name,
              values_from = value) %>% 
  rename_all(~ str_replace_all(.,"[:blank:]\\(.*\\)",""))%>%
  rename_all(~ str_replace_all(.,"[:blank:]", "\\_"))%>%
  rename_all(tolower)%>%
  inner_join(select(country.code, long_country_code, short_country_code, currency_unit))%>%
  mutate(group_var     = ifelse(is.na(official_exchange_rate)| long_country_code == "EMU", 
                                  paste(currency_unit, year, sep = ""), NA),
         exchange_rate = na.aggregate(official_exchange_rate, by = group_var),
         country_code  = short_country_code,
         year          = as.numeric(year)) %>%
  select(-group_var, -currency_unit, -official_exchange_rate, -long_country_code)

# IFS dataset
IFS <- read.csv("Data/Raw/IFS/IFS.csv", fileEncoding="UTF-8-BOM", check.names = FALSE) %>%
  select(!("")) %>%
  rename_all(tolower) %>%
  rename_all(~ str_replace_all(.,"[:blank:]", "\\_")) %>%
  filter(attribute %in% c("Value")) %>%
  filter(indicator_code %in% c("FPOLM_PA", "FIDR_PA", "FIDFR_PA")) %>%
  select(-country_name, -attribute, -indicator_name) %>%
  mutate(across(where(is.character) & !contains("indicator"),as.numeric)) %>%
  pivot_longer(cols      = -c("country_code", "indicator_code"),
               names_to = c("year", "month"),
               names_pattern = "(.*)m(.*)",
               values_to = "policy_rate") %>%
  pivot_wider(names_from = "indicator_code",
              values_from = "policy_rate") %>%
  mutate(policy_rate = ifelse(country_code == 163, FIDFR_PA, ifelse(is.na(FPOLM_PA),FIDR_PA,FPOLM_PA))) %>%
  inner_join(select(ifs.country.code, country_code, long_country_code)) %>%
  inner_join(select(country.code, long_country_code, short_country_code)) %>%
  select(country_code = short_country_code, year, month, policy_rate) %>%
  ungroup() %>% 
  arrange(country_code, year, month) %>% 
  group_by(country_code) %>%
  mutate(policy_rate.ma3    = rollapply(policy_rate, 3, mean, align='right', fill=NA),
         policy_rate.cma6   = rollapply(policy_rate, 6, mean, align='center', fill=NA),
         policy_rate.delta3 = policy_rate - lag(policy_rate, n = 2),
         year               = as.numeric(year),
         month              = as.numeric(month))
 
# Bank Regulation and Supervision
load("Data/Raw/BRSS/Output/BRSS.Rdata") 

country.names <- BRSS %>%
     rename_all(tolower) %>%
     select(country_name = country.name,
            country_code = country)%>%
  distinct(country_name, .keep_all = TRUE)

BRSS <- BRSS %>%
  rename_all(tolower) %>%
  rename(country_code = country)%>%
  select(-country.name)

save(list = c("IRB.indicator", "WDI", "BRSS", "country.names", "basel.indicator", "IFS", "SSM.indicator"),
     file=paste0("Data/Temp/AuxiliarData.Rda"))

##============================================================================##
## Dealscan dataset                                                           ##
##============================================================================##

# Import collected datasets from xls files and merge all in data frame dealscan.data

data.path <-  "Data/Raw/Dealscan" 
dealscan.data <- list()

for(i in list.files(path = data.path)) {

data.names <-  list.files(path = paste(data.path, i, sep = "/"), pattern="\\.xls", full.names = TRUE)
variables.names <- names(read_xls(data.names[1], n_max = 0))
column.type <- ifelse(grepl("Date", variables.names), "date", "guess")

dealscan.data[[i]] <- data.names %>%
  lapply(read_xls, sheet = 1, col_types = column.type) %>%   
  rbindlist(use.names=TRUE, fill = TRUE) 
}

MyFunc <- function(data) {
  data %>% group_by(`Tranche Id`) %>%
    mutate(id_aux = n():1) 
}
tranche.data <- dealscan.data[which(!grepl("Deal|Lender$|Borrower", names(dealscan.data)))]%>%
  map(MyFunc) %>%
  reduce(full_join, by = c("Tranche Id", "Deal Id", "id_aux"))

tranche.data <- dealscan.data[which(grepl("Borrower", names(dealscan.data)))]%>%
  map(MyFunc) %>%
  reduce(full_join, by = c("Tranche Id", "Deal Id", "Borrower Id", "id_aux")) %>%
  full_join(tranche.data, by = c("Tranche Id", "Deal Id", "id_aux")) %>%
  drop_na(`Tranche Id`) %>%
  filter(`Tranche O/A` == "Origination") %>%
  rename_all(funs(str_replace_all(.,"[:punct:]","")))%>%
  rename_all(funs(str_replace_all(.,"[[:space:]]","_")))%>%
  rename_all(tolower)

deal.data <- dealscan.data[which(grepl("Deal", names(dealscan.data)))]%>%
  reduce(full_join, by = c("Deal Id")) %>%
  distinct(`Deal Id`, .keep_all = TRUE) %>%
  rename_all(funs(str_replace_all(.,"[:punct:]","")))%>%
  rename_all(funs(str_replace_all(.,"[[:space:]]","_")))%>%
  rename_all(tolower)

lender.data <- dealscan.data[which(grepl("Lender$", names(dealscan.data)))]%>%
  reduce(full_join, by = c("Tranche Id")) %>%
  rename_all(funs(str_replace_all(.,"\\([:graph:]\\)","")))%>%
  rename_all(funs(str_replace_all(.,"[[:space:]]","_")))%>%
  rename_all(tolower) %>%
  mutate(lender_share = as.numeric(ifelse(lender_share == "N/A", NA, lender_share)),
         lender_commit = as.numeric(str_replace_all(lender_commit,"[[:alpha:]]|[[:blank:]]","")),
         lender_parent_name = tolower(str_replace_all(lender_parent_name,"[[:space:]]",""))) %>%
  full_join(parent.lender.link, by = c("lender_parent_name"))

save(list = c("tranche.data", "deal.data", "lender.data"),
     file=paste0("Data/Temp/DealScanData.Rda"))

##============================================================================##
## Pillar-III reports                                                         ##
##============================================================================##

# Import collected datasets from xlsx files and merge all in data frame data.pillar3
data.path  <- "Data/Raw/Pillar3"
data.names <-  list.files(path = data.path,pattern="\\.xlsx",full.names = TRUE)
data.list  <- data.names %>%
  lapply(read_xlsx,sheet = 1,  skip=1)   
pillar3.data <-  rbindlist(data.list, use.names=TRUE,fill = TRUE) %>%
  select(1:25) %>%
  mutate_at(vars(EAD:PD), as.numeric)%>%
  rename(portfolio_1         = `Portfolio - level 1`,
         portfolio_2         = `Portfolio - level 2`,
         country_code_lender = Country) %>%
  mutate(month       = as.numeric(sapply(Date, str_sub, start= 4, end=5)),
         year        = as.numeric(sapply(Date, str_sub, start= -4)),
         year        = ifelse(is.na(month), year, ifelse(month<6, year-1, year)),
         bvdid       = as.factor(bvdid),
         Method      = as.factor(Method),
         method2     = as.factor(ifelse(Method %in% c("IRB"), "IRB", sapply(Method, str_sub, start= 2, end=4))),
         portfolio_1 = as.factor(portfolio_1),
         portfolio_2 = as.factor(portfolio_2))

# Add short names to data frame and currency and inflation conversion
pillar3.data <- IRB.indicator %>%
  select(bank_name, bvdid = bvdid_new) %>% 
  distinct(bvdid, .keep_all = TRUE) %>%
  inner_join(pillar3.data) %>%
  # Add macro variables
  left_join(select(WDI, country_code_lender = country_code, year, exchange_rate), 
            by = c("country_code_lender", "year")) %>%
  # Convert to USD
  mutate(EAD = EAD/exchange_rate,
         RWA = RWA/exchange_rate)%>%
  filter(method2 == "IRB", portfolio_1 == "Wholesale", portfolio_2 == "Corporate" | portfolio_2 == "Total") %>%
  group_by(bank_name, bvdid, country_code_lender, year) %>%
  summarise(RWA = sum(RWA, na.rm = TRUE),
            EAD = sum(EAD, na.rm = TRUE)) %>%
  mutate(RW = ifelse(RWA == 0 | EAD == 0, NA, RWA/EAD)) %>%
  group_by(bvdid) %>% 
  arrange(bvdid, year) %>%
  fill(RW, .direction = "up") %>%
  group_by(country_code_lender, year) %>%
  mutate(mean.RW     = mean(RW, na.rm = TRUE),
         n_banks     = n(),
         mean.RW_adj = (n_banks*mean.RW-RW)/(n_banks-1)) %>%
  ungroup()

# Save data frame
save(pillar3.data, file=paste0("Data/Temp/Pillar3Data.Rda"))


##============================================================================##
## Orbis Bank Focus (BankScope)                                               ##
##============================================================================##

# Import Orbis dataset from excel files 
data.path  <- "Data/Raw/BankScope"
data.names <-  list.files(path = data.path,pattern = ".*(201.)\\.xlsx", full.names = TRUE)
data.list  <- data.names %>%
  lapply(read_xlsx, sheet = 1) 

#------------------------------------------------------------------------------#
# Reshape data from wide to long                                               #
#------------------------------------------------------------------------------#

for(i in 1:length(data.list)) {
  data.list[[i]] <- data.list[[i]] %>%
    # Remove row index, set variables names to lower case, rename id variable, and keep only selected sample
    rename_all(tolower)  %>%
    rename_at(vars(contains("bvd")), 
              funs(str_replace(.,"bvd .*", "bvdid"))) %>%
    mutate(disk = paste(i)) %>% 
    select(disk, everything()) %>%
    # Standardize variables names to reshape dataset from wide to long
    rename_all(~ str_replace_all(., "[:space:]|[:punct:]|year|=", ""))  %>%
    rename_at(vars(contains("lastavailyr")), ~ str_replace(.,"last.*", "10")) %>%
    rename_all(~ str_replace_all(., "([\\w])([0-9]+)$", "\\1\\.\\2")) %>%
    # Turn numeric variables to numeric format
    mutate_at(c(grep("usd", names(.))[1]:ncol(.)), as.numeric) %>%
    # Reshape datasets to long format
    pivot_longer(cols = c(grep("\\.\\d", names(.))),
                 names_to = c(".value", "year"),
                 names_pattern = "(.+)\\.(.+)")
  # Standardize variable names across datasets
  names(data.list[[i]]) <- names(data.list[[1]])
}

# Bind datasets from different disks  
bankscope <- rbindlist(data.list, use.names=TRUE, fill = TRUE)

#------------------------------------------------------------------------------#
# First layer of data cleaning                                                 #
#------------------------------------------------------------------------------#

bankscope <- bankscope %>%
  # Create and correct year variable
  mutate(month = as.numeric(sapply(closingdate, str_sub, start= 6, end=7)),
         year  = as.numeric(sapply(closingdate, str_sub, start= 1, end=4)),
         year  = ifelse(is.na(month), year, ifelse(month<6, year-1, year))) %>%
  # Harmonize bvdid (for some banks and countries it has changed from one disk to another)
  full_join(IRB.indicator, by = c("bvdid" = "bvdid_old", "year")) %>%
  full_join(IRB.indicator, by = c("bvdid" = "bvdid_new", "year")) %>%
  mutate(bvdid = ifelse(is.na(bvdid_old), ifelse(is.na(bvdid_new), bvdid, bvdid_new), bvdid)) %>%
  # Keep unique bank-year observations from sample, and remove strange observations from 1969 (?!?)
  filter(bvdid %in% IRB.indicator$bvdid_new, year != 1969, conscode %in% c("C1","C2","U1")) %>%
  group_by(bvdid, year) %>% 
  arrange(desc(disk), .by_group = TRUE) %>%
  distinct(bvdid, year, .keep_all = TRUE) %>% 
  ungroup() %>%
  # Fix variables from the IRB indicator data frame
  mutate(bank_name    = ifelse(is.na(bank_name.x), bank_name.y, bank_name.x),
         country_code_lender = ifelse(is.na(country_code_lender.x), country_code_lender.y, country_code_lender.x)) %>%
  # Reorder variables and remove auxiliar variables
  select(bank_name,  country_code_lender, everything(), -contains("."),
         -c("companyname", "countryisocode","lastavail", "guoname", "conscode",
            "status", "listeddelistedunlisted", "delisteddate", "guobvdid", "guocountryisocode",
            "guotype", "closingdate", "month", "1", "bvdid_new", "bvdid_old")) %>%
  ungroup()


# Save data frame
save(bankscope,file=paste0("Data/Temp/BankScope.Rda"))



