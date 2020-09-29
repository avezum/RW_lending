##=============================================================================##
## Project:  RW Arbitrage and Bank Lending                                     ##
## Author: Lucas Avezum, Tilburg University                                    ##
## Description: this file cleans the raw dataset in .R format                  ##
##=============================================================================##

##=============================================================================##
## Code setup                                                                  ##
##=============================================================================##

rm(list = ls())  
library(tidyverse)                ## Data manipulation, pipe operator
#library(zoo)                      ## Apply function to rolling margins of data
library(DescTools)                ## Command to winsorize variables
library(readxl)                   ## Command to open xlsx files                                                     
#library(Hmisc)                    ## Weighted variance and mean functions
#library(moments)                  ## Moments functions
#library(splitstackshape)          ## Command to expand data
#source("Auxiliar/Function.R")     ## Capital requirements functions
library(lfe)                  ## High-dimensional fixed effects
# Open dirt data frame
load("Data/Temp/BankScope.Rda")
load("Data/Temp/Pillar3Data.Rda")
load("Data/Temp/DealScanData.Rda")
load("Data/Temp/AuxiliarData.Rda")


##============================================================================##
## Merge datasets                                                             ##
##============================================================================##

loan.data <- lender.data %>%
  select(tranche_id, bvdid, lender_parent_name, lender_share, primary_role) %>%
  filter(between(lender_share, 0, 100)) %>%
  inner_join(
    select(tranche.data, tranche_id, tranche_active_date, tranche_amount_converted_musd, country),
    by = c("tranche_id")) %>%
  mutate_if(is.numeric,Winsorize,probs = c(0.001, 0.999) ,na.rm = TRUE)%>%
  mutate(month              = as.numeric(sapply(tranche_active_date, str_sub, start= 6, end=7)),
         year               = as.numeric(sapply(tranche_active_date, str_sub, start= 1, end=4)),
         year               = ifelse(is.na(month), year, ifelse(month<6, year-1, year)),
         lender_amount_musd = tranche_amount_converted_musd*lender_share/100)%>%
  group_by(tranche_id, bvdid, lender_parent_name, country, year)%>%
  summarise(lender_amount_musd = sum(lender_amount_musd, na.rm = TRUE)) %>%
  full_join(select(ungroup(pillar3.data), -c("Country", "name")), by = c("bvdid", "year")) %>%
  full_join(select(distinct(ungroup(pillar3.data), bvdid, .keep_all = TRUE), bvdid, name), by = c("bvdid")) %>%
  left_join(select(IRB.indicator, bvdid = bvdid_new, year, IRB, Country), by = c("bvdid", "year"))%>% 
  left_join(basel.indicator, by = c("Country"))%>%
  left_join(BRSS, by = c("Country", "year"))%>%
  left_join(select(filter(WDI, Country=="US"), year, cpi), by = c("year")) %>%
  group_by(lender_parent_name, year)%>%
  mutate(RW                = ifelse(IRB==0, 1, RW),
         mean.RW           = ifelse(IRB==0, 1,mean.RW),
         mean.RW_adj       = ifelse(IRB==0, 1,mean.RW_adj),
         basel             = ifelse(year>=basel, 1, 0),
         us.indicator      = ifelse(Country == "US", "US", "Other"),
         year.factor       = as.factor(year),
         n_loans           = n(),
         lender_amount     = lender_amount_musd/cpi,
         log.lender_amount = ifelse(lender_amount>0, log(lender_amount), NA)
    )%>%
  group_by(tranche_id)%>%
  mutate(SA.indicator = mean(RW, na.rm = TRUE)) %>%
  group_by(lender_parent_name)%>% 
  arrange(lender_parent_name, year) %>%
  fill(RW, .direction = "up")

save(loan.data, file=paste0("Data/Datasets/LoanData.Rda"))
#------------------------------------------------------------------------------#
# scrap                                                                        #
#------------------------------------------------------------------------------#



currency <- tranche.data %>% ungroup() %>%
  select(tranche_currency) %>%  distinct(tranche_currency, .keep_all = TRUE) 

p <- ggplot(data = data, 
            mapping = aes(x = RW, y = lender_share))
p + geom_point() + geom_smooth(method = "lm")
  
p <- ggplot(data = filter(pillar3.data,!is.na(RW), Country == "CA"), 
            mapping = aes(y = RW, x = year))
p + geom_line()+ facet_wrap(~name) 

data %>% 
  #filter(!is.na(Country))%>% 
  mutate(us.indicator = ifelse(Country == "US", "US", "Other"),
         currency.indicator = ifelse(tranche_currency == "U.S. Dollar", "U.S. Dollar", "Other"))%>%
  ggplot(mapping = aes(x = year, y = (lender_amount_musd/1000) )) + 
  geom_col() +
  facet_wrap(~ currency.indicator)

# Run the models
 summary(data)
  
 summary(felm(log.lender_amount_musd ~ RW | bvdid + tranche_id | 0 | 0,
      data = filter(loan.data, year> 1994, country != country.name)), robust = TRUE )

 
 summary(felm(log.lender_amount_musd ~ 1 | bvdid + tranche_id | (RW ~ mean.RW + IRB) | bvdid,
              data = filter(loan.data, SA.indicator < 1, country != country.name))$stage1) 
 
 summary(felm(log.lender_amount_musd ~ 1 | bvdid + tranche_id | (RW ~ basel) | 0,
              data = filter(loan.data, year> 1994,  country != country.name))$stage1)
  
  summary(felm(log.lender_amount_musd ~ RW | bvdid + tranche_id | 0 | 0,
               data = filter(data, SA.indicator < 1
                             ,primary_role == "Participant"))) 
  
  summary(felm(log.lender_amount_musd ~ 1 | bvdid + tranche_id | (RW ~ mean.RW + IRB) | 0,
               data = filter(data 
                            #,Country != "US"
                            ,primary_role == "Participant"
                            ,SA.indicator < 1)))  
  
summary(felm(log.lender_amount_musd ~ RW:us.indicator | bvdid + year | 0 | 0,
               data = filter(data, SA.indicator < 1)))

summary(felm(log.lender_amount_musd ~ RW:Country | bvdid + year | 0 | 0,
             data = filter(data, SA.indicator < 1)))

summary(felm(log.lender_amount_musd ~ RW:year.factor | bvdid + tranche_id | 0 | 0,
             data = filter(data, SA.indicator < 1
                           #,Country != "US"
             )))

summary(felm(log.share ~ RW | bvdid + tranche_id | 0 | 0, data = data))

summary(felm(lender_amount_EAD ~ RW | bvdid + tranche_id | 0 | 0,
             data = filter(data, SA.indicator < 1))) 

p <- ggplot(data = filter(data, year > 2007, !is.na(bvdid)
                          , Country != "US"
                          ), 
            mapping = aes(x =  SA.indicator))
p + geom_histogram() 

  p <- ggplot(data = filter(data, year > 2007, !is.na(bvdid)), 
              mapping = aes(x =  log.lender_amount_EAD))
  p + geom_histogram() 
  +facet_wrap(~ Country)
  
  p <- ggplot(data = data, 
              mapping = aes(x = year, y = lender_amount_musd ))
  p + geom_col() 
  
#------------------------------------------------------------------------------#
# Aggregate Model                                                              #
#------------------------------------------------------------------------------#  
  
bank.data <- lender.data %>%
  select(tranche_id, bvdid, lender_parent_name, lender_share) %>%
  filter(between(lender_share, 0, 100)) %>%
  inner_join(
    select(tranche.data, tranche_id, tranche_active_date, tranche_amount_converted_musd),
    by = c("tranche_id")) %>%
  mutate_if(is.numeric,Winsorize,probs = c(0.01, 0.99) ,na.rm = TRUE)%>%
  mutate(month       = as.numeric(sapply(tranche_active_date, str_sub, start= 6, end=7)),
         year        = as.numeric(sapply(tranche_active_date, str_sub, start= 1, end=4)),
         year        = ifelse(is.na(month), year, ifelse(month<6, year-1, year)),
         lender_amount_musd     = tranche_amount_converted_musd*lender_share/100) %>%
  group_by(lender_parent_name, bvdid, year)%>%
  summarise(lender_amount_musd = sum(lender_amount_musd, na.rm = TRUE),
            lender_share = mean(lender_share, na.rm = TRUE),
            n_loans = n())%>%
  full_join(select(ungroup(pillar3.data), -Country), by = c("bvdid", "year")) %>%
  left_join(select(IRB.indicator, bvdid = bvdid_new, year, IRB, Country), by = c("bvdid", "year"))%>% 
  left_join(basel.indicator, by = c("Country"))%>%
  mutate(RW = ifelse(IRB==0, 1, RW),
         basel = ifelse(year>=basel, 1, 0),
         log.lender_amount_musd = log(lender_amount_musd),
         log.lender_share = log(lender_share))%>%
  group_by(lender_parent_name)%>%
  fill(RW, .direction = "up")

save(bank.data, file=paste0("Data/Datasets/BankData.Rda"))

p <- ggplot(data = data.aggregate, 
            mapping = aes(x =  log.lender_amount_musd))
p + geom_histogram() 

p <- ggplot(data = data.aggregate, 
            mapping = aes(x = year, y = mean.lender_amount_musd ))
p + geom_col() 

# Run the models
summary(filter(data.aggregate, !is.na(bvdid), year>1994))
summary(felm(lender_share ~ RW | bvdid + year | 0 | 0, data = filter(data.aggregate, year>1994)))

summary(felm(log.lender_amount_musd ~ RW | bvdid + year | 0 | 0 , data = filter(data.aggregate, year>2007)))  


country.data <- lender.data %>%
  select(tranche_id, bvdid, lender_parent_name, lender_share) %>%
  filter(between(lender_share, 0, 100)) %>%
  inner_join(
    select(tranche.data, tranche_id, tranche_active_date, tranche_amount_converted_musd),
    by = c("tranche_id")) %>%
  mutate_if(is.numeric,Winsorize,probs = c(0.01, 0.99) ,na.rm = TRUE)%>%
  mutate(month       = as.numeric(sapply(tranche_active_date, str_sub, start= 6, end=7)),
         year        = as.numeric(sapply(tranche_active_date, str_sub, start= 1, end=4)),
         year        = ifelse(is.na(month), year, ifelse(month<6, year-1, year)),
         lender_amount_musd     = tranche_amount_converted_musd*lender_share/100) %>%
  left_join(select(IRB.indicator, bvdid = bvdid_new, year, IRB, Country), by = c("bvdid", "year"))%>%
  group_by(Country, year)%>%
  summarise(sum.lender_amount_musd = sum(lender_amount_musd, na.rm = TRUE),
            mean.lender_amount_musd = mean(lender_amount_musd, na.rm = TRUE),
            lender_share = mean(lender_share, na.rm = TRUE),
            n_loans = n())

save(country.data, file=paste0("Data/Datasets/CountryData.Rda"))

country.aggregate %>% 
  filter(!is.na(Country),
         year>2007)%>% 
  mutate(us.indicator = ifelse(Country == "US", "US", "Other"))%>%
ggplot(mapping = aes(x = year, y = mean.lender_amount_musd )) + 
  geom_col()  +
  facet_wrap(~ Country)

