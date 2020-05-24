rm(list=ls())
gc()
library(tidyverse)
library(data.table)

setwd("~/win/project/LFO/Data Collection/WA/AOC Data/clean-data")

#############################################################################
########### costs data

sup_charge<-fread("1_SUPERIOR_Case_Charge_Data.txt", 
                  fill=TRUE,sep="|", quote='',
                  blank.lines.skip=TRUE, data.table=FALSE)

sup_sentence<-fread("2_Superior_Case_Sentence.txt", 
                    fill=TRUE,sep="|", quote='', 
                    blank.lines.skip=TRUE, 
                    data.table=FALSE)

priors<-read_delim("4_Prior_Convictions_Sentences.txt", delim="|")

sup_costs<-fread("7_SUPERIOR_Costs.txt", 
                 fill=TRUE,sep="|", quote='', 
                 blank.lines.skip=TRUE,
                 data.table=FALSE)

tmp<-priors%>%
  group_by(actor_key)%>%
  summarise(prior.felonies=sum(case_type=="Criminal Felony", na.rm=TRUE),
            prior.misd=sum(case_type=="Criminal Non-Traffic", na.rm=TRUE) + 
              sum(case_type=="Criminal", na.rm=TRUE) + 
              sum(case_type=="Criminal Traffic", na.rm=TRUE),
            prior.juvenile=sum(case_type=="Juvenile Offender"))


sup_sentence<-sup_sentence%>%
  mutate(sentence_fine_txt = as.numeric(sentence_fine_txt),
         cost_txt = as.numeric(cost_txt),
         attorney_fees_txt = as.numeric(attorney_fees_txt),
         sentence_fine_txt = ifelse(is.na(sentence_fine_txt), 
                                    0, sentence_fine_txt),
         cost_txt = ifelse(is.na(cost_txt), 
                           0, cost_txt),
         attorney_fees_txt = ifelse(is.na(attorney_fees_txt), 
                                    0, attorney_fees_txt)) %>% 
  mutate(trial = case_when(
    case_resolution_code%in%c("CVCT", "CVJV", "AQCT", "AQJV",
                              "GPJT", "GPAT", "DAT", "DJT") ~
      T,
    T ~ F
  )) %>% 
  select(county_name, court_name, case_key, actor_key, trial, adult_prison_flag,
         adult_jail_flag, adult_community_supervision_flag,
         case_file_date)

sup_sentence<-sup_sentence %>% 
  group_by(county_name, court_name, case_key, actor_key, 
           case_file_date) %>% 
  summarise(trial = sum(trial)>0,
            adult_prison_flag = sum(adult_prison_flag=="Y")>0,
            adult_jail_flag = sum(adult_jail_flag=="Y")>0,
            adult_community_supervision_flag = 
              sum(adult_community_supervision_flag=="Y")>0) %>% 
  distinct()

#### remove time, convert to year
sup_costs$year_filed<-substr(sup_costs$case_file_date, 1, 4)

####################################################
# Read demographics
##################################################

demo<-fread('3_Defendant_Demographics.txt', fill=TRUE,sep="|", quote='', 
            blank.lines.skip=TRUE, data.table=FALSE)

demo$race.ethn<-ifelse(demo$actor_race_txt=="Black", "Black", 
                       ifelse(demo$actor_race_txt=="American or Alaskan Native (Indian)", "AI/AN",
                              ifelse(demo$actor_ethnicity_txt=="Hispanic", "Latinx", 
                                     ifelse(demo$actor_race_txt=="Asian or Pacific Islander", "API",
                                            ifelse(demo$actor_race_txt=="White", "White", 
                                                   ifelse(demo$actor_race_txt=="Unknown", "Unknown", demo$actor_race_txt))))))

demo<-demo%>%
  select(actor_key, race.ethn, birth_date)%>%
  distinct()

##################################################
# merge, create actor/case-level data with sums for all lfo vars
####################################################
#### remove restitution accounts?

sup_costs<-sup_costs%>%
  filter(!(AR_type %in% c(
    "RESTITUTION", "RSTN-JT/SEVERAL", "INTEREST INCOME",
    "INTEREST-REST", "INT-RSTN JT/SEV"))) %>% 
  group_by(case_key, actor_key) %>% 
  summarise(AR_ordered_amt = sum(AR_ordered_amt),
            AR_adjusted = sum(AR_paid_amt + AR_balance_due - AR_interest_amt))
                          
charge_desc<-sup_charge%>%
  select(case_key, case_number, 
         offense_category_cd_desc, charge_result,
         law_title, law_number) %>% 
  filter(charge_result%in%c("GUILTY", "DV PLED PV", "DV P/P REV",
                            "GLTY-W-YIV")) %>% 
  mutate(offense_category_cd_desc = 
           ifelse(grepl("46.61.502", law_number), 
                  "DUI", offense_category_cd_desc)) %>% 
  mutate(offense_cat = case_when(
    offense_category_cd_desc=="Assault" ~ "Violent",
    offense_category_cd_desc=="Homicide" ~ "Violent",
    offense_category_cd_desc=="Robbery" ~ "Violent",
    offense_category_cd_desc=="Controlled Substance" ~ "Drugs",
    offense_category_cd_desc=="DUI" ~ "DUI",
    T ~ "Other"
  ))

charge_desc<-charge_desc %>% 
  group_by(case_key) %>% 
  summarise(Violent = sum(offense_cat == "Violent")>0,
            Drugs = sum(offense_cat == "Drugs")>0,
            DUI = sum(offense_cat == "DUI")>0,
            Other = sum(offense_cat=="Other")>0)
### merge

dat_out<-sup_costs %>% 
  left_join(charge_desc) %>% 
  left_join(demo) %>% 
  left_join(sup_sentence)

write_csv(dat_out, "~/win/project/LFO/Data Collection/WA/fe_scripts/Minnesota-ASR-Replication/WA_dat.csv")
