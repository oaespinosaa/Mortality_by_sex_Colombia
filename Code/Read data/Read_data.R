library(tidyverse)
library(readxl)
library(haven)

rm(list = ls());gc()

# Deaths ####
aux <- list()
for(year in 1980:2007){
  aux[[as.character(year)]] <- read_sav(paste0("Defun",year,".sav"))
}
for(year in 2008:2023){
  aux[[as.character(year)]] <- read_sav(paste0("nofetal",year,".sav"))
}
for (year in 1980:2023){
  colnames(aux[[as.character(year)]]) <- toupper(colnames(aux[[as.character(year)]]))
}
aux <- lapply(aux,function(x){x %>% mutate_all(as.character)})

# Select columns
cols_final <- c('COD_DPTO','COD_MUNIC','CODPTORE','CODMUNRE','ANO','MES',
                'SEXO','SEG_SOCIAL','GRU_ED1','C_BAS1')

# Categorise ICD-9 1980-1996 and ICD-10 1997-2023
icd9 <- Reduce(bind_rows,lapply(aux[1:17],function(x){subset(x,select = 'C_BAS1')})) %>%
  distinct() %>% mutate(cod3 = substr(C_BAS1,1,3)) %>%
  mutate(cause = case_when(cod3 %in% c(formatC(1:139,width = 3,flag = '0'))~'Infectious and parasitic diseases',
                           cod3 %in% c(140:175,179:239)~'Neoplasms',
                           cod3 %in% 279:289~'Diseases of the blood',
                           cod3 %in% 240:278~'Endocrine, nutritional, and metabolic diseases',
                           cod3 %in% 290:319~'Mental and behavioural disorders',
                           cod3 %in% 320:389~'Diseases of the nervous system',
                           cod3 %in% 390:459~'Diseases of the circulatory system',
                           cod3 %in% 460:519~'Diseases of the respiratory system',
                           cod3 %in% 520:579~'Diseases of the digestive system',
                           cod3 %in% 680:709~'Diseases of the skin and subcutaneous tissue',
                           cod3 %in% 710:739~'Diseases of the musculoskeletal system',
                           cod3 %in% 580:629~'Diseases of the genitourinary system',
                           cod3 %in% 630:676~'Maternal diseases',
                           cod3 %in% 760:779~'Conditions of the perinatal period',
                           cod3 %in% 740:759~'Congenital malformations',
                           cod3 %in% 780:799~'Causes of death not elsewhere classified',
                           cod3 %in% 800:999~'External causes'))
icd10 <- Reduce(bind_rows,lapply(aux[18:length(aux)],function(x){subset(x,select = 'C_BAS1')})) %>%
  distinct() %>% mutate(cod3 = substr(C_BAS1,1,3)) %>%
  mutate(cause = case_when(cod3 %in% c(paste0('A',formatC(0:99,width=2,flag='0')),
                                       paste0('B',formatC(0:99,width=2,flag='0')))~'Infectious and parasitic diseases',
                           cod3 %in% c(paste0('C',formatC(0:97,width=2,flag='0')),
                                       paste0('D',formatC(0:48,width=2,flag='0')))~'Neoplasms',
                           cod3 %in% paste0('D',50:89)~'Diseases of the blood',
                           cod3 %in% paste0('E',formatC(0:88,width=2,flag='0'))~'Endocrine, nutritional, and metabolic diseases',
                           cod3 %in% paste0('F',formatC(0:99,width=2,flag='0'))~'Mental and behavioural disorders',
                           cod3 %in% c(paste0('G',formatC(0:99,width=2,flag='0')),
                                       paste0('H',formatC(0:93,width=2,flag='0')))~'Diseases of the nervous system',
                           cod3 %in% paste0('I',formatC(0:99,width=2,flag='0'))~'Diseases of the circulatory system',
                           cod3 %in% paste0('J',formatC(0:98,width=2,flag='0'))~'Diseases of the respiratory system',
                           cod3 %in% paste0('K',formatC(0:92,width=2,flag='0'))~'Diseases of the digestive system',
                           cod3 %in% paste0('L',formatC(0:99,width=2,flag='0'))~'Diseases of the skin and subcutaneous tissue',
                           cod3 %in% paste0('M',formatC(0:99,width=2,flag='0'))~'Diseases of the musculoskeletal system',
                           cod3 %in% paste0('N',formatC(0:98,width=2,flag='0'))~'Diseases of the genitourinary system',
                           cod3 %in% paste0('O',formatC(0:99,width=2,flag='0'))~'Maternal diseases',
                           cod3 %in% paste0('P',formatC(0:96,width=2,flag='0'))~'Conditions of the perinatal period',
                           cod3 %in% paste0('Q',formatC(0:99,width=2,flag='0'))~'Congenital malformations',
                           cod3 %in% paste0('R',formatC(0:99,width=2,flag='0'))~'Causes of death not elsewhere classified',
                           cod3 %in% c(paste0('V',formatC(1:99,width=2,flag='0')),
                                       paste0('W',formatC(0:99,width=2,flag='0')),
                                       paste0('X',formatC(0:99,width=2,flag='0')),
                                       paste0('Y',formatC(0:89,width=2,flag='0')))~'External causes'))
cause_death <- bind_rows(icd9,icd10) %>% select(C_BAS1,cause) %>% arrange(C_BAS1)

# Modify classifications within each year
data <- Reduce(bind_rows,lapply(aux,function(x){
  y <- subset(x,select= cols_final[cols_final %in% colnames(x)]) %>%
    mutate(GRU_ED1 = as.numeric(GRU_ED1),
           month = as.numeric(MES),
           year = as.numeric(ANO)) %>% select(-c(ANO,MES)) %>%
    mutate(sex = case_when(SEXO == 1~'Male',
                            SEXO == 2~'Female',
                            SEXO == 3~'Indeterminate',
                            TRUE~NA)) %>% select(-SEXO)
  if('SEG_SOCIAL' %in% colnames(y)){
  y <- y %>% mutate(regime = case_when(SEG_SOCIAL == 1~'contr',
                                      SEG_SOCIAL == 2~'subs',
                                      TRUE~NA))
  }
  # Age group
  age_groups5 <- c('0','1-4',paste0(seq(5,80,5),'-',seq(9,84,5)),'85+')
  # 1980-1997
  if (unique(y$year) %in% 1980:1997){
    y <- y %>% mutate(age_group = case_when(GRU_ED1%in% 1:5~'<1',
                                            GRU_ED1%in% 6:7~'1-4',
                                            GRU_ED1%in% 8:9~'5-14',
                                            GRU_ED1%in% 10:11~'15-24',
                                            GRU_ED1%in% 12:18~'25-59',
                                            GRU_ED1%in% 19:24~'60+',
                                            GRU_ED1%in% 25~NA,
                                            TRUE~NA)) %>%
      mutate(age_group5 = case_when(GRU_ED1%in% 1:5~5,
                                    GRU_ED1%in% 6:7~7,
                                    GRU_ED1%in% 8:24~GRU_ED1,
                                    GRU_ED1%in% 25~NA,
                                    TRUE~NA)) %>%
      mutate(age_group5 =factor(age_group5,levels = c(5,7,8:24),
                                labels = age_groups5)) %>%
      select(-GRU_ED1)
  }else if (unique(y$year) %in% 1998:2007) {
    # 1998-2007
    y <- y %>% mutate(age_group = case_when(GRU_ED1%in% 1:6~'<1',
                                            GRU_ED1%in% 7:8~'1-4',
                                            GRU_ED1%in% 9:10~'5-14',
                                            GRU_ED1%in% 11:12~'15-24',
                                            GRU_ED1%in% 13:19~'25-59',
                                            GRU_ED1%in% 20:25~'60+',
                                            GRU_ED1%in% 26~NA,
                                            TRUE~NA)) %>%
      mutate(age_group5 = case_when(GRU_ED1%in% 1:6~6,
                                    GRU_ED1%in% 7:8~8,
                                    GRU_ED1%in% 9:25~GRU_ED1,
                                    GRU_ED1%in% 26~NA,
                                    TRUE~NA)) %>%
      mutate(age_group5 =factor(age_group5,levels = c(6,8,9:25),
                                labels = age_groups5)) %>%
      select(-GRU_ED1)
  }else{
  # 2008-2023
    y <- y %>% mutate(age_group = case_when(GRU_ED1%in% 1:6~'<1',
                                            GRU_ED1%in% 7:8~'1-4',
                                            GRU_ED1%in% 9:10~'5-14',
                                            GRU_ED1%in% 11:12~'15-24',
                                            GRU_ED1%in% 13:19~'25-59',
                                            GRU_ED1%in% 20:28~'60+',
                                            GRU_ED1%in% 29~NA,
                                            TRUE~NA)) %>%
      mutate(age_group5 = case_when(GRU_ED1%in% 1:6~6,
                                    GRU_ED1%in% 7:8~8,
                                    GRU_ED1%in% 9:25~GRU_ED1,
                                    GRU_ED1%in% 26:28~25,
                                    GRU_ED1%in% 29~NA,
                                    TRUE~NA)) %>%
      mutate(age_group5 =factor(age_group5,levels = c(6,8,9:25),
                                labels = age_groups5)) %>%
      select(-GRU_ED1)
  }
  return(y)
}))

# Save deaths data base
data <- data %>% mutate(cod_dpto = ifelse(is.na(COD_DPTO)|COD_DPTO=='',NA,formatC(as.numeric(COD_DPTO),width = 2,flag = '0')),
                        cod_dptore = ifelse(is.na(CODPTORE)|CODPTORE=='',NA,formatC(as.numeric(CODPTORE),width = 2,flag = '0')),
                        cod_mpio1 = ifelse(is.na(COD_MUNIC)|COD_MUNIC=='',NA,formatC(as.numeric(COD_MUNIC),width = 3,flag = '0')),
                        cod_mpiore1 = ifelse(is.na(CODMUNRE)|CODMUNRE=='',NA,formatC(as.numeric(CODMUNRE),width = 3,flag = '0'))) %>%
  mutate(cod_mpio = case_when({!is.na(cod_dpto)&!is.na(cod_mpio1)&cod_mpio1!='999'}~paste0(cod_dpto,cod_mpio1),
                              TRUE~NA),
         cod_mpiore = case_when({!is.na(cod_dptore)&!is.na(cod_mpiore1)&cod_mpiore1!='999'}~paste0(cod_dptore,cod_mpiore1),
                                TRUE~NA)) %>%
  select(-c(COD_DPTO,COD_MUNIC,CODPTORE,CODMUNRE)) %>%
  left_join(cause_death,by = 'C_BAS1')
# saveRDS(data,"deaths_1980_2023.rds")

# Population ####
# Municipality
pob1 <- read_xlsx("DCD-area-sexo-edad-proypoblacion-Mun-1985-1994.xlsx",skip = 11)
pob2 <- read_xlsx("DCD-area-sexo-edad-proypoblacion-Mun-1995-2004.xlsx",skip = 11)
pob3 <- read_xlsx("DCD-area-sexo-edad-proypoblacion-Mun-2005-2019.xlsx",skip = 11)
pob4 <- read_xlsx("DCD-area-sexo-edad-proypoblacion-Mun-2020-2035-ActPostCOVID-19.xlsx",skip = 8,
                  n_max = 53853)
colnames(pob1)[1:6] <- c("COD_DPTO","DPTO","COD_MPIO",'MPIO',"ANO","AREA")
colnames(pob2)[1:6] <- c("COD_DPTO","DPTO","COD_MPIO",'MPIO',"ANO","AREA")
colnames(pob3)[1:6] <- c("COD_DPTO","DPTO","MPIO","COD_MPIO","ANO","AREA")
colnames(pob4)[1:6] <- c("COD_DPTO","DPTO","COD_MPIO",'MPIO',"ANO","AREA")
pob <- bind_rows(pob1,pob2,pob3,pob4)
data <- pob %>% filter(AREA == "Total") %>% select(-c(COD_DPTO,DPTO,MPIO,AREA)) %>%
  pivot_longer(cols = -c(COD_MPIO,ANO),names_to = "age_sex",values_to = "N") %>%
  filter(!grepl("Total",age_sex)) %>%
  mutate(SEXO = ifelse(grepl("Hombres",age_sex),"Male","Female"),
         EDAD = as.numeric(gsub("[[:alpha:]_]","",tolower(age_sex)))) %>%
  select(-age_sex)
# saveRDS(data,"popmpio_1985_2025.rds")

# National
pob1 <- read_xlsx("DCD-area-sexo-edad-proypoblacion-Nac-1950-2019.xlsx",skip = 11)
pob2 <- read_xlsx("DCD-area-sexo-edad-proypoblacion-Nac-2020-2070.xlsx",skip = 11)
colnames(pob1)[3:4] <- c("ANO","AREA")
colnames(pob2)[3:4] <- c("ANO","AREA")
colnames(pob2)[grepl('y',colnames(pob2))] <- gsub(' y más','',colnames(pob2)[grepl('y',colnames(pob2))])
pob <- bind_rows(pob1,pob2)
data <- pob %>% filter(AREA == "Total") %>% select(-c(DP,DPNOM,AREA)) %>%
  pivot_longer(cols = -c(ANO),names_to = "age_sex",values_to = "N") %>%
  filter(!grepl("Total",age_sex)) %>%
  mutate(SEXO = ifelse(grepl("Hombres",age_sex),"Male","Female"),
         EDAD = as.numeric(gsub("[[:alpha:]_]","",tolower(age_sex)))) %>%
  select(-age_sex)
# saveRDS(data,"popnac_1950_2070.rds")
