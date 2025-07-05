library(tidyverse)
rm(list = ls());gc()

# Set variables ####
scl <- 1e5
scl6 <- 1e6
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
ggcolors3 <- gg_color_hue(3)
ggcolors10 <- gg_color_hue(10)
# Age groups
age_groups <- c('<1','1-4','5-14','15-24','25-59','60+')
break_ages <- c(0,1,5,15,25,60,125)
age_groups5 <- c('0','1-4',paste0(seq(5,80,5),'-',seq(9,84,5)),'85+')
break_ages5 <- c(0,1,seq(5,85,5))
# yll
airp <- data.frame(age_group = age_groups,
                   AIRP = c((break_ages[-c(6,7)]+break_ages[-c(1,7)]-1)/2,60))
# sex
sexs <- c('Male','Female')
# Urban/rural regions
regions <- read.csv('../Data/Categorias_de_Ruralidad.csv',sep = ";") %>%
  mutate(mpio = as.character(Municipali),dpto = as.character(Departam_1),
         cod_mpio = formatC(Municipaly,width = 5,flag = '0'),
         region = ifelse(Categoria_ %in% c('Rural','Rural Disperso'),'Rural',
                      'Urban')) %>%
  select(cod_mpio,dpto,mpio,region) %>%
  add_row(cod_mpio = '27493',dpto = 'Chocó',mpio = 'Nuevo Belén de Bajirá',region = 'Rural')
# --------------------------------- Data ---------------------------------------
cods11 <- c('11102','11265','11279','11769','11848','11850')
deaths_tot <- readRDS('../Data/deaths_1980_2023.rds') %>% 
  filter(sex %in% sexs,!is.na(age_group5)) %>% 
  # Cause external, non-exteral
  mutate(cause_ext = ifelse(cause == 'External causes','External causes','Non-external causes')) %>%
  # Fix divipola
  mutate(cod_mpiof = ifelse(is.na(cod_mpiore)|cod_dptore%in%c('01','75'),cod_mpio,cod_mpiore),
         cod_dptof = case_when(!is.na(cod_mpiof)~substr(cod_mpiof,1,2),
                               is.na(cod_mpiof)&!is.na(cod_dptore)&!cod_dptore%in%c('01','75')~cod_dptore,
                               is.na(cod_mpiof)&{is.na(cod_dptore)|cod_dptore%in%c('01','75')}&!is.na(cod_dpto)~cod_dpto,
                               TRUE~NA)
         ) %>%
  # Caqueta
  mutate(cod_mpiof = ifelse(year<=1981&cod_dptof == 83,paste0(18,substr(cod_mpiof,3,6)),cod_mpiof),
         cod_dptof = ifelse(year<=1981&cod_dptof == 83,'18',cod_dptof),
         cod_mpiof = ifelse(cod_mpiof %in% c('18767'),'18785',cod_mpiof),
         cod_mpiof = ifelse(cod_mpiof %in% c('18765'),'18756',cod_mpiof)) %>%
  # Santa Fe de Bogota
  mutate(cod_mpiof = ifelse(cod_mpiof %in% cods11,'11001',cod_mpiof)) %>%
  # Vichada
  mutate(cod_mpiof = ifelse(cod_mpiof%in%c('99572','99760'),'99773',cod_mpiof),
         cod_mpiof = ifelse(cod_mpiof%in%c('99496'),'99524',cod_mpiof)) %>%
  # Choco
  mutate(cod_mpiof = ifelse(cod_mpiof %in% '27086','27493',cod_mpiof)) %>%
  # Antioquia
  mutate(cod_mpiof = ifelse(cod_mpiof %in% '05625','05665',cod_mpiof))
popnac <- readRDS('../Data/popnac_1950_2070.rds') %>%
  mutate(age_group = cut(EDAD,breaks = break_ages,right = F,labels = age_groups)) %>%
  mutate(age_group5 = cut(EDAD,breaks = c(break_ages5,125),right = F,labels = age_groups5)) %>%
  group_by(year=ANO,sex=SEXO,age_group,age_group5) %>%
  summarise(pop = sum(N),.groups = 'drop') %>% filter(year %in% 1980:2023)
popmpio <- readRDS('../Data/popmpio_1985_2025.rds') %>%
  mutate(age_group = cut(EDAD,breaks = break_ages,right = F,labels = age_groups)) %>%
  group_by(cod_mpio = COD_MPIO,year=ANO,sex=SEXO,age_group) %>%
  summarise(pop = sum(N),.groups = 'drop') %>% filter(year <= 2023)
popbdua <- readRDS('../Data/popBDUA_2012_2024.rds') %>%
  mutate(year = as.numeric(year),
         age_group = cut(age,breaks = break_ages,right = F,labels = age_groups),
         regime = case_when(regime == 'CONTRIBUTIVO'~'contr',
                            regime == 'SUBSIDIADO'~'subs',
                            TRUE~NA),
         sex = case_when(sex == 'FEMENINO'~'Female',
                         sex == 'MASCULINO'~'Male',
                         TRUE~NA)) %>%
  group_by(year,regime,sex,age_group) %>%
  summarise(afil = sum(afil),.groups = 'drop') %>% filter(year <= 2023)

# Reference population ####
popref <- popnac %>% filter(year == 2000) %>% 
  group_by(age_group) %>% summarise(pop_ref = sum(pop),.groups = 'drop') %>%
  mutate(pop_tot = sum(pop_ref),prop_ref = pop_ref/pop_tot) %>% 
  select(age_group,pop_ref,prop_ref)

# ------------------------ Life expectancy at birth ----------------------------
# Year - Sex ####
lifeexp_sex <- popnac %>% group_by(year,sex,age_group5) %>%
  summarise(N = sum(pop),.groups = 'drop') %>%
  left_join(deaths_tot %>% group_by(year,sex,age_group5) %>% 
              summarise(D = n(),.groups = "drop"),by = c('year','sex','age_group5')) %>%
  mutate(year_aux = year) %>%
  left_join(data.frame(age_group5 = age_groups5,
                       age_li = break_ages5),by = 'age_group5') %>% select(-age_group5) %>%
  group_by(year,sex) %>% nest() %>%
  mutate(data = map(data,function(x){
    demog_data <- demography::demogdata(data = matrix(x$D/x$N, ncol = 1), 
                                        pop = matrix(x$N, ncol = 1), 
                                        ages = x$age_li,
                                        years = unique(x$year_aux),
                                        type="mortality",
                                        name="0",
                                        label="",
                                        lambda = 0)
    x$le <- 
      as.vector(unname(demography::lifetable(data = demog_data, 
                                             ages = demog_data$age,
                                             max.age = 85, 
                                             type = "period")$ex))
    return(x)})) %>% 
  unnest(data) %>% filter(age_li == 0) %>% ungroup %>%
  select(year,sex,LE0 = le)
# Year ####
lifeexp <- popnac %>% group_by(year,age_group5) %>%
  summarise(N = sum(pop),.groups = 'drop') %>%
  left_join(deaths_tot %>% group_by(year,age_group5) %>% 
              summarise(D = n(),.groups = "drop"),by = c('year','age_group5')) %>%
  mutate(year_aux = year) %>%
  left_join(data.frame(age_group5 = age_groups5,
                       age_li = break_ages5),by = 'age_group5') %>% select(-age_group5) %>%
  group_by(year) %>% nest() %>%
  mutate(data = map(data,function(x){
    demog_data <- demography::demogdata(data = matrix(x$D/x$N, ncol = 1), 
                                        pop = matrix(x$N, ncol = 1), 
                                        ages = x$age_li,
                                        years = unique(x$year_aux),
                                        type="mortality",
                                        name="0",
                                        label="",
                                        lambda = 0)
    x$le <- 
      as.vector(unname(demography::lifetable(data = demog_data, 
                                             ages = demog_data$age,
                                             max.age = 85, 
                                             type = "period")$ex))
    return(x)})) %>% 
  unnest(data) %>% filter(age_li == 0) %>% ungroup %>%
  select(year,LE0 = le)
# Decade ####
lifeexp_decade <- popnac %>% group_by(year,age_group5) %>%
  summarise(pop = sum(pop),.groups = 'drop') %>%
  mutate(decade = floor(year/10)*10) %>%
  group_by(decade,age_group5) %>%
  summarise(N = sum(pop),.groups = 'drop') %>%
  left_join(deaths_tot %>% mutate(decade = floor(year/10)*10) %>%
              group_by(decade,age_group5) %>% 
              summarise(D = n(),.groups = "drop"),by = c('decade','age_group5')) %>%
  mutate(year_aux = decade) %>%
  left_join(data.frame(age_group5 = age_groups5,
                       age_li = break_ages5),by = 'age_group5') %>% select(-age_group5) %>%
  group_by(decade) %>% nest() %>%
  mutate(data = map(data,function(x){
    demog_data <- demography::demogdata(data = matrix(x$D/x$N, ncol = 1), 
                                        pop = matrix(x$N, ncol = 1), 
                                        ages = x$age_li,
                                        years = unique(x$year_aux),
                                        type="mortality",
                                        name="0",
                                        label="",
                                        lambda = 0)
    x$le <- 
      as.vector(unname(demography::lifetable(data = demog_data, 
                                             ages = demog_data$age,
                                             max.age = 85, 
                                             type = "period")$ex))
    return(x)})) %>% 
  unnest(data) %>% filter(age_li == 0) %>% ungroup %>%
  select(decade,LE0 = le)

# ---------------------------------- Analysis ----------------------------------
deaths <- deaths_tot %>% filter(!is.na(cause));rm(deaths_tot)
# Deaths by year, sex, cause_ext and age_group ####
d_ij <- deaths %>% group_by(year,sex,cause_ext,age_group) %>% 
  summarise(dij = n(),.groups = 'drop') %>%
  mutate(dij = ifelse(!{year == 1991&age_group %in% age_groups[1:3]},dij,NA)) %>%
  arrange(sex,cause_ext,age_group,year) %>%
  group_by(sex,cause_ext,age_group) %>%
  mutate(dij = zoo::na.approx(dij)) %>% ungroup %>%
  filter(dij >= 10) %>%
  full_join(expand.grid(year = 1980:2023,age_group = age_groups,sex = sexs,
                        cause_ext = unique(deaths$cause_ext)),by = c('year','age_group','sex','cause_ext')) %>%
  replace_na(list(dij = 0))
# Deaths rates by year, sex, cause_ext and age_group in standard population ####
r_ij <- popnac %>% group_by(year,sex,age_group) %>%
  summarise(nj = sum(pop),.groups = 'drop') %>%
  full_join(d_ij,by = c('year','sex','age_group')) %>%
  mutate(rij = dij/nj) %>% left_join(popref,by = 'age_group') %>%
  mutate(rij_std = rij*prop_ref)  
# Deaths by year, sex and age_group ####
d_j <- d_ij %>% group_by(year,sex,age_group) %>% 
  summarise(dj = sum(dij),.groups = 'drop') %>%
  filter(dj >= 10)
# Deaths rates by year, sex and age_group in standard population ####
r_j <- popnac %>% group_by(year,sex,age_group) %>%
  summarise(nj = sum(pop),.groups = 'drop') %>% 
  full_join(d_j,by = c('year','sex','age_group')) %>%
  mutate(rj = dj/nj) %>% left_join(popref,by = 'age_group') %>%
  mutate(rj_std = rj*prop_ref)  
# Deaths by year, sex, region and age_group ####
# deaths %>% left_join(regions,by = c('cod_mpiof'='cod_mpio')) %>%
#   filter(is.na(region)) %>% group_by(cod_dptof,cod_mpiof) %>%
#   summarise(n = n(),years = paste0(unique(year),collapse = ','),.groups = 'drop') %>% filter(n >= 10) %>% View
d_regionj <- deaths %>% left_join(regions,by = c('cod_mpiof'='cod_mpio')) %>%
  filter(!is.na(region)) %>%
  group_by(year,sex,region,age_group) %>% 
  summarise(durj = n(),.groups = 'drop') %>%
  mutate(durj = ifelse(!{year == 1991&age_group %in% age_groups[1:3]},durj,NA)) %>%
  arrange(sex,region,age_group,year) %>%
  group_by(sex,region,age_group) %>%
  mutate(durj = zoo::na.approx(durj)) %>% ungroup %>%
  filter(durj >= 10) %>%
  full_join(expand.grid(year = 1980:2023,age_group = age_groups,sex = sexs,
                        region = c('Urban','Rural')),by = c('year','age_group','sex','region')) %>%
  replace_na(list(durj = 0))
# Deaths rates by year, sex, region and age_group in standard population ####
r_regionj <- popmpio %>% left_join(regions,by = 'cod_mpio') %>%
  group_by(year,sex,region,age_group) %>%
  summarise(nurj = sum(pop),.groups = 'drop') %>% 
  full_join(d_regionj %>% filter(year>=1985),
            by = c('year','sex','region','age_group')) %>%
  mutate(rurj = durj/nurj) %>% left_join(popref,by = 'age_group') %>%
  mutate(rurj_std = rurj*prop_ref)
# Deaths by year, sex, department and age_group ####
d_dptoj <- deaths %>% filter(!is.na(cod_dptof)) %>%
  group_by(year,sex,cod_dpto = cod_dptof,age_group) %>% 
  summarise(ddptoj = n(),.groups = 'drop') %>% 
  mutate(ddptoj = ifelse(!{year == 1991&age_group %in% age_groups[1:3]},ddptoj,NA)) %>%
  arrange(sex,cod_dpto,age_group,year) %>%
  group_by(sex,cod_dpto,age_group) %>%
  mutate(ddptoj = zoo::na.approx(ddptoj)) %>% ungroup %>%
  filter(ddptoj >= 10)
# Deaths rates by year, sex, department and age_group in standard population ####
r_dptoj <- popmpio %>% mutate(cod_dpto = substr(cod_mpio,1,2)) %>% 
  group_by(year,sex,cod_dpto,age_group) %>%
  summarise(ndptoj = sum(pop),.groups = 'drop') %>% 
  full_join(d_dptoj %>% filter(year>=1985),by = c('year','sex','cod_dpto','age_group')) %>%
  mutate(rdptoj = ddptoj/ndptoj) %>% left_join(popref,by = 'age_group') %>%
  mutate(rdptoj_std = rdptoj*prop_ref)
# Deaths by year, sex, regime, cause_ext and age_group ####
d_regimeij <- deaths %>% filter(!is.na(regime)) %>% 
  group_by(year,sex,regime,cause_ext,age_group) %>% 
  summarise(dregij = n(),.groups = 'drop') %>% 
  filter(dregij >= 10) %>%
  full_join(expand.grid(year = 1998:2023,age_group = age_groups,sex = sexs,regime = c('contr','subs'),
                        cause_ext = unique(deaths$cause_ext)),by = c('year','age_group','sex','cause_ext','regime')) %>%
  replace_na(list(dregij = 0))
# Deaths rates by year, sex, cause_ext and age_group in standard population ####
r_regimeij <- popbdua %>% group_by(year,sex,age_group,regime) %>%
  summarise(nregj = sum(afil),.groups = 'drop') %>%
  full_join(d_regimeij %>% filter(year>=2012),by = c('year','sex','age_group','regime')) %>%
  mutate(rregij = dregij/nregj) %>% left_join(popref,by = 'age_group') %>%
  mutate(rregij_std = rregij*prop_ref)
# Deaths by year, sex, regime and age_group ####
d_regimej <- deaths %>% filter(!is.na(regime)) %>% 
  group_by(year,sex,regime,age_group) %>%
  summarise(dregj = n(),.groups = 'drop') %>%
  filter(dregj >= 10) %>%
  full_join(expand.grid(year = 1998:2023,age_group = age_groups,sex = sexs,
                        regime = c('contr','subs')),by = c('year','age_group','sex','regime')) %>%
  replace_na(list(dregj = 0))
# Deaths rates by year, sex, regime and age_group in standard population ####
r_regimej <- popbdua %>% group_by(year,regime,sex,age_group) %>%
  summarise(nregj = sum(afil),.groups = 'drop') %>% 
  full_join(d_regimej %>% filter(year>=2012),by = c('year','sex','regime','age_group')) %>%
  mutate(rregj = dregj/nregj) %>% left_join(popref,by = 'age_group') %>%
  mutate(rregj_std = rregj*prop_ref)
# Deaths by year, sex, non-external cause and age_group ####
d_i2j <- deaths %>% group_by(year,sex,cause,age_group) %>% 
  summarise(dij = n(),.groups = 'drop') %>%
  mutate(dij = ifelse(!{year == 1991&age_group %in% age_groups[1:3]},dij,NA)) %>%
  full_join(expand.grid(year = 1990,age_group = age_groups[1:3],sex = sexs,
                        cause = unique(deaths$cause)),
            by = c('year','age_group','sex','cause')) %>%
  mutate(dij = ifelse({year == 1990&age_group %in% age_groups[1:3]},0,dij)) %>%
  arrange(sex,cause,age_group,year) %>%
  group_by(sex,cause,age_group) %>%
  mutate(dij = zoo::na.approx(dij)) %>% ungroup %>%
  filter(dij >= 10) %>%
  full_join(expand.grid(year = 1980:2023,age_group = age_groups,sex = sexs,
                        cause = unique(deaths$cause)),
            by = c('year','age_group','sex','cause')) %>%
  replace_na(list(dij = 0)) %>%
  filter(!{sex == 'Male'&cause == 'Maternal diseases'})
# Deaths rates by year, sex, non-external cause and age_group in standard population ####
r_i2j <- popnac %>% group_by(year,sex,age_group) %>%
  summarise(nj = sum(pop),.groups = 'drop') %>%
  full_join(d_i2j,by = c('year','sex','age_group')) %>%
  mutate(rij = dij/nj) %>% left_join(popref,by = 'age_group') %>%
  mutate(rij_std = rij*prop_ref)  
# Deaths by year, sex, external cause and age_group ####
d_i3j <- deaths %>% filter(!is.na(ext_cause)) %>%
  group_by(year,sex,ext_cause,age_group) %>% 
  summarise(dij = n(),.groups = 'drop') %>%
  bind_rows(deaths %>% filter(cause == 'External causes') %>%
              group_by(year,sex,ext_cause = cause,age_group) %>% 
              summarise(dij = n(),.groups = 'drop')) %>%
  mutate(dij = ifelse(!{year == 1991&age_group %in% age_groups[1:3]},dij,NA)) %>%
  full_join(expand.grid(year = 1990,age_group = age_groups[1:3],sex = sexs,
                        ext_cause = c('External causes','Accidents','Assault','Intentional self-harm')),
            by = c('year','age_group','sex','ext_cause')) %>%
  mutate(dij = ifelse({year == 1990&age_group %in% age_groups[1:3]},0,dij)) %>%
  arrange(sex,ext_cause,age_group,year) %>%
  group_by(sex,ext_cause,age_group) %>%
  mutate(dij = zoo::na.approx(dij)) %>% ungroup %>%
  filter(dij >= 10) %>%
  full_join(expand.grid(year = 1980:2023,age_group = age_groups,sex = sexs,
                        ext_cause = c('External causes','Accidents','Assault','Intentional self-harm')),
            by = c('year','age_group','sex','ext_cause')) %>%
  replace_na(list(dij = 0))
# Deaths rates by year, sex, external cause and age_group in standard population ####
r_i3j <- popnac %>% group_by(year,sex,age_group) %>%
  summarise(nj = sum(pop),.groups = 'drop') %>%
  full_join(d_i3j,by = c('year','sex','age_group')) %>%
  mutate(rij = dij/nj) %>% left_join(popref,by = 'age_group') %>%
  mutate(rij_std = rij*prop_ref)  
# Deaths by year, sex, regime, cause and age_group ####
d_regimei2j <- deaths %>% filter(!is.na(regime)) %>% 
  group_by(year,sex,regime,cause,age_group) %>% 
  summarise(dregij = n(),.groups = 'drop') %>% 
  filter(dregij >= 10) %>%
  full_join(expand.grid(year = 1998:2023,age_group = age_groups,sex = sexs,regime = c('contr','subs'),
                        cause = unique(deaths$cause)),by = c('year','age_group','sex','cause','regime')) %>%
  replace_na(list(dregij = 0)) %>%
  filter(!{sex == 'Male'&cause == 'Maternal diseases'})
# Deaths rates by year, sex, cause and age_group in standard population ####
r_regimei2j <- popbdua %>% group_by(year,sex,age_group,regime) %>%
  summarise(nregj = sum(afil),.groups = 'drop') %>%
  full_join(d_regimei2j %>% filter(year>=2012),by = c('year','sex','age_group','regime')) %>%
  mutate(rregij = dregij/nregj) %>% left_join(popref,by = 'age_group') %>%
  mutate(rregij_std = rregij*prop_ref)

# -------------------  Year - Standardised mortality rates ---------------------
# By Sex - National ####
mrnac_sex <- r_j %>% group_by(year,sex) %>% 
  summarise(d_std = sum(rj*pop_ref),
            r_std = sum(rj_std)*scl,.groups = 'drop') %>%
  mutate(se = 1 / sqrt(d_std),
         LCI = r_std * exp(-1.96 * se),
         UCI = r_std * exp(1.96 * se))
# cause
mrnac_sex_ext <- r_ij %>% group_by(year,sex,cause_ext) %>% 
  summarise(di_std = sum(rij*pop_ref),
            ri_std = sum(rij_std)*scl,.groups = 'drop') %>%
  mutate(se = 1 / sqrt(di_std),
         LCI = ri_std * exp(-1.96 * se),
         UCI = ri_std * exp(1.96 * se))
# By Sex - Urban/Rural ####
mrur_sex <- r_regionj %>% group_by(year,sex,region) %>% 
  summarise(dur_std = sum(rurj*pop_ref),
            rur_std = sum(rurj_std)*scl,.groups = 'drop') %>%
  mutate(se = 1 / sqrt(dur_std),
         LCI = rur_std * exp(-1.96 * se),
         UCI = rur_std * exp(1.96 * se))
# By Sex - Regime ####
mrregime_sex <- r_regimej %>% group_by(year,sex,regime) %>% 
  summarise(dreg_std = sum(rregj*pop_ref),
            rreg_std = sum(rregj_std)*scl,.groups = 'drop') %>%
  mutate(se = 1 / sqrt(dreg_std),
         LCI = rreg_std * exp(-1.96 * se),
         UCI = rreg_std * exp(1.96 * se))
# --------------------- Year - Years of potential life lost --------------------
# By Sex - National ####
yllnac_sex <- d_j %>%
  left_join(airp,by = 'age_group') %>%
  left_join(lifeexp,by = c('year')) %>%
  mutate(yearsll = ifelse(LE0 > AIRP,dj*(LE0 - AIRP),0)) %>%
  group_by(year,sex) %>% summarise(YLL = sum(yearsll),.groups = "drop") %>%
  left_join(popnac %>% group_by(year,sex) %>%
              summarise(n = sum(pop),.groups = 'drop'),by = c('year','sex')) %>% 
  mutate(yll_std = YLL*scl/n)
yllnac_sex_excess <- yllnac_sex %>% select(-YLL,-n) %>%
  pivot_wider(id_cols = year,names_from = sex,values_from = yll_std) %>%
  mutate(yll_excess = Male - Female)
# cause
yllnac_sex_ext <- d_ij %>%
  left_join(airp,by = 'age_group') %>%
  left_join(lifeexp,by = c('year')) %>%
  mutate(yearsll = ifelse(LE0 > AIRP,dij*(LE0 - AIRP),0)) %>%
  group_by(year,sex,cause_ext) %>% summarise(YLL = sum(yearsll),.groups = "drop") %>%
  left_join(popnac %>% group_by(year,sex) %>%
              summarise(n = sum(pop),.groups = 'drop'),by = c('year','sex')) %>% 
  mutate(yll_std = YLL*scl/n)
yllnac_sex_ext_excess <- yllnac_sex_ext %>% select(-YLL,-n) %>%
  pivot_wider(id_cols = c(year,cause_ext),names_from = sex,values_from = yll_std) %>%
  mutate(yll_excess = Male - Female)

# By Sex - Urban/Rural ####
yllur_sex <- d_regionj %>% 
  left_join(airp,by = 'age_group') %>%
  left_join(lifeexp,by = c('year')) %>%
  mutate(yearsll = ifelse(LE0 > AIRP,durj*(LE0 - AIRP),0)) %>%
  group_by(year,sex,region) %>% summarise(YLL = sum(yearsll),.groups = "drop") %>%
  right_join(popmpio %>% left_join(regions,by = 'cod_mpio') %>% group_by(year,sex,region) %>%
              summarise(n = sum(pop),.groups = 'drop'),by = c('year','sex','region')) %>% 
  mutate(yll_std = YLL*scl/n)

# By Sex - Regime ####
yllregime_sex <- d_regimej %>% 
  left_join(airp,by = 'age_group') %>%
  left_join(lifeexp,by = c('year')) %>%
  mutate(yearsll = ifelse(LE0 > AIRP,dregj*(LE0 - AIRP),0)) %>%
  group_by(year,sex,regime) %>% summarise(YLL = sum(yearsll),.groups = "drop") %>%
  right_join(popbdua %>% group_by(year,sex,regime) %>%
              summarise(n = sum(afil),.groups = 'drop'),by = c('year','sex','regime')) %>% 
  mutate(yll_std = YLL*scl/n)

# ------------------------------ Decades ---------------------------------------
# SMR by decade, sex and cause_ext ####
smr_ij <- r_ij %>% mutate(decade = floor(year/10)*10) %>%
  group_by(decade,sex,cause_ext,age_group,pop_ref,prop_ref) %>%
  summarise(dij = sum(dij),nj = sum(nj),.groups = 'drop') %>%
  mutate(rij = dij/nj,dij_std = rij*pop_ref,SMRij = rij*prop_ref*scl) %>%
  mutate(se = 1 / sqrt(dij_std),
         LCI = SMRij * exp(-1.96 * se),
         UCI = SMRij * exp(1.96 * se))
smr_i <- smr_ij %>%
  group_by(decade,sex,cause_ext) %>%
  summarise(SMRi = sum(SMRij),di_std = sum(dij_std),.groups = 'drop') %>%
  mutate(se = 1 / sqrt(di_std),
         LCI = SMRi * exp(-1.96 * se),
         UCI = SMRi * exp(1.96 * se))
smr_j <- r_j %>% mutate(decade = floor(year/10)*10) %>%
  group_by(decade,sex,age_group,pop_ref,prop_ref) %>%
  summarise(dj = sum(dj),nj = sum(nj),.groups = 'drop') %>%
  mutate(rj = dj/nj,dj_std = rj*pop_ref,SMRj = rj*prop_ref*scl) %>%
  mutate(se = 1 / sqrt(dj_std),
         LCI = SMRj * exp(-1.96 * se),
         UCI = SMRj * exp(1.96 * se))
smr <- smr_j %>%
  group_by(decade,sex) %>%
  summarise(SMR = sum(SMRj),d_std = sum(dj_std),.groups = 'drop') %>%
  mutate(se = 1 / sqrt(d_std),
         LCI = SMR * exp(-1.96 * se),
         UCI = SMR * exp(1.96 * se))
# SMR by decade, sex, cause_ext and regime ####
smr_regimeij <- r_regimeij %>% mutate(decade = floor(year/10)*10) %>%
  group_by(decade,sex,regime,age_group,cause_ext,pop_ref,prop_ref) %>%
  summarise(dregij = sum(dregij),nregj = sum(nregj),.groups = 'drop') %>%
  mutate(rregij = dregij/nregj,dregij_std = rregij*pop_ref,SMRregij = rregij*prop_ref*scl) %>%
  mutate(se = 1 / sqrt(dregij_std),
         LCI = SMRregij * exp(-1.96 * se),
         UCI = SMRregij * exp(1.96 * se))
smr_regimei <- smr_regimeij %>%
  group_by(decade,sex,cause_ext,regime) %>%
  summarise(SMRregi = sum(SMRregij),dregi_std = sum(dregij_std),.groups = 'drop') %>%
  mutate(se = 1 / sqrt(dregi_std),
         LCI = SMRregi * exp(-1.96 * se),
         UCI = SMRregi * exp(1.96 * se))
smr_regimej <- r_regimej %>% mutate(decade = floor(year/10)*10) %>%
  group_by(decade,sex,regime,age_group,pop_ref,prop_ref) %>%
  summarise(dregj = sum(dregj),nregj = sum(nregj),.groups = 'drop') %>%
  mutate(rregj = dregj/nregj, dregj_std = rregj*pop_ref,SMRregj = rregj*prop_ref*scl) %>%
  mutate(se = 1 / sqrt(dregj_std),
         LCI = SMRregj * exp(-1.96 * se),
         UCI = SMRregj * exp(1.96 * se))
smr_regime <- smr_regimej %>% 
  group_by(decade,sex,regime) %>%
  summarise(SMRreg = sum(SMRregj),dreg_std = sum(dregj_std),.groups = 'drop') %>%
  mutate(se = 1 / sqrt(dreg_std),
         LCI = SMRreg * exp(-1.96 * se),
         UCI = SMRreg * exp(1.96 * se))
# YLL by decade, sex and age_group ####
ylldec_agesex <- smr_j %>%
  left_join(airp,by = 'age_group') %>%
  left_join(lifeexp_decade,by = c('decade')) %>%
  mutate(yearsll = ifelse(LE0 > AIRP,dj*(LE0 - AIRP)/scl6,0))
deaths_sexage_excess <- ylldec_agesex %>% select(decade,sex,age_group,SMRj) %>%
  pivot_wider(id_cols = c(decade,age_group),names_from = sex,values_from = SMRj) %>%
  mutate(deaths_excess = Male - Female)
ylldec_sexage_excess <- ylldec_agesex %>% select(decade,sex,age_group,yearsll) %>%
  pivot_wider(id_cols = c(decade,age_group),names_from = sex,values_from = yearsll) %>%
  mutate(yll_excess = Male - Female)
# YLL by decade and sex ####
ylldec_sex <- ylldec_agesex %>% group_by(decade,sex) %>% 
  summarise(YLL = sum(yearsll),SMR = sum(SMRj),.groups = "drop")
deaths_sex_excess <- ylldec_sex %>% select(decade,sex,SMR) %>%
  pivot_wider(id_cols = c(decade),names_from = sex,values_from = SMR) %>%
  mutate(deaths_excess = Male - Female)
ylldec_sex_excess <- ylldec_sex %>% select(decade,sex,YLL) %>%
  pivot_wider(id_cols = c(decade),names_from = sex,values_from = YLL) %>%
  mutate(yll_excess = Male - Female)

# YLL by decade, sex, cause_ext and age_group ####
ylldec_agesex_ext <- smr_ij %>%
  left_join(airp,by = 'age_group') %>%
  left_join(lifeexp_decade,by = c('decade')) %>%
  mutate(yearsll = ifelse(LE0 > AIRP,dij*(LE0 - AIRP)/scl6,0))
deaths_sexage_ext_excess <- ylldec_agesex_ext %>% select(decade,sex,age_group,cause_ext,SMRij) %>%
  pivot_wider(id_cols = c(decade,age_group,cause_ext),names_from = sex,values_from = SMRij) %>%
  mutate(deaths_excess = Male - Female)
ylldec_sexage_ext_excess <- ylldec_agesex_ext %>% select(decade,sex,age_group,cause_ext,yearsll) %>%
  pivot_wider(id_cols = c(decade,age_group,cause_ext),names_from = sex,values_from = yearsll) %>%
  mutate(yll_excess = Male - Female)

# YLL by decade, sex and cause_ext ####
ylldec_sex_ext <- ylldec_agesex_ext %>% group_by(decade,sex,cause_ext) %>% 
  summarise(YLL = sum(yearsll),SMRi = sum(SMRij),.groups = "drop")
deaths_sex_ext_excess <- ylldec_sex_ext %>% select(decade,sex,cause_ext,SMRi) %>%
  pivot_wider(id_cols = c(decade,cause_ext),names_from = sex,values_from = SMRi) %>%
  mutate(deaths_excess = Male - Female)
ylldec_sex_ext_excess <- ylldec_sex_ext %>% select(decade,sex,cause_ext,YLL) %>%
  pivot_wider(id_cols = c(decade,cause_ext),names_from = sex,values_from = YLL) %>%
  mutate(yll_excess = Male - Female)

# SMR by decade, sex and department ####
smr_dpto <- r_dptoj %>% 
  full_join(expand.grid(year = 1985:2023,age_group = age_groups,sex = sexs,
                        cod_dpto = unique(deaths$cod_dptof)),
            by = c('year','age_group','sex','cod_dpto')) %>%
  mutate(decade = floor(year/10)*10) %>%
  group_by(decade,sex,age_group,cod_dpto,pop_ref,prop_ref) %>%
  summarise(ddptoj = ifelse(all(is.na(ddptoj)),NA,sum(ddptoj,na.rm = T)),
            ndptoj = sum(ndptoj),.groups = 'drop') %>%
  mutate(rdptoj = ddptoj/ndptoj,ddptoj = rdptoj*pop_ref,SMRdptoj = rdptoj*prop_ref*scl) %>%
  group_by(decade,sex,cod_dpto) %>%
  summarise(SMRdpto = ifelse(all(is.na(SMRdptoj)),NA,sum(SMRdptoj,na.rm = T)),
            ddpto_std = ifelse(all(is.na(SMRdptoj)),NA,sum(ddptoj,na.rm = T)),.groups = 'drop') %>%
  mutate(se = 1 / sqrt(ddpto_std),
         LCI = SMRdpto * exp(-1.96 * se),
         UCI = SMRdpto * exp(1.96 * se))

# ------------------------------ Figures ---------------------------------------
# Figure 1 ####
fig_smri <- mrnac_sex %>% mutate(cause_ext = 'Overall') %>%
  bind_rows(mrnac_sex_ext %>% select(year,sex,d_std = di_std,r_std = ri_std,LCI,UCI,cause_ext)) %>%
  mutate(cause_ext = factor(cause_ext,levels = c('Overall','External causes','Non-external causes')))%>%
  mutate(sex_cause_ext = paste0(sex,'-',cause_ext)) %>%
  ggplot(aes(x = year,y = r_std,group = sex_cause_ext)) +
  geom_line(aes(color = cause_ext,lty = sex),lwd = 1.2)+
  scale_color_manual(values = ggcolors3[c(3,1,2)])+
  #geom_ribbon(aes(ymin = LCI,ymax = UCI),fill = 'darkgray',alpha = 0.3)+
  labs(x = 'Year',y = 'Standardised mortality rates (x 100,000)',
       color = '',fill = '',lty = '') +
  scale_y_continuous(n.breaks = 10,labels = scales::label_number(big.mark = ','))+
  scale_x_continuous(breaks = seq(1980,2023,5))+
  theme_classic(base_size = 18)+
  theme(legend.position = 'bottom')

fig_ylli <- yllnac_sex_excess %>% mutate(cause_ext = 'Overall') %>%
  bind_rows(yllnac_sex_ext_excess) %>%
  mutate(cause_ext = factor(cause_ext,levels = c('Overall','External causes','Non-external causes')))%>%
  ggplot(aes(x = year,y = yll_excess,group = cause_ext)) +
  geom_line(aes(color = cause_ext),lwd = 1.2,lty = 4)+
  scale_color_manual(values = ggcolors3[c(3,1,2)])+
  labs(x = 'Year',y = 'Difference of YLLs (per 100,000 population)',
       color = '') +
  scale_y_continuous(n.breaks = 10,labels = scales::label_number(big.mark = ','))+
  scale_x_continuous(breaks = seq(1980,2023,5))+
  theme_classic(base_size = 18)+
  theme(legend.position = 'bottom',legend.key.width = unit(2,'cm'))

fig_smr_regime <- mrregime_sex %>% 
  mutate(regime2 = ifelse(regime == 'contr','Contributory','Subsidised')) %>%
  mutate(sex_regime = paste0(sex,'-',regime2)) %>%
  ggplot(aes(x = year,y = rreg_std,group = sex_regime)) +
  geom_line(aes(color = regime2,lty = sex),lwd = 1.2)+
  #geom_ribbon(aes(ymin = LCI,ymax = UCI),fill = 'darkgray',alpha = 0.5)+
  scale_color_manual(values = ggcolors10[c(5,8)])+
  labs(x = 'Year',y = 'Standardised mortality rates (x 100,000)',
       color = '',fill = '',lty = '') +
  scale_y_continuous(n.breaks = 10,labels = scales::label_number(big.mark = ','))+
  scale_x_continuous(breaks = seq(2012,2023,2))+
  theme_classic(base_size = 18)+
  theme(legend.position = 'bottom')

fig_smr_region <- mrur_sex %>% mutate(sex_region = paste0(sex,'-',region)) %>%
  ggplot(aes(x = year,y = rur_std,group = sex_region)) +
  geom_line(aes(color = region,lty = sex),lwd = 1.2)+
  # geom_ribbon(aes(ymin = LCI,ymax = UCI),fill = 'darkgray',alpha = 0.5)+
  scale_color_manual(values = ggcolors10[c(3,2)])+
  labs(x = 'Year',y = 'Standardised mortality rates (x 100,000)',
       color = '',fill = '',lty = '') +
  scale_y_continuous(n.breaks = 10,labels = scales::label_number(big.mark = ','))+
  scale_x_continuous(breaks = seq(1980,2023,5))+
  theme_classic(base_size = 18)+
  theme(legend.position = 'bottom')

# Figure 2 ####
fig_smrij <- r_ij %>% mutate(sex_cause_ext = paste0(sex,'-',cause_ext),
                age_group = factor(age_group,levels = age_groups)) %>%
  ggplot(aes(x = year,y = rij_std*scl,group = sex_cause_ext)) +
  facet_wrap(age_group~.,scales = 'free')+
  geom_line(aes(color = cause_ext,lty = sex),lwd = 1.3) + 
  scale_color_manual(values = ggcolors3[c(1,2)])+
  labs(x = 'Year',y = 'Standardised mortality rates (x 100,000)',
       color = '',fill = '',lty = '') +
  scale_y_continuous(n.breaks = 10,labels = scales::label_number(big.mark = ','))+
  scale_x_continuous(breaks = c(seq(1980,2023,5)))+
  theme_classic(base_size = 18)+
  theme(legend.position = 'bottom',strip.text = element_text(face = 'bold'),
        legend.key.width = unit(1,'cm'),strip.background.x = element_rect(color = 'white'))
# Life expectancy ####
fig_le <- lifeexp %>% mutate(sex = 'Both') %>% bind_rows(lifeexp_sex) %>%
  ggplot(aes(x = year,y = LE0,lty = sex)) +
  geom_line(lwd = 1,color = 'gray30')+
  labs(x = 'Year',y = 'Life expectancy at birth',
       color = '',fill = '',lty = '') +
  scale_y_continuous(n.breaks = 10,labels = scales::label_number(big.mark = ','))+
  scale_x_continuous(breaks = seq(1980,2023,5))+
  scale_linetype_manual(values = c(3,1,2)) +
  theme_classic(base_size = 16)+
  theme(legend.direction = 'vertical',
        legend.key.width = unit(1,'cm'),
        legend.margin = margin(0,0,0,0))

# Maps by department ####
map1 <- readRDS("../Data/MunicipiosBien.rds")
crs <- sf::st_crs(map1)
map <- sf::st_read('../Data/Mapa/MGN_DPTO_POLITICO.shp')
map <- sf::st_cast(map, "POLYGON")
map <- sf::st_transform(map, crs)
map <- map %>% filter(DPTO_CCDGO != '88') %>% 
  bind_rows(map1 %>% rename(DPTO_CCDGO = DPTO) %>% filter(DPTO_CCDGO == '88') %>%
              mutate(DPTO_CNMBR = 'ARCHIPIÉLAGO DE SAN ANDRÉS'))
rm(map1)
# Map with labels
map_dpto <- map %>% as.data.frame() %>% group_by(DPTO_CCDGO,DPTO_CNMBR) %>% 
  summarise(.groups = 'drop')
dptos <- map_dpto %>% pull(DPTO_CCDGO) %>% unique
auxpoint <- NULL
for (i in dptos){
  aux2 <- map %>% filter(DPTO_CCDGO == i)
  shp1 <- sf::st_as_sf(aux2)
  points_sf <- sf::st_centroid(shp1) %>%  sf::st_as_sf()
  points_df <- data.frame(sf::st_coordinates(points_sf)) %>%
    mutate(DPTO_CCDGO = i)
  if (i == 13){points_df = points_df[50,]}
  if (i == 52){points_df = points_df[2,]}
  if (i == 76){points_df = points_df[2,]}
  auxpoint <- bind_rows(auxpoint,points_df)
  print(i)
}
auxpoint <- auxpoint %>% left_join(map_dpto,by = c('DPTO_CCDGO')) %>%
  group_by(DPTO_CCDGO) %>% slice(1) %>% ungroup
data_map <- map %>% left_join(smr_dpto %>% select(-c(ddpto_std,se,LCI,UCI)) %>%
                                pivot_wider(id_cols = c(decade,cod_dpto),
                                            names_from = sex,values_from = SMRdpto) %>%
                                replace_na(list(Female = 0,Male = 0)) %>%
                                mutate(smr_excess = Male-Female) %>% select(-sexs),
                              by = c('DPTO_CCDGO'='cod_dpto'),relationship = 'many-to-many')
fig_dpto <- ggplot() + geom_sf(data = data_map,aes(fill = smr_excess)) + 
  facet_wrap(decade~.,ncol = 3,nrow = 2) + 
  labs(fill = 'Difference in standardised mortality rate (x 100,000)') +
  theme_void(base_size = 18) + 
  geom_text(data = auxpoint, aes(X, Y,label = DPTO_CNMBR), alpha = 0.5,
            size = 1.3) +
  scale_fill_gradient2(low = 'white',high = '#db2732',midpoint = 0,
                       na.value = "gray75") +
  theme(legend.position = 'bottom',
        legend.key.width = unit(1,'cm'),
        legend.text = element_text(size = 11))
# Heatmap by external cause of death ####
fig_heatmap <- r_i2j %>% filter(cause != 'External causes') %>%
  select(year,sex,age_group,cause,rij_std) %>%
  pivot_wider(id_cols = c(year,age_group,cause),
              names_from = sex,values_from = rij_std) %>%
  mutate(smr_excess = (Male-Female)*scl) %>% select(-sexs) %>%
  mutate(age_group = factor(age_group,levels = age_groups)) %>%
  ggplot(aes(x = year,y = cause,fill = smr_excess)) +
  facet_wrap(age_group~.)+
  geom_tile()+
  scale_fill_steps2(breaks = seq(-10,24,5),low = '#0ca2cb',high = '#db2732',
                    mid = 'gray100',midpoint = 0,na.value = "gray75",
                    guide = "coloursteps")+
  labs(x = 'Year',y = 'Non-external cause of death',fill = 'Difference in standardised mortality rate (x 100,000)')+
  theme_classic(base_size = 18)+
  theme(legend.position = 'bottom',strip.text = element_text(face = 'bold'),
        legend.key.width = unit(1.5,'cm'),strip.background.x = element_rect(color = 'white'))
# Heatmap by external cause of death ####
fig_heatmap_ext <- r_i3j %>% 
  select(year,sex,age_group,ext_cause,rij_std) %>%
  pivot_wider(id_cols = c(year,age_group,ext_cause),
              names_from = sex,values_from = rij_std) %>%
  mutate(smr_excess = (Male-Female)*scl) %>% select(-sexs) %>%
  mutate(age_group = factor(age_group,levels = age_groups),
         ext_cause = factor(ext_cause,levels = rev(c('External causes','Accidents','Assault','Intentional self-harm')))) %>%
  ggplot(aes(x = year,y = ext_cause,fill = smr_excess)) +
  facet_wrap(age_group~.)+
  geom_tile()+
  scale_fill_steps2(breaks = seq(0,100,20),low = '#0ca2cb',high = '#db2732',
                    mid = 'gray100',midpoint = 0,na.value = "gray75",
                    guide = "coloursteps")+
  # scale_fill_gradient2(low = '#0ca2cb',high = '#db2732',midpoint = 0,
  #                                  na.value = "gray75",
  #                      limits = c(-10,24))+
  labs(x = 'Year',y = 'External cause of death',fill = 'Difference in standardised mortality rate (x 100,000)')+
  theme_classic(base_size = 18)+
  theme(legend.position = 'bottom',strip.text = element_text(face = 'bold'),
        legend.key.width = unit(1.5,'cm'),strip.background.x = element_rect(color = 'white'))

# SMR by cause, regime and sex ####
causes <- c('Infectious and parasitic diseases','Neoplasms','Diseases of the blood',
            'Endocrine, nutritional, and metabolic diseases','Mental and behavioural disorders',
            'Diseases of the nervous system','Diseases of the circulatory system',
            'Diseases of the respiratory system','Diseases of the digestive system',
            'Diseases of the skin and subcutaneous tissue','Diseases of the musculoskeletal system',
            'Diseases of the genitourinary system','Maternal diseases',
            'Conditions of the perinatal period','Congenital malformations',
            'Causes of death not elsewhere classified','External causes')
fig_cause_regime <- r_regimei2j %>% group_by(year,sex,regime,cause) %>%
  summarise(SMRregi = sum(rregij_std)*scl,.groups = 'drop') %>%
  mutate(regime2 = ifelse(regime == 'contr','Contributory','Subsidised')) %>%
  ggplot(aes(x = year,y = SMRregi,lty = sex,col = regime2))+
  facet_wrap(cause~.,ncol = 5,scales = 'free',
             labeller = labeller(cause = label_wrap_gen(30)))+ 
  geom_line(lwd = 1.3)+
  scale_color_manual(values = ggcolors10[c(5,8)])+
  labs(x = 'Year',y = 'Standardised mortality rates (x 100,000)',
       color = '',fill = '',lty = '') +
  scale_y_continuous(n.breaks = 10,labels = scales::label_number(big.mark = ','))+
  scale_x_continuous(breaks = seq(2012,2023,2))+
  theme_classic(base_size = 18)+
  theme(legend.position = 'bottom',strip.background.x = element_rect(color = 'white'),
        strip.text = element_text(size = 12,face ='bold'))
# Percentage of deaths by cause ####
aux_fig_props <- r_regimei2j %>% 
  group_by(year,cause,regime,sex) %>% mutate(sumdregij = sum(dregij)) %>%
  ungroup %>% mutate(prop = dregij/sumdregij,
                     age_group = factor(age_group,levels = age_groups))
fn_fig_props <- function(data){data %>% ggplot(aes(x = year,y = prop*100,fill = age_group))+
    facet_wrap(cause~.,ncol = 5,scales = 'free',
               labeller = labeller(cause = label_wrap_gen(30)))+ 
    geom_area()+
    labs(x = 'Year',y = 'Proportion of deaths (%)',
         color = '',fill = '',lty = '') +
    scale_y_continuous(n.breaks = 10,labels = scales::label_number(big.mark = ','))+
    scale_x_continuous(breaks = seq(2012,2023,2))+
    scale_fill_brewer(palette = 'RdBu',guide = 'legend')+
    guides(fill = guide_legend(nrow = 1))+
    theme_classic(base_size = 18)+
    theme(legend.position = 'bottom',strip.background.x = element_rect(color = 'white'),
          strip.text = element_text(size = 12,face = 'bold'))}
fig_prop_male_subs <- aux_fig_props %>% 
  filter(sex == 'Male',regime == 'subs') %>% fn_fig_props
fig_prop_male_contr <- aux_fig_props %>% 
  filter(sex == 'Male',regime == 'contr') %>% fn_fig_props
fig_prop_female_subs <- aux_fig_props %>% 
  filter(sex == 'Female',regime == 'subs') %>% fn_fig_props
fig_prop_female_contr <- aux_fig_props %>% 
  filter(sex == 'Female',regime == 'contr') %>% fn_fig_props

# ------------------------------ Tables ----------------------------------------
# Table 1 ####
aux_tab1_dsmr <- bind_rows(smr %>%
  pivot_wider(id_cols = c(decade),names_from = sex,values_from = SMR) %>%
  mutate(dsmr = Male-Female) %>% select(-sexs) %>%
  mutate(regime = 'All people'),
smr_regime %>% 
  pivot_wider(id_cols = c(decade,regime),names_from = sex,values_from = SMRreg) %>%
  mutate(dsmr = Male-Female) %>% select(-sexs)) %>%
  mutate(regime = factor(regime,levels = c('All people','contr','subs'),
                         labels = c('All people','Contributory','Subsidised'))) %>%
  select(regime,decade,dsmr)

aux_tab1_smr <- bind_rows(smr %>%
  pivot_wider(id_cols = c(decade),names_from = sex,values_from = c(SMR,LCI,UCI)) %>%
  mutate(smr_ratio = SMR_Male/SMR_Female,
         LCI = UCI_Male/UCI_Female,UCI = LCI_Male/LCI_Female) %>% 
  select(-paste0(rep(c('SMR_','LCI_','UCI_'),each=2),sexs)) %>%
  mutate(regime = 'All people'),
smr_regime %>% rename(SMR = SMRreg) %>%
  pivot_wider(id_cols = c(decade,regime),names_from = sex,values_from = c(SMR,LCI,UCI)) %>%
  mutate(smr_ratio = SMR_Male/SMR_Female,
         LCI = UCI_Male/UCI_Female,UCI = LCI_Male/LCI_Female) %>% 
  select(-paste0(rep(c('SMR_','LCI_','UCI_'),each=2),sexs))) %>%
  mutate(regime = factor(regime,levels = c('All people','contr','subs'),
                         labels = c('All people','Contributory','Subsidised'))) %>%
  select(regime,decade,smr_ratio,LCI,UCI)

aux_tab1_dsmr_ext <- bind_rows(
  smr_i %>% select(decade,sex,cause_ext,SMRi) %>%
  pivot_wider(id_cols = c(decade,cause_ext),names_from = sex,values_from = SMRi) %>%
  mutate(dsmri = Male-Female) %>% select(-sexs) %>% mutate(regime = 'All people'),
smr_regimei %>% select(decade,sex,regime,cause_ext,SMRregi) %>%
  pivot_wider(id_cols = c(decade,regime,cause_ext),names_from = sex,values_from = SMRregi) %>%
  mutate(dsmri = Male-Female) %>% select(-sexs)) %>%
  mutate(regime = factor(regime,levels = c('All people','contr','subs'),
                         labels = c('All people','Contributory','Subsidised'))) %>%
  select(regime,decade,cause_ext,dsmri)

aux_tab1_smr_ext <- bind_rows(
smr_i %>% select(decade,sex,cause_ext,SMR = SMRi,LCI,UCI) %>%
  pivot_wider(id_cols = c(decade,cause_ext),names_from = sex,values_from = c(SMR,LCI,UCI)) %>%
  mutate(smri_ratio = SMR_Male/SMR_Female,
         LCIi = UCI_Male/UCI_Female,UCIi = LCI_Male/LCI_Female) %>% 
  select(-paste0(rep(c('SMR_','LCI_','UCI_'),each=2),sexs)) %>%
  mutate(regime = 'All people'),
smr_regimei %>% select(decade,sex,regime,cause_ext,SMR = SMRregi,LCI,UCI) %>%
  pivot_wider(id_cols = c(decade,regime,cause_ext),names_from = sex,values_from = c(SMR,LCI,UCI)) %>%
  mutate(smri_ratio = SMR_Male/SMR_Female,
         LCIi = UCI_Male/UCI_Female,UCIi = LCI_Male/LCI_Female) %>% 
  select(-paste0(rep(c('SMR_','LCI_','UCI_'),each=2),sexs))) %>%
  mutate(regime = factor(regime,levels = c('All people','contr','subs'),
                         labels = c('All people','Contributory','Subsidised'))) %>%
  select(regime,decade,cause_ext,smri_ratio,LCIi,UCIi)

tab1 <- full_join(aux_tab1_dsmr,aux_tab1_smr,by = c('regime','decade')) %>%
  full_join(aux_tab1_dsmr_ext %>% filter(cause_ext == 'Non-external causes') %>%
              select(-cause_ext) %>%
              rename(dsmr_next = dsmri),by = c('regime','decade')) %>%
  full_join(aux_tab1_smr_ext %>% filter(cause_ext == 'Non-external causes') %>%
              select(-cause_ext) %>%
              rename(smr_next = smri_ratio,LCI_next = LCIi,UCI_next = UCIi),by = c('regime','decade')) %>%
  full_join(aux_tab1_dsmr_ext %>% filter(cause_ext == 'External causes') %>%
              select(-cause_ext) %>%
              rename(dsmr_ext = dsmri),by = c('regime','decade')) %>%
  full_join(aux_tab1_smr_ext %>% filter(cause_ext == 'External causes') %>%
              select(-cause_ext) %>%
              rename(smr_ext = smri_ratio,LCI_ext = LCIi,UCI_ext = UCIi),by = c('regime','decade')) %>%
  arrange(regime,decade)

# Table 2 ####
aux_tab2_dsmr <- smr_j %>% select(age_group,decade,sex,SMRj) %>%
  pivot_wider(id_cols = c(age_group,decade),names_from = sex,values_from = SMRj) %>%
  mutate(dsmr = Male-Female) %>% select(-sexs)
aux_tab2_smr <- smr_j %>% select(age_group,decade,sex,SMR=SMRj,LCI,UCI) %>%
  pivot_wider(id_cols = c(age_group,decade),names_from = sex,values_from = c(SMR,LCI,UCI)) %>%
  mutate(smr_ratio = SMR_Male/SMR_Female,
         LCI = UCI_Male/UCI_Female,UCI = LCI_Male/LCI_Female) %>% 
  select(-paste0(rep(c('SMR_','LCI_','UCI_'),each=2),sexs))
aux_tab2_dsmr_ext <- smr_ij %>% select(age_group,decade,sex,cause_ext,SMRij) %>%
  pivot_wider(id_cols = c(age_group,decade,cause_ext),names_from = sex,values_from = SMRij) %>%
  mutate(dsmri = Male-Female) %>% select(-sexs)
aux_tab2_smr_ext <- smr_ij %>% select(age_group,decade,sex,cause_ext,SMR=SMRij,LCI,UCI) %>%
  pivot_wider(id_cols = c(age_group,decade,cause_ext),names_from = sex,values_from = c(SMR,LCI,UCI)) %>%
  mutate(smri_ratio = SMR_Male/SMR_Female,
         LCIi = UCI_Male/UCI_Female,UCIi = LCI_Male/LCI_Female) %>% 
  select(-paste0(rep(c('SMR_','LCI_','UCI_'),each=2),sexs))


tab2 <- full_join(aux_tab2_dsmr,aux_tab2_smr,by = c('age_group','decade')) %>%
  full_join(aux_tab2_dsmr_ext %>% filter(cause_ext == 'Non-external causes') %>%
              select(-cause_ext) %>%
              rename(dsmr_next = dsmri),by = c('age_group','decade')) %>%
  full_join(aux_tab2_smr_ext %>% filter(cause_ext == 'Non-external causes') %>%
              select(-cause_ext) %>%
              rename(smr_next = smri_ratio,LCI_next = LCIi,UCI_next = UCIi),by = c('age_group','decade')) %>%
  full_join(aux_tab2_dsmr_ext %>% filter(cause_ext == 'External causes') %>%
              select(-cause_ext) %>%
              rename(dsmr_ext = dsmri),by = c('age_group','decade')) %>%
  full_join(aux_tab2_smr_ext %>% filter(cause_ext == 'External causes') %>%
              select(-cause_ext) %>%
              rename(smr_ext = smri_ratio,LCI_ext = LCIi,UCI_ext = UCIi),by = c('age_group','decade')) %>%
  mutate(age_group = factor(age_group,levels = age_groups)) %>%
  arrange(age_group,decade)

# Table S2 ####
aux_tabs2_excess <- bind_rows(deaths_sex_excess %>% select(-sexs) %>% 
            left_join(ylldec_sex_excess%>% select(-sexs),
                      by = 'decade') %>%
            mutate(age_group = 'All ages'),
          deaths_sexage_excess %>% select(-sexs) %>% 
            left_join(ylldec_sexage_excess%>% select(-sexs),
                      by = c('decade','age_group'))) %>%
  select(age_group,decade,deaths_excess,yll_excess) %>%
  mutate(age_group = factor(age_group,levels = c('All ages',age_groups)))
aux_tabs2_excess_ext <- bind_rows(deaths_sex_ext_excess %>% select(-sexs) %>% 
                             left_join(ylldec_sex_ext_excess%>% select(-sexs),
                                       by = c('decade','cause_ext')) %>%
                             mutate(age_group = 'All ages'),
                           deaths_sexage_ext_excess %>% select(-sexs) %>% 
                             left_join(ylldec_sexage_ext_excess%>% select(-sexs),
                                       by = c('decade','age_group','cause_ext'))) %>%
  mutate(age_group = factor(age_group,levels = c('All ages',age_groups)))

tabs2 <- full_join(aux_tabs2_excess,
                   aux_tabs2_excess_ext %>% filter(cause_ext == 'External causes') %>%
                     select(age_group,decade,deaths_excess_ext=deaths_excess,yll_excess_ext = yll_excess),
                   by = c('age_group','decade')) %>%
  full_join(aux_tabs2_excess_ext %>% filter(cause_ext == 'Non-external causes') %>%
              select(age_group,decade,deaths_excess_next=deaths_excess,yll_excess_next = yll_excess),
            by = c('age_group','decade')) %>%
  arrange(age_group,decade)

################################################################################
# Export Excel Supplementary ####
# Tables
# writexl::write_xlsx(list('Table 1'=tab1,'Table 2'=tab2),
#                     path = '../Supplementary Excel file.xlsx')

# Export Paper ####
# Tables
tabdec <- function(x){formatC(x,digits = 2,format='f')}
writexl::write_xlsx(list(tab1 %>% 
                        mutate(smr_ratio = paste0(tabdec(smr_ratio),' (',tabdec(LCI),', ',tabdec(UCI),')'),
                               smr_next = paste0(tabdec(smr_next),' (',tabdec(LCI_next),', ',tabdec(UCI_next),')'),
                               smr_ext = paste0(tabdec(smr_ext),' (',tabdec(LCI_ext),', ',tabdec(UCI_ext),')')) %>%
                          select(-c(LCI,UCI,LCI_next,UCI_next,LCI_ext,UCI_ext)),
                         tab2 %>%
                          mutate(smr_ratio = paste0(tabdec(smr_ratio),' (',tabdec(LCI),', ',tabdec(UCI),')'),
                                 smr_next = paste0(tabdec(smr_next),' (',tabdec(LCI_next),', ',tabdec(UCI_next),')'),
                                 smr_ext = paste0(tabdec(smr_ext),' (',tabdec(LCI_ext),', ',tabdec(UCI_ext),')')) %>%
                          select(-c(LCI,UCI,LCI_next,UCI_next,LCI_ext,UCI_ext)),
                        tabs2),
                    path = '../Results/TablesExport.xlsx')
# Figures
library(patchwork)
fig1 <- (fig_smri+labs(subtitle = '(a)')+theme(legend.position = 'bottom',
                                               plot.subtitle = element_text(hjust = 0.5)) +
           fig_ylli+labs(subtitle = '(b)')+theme(legend.position = 'bottom',
                                                 plot.subtitle = element_text(hjust = 0.5)))/
  (fig_smr_regime+labs(subtitle = '(c)')+theme(legend.position = 'bottom',
                                               plot.subtitle = element_text(hjust = 0.5))+
     fig_smr_region+labs(subtitle = '(d)')+theme(legend.position = 'bottom',
                                                plot.subtitle = element_text(hjust = 0.5)))
ggsave(plot = fig1,filename = '../Results/Figure1.png',
       width = 40,height = 35,units = 'cm')
ggsave(plot = fig_smrij,filename = '../Results/Figure2.png',
       width = 50,height = 25,units = 'cm')
ggsave(plot = fig_le,filename = '../Results/FigureS1.png',
       width = 20,height = 10,units = 'cm')
ggsave(plot = fig_heatmap,filename = '../Results/FigureS2.png',
       width = 40,height = 30,units = 'cm')
ggsave(plot = fig_heatmap_ext,filename = '../Results/FigureS3.png',
       width = 40,height = 25,units = 'cm')
ggsave(plot = fig_cause_regime,filename = '../Results/FigureS4.png',
       width = 50,height = 40,units = 'cm')
ggsave(plot = fig_prop_male_subs,filename = '../Results/FigureS5.png',
       width = 50,height = 40,units = 'cm')
ggsave(plot = fig_prop_male_contr,filename = '../Results/FigureS6.png',
       width = 50,height = 40,units = 'cm')
ggsave(plot = fig_prop_female_subs,filename = '../Results/FigureS7.png',
       width = 50,height = 40,units = 'cm')
ggsave(plot = fig_prop_female_contr,filename = '../Results/FigureS8.png',
       width = 50,height = 40,units = 'cm')
ggsave(plot = fig_dpto,filename = '../Results/FigureS9.png',
       width = 40,height = 30,units = 'cm')
