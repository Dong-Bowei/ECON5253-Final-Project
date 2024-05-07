
X1950_2021_all_tornadoes <- read_csv("1950-2021_all_tornadoes.csv")

tornado <- X1950_2021_all_tornadoes %>% filter(yr > 1991 & yr < 2021) 

tornado$stf <- sprintf("%02s", tornado$stf)
tornado$f1 <- sprintf("%03s", tornado$f1)

tornado$County <- paste0(tornado$stf, tornado$f1)  

########################################################################################################
# County-Level Model                                                                                   #
########################################################################################################

tornado_1 <- tornado %>% select(30, 2, 11:15) %>% 
  rename(Year = yr) %>%
  filter(mag != -9 ) %>%#| mag != 0 | inj != 0 | fat != 0 | loss != 0 | closs != 0
  group_by(County, Year) %>%
  summarise(Mag = max(mag) + 1,
            Inj = sum(inj),
            Fat = sum(fat), 
            Loss = sum(loss),
            Closs = sum(closs),
            Count = n()) %>%
  unique()

# Combine this with my migration data set

soi.combine2 <- soi.combine1 %>% drop_na()

soi_tornado <- left_join(soi.combine2, tornado_1, by = c("County", "Year"))
soi_tornado_1 <- soi_tornado %>%
  replace_na(list(Mag = 0, Inj = 0, Fat = 0, Loss = 0, Closs = 0, Count = 0))  # Replace NA with 0


# Do the regression
 soi_tornado_1$Grossflow <- log(soi_tornado_1$Outflow + soi_tornado_1$Inflow)

 soi_tornado_1$Outflow <- log(soi_tornado_1$Outflow)
 
 soi_tornado_1$Inflow <- log1p(soi_tornado_1$Inflow)
 
 # soi_tornado_1$Inflow <- ifelse(soi_tornado_1$Inflow > 0, log(soi_tornado_1$Inflow),
                                      #  log(1))  # Handle zero values with a small epsilon
 soi_tornado_1$Netflow <- log1p(soi_tornado_1$Netflow)

 # soi_tornado_1$Netflow <- ifelse(soi_tornado_1$Netflow > 0, log(soi_tornado_1$Netflow),
                                   #  ifelse(soi_tornado_1$Netflow < 0, -log(-soi_tornado_1$Netflow),
                                   #        log(1)))  # Handle zero values with a small epsilon

 
 # soi_tornado_1$Grossflow <- soi_tornado_1$Outflow + soi_tornado_1$Inflow
 # soi_tornado_1$Grossflow <- log(soi_tornado_1$Grossflow)
 
 # soi_tornado_1$Mag <- log1p(soi_tornado_1$Black)
 # soi_tornado_1$Mag <- ifelse(soi_tornado_1$Mag > 0, log(soi_tornado_1$Mag),
                                       # log(1))  # Handle zero values with a small epsilon
 
 # soi_tornado_1$Inj <- log1p(soi_tornado_1$Inj)
 # soi_tornado_1$Inj <- ifelse(soi_tornado_1$Inj > 0, log(soi_tornado_1$Inj),
                                   # log(1))  # Number is small, no need to take log
 
 # soi_tornado_1$Fat <- log1p(soi_tornado_1$Fat)
 # soi_tornado_1$Fat <- ifelse(soi_tornado_1$Fat > 0, log(soi_tornado_1$Fat),
                            # log(1))  # Number is small, no need to take log
 
 soi_tornado_1$Loss <- log1p(soi_tornado_1$Loss)
 # soi_tornado_1$Loss <- ifelse(soi_tornado_1$Loss > 0, log(soi_tornado_1$Loss),
                            # log(1))  # Handle zero values with a small epsilon
 
 soi_tornado_1$Closs <- log1p(soi_tornado_1$Closs)
 # soi_tornado_1$Closs <- ifelse(soi_tornado_1$Closs > 0, log(soi_tornado_1$Closs),
                             # log(1))  # Handle zero values with a small epsilon
 
 # soi_tornado_1$Count <- log1p(soi_tornado_1$Count)
 # soi_tornado_1$Count <- ifelse(soi_tornado_1$Count > 0, log(soi_tornado_1$Count),
                              # log(1))  # Number is small, no need to take log
 
 soi_tornado_1$Pop <- log(soi_tornado_1$Pop)
 # soi_tornado_1$Gender_Gap <- log(soi_tornado_1$Gender_Gap)
 # soi_tornado_1$White <- log(soi_tornado_1$White)
 # soi_tornado_1$Black <- log1p(soi_tornado_1$Black)
 # soi_tornado_1$Black <- ifelse(soi_tornado_1$Black > 0, log(soi_tornado_1$Black),
                              # log(1))  # Handle zero values with a small epsilon
 
reg.1 <- feols(Inflow ~ Pop + Gender_Gap + White + Black + 
                 as.factor(Mag) | County + Year, data = soi_tornado_1)

reg.2 <- feols(Outflow ~ Pop + Gender_Gap + White + Black + 
                 as.factor(Mag) | County + Year, data = soi_tornado_1)

reg.3 <- feols(Netflow ~ Pop + Gender_Gap + White + Black + 
                 as.factor(Mag) | County + Year, data = soi_tornado_1)

# reg.4 <- feols(Grossflow ~ Pop + Gender_Gap + White + Black + 
               #  as.factor(Mag) | County + Year, data = soi_tornado_1)

reg.5 <- feols(Inflow ~  
                 as.factor(Mag) | County + Year, data = soi_tornado_1)

reg.6 <- feols(Outflow ~ 
                 as.factor(Mag) | County + Year, data = soi_tornado_1)

reg.7 <- feols(Netflow ~ 
                 as.factor(Mag) | County + Year, data = soi_tornado_1)

# reg.8 <- feols(Grossflow ~ 
               #  as.factor(Mag) | County + Year, data = soi_tornado_1)

etable(reg.5, reg.1, reg.6, reg.2, reg.7, reg.3, tex = TRUE)


# ----------------------------------------- Median Housing Price -----------------------------------------                                                                                 


Median_Housing$County <- sprintf("%05s", Median_Housing$County)
soi_tornado_2 <- left_join(soi_tornado_1, Median_Housing, by = c("County", "Year"))

reg.7 <- feols(log(Housing_Med) ~ Pop + Gender_Gap + White + Black + 
                 as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_2)

reg.8 <- feols(log(Housing_Med) ~ 
                 as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_2)

etable(reg.7, reg.8, tex = TRUE)

# ----------------------------------------- Labor Market Variable -----------------------------------------       

soi_tornado_3 <- left_join(soi_tornado_2, unemp.1, by = c("County", "Year"))

reg.9 <- feols(log(Civilian_Labor_Force) ~ Pop + Gender_Gap + White + Black + 
                 as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_3)

reg.10 <- feols(log(Civilian_Labor_Force) ~ 
                 as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_3)

reg.11 <- feols(Unemp_Rate ~ Pop + Gender_Gap + White + Black + 
                 as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_3)

reg.12 <- feols(Unemp_Rate ~ 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_3)

# ---------------------------------------------- Repulican -----------------------------------------------       

soi_tornado_4 <- left_join(soi_tornado_3, election.4, by = c("County", "Year"))

reg.13 <- feols(Republican ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_4)

reg.14 <- feols(Republican ~ 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_4)

etable(reg.8, reg.7, reg.10, reg.9, reg.12, reg.11, reg.14, reg.13, tex = TRUE)

# ---------------------------------------------- Industry -----------------------------------------------       


soi_tornado_5 <- left_join(soi_tornado_4, all_industry_wide1, by = c("County", "Year"))


reg.15 <- feols(log(Establishments_Construction) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.16 <- feols(log(Establishments_Information) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.17 <- feols(log(Establishments_Educ_Health) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.18 <- feols(log(Establishments_Manufacturing) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.19 <- feols(log(Establishments_Finance) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.20 <- feols(log(Establishments_Hospitality) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.21 <- feols(log(Establishments_Nature) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.22 <- feols(log(Establishments_Trade) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.23 <- feols(log(Establishments_Professional) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

# reg.24 <- feols(log(Establishments_Other_Services) ~ Pop + Gender_Gap + White + Black + 
              #    as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

etable(reg.15, reg.16, reg.17, reg.18, reg.19, reg.20, reg.21, reg.22, reg.23, tex = TRUE)


reg.25 <- feols(log(Employment_Construction) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.26 <- feols(log(Employment_Information) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.27 <- feols(log(Employment_Educ_Health) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.28 <- feols(log(Employment_Manufacturing) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.29 <- feols(log(Employment_Finance) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.30 <- feols(log(Employment_Hospitality) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.31 <- feols(log(Employment_Nature) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.32 <- feols(log(Employment_Trade) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.33 <- feols(log(Employment_Professional) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

# reg.34 <- feols(log(Employment_Other_Services) ~ Pop + Gender_Gap + White + Black + 
                 # as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

etable(reg.25, reg.26, reg.27, reg.28, reg.29, reg.30, reg.31, reg.32, reg.33, tex = TRUE)


reg.35 <- feols(log(Weekly_Wage_Construction) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.36 <- feols(log(Weekly_Wage_Information) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.37 <- feols(log(Weekly_Wage_Educ_Health) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.38 <- feols(log(Weekly_Wage_Manufacturing) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.39 <- feols(log(Weekly_Wage_Finance) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.40 <- feols(log(Weekly_Wage_Hospitality) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.41 <- feols(log(Weekly_Wage_Nature) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.42 <- feols(log(Weekly_Wage_Trade) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

reg.43 <- feols(log(Weekly_Wage_Professional) ~ Pop + Gender_Gap + White + Black + 
                  as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

# reg.44 <- feols(log(Weekly_Wage_Other_Services) ~ Pop + Gender_Gap + White + Black + 
                 # as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

etable(reg.35, reg.36, reg.37, reg.38, reg.39, reg.40, reg.41, reg.42, reg.43, tex = TRUE)





########################################################################################################
# Gravity Model                                                                                        #
########################################################################################################

tornado_1_o <- tornado_1 %>%
  rename(County_Origin = County, Mag_o = Mag, Inj_o = Inj, Fat_o = Fat, Loss_o = Loss, Closs_o = Closs, Count_o = Count)

tornado_1_d <- tornado_1 %>%
  rename(County_Destin = County, Mag_d = Mag, Inj_d = Inj, Fat_d = Fat, Loss_d = Loss, Closs_d = Closs, Count_d = Count)

soi_pop_o <- left_join(soi_pop, tornado_1_o, by = c("County_Origin", "Year"))
soi_pop_d <- left_join(soi_pop_o, tornado_1_d, by = c("County_Destin", "Year"))

soi_pop_d1 <- soi_pop_d %>%
  replace_na(list(Mag_o = 0, Inj_o = 0, Fat_o = 0, Loss_o = 0, Closs_o = 0, Count_o = 0, 
                  Mag_d = 0, Inj_d = 0, Fat_d = 0, Loss_d = 0, Closs_d = 0, Count_d = 0))  # Replace NA with 0

 soi_pop_d1$Mag = soi_pop_d1$Mag_d - soi_pop_d1$Mag_o
# soi_pop_d1$Mag <- ifelse(soi_pop_d1$Mag_d - soi_pop_d1$Mag_o > 0,
                      #   log(soi_pop_d1$Mag_d - soi_pop_d1$Mag_o),
                      #   ifelse(soi_pop_d1$Mag_d - soi_pop_d1$Mag_o < 0,
                      #          -log(- (soi_pop_d1$Mag_d - soi_pop_d1$Mag_o)),
                      #          log(1)))  # Handle zero values with a small epsilon

 soi_pop_d1$Inj = soi_pop_d1$Inj_d - soi_pop_d1$Inj_o
# soi_pop_d1$Inj <- ifelse(soi_pop_d1$Inj_d - soi_pop_d1$Inj_o > 0,
                      #   log(soi_pop_d1$Inj_d - soi_pop_d1$Inj_o),
                      #   ifelse(soi_pop_d1$Inj_d - soi_pop_d1$Inj_o < 0,
                      #          -log(- (soi_pop_d1$Inj_d - soi_pop_d1$Inj_o)),
                      #          log(1)))  # Handle zero values with a small epsilon

 soi_pop_d1$Fat = soi_pop_d1$Fat_d - soi_pop_d1$Fat_o
# soi_pop_d1$Fat <- ifelse(soi_pop_d1$Fat_d - soi_pop_d1$Fat_o > 0,
                      #   log(soi_pop_d1$Fat_d - soi_pop_d1$Fat_o),
                      #   ifelse(soi_pop_d1$Fat_d - soi_pop_d1$Fat_o < 0,
                      #          -log(- (soi_pop_d1$Fat_d - soi_pop_d1$Fat_o)),
                      #          log(1)))  # Handle zero values with a small epsilon

 soi_pop_d1$Loss = soi_pop_d1$Loss_d - soi_pop_d1$Loss_o 
# soi_pop_d1$Loss <- ifelse(soi_pop_d1$Loss_d - soi_pop_d1$Loss_o > 0,
                      #   log(soi_pop_d1$Loss_d - soi_pop_d1$Loss_o),
                      #   ifelse(soi_pop_d1$Loss_d - soi_pop_d1$Loss_o < 0,
                      #          -log(- (soi_pop_d1$Loss_d - soi_pop_d1$Loss_o)),
                      #          log(1)))  # Handle zero values with a small epsilon

 soi_pop_d1$Closs = soi_pop_d1$Closs_d - soi_pop_d1$Closs_o
# soi_pop_d1$Closs <- ifelse(soi_pop_d1$Closs_d - soi_pop_d1$Closs_o > 0,
                      #    log(soi_pop_d1$Closs_d - soi_pop_d1$Closs_o),
                      #    ifelse(soi_pop_d1$Closs_d - soi_pop_d1$Closs_o < 0,
                      #           -log(- (soi_pop_d1$Closs_d - soi_pop_d1$Closs_o)),
                      #           log(1)))  # Handle zero values with a small epsilon

 soi_pop_d1$Count = soi_pop_d1$Count_d - soi_pop_d1$Count_o
# soi_pop_d1$Count <- ifelse(soi_pop_d1$Count_d - soi_pop_d1$Count_o > 0,
                      #     log(soi_pop_d1$Count_d - soi_pop_d1$Count_o),
                      #     ifelse(soi_pop_d1$Count_d - soi_pop_d1$Count_o < 0,
                      #            -log(- (soi_pop_d1$Count_d - soi_pop_d1$Count_o)),
                      #            log(1)))  # Handle zero values with a small epsilon

soi_pop_d1$Pop = soi_pop_d1$Pop.d - soi_pop_d1$Pop.o
# soi_pop_d1$Pop <- ifelse(soi_pop_d1$Pop.d - soi_pop_d1$Pop.o > 0,
                      #     log(soi_pop_d1$Pop.d - soi_pop_d1$Pop.o),
                      #     ifelse(soi_pop_d1$Pop.d - soi_pop_d1$Pop.o < 0,
                      #            -log(- (soi_pop_d1$Pop.d - soi_pop_d1$Pop.o)),
                      #            log(1)))  # Handle zero values with a small epsilon

 soi_pop_d1$Gender_Gap = soi_pop_d1$Gender_Gap.d - soi_pop_d1$Gender_Gap.o
# soi_pop_d1$Gender_Gap <- ifelse(soi_pop_d1$Gender_Gap.d - soi_pop_d1$Gender_Gap.o > 0,
                      #   log(soi_pop_d1$Gender_Gap.d - soi_pop_d1$Gender_Gap.o),
                      #   ifelse(soi_pop_d1$Gender_Gap.d - soi_pop_d1$Gender_Gap.o < 0,
                      #          -log(- (soi_pop_d1$Gender_Gap.d - soi_pop_d1$Gender_Gap.o)),
                      #          log(1)))  # Handle zero values with a small epsilon

 soi_pop_d1$White = soi_pop_d1$White.d - soi_pop_d1$White.o
# soi_pop_d1$White <- ifelse(soi_pop_d1$White.d - soi_pop_d1$White.o > 0,
                      #          log(soi_pop_d1$White.d - soi_pop_d1$White.o),
                      #          ifelse(soi_pop_d1$White.d - soi_pop_d1$White.o < 0,
                      #                 -log(- (soi_pop_d1$White.d - soi_pop_d1$White.o)),
                      #                 log(1)))  # Handle zero values with a small epsilon

 soi_pop_d1$Black = soi_pop_d1$Black.d - soi_pop_d1$Black.o
# soi_pop_d1$Black <- ifelse(soi_pop_d1$Black.d - soi_pop_d1$Black.o > 0,
                      #     log(soi_pop_d1$Black.d - soi_pop_d1$Black.o),
                      #     ifelse(soi_pop_d1$Black.d - soi_pop_d1$Black.o < 0,
                      #            -log(- (soi_pop_d1$Black.d - soi_pop_d1$Black.o)),
                      #            log(1)))  # Handle zero values with a small epsilon



gri.1 <- feols(n2 ~ mi_to_county +
                 Mag + Inj + Fat + Loss + Closs + Count| County_Origin + County_Destin + Year, data = soi_pop_d1)

gri.2 <- feols(n2 ~ mi_to_county +
                 Mag + Inj + Fat + Loss + Closs + Count| OD + Year, data = soi_pop_d1)

gri.3 <- feols(n2 ~ mi_to_county + Pop + Gender_Gap + White + Black +
                 Mag + Inj + Fat + Loss + Closs + Count| County_Origin + County_Destin + Year, data = soi_pop_d1)

gri.4 <- feols(n2 ~ mi_to_county + Pop + Gender_Gap + White + Black +
                 Mag + Inj + Fat + Loss + Closs + Count| OD + Year, data = soi_pop_d1)

etable(gri.1, gri.3, gri.2, gri.4, tex = TRUE)

mean(soi_pop_d1$n2)

########################################################################################################
# Event Study                                                                                          #
########################################################################################################



# Only focus on tornadoes that 
  # Ocurred between 2000 - 2020
  # EF4 and EF5 rated

# Create a copy of the ES dataset
ES <- soi_tornado_4 %>% filter(Year > 1999)

# Assuming 'ES' is your dataset containing tornado data

# Create a binary indicator 'treated' to mark treated counties
ES$treat <- ifelse(ES$County %in% unique(ES$County[ES$Mag > 4]), 1, 0)

# replace the ones with multiple hits 

ES$treat[ES$County == "01049" & ES$Year == 2010] <- 0
ES$treat[ES$County == "01071" & ES$Year == 2008] <- 0
ES$treat[ES$County == "29145" & ES$Year == 2008] <- 0
ES$treat[ES$County == "29157" & ES$Year == 2017] <- 0
ES$treat[ES$County == "31179" & ES$Year == 2013] <- 0
ES$treat[ES$County == "40027" & ES$Year == 2003] <- 0
ES$treat[ES$County == "40027" & ES$Year == 2010] <- 0
ES$treat[ES$County == "47113" & ES$Year == 2003] <- 0

# Assuming 'ES' is your dataset containing county-level tornado data
# Ensure 'Year' and 'Mag' are columns in your dataset

# Assuming 'ES' is your dataset containing county-level tornado data
# Ensure 'Year' and 'Mag' are columns in your dataset

ES$treated <- ifelse(ES$Mag > 4, 1, 0)

last_treated_year <- with(ES, tapply(Year[treated == 1], County[treated == 1], FUN = max))

# Create a new variable _nfd based on the last treated year for each county
ES$`_nfd` <- last_treated_year[ES$County]

# Display the resulting dataset with the new _nfd variable
head(ES)

ES$post <- ifelse(ES$Year >= ES$`_nfd`, 1, 0)

# Set 'year_treated' to 10000 where 'treat' is 0
ES$year_treated <- ifelse(ES$treat == 0 | is.na(ES$treat), 10000, ES$`_nfd`)

ES$time_to_treat <- ES$Year - ES$`_nfd`

# Set 'post' to 0 where 'treat' is 0
# dat1$time_to_treat[dat1$treat == 0] <- 0

ES$Housing_Med <- log(ES$Housing_Med)
ES$Civilian_Labor_Force <- log(ES$Civilian_Labor_Force)
# ES$Unemp_Rate <- log(ES$Unemp_Rate)
# ES$Republican <- log(ES$Republican)


ES$time_to_treat[ES$treat == 0] <- 0
ES$time_to_treat <- as.vector(ES$time_to_treat)
ES$treat <- as.vector(ES$treat)



mod_twfe = feols(Netflow ~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                 Pop + Gender_Gap + White + Black  |                    ## Other controls
                   County + Year,                             ## FEs
                  cluster = ~Year,                          ## Clustered SEs
                 data = ES)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

etable(mod_twfe, tex = TRUE)


ES.10 <- ES %>% filter(time_to_treat > -11 & time_to_treat < 11)
  
# Following Sun and Abraham, we give our never-treated units a fake "treatment"
# date far outside the relevant study period.
# ES[, year_treated := ifelse(treat==0, 10000, `_nfd`)]

# Assuming 'dat1' is your dataset and '_nfd' is the variable you want to assign to 'year_treated' when 'treat' is 1

mod_twfe.1 = feols(Inflow ~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                   Pop + Gender_Gap + White + Black  |                    ## Other controls
                   County + Year,                             ## FEs
                 cluster = ~Year,                          ## Clustered SEs
                 data = ES.10)

mod_twfe.2 = feols(Outflow ~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                     Pop + Gender_Gap + White + Black  |                    ## Other controls
                     County + Year,                             ## FEs
                   cluster = ~Year,                          ## Clustered SEs
                   data = ES.10)

mod_twfe.3 = feols(Netflow ~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                     Pop + Gender_Gap + White + Black  |                    ## Other controls
                     County + Year,                             ## FEs
                   cluster = ~Year,                          ## Clustered SEs
                   data = ES.10)

mod_twfe.31 = feols(Grossflow ~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                     Pop + Gender_Gap + White + Black  |                    ## Other controls
                     County + Year,                             ## FEs
                   cluster = ~Year,                          ## Clustered SEs
                   data = ES.10)

etable(mod_twfe.1, mod_twfe.2, mod_twfe.3, mod_twfe.31, tex = TRUE)

mod_twfe.4 = feols(Housing_Med ~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                     Pop + Gender_Gap + White + Black  |                    ## Other controls
                     County + Year,                             ## FEs
                   cluster = ~Year,                          ## Clustered SEs
                   data = ES.10)

mod_twfe.5 = feols(Civilian_Labor_Force ~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                     Pop + Gender_Gap + White + Black  |                    ## Other controls
                     County + Year,                             ## FEs
                   cluster = ~Year,                          ## Clustered SEs
                   data = ES.10)

mod_twfe.6 = feols(Unemp_Rate ~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                     Pop + Gender_Gap + White + Black  |                    ## Other controls
                     County + Year,                             ## FEs
                   cluster = ~Year,                          ## Clustered SEs
                   data = ES.10)

mod_twfe.7 = feols(Republican ~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                     Pop + Gender_Gap + White + Black  |                    ## Other controls
                     County + Year,                             ## FEs
                   cluster = ~Year,                          ## Clustered SEs
                   data = ES.10)

etable(mod_twfe.4, tex = TRUE)

# Now we re-run our model from earlier, but this time swapping out 
# `i(time_to_treat, treat, ref = -1)` for `sunab(year_treated, year)`.
# See `?sunab`.

# ------------------------------------------- Inflow -------------------------------------------

iplot(mod_twfe.4, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.4 = feols(Housing_Med ~ sunab(year_treated, Year) + ## The only thing that's changed
                 Pop + Gender_Gap + White + Black |
                 County + Year,
               cluster = ~County,
               data = ES.10)

iplot(list(mod_twfe.4, mod_sa.4), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))

# ------------------------------------------- Outflow -------------------------------------------

iplot(mod_twfe.2, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.2 = feols(Outflow ~ sunab(year_treated, Year) + ## The only thing that's changed
                   Pop + Gender_Gap + White + Black |
                   County + Year,
                 cluster = ~County,
                 data = ES.10)

iplot(list(mod_twfe.2, mod_sa.2), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))

# ------------------------------------------- Netflow -------------------------------------------

iplot(mod_twfe.3, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.3 = feols(Netflow ~ sunab(year_treated, Year) + ## The only thing that's changed
                   Pop + Gender_Gap + White + Black |
                   County + Year,
                 cluster = ~County,
                 data = ES.10)

iplot(list(mod_twfe.3, mod_sa.3), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))

# ------------------------------------------- Grossflow -------------------------------------------

iplot(mod_twfe.31, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.31 = feols(Grossflow ~ sunab(year_treated, Year) + ## The only thing that's changed
                   Pop + Gender_Gap + White + Black |
                   County + Year,
                 cluster = ~County,
                 data = ES.10)

iplot(list(mod_twfe.31, mod_sa.31), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))


# ------------------------------------------- Housing -------------------------------------------

iplot(mod_twfe.4, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.4 = feols(Housing_Med ~ sunab(year_treated, Year) + ## The only thing that's changed
                   Pop + Gender_Gap + White + Black |
                   County + Year,
                 cluster = ~County,
                 data = ES.10)

iplot(list(mod_twfe.4, mod_sa.4), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))

# ------------------------------------------- Labor -------------------------------------------

iplot(mod_twfe.5, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.5 = feols(Civilian_Labor_Force ~ sunab(year_treated, Year) + ## The only thing that's changed
                   Pop + Gender_Gap + White + Black |
                   County + Year,
                 cluster = ~County,
                 data = ES.10)

iplot(list(mod_twfe.5, mod_sa.5), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))

# ------------------------------------------- Unemployment -------------------------------------------

iplot(mod_twfe.6, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.6 = feols(Unemp_Rate ~ sunab(year_treated, Year) + ## The only thing that's changed
                   Pop + Gender_Gap + White + Black |
                   County + Year,
                 cluster = ~County,
                 data = ES.10)

iplot(list(mod_twfe.6, mod_sa.6), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))

# ------------------------------------------- Republican -------------------------------------------

iplot(mod_twfe.7, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.7 = feols(Republican ~ sunab(year_treated, Year) + ## The only thing that's changed
                   Pop + Gender_Gap + White + Black |
                   County + Year,
                 cluster = ~County,
                 data = ES.10)

iplot(list(mod_twfe.7, mod_sa.7), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))

# ------------------------------------------- Migration -------------------------------------------

etable(mod_twfe.1, mod_sa.1, mod_twfe.2, mod_sa.2, mod_twfe.3, mod_sa.3, mod_twfe.31, mod_sa.31, tex = TRUE)

# ------------------------------------------- Others -------------------------------------------

etable(mod_twfe.4, mod_sa.4, mod_twfe.5, mod_sa.5, mod_twfe.6, mod_sa.6, mod_twfe.7, mod_sa.7, tex = TRUE)





########################################################################################################
# Original Resource                                                                                    #
########################################################################################################

# https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html

# Load and prepare data
 dat = fread("https://raw.githubusercontent.com/LOST-STATS/LOST-STATS.github.io/master/Model_Estimation/Data/Event_Study_DiD/bacon_example.csv") 

# Let's create a more user-friendly indicator of which states received treatment
 dat[, treat := ifelse(is.na(`_nfd`), 0, 1)]

# Create a "time_to_treatment" variable for each state, so that treatment is
# relative for all treated units. For the never-treated (i.e. control) units,
# we'll arbitrarily set the "time_to_treatment" value at 0. This value 
# doesn't really matter, since it will be canceled by the treat==0 interaction
# anyway. But we do want to make sure they aren't NA, otherwise feols would drop 
# these never-treated observations at estimation time and our results will be 
# off.
dat[, time_to_treat := ifelse(treat==1, year - `_nfd`, 0)]


mod_twfe = feols(asmrs ~ i(time_to_treat, treat, ref = -1) + ## Our key interaction: time × treatment status
                   pcinc + asmrh + cases |                    ## Other controls
                   stfips + year,                             ## FEs
                 cluster = ~stfips,                          ## Clustered SEs
                 data = dat)
iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

etable(mod_twfe, tex = TRUE)

# Following Sun and Abraham, we give our never-treated units a fake "treatment"
# date far outside the relevant study period.
dat[, year_treated := ifelse(treat==0, 10000, `_nfd`)]

# Now we re-run our model from earlier, but this time swapping out 
# `i(time_to_treat, treat, ref = -1)` for `sunab(year_treated, year)`.
# See `?sunab`.
mod_sa = feols(asmrs ~ sunab(year_treated, year) + ## The only thing that's changed
                 pcinc + asmrh + cases |
                 stfips + year,
               cluster = ~stfips,
               data = dat)

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))
















