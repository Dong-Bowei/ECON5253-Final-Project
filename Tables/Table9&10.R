########################################################################################################
# Event Study                                                                                          #
########################################################################################################



# Only focus on tornadoes that 
# Ocurred between 2000 - 2020
# EF4 and EF5 rated

# Create a copy of the ES dataset
ES1 <- soi_tornado_5 %>% filter(Year > 1999)

# Assuming 'ES' is your dataset containing tornado data

# Create a binary indicator 'treated' to mark treated counties
ES1$treat <- ifelse(ES1$County %in% unique(ES1$County[ES1$Mag > 4]), 1, 0)

# replace the ones with multiple hits 

ES1$treat[ES1$County == "01049" & ES1$Year == 2010] <- 0
ES1$treat[ES1$County == "01071" & ES1$Year == 2008] <- 0
ES1$treat[ES1$County == "29145" & ES1$Year == 2008] <- 0
ES1$treat[ES1$County == "29157" & ES1$Year == 2017] <- 0
ES1$treat[ES1$County == "31179" & ES1$Year == 2013] <- 0
ES1$treat[ES1$County == "40027" & ES1$Year == 2003] <- 0
ES1$treat[ES1$County == "40027" & ES1$Year == 2010] <- 0
ES1$treat[ES1$County == "47113" & ES1$Year == 2003] <- 0

# Assuming 'ES' is your dataset containing county-level tornado data
# Ensure 'Year' and 'Mag' are columns in your dataset

# Assuming 'ES' is your dataset containing county-level tornado data
# Ensure 'Year' and 'Mag' are columns in your dataset

ES1$treated <- ifelse(ES1$Mag > 3, 1, 0)

last_treated_year <- with(ES1, tapply(Year[treated == 1], County[treated == 1], FUN = max))

# Create a new variable _nfd based on the last treated year for each county
ES1$`_nfd` <- last_treated_year[ES1$County]

# Display the resulting dataset with the new _nfd variable
head(ES1)

ES1$post <- ifelse(ES1$Year >= ES1$`_nfd`, 1, 0)

# Set 'year_treated' to 10000 where 'treat' is 0
ES1$year_treated <- ifelse(ES1$treat == 0 | is.na(ES1$treat), 10000, ES1$`_nfd`)

ES1$time_to_treat <- ES1$Year - ES1$`_nfd`

# Set 'post' to 0 where 'treat' is 0
# dat1$time_to_treat[dat1$treat == 0] <- 0


ES1$time_to_treat[ES1$treat == 0] <- 0
ES1$time_to_treat <- as.vector(ES1$time_to_treat)
ES1$treat <- as.vector(ES1$treat)



mod_twfe = feols(Netflow ~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                   Pop + Gender_Gap + White + Black  |                    ## Other controls
                   County + Year,                             ## FEs
                 cluster = ~Year,                          ## Clustered SEs
                 data = ES1)

iplot(mod_twfe.c, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

etable(mod_twfe.c, tex = TRUE)


ES1.10 <- ES1 %>% filter(time_to_treat > -11 & time_to_treat < 11)

# Following Sun and Abraham, we give our never-treated units a fake "treatment"
# date far outside the relevant study period.
# ES[, year_treated := ifelse(treat==0, 10000, `_nfd`)]

# Assuming 'dat1' is your dataset and '_nfd' is the variable you want to assign to 'year_treated' when 'treat' is 1

# ------------------------------------------- Construction -------------------------------------------

mod_twfe.i1 = feols( log(Employment_Construction)~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                       Pop + Gender_Gap + White + Black  |                    ## Other controls
                       County + Year,                             ## FEs
                     cluster = ~Year,                          ## Clustered SEs
                     data = ES1.10)

iplot(mod_twfe.i1, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.i1 = feols(log(Employment_Nature) ~ sunab(year_treated, Year) + ## The only thing that's changed
                    Pop + Gender_Gap + White + Black |
                    County + Year,
                  cluster = ~County,
                  data = ES1.10)

iplot(list(mod_twfe.i1, mod_sa.i1), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))

# ------------------------------------------- Information -------------------------------------------

mod_twfe.i2 = feols( log(Employment_Information)~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                       Pop + Gender_Gap + White + Black  |                    ## Other controls
                       County + Year,                             ## FEs
                     cluster = ~Year,                          ## Clustered SEs
                     data = ES1.10)

iplot(mod_twfe.i2, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.i2 = feols(log(Employment_Information) ~ sunab(year_treated, Year) + ## The only thing that's changed
                    Pop + Gender_Gap + White + Black |
                    County + Year,
                  cluster = ~County,
                  data = ES1.10)

iplot(list(mod_twfe.i2, mod_sa.i2), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))


# ------------------------------------------- Educ_Health -------------------------------------------

mod_twfe.i3 = feols( log(Employment_Educ_Health)~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                       Pop + Gender_Gap + White + Black  |                    ## Other controls
                       County + Year,                             ## FEs
                     cluster = ~Year,                          ## Clustered SEs
                     data = ES1.10)

iplot(mod_twfe.i3, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.i3 = feols(log(Employment_Educ_Health) ~ sunab(year_treated, Year) + ## The only thing that's changed
                    Pop + Gender_Gap + White + Black |
                    County + Year,
                  cluster = ~County,
                  data = ES1.10)

iplot(list(mod_twfe.i3, mod_sa.i3), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))


# ------------------------------------------- Manufacturing -------------------------------------------

mod_twfe.i4 = feols( log(Employment_Manufacturing)~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                       Pop + Gender_Gap + White + Black  |                    ## Other controls
                       County + Year,                             ## FEs
                     cluster = ~Year,                          ## Clustered SEs
                     data = ES1.10)

iplot(mod_twfe.i4, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.i4 = feols(log(Employment_Manufacturing) ~ sunab(year_treated, Year) + ## The only thing that's changed
                    Pop + Gender_Gap + White + Black |
                    County + Year,
                  cluster = ~County,
                  data = ES1.10)

iplot(list(mod_twfe.i4, mod_sa.i4), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))

# ------------------------------------------- Finance -------------------------------------------

mod_twfe.i5 = feols( log(Employment_Finance)~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                       Pop + Gender_Gap + White + Black  |                    ## Other controls
                       County + Year,                             ## FEs
                     cluster = ~Year,                          ## Clustered SEs
                     data = ES1.10)

iplot(mod_twfe.i5, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.i5 = feols(log(Employment_Finance) ~ sunab(year_treated, Year) + ## The only thing that's changed
                    Pop + Gender_Gap + White + Black |
                    County + Year,
                  cluster = ~County,
                  data = ES1.10)

iplot(list(mod_twfe.i5, mod_sa.i5), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))

# ------------------------------------------- Hospitality -------------------------------------------

mod_twfe.i6 = feols( log(Employment_Hospitality)~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                       Pop + Gender_Gap + White + Black  |                    ## Other controls
                       County + Year,                             ## FEs
                     cluster = ~Year,                          ## Clustered SEs
                     data = ES1.10)

iplot(mod_twfe.i6, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.i6 = feols(log(Employment_Hospitality) ~ sunab(year_treated, Year) + ## The only thing that's changed
                    Pop + Gender_Gap + White + Black |
                    County + Year,
                  cluster = ~County,
                  data = ES1.10)

iplot(list(mod_twfe.i6, mod_sa.i6), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))

# ------------------------------------------- Nature -------------------------------------------

mod_twfe.i7 = feols( log(Employment_Nature)~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                       Pop + Gender_Gap + White + Black  |                    ## Other controls
                       County + Year,                             ## FEs
                     cluster = ~Year,                          ## Clustered SEs
                     data = ES1.10)

iplot(mod_twfe.i7, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.i7 = feols(log(Employment_Nature) ~ sunab(year_treated, Year) + ## The only thing that's changed
                    Pop + Gender_Gap + White + Black |
                    County + Year,
                  cluster = ~County,
                  data = ES1.10)

iplot(list(mod_twfe.i7, mod_sa.i7), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))


# ------------------------------------------- Trade -------------------------------------------

mod_twfe.i8 = feols( log(Employment_Trade)~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                       Pop + Gender_Gap + White + Black  |                    ## Other controls
                       County + Year,                             ## FEs
                     cluster = ~Year,                          ## Clustered SEs
                     data = ES1.10)

iplot(mod_twfe.i8, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.i8 = feols(log(Employment_Trade) ~ sunab(year_treated, Year) + ## The only thing that's changed
                    Pop + Gender_Gap + White + Black |
                    County + Year,
                  cluster = ~County,
                  data = ES1.10)

iplot(list(mod_twfe.i8, mod_sa.i8), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))


# ------------------------------------------- Professional -------------------------------------------

mod_twfe.i9 = feols( log(Employment_Professional)~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                       Pop + Gender_Gap + White + Black  |                    ## Other controls
                       County + Year,                             ## FEs
                     cluster = ~Year,                          ## Clustered SEs
                     data = ES1.10)

iplot(mod_twfe.i9, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.i9 = feols(log(Employment_Professional) ~ sunab(year_treated, Year) + ## The only thing that's changed
                    Pop + Gender_Gap + White + Black |
                    County + Year,
                  cluster = ~County,
                  data = ES1.10)

iplot(list(mod_twfe.i9, mod_sa.i9), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))




etable(mod_twfe.i1,mod_twfe.i2,mod_twfe.i3, mod_twfe.i4,mod_twfe.i5,mod_twfe.i6, mod_twfe.i7, mod_twfe.i8,mod_twfe.i9, tex = TRUE)








# ------------------------------------------- Construction -------------------------------------------

mod_twfe.i11 = feols( log(Weekly_Wage_Construction)~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                        Pop + Gender_Gap + White + Black  |                    ## Other controls
                        County + Year,                             ## FEs
                      cluster = ~Year,                          ## Clustered SEs
                      data = ES1.10)

iplot(mod_twfe.i11, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.i11 = feols(log(Weekly_Wage_Construction) ~ sunab(year_treated, Year) + ## The only thing that's changed
                     Pop + Gender_Gap + White + Black |
                     County + Year,
                   cluster = ~County,
                   data = ES1.10)

iplot(list(mod_twfe.i11, mod_sa.i11), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))

# ------------------------------------------- Information -------------------------------------------

mod_twfe.i21 = feols( log(Weekly_Wage_Information)~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                        Pop + Gender_Gap + White + Black  |                    ## Other controls
                        County + Year,                             ## FEs
                      cluster = ~Year,                          ## Clustered SEs
                      data = ES1.10)

iplot(mod_twfe.i21, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.i21 = feols(log(Weekly_Wage_Information) ~ sunab(year_treated, Year) + ## The only thing that's changed
                     Pop + Gender_Gap + White + Black |
                     County + Year,
                   cluster = ~County,
                   data = ES1.10)

iplot(list(mod_twfe.i21, mod_sa.i21), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))


# ------------------------------------------- Educ_Health -------------------------------------------

mod_twfe.i31 = feols( log(Weekly_Wage_Educ_Health)~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                        Pop + Gender_Gap + White + Black  |                    ## Other controls
                        County + Year,                             ## FEs
                      cluster = ~Year,                          ## Clustered SEs
                      data = ES1.10)

iplot(mod_twfe.i31, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.i31 = feols(log(Weekly_Wage_Educ_Health) ~ sunab(year_treated, Year) + ## The only thing that's changed
                     Pop + Gender_Gap + White + Black |
                     County + Year,
                   cluster = ~County,
                   data = ES1.10)

iplot(list(mod_twfe.i31, mod_sa.i31), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))


# ------------------------------------------- Manufacturing -------------------------------------------

mod_twfe.i41 = feols( log(Weekly_Wage_Manufacturing)~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                        Pop + Gender_Gap + White + Black  |                    ## Other controls
                        County + Year,                             ## FEs
                      cluster = ~Year,                          ## Clustered SEs
                      data = ES1.10)

iplot(mod_twfe.i41, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.i41 = feols(log(Weekly_Wage_Manufacturing) ~ sunab(year_treated, Year) + ## The only thing that's changed
                     Pop + Gender_Gap + White + Black |
                     County + Year,
                   cluster = ~County,
                   data = ES1.10)

iplot(list(mod_twfe.i41, mod_sa.i41), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))

# ------------------------------------------- Finance -------------------------------------------

mod_twfe.i51 = feols( log(Weekly_Wage_Finance)~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                        Pop + Gender_Gap + White + Black  |                    ## Other controls
                        County + Year,                             ## FEs
                      cluster = ~Year,                          ## Clustered SEs
                      data = ES1.10)

iplot(mod_twfe.i51, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.i51 = feols(log(Weekly_Wage_Finance) ~ sunab(year_treated, Year) + ## The only thing that's changed
                     Pop + Gender_Gap + White + Black |
                     County + Year,
                   cluster = ~County,
                   data = ES1.10)

iplot(list(mod_twfe.i51, mod_sa.i51), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))

# ------------------------------------------- Hospitality -------------------------------------------

mod_twfe.i61 = feols( log(Weekly_Wage_Hospitality)~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                        Pop + Gender_Gap + White + Black  |                    ## Other controls
                        County + Year,                             ## FEs
                      cluster = ~Year,                          ## Clustered SEs
                      data = ES1.10)

iplot(mod_twfe.i61, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.i61 = feols(log(Weekly_Wage_Hospitality) ~ sunab(year_treated, Year) + ## The only thing that's changed
                     Pop + Gender_Gap + White + Black |
                     County + Year,
                   cluster = ~County,
                   data = ES1.10)

iplot(list(mod_twfe.i61, mod_sa.i61), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))

# ------------------------------------------- Nature -------------------------------------------

mod_twfe.i71 = feols( log(Weekly_Wage_Nature)~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                        Pop + Gender_Gap + White + Black  |                    ## Other controls
                        County + Year,                             ## FEs
                      cluster = ~Year,                          ## Clustered SEs
                      data = ES1.10)

iplot(mod_twfe.i71, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.i71 = feols(log(Weekly_Wage_Nature) ~ sunab(year_treated, Year) + ## The only thing that's changed
                     Pop + Gender_Gap + White + Black |
                     County + Year,
                   cluster = ~County,
                   data = ES1.10)

iplot(list(mod_twfe.i71, mod_sa.i71), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))


# ------------------------------------------- Trade -------------------------------------------

mod_twfe.i81 = feols( log(Weekly_Wage_Trade)~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                        Pop + Gender_Gap + White + Black  |                    ## Other controls
                        County + Year,                             ## FEs
                      cluster = ~Year,                          ## Clustered SEs
                      data = ES1.10)

iplot(mod_twfe.i81, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.i81 = feols(log(Weekly_Wage_Trade) ~ sunab(year_treated, Year) + ## The only thing that's changed
                     Pop + Gender_Gap + White + Black |
                     County + Year,
                   cluster = ~County,
                   data = ES1.10)

iplot(list(mod_twfe.i81, mod_sa.i81), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))


# ------------------------------------------- Professional -------------------------------------------

mod_twfe.i91 = feols( log(Weekly_Wage_Professional)~ i(time_to_treat, treat, ref = -1) +  ## Our key interaction: time × treatment status
                        Pop + Gender_Gap + White + Black  |                    ## Other controls
                        County + Year,                             ## FEs
                      cluster = ~Year,                          ## Clustered SEs
                      data = ES1.10)

iplot(mod_twfe.i91, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_sa.i91 = feols(log(Weekly_Wage_Professional) ~ sunab(year_treated, Year) + ## The only thing that's changed
                     Pop + Gender_Gap + White + Black |
                     County + Year,
                   cluster = ~County,
                   data = ES1.10)

iplot(list(mod_twfe.i91, mod_sa.i91), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))




etable(mod_twfe.i11,mod_twfe.i21,mod_twfe.i31, mod_twfe.i41,mod_twfe.i51,mod_twfe.i61, mod_twfe.i71, mod_twfe.i81,mod_twfe.i91, tex = TRUE)