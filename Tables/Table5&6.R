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

