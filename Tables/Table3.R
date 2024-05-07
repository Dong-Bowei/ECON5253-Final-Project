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