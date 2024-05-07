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