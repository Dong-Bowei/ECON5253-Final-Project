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
as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_5)

etable(reg.35, reg.36, reg.37, reg.38, reg.39, reg.40, reg.41, reg.42, reg.43, tex = TRUE)