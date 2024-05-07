
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
                 as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_1)

reg.2 <- feols(Outflow ~ Pop + Gender_Gap + White + Black + 
                 as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_1)

reg.3 <- feols(Netflow ~ Pop + Gender_Gap + White + Black + 
                 as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_1)

reg.4 <- feols(Grossflow ~ Pop + Gender_Gap + White + Black + 
                 as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_1)

reg.5 <- feols(Inflow ~  
                 as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_1)

reg.6 <- feols(Outflow ~ 
                 as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_1)

reg.7 <- feols(Netflow ~ 
                 as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_1)

 reg.8 <- feols(Grossflow ~ 
                 as.factor(Mag) + Inj + Fat + Loss + Closs + Count| County + Year, data = soi_tornado_1)

etable(reg.5, reg.1, reg.6, reg.2, reg.7, reg.3, reg.8, reg.4, tex = TRUE)