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