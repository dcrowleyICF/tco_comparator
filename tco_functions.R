library(tidyverse)

price_elec <- 0.5 #dollars per kWh
price_gas <-  5 #dollars per gallon

eff_ICE <-  30 #mpg
eff_EV_mpge <- 100 #mpge
eff_EV <- eff_EV_mpge/33.705 #convert to mi/kWh

cost_maint_ICE <- 0.1013 #dollars per mile
cost_maint_EV <- 0.0789 # dollars per mile

cost_drive_EV <- price_elec/eff_EV
cost_drive_ICE <- price_gas/eff_ICE

price_EV = 10000
price_ICE = 10000
borrowing_rate_percent = 8
borrowing_rate = borrowing_rate_percent/100
loan_term = 84 #months
lifetime_mileage = 150000
annual_mileage = 15000
mileage_EV = 0
mileage_ICE = 0
EV_remaining_mileage = lifetime_mileage - mileage_EV
ICE_remaining_mileage = lifetime_mileage - mileage_ICE
payment_EV = price_EV*borrowing_rate*(1+borrowing_rate)^loan_term/((1+borrowing_rate)^loan_term - 1)
payment_ICE = price_ICE*borrowing_rate*(1+borrowing_rate)^loan_term/((1+borrowing_rate)^loan_term - 1)
cost_own_EV = payment_EV/annual_mileage
cost_own_ICE = payment_ICE/annual_mileage

tco_ICE = cost_drive_ICE +cost_maint_ICE + cost_own_ICE
tco_EV = cost_drive_EV +cost_maint_EV + cost_own_EV
annualTCO_ICE = tco_ICE*annual_mileage
annualTCO_EV =  tco_EV*annual_mileage
EV_equivalent_MPG = price_gas/(cost_drive_EV+cost_maint_EV+cost_own_EV-cost_maint_ICE-cost_own_ICE) #mpg where TCO is equivalent, not cost to drive
EV_equivalent_MPG_driveonly = price_gas/(cost_drive_EV) #mpg where cost to drive is equivalent
