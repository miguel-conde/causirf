source("./causality/utils_sto_causal.R")

# DATA --------------------------------------------------------------------

data(temp_co2_data)


# PAPER -------------------------------------------------------------------

####
obj_causal_temp_co2 <- est_causal(temp_co2_data, "temp", "co2", J = 20, lambda = 10)
autoplot.est_causal(obj_causal_temp_co2)
evr(obj_causal_temp_co2)

####
obj_causal_co2_temp <- est_causal(temp_co2_data, "co2", "temp", J = 20, lambda = 10)
autoplot.est_causal(obj_causal_co2_temp)
evr(obj_causal_co2_temp)
