# AAA ----

fit_ADAM_ETS <- auto.adam(ts_avalanche_wet_2022,model="AAA", lags=c(1,1,12),
                          orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fit_ADAM_ETS




# AAN ----

fit_ADAM_ETS <- auto.adam(ts_avalanche_wet_2022, model = "AAN", lags=c(1,1,12),orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fit_ADAM_ETS




# ANA ----

fit_ADAM_ETS <- auto.adam(ts_avalanche_wet_2022, model = "ANA", lags=c(1,1,12),orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fit_ADAM_ETS




# NAA ----

fit_ADAM_ETS <- auto.adam(ts_avalanche_wet_2022, model = "NAA", lags=c(1,1,12),orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fit_ADAM_ETS




# ANN ----

fit_ADAM_ETS <- auto.adam(ts_avalanche_wet_2022, model = "ANN", lags=c(1,1,12),orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fit_ADAM_ETS





# NAN ----

fit_ADAM_ETS <- auto.adam(ts_avalanche_wet_2022, model = "NAN", lags=c(1,1,12),orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fit_ADAM_ETS




# NNA ----

fit_ADAM_ETS <- auto.adam(ts_avalanche_wet_2022, model = "NNA", lags=c(1,1,12),orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fit_ADAM_ETS




# NNN ----

fit_ADAM_ETS <- auto.adam(ts_avalanche_wet_2022, model = "NNN", lags=c(1,1,12),orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fit_ADAM_ETS







