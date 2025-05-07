# AAA ----

fit_AES_AAA <- auto.adam(ts_avalanche_wet_2022,model="AAA", lags=c(1,1,12),
                          orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fit_AES_AAA




# AAN ----

fit_AES_AAN <- auto.adam(ts_avalanche_wet_2022, model = "AAN", lags=c(1,1,12),orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fit_AES_AAN




# ANA ----

fit_AES_ANA <- auto.adam(ts_avalanche_wet_2022, model = "ANA", lags=c(1,1,12),orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fit_AES_ANA




# NAA ----

fit_AES_NAA <- auto.adam(ts_avalanche_wet_2022, model = "NAA", lags=c(1,1,12),orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fit_AES_NAA



# ANN ----

fit_AES_ANN <- auto.adam(ts_avalanche_wet_2022, model = "ANN", lags=c(1,1,12),orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fit_AES_ANN





# NAN ----

fit_AES_NAN <- auto.adam(ts_avalanche_wet_2022, model = "NAN", lags=c(1,1,12),orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fit_AES_NAN




# NNA ----

fit_AES_NNA <- auto.adam(ts_avalanche_wet_2022, model = "NNA", lags=c(1,1,12),orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fit_AES_NNA




# NNN ----

fit_AES_NNN <- auto.adam(ts_avalanche_wet_2022, model = "NNN", lags=c(1,1,12),orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fit_AES_NNN







