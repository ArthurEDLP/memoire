# AAA ----

fit_ADAM_ETS_AAA <- auto.adam(ts_severe_weather_2022, model = "AAA", lags = c(1, 12), select = TRUE)
fit_ADAM_ETS_AAA




# AAN ----

fit_ADAM_ETS_AAN <- auto.adam(ts_severe_weather_2022, model = "AAN", lags = c(1, 12), select = TRUE)
fit_ADAM_ETS_AAN




# ANA ----

fit_ADAM_ETS_ANA <- auto.adam(ts_severe_weather_2022, model = "ANA", lags = c(1, 12), select = TRUE)
fit_ADAM_ETS_ANA




# NAA ----

fit_ADAM_ETS_NAA <- auto.adam(ts_severe_weather_2022, model = "NAA", lags = c(1, 12), select = TRUE)
fit_ADAM_ETS_NAA




# ANN ----

fit_ADAM_ETS_ANN <- auto.adam(ts_severe_weather_2022, model = "ANN", lags = c(1, 12), select = TRUE)
fit_ADAM_ETS_ANN





# NAN ----

fit_ADAM_ETS_NAN <- auto.adam(ts_severe_weather_2022, model = "NAN", lags = c(1, 12), select = TRUE)
fit_ADAM_ETS_NAN


 

# NNA ----

fit_ADAM_ETS_NNA <- auto.adam(ts_severe_weather_2022, model = "NNA", lags = c(1, 12), select = TRUE)
fit_ADAM_ETS_NNA




# NNN ----

fit_ADAM_ETS_NNN <- auto.adam(ts_severe_weather_2022, model = "NNN", lags = c(1, 12), select = TRUE)
fit_ADAM_ETS_NNN







