CHXRSA = read.csv("~/Desktop/stat429/project data/CHXRSA.csv")
MORTGAGE15US = read.csv("~/Desktop/stat429/project data/MORTGAGE15US.csv")
NFCICREDIT = read.csv("~/Desktop/stat429/project data/NFCICREDIT.csv")
CHIC917BPPRIVSA = read.csv("~/Desktop/stat429/project data/CHIC917BPPRIVSA.csv")
CUURA207SEHA = read.csv("~/Desktop/stat429/project data/CUURA207SEHA.csv")
APUS23A74714 = read.csv("~/Desktop/stat429/project data/APUS23A74714.csv")
CHIC917URN = read.csv("~/Desktop/stat429/project data/CHIC917URN.csv")
APU0000708111 = read.csv("~/Desktop/stat429/project data/APU0000708111.csv")
CHXRNSA = read.csv("~/Desktop/stat429/project data/CHXRNSA.csv")
isolatedCHXRNSA = CHXRNSA$CHXRNSA

library(dplyr)
complete = left_join(CHXRSA, MORTGAGE15US, by = "observation_date") %>% 
  left_join(., NFCICREDIT, by = "observation_date") %>% 
  left_join(., CHIC917BPPRIVSA, by = "observation_date") %>% 
  left_join(., CUURA207SEHA, by = "observation_date") %>% 
  left_join(., APUS23A74714, by = "observation_date") %>% 
  left_join(., CHIC917URN, by = "observation_date") %>% 
  left_join(., APU0000708111, by = "observation_date") %>% 
  left_join(., CHXRNSA, by = "observation_date")

ts.plot(complete)

dictionary = list(CHXRSA = "Chicago Home Price Index",
                  MORTGAGE15US = "15-Year Fixed Rate Mortgage",
                  CHIC917BPPRIVSA = "New Private Housing Structures Authorized",
                  NFCICREDIT = "National Financial Conditions Credit Subindex",
                  CUURA207SEHA = "Consumer Price Index - rent in Chicago",
                  APUS23A74714 = "Average Gas Price Chicago",
                  CHIC917URN = "Unemployment Rate in Chicago",
                  APU0000708111 = "Average Price of Eggs",
                  CHXRNSA = "Chicago Home Price Index - NOT ADJUSTED")
complete$t = seq(from = as.Date("1991-09-01"), by = "month", 
                 length.out = nrow(complete))

complete = na.omit(complete)
#***** OCTOBER 2013 IS DROPPED FROM THIS COMPLETE SERIES - NA WAS OBSERVED

basic = lm(CHXRNSA ~ MORTGAGE15US + CHIC917BPPRIVSA + NFCICREDIT + CUURA207SEHA + 
             APUS23A74714 + CHIC917URN + APU0000708111 + t, data = complete)

summary(basic)
#after running BASIC regression, we can see that the significant indicators are 
#the intercept, CHIC917BPPRIVSA, NFCICREDIT, CUURA207SEHA, APUS23A74714,
#and time t. 

write.csv(complete, file = "~/Desktop/stat429/project data/projectdata.csv")

dictionary

acf(resid(basic), lag.max = 403)
pacf(resid(basic), lag.max = 403)

#all but one (NFCICREDIT) of the variables are NOT stationary. so, let's fit
#a new model with all of the non-stationary data differenced. after running
#adf tests and observing the plots, we can see that the data is relatively
#stationary, however quite a bit of the datasets have increasing variances over
#time. 
#after differencing NFCICREDIT, however, we obtain a plot that has mean 
#centered around 0 and a more constant variance. the acf of diff(NFCICREDIT)
#also looks far more normal than that of NFCICREDIT regular, so we will 
#difference it. 

adf.test(diff(complete$CHXRSA))
adf.test(diff(complete$MORTGAGE15US))
adf.test(diff(complete$CHIC917BPPRIVSA))
adf.test(complete$NFCICREDIT)
adf.test(diff(complete$CUURA207SEHA))
adf.test(diff(complete$APUS23A74714))
adf.test(diff(complete$CHIC917URN))
adf.test(diff(complete$APU0000708111))
adf.test(complete$CHXRNSA)

diff_data = data.frame(dCHXRSA = diff(complete$CHXRSA),
                       dMORTGAGE15US = diff(complete$MORTGAGE15US),
                       dCHIC917BPPRIVSA = diff(complete$CHIC917BPPRIVSA),
                       dNFCICREDIT = diff(complete$NFCICREDIT),
                       dCUURA207SEHA = diff(complete$CUURA207SEHA),
                       dAPUS23A74714 = diff(complete$APUS23A74714),
                       dCHIC917URN = diff(complete$CHIC917URN),
                       dAPU0000708111 = diff(complete$APU0000708111),
                       dCHXRNSA = diff(complete$CHXRNSA),
                       dt = 2:nrow(complete))
ts.plot(diff_data)

diffmod = lm(dCHXRNSA ~ dMORTGAGE15US + dCHIC917BPPRIVSA + dNFCICREDIT + 
               dCUURA207SEHA + dAPUS23A74714 + 
               dCHIC917URN + dAPU0000708111 + dt, data = diff_data)
summary(diffmod)

acf(resid(diffmod), lag.max = 403)
pacf(resid(diffmod), lag.max = 403)

#after differencing the data, we see far less statistically significant
#indicators. HOWEVER, we also see that we see significant ACF values
#outside of the acceptable regions, all the way up to ~lag 150. so, we should
#make a new model that accounts for autocorrelation.

library(nlme)
glsmod = gls(dCHXRNSA ~ dMORTGAGE15US + dCHIC917BPPRIVSA + dNFCICREDIT + 
               dCUURA207SEHA + dAPUS23A74714 + 
               dCHIC917URN + dAPU0000708111 + dt, 
             data = diff_data)
summary(glsmod)
Box.test(residuals(glsmod), lag = 20, type = "Ljung-Box")

auto.arima(diff_data$dCHXRSA, xreg = xreg_matrix)

glscorr = gls(dCHXRNSA ~ dMORTGAGE15US + dCHIC917BPPRIVSA + dNFCICREDIT + 
               dCUURA207SEHA + dAPUS23A74714 + 
               dCHIC917URN + dAPU0000708111 + dt, 
             data = diff_data, correlation = corARMA(form = ~dt, p = 4))
summary(glscorr)

#EVEN AFTER running a gls model, we are still getting severe autocorrelation
#this is likely due to the huge dip in housing prices during the 2008 recession
#if we add an autocorrelated errors (AR(4) per auto.arima()), we get a model
#that slightly fits CHXRNSA better. we will keep glscorr as one model we look at
#lets fit a model with only the significant predictors, and time t

reducedgls = gls(dCHXRNSA ~ dMORTGAGE15US + dAPUS23A74714 + dAPU0000708111 + dt,
                 data = diff_data, correlation = corARMA(form = ~dt, p = 4))
summary(reducedgls)

#after doing all of this, I figured that a seasonal trend in the ACF -
#the sign changes every 4 lags. 
#we can say this is an SARIMA(4, 1, 2)(0, 1, 0)[12] model.

#------------CHXRSA AFTER box-cox transformation. 
BC_CHXRSA = BoxCox(CHXRSA$CHXRSA, lambda = BoxCox.lambda(CHXRSA$CHXRSA))
dBC_CHXRSA = diff(BC_CHXRSA)
dBC_CHXRSA_aligned = dBC_CHXRSA[-1]
diff_data$dBC_CHXRSA = dBC_CHXRSA_aligned

gls_bc = gls(dBC_CHXRSA ~  dMORTGAGE15US + dCHIC917BPPRIVSA + dNFCICREDIT + 
               dCUURA207SEHA + dAPUS23A74714 + 
               dCHIC917URN + dAPU0000708111 + dt, data = diff_data,
             correlation = corARMA(form = ~dt, p = 4, q = 1))
summary(gls_bc)



#------------SARIMA FITTING for CHXRNSA. 

acf(diff(diff_data$dCHXRNSA,lag = 12))

library(astsa)
sarima(isolatedCHXRNSA, 4, 1, 2, P = 0, D = 1, Q = 0, S = 12)

isolatedCHXRNSA = CHXRNSA$CHXRNSA

fitsarima = auto.arima(isolatedCHXRNSA, seasonal = TRUE)
summary(fitsarima)

sarima(isolatedCHXRNSA, 1, 1, 2, P = 0, D = 1, Q = 0, S = 12)

#after using auto.arima(), i found that a SARIMA(1, 1, 2)(0, 1, 0)[12] is
#better. 

#------------FORECASTING
#due to the strange nature of the data, i'm choosing to run 3 models - 
#a) gls_bc: this is a full model that takes differenced independent variables,
#and a differenced BoxCox transformed CHXRSA. I chose to do both BoxCox and
#differenced transformation as I thought this would help mitigate the impact
#that the 2008 dip had. after running this model, i got the most normal looking
#ACF plot of the 3 models, so i'm choosing to include this.

last_dt = max(diff_data$dt)
last_row = tail(diff_data, 1)

newdata_bc = data.frame(
  dMORTGAGE15US    = rep(last_row$dMORTGAGE15US, 5),
  dCHIC917BPPRIVSA = rep(last_row$dCHIC917BPPRIVSA, 5),
  dNFCICREDIT      = rep(last_row$dNFCICREDIT, 5),
  dCUURA207SEHA    = rep(last_row$dCUURA207SEHA, 5),
  dAPUS23A74714    = rep(last_row$dAPUS23A74714, 5),
  dCHIC917URN      = rep(last_row$dCHIC917URN, 5),
  dAPU0000708111   = rep(last_row$dAPU0000708111, 5),
  dt               = (last_dt + 1):(last_dt + 5)
)

pred_diff = predict(gls_bc, newdata = newdata_bc)
last_BC = tail(BC_CHXRSA, 1)
forecast_BC = last_BC + cumsum(pred_diff)

forecasted_CHXRSA = InvBoxCox(forecast_BC, BoxCox.lambda(CHXRSA$CHXRSA))
forecasted_CHXRSA[403:407] = forecasted_CHXRSA

#b) reducedgls - this is a standard gls model, taking in independent regressors
#which were deemed significant at an 0.05 level from a larger gls() call that
#regresses with autocorrelated errors following AR(4) per a auto.arima() call.

reduced_nd = data.frame(
  dMORTGAGE15US    = rep(last_row$dMORTGAGE15US, 5),
  dAPUS23A74714    = rep(last_row$dAPUS23A74714, 5),
  dAPU0000708111   = rep(last_row$dAPU0000708111, 5),
  dt               = (last_dt + 1):(last_dt + 5)
)

pred_reduced = predict(reducedgls, newdata = reduced_nd)
last_CHXRNSA = tail(CHXRNSA$CHXRNSA, 1)
CHXRNSA_fore = last_CHXRNSA + cumsum(pred_reduced)
CHXRNSA_fore[459:463] = CHXRNSA_fore
CHXRNSA_fore[1:458] = NA

#c) SARIMA(4, 1, 2)(0, 1, 0)[12]. while technically out of the scope of my 
#project, the data is extremely strange so i thought fitting a SARIMA model to
#the non seasonally differenced CHXRNSA would maybe better predict future values
#than the gls() for the CHXRNSA and diff(BoxCox(CHXRSA))

tsCHXRNSA = ts(isolatedCHXRNSA, frequency = 12)
sarimafor = sarima.for(tsCHXRNSA, n.ahead = 5, 4, 1, 2, 
                       P = 0, D = 1, Q = 0, S = 12)
sarimafor

forecasted_CHXRSA[1:402] = NA
plot(CHXRSA$CHXRSA, xlim = c(0, 407))
points(forecasted_CHXRSA, xlim = c(403, 404, 405, 406, 407), col = "red")
plot(CHXRSA$CHXRSA, xlim = c(200, 407))
points(forecasted_CHXRSA, xlim = c(403, 404, 405, 406, 407), col = "red")

plot(isolatedCHXRNSA, xlim = c(0, 463))
points(CHXRNSA_fore, xlim = c(459, 460, 461, 462, 463), col = "red")

plot(isolatedCHXRNSA, xlim = c(200, 463))
points(CHXRNSA_fore, xlim = c(459, 460, 461, 462, 463), col = "red")

