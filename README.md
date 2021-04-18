# Financial Econometrics R projects

Projects prepared for Financial Econometrics II classes at Warsaw School of Economics using GNU R

## Project I - analysis of the Dutch government bond's yield

### Introduction

In this report we're going to:
- analyse the Dutch bond's yield,
- model them using ARIMA and VAR,
- analyse the properties of the estimated models from the econometric point of view,
- calibrate models to obtain 1-period forecasts,
- measure the out-of-sample accuracy,
- compare future forecasts with those published by the EU Commission.

### Initial analysis

In this chapter we're going to analyse the available data, focusing on the Dutch bonds yield series integration and 
(partial) autocorrelation. 

#### Raw data plot

![ts plot](https://github.com/jcierocki/financial-econometrics-R/blob/main/project1/output/ts.png)

#### Autocorrelation

![ts plot](https://github.com/jcierocki/financial-econometrics-R/blob/main/project1/output/acf.png)

![ts plot](https://github.com/jcierocki/financial-econometrics-R/blob/main/project1/output/pacf.png)

The above plots are typical for AR(1) (or equivalently infinite MA) process but let's notice that the partial autocorrelation 
for lag 1 is in fact equal to zero which suggests non-stationarity and clearly makes modelling this series, without any 
transformation, pointless.

#### Tests for stationarity

We're going to pursue *Phillips-Perron* (null hypothesis = non-stationary) and *KPSS* (null hypothesis = stationary) tests. *Dickey-Fuler* test would be omitted as the PP test
is its extension.

The critical level of significance is usually assumed at 0.05, but in our analysis we're going to treat *p-values* 
between 0.01 and 0.1 as not fully conclusive (nfc).

On raw series we've obtained:
- *PP test p-value* = 0.08239 ~ null (non-stationary, nfc)
- *KPSS test p-value* = 0.01 ~ alternative (non-stationary)

On first differences series we've obtained:
- *PP test p-value* = 0.01 ~ alternative (stationary)
- *KPSS test p-value* = 0.1 ~ null (stationary)

The results suggest that the process is actually integrated at level 1, and the models for the first differences should be used. 

#### Autocorrelation of first differences

![ts plot](https://github.com/jcierocki/financial-econometrics-R/blob/main/project1/output/acf_diff.png)

![ts plot](https://github.com/jcierocki/financial-econometrics-R/blob/main/project1/output/acf_diff.png)

ACF for the first differences looks pretty random, with the majority of the lags not being significantly correlated. A partial 
autocorrelation is significant for levels 1 and 7, and because of that we will set the maximum AR lag at 7 while calibrating the model.

### ARIMA models

We're now going to calibrate and estimate ARIMA model using the iterative algorithm proposed by *Rob Hyndman (2008)*. We 
will disable the drift, set the order of differentiation (*d*) at 1, and the initial *p* and *q* values at 7 and 0, as we 
expect that the ARIMA(7,1,0) would best fits the data.

