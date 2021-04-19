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

![ts plot](https://github.com/jcierocki/financial-econometrics-R/blob/main/project1/output/pacf_diff.png)

ACF for the first differences looks pretty random, with the majority of the lags not being significantly correlated. A partial 
autocorrelation is significant for levels 1 and 7, and because of that we will set the maximum AR lag at 7 while calibrating the model.

### ARIMA models

We're now going to calibrate and estimate ARIMA model using the iterative algorithm proposed by *Rob Hyndman (2008)*. We 
will disable the drift, set the order of differentiation (*d*) at 1, and the initial *p* and *q* values at 7 and 0, as we 
expect that the ARIMA(7,1,0) would best fits the data. The dataset is enough large (3908 observations), so we will use AIC 
information criteria instead of AICc.

| p,d,q |         aic|
|:------|-----------:|
| 7,1,0 |   -13888.93|
| 7,1,1 |   -13886.92|
| 0,1,1 |   -13886.66|
| 1,1,0 |   -13886.51|
| 6,1,1 |   -13881.67|
| 0,1,0 |   -13881.29|

The results met our expectations when it comes to the best model - ARIMA(7,1,0). Unfortunately the differences between the 
ARIMA(0,1,0), equivalent to random walk, and the other models are very insignificant.

Obtained ARIMA(7,1,0) model has the following specification:

|coefficient | estimate| p-value|
|:-----------|--------:|-------:|
|ar1         |   0.0445|  0.0054|
|ar2         |  -0.0124|  0.4398|
|ar3         |  -0.0158|  0.3241|
|ar4         |   0.0147|  0.3594|
|ar5         |  -0.0216|  0.1778|
|ar6         |   0.0112|  0.4844|
|ar7         |  -0.0509|  0.0015|

and measures:

|measure|       value|
|:------|-----------:|
|LogLik |   6952.4671|
|AIC    | -13888.9342|
|BIC    | -13838.7700|
|RMSE   |      0.0408|

Its IRF looks like:

![ts plot](https://github.com/jcierocki/financial-econometrics-R/blob/main/project1/output/arima_irf.png)

showing that the impulse expires in approximately 3 weeks (15 business days) with the higher variance along first 8 days.

In the further analysis we're going to compare the following ARIMA models:
- 7,1,0
- 1,1,0
- 0,1,0 (RW)

We won't use ARIMA(0,1,1) due to its' property of impulse vanishing after 1 period (day) leaving constant differences same 
for all horizons of forecast which we assume to be not sufficient solution for long-term prediction.

### VAR models

