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

#### Raw data plot

![ts plot](https://github.com/jcierocki/financial-econometrics-R/blob/main/project1/output/ts.png)

#### Autocorrelation plots

![ts plot](https://github.com/jcierocki/financial-econometrics-R/blob/main/project1/output/acf.png)

![ts plot](https://github.com/jcierocki/financial-econometrics-R/blob/main/project1/output/pacf.png)

#### Tests for stationarity

We're going to pursue *Phillips-Perron* and *KPSS* tests. *Dickey-Fuler* test would be omitted as the PP test
is its extension.

On raw series we've obtained:
- *p-value* = 