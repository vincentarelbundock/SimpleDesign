

# `SimpleDesign`

A simple package to simulate and diagnose research designs:

-   Simulate
-   Fit
-   Diagnose
    -   Bias
    -   RMSE
    -   Power
    -   Coverage

This is strongly influenced by the amazing `DeclareDesign` package.

Warning: This was just a quick Saturday morning project to see if I
could roll my own simple solution. Nobody should use this for serious
work.

## Simple data generating process

First, we load the package and define a `dgp()` function that encodes a
simple data generating process (DGP): A two-arm randomized controlled
trial with *N* = 100 observations, an outcome *Y*, a random treatment
*T*, and a treatment effect of *θ* = 0.5.

The `dgp()` function must return a data frame when called. That data
frame must also have a “truth” attribute, which holds the true value of
the estimand that we are targetting.

``` r
library(SimpleDesign)

dgp = function(theta = 0.5, N = 50) {
  T = rbinom(N, 1, .5) # treatment
  e = rnorm(N) # noise
  Y = theta * T + e # outcome

  # output data frame
  data = data.frame(Y, T)

  # set the "truth" attribute
  attr(data, "truth") = theta
  return(data)
}

dgp() |> head()
```

                Y T
    1 -0.37302813 0
    2 -1.14314590 1
    3  0.63552187 0
    4 -1.15922120 1
    5 -0.34179292 1
    6 -0.04156007 0

Next, we define a `fit()` function. This function accepts a data frame,
fits a model, and returns a data frame of estimates. That data frame
must absolutely include an `estimate` column with numeric values.

``` r
fit = function(data) {
  model = lm(Y ~ T, data = data)
  results = data.frame(
    estimator = "OLS",
    estimate = coef(model)["T"]
  )
  return(results)
}

dgp() |> fit()
```

      estimator  estimate
    T       OLS 0.5859094

Finally, we feed both the `dgp()` and the `fit()` functions to the
`diagnose()` function. This function will simulate data from the `dgp()`
function, fit models using the `fit()` function, and return a data frame
with diagnostic statistics. The `truth` attribute of the generated data
specifies the true value of the quantity of interest, against which we
benchmark estimates.

``` r
diagnose(dgp, fit)
```

      estimator truth       bias      rmse
    1       OLS   0.5 0.01775981 0.2569797

The `fit()` function can also produce data frames with additional
columns like `conf.low`, `conf.high`, and `p.values`. When those columns
are present, `diagnose()` will generate more useful diagnostic
statistics.

Extracting that information from models is relatively easy, but it is
inconvenient. To make this process easier, `SimpleDesign` supplies the
`extract_estimates()` function. The mandatory `label` argument adds a
new column with a unique label. The optional `term` argument specifies
the subset of parameters to extract from the model.

``` r
fit = function(data) {
  model = lm(Y ~ T, data = data)
  results = extract_estimates(OLS = model, term = "T")
  return(results)
}

dgp() |> fit()
```

      estimator term  estimate   p.value   conf.low conf.high
    1       OLS    T 0.5012431 0.1422529 -0.1682288  1.170715

Since the `fit()` output now includes the *p* value and confidence
interval, `diagnose()` now reports more useful statistics.

``` r
diagnose(dgp, fit)
```

      estimator term truth         bias      rmse power coverage
    1       OLS    T   0.5 -0.001086504 0.2776061  0.37     0.92

## DGP parameters

The `dgp()` function has two arguments to control the sample and effect
sizes. We can diagnose several research designs in one go by supplying a
data frame of DGP parameters to the `dgp_parameters` argument of
`diagnose()`. In this example, we use the `expand.grid()` function from
base `R` to build a data frame with all combinations of parameter
values.

``` r
param = expand.grid(N = c(100, 500), theta = c(0.1, 0.5, 1))
param
```

        N theta
    1 100   0.1
    2 500   0.1
    3 100   0.5
    4 500   0.5
    5 100   1.0
    6 500   1.0

``` r
diagnose(dgp, fit, N = 100, dgp_parameters = param)
```

      estimator term truth   N theta         bias       rmse power coverage
    1       OLS    T   0.1 100   0.1 -0.000039723 0.17967811  0.07     0.99
    2       OLS    T   0.1 500   0.1  0.007538356 0.09246109  0.23     0.92
    3       OLS    T   0.5 100   0.5  0.052107175 0.20274188  0.84     0.94
    4       OLS    T   0.5 500   0.5  0.005333471 0.08670184  1.00     0.96
    5       OLS    T   1.0 100   1.0  0.010563482 0.21025858  1.00     0.95
    6       OLS    T   1.0 500   1.0  0.003589011 0.08514632  1.00     0.96

## Complex data generating process

Since `dgp()` is just a standard `R` function, users are free to define
complex data generating processes using whatever helper functions they
wish. For example, the `fabricatr` and `randomizr` packages offer
extremely powerful functions to generate simulated data with special
random assignment schemes. For example, this `dgp()` generates data from
a block random assignment design.

``` r
dgp = function(n_blocks = 3, n_indiv = 100, e_sd = 1) {
  data = fabricatr::fabricate(
    # block-level variables
    block = fabricatr::add_level(
      N = n_blocks,

      # individual treatment effect
      tau = c(4, 2, 0)
    ),

    # individual-level variables
    indiv = fabricatr::add_level(
      N = n_indiv,

      # noise
      e = rnorm(N, sd = e_sd),

      # potential outcomes
      Y_T_0 = e,
      Y_T_1 = e + tau
    )
  )
  data$T = randomizr::block_ra(blocks = data$block, block_prob = c(.5, .7, .9))
  data$Y = ifelse(data$T == 1, data$Y_T_1, data$Y_T_0)

  # define truth in terms of potential outcomes
  attr(data, "truth") = mean(data$Y_T_1 - data$Y_T_0)

  return(data)
}

dgp() |> head()
```

      block tau indiv          e      Y_T_0    Y_T_1 T          Y
    1     1   4   001  1.0472749  1.0472749 5.047275 1  5.0472749
    2     1   4   002 -0.4902355 -0.4902355 3.509765 0 -0.4902355
    3     1   4   003  2.1545182  2.1545182 6.154518 0  2.1545182
    4     1   4   004  0.9860548  0.9860548 4.986055 0  0.9860548
    5     1   4   005  1.4483193  1.4483193 5.448319 0  1.4483193
    6     1   4   006  0.7895540  0.7895540 4.789554 1  4.7895540

``` r
dgp() |> attr("truth")
```

    [1] 2

Now, we define a fit function to see if a “naive” linear regression
model retrieves a good estimate of the estimand.

``` r
fit = function(data) {
  model = lm(Y ~ T, data = data)
  results = extract_estimates(OLS = model, term = "T")
  return(results)
}
```

Finally, we diagnose the research design.

``` r
diagnose(dgp, fit)
```

      estimator term truth      bias      rmse power coverage
    1       OLS    T     2 -0.386235 0.4063862     1     0.62

## Comparing estimators

Perhaps we should switch estimators. As described on the [DeclareDesign
blog](https://declaredesign.org/blog/posts/biased-fixed-effects.html),
we could try to control for blocks fixed effects in the linear model, or
estimate differences-in-means in each group and then average them. That
last option is implemented by the `differences_in_means()` function from
the `estimatr` package.

Again, we use the `extract_estimates()` helper function, which accepts a
named list of models, and returns a simple data frame with appropriate
labels. To illustrate, let’s simulate a single dataset, store three
fitted models in a named list, and call `extract_estimates()`. For fun,
we also set the *α* level used to build confidence intervals to 0.01.

``` r
data = dgp()

extract_estimates(
  `Naive LM` = lm(Y ~ T, data = data),
  `Block controls` = lm(Y ~ T + block, data = data),
  `DinM` = estimatr::difference_in_means(Y ~ T, blocks = block, data = data),
  term = "T",
  alpha = 0.01
)
```

           estimator term estimate      p.value conf.low conf.high
    1       Naive LM    T 1.688476 2.062754e-16 1.159268  2.217683
    2 Block controls    T 2.628537 1.708860e-63 2.226028  3.031047
    3           DinM    T 2.071378 9.103336e-57 1.735331  2.407425

Using this helper function, we can define a new `fit()` and compare
different modelling strategies:

``` r
fit = function(data) {
  model = lm(Y ~ T, data = data)
  results = extract_estimates(
    `Naive LM` = lm(Y ~ T, data = data),
    `Block controls` = lm(Y ~ T + block, data = data),
    `DinM` = estimatr::difference_in_means(Y ~ T, blocks = block, data = data),
    term = "T",
    alpha = 0.01
  )
  return(results)
}
diagnose(dgp, fit)
```

           estimator term truth         bias      rmse power coverage
    1 Block controls    T     2  0.579808242 0.5948968     1     0.10
    2           DinM    T     2 -0.006437012 0.1472254     1     0.99
    3       Naive LM    T     2 -0.383467860 0.4036736     1     0.88

These results show that the difference-in-means strategy yields unbiased
results with adequate coverage. We could extend our investigation to
consider different DGP parameters:

``` r
param = expand.grid(n_indiv = c(100, 500), e_sd = c(1, 2))

diagnose(dgp, fit, dgp_parameters = param)
```

            estimator term truth n_indiv e_sd          bias       rmse power coverage
    1  Block controls    T     2     100    1  0.5900080579 0.60494390     1     0.07
    2  Block controls    T     2     100    2  0.5514600278 0.60759485     1     0.78
    3  Block controls    T     2     500    1  0.5740810393 0.57671783     1     0.00
    4  Block controls    T     2     500    2  0.5848918067 0.59858053     1     0.00
    5            DinM    T     2     100    1  0.0009265808 0.13423367     1     1.00
    6            DinM    T     2     100    2 -0.0136474020 0.31326195     1     0.98
    7            DinM    T     2     500    1 -0.0140148171 0.06272271     1     1.00
    8            DinM    T     2     500    2 -0.0028456426 0.13729211     1     0.99
    9        Naive LM    T     2     100    1 -0.3788130041 0.39582041     1     0.94
    10       Naive LM    T     2     100    2 -0.4028174998 0.46559315     1     0.94
    11       Naive LM    T     2     500    1 -0.3872243800 0.39076056     1     0.01
    12       Naive LM    T     2     500    2 -0.3759156865 0.39481790     1     0.39
