# 1. Dashboard madre

```r
rsconnect::deployApp(
  appDir = "SIM",
  appPrimaryDoc = "Sistemainformativo.Rmd",
  appFiles = c(
    "Sistemainformativo.Rmd",
    "data/master_pa.rds",
    "data/nuts_cache.rds"
  ),
  forceUpdate = TRUE
)
```

# 2. Dashboard ANAC

```r
rsconnect::deployApp(
  appDir = "ANAC",
  appPrimaryDoc = "ANAC.Rmd",
  appFiles = c(
    "ANAC.Rmd",
    "data/dashboard_data.rds",
    "data/INDICATORS_CPV_ANAC.json",
    "data/regioni_shape.rds"
  ),
  forceUpdate = TRUE
)
```


# 3. Dashboard PagoPA


```r
rsconnect::deployApp(
  appDir = "PagoPA",
  appPrimaryDoc = "PagoPA.Rmd",
  appFiles = c(
    "PagoPA.Rmd",
    "data/INDICATORS_PAGOPA.json",
    "data/fil_reg.rds",
    "data/regioni_shape.rds"
  ),
  forceUpdate = TRUE
)
```