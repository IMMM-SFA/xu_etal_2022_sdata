library("dplyr")
library("readxl")

source("approx-cdf.R")

df.sqft <- readxl::read_excel("MECS/Table9_1.xlsx", skip=15,
                     col_names=c("NAICS Code", "Subsector and Industry",
                       "area million sqft",
                                 "num establishments",
                                 "mean area per establishment sqft",
                                 "num buildings",
                                 "mean num of buildings per establishment")) %>%
  dplyr::mutate_at(vars(`area million sqft`:`mean num of buildings per establishment`), as.numeric) %>%
  {.}

df.energy <- readxl::read_excel("MECS/Table1_2.xlsx", skip=13
                   , col_names=c("NAICS Code", "Subsector and Industry",
                                 "Total", "Net Electricity",
                                 "Residual Fuel Oil", "Distillate Fuel Oil",
                                 "Natural Gas",
                                 "HGL (excluding natural gasoline)", "Coal",
                                 "Coke and Breeze", "Other",
                                 "Shipments of Energy Sources Produced Onsite")) %>%
  dplyr::slice(c(1:82, 85)) %>%
  {.}

df.energy %>%
  dplyr::slice(nrow(.)) %>%
  tidyr::gather(fuel, tBtu, -(`NAICS Code`:`Subsector and Industry`)) %>%
  dplyr::slice(2:nrow(.)) %>%
  dplyr::mutate(tBtu = as.numeric(tBtu)) %>%
  ggplot2::ggplot(ggplot2::aes(x="", y=tBtu, fill=fuel)) +
  ggplot2::geom_bar(stat="identity", width=1, color="white") +
  ggplot2::coord_polar("y", start=0) +
  ggplot2::theme_void() +
  ggplot2::scale_fill_brewer(palette = "Set1") +
  ggplot2::ggtitle("Total consumption by fuel")
ggplot2::ggsave("images/consumption_pie.png", width=7, height=4)

## join energy and sqft
df.energy.wide <- df.energy %>%
  dplyr::filter(!is.na(`NAICS Code`)) %>%
  dplyr::select(`NAICS Code`, `Subsector and Industry`, `Net Electricity`, `Natural Gas`) %>%
  tidyr::gather(fuel, tBtu, -(`NAICS Code`:`Subsector and Industry`)) %>%
  dplyr::mutate(tBtu.num = as.numeric(tBtu)) %>%
  {.}

## compute fuel consumption per sqft
df.energy.intensity <- df.sqft %>%
  dplyr::select(`NAICS Code`, `area million sqft`, `num establishments`, `num buildings`) %>%
  dplyr::inner_join(df.energy.wide, by="NAICS Code") %>%
  dplyr::mutate(Btu.per.sqft = tBtu.num/`area million sqft` * 1e6) %>%
  dplyr::filter(nchar(`NAICS Code`) == 3) %>%
  {.}

df.energy.intensity %>%
  dplyr::select(fuel, `NAICS Code`, `Subsector and Industry`, tBtu, `area million sqft`, Btu.per.sqft, `num establishments`) %>%
  readr::write_csv("MECS/energy_intensity_per_type.csv")

df.elec <- df.energy.intensity %>%
  dplyr::filter(fuel == "Net Electricity") %>%
  {.}

df.gas <- df.energy.intensity %>%
  dplyr::filter(fuel == "Natural Gas") %>%
  {.}

library(data.table)

#' Calculates a weighted type-1 quantiles
#'
#' @param x instance values
#' @param w instance importance weights
#' @param probs quantile levels to calculate
#'
#' @return quantile values corresponding to levels \code{probs}
#'
#' @seealso quantile
#'
#' @importFrom data.table setDT set
weighted_quantile_type1 <- function(x, w, probs, na.rm=FALSE) {
  if (!na.rm && any(is.na(x))) {
    stop ('na.rm=FALSE but there are NAs in x')
  }
  if (any(is.na(w))) stop ('NA weights are not allowed')
  if (any(w<=0)) stop ('Negative and zero weighted are not allowed')
  ## (weights of could cause duplicate results)
  if (length(x) != length(w)) stop ('lengths of x and w should match') 
  should_include = !is.na(x)
  x <- x[should_include]
  w <- w[should_include]
  DT = setDT(data.frame(x=x, w=w), key="x")
  set(DT,, "p", cumsum(DT[["w"]]))
  DT[list(probs*sum(w)), x, on="p", roll=-Inf, rollends=c(TRUE,TRUE), nomatch=NA]
}

## from: https://aakinshin.net/posts/weighted-quantiles/
# Weighted generic quantile estimator
wquantile.generic <- function(x, probs, cdf.gen, weights = NA) {
  n <- length(x)
  if (any(is.na(weights)))
    weights <- rep(1 / n, n)
  nw <- sum(weights)^2 / sum(weights^2) # Kish's effective sample size

  indexes <- order(x)
  x <- x[indexes]
  weights <- weights[indexes]

  weights <- weights / sum(weights)
  cdf.probs <- cumsum(c(0, weights))
  
  sapply(probs, function(p) {
    cdf <- cdf.gen(nw, p)
    q <- cdf(cdf.probs)
    w <- tail(q, -1) - head(q, -1)
    sum(w * x)
  })
}

# Weighted Harrell-Davis quantile estimator
whdquantile <- function(x, probs, weights = NA) {
  cdf.gen <- function(n, p) return(function(cdf.probs) {
    pbeta(cdf.probs, (n + 1) * p, (n + 1) * (1 - p))
  })
  wquantile.generic(x, probs, cdf.gen, weights)
}

# Weighted Type 7 quantile estimator
wquantile <- function(x, probs, weights = NA) {
  cdf.gen <- function(n, p) return(function(cdf.probs) {
    h <- p * (n - 1) + 1
    u <- pmax((h - 1) / n, pmin(h / n, cdf.probs))
    u * n - h + 1
  })
  wquantile.generic(x, probs, cdf.gen, weights)
}

## discussion about the problem
## https://github.com/harrelfe/Hmisc/issues/97
library("Hmisc")

samples = c(1, 2, 3)
wt = c(0.1, 0.1, 0.1)

## normwt resolved it
Hmisc::wtd.quantile(x=samples, probs=0.5, weights=wt, normwt = TRUE)
weighted_quantile_type1(x=samples, w=wt, probs=0.5)

samples = c(1, 2)
wt = c(1, 9)
## Hmisc has the wrong result
Hmisc::wtd.quantile(x=samples, probs=0.0, weights=wt, normwt = TRUE)

weighted_quantile_type1(x = c(1, rep(9, 4)), w = rep(1, 5), probs = c(0.25, 0.5, 0.75))

## zero weights will fail
weighted_quantile_type1(x = c(1, rep(9, 4)), w = c(0.2,rep(0.2,4)), probs = c(0.2, 0.25, 0.5, 0.75))

quantile(x = c(1, 9, 9, 9, 9), probs = c(0.25, 0.5, 0.75), type=1L)

quantile(x = c(1, 2), probs = 0.5, type=1L)
quantile(x = c(1, 2), probs = 0.5, type=7L)
wquantile(x = c(1, 2), probs = 0.5)
whdquantile(x = c(1, 2), probs = 0.5)

df.elec %>%
  summary()

df.gas %>%
  summary()

approx_cdf_type7_maybe = function(x, w) {
  assert_that(is.numeric(x) && length(x) >= 1L)
  assert_that(is.numeric(w) && all(w >= 1)) # for type 7, want weights >= 1
  assert_that(length(x) == length(w))
  ##
  tibble::tibble(q=x, w=w) %>%
    dplyr::group_by(q) %>%
    dplyr::summarize(w=sum(w), .groups="drop") %>%
    dplyr::mutate(cw=cumsum(w)) %>%
    ## ## this isn't right.  there should be jumps for weights > 1 even internally
    ## dplyr::mutate(tau=(cumsum(w)-1)/(sum(w)-1)) %>%
    ## {approx_cdf_from_quantiles(.[["q"]], .[["tau"]])}
    dplyr::mutate(ws.at = cw) %>%
    dplyr::mutate(ws.before = c(0, cw[-length(cw)]+1)) %>%
    {approx_cdf(.$q, .$ws.before/.$ws.at[[length(.$ws.at)]], .$ws.at/.$ws.at[[length(.$ws.at)]])}
}

weighted_quantile_type7_maybe = function(x, w, probs) {
  assert_that(is.numeric(probs) && all(0 <= probs & probs <= 1))
  ##
  approx.cdf = approx_cdf_type7_maybe(x, w)
  quantile(approx.cdf, probs)
}

weighted_quantile_via_rep = function(x, w, probs, type=7L) {
  assert_that(is.numeric(x))
  assert_that(is_integerish(w) && all(w >= 0))
  assert_that(length(x) == length(w))
  assert_that(is.numeric(probs) && all(0 <= probs & probs <= 1))
  assert_that(is_scalar_integerish(type))
  ##
  quantile(rep(x, times=w), probs, type=type)
}

gas.75.comparison =
  tibble::tribble(~ method, ~weighted.quantile,
                  "type 1",
                  weighted_quantile_type1(
                    x = df.gas$Btu.per.sqft,
                    w = df.gas$`num establishments`,
                    probs=0.75, na.rm = TRUE),
                  "type 1 via rep",
                  weighted_quantile_via_rep(x = df.gas$Btu.per.sqft, w = df.gas$`num establishments`, probs = 0.75, type=1L),
                  "wquantile (type 7)",
                  wquantile(x = df.gas$Btu.per.sqft, probs = 0.75, weights = df.gas$`num establishments`),
                  "type 7 maybe",
                  weighted_quantile_type7_maybe(x = df.gas$Btu.per.sqft, w = df.gas$`num establishments`, probs = 0.75),
                  "type 7 via rep",
                  weighted_quantile_via_rep(x = df.gas$Btu.per.sqft, w = df.gas$`num establishments`, probs = 0.75, type=7L),
                  "whdquantile",
                  whdquantile(x = df.gas$Btu.per.sqft, probs = 0.75, weights = df.gas$`num establishments`),
                  )
print(gas.75.comparison)
plot(approx_cdf_type7_maybe(df.gas$Btu.per.sqft, df.gas$`num establishments`)) +
  geom_vline(aes(xintercept=weighted.quantile, colour=method, linetype=method),
             gas.75.comparison)

elec.75.comparison =
  tibble::tribble(~ method, ~weighted.quantile,
                  "type 1",
                  weighted_quantile_type1(
                    x = df.elec$Btu.per.sqft,
                    w = df.elec$`num establishments`,
                    probs=0.75, na.rm = TRUE),
                  "type 1 via rep",
                  weighted_quantile_via_rep(x = df.elec$Btu.per.sqft, w = df.elec$`num establishments`, probs = 0.75, type=1L),
                  "wquantile (type 7)",
                  wquantile(x = df.elec$Btu.per.sqft, probs = 0.75, weights = df.elec$`num establishments`),
                  "type 7 maybe",
                  weighted_quantile_type7_maybe(x = df.elec$Btu.per.sqft, w = df.elec$`num establishments`, probs = 0.75),
                  "type 7 via rep",
                  weighted_quantile_via_rep(x = df.elec$Btu.per.sqft, w = df.elec$`num establishments`, probs = 0.75, type=7L),
                  "whdquantile",
                  whdquantile(x = df.elec$Btu.per.sqft, probs = 0.75, weights = df.elec$`num establishments`),
                  )
print(elec.75.comparison)
plot(approx_cdf_type7_maybe(df.elec$Btu.per.sqft, df.elec$`num establishments`)) +
  geom_vline(aes(xintercept=weighted.quantile, colour=method, linetype=method),
             elec.75.comparison)

weighted_quantile_type1(x = df.elec$Btu.per.sqft,
                        w = df.elec$`num establishments`,
                        probs=c(0.25, 0.75), na.rm = TRUE)
## [1]  81045.75 264887.06       kBtu/sqft

weighted_quantile_via_rep(x = df.elec$Btu.per.sqft, w = df.elec$`num establishments`, probs = c(0.25, 0.75), type=7L)

weighted_quantile_via_rep(x = df.gas$Btu.per.sqft, w = df.gas$`num establishments`, probs = c(0.25, 0.75), type=7L)

## type 7 quantile
wquantile(x = df.elec$Btu.per.sqft, probs = c(0.25, 0.75), weights = df.elec$`num establishments`)
## [1]  81045.75 216794.15

## Harrell-Davis quantile estimator
whdquantile(x = df.elec$Btu.per.sqft, probs = c(0.25, 0.75), weights = df.elec$`num establishments`)
## [1]  82673.53 241553.82

weighted_quantile_type1(x = df.gas$Btu.per.sqft,
                        w = df.gas$`num establishments`,
                        probs=c(0.25, 0.75), na.rm = TRUE)
## [1]  76452.6 194945.8         kBtu/sqft

## type 7 quantile
wquantile(x = df.gas$Btu.per.sqft, probs = c(0.25, 0.75), weights = df.gas$`num establishments`)
## [1]  76983.87 264856.85

## Harrell-Davis quantile estimator
whdquantile(x = df.gas$Btu.per.sqft, probs = c(0.25, 0.75), weights = df.gas$`num establishments`)
## [1]  73370.62 498939.92
