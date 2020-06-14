## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----semnova------------------------------------------------------------------
library(semnova)

set.seed(323412431)

data <- get_test_data()

idata <- expand.grid(A = c("A1", "A2", "A3"), B = c("B1", "B2"))

mmodel <- create_mmodel(
    A1B1 = "var1",
    A2B1 = "var2",
    A3B1 = "var3",
    A1B2 = "var4",
    A2B2 = "var5",
    A3B2 = "var6",
    lv_scaling = "referent"
)

fit_semnova <-
    semnova(
        formula = cbind(A1B1, A2B1, A3B1, A1B2, A2B2, A3B2) ~ 1,
        data = data,
        idata = idata,
        idesign = ~ A * B,
        mmodel = mmodel
    )

summary(fit_semnova)

## ----lgc----------------------------------------------------------------------
library(semnova)

set.seed(323412431)

data <- get_test_data()

mmodel <- create_mmodel(
    A1B1 = "var1",
    A2B1 = "var2",
    A3B1 = "var3",
    A1B2 = "var4",
    A2B2 = "var5",
    A3B2 = "var6",
    lv_scaling = "referent"
)

hypotheses <- list(
    Intercept = c(1),
    A        = c(2, 3),
    B        = c(4),
    AB       = c(5, 6)
)

C_matrix <- matrix(
    c(
        1, 1, 0, 1, 1, 0,
        1, 0, 1, 1, 0, 1,
        1,-1,-1, 1,-1,-1,
        1, 1, 0,-1,-1, 0,
        1, 0, 1,-1, 0,-1,
        1,-1,-1,-1, 1, 1
    ),
    nrow = 6
)

fit_lgc <- lgc(data, mmodel, C_matrix, hypotheses)

summary(fit_lgc)

