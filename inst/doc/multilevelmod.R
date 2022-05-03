## ---- include = FALSE---------------------------------------------------------
library(tidymodels)
library(multilevelmod)

## ---- results = 'hide'--------------------------------------------------------
library(tidymodels)
library(multilevelmod)

tidymodels_prefer()
theme_set(theme_bw())

## -----------------------------------------------------------------------------
data(sleepstudy, package = "lme4")

sleepstudy %>% 
  ggplot(aes(x = Days, y = Reaction)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~ Subject) 

## -----------------------------------------------------------------------------
new_subject <- tibble(
  Days = 0:9, 
  Subject = "one"
)

## ----gee----------------------------------------------------------------------
gee_spec <- 
  linear_reg() %>% 
  set_engine("gee", corstr = "exchangeable")

gee_fit <- 
  gee_spec %>% 
  fit(Reaction ~ Days + id_var(Subject), data = sleepstudy)

gee_fit

## -----------------------------------------------------------------------------
predict(gee_fit, new_subject %>% select(Days)) %>% 
  bind_cols(new_subject)

## ----gls----------------------------------------------------------------------
library(nlme) # <- Only need to load this to get cor*() functions

gls_spec <- 
  linear_reg() %>% 
  set_engine("gls", correlation = corCompSymm(form = ~ 1 | Subject))

gls_fit <- 
  gls_spec %>% 
  fit(Reaction ~ Days, data = sleepstudy)

gls_fit

## ----gls-pred-----------------------------------------------------------------
predict(gls_fit, new_subject %>% select(Days)) %>% 
  bind_cols(new_subject)

## ----lme----------------------------------------------------------------------
lme_spec <- 
  linear_reg() %>% 
  set_engine("lme", random = ~ 1 | Subject)

lme_fit <- 
  lme_spec %>% 
  fit(Reaction ~ Days, data = sleepstudy)

lme_fit

## ----lme-pred-----------------------------------------------------------------
predict(lme_fit, new_subject) %>% 
  bind_cols(new_subject)

# For this design, this is the same prediction as a training set point:
predict(lme_fit, sleepstudy %>% filter(Subject == "308"))

## ----lmer---------------------------------------------------------------------
lmer_spec <- 
  linear_reg() %>% 
  set_engine("lmer")

lmer_fit <- 
  lmer_spec %>% 
  fit(Reaction ~ Days + (1|Subject), data = sleepstudy)

lmer_fit

## ----lmer-pred----------------------------------------------------------------
predict(lmer_fit, new_subject) %>% 
  bind_cols(new_subject)

## -----------------------------------------------------------------------------
required_pkgs(lmer_spec)

## ----wflow--------------------------------------------------------------------
lmer_wflow <- 
  workflow() %>% 
  add_variables(outcomes = Reaction, predictors = c(Days, Subject)) %>% 
  add_model(lmer_spec, formula = Reaction ~ Days + (1|Subject))

lmer_wflow %>% fit(data = sleepstudy)

## ----rec----------------------------------------------------------------------
rec <- 
  recipe(Reaction ~ Days + Subject, data = sleepstudy) %>%
  add_role(Subject, new_role = "exp_unit") %>%
  step_zv(all_predictors(), -has_role("exp_unit"))

lmer_wflow %>%
  remove_variables() %>%
  add_recipe(rec) %>%
  fit(data = sleepstudy)

## -----------------------------------------------------------------------------
lmer_wflow %>% 
  fit(data = sleepstudy) %>% # <- returns a workflow
  extract_fit_engine()       # <- returns the lmer object

