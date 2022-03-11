## ---- include = FALSE---------------------------------------------------------
library(contrast)
library(tidymodels)
library(multilevelmod)

## ---- results = 'hide'--------------------------------------------------------
library(contrast)
library(tidymodels)
library(multilevelmod)

tidymodels_prefer()
theme_set(theme_bw())

## -----------------------------------------------------------------------------
two_factor_incompl %>% count(day, config, group)

## -----------------------------------------------------------------------------
two_factor_incompl %>% 
  ggplot(aes(x = day, y = expression, col = config)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~ subject) 

## -----------------------------------------------------------------------------
subject_four <- tibble(group = unique(two_factor_incompl$group), subject = "four")

## ----gee----------------------------------------------------------------------
gee_spec <- 
  linear_reg() %>% 
  set_engine("gee", corstr = "exchangeable")

gee_fit <- 
  gee_spec %>% 
  fit(expression ~ group + id_var(subject), data = two_factor_incompl)

gee_fit

## -----------------------------------------------------------------------------
predict(gee_fit, subject_four %>% select(group)) %>% 
  bind_cols(subject_four)

## ----gls----------------------------------------------------------------------
library(nlme) # <- Only need to load this to get cor*() functions

gls_spec <- 
  linear_reg() %>% 
  set_engine("gls", correlation = corCompSymm(form = ~ 1 | subject))

gls_fit <- 
  gls_spec %>% 
  fit(expression ~ group, data = two_factor_incompl)

gls_fit

## ----gls-pred-----------------------------------------------------------------
predict(gls_fit, subject_four %>% select(group)) %>% 
  bind_cols(subject_four)

## ----lme----------------------------------------------------------------------
lme_spec <- 
  linear_reg() %>% 
  set_engine("lme", random = ~ 1 | subject)

lme_fit <- 
  lme_spec %>% 
  fit(expression ~ group, data = two_factor_incompl)

lme_fit

## ----lme-pred-----------------------------------------------------------------
predict(lme_fit, subject_four) %>% 
  bind_cols(subject_four)

# For this design, this is the same prediction as a training set point:
predict(lme_fit, two_factor_incompl %>% filter(subject == "donor1"))

## ----lmer---------------------------------------------------------------------
lmer_spec <- 
  linear_reg() %>% 
  set_engine("lmer")

lmer_fit <- 
  lmer_spec %>% 
  fit(expression ~ group + (1|subject), data = two_factor_incompl)

lmer_fit

## ----lmer-pred----------------------------------------------------------------
predict(lmer_fit, subject_four) %>% 
  bind_cols(subject_four)

## -----------------------------------------------------------------------------
req_pkgs(lmer_spec)

## ----wflow--------------------------------------------------------------------
lmer_wflow <- 
  workflow() %>% 
  add_variables(outcomes = expression, predictors = c(group, subject)) %>% 
  add_model(lmer_spec, formula = expression ~ group + (1|subject))

lmer_wflow %>% fit(data = two_factor_incompl)

## ----rec----------------------------------------------------------------------
rec <- 
  recipe(expression ~ group + subject, data = two_factor_incompl) %>%
  add_role(subject, new_role = "exp_unit") %>%
  step_novel(all_nominal_predictors(), -has_role("exp_unit"))

lmer_wflow %>%
  remove_variables() %>%
  add_recipe(rec) %>%
  fit(data = two_factor_incompl)

## -----------------------------------------------------------------------------
lmer_wflow %>% 
  fit(data = two_factor_incompl) %>% # <- returns a workflow
  extract_fit_engine()               # <- returns the lmer object

