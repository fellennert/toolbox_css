needs(tidyverse, sf, maps, tigris, janitor)

library(rappdirs)

user_cache_dir()
options(tigris_use_cache = FALSE)
us_counties <- counties(cb = TRUE, resolution = "20m", year = 2016) |> 
  st_transform(5070) |> 
  mutate(FIPS = str_c(STATEFP, COUNTYFP))

child_pov <- read_csv("https://raw.githubusercontent.com/chrismgentry/Spatial-Regression/master/Data/childpov18_southfull.csv") |> 
  mutate(FIPS = as.character(FIPS))

us_counties_child_pov <- left_join(us_counties, child_pov, by = "FIPS") |> 
  clean_names() |> 
  filter(state %in% c("FL", "AL")) |> 
  rename(child_poverty = x2016_child_poverty)



us_counties_child_pov |> 
  ggplot() +
  geom_sf(aes(fill = child_poverty), 
          color = "gray70", 
          size = 0.1) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(size = 8)
  ) +
  labs(
    title = "Child Poverty in FL",
    subtitle = "Contiguous Albers Equal Area Projection",
    caption = "Data source: US Census Bureau via tigris",
    fill = "Child Poverty (%)"
  )

## morans I

library(tidyverse)
library(sf)
library(spdep)
library(tmap)

us_counties_child_pov <- us_counties_child_pov |> 
  st_make_valid()

fl_al_nb <- poly2nb(us_counties_child_pov, queen = TRUE)

fl_al_weights <- nb2listw(fl_al_nb, style = "W", zero.policy = TRUE)

# Calculate Global Moran's I
morans_i <- moran.test(us_counties_child_pov$child_poverty, 
                       fl_al_weights, 
                       zero.policy = TRUE)
morans_i

## regression
reg_mod <- lm(child_poverty ~ rural + urban + lnmanufacturing + lnag + 
  lnretail + lnhealthss + lnconstruction + lnlesshs + 
  lnunemployment + lnsinglemom + lnblack + lnhispanic + 
  lnuninsured + lnincome_ratio + lnteenbirth + lnunmarried + as.factor(state),
  data = us_counties_child_pov)

stargazer::stargazer(reg_mod)

us_counties_child_pov_res <-us_counties_child_pov |> 
  mutate(residuals = residuals(reg_mod),
         fitted = fitted(reg_mod))


fl_al_nb <- poly2nb(us_counties_child_pov_res, queen = TRUE)
fl_al_weights <- nb2listw(fl_al_nb, style = "W", zero.policy = TRUE)
residual_morans <- moran.test(us_counties_child_pov_res$residuals, 
                              fl_al_weights, 
                              zero.policy = TRUE)

us_counties_child_pov_res |> 
  ggplot() +
  geom_sf(aes(fill = residuals), 
          color = "gray70", 
          size = 0.1) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(size = 8)
  ) +
  labs(
    title = "Child Poverty in FL/AL",
    subtitle = "Contiguous Albers Equal Area Projection",
    caption = "Data source: US Census Bureau via tigris",
    fill = "Residuals"
  )

## local morans I

local_morans_resid <- localmoran(us_counties_child_pov_res$residuals, 
                                 fl_al_weights, 
                                 zero.policy = TRUE)

# Add local Moran's I of residuals to data
us_counties_child_pov_res <- us_counties_child_pov_res |> 
  mutate(
    local_i_resid = local_morans_resid[, "Ii"],
    local_i_p_resid = local_morans_resid[, "Pr(z != E(Ii))"]
  )

# Create diagnostic plots
# 1. Residuals Map
ggplot(us_counties_child_pov_res) +
  geom_sf(aes(fill = residuals), color = "gray70", size = 0.1) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0,
    name = "Residuals"
  ) +
  theme_minimal() +
  labs(
    title = "OLS Residuals",
    subtitle = "Red indicates over-prediction, Blue indicates under-prediction"
  )

## local morans I of res

ggplot(us_counties_child_pov_res) +
  geom_sf(aes(fill = local_i_resid), color = "gray70", size = 0.1) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0,
    name = "Local Moran's I\nof Residuals"
  ) +
  theme_minimal() +
  labs(
    title = "Local Moran's I of Residuals",
    subtitle = "Spatial Clustering of Model Residuals"
  )


#### spatial models

library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)

# Create spatial lags of independent variables
us_counties_child_pov <- us_counties_child_pov |> 
  mutate(
    W_rural = lag.listw(fl_al_weights, rural),
    W_urban = lag.listw(fl_al_weights, urban),
    W_lnmanufacturing = lag.listw(fl_al_weights, lnmanufacturing),
    W_lnag = lag.listw(fl_al_weights, lnag),
    W_lnretail = lag.listw(fl_al_weights, lnretail),
    W_lnhealthss = lag.listw(fl_al_weights, lnhealthss),
    W_lnconstruction = lag.listw(fl_al_weights, lnconstruction),
    W_lnlesshs = lag.listw(fl_al_weights, lnlesshs),
    W_lnunemployment = lag.listw(fl_al_weights, lnunemployment),
    W_lnsinglemom = lag.listw(fl_al_weights, lnsinglemom),
    W_lnblack = lag.listw(fl_al_weights, lnblack),
    W_lnhispanic = lag.listw(fl_al_weights, lnhispanic),
    W_lnuninsured = lag.listw(fl_al_weights, lnuninsured),
    W_lnincome_ratio = lag.listw(fl_al_weights, lnincome_ratio),
    W_lnteenbirth = lag.listw(fl_al_weights, lnteenbirth),
    W_lnunmarried = lag.listw(fl_al_weights, lnunmarried)
  )

# Fit SLX model
slx_formula <- child_poverty ~ rural + urban + lnmanufacturing + lnag + 
  lnretail + lnhealthss + lnconstruction + lnlesshs + 
  lnunemployment + lnsinglemom + lnblack + lnhispanic + 
  lnuninsured + lnincome_ratio + lnteenbirth + lnunmarried +
  W_rural + W_urban + W_lnmanufacturing + W_lnag + 
  W_lnretail + W_lnhealthss + W_lnconstruction + W_lnlesshs + 
  W_lnunemployment + W_lnsinglemom + W_lnblack + W_lnhispanic + 
  W_lnuninsured + W_lnincome_ratio + W_lnteenbirth + W_lnunmarried

slx_mod <- lm(slx_formula, data = us_counties_child_pov)

# Add SLX residuals to data
us_counties_child_pov <- us_counties_child_pov |> 
  mutate(
    slx_residuals = residuals(slx_mod),
    slx_fitted = fitted(slx_mod)
  )

# Test for spatial autocorrelation in SLX residuals
slx_residual_morans <- moran.test(us_counties_child_pov$slx_residuals, 
                                  fl_al_weights, 
                                  zero.policy = TRUE)

# Calculate local Moran's I for SLX residuals
local_morans_slx <- localmoran(us_counties_child_pov$slx_residuals, 
                               fl_al_weights, 
                               zero.policy = TRUE)

us_counties_child_pov <- us_counties_child_pov |> 
  mutate(
    local_i_slx = local_morans_slx[, "Ii"],
    local_i_p_slx = local_morans_slx[, "Pr(z != E(Ii))"]
  )

# Create diagnostic plots
# 1. SLX Residuals Map
slx_residuals_map <- ggplot(us_counties_child_pov) +
  geom_sf(aes(fill = slx_residuals), color = "gray70", size = 0.1) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0,
    name = "SLX Residuals"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(size = 8)
  ) +
  labs(
    title = "SLX Model Residuals",
    subtitle = "FL Child Poverty",
    caption = "Red indicates over-prediction, Blue indicates under-prediction"
  )

# 2. Local Moran's I of SLX Residuals
slx_local_i_map <- ggplot(us_counties_child_pov) +
  geom_sf(aes(fill = local_i_slx), color = "gray70", size = 0.1) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0,
    name = "Local Moran's I\nof SLX Residuals"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(size = 8)
  ) +
  labs(
    title = "Local Moran's I of SLX Residuals",
    subtitle = "FL Child Poverty",
    caption = "Spatial Clustering of SLX Model Residuals"
  )

# Print results
print("SLX Model Summary:")
print(summary(slx_mod))

print("\nMoran's I test for SLX residuals:")
print(slx_residual_morans)

# Compare models
models_comparison <- data.frame(
  Model = c("OLS", "SLX"),
  R_squared = c(summary(reg_mod)$r.squared, summary(slx_mod)$r.squared),
  Adj_R_squared = c(summary(reg_mod)$adj.r.squared, summary(slx_mod)$adj.r.squared),
  AIC = c(AIC(reg_mod), AIC(slx_mod))
)

print("\nModel Comparison:")
print(models_comparison)

# Print maps
print(slx_residuals_map)
print(slx_local_i_map)

# Optional: Create stargazer output for both models
if(require(stargazer)) {
  stargazer::stargazer(reg_mod, slx_mod,
                       title = "Regression Results",
                       column.labels = c("OLS", "SLX"))
}


## other models

library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)

# Prepare formula (using your existing specification)
model_formula <- child_poverty ~ rural + urban + lnmanufacturing + lnag + 
  lnretail + lnhealthss + lnconstruction + lnlesshs + 
  lnunemployment + lnsinglemom + lnblack + lnhispanic + 
  lnuninsured + lnincome_ratio + lnteenbirth + lnunmarried

# 1. Run Lagrange Multiplier diagnostics
lm_test <- lm.RStests(reg_mod, fl_al_weights, test = "all")
print("Lagrange Multiplier Diagnostics:")
print(lm_test)

# 2. Fit Spatial Lag Model (SAR)
sar_mod <- lagsarlm(model_formula,
                    data = us_counties_child_pov,
                    listw = fl_al_weights,
                    zero.policy = TRUE)

sar_mod |> summary()

# 3. Fit Spatial Error Model (SEM)
sem_mod <- errorsarlm(model_formula,
                      data = us_counties_child_pov,
                      listw = fl_al_weights,
                      zero.policy = TRUE)
sem_mod |> summary()

# 4. Fit Spatial Durbin Model (SDM)
sdm_mod <- lagsarlm(model_formula,
                    data = us_counties_child_pov,
                    listw = fl_al_weights,
                    type = "mixed",
                    zero.policy = TRUE)

# Extract residuals from all models and add to data
us_counties_child_pov <- us_counties_child_pov |> 
  mutate(
    sar_residuals = residuals(sar_mod),
    sem_residuals = residuals(sem_mod),
    sdm_residuals = residuals(sdm_mod),
    sar_fitted = fitted(sar_mod),
    sem_fitted = fitted(sem_mod),
    sdm_fitted = fitted(sdm_mod)
  )

# Test for spatial autocorrelation in residuals for each model
sar_residual_morans <- moran.test(us_counties_child_pov$sar_residuals, 
                                  fl_al_weights, 
                                  zero.policy = TRUE)
sem_residual_morans <- moran.test(us_counties_child_pov$sem_residuals, 
                                  fl_al_weights, 
                                  zero.policy = TRUE)
sdm_residual_morans <- moran.test(us_counties_child_pov$sdm_residuals, 
                                  fl_al_weights, 
                                  zero.policy = TRUE)

# Create residual maps for each model
create_residual_map <- function(data, residual_var, title) {
  ggplot(data) +
    geom_sf(aes(fill = .data[[residual_var]]), color = "gray70", size = 0.1) +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0,
      name = "Residuals"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5),
      axis.text = element_text(size = 8)
    ) +
    labs(
      title = title,
      subtitle = "FL/AL Child Poverty",
      caption = "Red indicates over-prediction, Blue indicates under-prediction"
    )
}

# Generate residual maps
sar_map <- create_residual_map(us_counties_child_pov, "sar_residuals", "SAR Model Residuals")
sem_map <- create_residual_map(us_counties_child_pov, "sem_residuals", "SEM Model Residuals")
sdm_map <- create_residual_map(us_counties_child_pov, "sdm_residuals", "SDM Model Residuals")

# Compare all models
models_comparison <- data.frame(
  Model = c("OLS", "SLX", "SAR", "SEM", "SDM"),
  LogLik = c(logLik(reg_mod), logLik(slx_mod), logLik(sar_mod), 
             logLik(sem_mod), logLik(sdm_mod)),
  AIC = c(AIC(reg_mod), AIC(slx_mod), AIC(sar_mod), 
          AIC(sem_mod), AIC(sdm_mod))
) |> 
  arrange(AIC)

# Calculate impacts for SAR and SDM models
sar_impacts <- impacts(sar_mod, listw = fl_al_weights)
sdm_impacts <- impacts(sdm_mod, listw = fl_al_weights)

# Format impacts for easier viewing
format_impacts <- function(impact_obj) {
  data.frame(
    Variable = names(impact_obj$direct),
    Direct = impact_obj$direct,
    Indirect = impact_obj$indirect,
    Total = impact_obj$total
  )
}

sar_impacts_df <- format_impacts(sar_impacts)
sdm_impacts_df <- format_impacts(sdm_impacts)

# Print results
print("Model Comparison:")
print(models_comparison)

print("\nMoran's I tests for residuals:")
print("SAR Model:")
print(sar_residual_morans)
print("SEM Model:")
print()
print("SDM Model:")
print(sdm_residual_morans)

print("\nSAR Model Impacts:")
print(sar_impacts_df)

print("\nSDM Model Impacts:")
print(sdm_impacts_df)

# Print diagnostic plots
print(sar_map)
print(sem_map)
print(sdm_map)

# Optional: Create stargazer output for all models
if(require(stargazer)) {
  stargazer::stargazer(reg_mod, slx_mod, sar_mod, sem_mod, sdm_mod,
                       title = "Regression Results",
                       #type = "text",
                       column.labels = c("OLS", "SLX", "SAR", "SEM", "SDM"))
}

# Optional: Save impacts to CSV
# write_csv(sar_impacts_df, "sar_impacts.csv")
# write_csv(sdm_impacts_df, "sdm_impacts.csv")

## sdem
# Fit SDEM model
sdem_mod <- errorsarlm(model_formula,
                       data = us_counties_child_pov,
                       listw = fl_al_weights,
                       etype = "emixed",
                       zero.policy = TRUE)

# Update models comparison
models_comparison <- data.frame(
  Model = c("OLS", "SLX", "SAR", "SEM", "SDM", "SDEM"),
  LogLik = c(logLik(reg_mod), logLik(slx_mod), logLik(sar_mod), 
             logLik(sem_mod), logLik(sdm_mod), logLik(sdem_mod)),
  AIC = c(AIC(reg_mod), AIC(slx_mod), AIC(sar_mod), 
          AIC(sem_mod), AIC(sdm_mod), AIC(sdem_mod))
) |> 
  arrange(AIC)

# Calculate impacts for all spatial models
sar_impacts <- impacts(sar_mod, listw = fl_al_weights)
sdm_impacts <- impacts(sdm_mod, listw = fl_al_weights)
sdem_impacts <- impacts(sdem_mod, listw = fl_al_weights)

# Format impacts
format_impacts <- function(impact_obj) {
  data.frame(
    Variable = names(impact_obj$direct),
    Direct = impact_obj$direct,
    Indirect = impact_obj$indirect,
    Total = impact_obj$total
  )
}

sar_impacts_df <- format_impacts(sar_impacts)
sdm_impacts_df <- format_impacts(sdm_impacts)
sdem_impacts_df <- format_impacts(sdem_impacts)

# Add SDEM residuals to data
us_counties_child_pov <- us_counties_child_pov |> 
  mutate(sdem_residuals = residuals(sdem_mod),
         sdem_fitted = fitted(sdem_mod))

# Test SDEM residuals
sdem_residual_morans <- moran.test(us_counties_child_pov$sdem_residuals, 
                                   fl_al_weights, 
                                   zero.policy = TRUE)

# Create SDEM residual map
sdem_map <- ggplot(us_counties_child_pov) +
  geom_sf(aes(fill = sdem_residuals), color = "gray70", size = 0.1) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0,
    name = "Residuals"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(size = 8)
  ) +
  labs(
    title = "SDEM Model Residuals",
    subtitle = "FL/AL Child Poverty",
    caption = "Red indicates over-prediction, Blue indicates under-prediction"
  )

# Print results
print("Model Comparison:")
print(models_comparison)

print("\nMoran's I tests for residuals:")
print("SAR Model:")
print(sar_residual_morans)
print("SEM Model:")
print(sem_residual_morans)
print("SDM Model:")
print(sdm_residual_morans)
print("SDEM Model:")
print(sdem_residual_morans)

print("\nSAR Model Impacts:")
print(sar_impacts_df)
print("\nSDM Model Impacts:")
print(sdm_impacts_df)
print("\nSDEM Model Impacts:")
print(sdem_impacts_df)

# Print maps
print(sar_map)
print(sem_map)
print(sdm_map)
print(sdem_map)

# Stargazer output
if(require(stargazer)) {
  stargazer::stargazer(reg_mod, slx_mod, sdem_mod, sar_mod, sem_mod, sdm_mod,
                       title = "Regression Results")
}

sar_mod |> summary()

slx_mod |> summary()
sem_mod |> summary()
