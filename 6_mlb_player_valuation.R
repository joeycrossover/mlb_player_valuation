source("code/libraries.R")

# the following code runs correlation analysis to identify
# potential features with predictive power
# and then fits statistical models
# to project player contract terms
# author: joseph coleman, 5/28/2025

# load data ----
# load the cluster_df data from '5_mlb_player_clustering.R'
cluster_df <- readxl::read_xlsx("data/cluster_df.xlsx")
cluster_df <- fastDummies::dummy_cols(cluster_df, select_columns = "pos")
cluster_df <- fastDummies::dummy_cols(cluster_df, select_columns = "cluster")
cluster_df[, 105:119] <- cluster_df[, 105:119] %>%
  mutate(across(everything(), ~replace_na(., 0)))

# load the normalized contract year stats from '5_mlb_player_clustering.R'
normalized_cluster_df <- readxl::read_xlsx("data/normalized_cluster_df.xlsx")
normalized_cluster_df <- fastDummies::dummy_cols(normalized_cluster_df, select_columns = "pos")
normalized_cluster_df <- fastDummies::dummy_cols(normalized_cluster_df, select_columns = "cluster")
normalized_cluster_df[, 105:119] <- normalized_cluster_df[, 105:119] %>%
  mutate(across(everything(), ~replace_na(., 0)))

# correlation analysis ----
# lets measure which variables have strongest linear correlation to our target variables (value and yrs)
numeric_vars <- cluster_df %>%
  select(10:ncol(cluster_df))

# get correlation matrix with target variable
correlations <- cor(numeric_vars, use = "complete.obs")

# view sorted correlations with 'value'
correlations_with_value <- as.data.frame(sort(correlations[, "value"], decreasing = TRUE))
correlations_with_value$feature <- rownames(correlations_with_value)
colnames(correlations_with_value)[1] <- "correlation"

top_corr_value_features <- correlations_with_value %>%
  dplyr::slice(c(4:19, 121:127))

# plot
ggplot(top_corr_value_features, aes(x = reorder(feature, correlation), y = correlation)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(limits = c(-0.5, 0.75)) +
  labs(
    title = "Top Features Correlated with Contract Value",
    x = "Feature",
    y = "Correlation with Contract Value"
  ) +
  theme_minimal(base_size = 14)

# view sorted correlations with 'years'
correlations_with_length <- as.data.frame(sort(correlations[, "yrs"], decreasing = TRUE))
correlations_with_length$feature <- rownames(correlations_with_length)
colnames(correlations_with_length)[1] <- "correlation"

top_corr_length_features <- correlations_with_length %>%
  dplyr::slice(c(3:4, 6:14, 119:127))

# player_age
# -0.4599379544

# plot
ggplot(top_corr_length_features, aes(x = reorder(feature, correlation), y = correlation)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(limits = c(-0.5, 0.75)) +
  labs(title = "Top Features Correlated with Contract Length",
       x = "Feature",
       y = "Correlation with Contract Length") +
  theme_minimal(base_size = 14)

# modeling ----
set.seed(123)

min_salary <- 760000
min_years <- 1

# model comparison functions
rmse_manual <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

r2_manual <- function(actual, predicted) {
  cor(actual, predicted)^2
}

# let's train on players that signed contracts before 2025
train_data <- normalized_cluster_df %>% filter(start < 2025)

# and we'll test on the players that just signed deals this year
# ends up being about a 75/25 split, which is cool
test_data <- normalized_cluster_df %>% filter(start >= 2025)

# model 1: linear regression with correlated features only
selected_value_features <- rownames(top_corr_value_features)
selected_length_features <- rownames(top_corr_length_features)
saveRDS(selected_value_features, "models/selected_value_features.rds")
saveRDS(selected_length_features, "models/selected_length_features.rds")

lmlc_df_train <- train_data %>% select(yrs, all_of(selected_length_features))
lmlc <- lm(yrs ~ ., data = lmlc_df_train)
train_data$lmlc_pred <- predict(lmlc, newdata = lmlc_df_train)
train_data$lmlc_pred <- pmax(train_data$lmlc_pred, min_years)

lmvc_df_train <- train_data %>% select(value, all_of(selected_value_features))
lmvc <- lm(value ~ ., data = lmvc_df_train)
train_data$lmvc_pred <- predict(lmvc, newdata = lmvc_df_train)
train_data$lmvc_pred <- pmax(train_data$lmvc_pred, min_salary)

# model 2: lasso regression (directly)
y_train_value <- train_data$value
y_train_length <- train_data$yrs
x_train <- as.matrix(train_data[, 13:136])

lasso_length_cv <- cv.glmnet(x_train, y_train_length, alpha = 1, nfolds = 10)
best_lambda_length <- lasso_length_cv$lambda.min

lasso_value_cv <- cv.glmnet(x_train, y_train_value, alpha = 1, nfolds = 10)
best_lambda_value <- lasso_value_cv$lambda.min

glml <- glmnet(x_train, y_train_length, alpha = 1, lambda = best_lambda_length)
glmv <- glmnet(x_train, y_train_value, alpha = 1, lambda = best_lambda_value)

train_data$glml_pred <- predict(glml, s = best_lambda_length, newx = x_train)
train_data$glmv_pred <- predict(glmv, s = best_lambda_value, newx = x_train)

train_data$glml_pred <- pmax(train_data$glml_pred, min_years)
train_data$glmv_pred <- pmax(train_data$glmv_pred, min_salary)

# model 3: linear regression on lasso-selected features
lasso_coefs_length <- coef(glml)
lasso_coefs_length_df <- as.data.frame(as.matrix(lasso_coefs_length))
colnames(lasso_coefs_length_df)[1] <- "coefficient"
lasso_coefs_length_df$feature <- rownames(lasso_coefs_length_df)

important_features_length <- lasso_coefs_length_df %>%
  filter(coefficient != 0 & feature != "(Intercept)")

selected_features_lasso_length <- important_features_length$feature

lasso_coefs_value <- coef(glmv)
lasso_coefs_value_df <- as.data.frame(as.matrix(lasso_coefs_value))
colnames(lasso_coefs_value_df)[1] <- "coefficient"
lasso_coefs_value_df$feature <- rownames(lasso_coefs_value_df)

important_features_value <- lasso_coefs_value_df %>%
  filter(coefficient != 0 & feature != "(Intercept)")

selected_features_lasso_value <- important_features_value$feature

lmll_df_train <- train_data %>% select(yrs, all_of(selected_features_lasso_length))
lmll <- lm(yrs ~ ., data = lmll_df_train)
train_data$lmll_pred <- predict(lmll, newdata = lmll_df_train)
train_data$lmll_pred <- pmax(train_data$lmll_pred, min_years)

lmvl_df_train <- train_data %>% select(value, all_of(selected_features_lasso_value))
lmvl <- lm(value ~ ., data = lmvl_df_train)
train_data$lmvl_pred <- predict(lmvl, newdata = lmvl_df_train)
train_data$lmvl_pred <- pmax(train_data$lmvl_pred, min_salary)

# model 4: xgboost
dtrain_length <- xgb.DMatrix(data = x_train, label = y_train_length)
dtrain_value <- xgb.DMatrix(data = x_train, label = y_train_value)

xgb_params <- list(
  objective = "reg:squarederror",
  eta = 0.05,                 # smaller step size to avoid overfitting
  max_depth = 3,              # shallow trees to control complexity
  subsample = 0.8,            # row sampling
  colsample_bytree = 0.8,     # column sampling
  gamma = 2,                  # min split loss to make a further partition
  lambda = 5,                 # L2 regularization
  alpha = 1                   # L1 regularization
)

xgbcv_length <- xgb.cv(
  params = xgb_params,
  data = dtrain_length,
  nrounds = 1000,
  nfold = 5,
  metrics = "rmse",
  early_stopping_rounds = 20,
  verbose = 0
)

best_nrounds_length <- xgbcv_length$best_iteration

xgbl <- xgboost(
  params = xgb_params,
  data = dtrain_length,
  nrounds = best_nrounds_length,
  verbose = 0
)

xgbcv_value <- xgb.cv(
  params = xgb_params,
  data = dtrain_value,
  nrounds = 1000,
  nfold = 5,
  metrics = "rmse",
  early_stopping_rounds = 20,
  verbose = 0
)

best_nrounds_value <- xgbcv_value$best_iteration

xgbv <- xgboost(
  params = xgb_params,
  data = dtrain_value,
  nrounds = best_nrounds_value,
  verbose = 0
)

train_data$xgbl_pred <- predict(xgbl, newdata = x_train)
train_data$xgbv_pred <- predict(xgbv, newdata = x_train)

train_data$xgbl_pred <- pmax(train_data$xgbl_pred, min_years)
train_data$xgbv_pred <- pmax(train_data$xgbv_pred, min_salary)

# model 5: multivariate regression
multivariate_features <- rownames(rbind(top_corr_length_features, top_corr_value_features) %>% select(feature) %>% distinct())

mlm_df_train <- train_data %>% select(yrs, value, multivariate_features)
mlm <- lm(cbind(yrs, value) ~ ., data = mlm_df_train)

mlm_pred <- predict(mlm, newdata = mlm_df_train)

train_data$mlml_pred <- mlm_pred[, 1]
train_data$mlmv_pred <- mlm_pred[, 2]

train_data$mlml_pred <- pmax(train_data$mlml_pred, min_years)
train_data$mlmv_pred <- pmax(train_data$mlmv_pred, min_salary)

# model comparison training set
train_y_true_length <- train_data$yrs
train_y_true_value <- train_data$value

train_rmse_lmlc <- rmse_manual(train_y_true_length, train_data$lmlc_pred)
train_r2_lmlc <- r2_manual(train_y_true_length, train_data$lmlc_pred)

train_rmse_lmvc <- rmse_manual(train_y_true_value, train_data$lmvc_pred)
train_r2_lmvc <- r2_manual(train_y_true_value, train_data$lmvc_pred)

train_rmse_glml <- rmse_manual(train_y_true_length, train_data$glml_pred)
train_r2_glml <- r2_manual(train_y_true_length, train_data$glml_pred)

train_rmse_glmv <- rmse_manual(train_y_true_value, train_data$glmv_pred)
train_r2_glmv <- r2_manual(train_y_true_value, train_data$glmv_pred)

train_rmse_lmll <- rmse_manual(train_y_true_length, train_data$lmll_pred)
train_r2_lmll <- r2_manual(train_y_true_length, train_data$lmll_pred)

train_rmse_lmvl <- rmse_manual(train_y_true_value, train_data$lmvl_pred)
train_r2_lmvl <- r2_manual(train_y_true_value, train_data$lmvl_pred)

train_rmse_xgbl <- rmse_manual(train_y_true_length, train_data$xgbl_pred)
train_r2_xgbl <- r2_manual(train_y_true_length, train_data$xgbl_pred)

train_rmse_xgbv <- rmse_manual(train_y_true_value, train_data$xgbv_pred)
train_r2_xgbv <- r2_manual(train_y_true_value, train_data$xgbv_pred)

train_rmse_mlml <- rmse_manual(train_y_true_length, train_data$mlml_pred)
train_r2_mlml <- r2_manual(train_y_true_length, train_data$mlml_pred)

train_rmse_mlmv <- rmse_manual(train_y_true_value, train_data$mlmv_pred)
train_r2_mlmv <- r2_manual(train_y_true_value, train_data$mlmv_pred)

train_performance_summary <- data.frame(
  Model = c("Contract Length Model (Correlated)", "Contract Length Model (Lasso)", 
            "Contract Length Model (Lasso-selected + LM)", "Contract Length Model (xgBoost)",
            "Contract Length Model (Multivariate)",
            "Contract Value Model (Correlated)", "Contract Value Model (Lasso)", 
            "Contract Value Model (Lasso-selected + LM)", "Contract Value Model (xgBoost)",
            "Contract Value Model (Multivariate)"),
  RMSE = c(train_rmse_lmlc, train_rmse_glml, train_rmse_lmll, train_rmse_xgbl, train_rmse_mlml,
           train_rmse_lmvc, train_rmse_glmv, train_rmse_lmvl, train_rmse_xgbv, train_rmse_mlmv),
  R2 = c(train_r2_lmlc, train_r2_glml, train_r2_lmll, train_r2_xgbl, train_r2_mlml,
         train_r2_lmvc, train_r2_glmv, train_r2_lmvl, train_r2_xgbv, train_r2_mlmv)
)

# make predictions for contracts signed by players in 2025
# model 1
lmlc_df_test <- test_data %>% select(all_of(selected_length_features))
test_data$lmlc_pred <- predict(lmlc, newdata = lmlc_df_test)
test_data$lmlc_pred <- pmax(test_data$lmlc_pred, min_years)

lmvc_df_test <- test_data %>% select(all_of(selected_value_features))
test_data$lmvc_pred <- predict(lmvc, newdata = lmvc_df_test)
test_data$lmvc_pred <- pmax(test_data$lmvc_pred, min_salary)

# model 2
x_test <- as.matrix(test_data[, 13:136])
test_data$glml_pred <- predict(glml, s = best_lambda_length, newx = x_test)
test_data$glmv_pred <- predict(glmv, s = best_lambda_value, newx = x_test)

test_data$glml_pred <- pmax(test_data$glml_pred, min_years)
test_data$glmv_pred <- pmax(test_data$glmv_pred, min_salary)

# model 3
lmll_df_test <- test_data %>% select(all_of(selected_features_lasso_length))
test_data$lmll_pred <- predict(lmll, newdata = lmll_df_test)
test_data$lmll_pred <- pmax(test_data$lmll_pred, min_years)

lmvl_df_test <- test_data %>% select(all_of(selected_features_lasso_value))
test_data$lmvl_pred <- predict(lmvl, newdata = lmvl_df_test)
test_data$lmvl_pred <- pmax(test_data$lmvl_pred, min_salary)

# model 4
test_data$xgbl_pred <- predict(xgbl, newdata = x_test)
test_data$xgbv_pred <- predict(xgbv, newdata = x_test)

test_data$xgbl_pred <- pmax(test_data$xgbl_pred, min_years)
test_data$xgbv_pred <- pmax(test_data$xgbv_pred, min_salary)

# model 5
mlm_df_test <- test_data %>% select(multivariate_features)
mlm_test_pred <- predict(mlm, newdata = mlm_df_test)

test_data$mlml_pred <- mlm_test_pred[, 1]
test_data$mlmv_pred <- mlm_test_pred[, 2]

test_data$mlml_pred <- pmax(test_data$mlml_pred, min_years)
test_data$mlmv_pred <- pmax(test_data$mlmv_pred, min_salary)

# compare model results test data
test_y_true_length <- test_data$yrs
test_y_true_value <- test_data$value

test_rmse_lmlc <- rmse_manual(test_y_true_length, test_data$lmlc_pred)
test_r2_lmlc <- r2_manual(test_y_true_length, test_data$lmlc_pred)

test_rmse_lmvc <- rmse_manual(test_y_true_value, test_data$lmvc_pred)
test_r2_lmvc <- r2_manual(test_y_true_value, test_data$lmvc_pred)

test_rmse_glml <- rmse_manual(test_y_true_length, test_data$glml_pred)
test_r2_glml <- r2_manual(test_y_true_length, test_data$glml_pred)

test_rmse_glmv <- rmse_manual(test_y_true_value, test_data$glmv_pred)
test_r2_glmv <- r2_manual(test_y_true_value, test_data$glmv_pred)

test_rmse_lmll <- rmse_manual(test_y_true_length, test_data$lmll_pred)
test_r2_lmll <- r2_manual(test_y_true_length, test_data$lmll_pred)

test_rmse_lmvl <- rmse_manual(test_y_true_value, test_data$lmvl_pred)
test_r2_lmvl <- r2_manual(test_y_true_value, test_data$lmvl_pred)

test_rmse_xgbl <- rmse_manual(test_y_true_length, test_data$xgbl_pred)
test_r2_xgbl <- r2_manual(test_y_true_length, test_data$xgbl_pred)

test_rmse_xgbv <- rmse_manual(test_y_true_value, test_data$xgbv_pred)
test_r2_xgbv <- r2_manual(test_y_true_value, test_data$xgbv_pred)

test_rmse_mlml <- rmse_manual(test_y_true_length, test_data$mlml_pred)
test_r2_mlml <- r2_manual(test_y_true_length, test_data$mlml_pred)

test_rmse_mlmv <- rmse_manual(test_y_true_value, test_data$mlmv_pred)
test_r2_mlmv <- r2_manual(test_y_true_value, test_data$mlmv_pred)

test_performance_summary <- data.frame(
  Model = c("Contract Length Model (Correlated)", "Contract Length Model (Lasso)", 
            "Contract Length Model (Lasso-selected + LM)", "Contract Length Model (xgBoost)",
            "Contract Length Model (Multivariate)",
            "Contract Value Model (Correlated)", "Contract Value Model (Lasso)", 
            "Contract Value Model (Lasso-selected + LM)", "Contract Value Model (xgBoost)",
            "Contract Value Model (Multivariate)"),
  RMSE = c(test_rmse_lmlc, test_rmse_glml, test_rmse_lmll, test_rmse_xgbl, test_rmse_mlml,
           test_rmse_lmvc, test_rmse_glmv, test_rmse_lmvl, test_rmse_xgbv, test_rmse_mlmv),
  R2 = c(test_r2_lmlc, test_r2_glml, test_r2_lmll, test_r2_xgbl, test_r2_mlml,
         test_r2_lmvc, test_r2_glmv, test_r2_lmvl, test_r2_xgbv, test_r2_mlmv)
)

# plot actuals vs predictions
actual_length <- test_data$yrs
actual_value <- test_data$value

proj_yrs <- test_data$lmlc_pred
proj_value <- test_data$mlmv_pred

v_residuals <- actual_value - proj_value
classification <- case_when(
  actual_value < 50 ~ ifelse(abs(v_residuals) < 0.5 * actual_value, "fair value",
                      ifelse(v_residuals < 0, "undervalued", "overvalued")),
  actual_value < 150 ~ ifelse(abs(v_residuals) < 0.35 * actual_value, "fair value",
                       ifelse(v_residuals < 0, "undervalued", "overvalued")),
  TRUE ~ ifelse(abs(v_residuals) < 0.25 * actual_value, "fair value",
                ifelse(v_residuals < 0, "undervalued", "overvalued"))
)

results_df <- test_data
results_df$classification <- classification
proj_aav <- proj_value / proj_yrs
results_df$proj_aav <- proj_aav
results_df <- results_df %>% select(1:12, lmlc_pred, mlmv_pred, proj_aav, classification, multivariate_features) %>%
  mutate(value = value / 1000000,
         aav = round(aav / 1000000, 1),
         mlmv_pred = round(mlmv_pred / 1000000, 1),
         lmlc_pred = round(lmlc_pred),
         proj_aav = round(proj_aav / 1000000, 1)) %>%
  rename("proj_yrs" = lmlc_pred,
         "proj_value" = mlmv_pred)

ribbon_data <- bind_rows(
  data.frame(value = seq(0, 50, by = 2)) %>%
    mutate(lower = value * 0.5, upper = value * 1.5),
  data.frame(value = seq(50, 150, by = 2)) %>%
    mutate(lower = value * 0.65, upper = value * 1.35),
  data.frame(value = seq(150, 800, by = 2)) %>%
    mutate(lower = value * 0.75, upper = value * 1.25)
)

label_subset <- results_df %>%
  group_by(classification) %>%
  slice_max(order_by = value, n = 3) %>%
  pull(full_name)

ggplot(results_df %>% rename(Classification = classification), aes(x = value, y = proj_value, color = Classification)) +
  geom_ribbon(
    data = ribbon_data,
    aes(x = value, ymin = lower, ymax = upper),
    fill = "grey80", alpha = 0.3, inherit.aes = FALSE
  ) +
  ggrepel::geom_text_repel(
    data = subset(results_df %>% rename(Classification = classification), full_name %in% label_subset),
    aes(label = full_name),
    size = 3.5,
    show.legend = FALSE,
    max.overlaps = 25,
    box.padding = 0.3,
    point.padding = 0.3,
    segment.color = 'grey60'
  ) +
  geom_point(size = 2, stroke = 0.5, alpha = 0.85) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(
    x = "Actual Contract Value (millions)",
    y = "Predicted Contract Value (millions)",
    title = "Actual vs Predicted Contract Value (2025 Signees)",
    subtitle = "Tiered error bands: ±50% (under $50M), ±35% ($50–150M), ±25% (over $150M)"
  ) +
  scale_color_manual(values = c(
    "fair value" = "blue",
    "undervalued" = "darkgreen",
    "overvalued" = "red"
  )) +
  scale_x_continuous(limits = c(0, 800), breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800)) +
  scale_y_continuous(limits = c(0, 800), breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800)) +
  theme_minimal()

results_df <- results_df %>% select(1:16) %>%
  merge(., cluster_df %>% select(player_id, all_of(multivariate_features)), by = "player_id")

writexl::write_xlsx(results_df, "data/test_contract_projections.xlsx")

valuation_summary <- results_df %>%
  select(classification, 10:15, 17:ncol(results_df)) %>%
  group_by(classification) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

writexl::write_xlsx(valuation_summary, "data/test_valuation_summary.xlsx")

features_to_plot <- c(
  "career_avg_war", "career_woba", "career_xwoba",
  "silver_slugger", "all-star", "career_avg_hr", "player_age", "delta_ab"
)

# Reshape to long format for ggplot
melted_results <- valuation_summary %>%
  select(classification, all_of(features_to_plot)) %>%
  pivot_longer(-classification, names_to = "feature", values_to = "average")

# Plot
# ggplot(melted_results, aes(x = average, y = feature, fill = classification)) +
#   geom_col(position = "dodge") +
#   labs(
#     title = "Feature Averages by Player Valuation (Test Set)",
#     x = "Average Value",
#     y = "Feature"
#   ) +
#   theme_minimal(base_size = 14) +
#   scale_fill_manual(values = c(
#     "undervalued" = "darkgreen",
#     "fair value" = "blue",
#     "overvalued" = "red"
#   ))

melted_scaled <- melted_results %>%
  group_by(feature) %>%
  mutate(
    z_score = (average - mean(average)) / sd(average),
    scaled_value = z_score - min(z_score) + 0.01  # ensure strictly > 0
  ) %>%
  ungroup()

ggplot(melted_results, aes(x = classification, y = average, fill = classification)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(
    aes(label = round(average, 2)),
    position = position_dodge(width = 0.6),
    vjust = 1.5,
    color = "white",
    size = 3.5
  ) +
  facet_wrap(~ feature, scales = "free_y") +
  labs(
    title = "Average Feature Values by Player Valuation (Test Set)",
    x = "Player Valuation",
    y = "Average Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c(
    "undervalued" = "darkgreen",
    "fair value" = "blue",
    "overvalued" = "red"
  ))