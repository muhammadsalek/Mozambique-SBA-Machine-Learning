


# ==========================================
# 1. Load required libraries
# ==========================================

library(haven)
library(dplyr)
library(caret)
library(mice)

set.seed(123)

# ==========================================
# 2. Load original dataset
# ==========================================

data <- read_dta(
  "D:/Research/BDHS Research/Nepal/SBA/Mozambique/Data/BDHS_cleaned.dta"
)

cat("Original dataset:", dim(data), "\n")

# ==========================================
# 3. Define SBA outcome
# ==========================================

data$SBA <- haven::as_factor(data$SBA)

data$SBA <- factor(
  data$SBA,
  levels = c(
    "Unskilled birth attendant",
    "Skilled birth attendant"
  ),
  labels = c("Unskilled", "Skilled")
)

print(table(data$SBA, useNA = "ifany"))
print(levels(data$SBA))

# ==========================================
# 4. Convert labelled predictors
# ==========================================

data_clean <- data %>%
  mutate(across(where(haven::is.labelled),
                ~ haven::as_factor(.)))

# Check variable classes
print(sapply(data_clean, class))

# Check original missing values
print(colSums(is.na(data_clean)))

# ==========================================
# 5. Stratified Train-Test Split (80:20)
# ==========================================

set.seed(123)

train_index <- caret::createDataPartition(
  data_clean$SBA,
  p = 0.80,
  list = FALSE
)

train_data <- data_clean[train_index, ]
test_data  <- data_clean[-train_index, ]

cat("\nTraining shape:", dim(train_data), "\n")
cat("Testing shape:", dim(test_data), "\n")

cat("\nTraining SBA distribution (%):\n")
print(prop.table(table(train_data$SBA)) * 100)

cat("\nTesting SBA distribution (%):\n")
print(prop.table(table(test_data$SBA)) * 100)

# ==========================================
# 6. Separate outcome from predictors
# ==========================================

train_x <- train_data %>%
  select(-SBA)

test_x <- test_data %>%
  select(-SBA)

# ==========================================
# 7. Define MICE methods
# ==========================================

method_vec <- sapply(train_x, function(x) {
  
  if (is.numeric(x)) {
    "pmm"
  } else if (is.factor(x) && nlevels(x) == 2) {
    "logreg"
  } else if (is.factor(x) && nlevels(x) > 2) {
    "polyreg"
  } else {
    ""
  }
  
})

print(method_vec)

# ==========================================
# 8. MICE on training data only
# ==========================================

set.seed(123)

imp_train <- mice(
  train_x,
  m = 5,
  maxit = 10,
  method = method_vec,
  seed = 123,
  printFlag = TRUE
)

# First completed training dataset
train_imputed_x <- complete(imp_train, 1)

# Add outcome back
train_imputed <- train_imputed_x
train_imputed$SBA <- train_data$SBA

# ==========================================
# 9. Impute test data using training models
# ==========================================

# Use trained MICE models to impute new test rows.
# Test rows are ignored during model fitting.

combined_x <- rbind(train_x, test_x)

ignore_rows <- c(
  rep(FALSE, nrow(train_x)),
  rep(TRUE, nrow(test_x))
)

set.seed(123)

imp_combined <- mice(
  combined_x,
  m = 5,
  maxit = 10,
  method = method_vec,
  ignore = ignore_rows,
  seed = 123,
  printFlag = TRUE
)

completed_x <- complete(imp_combined, 1)

test_imputed_x <- completed_x[
  (nrow(train_x) + 1):nrow(completed_x),
  ,
  drop = FALSE
]

test_imputed <- test_imputed_x
test_imputed$SBA <- test_data$SBA

# ==========================================
# 10. Final verification
# ==========================================

cat("\nTraining dimensions:\n")
print(dim(train_imputed))

cat("\nTesting dimensions:\n")
print(dim(test_imputed))

cat("\nRemaining training missing values:\n")
print(colSums(is.na(train_imputed)))

cat("\nRemaining testing missing values:\n")
print(colSums(is.na(test_imputed)))

cat("\nTotal missing values:\n")

print(sum(is.na(train_imputed)))
print(sum(is.na(test_imputed)))

# Check outcome distribution
print(table(train_imputed$SBA))
print(table(test_imputed$SBA))













library(ggplot2)
library(dplyr)

# ==========================================
# 1. SBA Distribution
# ==========================================

sba_plot_data <- train_imputed %>%
  count(SBA) %>%
  mutate(
    Percentage = round(100 * n / sum(n), 2),
    Label = paste0(SBA, "\n", Percentage, "%")
  )

print(sba_plot_data)

# Bright, clean colors
sba_colors <- c(
  "Unskilled" = "#22C7C9",
  "Skilled" = "#3488E8"
)

# ==========================================
# 2. Publication-Style Pie Chart
# ==========================================

p_pie <- ggplot(
  sba_plot_data,
  aes(x = "", y = n, fill = SBA)
) +
  geom_col(
    width = 1,
    color = "white",
    linewidth = 1.2
  ) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = Label),
    position = position_stack(vjust = 0.5),
    size = 4.5,
    fontface = "bold",
    color = "white",
    lineheight = 1.15
  ) +
  scale_fill_manual(values = sba_colors) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "none",
    plot.background = element_rect(
      fill = "white", color = NA
    ),
    plot.margin = margin(15, 15, 15, 15)
  )

print(p_pie)

# ==========================================
# 3. Publication-Style Donut Chart
# ==========================================

p_donut <- ggplot(
  sba_plot_data,
  aes(x = 2, y = n, fill = SBA)
) +
  geom_col(
    width = 0.85,
    color = "white",
    linewidth = 1.2
  ) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = Label),
    position = position_stack(vjust = 0.5),
    size = 4,
    fontface = "bold",
    color = "white",
    lineheight = 1.1
  ) +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = sba_colors) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "none",
    plot.background = element_rect(
      fill = "white", color = NA
    ),
    plot.margin = margin(15, 15, 15, 15)
  )

print(p_donut)










library(Boruta)
library(caret)
library(dplyr)
library(ggplot2)

# ==========================================
# 1. Prepare Training Data
# ==========================================

train_data <- train_imputed
train_data$SBA <- as.factor(train_data$SBA)

# Identify numeric predictors
num_vars <- names(train_data)[sapply(train_data, is.numeric)]

# Scale numeric predictors and check correlation
if (length(num_vars) > 1) {
  
  train_data[num_vars] <- scale(train_data[num_vars])
  
  cor_mat <- cor(train_data[, num_vars, drop = FALSE])
  
  highCor <- findCorrelation(cor_mat, cutoff = 0.90)
  
  if (length(highCor) > 0) {
    cat("Highly correlated predictors removed:\n")
    print(num_vars[highCor])
    
    train_data <- train_data[
      , !names(train_data) %in% num_vars[highCor]
    ]
  }
  
} else {
  cat("Correlation filtering skipped.\n")
}

# ==========================================
# 2. Run Boruta
# ==========================================

set.seed(123)

boruta_result <- Boruta(
  SBA ~ .,
  data = train_data,
  doTrace = 2,
  maxRuns = 100
)

# Resolve tentative features
boruta_final <- TentativeRoughFix(boruta_result)

# Extract confirmed predictors
final_vars <- getSelectedAttributes(
  boruta_final,
  withTentative = FALSE
)

cat("\nConfirmed predictors:\n")
print(final_vars)

cat("\nTotal confirmed:", length(final_vars), "\n")

# Selected training dataset
train_boruta <- train_data[
  , c(final_vars, "SBA"), drop = FALSE
]

# ==========================================
# 3. Boruta Importance Summary
# ==========================================

boruta_stats <- attStats(boruta_final)

print(
  boruta_stats[
    , c("meanImp", "medianImp", "decision")
  ]
)

# ==========================================
# 4. Traditional Boruta Boxplot
# ==========================================

par(
  mar = c(11, 4, 2, 1),
  bg = "white"
)

plot(
  boruta_final,
  cex.axis = 0.75,
  las = 2,
  xlab = "",
  ylab = "Feature Importance",
  main = "",
  colCode = c(
    "#F05A71",  # Rejected: bright coral
    "#FFD166",  # Tentative: amber
    "#21B68B",  # Confirmed: emerald
    "#438FE8"   # Shadow: blue
  )
)

# ==========================================
# 5. Importance Barplot
# ==========================================

boruta_plot_df <- data.frame(
  Feature = rownames(boruta_stats),
  MeanImp = boruta_stats$meanImp,
  MinImp = boruta_stats$minImp,
  MaxImp = boruta_stats$maxImp,
  Decision = boruta_stats$decision
)

# Sort by mean importance
boruta_plot_df <- boruta_plot_df[
  order(boruta_plot_df$MeanImp, decreasing = FALSE),
]

boruta_plot_df$Feature <- factor(
  boruta_plot_df$Feature,
  levels = boruta_plot_df$Feature
)

# Bright publication-style colors
boruta_colors <- c(
  "Confirmed" = "#21B68B",
  "Rejected" = "#F05A71",
  "Tentative" = "#FFD166"
)

p_boruta <- ggplot(
  boruta_plot_df,
  aes(x = Feature, y = MeanImp, fill = Decision)
) +
  geom_col(
    width = 0.72,
    color = "white",
    linewidth = 0.35
  ) +
  geom_errorbar(
    aes(ymin = MinImp, ymax = MaxImp),
    width = 0.22,
    linewidth = 0.45,
    color = "#343A40"
  ) +
  coord_flip() +
  scale_fill_manual(values = boruta_colors) +
  labs(
    x = NULL,
    y = "Mean Feature Importance",
    fill = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.y = element_text(
      size = 10.5,
      color = "#222222"
    ),
    axis.text.x = element_text(
      size = 10,
      color = "#333333"
    ),
    axis.title.x = element_text(
      size = 12,
      face = "bold"
    ),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 11),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.grid.major.x = element_line(
      color = "#ECEFF1",
      linewidth = 0.3
    ),
    panel.grid.minor = element_blank()
  )

print(p_boruta)








# ==========================================
# 5. Boruta Feature Importance Barplot
# ==========================================

library(ggplot2)

boruta_plot_df <- data.frame(
  Feature = rownames(boruta_stats),
  MeanImp = boruta_stats$meanImp,
  MinImp = boruta_stats$minImp,
  MaxImp = boruta_stats$maxImp,
  Decision = boruta_stats$decision
)

# Order features by importance
boruta_plot_df <- boruta_plot_df[
  order(boruta_plot_df$MeanImp, decreasing = FALSE),
]

boruta_plot_df$Feature <- factor(
  boruta_plot_df$Feature,
  levels = boruta_plot_df$Feature
)

# Bright, refined colors
boruta_colors <- c(
  "Confirmed" = "#00C896",  # Vibrant emerald
  "Rejected"  = "#FF6482",  # Bright coral pink
  "Tentative" = "#FFBF47"   # Warm golden yellow
)

# Publication-style barplot
p_boruta <- ggplot(
  boruta_plot_df,
  aes(x = Feature, y = MeanImp, fill = Decision)
) +
  geom_col(
    width = 0.68,
    color = "white",
    linewidth = 0.45
  ) +
  geom_errorbar(
    aes(ymin = MinImp, ymax = MaxImp),
    width = 0.18,
    linewidth = 0.55,
    color = "#25364A"
  ) +
  coord_flip() +
  scale_fill_manual(
    values = boruta_colors,
    drop = FALSE
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.10))
  ) +
  labs(
    x = NULL,
    y = "Mean Feature Importance",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    # Feature labels
    axis.text.y = element_text(
      size = 11,
      face = "plain",
      color = "#172B4D"
    ),
    
    # Importance values
    axis.text.x = element_text(
      size = 10.5,
      color = "#344054"
    ),
    
    axis.title.x = element_text(
      size = 12,
      face = "bold",
      color = "#172B4D",
      margin = margin(t = 10)
    ),
    
    # Clean grid
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      color = "#E8EDF3",
      linewidth = 0.35
    ),
    
    # Legend
    legend.position = "top",
    legend.justification = "center",
    legend.text = element_text(
      size = 11,
      color = "#263445"
    ),
    legend.key.size = grid::unit(0.55, "cm"),
    
    # Background
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    # Spacing
    plot.margin = margin(12, 18, 12, 12)
  )

print(p_boruta)











#### Model Training ####

# =====================================================
# 1. LIBRARIES
# =====================================================

library(caret)
library(dplyr)
library(tidyr)
library(pROC)
library(ggplot2)
library(randomForest)
library(rpart)
library(kernlab)
library(xgboost)
library(nnet)

set.seed(123)

# =====================================================
# 2. BORUTA-SELECTED TRAIN AND TEST DATA
# =====================================================

stopifnot(length(final_vars) > 0)

train_ml <- as.data.frame(
  train_imputed[, c(final_vars, "SBA"), drop = FALSE]
)

test_ml <- as.data.frame(
  test_imputed[, c(final_vars, "SBA"), drop = FALSE]
)

# Outcome: Skilled is the positive class
train_ml$SBA <- factor(
  as.character(train_ml$SBA),
  levels = c("Skilled", "Unskilled")
)

test_ml$SBA <- factor(
  as.character(test_ml$SBA),
  levels = c("Skilled", "Unskilled")
)

# =====================================================
# 3. FIX FACTOR LEVELS
# =====================================================

for (nm in final_vars) {
  
  if (is.factor(train_ml[[nm]]) ||
      is.character(train_ml[[nm]])) {
    
    old_levels <- if (is.factor(train_ml[[nm]])) {
      levels(train_ml[[nm]])
    } else {
      unique(as.character(train_ml[[nm]]))
    }
    
    new_levels <- iconv(
      old_levels,
      to = "ASCII//TRANSLIT",
      sub = "_"
    )
    
    new_levels <- gsub(
      "[^A-Za-z0-9]+", "_", new_levels
    )
    
    new_levels <- gsub(
      "^_+|_+$", "", new_levels
    )
    
    new_levels[is.na(new_levels) |
                 new_levels == ""] <- "Category"
    
    new_levels <- make.unique(
      new_levels, sep = "_"
    )
    
    # Same mapping in train and test
    train_ml[[nm]] <- factor(
      as.character(train_ml[[nm]]),
      levels = old_levels,
      labels = new_levels
    )
    
    test_ml[[nm]] <- factor(
      as.character(test_ml[[nm]]),
      levels = old_levels,
      labels = new_levels
    )
  }
}

# Consistent column names
clean_names <- make.names(
  iconv(
    names(train_ml),
    to = "ASCII//TRANSLIT",
    sub = "_"
  ),
  unique = TRUE
)

names(train_ml) <- clean_names
names(test_ml) <- clean_names

stopifnot(
  identical(names(train_ml), names(test_ml)),
  !anyNA(train_ml),
  !anyNA(test_ml)
)

# =====================================================
# 4. CREATE CONSISTENT NUMERIC DESIGN MATRICES
# =====================================================

# This replaces the recipe that caused the error.
# Categorical variables are converted to dummy columns.
# The SAME coding is used for training and testing.

x_train <- train_ml[, names(train_ml) != "SBA",
                    drop = FALSE]

x_test <- test_ml[, names(test_ml) != "SBA",
                  drop = FALSE]

design_terms <- terms(~ ., data = x_train)

X_train <- model.matrix(
  design_terms,
  data = x_train
)[, -1, drop = FALSE]

X_test <- model.matrix(
  design_terms,
  data = x_test
)[, -1, drop = FALSE]

# Align test matrix to training matrix
X_test <- X_test[
  , colnames(X_train), drop = FALSE
]

# Remove constant columns based on training only
keep_cols <- apply(
  X_train, 2,
  function(x) length(unique(x)) > 1
)

X_train <- X_train[
  , keep_cols, drop = FALSE
]

X_test <- X_test[
  , keep_cols, drop = FALSE
]

X_train <- as.data.frame(X_train)
X_test <- as.data.frame(X_test)

names(X_train) <- make.names(
  names(X_train), unique = TRUE
)

names(X_test) <- names(X_train)

y_train <- train_ml$SBA
y_test <- test_ml$SBA

stopifnot(
  ncol(X_train) > 0,
  identical(names(X_train), names(X_test)),
  !anyNA(X_train),
  !anyNA(X_test)
)

cat("\nOriginal Boruta predictors:",
    length(final_vars), "\n")

cat("Encoded predictors:",
    ncol(X_train), "\n")

cat("Training observations:",
    nrow(X_train), "\n")

cat("Testing observations:",
    nrow(X_test), "\n")

print(table(y_train))
print(table(y_test))

# =====================================================
# 5. STRATIFIED 5-FOLD CROSS-VALIDATION
# =====================================================

set.seed(123)

folds <- createFolds(
  y_train,
  k = 5,
  returnTrain = TRUE
)

ctrl <- trainControl(
  method = "cv",
  number = 5,
  index = folds,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  allowParallel = FALSE,
  verboseIter = FALSE
)

# =====================================================
# 6. HYPERPARAMETER TUNING GRIDS
# =====================================================

p <- ncol(X_train)

rf_grid <- expand.grid(
  mtry = unique(pmax(
    1,
    pmin(p, c(
      floor(sqrt(p)),
      floor(p / 3),
      floor(p / 2),
      p
    ))
  ))
)

dt_grid <- expand.grid(
  cp = c(0.001, 0.005, 0.01, 0.03, 0.05)
)

knn_grid <- expand.grid(
  k = c(3, 5, 7, 9, 11, 15)
)

svm_grid <- expand.grid(
  sigma = c(0.001, 0.01, 0.05),
  C = c(0.1, 1, 10)
)

xgb_grid <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(3, 5),
  eta = c(0.05, 0.10),
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)

nnet_grid <- expand.grid(
  size = c(3, 5, 7),
  decay = c(0.001, 0.01, 0.1)
)

# =====================================================
# 7. TRAIN ALL SEVEN MODELS
# =====================================================

results <- list()

fit_model <- function(
    model_name,
    method_name,
    tuning_grid = NULL,
    preprocess = NULL,
    ...
) {
  
  cat("\n================================\n")
  cat("Training:", model_name, "\n")
  cat("================================\n")
  
  set.seed(123)
  
  args <- list(
    x = X_train,
    y = y_train,
    method = method_name,
    trControl = ctrl,
    metric = "ROC"
  )
  
  if (!is.null(tuning_grid)) {
    args$tuneGrid <- tuning_grid
  }
  
  if (!is.null(preprocess)) {
    args$preProcess <- preprocess
  }
  
  args <- c(args, list(...))
  
  tryCatch(
    do.call(caret::train, args),
    error = function(e) {
      message(
        "FAILED: ", model_name,
        "\n", conditionMessage(e)
      )
      NULL
    }
  )
}

# Random Forest
results[["Random Forest"]] <- fit_model(
  "Random Forest",
  "rf",
  rf_grid,
  ntree = 500
)

# Decision Tree
results[["Decision Tree"]] <- fit_model(
  "Decision Tree",
  "rpart",
  dt_grid
)

# KNN
results[["KNN"]] <- fit_model(
  "KNN",
  "knn",
  knn_grid,
  preprocess = c("center", "scale")
)

# Logistic Regression
results[["Logistic Regression"]] <- fit_model(
  "Logistic Regression",
  "glm",
  family = binomial()
)

# SVM
results[["SVM"]] <- fit_model(
  "SVM",
  "svmRadial",
  svm_grid,
  preprocess = c("center", "scale")
)

# XGBoost
results[["XGBoost"]] <- fit_model(
  "XGBoost",
  "xgbTree",
  xgb_grid,
  nthread = 1,
  verbosity = 0
)

# Neural Network
results[["Neural Network"]] <- fit_model(
  "Neural Network",
  "nnet",
  nnet_grid,
  preprocess = c("center", "scale"),
  trace = FALSE,
  MaxNWts = 10000,
  maxit = 500
)

# Check training status
training_status <- data.frame(
  Model = names(results),
  Status = ifelse(
    vapply(results, is.null, logical(1)),
    "Failed",
    "Success"
  )
)

print(training_status)

# Keep successful models
results <- Filter(
  Negate(is.null),
  results
)

if (length(results) == 0) {
  stop("All models failed. Inspect warnings().")
}

# =====================================================
# 8. BEST TUNING PARAMETERS
# =====================================================

best_tuning <- bind_rows(
  lapply(names(results), function(m) {
    
    data.frame(
      Model = m,
      Parameter = paste(
        names(results[[m]]$bestTune),
        unlist(results[[m]]$bestTune),
        sep = "=",
        collapse = "; "
      )
    )
  })
)

print(best_tuning)

# =====================================================
# 9. PERFORMANCE METRICS
# =====================================================

calculate_metrics <- function(truth, pred, prob) {
  
  y <- as.integer(truth == "Skilled")
  
  p <- pmin(
    pmax(as.numeric(prob), 1e-6),
    1 - 1e-6
  )
  
  TP <- sum(y == 1 & pred == "Skilled")
  TN <- sum(y == 0 & pred == "Unskilled")
  FP <- sum(y == 0 & pred == "Skilled")
  FN <- sum(y == 1 & pred == "Unskilled")
  
  div <- function(a, b) {
    if (b == 0) return(NA_real_)
    a / b
  }
  
  sensitivity <- div(TP, TP + FN)
  specificity <- div(TN, TN + FP)
  precision <- div(TP, TP + FP)
  npv <- div(TN, TN + FN)
  
  accuracy <- (TP + TN) / length(y)
  
  f1 <- div(
    2 * TP,
    2 * TP + FP + FN
  )
  
  balanced_accuracy <- (
    sensitivity + specificity
  ) / 2
  
  mcc <- div(
    TP * TN - FP * FN,
    sqrt(
      (TP + FP) *
        (TP + FN) *
        (TN + FP) *
        (TN + FN)
    )
  )
  
  pe <- (
    (TP + FP) * (TP + FN) +
      (TN + FN) * (TN + FP)
  ) / length(y)^2
  
  kappa <- div(
    accuracy - pe,
    1 - pe
  )
  
  auroc <- tryCatch(
    as.numeric(
      pROC::auc(
        pROC::roc(
          response = y,
          predictor = p,
          levels = c(0, 1),
          direction = "<",
          quiet = TRUE
        )
      )
    ),
    error = function(e) NA_real_
  )
  
  brier <- mean((p - y)^2)
  
  lp <- qlogis(p)
  
  cal_intercept <- tryCatch(
    suppressWarnings(
      unname(coef(glm(
        y ~ 1,
        offset = lp,
        family = binomial()
      ))[1])
    ),
    error = function(e) NA_real_
  )
  
  cal_slope <- tryCatch(
    suppressWarnings(
      unname(coef(glm(
        y ~ lp,
        family = binomial()
      ))[2])
    ),
    error = function(e) NA_real_
  )
  
  youden_j <- sensitivity + specificity - 1
  
  c(
    Accuracy = accuracy,
    Balanced_Accuracy = balanced_accuracy,
    Sensitivity = sensitivity,
    Specificity = specificity,
    Precision = precision,
    NPV = npv,
    F1 = f1,
    MCC = mcc,
    Kappa = kappa,
    AUROC = auroc,
    Brier_Score = brier,
    Calibration_Intercept = cal_intercept,
    Calibration_Slope = cal_slope,
    Youden_J = youden_j
  )
}

# =====================================================
# 10. BOOTSTRAP 95% CONFIDENCE INTERVALS
# =====================================================

bootstrap_ci <- function(
    truth, pred, prob,
    B = 1000, seed = 123
) {
  
  set.seed(seed)
  
  point <- calculate_metrics(
    truth, pred, prob
  )
  
  n <- length(truth)
  
  boot <- replicate(B, {
    
    # Ordinary bootstrap:
    # also captures uncertainty in class prevalence
    idx <- sample(
      seq_len(n),
      size = n,
      replace = TRUE
    )
    
    calculate_metrics(
      truth[idx],
      pred[idx],
      prob[idx]
    )
  })
  
  lower <- apply(boot, 1, function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 20) return(NA_real_)
    unname(quantile(x, 0.025))
  })
  
  upper <- apply(boot, 1, function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 20) return(NA_real_)
    unname(quantile(x, 0.975))
  })
  
  data.frame(
    Metric = names(point),
    Estimate = as.numeric(point),
    Lower_95CI = as.numeric(lower),
    Upper_95CI = as.numeric(upper)
  )
}

# =====================================================
# 11. HELD-OUT TEST SET EVALUATION
# =====================================================

performance_list <- list()
test_predictions <- list()

for (m in names(results)) {
  
  cat("\nEvaluating:", m, "\n")
  
  model <- results[[m]]
  
  pred <- predict(
    model,
    newdata = X_test
  )
  
  prob <- predict(
    model,
    newdata = X_test,
    type = "prob"
  )[, "Skilled"]
  
  test_predictions[[m]] <- data.frame(
    SBA = y_test,
    Predicted = pred,
    Probability = prob
  )
  
  performance_list[[m]] <- bootstrap_ci(
    truth = y_test,
    pred = pred,
    prob = prob,
    B = 1000,
    seed = 123
  ) %>%
    mutate(
      Model = m,
      .before = 1
    )
}

# =====================================================
# 12. RESULTS WITH 95% CI
# =====================================================

performance_long <- bind_rows(
  performance_list
)

performance_long <- performance_long %>%
  mutate(
    Result = ifelse(
      is.na(Lower_95CI) |
        is.na(Upper_95CI),
      sprintf("%.3f (CI unavailable)", Estimate),
      sprintf(
        "%.3f (%.3f–%.3f)",
        Estimate,
        Lower_95CI,
        Upper_95CI
      )
    )
  )

performance_table <- performance_long %>%
  select(Model, Metric, Result) %>%
  pivot_wider(
    names_from = Metric,
    values_from = Result
  )

print(
  performance_table,
  width = Inf
)

print(
  performance_long,
  n = Inf
)

# =====================================================
# 13. CONFUSION MATRICES
# =====================================================

for (m in names(test_predictions)) {
  
  cat("\nModel:", m, "\n")
  
  print(
    confusionMatrix(
      test_predictions[[m]]$Predicted,
      test_predictions[[m]]$SBA,
      positive = "Skilled"
    )
  )
}

# =====================================================
# 14. CALIBRATION PLOTS
# =====================================================

calibration_data <- bind_rows(
  lapply(names(test_predictions), function(m) {
    
    x <- test_predictions[[m]]
    
    data.frame(
      Model = m,
      Observed = as.integer(
        x$SBA == "Skilled"
      ),
      Predicted_Probability = x$Probability
    )
  })
)

p_calibration <- ggplot(
  calibration_data,
  aes(
    x = Predicted_Probability,
    y = Observed
  )
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "#777777"
  ) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    color = "#1688D4",
    linewidth = 1.1
  ) +
  facet_wrap(
    ~ Model,
    ncol = 3
  ) +
  coord_cartesian(
    xlim = c(0, 1),
    ylim = c(0, 1)
  ) +
  labs(
    x = "Predicted Probability of Skilled SBA",
    y = "Observed Proportion"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.text = element_text(
      face = "bold",
      size = 10
    ),
    panel.border = element_rect(
      color = "#E1E6EB",
      fill = NA
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    )
  )

print(p_calibration)




















#### Evaluations ####

# =====================================================
# 1. LIBRARIES
# =====================================================

library(caret)
library(dplyr)
library(tidyr)
library(pROC)
library(ggplot2)
library(tibble)

set.seed(123)

# =====================================================
# 2. PERFORMANCE METRICS
# =====================================================

calculate_metrics <- function(truth, pred, prob) {
  
  truth <- as.character(truth)
  pred <- as.character(pred)
  
  stopifnot(
    length(truth) == length(pred),
    length(truth) == length(prob),
    all(truth %in% c("Skilled", "Unskilled")),
    all(pred %in% c("Skilled", "Unskilled")),
    all(is.finite(prob)),
    all(prob >= 0 & prob <= 1)
  )
  
  y <- as.numeric(truth == "Skilled")
  
  # Clip only for logit-based calibration
  p <- pmin(
    pmax(as.numeric(prob), 1e-6),
    1 - 1e-6
  )
  
  # Numeric counts prevent integer overflow
  TP <- as.numeric(sum(y == 1 & pred == "Skilled"))
  TN <- as.numeric(sum(y == 0 & pred == "Unskilled"))
  FP <- as.numeric(sum(y == 0 & pred == "Skilled"))
  FN <- as.numeric(sum(y == 1 & pred == "Unskilled"))
  
  safe_div <- function(a, b) {
    if (!is.finite(b) || b == 0) {
      return(NA_real_)
    }
    a / b
  }
  
  # Classification metrics
  n <- as.numeric(length(y))
  
  accuracy <- safe_div(TP + TN, n)
  sensitivity <- safe_div(TP, TP + FN)
  specificity <- safe_div(TN, TN + FP)
  precision <- safe_div(TP, TP + FP)
  npv <- safe_div(TN, TN + FN)
  
  f1 <- safe_div(
    2 * TP,
    2 * TP + FP + FN
  )
  
  balanced_accuracy <- (
    sensitivity + specificity
  ) / 2
  
  # Matthews Correlation Coefficient
  mcc <- safe_div(
    TP * TN - FP * FN,
    sqrt(
      (TP + FP) *
        (TP + FN) *
        (TN + FP) *
        (TN + FN)
    )
  )
  
  # Cohen's Kappa
  pe <- (
    (TP + FP) * (TP + FN) +
      (TN + FN) * (TN + FP)
  ) / n^2
  
  kappa <- safe_div(
    accuracy - pe,
    1 - pe
  )
  
  # AUROC
  auroc <- tryCatch({
    
    if (length(unique(y)) < 2) {
      NA_real_
    } else {
      as.numeric(
        pROC::auc(
          pROC::roc(
            response = y,
            predictor = as.numeric(prob),
            levels = c(0, 1),
            direction = "<",
            quiet = TRUE
          )
        )
      )
    }
    
  }, error = function(e) NA_real_)
  
  # Brier Score: original probabilities
  brier <- mean(
    (as.numeric(prob) - y)^2
  )
  
  # Calibration metrics
  lp <- qlogis(p)
  
  # Calibration-in-the-large intercept
  # Ideal value = 0
  cal_intercept <- tryCatch({
    
    fit <- suppressWarnings(
      glm(
        y ~ 1,
        offset = lp,
        family = binomial()
      )
    )
    
    as.numeric(coef(fit)[1])
    
  }, error = function(e) NA_real_)
  
  # Calibration slope
  # Ideal value = 1
  cal_slope <- tryCatch({
    
    fit <- suppressWarnings(
      glm(
        y ~ lp,
        family = binomial()
      )
    )
    
    as.numeric(coef(fit)[2])
    
  }, error = function(e) NA_real_)
  
  # Youden's J at existing classification threshold
  youden_j <- sensitivity + specificity - 1
  
  c(
    Accuracy = accuracy,
    Balanced_Accuracy = balanced_accuracy,
    Sensitivity = sensitivity,
    Specificity = specificity,
    Precision = precision,
    NPV = npv,
    F1 = f1,
    MCC = mcc,
    Kappa = kappa,
    AUROC = auroc,
    Brier_Score = brier,
    Calibration_Intercept = cal_intercept,
    Calibration_Slope = cal_slope,
    Youden_J = youden_j
  )
}

# =====================================================
# 3. BOOTSTRAP 95% CONFIDENCE INTERVALS
# =====================================================

bootstrap_ci <- function(
    truth, pred, prob,
    B = 1000,
    seed = 123
) {
  
  set.seed(seed)
  
  point <- calculate_metrics(
    truth, pred, prob
  )
  
  n <- length(truth)
  
  boot <- replicate(B, {
    
    idx <- sample(
      seq_len(n),
      size = n,
      replace = TRUE
    )
    
    calculate_metrics(
      truth[idx],
      pred[idx],
      prob[idx]
    )
  })
  
  lower <- apply(
    boot, 1,
    function(x) {
      
      x <- x[is.finite(x)]
      
      if (length(x) < 20) {
        return(NA_real_)
      }
      
      as.numeric(
        quantile(
          x,
          probs = 0.025,
          names = FALSE
        )
      )
    }
  )
  
  upper <- apply(
    boot, 1,
    function(x) {
      
      x <- x[is.finite(x)]
      
      if (length(x) < 20) {
        return(NA_real_)
      }
      
      as.numeric(
        quantile(
          x,
          probs = 0.975,
          names = FALSE
        )
      )
    }
  )
  
  data.frame(
    Metric = names(point),
    Estimate = as.numeric(point),
    Lower_95CI = as.numeric(lower),
    Upper_95CI = as.numeric(upper),
    row.names = NULL
  )
}

# =====================================================
# 4. HELD-OUT TEST SET EVALUATION
# =====================================================

stopifnot(
  exists("results"),
  exists("X_test"),
  exists("y_test"),
  length(results) > 0
)

performance_list <- list()
test_predictions <- list()

for (m in names(results)) {
  
  cat("\nEvaluating:", m, "\n")
  
  model <- results[[m]]
  
  pred <- predict(
    model,
    newdata = X_test
  )
  
  prob <- predict(
    model,
    newdata = X_test,
    type = "prob"
  )[, "Skilled"]
  
  stopifnot(
    length(pred) == length(y_test),
    length(prob) == length(y_test),
    !anyNA(pred),
    all(is.finite(prob)),
    all(prob >= 0 & prob <= 1)
  )
  
  test_predictions[[m]] <- data.frame(
    SBA = y_test,
    Predicted = pred,
    Probability = as.numeric(prob)
  )
  
  performance_list[[m]] <- bootstrap_ci(
    truth = y_test,
    pred = pred,
    prob = prob,
    B = 1000,
    seed = 123
  ) %>%
    mutate(
      Model = m,
      .before = 1
    )
}

# =====================================================
# 5. PERFORMANCE TABLE WITH 95% CI
# =====================================================

performance_long <- dplyr::bind_rows(
  performance_list
) %>%
  tibble::as_tibble()

performance_long <- performance_long %>%
  mutate(
    Result = case_when(
      is.na(Estimate) ~ "Not estimable",
      
      is.na(Lower_95CI) |
        is.na(Upper_95CI) ~
        sprintf(
          "%.3f (CI unavailable)",
          Estimate
        ),
      
      TRUE ~ sprintf(
        "%.3f (%.3f–%.3f)",
        Estimate,
        Lower_95CI,
        Upper_95CI
      )
    )
  )

performance_table <- performance_long %>%
  select(Model, Metric, Result) %>%
  tidyr::pivot_wider(
    names_from = Metric,
    values_from = Result
  )

# Main publication-style table
print(
  tibble::as_tibble(performance_table),
  n = Inf,
  width = Inf
)

# Detailed numerical results
# FIX: convert to tibble before using n = Inf
print(
  tibble::as_tibble(performance_long),
  n = Inf,
  width = Inf
)

# Alternative RStudio table viewers
# View(performance_table)
# View(performance_long)

# =====================================================
# 6. CONFUSION MATRICES
# =====================================================

confusion_matrices <- list()

for (m in names(test_predictions)) {
  
  cat("\n================================\n")
  cat("Model:", m, "\n")
  cat("================================\n")
  
  confusion_matrices[[m]] <- caret::confusionMatrix(
    data = factor(
      test_predictions[[m]]$Predicted,
      levels = c("Skilled", "Unskilled")
    ),
    reference = factor(
      test_predictions[[m]]$SBA,
      levels = c("Skilled", "Unskilled")
    ),
    positive = "Skilled"
  )
  
  print(confusion_matrices[[m]])
}

# =====================================================
# 7. CALIBRATION DATA
# =====================================================

calibration_data <- dplyr::bind_rows(
  lapply(names(test_predictions), function(m) {
    
    x <- test_predictions[[m]]
    
    data.frame(
      Model = m,
      Observed = as.numeric(
        x$SBA == "Skilled"
      ),
      Predicted_Probability = x$Probability
    )
  })
)

# =====================================================
# 8. CALIBRATION PLOTS
# =====================================================

p_calibration <- ggplot(
  calibration_data,
  aes(
    x = Predicted_Probability,
    y = Observed
  )
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "#8A94A6",
    linewidth = 0.7
  ) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    color = "#1688D4",
    linewidth = 1.2
  ) +
  facet_wrap(
    ~ Model,
    ncol = 3
  ) +
  coord_cartesian(
    xlim = c(0, 1),
    ylim = c(0, 1)
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.2)
  ) +
  labs(
    x = "Predicted Probability of Skilled SBA",
    y = "Observed SBA Proportion"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.text = element_text(
      face = "bold",
      size = 10,
      color = "#172B4D"
    ),
    axis.text = element_text(
      color = "#344054"
    ),
    axis.title = element_text(
      color = "#172B4D",
      face = "bold"
    ),
    panel.border = element_rect(
      color = "#E1E6EB",
      fill = NA,
      linewidth = 0.4
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    )
  )

print(p_calibration)

# =====================================================
# 9. QUICK PERFORMANCE SUMMARY
# =====================================================

performance_summary <- performance_long %>%
  filter(
    Metric %in% c(
      "Accuracy",
      "Balanced_Accuracy",
      "Sensitivity",
      "Specificity",
      "F1",
      "MCC",
      "AUROC",
      "Brier_Score",
      "Calibration_Intercept",
      "Calibration_Slope",
      "Youden_J"
    )
  ) %>%
  select(Model, Metric, Estimate) %>%
  pivot_wider(
    names_from = Metric,
    values_from = Estimate
  ) %>%
  arrange(desc(AUROC))

print(
  tibble::as_tibble(performance_summary),
  n = Inf,
  width = Inf
)

















#### 95% CI ####

# =====================================================
# 9. FINAL PERFORMANCE SUMMARY WITH 95% CI
# =====================================================

library(dplyr)
library(tidyr)
library(tibble)

performance_summary_CI <- performance_long %>%
  mutate(
    Result = case_when(
      is.na(Estimate) ~ "Not estimable",
      is.na(Lower_95CI) | is.na(Upper_95CI) ~
        sprintf("%.3f (CI unavailable)", Estimate),
      TRUE ~ sprintf(
        "%.3f (%.3f–%.3f)",
        Estimate,
        Lower_95CI,
        Upper_95CI
      )
    )
  ) %>%
  select(Model, Metric, Result) %>%
  pivot_wider(
    names_from = Metric,
    values_from = Result
  )

# Sort models by AUROC (highest first)
model_order <- performance_long %>%
  filter(Metric == "AUROC") %>%
  arrange(desc(Estimate)) %>%
  pull(Model)

performance_summary_CI <- performance_summary_CI %>%
  mutate(
    Model = factor(Model, levels = model_order)
  ) %>%
  arrange(Model) %>%
  mutate(Model = as.character(Model))

# Display all metrics with 95% CI
print(
  as_tibble(performance_summary_CI),
  n = Inf,
  width = Inf
)

# Optional: view full table in RStudio
print(performance_summary_CI)








#### Resulsts save ####

# =====================================================
# 10. SAVE PERFORMANCE TABLE WITH 95% CI
# =====================================================

library(openxlsx)

# Output folder
output_path <- "D:/Research/BDHS Research/Nepal/SBA/Mozambique/Manuscript/New submissions/Revision_V1/Updates Tbales"

# Create folder if it does not exist
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

# Save Excel file
openxlsx::write.xlsx(
  performance_summary_CI,
  file = file.path(
    output_path,
    "ML_Performance_95CI.xlsx"
  ),
  overwrite = TRUE
)

# Save CSV file
write.csv(
  performance_summary_CI,
  file = file.path(
    output_path,
    "ML_Performance_95CI.csv"
  ),
  row.names = FALSE
)

cat("\nFiles successfully saved to:\n", output_path, "\n")













#### Visualizations ####




# =====================================================
# FIXED PUBLICATION-QUALITY THEME
# =====================================================

theme_journal <- function(base_size = 12) {
  
  ggplot2::theme_classic(base_size = base_size) +
    
    ggplot2::theme(
      
      plot.background = ggplot2::element_rect(
        fill = "white",
        color = NA
      ),
      
      panel.background = ggplot2::element_rect(
        fill = "white",
        color = NA
      ),
      
      axis.title = ggplot2::element_text(
        size = 12,
        face = "bold",
        color = "#202124"
      ),
      
      axis.text = ggplot2::element_text(
        size = 10,
        color = "#454545"
      ),
      
      axis.line = ggplot2::element_line(
        color = "#454545",
        linewidth = 0.45
      ),
      
      axis.ticks = ggplot2::element_line(
        color = "#454545",
        linewidth = 0.35
      ),
      
      panel.grid.major.y = ggplot2::element_line(
        color = "#F0F0F0",
        linewidth = 0.3
      ),
      
      legend.title = ggplot2::element_blank(),
      
      legend.text = ggplot2::element_text(
        size = 10
      ),
      
      legend.position = "bottom",
      
      legend.key.width = grid::unit(
        1.3, "cm"
      ),
      
      strip.background = ggplot2::element_blank(),
      
      strip.text = ggplot2::element_text(
        face = "bold",
        size = 10
      ),
      
      plot.margin = ggplot2::margin(
        t = 10,
        r = 12,
        b = 10,
        l = 10,
        unit = "pt"
      )
    )
}


#### Recall ####

# =====================================================
# RECREATE CALIBRATION FIGURE
# =====================================================

library(ggplot2)

p_calibration_clean <- ggplot(
  calibration_bins,
  aes(
    x = Mean_Predicted,
    y = Observed_Rate
  )
) +
  
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "#A0A0A0",
    linewidth = 0.65
  ) +
  
  geom_errorbar(
    aes(
      ymin = CI_Lower,
      ymax = CI_Upper,
      color = Model
    ),
    width = 0.012,
    linewidth = 0.45,
    alpha = 0.65
  ) +
  
  geom_line(
    aes(
      color = Model,
      group = Model
    ),
    linewidth = 0.9
  ) +
  
  geom_point(
    aes(color = Model),
    size = 2
  ) +
  
  facet_wrap(
    ~ Model,
    ncol = 3
  ) +
  
  scale_color_manual(
    values = model_colors
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 1, 0.25)
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 1, 0.25)
  ) +
  
  coord_fixed(
    ratio = 1,
    xlim = c(0, 1),
    ylim = c(0, 1)
  ) +
  
  labs(
    x = "Mean Predicted Probability",
    y = "Observed Proportion of Skilled SBA"
  ) +
  
  theme_journal() +
  
  theme(
    legend.position = "none",
    panel.border = element_rect(
      fill = NA,
      color = "#E0E0E0",
      linewidth = 0.4
    )
  )

# Display figure
print(p_calibration_clean)








#### AUC ROC and PR_Curve ####

# ============================================================
# PUBLICATION-QUALITY ROC AND PRECISION-RECALL FIGURES
# Seven ML Models | Clean, Bright, White Background
# No AUC/AP Values | No Titles | No Automatic Saving
# ============================================================

library(ggplot2)
library(dplyr)
library(pROC)

stopifnot(
  exists("test_predictions"),
  length(test_predictions) > 0
)

# ============================================================
# 1. MODEL NAMES AND BRIGHT COLORS
# ============================================================

model_names <- names(test_predictions)

model_colors <- c(
  "Random Forest"       = "#0077BB",
  "Decision Tree"       = "#EE7733",
  "KNN"                 = "#009988",
  "Logistic Regression" = "#CC3311",
  "SVM"                 = "#AA4499",
  "XGBoost"             = "#3366CC",
  "Neural Network"      = "#DD4477"
)

model_colors <- model_colors[model_names]

stopifnot(!anyNA(model_colors))

# ============================================================
# 2. CLEAN JOURNAL THEME
# ============================================================

theme_journal_clean <- function() {
  
  ggplot2::theme_classic(base_size = 13) +
    
    ggplot2::theme(
      
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      
      panel.background = element_rect(
        fill = "white",
        color = NA
      ),
      
      axis.title = element_text(
        size = 13,
        face = "bold",
        color = "#202124"
      ),
      
      axis.text = element_text(
        size = 11,
        color = "#404040"
      ),
      
      axis.line = element_line(
        color = "#333333",
        linewidth = 0.55
      ),
      
      axis.ticks = element_line(
        color = "#333333",
        linewidth = 0.4
      ),
      
      panel.grid.major = element_line(
        color = "#EEEEEE",
        linewidth = 0.3
      ),
      
      panel.grid.minor = element_blank(),
      
      legend.position = "bottom",
      
      legend.title = element_blank(),
      
      legend.text = element_text(
        size = 10.5,
        color = "#202124"
      ),
      
      legend.key.width = grid::unit(
        1.3, "cm"
      ),
      
      legend.spacing.x = grid::unit(
        0.35, "cm"
      ),
      
      plot.margin = ggplot2::margin(
        t = 12,
        r = 15,
        b = 10,
        l = 12,
        unit = "pt"
      )
    )
}

# ============================================================
# 3. ROC CURVE DATA
# ============================================================

roc_data <- list()

for (m in model_names) {
  
  x <- test_predictions[[m]]
  
  y <- as.integer(
    as.character(x$SBA) == "Skilled"
  )
  
  prob <- as.numeric(x$Probability)
  
  roc_obj <- pROC::roc(
    response = y,
    predictor = prob,
    levels = c(0, 1),
    direction = "<",
    quiet = TRUE
  )
  
  roc_data[[m]] <- data.frame(
    Model = m,
    FPR = 1 - roc_obj$specificities,
    TPR = roc_obj$sensitivities
  )
}

roc_plot_data <- dplyr::bind_rows(roc_data)

roc_plot_data$Model <- factor(
  roc_plot_data$Model,
  levels = model_names
)

# ============================================================
# 4. CLEAN ROC FIGURE
# ============================================================

p_roc <- ggplot(
  roc_plot_data,
  aes(
    x = FPR,
    y = TPR,
    color = Model,
    group = Model
  )
) +
  
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "#B8B8B8",
    linewidth = 0.65
  ) +
  
  geom_path(
    linewidth = 1.15,
    lineend = "round"
  ) +
  
  scale_color_manual(
    values = model_colors,
    breaks = model_names
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    expand = expansion(mult = 0)
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 1, 0.2),
    expand = expansion(mult = 0)
  ) +
  
  coord_fixed(
    ratio = 1,
    xlim = c(0, 1),
    ylim = c(0, 1)
  ) +
  
  labs(
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = NULL
  ) +
  
  theme_journal_clean() +
  
  guides(
    color = guide_legend(
      ncol = 2,
      byrow = TRUE,
      override.aes = list(
        linewidth = 2
      )
    )
  )

print(p_roc)

# ============================================================
# 5. PRECISION-RECALL CURVE DATA
# ============================================================

make_pr_curve <- function(truth, probability) {
  
  y <- as.integer(
    as.character(truth) == "Skilled"
  )
  
  p <- as.numeric(probability)
  
  stopifnot(
    length(y) == length(p),
    !anyNA(y),
    all(is.finite(p)),
    sum(y == 1) > 0,
    sum(y == 0) > 0
  )
  
  # Sort descending probabilities
  ord <- order(p, decreasing = TRUE)
  
  y <- y[ord]
  p <- p[ord]
  
  # Cumulative true and false positives
  tp <- cumsum(y == 1)
  fp <- cumsum(y == 0)
  
  # Handle tied probabilities
  last_index <- c(
    which(diff(p) != 0),
    length(p)
  )
  
  tp <- tp[last_index]
  fp <- fp[last_index]
  
  precision <- tp / (tp + fp)
  recall <- tp / sum(y == 1)
  
  data.frame(
    Recall = c(0, recall),
    Precision = c(1, precision)
  )
}

pr_data <- list()

for (m in model_names) {
  
  x <- test_predictions[[m]]
  
  pr_data[[m]] <- data.frame(
    Model = m,
    make_pr_curve(
      truth = x$SBA,
      probability = x$Probability
    )
  )
}

pr_plot_data <- dplyr::bind_rows(pr_data)

pr_plot_data$Model <- factor(
  pr_plot_data$Model,
  levels = model_names
)

# Baseline = prevalence of positive class
baseline_precision <- mean(
  as.character(
    test_predictions[[1]]$SBA
  ) == "Skilled"
)

# ============================================================
# 6. CLEAN PRECISION-RECALL FIGURE
# ============================================================

p_pr <- ggplot(
  pr_plot_data,
  aes(
    x = Recall,
    y = Precision,
    color = Model,
    group = Model
  )
) +
  
  geom_hline(
    yintercept = baseline_precision,
    linetype = "dashed",
    color = "#B8B8B8",
    linewidth = 0.65
  ) +
  
  geom_step(
    direction = "vh",
    linewidth = 1.1
  ) +
  
  scale_color_manual(
    values = model_colors,
    breaks = model_names
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    expand = expansion(mult = 0)
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 1, 0.2),
    expand = expansion(mult = 0)
  ) +
  
  coord_fixed(
    ratio = 1,
    xlim = c(0, 1),
    ylim = c(0, 1)
  ) +
  
  labs(
    x = "Recall (Sensitivity)",
    y = "Precision (Positive Predictive Value)",
    color = NULL
  ) +
  
  theme_journal_clean() +
  
  guides(
    color = guide_legend(
      ncol = 2,
      byrow = TRUE,
      override.aes = list(
        linewidth = 2
      )
    )
  )

print(p_pr)

# ============================================================
# 7. DISPLAY FIGURES
# ============================================================

# ROC Curve
print(p_roc)

# Precision-Recall Curve
print(p_pr)

# No numerical tables
# No AUC or AP values in legends
# No titles or captions
# No automatic figure saving









#### De lUNG Test ####

# ============================================================
# PAIRWISE DELONG TEST FOR SEVEN ML MODELS
# AUROC Comparison | Paired Test | Holm Correction
# ============================================================

library(pROC)
library(dplyr)
library(tidyr)
library(ggplot2)

stopifnot(
  exists("test_predictions"),
  length(test_predictions) >= 2
)

# ============================================================
# 1. CREATE ROC OBJECTS
# ============================================================

model_names <- names(test_predictions)

roc_objects <- list()

reference_truth <- as.character(
  test_predictions[[1]]$SBA
)

for (m in model_names) {
  
  x <- test_predictions[[m]]
  
  truth <- as.character(x$SBA)
  prob <- as.numeric(x$Probability)
  
  # All models must use identical test observations
  # in identical order
  stopifnot(
    identical(truth, reference_truth),
    all(truth %in% c("Skilled", "Unskilled")),
    all(is.finite(prob)),
    all(prob >= 0 & prob <= 1)
  )
  
  roc_objects[[m]] <- pROC::roc(
    response = factor(
      truth,
      levels = c("Unskilled", "Skilled")
    ),
    predictor = prob,
    levels = c("Unskilled", "Skilled"),
    direction = "<",
    quiet = TRUE
  )
}

# ============================================================
# 2. ALL PAIRWISE DELONG TESTS
# ============================================================

model_pairs <- combn(
  model_names,
  2,
  simplify = FALSE
)

delong_results <- lapply(model_pairs, function(pair) {
  
  m1 <- pair[1]
  m2 <- pair[2]
  
  test <- pROC::roc.test(
    roc_objects[[m1]],
    roc_objects[[m2]],
    method = "delong",
    paired = TRUE,
    alternative = "two.sided"
  )
  
  auc1 <- as.numeric(
    pROC::auc(roc_objects[[m1]])
  )
  
  auc2 <- as.numeric(
    pROC::auc(roc_objects[[m2]])
  )
  
  data.frame(
    Model_1 = m1,
    Model_2 = m2,
    AUROC_1 = auc1,
    AUROC_2 = auc2,
    AUROC_Difference = auc1 - auc2,
    Z_Statistic = as.numeric(test$statistic),
    P_Value = as.numeric(test$p.value)
  )
})

delong_results <- dplyr::bind_rows(
  delong_results
)

# ============================================================
# 3. MULTIPLE-TESTING CORRECTION
# ============================================================

delong_results <- delong_results %>%
  mutate(
    P_Holm = p.adjust(
      P_Value,
      method = "holm"
    ),
    Significant_Holm = ifelse(
      P_Holm < 0.05,
      "Yes",
      "No"
    )
  ) %>%
  arrange(P_Holm)

# ============================================================
# 4. DISPLAY FULL RESULTS
# ============================================================

print(
  tibble::as_tibble(delong_results),
  n = Inf,
  width = Inf
)

# ============================================================
# 5. JOURNAL-READY COMPARISON TABLE
# ============================================================

delong_table <- delong_results %>%
  mutate(
    Comparison = paste(
      Model_1,
      "vs",
      Model_2
    ),
    `AUROC Difference` = sprintf(
      "%.3f",
      AUROC_Difference
    ),
    `DeLong p-value` = format.pval(
      P_Value,
      digits = 3,
      eps = 0.001
    ),
    `Holm-adjusted p-value` = format.pval(
      P_Holm,
      digits = 3,
      eps = 0.001
    )
  ) %>%
  select(
    Comparison,
    `AUROC Difference`,
    `DeLong p-value`,
    `Holm-adjusted p-value`,
    Significant_Holm
  )

print(
  tibble::as_tibble(delong_table),
  n = Inf,
  width = Inf
)

# ============================================================
# 6. PAIRWISE P-VALUE MATRIX
# ============================================================

p_matrix <- matrix(
  NA_real_,
  nrow = length(model_names),
  ncol = length(model_names),
  dimnames = list(
    model_names,
    model_names
  )
)

diag(p_matrix) <- 1

for (i in seq_len(nrow(delong_results))) {
  
  m1 <- delong_results$Model_1[i]
  m2 <- delong_results$Model_2[i]
  
  p_matrix[m1, m2] <- delong_results$P_Holm[i]
  p_matrix[m2, m1] <- delong_results$P_Holm[i]
}

print(round(p_matrix, 4))

# ============================================================
# 7. OPTIONAL HEATMAP
# ============================================================

heatmap_data <- as.data.frame(
  as.table(p_matrix)
)

names(heatmap_data) <- c(
  "Model_1",
  "Model_2",
  "Adjusted_P"
)

heatmap_data$Label <- ifelse(
  heatmap_data$Model_1 == heatmap_data$Model_2,
  "\u2014",
  ifelse(
    heatmap_data$Adjusted_P < 0.001,
    "<0.001",
    sprintf("%.3f", heatmap_data$Adjusted_P)
  )
)

p_delong_heatmap <- ggplot(
  heatmap_data,
  aes(
    x = Model_1,
    y = Model_2,
    fill = Adjusted_P
  )
) +
  geom_tile(
    color = "white",
    linewidth = 1
  ) +
  geom_text(
    aes(label = Label),
    size = 3.2,
    color = "#202124"
  ) +
  scale_fill_gradient(
    low = "#B7D9F5",
    high = "#FFFFFF",
    limits = c(0, 1),
    name = "Adjusted p"
  ) +
  coord_equal() +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    )
  )

print(p_delong_heatmap)

# No automatic file saving









# =====================================================
# SAVE DELONG TEST RESULTS
# =====================================================

library(openxlsx)

output_path <- paste0(
  "D:/Research/BDHS Research/Nepal/SBA/",
  "Mozambique/Manuscript/New submissions/",
  "Revision_V1/Updates Tbales"
)

if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

# Excel workbook with multiple sheets
openxlsx::write.xlsx(
  list(
    "DeLong_Full_Results" = delong_results,
    "Journal_Ready_Table" = delong_table,
    "Adjusted_P_Matrix" = as.data.frame(p_matrix)
  ),
  file = file.path(
    output_path,
    "DeLong_Test_Results.xlsx"
  ),
  overwrite = TRUE
)

# CSV: journal-ready comparison table
write.csv(
  delong_table,
  file = file.path(
    output_path,
    "DeLong_Journal_Table.csv"
  ),
  row.names = FALSE
)

# CSV: complete numerical results
write.csv(
  delong_results,
  file = file.path(
    output_path,
    "DeLong_Full_Results.csv"
  ),
  row.names = FALSE
)

cat("\nDeLong results saved successfully!\n")
cat("Location:", output_path, "\n")










#### Complete R code: SHAP Beeswarm, Bar, Waterfall, Dependence and LIME####


install.packages(c("shapviz", "lime", "ggplot2", "patchwork"))







# ============================================================
# XGBOOST EXPLAINABILITY ANALYSIS
# SHAP + BEESWARM + BAR + WATERFALL + DEPENDENCE + LIME
# Existing trained model | No retraining | No saving
# ============================================================

library(xgboost)
library(shapviz)
library(lime)
library(ggplot2)
library(patchwork)

set.seed(123)

# ============================================================
# 1. RETRIEVE EXISTING TRAINED XGBOOST MODEL
# ============================================================

stopifnot(
  exists("results"),
  exists("X_train"),
  exists("X_test"),
  "XGBoost" %in% names(results)
)

xgb_caret <- results[["XGBoost"]]

if (
  is.list(xgb_caret) &&
  !inherits(xgb_caret, "train") &&
  "model" %in% names(xgb_caret)
) {
  xgb_caret <- xgb_caret$model
}

stopifnot(inherits(xgb_caret, "train"))

xgb_native <- xgb_caret$finalModel

stopifnot(inherits(xgb_native, "xgb.Booster"))

# ============================================================
# 2. PREPARE TRAIN AND TEST FEATURES
# ============================================================

train_x <- as.data.frame(X_train)
test_x  <- as.data.frame(X_test)

stopifnot(
  nrow(train_x) > 0,
  nrow(test_x) > 0,
  identical(colnames(train_x), colnames(test_x)),
  all(vapply(train_x, is.numeric, logical(1))),
  all(vapply(test_x, is.numeric, logical(1))),
  !anyNA(train_x),
  !anyNA(test_x),
  all(is.finite(as.matrix(train_x))),
  all(is.finite(as.matrix(test_x)))
)

# Verify the feature names expected by the trained booster.
booster_features <- xgb_native$feature_names

if (!is.null(booster_features)) {
  stopifnot(
    identical(booster_features, colnames(test_x))
  )
}

# ============================================================
# 3. SELECT TEST OBSERVATIONS FOR SHAP
# ============================================================

set.seed(123)

shap_ids <- sample(
  seq_len(nrow(test_x)),
  size = min(500, nrow(test_x))
)

X_shap <- as.matrix(
  test_x[shap_ids, , drop = FALSE]
)

storage.mode(X_shap) <- "double"

# ============================================================
# 4. COMPUTE SHAP VALUES
# ============================================================

shap_values <- shapviz::shapviz(
  xgb_native,
  X_pred = X_shap,
  X = as.data.frame(X_shap)
)

# ============================================================
# 5. CLEAN JOURNAL THEME
# ============================================================

theme_shap <- function() {
  
  ggplot2::theme_classic(base_size = 12) +
    
    ggplot2::theme(
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      
      panel.background = element_rect(
        fill = "white",
        color = NA
      ),
      
      axis.title = element_text(
        size = 12,
        face = "bold",
        color = "#202124"
      ),
      
      axis.text = element_text(
        size = 10,
        color = "#303030"
      ),
      
      axis.line = element_line(
        color = "#555555",
        linewidth = 0.4
      ),
      
      axis.ticks = element_line(
        color = "#555555",
        linewidth = 0.4
      ),
      
      panel.grid.major.x = element_line(
        color = "#EFEFEF",
        linewidth = 0.3
      ),
      
      panel.grid.minor = element_blank(),
      
      plot.title = element_blank(),
      
      legend.title = element_text(
        size = 10
      ),
      
      legend.text = element_text(
        size = 9
      ),
      
      plot.margin = ggplot2::margin(
        t = 10,
        r = 12,
        b = 10,
        l = 10,
        unit = "pt"
      )
    )
}

# ============================================================
# 6. SHAP BEESWARM
# ============================================================

# Use shapviz's built-in colour scale.
# This avoids the duplicate colour-scale warning.

p_shap_bee <- shapviz::sv_importance(
  shap_values,
  kind = "bee",
  max_display = 15,
  alpha = 0.75
) +
  
  labs(
    x = "SHAP value (log-odds)",
    y = NULL,
    title = NULL
  ) +
  
  theme_shap()

print(p_shap_bee)

# ============================================================
# 7. GLOBAL SHAP FEATURE IMPORTANCE
# ============================================================

p_shap_bar <- shapviz::sv_importance(
  shap_values,
  kind = "bar",
  max_display = 15,
  fill = "#1687D9"
) +
  
  labs(
    x = "Mean absolute SHAP value (log-odds)",
    y = NULL,
    title = NULL
  ) +
  
  theme_shap()

print(p_shap_bar)

# ============================================================
# 8. COMBINED GLOBAL SHAP FIGURE
# ============================================================

p_shap_combined <- (
  p_shap_bar | p_shap_bee
) +
  
  patchwork::plot_layout(
    widths = c(1, 1.3)
  )

print(p_shap_combined)

# ============================================================
# 9. SHAP WATERFALL
# ============================================================

# This is an illustrative test observation.

case_id <- 1

p_waterfall <- shapviz::sv_waterfall(
  shap_values,
  row_id = case_id,
  max_display = 12
) +
  
  labs(
    title = NULL
  ) +
  
  theme_shap()

print(p_waterfall)

# ============================================================
# 10. SHAP DEPENDENCE
# ============================================================

shap_matrix <- shap_values$S

mean_abs_shap <- colMeans(
  abs(shap_matrix)
)

top_feature <- names(
  sort(
    mean_abs_shap,
    decreasing = TRUE
  )
)[1]

p_dependence <- shapviz::sv_dependence(
  shap_values,
  v = top_feature,
  color_var = "auto",
  alpha = 0.65,
  size = 1.5
) +
  
  labs(
    title = NULL,
    y = "SHAP value (log-odds)"
  ) +
  
  theme_shap()

print(p_dependence)

# ============================================================
# 11. LIME MODEL WRAPPER
# ============================================================

# Use a separate wrapper class to avoid overriding
# prediction methods for other caret models.

lime_xgb_model <- structure(
  list(
    fitted = xgb_caret,
    feature_names = colnames(train_x)
  ),
  class = "lime_xgb_wrapper"
)

predict_model.lime_xgb_wrapper <- function(
    x, newdata, type, ...) {
  
  newdata <- as.data.frame(newdata)
  
  missing_features <- setdiff(
    x$feature_names,
    colnames(newdata)
  )
  
  if (length(missing_features) > 0) {
    stop("LIME prediction data is missing model features.")
  }
  
  newdata <- newdata[
    ,
    x$feature_names,
    drop = FALSE
  ]
  
  probs <- predict(
    x$fitted,
    newdata = newdata,
    type = "prob"
  )
  
  if (!all(
    c("Unskilled", "Skilled") %in% names(probs)
  )) {
    stop(
      "Expected probability columns Unskilled and Skilled."
    )
  }
  
  probs <- as.data.frame(
    probs[, c("Unskilled", "Skilled"), drop = FALSE]
  )
  
  if (
    anyNA(probs) ||
    !all(is.finite(as.matrix(probs)))
  ) {
    stop("Non-finite probabilities in LIME predictions.")
  }
  
  probs
}

model_type.lime_xgb_wrapper <- function(x, ...) {
  "classification"
}

# ============================================================
# 12. CHECK LIME PREDICTIONS
# ============================================================

check_cases <- test_x[
  seq_len(min(10, nrow(test_x))),
  ,
  drop = FALSE
]

check_prob <- predict_model.lime_xgb_wrapper(
  lime_xgb_model,
  check_cases,
  type = "prob"
)

stopifnot(
  all(as.matrix(check_prob) >= 0),
  all(as.matrix(check_prob) <= 1)
)

# Compare LIME wrapper predictions with native booster.
# They should agree if preprocessing and model match.

native_check <- as.numeric(
  predict(
    xgb_native,
    as.matrix(check_cases)
  )
)

caret_check <- check_prob$Skilled

if (!isTRUE(all.equal(
  native_check,
  caret_check,
  tolerance = 1e-5,
  check.attributes = FALSE
))) {
  stop(
    "Native booster and caret predictions differ. ",
    "Check preprocessing and positive-class mapping."
  )
}

# ============================================================
# 13. PREPARE LIME BACKGROUND DATA
# ============================================================

# Use a reproducible subset of training predictors
# to reduce computation time.

set.seed(123)

background_ids <- sample(
  seq_len(nrow(train_x)),
  size = min(2000, nrow(train_x))
)

lime_background <- train_x[
  background_ids,
  ,
  drop = FALSE
]

# ============================================================
# 14. CREATE LIME EXPLAINER
# ============================================================

# Disable continuous-variable binning to help
# preserve variation in local perturbations.

set.seed(123)

lime_explainer <- lime::lime(
  x = lime_background,
  model = lime_xgb_model,
  bin_continuous = FALSE
)

# ============================================================
# 15. CHOOSE TEST CASES
# ============================================================

test_probs <- predict_model.lime_xgb_wrapper(
  lime_xgb_model,
  test_x,
  type = "prob"
)$Skilled

# Prioritize non-extreme probabilities.

candidate_ids <- which(
  test_probs > 0.15 &
    test_probs < 0.85
)

if (length(candidate_ids) == 0) {
  candidate_ids <- order(
    abs(test_probs - 0.5)
  )
}

selected_ids <- head(
  candidate_ids,
  5
)

# ============================================================
# 16. ROBUST LIME EXPLANATIONS
# ============================================================

# Some cases may fail when local predicted probabilities
# have insufficient variation.
#
# Try multiple kernel widths and retain only
# successfully computed explanations.

lime_results <- list()

kernel_options <- c(3, 5, 10)

for (i in selected_ids) {
  
  one_case <- test_x[
    i,
    ,
    drop = FALSE
  ]
  
  case_result <- NULL
  
  for (kw in kernel_options) {
    
    attempt <- tryCatch(
      
      lime::explain(
        x = one_case,
        explainer = lime_explainer,
        labels = "Skilled",
        n_features = 8,
        n_permutations = 5000,
        kernel_width = kw,
        feature_select = "highest_weights"
      ),
      
      error = function(e) NULL
    )
    
    if (!is.null(attempt) &&
        nrow(attempt) > 0) {
      
      case_result <- attempt
      break
    }
  }
  
  if (!is.null(case_result)) {
    
    case_result$case <- paste0(
      "Case ",
      i
    )
    
    lime_results[[length(lime_results) + 1]] <-
      case_result
  }
}

# ============================================================
# 17. COMBINE VALID LIME RESULTS
# ============================================================

if (length(lime_results) > 0) {
  
  lime_explanation <- do.call(
    rbind,
    lime_results
  )
  
  # ==========================================================
  # 18. CLEAN LIME FIGURE
  # ==========================================================
  
  p_lime <- lime::plot_features(
    lime_explanation
  ) +
    
    labs(
      title = NULL,
      x = NULL,
      y = NULL
    ) +
    
    theme_shap() +
    
    theme(
      legend.position = "none",
      
      strip.background = element_rect(
        fill = "#F5F8FC",
        color = "#E2E8F0"
      ),
      
      strip.text = element_text(
        size = 9,
        face = "bold"
      ),
      
      axis.text.y = element_text(
        size = 8
      ),
      
      panel.grid.major.y = element_blank()
    )
  
  print(p_lime)
  
} else {
  
  lime_explanation <- NULL
  p_lime <- NULL
  
  message(
    "LIME explanations could not be computed ",
    "for the selected observations. ",
    "SHAP figures remain available."
  )
}

# ============================================================
# 19. CHECK LIME LOCAL FIDELITY
# ============================================================

if (!is.null(lime_explanation)) {
  
  fidelity_cols <- intersect(
    c(
      "case",
      "label",
      "model_r2",
      "model_prediction",
      "model_intercept"
    ),
    names(lime_explanation)
  )
  
  if (length(fidelity_cols) > 0) {
    
    lime_fidelity <- unique(
      lime_explanation[
        ,
        fidelity_cols,
        drop = FALSE
      ]
    )
    
    # Stored for inspection; not printed automatically.
  }
}

# ============================================================
# CREATED OBJECTS
# ============================================================

# p_shap_bee
# p_shap_bar
# p_shap_combined
# p_waterfall
# p_dependence
# p_lime
# shap_values
# lime_explanation
# lime_fidelity (when available)

# No ggsave()
# No retraining
# No automatic saving

















# ============================================================
# PREMIUM JOURNAL-STYLE XGBOOST EXPLAINABILITY FIGURES
# SHAP + BEESWARM + BAR + WATERFALL + DEPENDENCE + LIME
#
# Visualization update only
# No retraining | No SHAP recalculation | No saving
# No figure titles
# ============================================================

library(ggplot2)
library(shapviz)
library(lime)
library(patchwork)

stopifnot(exists("shap_values"))

# ============================================================
# 1. PREMIUM COLOUR PALETTE
# ============================================================

COL_BLUE      <- "#1976D2"
COL_SKY       <- "#40B4E5"
COL_NAVY      <- "#174A7E"
COL_CORAL     <- "#F06478"
COL_PINK      <- "#E74781"
COL_TEAL      <- "#17A398"
COL_PURPLE    <- "#8065CF"
COL_TEXT      <- "#243247"
COL_MUTED     <- "#65748B"
COL_GRID      <- "#EDF2F7"
COL_BORDER    <- "#DFE7EF"
COL_WHITE     <- "#FFFFFF"

# ============================================================
# 2. UNIVERSAL PREMIUM JOURNAL THEME
# ============================================================

theme_premium <- function(base_size = 12) {
  
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = "sans"
  ) +
    
    ggplot2::theme(
      
      plot.background = element_rect(
        fill = COL_WHITE,
        colour = NA
      ),
      
      panel.background = element_rect(
        fill = COL_WHITE,
        colour = NA
      ),
      
      panel.border = element_blank(),
      
      panel.grid.major.x = element_line(
        colour = COL_GRID,
        linewidth = 0.35
      ),
      
      panel.grid.major.y = element_blank(),
      
      panel.grid.minor = element_blank(),
      
      axis.title.x = element_text(
        size = 12,
        face = "bold",
        colour = COL_TEXT,
        margin = ggplot2::margin(t = 10)
      ),
      
      axis.title.y = element_text(
        size = 12,
        face = "bold",
        colour = COL_TEXT,
        margin = ggplot2::margin(r = 10)
      ),
      
      axis.text.x = element_text(
        size = 10,
        colour = COL_MUTED
      ),
      
      axis.text.y = element_text(
        size = 10.5,
        colour = COL_TEXT
      ),
      
      axis.ticks = element_blank(),
      
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      
      legend.position = "right",
      
      legend.title = element_text(
        size = 10,
        face = "bold",
        colour = COL_TEXT
      ),
      
      legend.text = element_text(
        size = 9.5,
        colour = COL_MUTED
      ),
      
      legend.key = element_rect(
        fill = COL_WHITE,
        colour = NA
      ),
      
      strip.background = element_rect(
        fill = "#F3F7FC",
        colour = COL_BORDER,
        linewidth = 0.5
      ),
      
      strip.text = element_text(
        size = 9.5,
        face = "bold",
        colour = COL_NAVY
      ),
      
      plot.margin = ggplot2::margin(
        t = 14,
        r = 18,
        b = 14,
        l = 14,
        unit = "pt"
      )
    )
}

# ============================================================
# 3. SHAP BEESWARM
# ============================================================

# shapviz already supplies a continuous colour scale.
# Use its built-in scale to avoid duplicate-scale warnings.

p_shap_bee <- shapviz::sv_importance(
  shap_values,
  kind = "bee",
  max_display = 15,
  alpha = 0.72
) +
  
  ggplot2::geom_vline(
    xintercept = 0,
    colour = "#9BAABD",
    linewidth = 0.55,
    linetype = "dashed"
  ) +
  
  ggplot2::labs(
    x = "SHAP value (log-odds)",
    y = NULL,
    title = NULL
  ) +
  
  theme_premium() +
  
  ggplot2::theme(
    legend.position = "right",
    axis.text.y = element_text(
      size = 10.5,
      colour = COL_TEXT
    )
  )

print(p_shap_bee)

# ============================================================
# 4. GLOBAL SHAP BAR PLOT
# ============================================================

p_shap_bar <- shapviz::sv_importance(
  shap_values,
  kind = "bar",
  max_display = 15,
  fill = COL_BLUE
) +
  
  ggplot2::labs(
    x = "Mean absolute SHAP value (log-odds)",
    y = NULL,
    title = NULL
  ) +
  
  theme_premium() +
  
  ggplot2::theme(
    legend.position = "none",
    axis.text.y = element_text(
      size = 10.5,
      colour = COL_TEXT
    )
  )

print(p_shap_bar)

# ============================================================
# 5. COMBINED GLOBAL SHAP FIGURE
# ============================================================

# More horizontal space for the beeswarm and its legend.

p_shap_combined <- (
  p_shap_bar | p_shap_bee
) +
  
  patchwork::plot_layout(
    widths = c(1, 1.45)
  ) &
  
  ggplot2::theme(
    plot.margin = ggplot2::margin(
      12, 14, 12, 14,
      unit = "pt"
    )
  )

print(p_shap_combined)

# ============================================================
# 6. SHAP WATERFALL
# ============================================================

# Keep the same previously selected observation.
if (!exists("case_id")) {
  case_id <- 1
}

p_waterfall <- shapviz::sv_waterfall(
  shap_values,
  row_id = case_id,
  max_display = 12,
  fill_colors = c(
    COL_BLUE,
    COL_CORAL
  )
) +
  
  ggplot2::labs(
    title = NULL,
    subtitle = NULL,
    caption = NULL
  ) +
  
  theme_premium() +
  
  ggplot2::theme(
    panel.grid.major.x = element_line(
      colour = COL_GRID,
      linewidth = 0.3
    ),
    legend.position = "none"
  )

print(p_waterfall)

# ============================================================
# 7. SHAP DEPENDENCE
# ============================================================

# Automatically select the most influential feature.

shap_matrix <- shap_values$S

mean_abs_shap <- colMeans(
  abs(shap_matrix)
)

top_feature <- names(
  sort(
    mean_abs_shap,
    decreasing = TRUE
  )
)[1]

p_dependence <- shapviz::sv_dependence(
  shap_values,
  v = top_feature,
  color_var = "auto",
  alpha = 0.68,
  size = 1.8
) +
  
  ggplot2::labs(
    x = top_feature,
    y = "SHAP value (log-odds)",
    title = NULL
  ) +
  
  theme_premium() +
  
  ggplot2::theme(
    panel.grid.major.y = element_line(
      colour = COL_GRID,
      linewidth = 0.3
    )
  )

print(p_dependence)

# ============================================================
# 8. LIME LOCAL EXPLANATION
# ============================================================

if (
  exists("lime_explanation") &&
  !is.null(lime_explanation) &&
  nrow(lime_explanation) > 0
) {
  
  p_lime <- lime::plot_features(
    lime_explanation
  ) +
    
    ggplot2::labs(
      title = NULL,
      subtitle = NULL,
      x = "Local feature contribution",
      y = NULL
    ) +
    
    theme_premium(base_size = 11) +
    
    ggplot2::theme(
      
      legend.position = "none",
      
      strip.background = element_rect(
        fill = "#F1F6FD",
        colour = COL_BORDER,
        linewidth = 0.55
      ),
      
      strip.text = element_text(
        size = 9,
        face = "bold",
        colour = COL_NAVY
      ),
      
      axis.text.y = element_text(
        size = 8.5,
        colour = COL_TEXT
      ),
      
      axis.text.x = element_text(
        size = 9,
        colour = COL_MUTED
      ),
      
      panel.grid.major.y = element_blank(),
      
      panel.spacing = grid::unit(
        1.2,
        "lines"
      )
    )
  
  print(p_lime)
  
} else {
  
  p_lime <- NULL
  
  message(
    "No successful LIME explanation available."
  )
}

# ============================================================
# 9. FIGURE OBJECTS
# ============================================================

# p_shap_bee
# p_shap_bar
# p_shap_combined
# p_waterfall
# p_dependence
# p_lime

# All figures remain available in RStudio.
# Export manually from the Plots pane.
#
# No ggsave()
# No model retraining
# No automatic file saving






# ============================================================
# XGBOOST EXPLAINABILITY — FINAL CLEAN FIGURES
# SHAP + LIME + WATERFALL + DEPENDENCE
# Smaller LIME fonts | Wrapped variable labels
# No retraining | No automatic saving
# ============================================================

library(ggplot2)
library(shapviz)
library(lime)
library(patchwork)
library(grid)

stopifnot(exists("shap_values"))

# ============================================================
# 1. COLOUR PALETTE
# ============================================================

COL_BLUE   <- "#1976D2"
COL_CORAL  <- "#F06478"
COL_NAVY   <- "#174A7E"
COL_TEXT   <- "#243247"
COL_MUTED  <- "#65748B"
COL_GRID   <- "#EDF2F7"
COL_BORDER <- "#DFE7EF"

# ============================================================
# 2. CLEAN JOURNAL THEME
# ============================================================

theme_premium <- function(base_size = 11) {
  
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = "sans"
  ) +
    
    ggplot2::theme(
      
      plot.background = element_rect(
        fill = "white",
        colour = NA
      ),
      
      panel.background = element_rect(
        fill = "white",
        colour = NA
      ),
      
      panel.grid.major.x = element_line(
        colour = COL_GRID,
        linewidth = 0.3
      ),
      
      panel.grid.major.y = element_blank(),
      
      panel.grid.minor = element_blank(),
      
      axis.title = element_text(
        size = 10,
        face = "bold",
        colour = COL_TEXT
      ),
      
      axis.text.x = element_text(
        size = 9,
        colour = COL_MUTED
      ),
      
      axis.text.y = element_text(
        size = 9,
        colour = COL_TEXT
      ),
      
      axis.ticks = element_blank(),
      
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      
      legend.title = element_text(
        size = 9,
        face = "bold"
      ),
      
      legend.text = element_text(
        size = 8
      ),
      
      strip.background = element_rect(
        fill = "#F3F7FC",
        colour = COL_BORDER,
        linewidth = 0.4
      ),
      
      strip.text = element_text(
        size = 8,
        face = "bold",
        colour = COL_NAVY
      ),
      
      plot.margin = ggplot2::margin(
        t = 10,
        r = 12,
        b = 10,
        l = 12,
        unit = "pt"
      )
    )
}

# ============================================================
# 3. SHAP BEESWARM
# ============================================================

p_shap_bee <- shapviz::sv_importance(
  shap_values,
  kind = "bee",
  max_display = 15,
  alpha = 0.7
) +
  
  labs(
    x = "SHAP value (log-odds)",
    y = NULL,
    title = NULL
  ) +
  
  theme_premium(11) +
  
  theme(
    axis.text.y = element_text(
      size = 9
    )
  )

print(p_shap_bee)

# ============================================================
# 4. GLOBAL SHAP BAR
# ============================================================

p_shap_bar <- shapviz::sv_importance(
  shap_values,
  kind = "bar",
  max_display = 15,
  fill = COL_BLUE
) +
  
  labs(
    x = "Mean absolute SHAP value (log-odds)",
    y = NULL,
    title = NULL
  ) +
  
  theme_premium(11)

print(p_shap_bar)

# ============================================================
# 5. COMBINED SHAP FIGURE
# ============================================================

p_shap_combined <- (
  p_shap_bar | p_shap_bee
) +
  
  patchwork::plot_layout(
    widths = c(1, 1.5)
  )

print(p_shap_combined)

# ============================================================
# 6. SHAP WATERFALL
# ============================================================

if (!exists("case_id")) {
  case_id <- 1
}

p_waterfall <- shapviz::sv_waterfall(
  shap_values,
  row_id = case_id,
  max_display = 12
) +
  
  labs(
    title = NULL,
    subtitle = NULL
  ) +
  
  theme_premium(11)

print(p_waterfall)

# ============================================================
# 7. SHAP DEPENDENCE
# ============================================================

mean_abs_shap <- colMeans(
  abs(shap_values$S)
)

top_feature <- names(
  sort(
    mean_abs_shap,
    decreasing = TRUE
  )
)[1]

p_dependence <- shapviz::sv_dependence(
  shap_values,
  v = top_feature,
  color_var = "auto",
  alpha = 0.65,
  size = 1.6
) +
  
  labs(
    title = NULL,
    y = "SHAP value (log-odds)"
  ) +
  
  theme_premium(11) +
  
  theme(
    panel.grid.major.y = element_line(
      colour = COL_GRID,
      linewidth = 0.3
    )
  )

print(p_dependence)

# ============================================================
# 8. LIME — PREPARE LABELS
# ============================================================

if (
  exists("lime_explanation") &&
  !is.null(lime_explanation) &&
  nrow(lime_explanation) > 0
) {
  
  # Make a plotting copy; original results remain unchanged.
  
  lime_plot_data <- lime_explanation
  
  # Wrap long feature descriptions at approximately
  # 22 characters for better readability.
  
  if ("feature_desc" %in% names(lime_plot_data)) {
    
    lime_plot_data$feature_desc <- vapply(
      as.character(lime_plot_data$feature_desc),
      function(z) {
        paste(
          strwrap(z, width = 22),
          collapse = "\n"
        )
      },
      character(1)
    )
  }
  
  # Shorten displayed case labels.
  
  if ("case" %in% names(lime_plot_data)) {
    
    lime_plot_data$case <- as.character(
      lime_plot_data$case
    )
  }
  
  # ==========================================================
  # 9. LIME — CLEAN SMALL-FONT FIGURE
  # ==========================================================
  
  p_lime <- lime::plot_features(
    lime_plot_data
  ) +
    
    labs(
      title = NULL,
      subtitle = NULL,
      x = "Local feature contribution",
      y = NULL
    ) +
    
    theme_premium(base_size = 9) +
    
    theme(
      
      # Smaller feature names
      axis.text.y = element_text(
        size = 6.2,
        colour = COL_TEXT,
        lineheight = 0.9,
        margin = ggplot2::margin(r = 3)
      ),
      
      # Smaller x-axis numbers
      axis.text.x = element_text(
        size = 7,
        colour = COL_MUTED
      ),
      
      # Smaller axis title
      axis.title.x = element_text(
        size = 8,
        face = "bold",
        colour = COL_TEXT,
        margin = ggplot2::margin(t = 7)
      ),
      
      # Compact case headings
      strip.text = element_text(
        size = 7,
        face = "bold",
        colour = COL_NAVY
      ),
      
      strip.background = element_rect(
        fill = "#F2F7FD",
        colour = COL_BORDER,
        linewidth = 0.4
      ),
      
      # More space between case panels
      panel.spacing.x = grid::unit(
        1.5,
        "lines"
      ),
      
      panel.spacing.y = grid::unit(
        2,
        "lines"
      ),
      
      panel.grid.major.y = element_blank(),
      
      panel.grid.minor = element_blank(),
      
      legend.position = "none",
      
      plot.margin = ggplot2::margin(
        t = 12,
        r = 18,
        b = 12,
        l = 18,
        unit = "pt"
      )
    )
  
  # Keep no more than 2 case panels per row
  # when the underlying LIME plot supports facets.
  
  if (inherits(p_lime$facet, "FacetWrap")) {
    
    p_lime <- p_lime +
      facet_wrap(
        ~ case,
        ncol = 2,
        scales = "free_y"
      )
  }
  
  print(p_lime)
  
} else {
  
  p_lime <- NULL
  
  message(
    "LIME explanation not available."
  )
}

# ============================================================
# 10. OPTIONAL: EVEN SMALLER LIME LABELS
# ============================================================

# If variable names still overlap, run:

if (!is.null(p_lime)) {
  
  p_lime_small <- p_lime +
    
    theme(
      axis.text.y = element_text(
        size = 5.2,
        lineheight = 0.85
      ),
      
      strip.text = element_text(
        size = 6.5
      ),
      
      axis.text.x = element_text(
        size = 6.5
      )
    )
  
  print(p_lime_small)
}

# ============================================================
# FIGURE OBJECTS
# ============================================================

# p_shap_bee
# p_shap_bar
# p_shap_combined
# p_waterfall
# p_dependence
# p_lime
# p_lime_small

# No retraining
# No automatic saving
# No figure titles
# ============================================================











#### Part A — Bootstrap 95% CI for all 7 models####


# ============================================================
# PART A: BOOTSTRAP 95% CONFIDENCE INTERVALS
# MOZAMBIQUE SBA — SEVEN MACHINE LEARNING MODELS
#
# FIXED PRINT ERROR
# PAIRED BOOTSTRAP RESAMPLING
# PUBLICATION-QUALITY FIGURES
#
# No retraining | No automatic saving
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(pROC)
library(PRROC)

set.seed(123)

# ============================================================
# 1. CHECK EXISTING PREDICTIONS
# ============================================================

stopifnot(
  exists("test_predictions"),
  is.list(test_predictions),
  length(test_predictions) > 0
)

model_names <- names(test_predictions)

required_cols <- c(
  "SBA",
  "Probability"
)

for (model in model_names) {
  
  dat <- test_predictions[[model]]
  
  stopifnot(
    all(required_cols %in% names(dat)),
    nrow(dat) > 0,
    all(as.character(dat$SBA) %in%
          c("Skilled", "Unskilled")),
    all(is.finite(as.numeric(dat$Probability))),
    all(as.numeric(dat$Probability) >= 0),
    all(as.numeric(dat$Probability) <= 1)
  )
}

cat(
  "\nModels detected:",
  length(model_names),
  "\n"
)

print(model_names)

# ============================================================
# 2. SETTINGS
# ============================================================

B <- 2000
threshold <- 0.50

metric_names <- c(
  "AUROC",
  "AUPRC",
  "Accuracy",
  "Sensitivity",
  "Specificity",
  "F1",
  "Brier"
)

# ============================================================
# 3. VERIFY PAIRED TEST OBSERVATIONS
# ============================================================

reference_actual <- as.character(
  test_predictions[[1]]$SBA
)

n_test <- length(reference_actual)

for (model in model_names) {
  
  current_actual <- as.character(
    test_predictions[[model]]$SBA
  )
  
  if (
    length(current_actual) != n_test ||
    !identical(current_actual, reference_actual)
  ) {
    stop(
      "Test observations differ across models. ",
      "Paired bootstrap requires identical row order."
    )
  }
}

# IMPORTANT:
# Matching SBA labels alone cannot prove row identity.
# Confirm all prediction objects retain the same test-row order.

# ============================================================
# 4. PERFORMANCE METRIC FUNCTION
# ============================================================

calculate_metrics <- function(
    actual,
    probability,
    threshold = 0.50
) {
  
  actual <- as.character(actual)
  probability <- as.numeric(probability)
  
  y <- as.integer(
    actual == "Skilled"
  )
  
  predicted <- ifelse(
    probability >= threshold,
    "Skilled",
    "Unskilled"
  )
  
  tp <- sum(
    predicted == "Skilled" &
      actual == "Skilled"
  )
  
  tn <- sum(
    predicted == "Unskilled" &
      actual == "Unskilled"
  )
  
  fp <- sum(
    predicted == "Skilled" &
      actual == "Unskilled"
  )
  
  fn <- sum(
    predicted == "Unskilled" &
      actual == "Skilled"
  )
  
  safe_div <- function(a, b) {
    if (b == 0) {
      return(NA_real_)
    }
    a / b
  }
  
  sensitivity <- safe_div(
    tp,
    tp + fn
  )
  
  specificity <- safe_div(
    tn,
    tn + fp
  )
  
  precision <- safe_div(
    tp,
    tp + fp
  )
  
  f1 <- if (
    is.na(precision) ||
    is.na(sensitivity) ||
    precision + sensitivity == 0
  ) {
    
    NA_real_
    
  } else {
    
    2 * precision * sensitivity /
      (precision + sensitivity)
  }
  
  accuracy <- mean(
    predicted == actual
  )
  
  brier <- mean(
    (probability - y)^2
  )
  
  if (length(unique(actual)) < 2) {
    
    auroc <- NA_real_
    auprc <- NA_real_
    
  } else {
    
    roc_obj <- pROC::roc(
      response = actual,
      predictor = probability,
      levels = c(
        "Unskilled",
        "Skilled"
      ),
      direction = "<",
      quiet = TRUE
    )
    
    auroc <- as.numeric(
      pROC::auc(roc_obj)
    )
    
    auprc <- PRROC::pr.curve(
      scores.class0 = probability[y == 1],
      scores.class1 = probability[y == 0],
      curve = FALSE
    )$auc.integral
  }
  
  c(
    AUROC = auroc,
    AUPRC = auprc,
    Accuracy = accuracy,
    Sensitivity = sensitivity,
    Specificity = specificity,
    F1 = f1,
    Brier = brier
  )
}

# ============================================================
# 5. GENERATE SHARED BOOTSTRAP SAMPLES
# ============================================================

# Shared indices preserve paired comparisons.

set.seed(123)

bootstrap_indices <- replicate(
  B,
  sample.int(
    n_test,
    size = n_test,
    replace = TRUE
  ),
  simplify = FALSE
)

cat(
  "\nBootstrap samples generated:",
  B,
  "\n"
)

# ============================================================
# 6. RUN BOOTSTRAP FOR ALL MODELS
# ============================================================

bootstrap_results <- list()

bootstrap_distributions <- list()

for (model in model_names) {
  
  cat(
    "\nProcessing:",
    model,
    "\n"
  )
  
  dat <- test_predictions[[model]]
  
  actual <- as.character(
    dat$SBA
  )
  
  probability <- as.numeric(
    dat$Probability
  )
  
  point_estimates <- calculate_metrics(
    actual,
    probability,
    threshold
  )
  
  boot_matrix <- matrix(
    NA_real_,
    nrow = B,
    ncol = length(metric_names),
    dimnames = list(
      NULL,
      metric_names
    )
  )
  
  for (b in seq_len(B)) {
    
    ids <- bootstrap_indices[[b]]
    
    boot_matrix[b, ] <- calculate_metrics(
      actual[ids],
      probability[ids],
      threshold
    )
  }
  
  bootstrap_distributions[[model]] <-
    as.data.frame(boot_matrix)
  
  ci_list <- lapply(
    metric_names,
    function(metric) {
      
      values <- boot_matrix[, metric]
      
      values <- values[
        is.finite(values)
      ]
      
      if (length(values) == 0) {
        
        lower <- NA_real_
        upper <- NA_real_
        
      } else {
        
        ci <- stats::quantile(
          values,
          probs = c(
            0.025,
            0.975
          ),
          na.rm = TRUE,
          names = FALSE
        )
        
        lower <- ci[1]
        upper <- ci[2]
      }
      
      data.frame(
        Model = model,
        Metric = metric,
        Estimate = unname(
          point_estimates[metric]
        ),
        CI_Lower = lower,
        CI_Upper = upper,
        Valid_Bootstraps = length(values),
        stringsAsFactors = FALSE
      )
    }
  )
  
  bootstrap_results[[model]] <-
    dplyr::bind_rows(ci_list)
}

cat(
  "\nAll bootstrap analyses completed successfully.\n"
)

# ============================================================
# 7. CREATE FINAL RESULTS TABLE
# ============================================================

bootstrap_ci_table <- dplyr::bind_rows(
  bootstrap_results
)

bootstrap_ci_table <- bootstrap_ci_table %>%
  mutate(
    Estimate = round(
      Estimate,
      4
    ),
    CI_Lower = round(
      CI_Lower,
      4
    ),
    CI_Upper = round(
      CI_Upper,
      4
    ),
    `Estimate (95% CI)` = sprintf(
      "%.3f (%.3f–%.3f)",
      Estimate,
      CI_Lower,
      CI_Upper
    )
  )

# ============================================================
# 8. FIXED RESULTS PRINTING
# ============================================================

cat(
  "\n====================================\n",
  "BOOTSTRAP 95% CONFIDENCE INTERVALS\n",
  "====================================\n"
)

# FIX:
# Convert to tibble before using n = Inf.

print(
  tibble::as_tibble(
    bootstrap_ci_table
  ),
  n = Inf,
  width = Inf
)

# Alternative base R display:
# print(as.data.frame(bootstrap_ci_table))

# ============================================================
# 9. CREATE WIDE PUBLICATION TABLE
# ============================================================

bootstrap_publication_table <-
  bootstrap_ci_table %>%
  
  select(
    Model,
    Metric,
    `Estimate (95% CI)`
  ) %>%
  
  tidyr::pivot_wider(
    names_from = Metric,
    values_from = `Estimate (95% CI)`
  )

cat(
  "\nPUBLICATION-READY RESULTS TABLE\n"
)

print(
  tibble::as_tibble(
    bootstrap_publication_table
  ),
  n = Inf,
  width = Inf
)

# ============================================================
# 10. FIGURE THEME
# ============================================================

theme_bootstrap <- function() {
  
  ggplot2::theme_minimal(
    base_size = 11
  ) +
    
    ggplot2::theme(
      
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      
      panel.background = element_rect(
        fill = "white",
        color = NA
      ),
      
      panel.grid.major.x = element_line(
        color = "#EDF2F7",
        linewidth = 0.35
      ),
      
      panel.grid.major.y = element_blank(),
      
      panel.grid.minor = element_blank(),
      
      axis.title = element_text(
        size = 11,
        face = "bold",
        color = "#243247"
      ),
      
      axis.text.x = element_text(
        size = 9,
        color = "#65748B"
      ),
      
      axis.text.y = element_text(
        size = 10,
        color = "#243247"
      ),
      
      strip.background = element_rect(
        fill = "#F2F7FD",
        color = "#E1EAF3"
      ),
      
      strip.text = element_text(
        size = 9,
        face = "bold",
        color = "#174A7E"
      ),
      
      plot.title = element_blank(),
      
      plot.subtitle = element_blank(),
      
      legend.position = "none",
      
      plot.margin = ggplot2::margin(
        12,
        16,
        12,
        12,
        unit = "pt"
      )
    )
}

# ============================================================
# 11. AUROC FOREST PLOT
# ============================================================

auroc_plot_data <- bootstrap_ci_table %>%
  filter(
    Metric == "AUROC"
  ) %>%
  arrange(
    Estimate
  ) %>%
  mutate(
    Model = factor(
      Model,
      levels = Model
    )
  )

p_bootstrap_auc <- ggplot(
  auroc_plot_data,
  aes(
    x = Estimate,
    y = Model
  )
) +
  
  geom_errorbar(
    aes(
      xmin = CI_Lower,
      xmax = CI_Upper
    ),
    orientation = "y",
    width = 0.18,
    linewidth = 0.9,
    color = "#5B9BD5"
  ) +
  
  geom_point(
    shape = 21,
    size = 3.8,
    fill = "#19A7E0",
    color = "white",
    stroke = 0.8
  ) +
  
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(
      0,
      1,
      0.2
    )
  ) +
  
  labs(
    x = "AUROC (95% bootstrap CI)",
    y = NULL,
    title = NULL
  ) +
  
  theme_bootstrap()

print(p_bootstrap_auc)

# ============================================================
# 12. ALL PERFORMANCE METRICS FOREST PLOT
# ============================================================

all_metrics_data <- bootstrap_ci_table %>%
  filter(
    Metric != "Brier"
  )

p_bootstrap_all <- ggplot(
  all_metrics_data,
  aes(
    x = Estimate,
    y = Model
  )
) +
  
  geom_errorbar(
    aes(
      xmin = CI_Lower,
      xmax = CI_Upper
    ),
    orientation = "y",
    width = 0.16,
    linewidth = 0.7,
    color = "#5B9BD5"
  ) +
  
  geom_point(
    shape = 21,
    size = 2.8,
    fill = "#F06478",
    color = "white",
    stroke = 0.5
  ) +
  
  facet_wrap(
    ~ Metric,
    ncol = 3
  ) +
  
  scale_x_continuous(
    limits = c(0, 1),
    breaks = c(
      0,
      0.5,
      1
    )
  ) +
  
  labs(
    x = "Performance estimate (95% CI)",
    y = NULL,
    title = NULL
  ) +
  
  theme_bootstrap() +
  
  theme(
    axis.text.y = element_text(
      size = 8
    ),
    panel.spacing = grid::unit(
      1.2,
      "lines"
    )
  )

print(p_bootstrap_all)

# ============================================================
# 13. BRIER SCORE FOREST PLOT
# ============================================================

brier_plot_data <- bootstrap_ci_table %>%
  filter(
    Metric == "Brier"
  )

p_bootstrap_brier <- ggplot(
  brier_plot_data,
  aes(
    x = Estimate,
    y = reorder(
      Model,
      -Estimate
    )
  )
) +
  
  geom_errorbar(
    aes(
      xmin = CI_Lower,
      xmax = CI_Upper
    ),
    orientation = "y",
    width = 0.18,
    linewidth = 0.9,
    color = "#5B9BD5"
  ) +
  
  geom_point(
    shape = 21,
    size = 3.6,
    fill = "#17A398",
    color = "white",
    stroke = 0.7
  ) +
  
  scale_x_continuous(
    limits = c(0, 1)
  ) +
  
  labs(
    x = "Brier score (95% bootstrap CI)",
    y = NULL,
    title = NULL
  ) +
  
  theme_bootstrap()

print(p_bootstrap_brier)

# ============================================================
# 14. FINAL OUTPUT OBJECTS
# ============================================================

# bootstrap_ci_table
# bootstrap_publication_table
# bootstrap_distributions
# bootstrap_indices
#
# p_bootstrap_auc
# p_bootstrap_all
# p_bootstrap_brier

cat(
  "\n====================================\n",
  "BOOTSTRAP ANALYSIS FINISHED\n",
  "Models:", length(model_names), "\n",
  "Bootstrap iterations:", B, "\n",
  "Test observations:", n_test, "\n",
  "Confidence level: 95%\n",
  "No model retraining\n",
  "No automatic saving\n",
  "====================================\n"
)













#### Part B####

# ============================================================
# FIX: INVALID UTF-8 IN SUBGROUP VARIABLES
# REPLACE PREVIOUS SECTIONS 4–7
# ============================================================

# 4. SAFE TEXT CONVERSION

safe_utf8 <- function(x) {
  
  # Preserve factor labels rather than integer codes.
  x <- as.character(x)
  
  # Convert from the current locale to UTF-8.
  # Invalid byte sequences are replaced with a marker.
  x <- iconv(
    x,
    from = "",
    to = "UTF-8",
    sub = "?"
  )
  
  x
}

# Build subgroup dataset

subgroup_data <- data.frame(
  Actual = as.character(xgb_pred$SBA),
  Probability = as.numeric(xgb_pred$Probability),
  stringsAsFactors = FALSE
)

for (variable in names(subgroup_columns)) {
  
  column_name <- subgroup_columns[[variable]]
  
  raw_values <- subgroup_source[[column_name]]
  
  subgroup_data[[variable]] <- safe_utf8(
    raw_values
  )
}

# Check the cleaned subgroup categories

for (variable in names(subgroup_columns)) {
  
  cat("\nVariable:", variable, "\n")
  
  print(
    table(
      subgroup_data[[variable]],
      useNA = "ifany"
    )
  )
}

# ============================================================
# 5. AUROC FUNCTION
# ============================================================

calculate_subgroup_auc <- function(actual, probability) {
  
  actual <- as.character(actual)
  probability <- as.numeric(probability)
  
  if (
    length(actual) < 2 ||
    length(unique(actual)) < 2
  ) {
    return(NA_real_)
  }
  
  as.numeric(
    pROC::auc(
      pROC::roc(
        response = actual,
        predictor = probability,
        levels = c("Unskilled", "Skilled"),
        direction = "<",
        quiet = TRUE
      )
    )
  )
}

# ============================================================
# 6. BOOTSTRAP SETTINGS
# ============================================================

B_subgroup <- 2000
min_group_n <- 30

set.seed(123)

subgroup_results <- list()

# ============================================================
# 7. RUN SUBGROUP BOOTSTRAP
# ============================================================

for (variable in names(subgroup_columns)) {
  
  cat("\nProcessing subgroup variable:", variable, "\n")
  
  groups <- unique(subgroup_data[[variable]])
  
  groups <- groups[
    !is.na(groups) &
      nzchar(trimws(groups))
  ]
  
  for (group in groups) {
    
    dat <- subgroup_data[
      !is.na(subgroup_data[[variable]]) &
        subgroup_data[[variable]] == group &
        !is.na(subgroup_data$Actual) &
        is.finite(subgroup_data$Probability),
      ,
      drop = FALSE
    ]
    
    n <- nrow(dat)
    
    n_skilled <- sum(dat$Actual == "Skilled")
    n_unskilled <- sum(dat$Actual == "Unskilled")
    
    if (
      n < min_group_n ||
      n_skilled < 2 ||
      n_unskilled < 2
    ) {
      next
    }
    
    point_auc <- calculate_subgroup_auc(
      dat$Actual,
      dat$Probability
    )
    
    # Stratified bootstrap retains both outcome classes.
    positive_ids <- which(dat$Actual == "Skilled")
    negative_ids <- which(dat$Actual == "Unskilled")
    
    boot_auc <- replicate(
      B_subgroup,
      {
        ids <- c(
          sample(
            positive_ids,
            length(positive_ids),
            replace = TRUE
          ),
          sample(
            negative_ids,
            length(negative_ids),
            replace = TRUE
          )
        )
        
        calculate_subgroup_auc(
          dat$Actual[ids],
          dat$Probability[ids]
        )
      }
    )
    
    boot_auc <- boot_auc[is.finite(boot_auc)]
    
    ci <- stats::quantile(
      boot_auc,
      probs = c(0.025, 0.975),
      na.rm = TRUE,
      names = FALSE
    )
    
    subgroup_results[[
      length(subgroup_results) + 1
    ]] <- data.frame(
      Variable = variable,
      Group = group,
      N = n,
      Skilled_N = n_skilled,
      Unskilled_N = n_unskilled,
      AUROC = point_auc,
      CI_Lower = ci[1],
      CI_Upper = ci[2],
      Valid_Bootstraps = length(boot_auc),
      stringsAsFactors = FALSE
    )
  }
}

# ============================================================
# 8. REBUILD RESULTS TABLE
# ============================================================

if (length(subgroup_results) == 0) {
  stop("No eligible subgroup results were generated.")
}

subgroup_ci_table <- dplyr::bind_rows(
  subgroup_results
)

subgroup_ci_table <- subgroup_ci_table %>%
  dplyr::mutate(
    `AUROC (95% CI)` = sprintf(
      "%.3f (%.3f–%.3f)",
      AUROC,
      CI_Lower,
      CI_Upper
    )
  )

print(
  tibble::as_tibble(subgroup_ci_table),
  n = Inf,
  width = Inf
)

# Previous forest-plot code can now be run.




























# ============================================================
# FINAL FIXED SUBGROUP FOREST PLOTS
# MOZAMBIQUE SBA — XGBOOST
# No retraining | No bootstrap rerun | No saving
# ============================================================

library(ggplot2)
library(dplyr)

stopifnot(exists("subgroup_ci_table"))

# ============================================================
# 1. PREPARE DATA
# ============================================================

plot_data <- as.data.frame(subgroup_ci_table)

plot_data$Group <- as.character(plot_data$Group)
plot_data$Variable <- as.character(plot_data$Variable)

# Fix known encoding issue for display only
plot_data$Group[
  plot_data$Group == "zamb?zia"
] <- "Zambezia"

region_names <- c(
  "niassa" = "Niassa",
  "cabo delgado" = "Cabo Delgado",
  "nampula" = "Nampula",
  "tete" = "Tete",
  "manica" = "Manica",
  "sofala" = "Sofala",
  "inhambane" = "Inhambane",
  "gaza" = "Gaza",
  "maputo" = "Maputo",
  "cidade de maputo" = "Cidade de Maputo"
)

for (old_name in names(region_names)) {
  plot_data$Group[
    plot_data$Variable == "Region" &
      plot_data$Group == old_name
  ] <- region_names[[old_name]]
}

# ============================================================
# 2. CLEAN JOURNAL THEME
# ============================================================

theme_journal <- function(base_size = 11) {
  
  ggplot2::theme_minimal(base_size = base_size) +
    
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = "white",
        color = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = "white",
        color = NA
      ),
      panel.grid.major.x = ggplot2::element_line(
        color = "#EAF0F6",
        linewidth = 0.35
      ),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      
      axis.text.x = ggplot2::element_text(
        size = 9,
        color = "#65748B"
      ),
      axis.text.y = ggplot2::element_text(
        size = 10,
        color = "#26354A"
      ),
      axis.title.x = ggplot2::element_text(
        size = 11,
        face = "bold",
        color = "#26354A"
      ),
      
      strip.background = ggplot2::element_rect(
        fill = "#F2F7FD",
        color = "#E2EAF3"
      ),
      strip.text = ggplot2::element_text(
        size = 10,
        face = "bold",
        color = "#174A7E"
      ),
      
      legend.position = "none",
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      
      # FIXED: Explicit ggplot2 namespace
      plot.margin = ggplot2::margin(
        12, 18, 12, 12,
        unit = "pt"
      )
    )
}

# ============================================================
# 3. RESIDENCE FOREST PLOT
# ============================================================

residence_data <- plot_data %>%
  dplyr::filter(Variable == "Residence") %>%
  dplyr::arrange(AUROC) %>%
  dplyr::mutate(
    Group_Label = paste0(
      Group, " (n=", N, ")"
    ),
    Group_Label = factor(
      Group_Label,
      levels = Group_Label
    )
  )

p_residence <- ggplot(
  residence_data,
  aes(x = AUROC, y = Group_Label)
) +
  
  geom_segment(
    aes(
      x = CI_Lower,
      xend = CI_Upper,
      yend = Group_Label
    ),
    color = "#4A90D0",
    linewidth = 1.1
  ) +
  
  geom_point(
    shape = 21,
    size = 4,
    fill = "#20B2E6",
    color = "white",
    stroke = 0.8
  ) +
  
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  
  labs(
    x = "AUROC (95% bootstrap CI)",
    y = NULL
  ) +
  
  theme_journal()

print(p_residence)

# ============================================================
# 4. REGION FOREST PLOT
# ============================================================

region_data <- plot_data %>%
  dplyr::filter(Variable == "Region") %>%
  dplyr::arrange(AUROC) %>%
  dplyr::mutate(
    Group_Label = paste0(
      Group, " (n=", N, ")"
    ),
    Group_Label = factor(
      Group_Label,
      levels = Group_Label
    )
  )

p_region <- ggplot(
  region_data,
  aes(x = AUROC, y = Group_Label)
) +
  
  geom_segment(
    aes(
      x = CI_Lower,
      xend = CI_Upper,
      yend = Group_Label
    ),
    color = "#4A90D0",
    linewidth = 0.95
  ) +
  
  geom_point(
    shape = 21,
    size = 3.5,
    fill = "#F06478",
    color = "white",
    stroke = 0.7
  ) +
  
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  
  labs(
    x = "AUROC (95% bootstrap CI)",
    y = NULL
  ) +
  
  theme_journal()

print(p_region)

# ============================================================
# 5. COMBINED SUBGROUP FOREST PLOT
# ============================================================

combined_data <- plot_data %>%
  dplyr::mutate(
    Group_Label = paste0(
      Group, " (n=", N, ")"
    )
  )

p_subgroup_combined <- ggplot(
  combined_data,
  aes(
    x = AUROC,
    y = reorder(Group_Label, AUROC)
  )
) +
  
  geom_segment(
    aes(
      x = CI_Lower,
      xend = CI_Upper,
      yend = reorder(Group_Label, AUROC)
    ),
    color = "#4A90D0",
    linewidth = 0.9
  ) +
  
  geom_point(
    shape = 21,
    size = 3.4,
    fill = "#20B2E6",
    color = "white",
    stroke = 0.7
  ) +
  
  facet_wrap(
    ~ Variable,
    scales = "free_y",
    ncol = 1
  ) +
  
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  
  labs(
    x = "AUROC (95% bootstrap CI)",
    y = NULL
  ) +
  
  theme_journal()

print(p_subgroup_combined)

# ============================================================
# 6. DISPLAY FINAL RESULTS
# ============================================================

subgroup_final_table <- plot_data %>%
  dplyr::select(
    Variable,
    Group,
    N,
    Skilled_N,
    Unskilled_N,
    AUROC,
    CI_Lower,
    CI_Upper
  ) %>%
  dplyr::arrange(
    Variable,
    dplyr::desc(AUROC)
  )

print(
  tibble::as_tibble(subgroup_final_table),
  n = Inf,
  width = Inf
)

cat("\nAll subgroup plots created successfully.\n")

# ============================================================
# AVAILABLE FIGURES
# ============================================================

# p_residence
# p_region
# p_subgroup_combined
#
# No retraining
# No bootstrap rerun
# No automatic saving
# ============================================================




