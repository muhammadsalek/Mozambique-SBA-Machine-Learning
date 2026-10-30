# Sample `renv` Workflow (Template)

This is a template workflow for reproducible dependency management in R. Package versions and project-specific dependencies must be confirmed from the actual analysis environment.

## 1) Initialize environment

```r
install.packages("renv")
renv::init()
```

## 2) Install required packages

```r
# Replace with project-verified package list
install.packages(c(
  "xgboost", "e1071", "nnet", "randomForest", "class", "rpart",
  "Boruta", "pROC", "caret", "yardstick", "shapviz", "lime"
))
```

## 3) Snapshot dependencies

```r
renv::snapshot()
```

## 4) Recreate environment on another machine

```r
renv::restore()
```

## 5) Ongoing updates

```r
# After adding/removing dependencies
renv::snapshot()
```

## Notes

- Confirm package set and modeling stack against original R scripts before publication.
- Do not commit controlled-access DHS microdata.
