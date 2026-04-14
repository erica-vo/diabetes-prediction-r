############################################################
# DSA1101 – Statistical Report - Vo Ngoc Gia Bao - A0325652E (Sem 1 AY2025/26)
############################################################
library(ggplot2)
library(scales)

##  Part 1 Introduction
set.seed(611)
setwd("D:/NUS/DSA/Class of 2029/Year 1 Sem 1/DSA1101/Data")

# Read data set 
dat <- read.csv("diabetes-dataset.csv")
head(dat)

##  Part 2 EDA: Exploring the variables and association
dir.create("figs_report", showWarnings = FALSE)

# 2.1) Description of the data set

# Coerce categorical variables to factors
dat$gender          <- as.factor(dat$gender)
dat$smoking_history <- as.factor(dat$smoking_history)
dat$hypertension    <- as.factor(dat$hypertension)     # 0/1 factor
dat$heart_disease   <- as.factor(dat$heart_disease)    # 0/1 factor
dat$diabetes        <- as.factor(dat$diabetes)         # 0/1 factor (for EDA)

# Numeric variables stay numeric: age, bmi, HbA1c_level, blood_glucose_level

# Basic structure, summaries, and missing values
str(dat)
summary(dat)
colSums(is.na(dat)) # Check for missing values

# Distribution of the response
tab_y <- table(dat$diabetes)
tab_y
prop.table(tab_y)

# Setting the general theme 
theme_set(
  theme_gray(base_size = 14) +
    theme(
      plot.title    = element_text(hjust = 0.5, face = "bold", size = 16),
      legend.position = "top",
      panel.background = element_rect(fill = "#F2F2F2", colour = NA), 
      panel.border     = element_rect(colour = "black", fill = NA, linewidth = 0.6),
      panel.grid.major = element_line(colour = "white",  linewidth = 0.8), 
      panel.grid.minor = element_line(colour = "#ECECEC", linewidth = 0.6) 
    )
)
title_center <- theme(
  plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
)

# Balanced grid helpers
grid_cont  <- list (
  scale_x_continuous(breaks = breaks_extended(n = 6), minor_breaks = waiver()),
  scale_y_continuous(breaks = breaks_extended(n = 6), minor_breaks = waiver())
)
grid_yonly <- list ( 
  scale_y_continuous(breaks = breaks_extended(n = 6), minor_breaks = waiver())
)

cols <- c("0"="#9EC5FE","1"="#FF9AA2") 

# Fig 1: Response distribution
p_y <- ggplot(dat, aes(x = diabetes, fill = diabetes)) +
  geom_bar(width = 0.6) +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.35, size = 5) +
  scale_fill_manual(values = cols) +
  scale_y_continuous(expand = expansion(mult = c(0, .14))) +
  labs(title = "Distribution of Diabetes (0 vs 1)", x = "Diabetes", y = "Count") +
  title_center + grid_yonly + theme(legend.position = "top")
ggsave("figs_report/fig_y_bar.png", p_y, width = 9, height = 6, dpi = 120)

# 2.2) Categorical vs Diabetes: counts and row-wise proportions

# Distribution of each categorical variables 
library(dplyr)
library(tidyr)
library(ggplot2)

cat_vars <- c("gender","smoking_history","hypertension","heart_disease")
dat$diabetes <- factor(dat$diabetes, levels = c(0,1), labels = c("0","1"))

# Reshape to long format: one row per (id, variable, level)
df_long <- dat %>%
  pivot_longer(all_of(cat_vars), names_to = "variable", values_to = "level")

names(cols) <- levels(df_long$diabetes)

# Fig 2. Distribution of Categorical variables 
p_counts <- ggplot(df_long, aes(x = level)) +
  geom_bar(width = 0.9, colour = "black", linewidth = 0.25, fill = "grey75") +
  facet_wrap(~ variable, ncol = 2, scales = "free_x") +
  labs(title = "Distribution of Categorical Variables", x = "Value", y = "Count") +
  theme_gray(base_size = 12) +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6)
  )

ggsave("figs_report/cat_counts.png", p_counts, width = 10.5, height = 4.2, dpi = 120)

tab_gender <- table(dat$gender, dat$diabetes); tab_gender
prop.table(tab_gender, 1)

tab_smoke  <- table(dat$smoking_history, dat$diabetes); tab_smoke
prop.table(tab_smoke, 1)

tab_htn    <- table(dat$hypertension, dat$diabetes); tab_htn
prop.table(tab_htn, 1)

tab_hd     <- table(dat$heart_disease, dat$diabetes); tab_hd
prop.table(tab_hd, 1)

# Fig.3. Relationship of categorical features to the response variable
# Create and combine 4 stacked bars into a single faceted figure 
# Plot row-normalised stacked bars with facets (one panel per variable)
p_stack_all <- ggplot(df_long, aes(x = level, fill = diabetes)) +
  geom_bar(position = "fill", colour = "black", linewidth = 0.25, width = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = cols, name = "Diabetes",
                    labels = c("0" = "Non-Diabetic", "1" = "Diabetic")) +
  facet_wrap(~ variable, ncol = 2, scales = "free_x") +
  labs(title = "Proportion of Diabetes across Categorical Variables",
       x = "Value", y = "Proportion") +
  theme_gray(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right",  
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6)
  )

ggsave("figs_report/stack_categoricals.png",
       p_stack_all, width = 10.5, height = 4.2, dpi = 120)

# 2.3) Numeric vs Diabetes: histograms and boxplots
num_vars <- c("age", "bmi", "HbA1c_level", "blood_glucose_level")

# Fig.4. Distribution of numerical variables
# Create and combine 4 HISTOGRAMS into a single faceted figure
df_long_num <- dat %>%
  pivot_longer(all_of(num_vars), names_to = "variable", values_to = "value")

p_hist_all <- ggplot(df_long_num, aes(x = value, fill = diabetes)) +
  geom_histogram(position = "identity", bins = 30, colour = NA) +
  scale_fill_manual(values = cols, name = "Diabetes",
                    breaks = levels(dat$diabetes), labels = levels(dat$diabetes)) +
  facet_wrap(~ variable, ncol = 2, scales = "free") +
  labs(title = "Distributions of Numerical Variables by Diabetes",
       x = "Value", y = "Count") 

ggsave("figs_report/hists_numerics.png",
       p_hist_all, width = 10.5, height = 6.5, dpi = 120)

# Fig.5. Relationship of numerical features to the response variable
# Create and combine 4 BOXPLOTS into a single faceted figure 
p_box_all <- ggplot(df_long_num, aes(x = diabetes, y = value, fill = diabetes)) +
  geom_boxplot(width = 0.65, notch = FALSE, outlier.shape = 16,
               outlier.size = 1.6, median.linewidth = 0.9) +
  scale_fill_manual(values = cols, name = "Diabetes",
                    breaks = levels(dat$diabetes), labels = levels(dat$diabetes)) +
  facet_wrap(~ variable, ncol = 2, scales = "free_y") +
  labs(title = "Numerical Variables ~ Diabetes (Boxplots)",
       x = "Diabetes", y = "Value")
ggsave("figs_report/box_numerics.png",
       p_box_all, width = 10.5, height = 6.5, dpi = 120)


# Part 3 Building models 

## Create 1/10 subset (stratified, keep all levels)

cat_vars <- c("gender","smoking_history","hypertension","heart_disease")
num_vars <- c("age","bmi","HbA1c_level","blood_glucose_level")
target   <- "diabetes"

# ensure types
dat[cat_vars] <- lapply(dat[cat_vars], \(x) factor(x))
dat[[target]] <- factor(dat[[target]], levels = c("0","1"))

# stratified by diabetes
n <- nrow(dat); n_sub <- round(0.10 * n)
p1 <- mean(dat[[target]] == "1")
n1 <- round(n_sub * p1); n0 <- n_sub - n1
idx1 <- which(dat[[target]] == "1")
idx0 <- which(dat[[target]] == "0")
dat_sub <- dat[c(sample(idx1, n1), sample(idx0, n0)), , drop = FALSE]

# ensure no categorical level is lost (add min. 1 row/level if missing), then trim back
for (v in cat_vars) {
  miss <- setdiff(levels(dat[[v]]), levels(droplevels(dat_sub[[v]])))
  if (length(miss)) {
    add <- do.call(rbind, lapply(miss, function(lv) dat[which(dat[[v]] == lv)[1], , drop = FALSE]))
    dat_sub <- droplevels(rbind(dat_sub, add))
    # trim: remove from majority class to keep class ratio close
    if (nrow(dat_sub) > n_sub) {
      maj <- names(sort(table(dat_sub[[target]]), decreasing = TRUE))[1]
      rm  <- sample(which(dat_sub[[target]] == maj), nrow(dat_sub) - n_sub)
      dat_sub <- dat_sub[-rm, , drop = FALSE]
    }
  }
}

## quick checks 
cat("Proportion diabetes (full vs subset):\n")
print(round(prop.table(table(dat[[target]])), 4))
print(round(prop.table(table(dat_sub[[target]])), 4))

cat("\nProportion by level (full vs subset):\n")
for (v in cat_vars) {
  full_p <- round(prop.table(table(dat[[v]])), 4)
  sub_p  <- round(prop.table(table(droplevels(dat_sub[[v]]))), 4)
  sub_p  <- sub_p[names(full_p)]; sub_p[is.na(sub_p)] <- 0
  cat("\n- ", v, "\n", sep = ""); print(data.frame(level = names(full_p), full = as.numeric(full_p), sub = as.numeric(sub_p)), row.names = FALSE)
}

summary(dat)
# gender           age        hypertension heart_disease    smoking_history 
# Female:58552   Min.   : 0.08   0:92515      0:96058       current    : 9286  
# Male  :41430   1st Qu.:24.00   1: 7485      1: 3942       ever       : 4004  
# Other :   18   Median :43.00                              former     : 9352  
#                Mean   :41.89                              never      :35095  
#                3rd Qu.:60.00                              No Info    :35816  
#                Max.   :80.00                              not current: 6447  
# bmi             HbA1c_level    blood_glucose_level  diabetes 
# Min.   :10.01   Min.   :3.500   Min.   : 80.0       0:91500  
# 1st Qu.:23.63   1st Qu.:4.800   1st Qu.:100.0       1: 8500  
# Median :27.32   Median :5.800   Median :140.0                
# Mean   :27.32   Mean   :5.528   Mean   :138.1                
# 3rd Qu.:29.58   3rd Qu.:6.200   3rd Qu.:159.0                
# Max.   :95.69   Max.   :9.000   Max.   :300.0 

summary(dat_sub)
# gender          age        hypertension heart_disease    smoking_history
# Female:5819   Min.   : 0.08   0:9221       0:9585        current    : 909  
# Male  :4179   1st Qu.:24.00   1: 779       1: 415        ever       : 373  
# Other :   2   Median :43.00                              former     : 990  
#               Mean   :41.94                              never      :3527  
#               3rd Qu.:60.00                              No Info    :3561  
#               Max.   :80.00                              not current: 640  
# bmi             HbA1c_level    blood_glucose_level  diabetes
# Min.   :10.89   Min.   :3.500   Min.   : 80.0       0:9150  
# 1st Qu.:23.65   1st Qu.:4.800   1st Qu.:100.0       1: 850  
# Median :27.32   Median :5.800   Median :140.0               
# Mean   :27.25   Mean   :5.532   Mean   :138.1               
# 3rd Qu.:29.48   3rd Qu.:6.200   3rd Qu.:159.0               
# Max.   :75.78   Max.   :9.000   Max.   :300.0    


# 3.1 KNN - choose the best k 
library(class)       
set.seed(611)
# outcome & numeric predictors
y_sub   <- factor(dat_sub$diabetes, levels = c("0","1"))
num_vars <- c("age","bmi","HbA1c_level","blood_glucose_level")

# 1) Stratified 5-fold ids (each fold has both classes)
idx0 <- which(y_sub == "0")
idx1 <- which(y_sub == "1")

folds_j <- integer(nrow(dat_sub))
folds_j[idx0] <- sample(rep(1:5, length.out = length(idx0)))
folds_j[idx1] <- sample(rep(1:5, length.out = length(idx1)))

# 2) Metric (unchanged)
get_metrics <- function(y_true, y_pred) {
  tab <- table(y_true, y_pred)
  TP <- tab["1","1"]; TN <- tab["0","0"]; FP <- tab["0","1"]; FN <- tab["1","0"]
  FPR <- FP / (FP + TN)
  TPR <- TP / (TP + FN)
  ACC <- (TP + TN) / sum(tab)
  c(FPR = FPR, TPR = TPR, ACC = ACC)
}

# 3) k grid (odd) and CV loop (unchanged, only dat_sub & folds_j used)
k_grid <- seq(3, 51, by = 2)
cv_out <- data.frame(k = integer(), fold = integer(),
                     FPR = double(), TPR = double(), ACC = double())

for (k in k_grid) {
  for (j in 1:5) {
    idx_tr <- which(folds_j != j)
    idx_te <- which(folds_j == j)
    
    X_tr_raw <- as.matrix(dat_sub[idx_tr, num_vars, drop = FALSE])
    X_te_raw <- as.matrix(dat_sub[idx_te, num_vars, drop = FALSE])
    
    mu  <- colMeans(X_tr_raw)
    sds <- pmax(apply(X_tr_raw, 2, sd), 1e-9)
    
    X_tr <- scale(X_tr_raw, center = mu, scale = sds)
    X_te <- scale(X_te_raw, center = mu, scale = sds)
    
    y_tr <- y_sub[idx_tr]
    y_te <- y_sub[idx_te]
    
    y_hat <- knn(train = X_tr, test = X_te, cl = y_tr, k = k)
    
    m <- get_metrics(y_te, y_hat)
    cv_out <- rbind(cv_out, data.frame(k = k, fold = j,
                                       FPR = m["FPR"], TPR = m["TPR"], ACC = m["ACC"]))
  }
}

# 4) Pick best k by mean FPR + quick plot (unchanged)
agg_knn <- aggregate(cbind(FPR, TPR, ACC) ~ k, data = cv_out, FUN = mean)

# Fig.6. FPR performance of k-NN models 
library(ggplot2)
p <- ggplot(agg_knn, aes(k, FPR)) +
  geom_line() + geom_point() +
  labs(title = "KNN 5-fold CV: mean FPR vs k", x = "k", y = "Mean FPR")
ggsave("figs_report/knn_cv_fpr.png", p, width = 7.5, height = 4.8, dpi = 120)
best_k  <- 29 #by looking at the plot and "agg_knn"
cat("Best k by mean Type I error (FPR):", best_k, "\n")

library(grid); library(gridExtra); library(gtable)

# prep data
tbl <- agg_knn[order(agg_knn$k), ]
colnames(tbl) <- c("k","FPR","TPR","ACC")
tbl$k   <- as.integer(round(tbl$k))
tbl$FPR <- sprintf("%.6f", tbl$FPR)
tbl$TPR <- sprintf("%.6f", tbl$TPR)
tbl$ACC <- sprintf("%.6f", tbl$ACC)

cut  <- ceiling(nrow(tbl)/2)
tbl1 <- tbl[1:cut, ]
tbl2 <- tbl[(cut+1):nrow(tbl), ]

# theme for the tables 
zebra <- rep(c("#FFFFFF","#F7F7F7"), length.out = nrow(tbl1))
tt <- ttheme_minimal(
  core    = list(padding = unit(c(1.5, 5), "mm"),
                 bg_params = list(fill = zebra),
                 fg_params = list(cex = 0.96)),
  colhead = list(padding = unit(c(1.8, 5), "mm"),
                 bg_params = list(fill = "#EEEEEE"),
                 fg_params = list(fontface = "bold", cex = 1.0))
)

tg1 <- tableGrob(tbl1, rows = NULL, theme = tt)
tg2 <- tableGrob(tbl2, rows = NULL, theme = tt)
tg1$widths <- tg2$widths  # canh cột

# border around the tables 
add_border <- function(g) gtable_add_grob(
  g, rectGrob(gp = gpar(fill = NA, col = "grey60", lwd = 0.8)),
  t = 1, l = 1, b = nrow(g), r = ncol(g)
)
tg1 <- add_border(tg1); tg2 <- add_border(tg2)

# Fig.7. Summary FPR, TPR, ACC of of k-NN models
# create and save the table 
png("figs_report/knn_cv_table_side_by_side.png",
    width = 1400, height = 720, res = 140)
grid.draw(arrangeGrob(
  tg1, nullGrob(), tg2,
  ncol = 3,
  widths = unit.c(unit(1, "null"), unit(0.035, "npc"), unit(1, "null")) 
))
dev.off()


# 3.2 Decision Tree (CV by minsplit; split = "information") 

library(rpart); library(rpart.plot)
set.seed(611) 
# outcome ~ predictors 
form_dt <- diabetes ~ gender + smoking_history + hypertension + heart_disease +
  age + bmi + HbA1c_level + blood_glucose_level

mins_grid <- seq(10, 100, by = 5)   
cv_dt <- data.frame(minsplit=integer(), fold=integer(),
                    FPR=double(), TPR=double(), ACC=double())

for (ms in mins_grid) {
  for (j in 1:5) {
    id_tr <- which(folds_j != j)
    id_te <- which(folds_j == j)
    tr <- dat_sub[id_tr, ]; te <- dat_sub[id_te, ]
    
    fit <- rpart(
      form_dt, data = tr, method = "class",
      parms   = list(split = "information"),
      control = rpart.control(minsplit = ms, cp = 0, xval = 0)  
    )
    
    y_hat <- predict(fit, newdata = te, type = "class")  # ngưỡng 0.5 mặc định
    m <- get_metrics(te$diabetes, y_hat)
    
    cv_dt <- rbind(cv_dt, data.frame(minsplit=ms, fold=j,
                                     FPR=m["FPR"], TPR=m["TPR"], ACC=m["ACC"]))
  }
}

# choose best minsplit according to mean FPR
agg_dt <- aggregate(cbind(FPR,TPR,ACC) ~ minsplit, data=cv_dt, FUN=mean)
best_minsplit <- 60
cat("Best minsplit by mean FPR:", best_minsplit, "\n")

# Fig.9. FPR performance of k-NN models 
# create the curve of mean FPR vs minsplit
library(ggplot2)
p_dt <- ggplot(agg_dt, aes(minsplit, FPR)) +
  geom_line() + geom_point() +
  labs(title="DT 5-fold CV: mean FPR vs minsplit",
       x="minsplit", y="Mean FPR")
ggsave("figs_report/dt_cv_fpr.png", p_dt, width=7.2, height=4.4, dpi=120)

# refit the best decision tree on full subset and create, save
fit_dt_best <- rpart(
  form_dt, data = dat_sub, method = "class",
  parms   = list(split = "information"),
  control = rpart.control(minsplit = best_minsplit)
)

# Fig.10. 60-minsplit decision tree 
png("figs_report/dt_best_tree.png", width=1400, height=900, res=130)
rpart.plot(fit_dt_best, type=4, extra=2, varlen=0, faclen=0, clip.right.labs=FALSE)
dev.off()

# Fig.8. Summary FPR, TPR, ACC of of DT models 
library(grid); library(gridExtra); library(gtable)

#  prep data
tbl <- agg_dt[order(agg_dt$minsplit), ]
colnames(tbl) <- c("minsplit","FPR","TPR","ACC")
tbl$ minsplit  <- as.integer(round(tbl$minsplit))
tbl$FPR <- sprintf("%.6f", tbl$FPR)
tbl$TPR <- sprintf("%.6f", tbl$TPR)
tbl$ACC <- sprintf("%.6f", tbl$ACC)

cut  <- ceiling(nrow(tbl)/2)
tbl1 <- tbl[1:cut, ]
tbl2 <- tbl[(cut+1):nrow(tbl), ]

zebra <- rep(c("#FFFFFF","#F7F7F7"), length.out = nrow(tbl1))

tg1 <- tableGrob(tbl1, rows = NULL, theme = tt)
tg2 <- tableGrob(tbl2, rows = NULL, theme = tt)
tg1$widths <- tg2$widths  

add_border <- function(g) gtable_add_grob(
  g, rectGrob(gp = gpar(fill = NA, col = "grey60", lwd = 0.8)),
  t = 1, l = 1, b = nrow(g), r = ncol(g)
)
tg1 <- add_border(tg1); tg2 <- add_border(tg2)

png("figs_report/dt_cv_table_side_by_side.png",
    width = 1400, height = 720, res = 140)
grid.draw(arrangeGrob(
  tg1, nullGrob(), tg2,
  ncol = 3,
  widths = unit.c(unit(1, "null"), unit(0.035, "npc"), unit(1, "null")) # khoảng cách giữa 2 bảng
))
dev.off()


# 3.3) Logistic Regression

## Backward regression (p-value) 
M1 <- glm( diabetes ~., data = dat_sub,family = binomial)
summary(M1)

M2 <- glm( diabetes ~ gender + age+ hypertension + heart_disease + bmi + HbA1c_level +blood_glucose_level,
          data = dat_sub,family = binomial)
summary(M2)

M3 <- glm( diabetes ~ age+ hypertension + heart_disease + bmi + HbA1c_level +blood_glucose_level,
          data = dat_sub,family = binomial)
summary(M3)

# tiny helper for CV mean metrics (uses folds_j & get_metrics already defined)
cv_glm_mean <- function(fm, data, folds) {
  out <- data.frame(FPR=double(), TPR=double(), ACC=double())
  for (j in 1:5) {
    tr <- data[folds != j, , drop = FALSE]
    te <- data[folds == j, , drop = FALSE]
    fit <- glm(fm, data = tr, family = binomial())
    p   <- predict(fit, te, type = "response")
    yhat<- factor(ifelse(p >= 0.5, "1", "0"), levels = c("0","1"))
    out <- rbind(out, as.list(get_metrics(te$diabetes, yhat)))
  }
  colMeans(out)
}

# run CV for the three LR specs
res_lr <- rbind(
  M1_full             = cv_glm_mean(formula(M1), dat_sub, folds_j),
  M2_drop_smoking     = cv_glm_mean(formula(M2), dat_sub, folds_j),
  M3_drop_smk_gender  = cv_glm_mean(formula(M3), dat_sub, folds_j)
)
print(round(res_lr, 6))

# choose by lowest mean FPR; tie-break by fewer terms
k_terms <- c(
  M1_full            = length(attr(terms(formula(M1)), "term.labels")),
  M2_drop_smoking    = length(attr(terms(formula(M2)), "term.labels")),
  M3_drop_smk_gender = length(attr(terms(formula(M3)), "term.labels"))
)
ord <- order(res_lr[,"FPR"], k_terms[rownames(res_lr)])
best_lr_name <- rownames(res_lr)[ord][1]
cat("Selected LR (min mean FPR, tie-break by fewer vars):", best_lr_name, "\n")

# refit the selected LR on full subset for downstream evaluation
fm_best    <- switch(best_lr_name,
                     M1_full            = formula(M1),
                     M2_drop_smoking    = formula(M2),
                     M3_drop_smk_gender = formula(M3))
fit_lr_best <- glm(fm_best, data = dat_sub, family = binomial())
summary(fit_lr_best)

# Fig.11. Bar charts of mean FPR, TPR, ACC for LR models 
# quick bar plot of mean FPR
library(ggplot2)
df_lr <- data.frame(Model = rownames(res_lr), FPR = res_lr[,"FPR"], row.names = NULL)
p_lr <- ggplot(df_lr, aes(Model, FPR, fill = Model)) +
  geom_col(width = 0.6, color = "black") +
  geom_text(aes(label = sprintf("%.4f", FPR)), vjust = -0.35, size = 3.8) +
  scale_fill_manual(values = c("#A3C9FF","#FF9AA2","#B5EAD7"), guide = "none") +
  labs(title = "Logistic Regression (subset): 5-fold mean FPR",
       x = NULL, y = "Mean FPR") +
  theme_gray(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("figs_report/lr_cv_fpr.png", p_lr, width = 7.2, height = 4.6, dpi = 120)


plot_df <- as.data.frame(res_lr)
plot_df$Model <- rownames(res_lr)
plot_df <- pivot_longer(plot_df, cols = c(FPR, TPR, ACC),
                        names_to = "Metric", values_to = "Value")

# order models nicely on x-axis
plot_df$Model  <- factor(plot_df$Model,
                         levels = c("M1_full","M2_drop_smoking","M3_drop_smk_gender"))
plot_df$Metric <- factor(plot_df$Metric, levels = c("FPR","TPR","ACC"))

p_lr_all <- ggplot(plot_df, aes(x = Model, y = Value, fill = Metric)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, colour = "black") +
  geom_text(aes(label = sprintf("%.4f", Value)),
            position = position_dodge(width = 0.75), vjust = -0.25, size = 3.6) +
  scale_fill_manual(values = c(FPR = "#F8766D", TPR = "#00BA38", ACC = "#619CFF"),
                    name = "Metric") +
  labs(title = "Logistic Regression (subset): 5-fold mean FPR / TPR / ACC",
       x = NULL, y = "Mean metric value") +
  coord_cartesian(ylim = c(0, max(plot_df$Value) * 1.12)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("figs_report/lr_cv_metrics_grouped.png", p_lr_all,
       width = 8.6, height = 5.0, dpi = 120)

## Part 4. Model comparision on full data set 

set.seed(611)
library(class)
library(rpart)
library(pROC)
library(dplyr)
library(tidyr)
library(ggplot2)

# 4.1 shared 5-fold split on FULL data (stratified by diabetes)
y_full <- factor(dat$diabetes, levels = c("0","1"))
idx0 <- which(y_full == "0"); idx1 <- which(y_full == "1")
folds_full <- integer(nrow(dat))
folds_full[idx0] <- sample(rep(1:5, length.out = length(idx0)))
folds_full[idx1] <- sample(rep(1:5, length.out = length(idx1)))

# Best versions found on the 10k subset
best_k       <- 29
best_minsplit <- 60
fm_lr_best   <- diabetes ~ age + hypertension + heart_disease +
  bmi + HbA1c_level + blood_glucose_level  # LR (M3)

# 4.2 Metric helper (prob -> class at threshold; returns named vector)
metrics_from_prob <- function(y_true, p1, thr = 0.5) {
  y_true <- factor(y_true, levels = c("0","1"))
  y_hat  <- factor(ifelse(p1 >= thr, "1", "0"), levels = c("0","1"))
  tab <- table(y_true, y_hat)
  TP <- as.numeric(tab["1","1"]); TN <- as.numeric(tab["0","0"])
  FP <- as.numeric(tab["0","1"]); FN <- as.numeric(tab["1","0"])
  FPR <- ifelse((FP + TN) > 0, FP/(FP+TN), NA_real_)
  TPR <- ifelse((TP + FN) > 0, TP/(TP+FN), NA_real_)
  ACC <- (TP + TN) / sum(tab)
  Precision <- ifelse((TP + FP) > 0, TP/(TP+FP), NA_real_)
  c(FPR = FPR, TPR = TPR, Precision = Precision, ACC = ACC)
}

# Storage (per-fold metrics) and pooled scores for ROC
cv_knn <- cv_dt <- cv_lr <- matrix(NA_real_, nrow = 5, ncol = 4,
                                   dimnames = list(NULL, c("FPR","TPR","Precision","ACC")))
pool <- list(knn_y = c(), knn_p = c(),
             dt_y  = c(), dt_p  = c(),
             lr_y  = c(), lr_p  = c())

# 5-fold loop on FULL data 
num_vars <- c("age","bmi","HbA1c_level","blood_glucose_level")

for (j in 1:5) {
  tr <- dat[folds_full != j, , drop = FALSE]
  te <- dat[folds_full == j, , drop = FALSE]
  
  ## KNN: standardise numerics on training fold, carry to test; prob=TRUE for scores
  Xtr_raw <- as.matrix(tr[, num_vars, drop = FALSE])
  Xte_raw <- as.matrix(te[, num_vars, drop = FALSE])
  mu  <- colMeans(Xtr_raw)
  sds <- pmax(apply(Xtr_raw, 2, sd), 1e-9)
  Xtr <- scale(Xtr_raw, center = mu, scale = sds)
  Xte <- scale(Xte_raw, center = mu, scale = sds)
  ytr <- factor(tr$diabetes, levels = c("0","1"))
  yte <- factor(te$diabetes, levels = c("0","1"))
  
  knn_pred <- knn(train = Xtr, test = Xte, cl = ytr, k = best_k, prob = TRUE)
  # Convert "winning-class proportion" to P(Y=1)
  knn_prob1 <- ifelse(knn_pred == "1", attr(knn_pred, "prob"), 1 - attr(knn_pred, "prob"))
  cv_knn[j, ] <- metrics_from_prob(yte, knn_prob1)
  pool$knn_y <- c(pool$knn_y, as.numeric(as.character(yte)))
  pool$knn_p <- c(pool$knn_p, as.numeric(knn_prob1))
  
  ## Decision Tree: information split; probability for class "1"
  fit_dt <- rpart(
    diabetes ~ gender + smoking_history + hypertension + heart_disease +
      age + bmi + HbA1c_level + blood_glucose_level,
    data = tr, method = "class",
    parms   = list(split = "information"),
    control = rpart.control(minsplit = best_minsplit, cp = 0, xval = 0)
  )
  dt_prob1 <- predict(fit_dt, newdata = te, type = "prob")[, "1"]
  cv_dt[j, ] <- metrics_from_prob(yte, dt_prob1)
  pool$dt_y <- c(pool$dt_y, as.numeric(as.character(yte)))
  pool$dt_p <- c(pool$dt_p, as.numeric(dt_prob1))
  
  ## Logistic Regression (M3): probabilities from glm
  fit_lr <- glm(fm_lr_best, data = tr, family = binomial())
  lr_prob1 <- predict(fit_lr, newdata = te, type = "response")
  cv_lr[j, ] <- metrics_from_prob(yte, lr_prob1)
  pool$lr_y <- c(pool$lr_y, as.numeric(as.character(yte)))
  pool$lr_p <- c(pool$lr_p, as.numeric(lr_prob1))
}

# Mean metrics across folds & grouped bar chart 
res_full <- rbind(
  DT  = colMeans(cv_dt,  na.rm = TRUE),
  KNN = colMeans(cv_knn, na.rm = TRUE),
  LR  = colMeans(cv_lr,  na.rm = TRUE)
)

df_bar <- as.data.frame(res_full) |>
  tibble::rownames_to_column("Model") |>
  pivot_longer(cols = c(FPR, TPR, Precision, ACC),
               names_to = "Metric", values_to = "Value")

# Fig.12. Mean metrics of full 100k (shared 5-fold) for the best three model
metric_cols <- c(ACC = "#A3C9FF", FPR = "#FF9AA2", Precision = "#C7CEEA", TPR = "#B5EAD7")

p_bar <- ggplot(df_bar, aes(x = Model, y = Value, fill = Metric)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62, colour = "black", linewidth = 0.25) +
  geom_text(aes(label = sprintf("%.4f", Value)),
            position = position_dodge(width = 0.72), vjust = -0.25, size = 3.6) +
  scale_fill_manual(values = metric_cols, name = "Metric") +
  coord_cartesian(ylim = c(0, max(df_bar$Value) * 1.12)) +
  labs(title = "Full 100k (shared 5-fold): ACC, FPR, Precision, TPR",
       x = NULL, y = "Mean across folds") +
  theme_gray(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "top")
ggsave("figs_report/pt5_metrics_grouped.png", p_bar, width = 9, height = 5, dpi = 130)

# 4.3 ROC Curves and AUCs 
library(ROCR)   # for plotting
# Guard against any NA/Inf in pooled scores, then build ROCR predictors
ok_knn <- is.finite(pool$knn_p) & is.finite(pool$knn_y)
ok_dt  <- is.finite(pool$dt_p)  & is.finite(pool$dt_y)
ok_lr  <- is.finite(pool$lr_p)  & is.finite(pool$lr_y)

pred_knn <- prediction(pool$knn_p[ok_knn], pool$knn_y[ok_knn])
pred_dt  <- prediction(pool$dt_p [ok_dt ], pool$dt_y [ok_dt ])
pred_lr  <- prediction(pool$lr_p [ok_lr ], pool$lr_y [ok_lr ])

roc_knn_rocr <- performance(pred_knn, "tpr", "fpr")
roc_dt_rocr  <- performance(pred_dt,  "tpr", "fpr")
roc_lr_rocr  <- performance(pred_lr,  "tpr", "fpr")

auc_knn <- performance(pred_knn, "auc")@y.values[[1]]
auc_dt  <- performance(pred_dt,  "auc")@y.values[[1]]
auc_lr  <- performance(pred_lr,  "auc")@y.values[[1]]

# Fig.13. ROC curves and AUCs for three models 
png("figs_report/pt5_ROC_full_curve.png", width = 900, height = 620, res = 130)
par(mar = c(5,5,4.2,2) + 0.1)
plot(roc_knn_rocr, col = "black", lwd = 2.2,
     main = sprintf("ROC (5-fold pooled, full 100k)\nAUC  KNN: %.4f  |  DT: %.4f  |  LR: %.4f",
                    auc_knn, auc_dt, auc_lr))
plot(roc_dt_rocr, add = TRUE, col = "red",  lwd = 2.2)
plot(roc_lr_rocr, add = TRUE, col = "blue", lwd = 2.2)
abline(0, 1, lty = 3, col = "grey50")
legend("bottomright",
       legend = c(sprintf("KNN (AUC=%.4f)", auc_knn),
                  sprintf("DT  (AUC=%.4f)",  auc_dt),
                  sprintf("LR  (AUC=%.4f)",  auc_lr)),
       col = c("black","red","blue"), lwd = 2.2, bty = "n")
dev.off()

# 4.4 AUC significance (DeLong test) 
roc_knn <- pROC::roc(response = pool$knn_y[ok_knn], predictor = pool$knn_p[ok_knn], quiet = TRUE)
roc_dt  <- pROC::roc(response = pool$dt_y [ok_dt ], predictor = pool$dt_p [ok_dt ],  quiet = TRUE)
roc_lr  <- pROC::roc(response = pool$lr_y [ok_lr ], predictor = pool$lr_p [ok_lr ],  quiet = TRUE)
test_lr_vs_dt  <- roc.test(roc_lr,  roc_dt,  method = "delong")
test_lr_vs_knn <- roc.test(roc_lr,  roc_knn, method = "delong")
print(test_lr_vs_dt)
print(test_lr_vs_knn)




