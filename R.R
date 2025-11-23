library(epiDisplay)
library(dplyr)
library(rio)
library(gmodels)
library(forestplot)
library(openxlsx)
library(survival)
library(survminer)

load("D:/R/数据/50w_endpoints.RData")
load("D:/R/数据/50_data_baseline_questionnaires.RData")

ep <- a
bq <- aa
rm(a, aa)

# 发病 combine，死亡 odu --------------------------------------------

# 提取肺癌结局
sl_end <- dplyr::select(ep, 1, 122, 123, 126, 127)

# 提取基线变量
sl_bq <- dplyr::select(
  bq,
  1:5,
  age_at_study_date_x100,
  smoking_category, smoking_stopped_reason,
  smoking_stopped_years_ago, smoking_today_amount,
  alcohol_category, alc_weekly, is_female, total_alc_typ_day_g, met,
  diet_freq_fresh_veg, diet_freq_fresh_fruit, diet_freq_eggs,
  diet_freq_meat, diet_freq_other_staple, diet_freq_fish, bmi_calc,
  waist_mm, highest_education, marital_status, father_stroke, mother_stroke,
  siblings_stroke, father_heart_attack, mother_heart_attack, hours_since_last_ate_x10,
  siblings_heart_attack, stroke_or_tia_diag, chd_diag, cancer_diag,
  household_income, occupation, hypertension_diag_age, has_diabetes, diabetes_diag,
  diabetes_diag_age, random_glucose_x10, fasting_glucose_x10, father_cancer,
  mother_cancer, siblings_cancer, taking_chlor_metaformin, taking_insulin,
  sleep_hours, diet_freq_soybean, diet_freq_dairy, ever_lived_with_smoker,
  lived_with_smoker_years, children_cancer,
  self_rated_health, f_ratio, copd
)

# 合并基线与结局
sd <- merge(sl_bq, sl_end, by = "csid")
rm(bq, ep, sl_bq, sl_end)

# 排除基线癌症（非癌症人群）
table(sd$cancer_diag == 1)
a <- sd[sd$cancer_diag == 0, ]

# ================= 变量定义 =================

# 病程（年）
a$duration <- (a$age_at_study_date_x100 / 100 - a$diabetes_diag_age)

a$duration_new[is.na(a$diabetes_diag_age)] <- 0
a$duration_new[a$duration < 5] <- 1
a$duration_new[a$duration >= 5 & a$duration < 10] <- 2
a$duration_new[a$duration >= 10 & a$duration < 15] <- 3
a$duration_new[a$duration >= 15] <- 4
a$duration_new <- as.factor(a$duration_new)

a$duration_new1[is.na(a$diabetes_diag_age)] <- 0
a$duration_new1[a$duration < 5] <- 1
a$duration_new1[a$duration >= 5 & a$duration < 10] <- 2
a$duration_new1[a$duration >= 10] <- 3
a$duration_new1 <- as.factor(a$duration_new1)

# 随机血糖分类
a$random[a$random_glucose_x10 / 10 < 7.0] <- 0
a$random[a$random_glucose_x10 / 10 >= 7.0 & a$random_glucose_x10 / 10 < 9.0]  <- 1
a$random[a$random_glucose_x10 / 10 >= 9.0 & a$random_glucose_x10 / 10 < 11.0] <- 2
a$random[a$random_glucose_x10 / 10 >= 11.0 & a$random_glucose_x10 / 10 < 13.0] <- 3
a$random[a$random_glucose_x10 / 10 >= 13.0 & a$random_glucose_x10 / 10 < 15.0] <- 4
a$random[a$random_glucose_x10 / 10 >= 15.0] <- 5
a$random <- as.factor(a$random)

# 地区
a$region[a$region_code == 12] <- 0
a$region[a$region_code == 16] <- 1
a$region[a$region_code == 26] <- 2
a$region[a$region_code == 36] <- 3
a$region[a$region_code == 46] <- 4
a$region[a$region_code == 52] <- 5
a$region[a$region_code == 58] <- 6
a$region[a$region_code == 68] <- 7
a$region[a$region_code == 78] <- 8
a$region[a$region_code == 88] <- 9
a$region <- as.factor(a$region)

# 婚姻
a$marital_status <- as.factor(a$marital_status)

# 文化程度
a$highest_education_new[a$highest_education == 0] <- 0
a$highest_education_new[a$highest_education == 1] <- 1
a$highest_education_new[a$highest_education == 2] <- 1
a$highest_education_new[a$highest_education == 3] <- 2
a$highest_education_new[a$highest_education == 4] <- 2
a$highest_education_new[a$highest_education == 5] <- 2
a$highest_education_new <- as.factor(a$highest_education_new)

# 饮酒
a$alcohol_category_new[a$alcohol_category == 1] <- 0
a$alcohol_category_new[a$alcohol_category == 2] <- 1
a$alcohol_category_new[a$alcohol_category == 3] <- 2
a$alcohol_category_new[a$alcohol_category == 4] <- 2
a$alcohol_category_new[a$alcohol_category == 5] <- 2
a$alcohol_category_new[a$alcohol_category == 6] <- 3
a$alcohol_category_new <- as.factor(a$alcohol_category_new)

# 吸烟
a$smoking_new[a$smoking_category == 1] <- 0
a$smoking_new[a$smoking_category == 2] <- 0
a$smoking_new[a$smoking_stopped_years_ago >= 5] <- 1
a$smoking_new[a$smoking_stopped_years_ago < 5] <- 2
a$smoking_new[a$smoking_today_amount < 15] <- 3
a$smoking_new[a$smoking_today_amount >= 15 & a$smoking_today_amount <= 24] <- 4
a$smoking_new[a$smoking_today_amount >= 25] <- 5
a$smoking_new[is.na(a$smoking_new)] <- 6
a$smoking_new <- as.factor(a$smoking_new)

# 红肉频率
a$meat_new[a$diet_freq_meat == 0] <- 9
a$meat_new[a$diet_freq_meat == 1] <- 8
a$meat_new[a$diet_freq_meat == 2] <- 7
a$meat_new[a$diet_freq_meat == 3] <- 6
a$meat_new[a$diet_freq_meat == 4] <- 5
a$meat_new <- as.factor(a$meat_new)

# 水果频率
a$fruit_new[a$diet_freq_fresh_fruit == 0] <- 9
a$fruit_new[a$diet_freq_fresh_fruit == 1] <- 8
a$fruit_new[a$diet_freq_fresh_fruit == 2] <- 7
a$fruit_new[a$diet_freq_fresh_fruit == 3] <- 6
a$fruit_new[a$diet_freq_fresh_fruit == 4] <- 5
a$fruit_new <- as.factor(a$fruit_new)

# 蔬菜频率
a$veg_new[a$diet_freq_fresh_veg == 0] <- 9
a$veg_new[a$diet_freq_fresh_veg == 1] <- 8
a$veg_new[a$diet_freq_fresh_veg == 2] <- 7
a$veg_new[a$diet_freq_fresh_veg == 3] <- 6
a$veg_new[a$diet_freq_fresh_veg == 4] <- 5
a$veg_new <- as.factor(a$veg_new)

# 肿瘤家族史
a$cancer_history[a$mother_cancer == 0 & a$father_cancer == 0 &
                   a$siblings_cancer == 0 & a$children_cancer == 0] <- 0
a$cancer_history[a$mother_cancer == 1 | a$father_cancer == 1 |
                   a$siblings_cancer == 1 | a$children_cancer == 1] <- 1
a$cancer_history[is.na(a$cancer_history)] <- 2
a$cancer_history <- as.factor(a$cancer_history)

# 被动吸烟
a$passive_smoking[a$ever_lived_with_smoker == 0] <- 0
a$passive_smoking[a$lived_with_smoker_years < 20] <- 1
a$passive_smoking[a$lived_with_smoker_years >= 20] <- 2
a$passive_smoking <- as.factor(a$passive_smoking)

# 降糖药
a$metaformin[a$taking_chlor_metaformin == 0] <- 0
a$metaformin[a$taking_chlor_metaformin == 1] <- 1
a$metaformin[is.na(a$taking_chlor_metaformin)] <- 2
a$metaformin <- as.factor(a$metaformin)

# 胰岛素
a$insulin[a$taking_insulin == 0] <- 0
a$insulin[a$taking_insulin == 1] <- 1
a$insulin[is.na(a$taking_insulin)] <- 2
a$insulin <- as.factor(a$insulin)

# BMI 缺失填补（用总体均值 23.66）
a$bmi_calc[is.na(a$bmi_calc)] <- 23.66

# ================= 暴露处理 =================

# 根据空腹时间判断血糖
a$FBG <- ifelse(a$hours_since_last_ate_x10 >= 80, a$random_glucose_x10, NA)
a$RBG <- ifelse(a$hours_since_last_ate_x10 < 80,  a$random_glucose_x10, NA)

# 糖尿病状态（无 pre-diabetes）
# 0: 非糖尿病且不满足血糖诊断; 1: 检出糖尿病; 2: 自报糖尿病
a$diabetes_status[a$has_diabetes == 0 &
                    ((a$FBG >= 61 & a$FBG < 70) |
                       (a$fasting_glucose_x10 >= 61 & a$fasting_glucose_x10 < 70))] <- 1
a$diabetes_status[a$has_diabetes == 1] <- 2
a$diabetes_status[is.na(a$diabetes_status)] <- 0
a$diabetes_status <- as.factor(a$diabetes_status)

# ================= 结局处理 =================

# 肺癌结局与随访时间
a$ep_CKB0021_combined_datedeveloped <- as.Date(a$ep_CKB0021_combined_datedeveloped)
a$study_date <- as.Date(a$study_date)

a$ts_inctime <- (a$ep_CKB0021_combined_datedeveloped - a$study_date) / 365  # 年

a$lung[a$ep_CKB0021_combined_ep == 1] <- 1
a$lung[a$ep_CKB0021_combined_ep == 0] <- 0

a$time_lung <- as.numeric(a$ep_CKB0021_combined_datedeveloped - a$study_date)  # 天

# ================= 糖尿病状态与肺癌 =================

table(a$diabetes_status)
table(a$diabetes_status, a$lung)
aggregate(a$time_lung / 365, by = list(type = a$diabetes_status), sum)

# model 1
model_lung1 <- coxph(
  Surv(time_lung, lung) ~ diabetes_status + is_female +
    age_at_study_date_x100 + strata(region),
  data = a
)
summary(model_lung1)

# model 2
model_lung2 <- coxph(
  Surv(time_lung, lung) ~ diabetes_status + is_female +
    age_at_study_date_x100 + strata(region) +
    marital_status + highest_education_new +
    alcohol_category_new + smoking_new +
    diet_freq_meat + diet_freq_fresh_fruit + diet_freq_fresh_veg +
    bmi_calc + met + metaformin + insulin + cancer_history,
  data = a
)
summary(model_lung2)

# PH 假设检验 & 图（model 1）
model_lung1_test <- cox.zph(model_lung1)
print(model_lung1_test)
ggcoxzph(
  model_lung1_test, resid = TRUE, se = TRUE,
  point.col = "red", point.size = 1
)

# ================= 糖尿病人群子集 =================

dm <- a[a$diabetes_status == 2, ]

# 病程缺失置 0
dm$duration[is.na(dm$duration)] <- 0
bc <- dm[dm$duration >= 0, ]

table(bc$duration_new)
table(bc$duration_new, bc$lung)
aggregate(bc$time_lung / 365, by = list(type = bc$duration_new), sum)

# ------- 病程分组模型（duration_new） -------

# model 1
model_lung1 <- coxph(
  Surv(time_lung, lung) ~ duration_new + is_female +
    age_at_study_date_x100 + strata(region),
  data = bc
)
summary(model_lung1)

# 趋势性检验（duration_new 数值化）
model_lung1_trend <- coxph(
  Surv(time_lung, lung) ~ as.numeric(duration_new) +
    is_female + age_at_study_date_x100 + strata(region),
  data = bc
)
summary(model_lung1_trend)

# per 1-year duration
model_lung1_per <- coxph(
  Surv(time_lung, lung) ~ duration + is_female +
    age_at_study_date_x100 + strata(region),
  data = bc
)
summary(model_lung1_per)

# model 2
model_lung2 <- coxph(
  Surv(time_lung, lung) ~ duration_new + is_female +
    age_at_study_date_x100 + strata(region) +
    marital_status + highest_education_new +
    alcohol_category_new + smoking_new +
    diet_freq_meat + diet_freq_fresh_fruit + diet_freq_fresh_veg +
    bmi_calc + met + metaformin + insulin + cancer_history,
  data = bc
)
summary(model_lung2)

# PH 检验
model_lung2_test <- cox.zph(model_lung2)
print(model_lung2_test)
ggcoxzph(
  model_lung2_test, resid = TRUE, se = TRUE,
  point.col = "red", point.size = 1
)

# ================= 发病年龄（首次确诊年龄） =================

# 替换发病年龄：缺失用基线年龄
dm$diab_age <- ifelse(
  is.na(dm$diabetes_diag_age),
  dm$age_at_study_date_x100 / 100,
  dm$diabetes_diag_age
)

# 发病年龄分组
dm$age_new[dm$diab_age >= 60]                       <- 0
dm$age_new[dm$diab_age >= 50 & dm$diab_age < 60]   <- 1
dm$age_new[dm$diab_age >= 40 & dm$diab_age < 50]   <- 2
dm$age_new[dm$diab_age < 40]                       <- 3
dm$age_new <- as.factor(dm$age_new)

dm <- dm[dm$duration >= 0, ]

table(dm$age_new)
table(dm$age_new, dm$lung)
aggregate(dm$time_lung / 365, by = list(type = dm$age_new), sum)

# model 1
model_lung1 <- coxph(
  Surv(time_lung, lung) ~ age_new + is_female +
    age_at_study_date_x100 + strata(region),
  data = dm
)
summary(model_lung1)

# 趋势性检验
model_lung1_trend <- coxph(
  Surv(time_lung, lung) ~ as.numeric(age_new) +
    is_female + age_at_study_date_x100 + strata(region),
  data = dm
)
summary(model_lung1_trend)

# per 1-year diab_age
model_lung1_per <- coxph(
  Surv(time_lung, lung) ~ diab_age + is_female +
    age_at_study_date_x100 + strata(region),
  data = dm
)
summary(model_lung1_per)

# model 2
model_lung2 <- coxph(
  Surv(time_lung, lung) ~ age_new + is_female +
    age_at_study_date_x100 + strata(region) +
    marital_status + highest_education_new +
    alcohol_category_new + smoking_new +
    diet_freq_meat + diet_freq_fresh_fruit + diet_freq_fresh_veg +
    bmi_calc + met + metaformin + insulin + cancer_history,
  data = dm
)
summary(model_lung2)

# PH 检验
model_lung2_test <- cox.zph(model_lung2)
print(model_lung2_test)
ggcoxzph(
  model_lung2_test, resid = TRUE, se = TRUE,
  point.col = "red", point.size = 1
)

# ================= 16000 子样本：病程 + 发病年龄 =================

wq <- dm[!is.na(dm$duration), ]
wq <- wq[wq$duration > 0, ]   # duration > 0

# 病程重新分组（0–<5, 5–<10, 10–<15, ≥15）
wq$duration_n[wq$duration < 5]                      <- 0
wq$duration_n[wq$duration >= 5 & wq$duration < 10]  <- 1
wq$duration_n[wq$duration >= 10 & wq$duration < 15] <- 2
wq$duration_n[wq$duration >= 15]                    <- 3
wq$duration_n <- as.factor(wq$duration_n)

bc <- wq

table(bc$duration_n)
table(bc$duration_n, bc$lung)
aggregate(bc$time_lung / 365, by = list(type = bc$duration_n), sum)

# model 1（duration_n）
model_lung1 <- coxph(
  Surv(time_lung, lung) ~ duration_n + is_female +
    age_at_study_date_x100 + strata(region),
  data = bc
)
summary(model_lung1)

# 趋势性检验（duration_n）
model_lung1_trend <- coxph(
  Surv(time_lung, lung) ~ as.numeric(duration_n) +
    is_female + age_at_study_date_x100 + strata(region),
  data = bc
)
summary(model_lung1_trend)

# per 1-year duration
model_lung1_per <- coxph(
  Surv(time_lung, lung) ~ duration + is_female +
    age_at_study_date_x100 + strata(region),
  data = bc
)
summary(model_lung1_per)

# model 2
model_lung2 <- coxph(
  Surv(time_lung, lung) ~ duration_n + is_female +
    age_at_study_date_x100 + strata(region) +
    marital_status + highest_education_new +
    alcohol_category_new + smoking_new +
    diet_freq_meat + diet_freq_fresh_fruit + diet_freq_fresh_veg +
    bmi_calc + met + metaformin + insulin + cancer_history,
  data = bc
)
summary(model_lung2)

# PH 检验
model_lung2_test <- cox.zph(model_lung2)
print(model_lung2_test)
ggcoxzph(
  model_lung2_test, resid = TRUE, se = TRUE,
  point.col = "red", point.size = 1
)

# 发病年龄分组（在 bc 上）
bc$diab_age <- dm$diab_age[match(bc$csid, dm$csid)]  # 确保有 diab_age

bc$age_new[bc$diab_age >= 60]                        <- 0
bc$age_new[bc$diab_age >= 50 & bc$diab_age < 60]    <- 1
bc$age_new[bc$diab_age >= 40 & bc$diab_age < 50]    <- 2
bc$age_new[bc$diab_age < 40]                        <- 3
bc$age_new <- as.factor(bc$age_new)

table(bc$age_new)
table(bc$age_new, bc$lung)
aggregate(bc$time_lung / 365, by = list(type = bc$age_new), sum)

# model 1（age_new）
model_lung1 <- coxph(
  Surv(time_lung, lung) ~ age_new + is_female +
    age_at_study_date_x100 + strata(region),
  data = bc
)
summary(model_lung1)

# 趋势性检验（age_new）
model_lung1_trend <- coxph(
  Surv(time_lung, lung) ~ as.numeric(age_new) +
    is_female + age_at_study_date_x100 + strata(region),
  data = bc
)
summary(model_lung1_trend)

# per 1-year diab_age
model_lung1_per <- coxph(
  Surv(time_lung, lung) ~ diab_age + is_female +
    age_at_study_date_x100 + strata(region),
  data = bc
)
summary(model_lung1_per)

# model 2（age_new）
model_lung2 <- coxph(
  Surv(time_lung, lung) ~ age_new + is_female +
    age_at_study_date_x100 + strata(region) +
    marital_status + highest_education_new +
    alcohol_category_new + smoking_new +
    diet_freq_meat + diet_freq_fresh_fruit + diet_freq_fresh_veg +
    bmi_calc + met + metaformin + insulin + cancer_history,
  data = bc
)
summary(model_lung2)

# PH 检验（age_new）
model_lung2_test <- cox.zph(model_lung2)
print(model_lung2_test)
ggcoxzph(
  model_lung2_test, resid = TRUE, se = TRUE,
  point.col = "red", point.size = 1
)

# 趋势性检验 & per 1-year diab_age（多变量）
model_lung2_trend <- coxph(
  Surv(time_lung, lung) ~ as.numeric(age_new) +
    is_female + age_at_study_date_x100 + strata(region) +
    marital_status + highest_education_new +
    alcohol_category_new + smoking_new +
    diet_freq_meat + diet_freq_fresh_fruit + diet_freq_fresh_veg +
    bmi_calc + met + metaformin + insulin + cancer_history,
  data = bc
)
summary(model_lung2_trend)

model_lung2_per <- coxph(
  Surv(time_lung, lung) ~ diab_age + is_female +
    age_at_study_date_x100 + strata(region) +
    marital_status + highest_education_new +
    alcohol_category_new + smoking_new +
    diet_freq_meat + diet_freq_fresh_fruit + diet_freq_fresh_veg +
    bmi_calc + met + metaformin + insulin + cancer_history,
  data = bc
)
summary(model_lung2_per)
