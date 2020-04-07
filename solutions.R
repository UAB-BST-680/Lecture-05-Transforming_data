
# Load packages 
library(tidyverse)  # includes dplyr - filter / select
library(knitr)      # for kable
library(kableExtra) # for kable_styling and other utilities
library(tblStrings)

nhanes <- read_rds('data/NHANES_analysis_post_exclusions.rds')


# exercise 1 --------------------------------------------------------------

sbp_smry <- nhanes %>% 
  group_by(exam, sex) %>% 
  summarise(
    sbp_mn = mean(bp_sys_mmhg),
    sbp_se = sd(bp_sys_mmhg) / sqrt(n())
  )

# show this in class
nhanes %>% 
  group_by(exam, sex) %>% 
  summarise(across(
    .cols = c(bp_sys_mmhg, bp_dia_mmhg),
    .fns = list(
      mn = ~mean(.x), 
      se = ~sd(.x) / sqrt(n())
    )
  ))

# all numerics
nhanes %>% 
  group_by(exam, sex) %>% 
  summarise(across(
    .cols = is.numeric,
    .fns = list(
      mn = ~mean(.x), 
      se = ~sd(.x) / sqrt(n())
    )
  ))

# some numerics
nhanes %>% 
  group_by(exam, sex) %>% 
  summarise(across(
    .cols = c(is.numeric, -seqn, -psu, -strata, -wts_mec_2yr),
    .fns = list(
      mn = ~mean(.x), 
      se = ~sd(.x) / sqrt(n())
    )
  ))

write_rds(sbp_smry, 'solutions/01_solution.rds')

# exercise 2 --------------------------------------------------------------

p <- ggplot(sbp_smry) +
  aes(
    x = exam,
    y = sbp_mn,
    ymin = sbp_mn - sbp_se*2,
    ymax = sbp_mn + sbp_se*2,
    col = sex,
    fill = sex,
    group = sex
  ) +
  geom_line() +
  geom_errorbar(width = 0) +
  geom_point(shape = 21, col = 'black', size = 4) +
  theme_bw() +
  theme(text = element_text(size = 16)) +
  labs(x = 'NHANES Exam', y = 'Mean systolic blood pressure, mm Hg') + 
  scale_color_brewer(palette = 'Dark2') + 
  scale_fill_brewer(palette = 'Dark2')

write_rds(p, 'solutions/02_solution.rds')

# exercise 3 --------------------------------------------------------------

htn_smry <- nhanes %>% 
  group_by(exam, sex, race_ethnicity) %>% 
  mutate(htn = bp_sys_mmhg > 140 | bp_dia_mmhg > 90 | bp_meds == 'Yes') %>% 
  summarise(htn = mean(htn)) %>% 
  group_by(race_ethnicity) 

write_rds(htn_smry, 'solutions/03_solution.rds')

# show this to class ------------------------------------------------------

htn_tbl <- pivot_wider(htn_smry, names_from = exam, values_from = htn)

# exercise 4 --------------------------------------------------------------

library(gt)

exam_years <- c(
  "1999",
  "2001",
  "2003",
  "2005",
  "2007",
  "2009",
  "2011",
  "2013",
  "2015",
  "2017"
)

htn_gt <- htn_tbl %>% 
  group_by(sex) %>% 
  gt(rowname_col = 'race_ethnicity') %>% 
  fmt_percent(columns = exam_years, decimals = 1) %>% 
  fmt_missing(columns = exam_years) %>% 
  summary_rows(
    columns = exam_years, 
    groups = TRUE,
    fns = list('Overall' = ~mean(.x, na.rm = TRUE)), 
    formatter = fmt_percent
  ) %>% 
  tab_header("Trends in the prevalence of hypertension",
    subtitle = "Unweighted results from ten NHANES exams") %>% 
  tab_spanner(columns = exam_years, label = 'Exam year')

write_rds(htn_gt, 'solutions/04_solution.rds')

# exercise 5 --------------------------------------------------------------

htn_compare <- htn_smry %>% 
  group_by(sex, race_ethnicity) %>% 
  filter(exam %in% c('2013', '2017')) %>% 
  summarise(
    htn_2013 = htn[exam == '2013'],
    htn_2017 = htn[exam == '2017']
  ) %>% 
  mutate(
    increase_abs = htn_2017 - htn_2013,
    increase_prc = htn_2017 / htn_2013,
    increase_prc = increase_prc - 1
  ) %>% 
  arrange(sex, desc(increase_abs))

write_rds(htn_compare, 'solutions/05_solution.rds')

# exercise 6 --------------------------------------------------------------

gt_compare <- htn_compare %>% 
  gt(groupname_col = 'sex', rowname_col = 'race_ethnicity') %>% 
  tab_spanner(
    columns = vars(htn_2013, htn_2017), 
    label = 'Prevalence of hypertension'
  ) %>% 
  tab_spanner(columns = vars(increase_prc, increase_abs),
    label = 'Increase in prevalence') %>%
  cols_label(
    htn_2013 = '2013',
    htn_2017 = '2017',
    increase_prc = 'Relative',
    increase_abs = 'Absolute'
  ) %>% 
  fmt_percent(
    columns = vars(
      htn_2013,
      htn_2017,
      increase_prc,
      increase_abs
    ),
    decimals = 1 
  ) %>% 
  data_color(
    columns = vars(increase_abs),
    colors = scales::col_numeric(
      palette = c("white", "orange"),
      domain = c(
        min(htn_compare$increase_abs), 
        max(htn_compare$increase_abs)
      )
    )
  )

write_rds(gt_compare, 'solutions/06_solution.rds')

