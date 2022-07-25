library(here)
library(tidyverse)
library(readxl)
library(meta)

pprv <- read_xlsx(here("data", "systematic_review_data.xlsx")) %>%
  select(Authors, `Target population`, `Test type` = Test_type, Reference = Refrence, Cutoff, `Sample size`, TP, TN, FP, FN) %>%
  mutate(`Test type` = case_when(`Test type` == "ElISA" ~ "ELISA",
                               TRUE ~ str_trim(`Test type`)),
         Reference = str_trim(Reference),
         `Target population` = case_when(str_detect(`Target population`, "catttle") ~ "Sheep, goat and cattle",
                                         TRUE ~ str_trim(`Target population`)),
         Authors = case_when(Authors == "Singh (2004)" & `Target population` == "Sheep, goat and cattle" ~ "Singh (2004a)",
                             Authors == "Singh (2004)" & `Target population` == "Unclear" ~ "Singh (2004b)",
                             TRUE ~ str_trim(Authors))) %>%
  arrange(Authors)

eligible_tests <- pprv %>%
  group_by(`Test type`) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  pull(`Test type`)

sensitivity_logit <- pprv %>%
  filter(`Test type` %in% c(eligible_tests)) %>%
  metaprop(TP, TP + FN,
           method.incr = "only0",
           method.ci = "CP",
           sm = "PLOGIT",
           common = FALSE,
           random = TRUE,
           studlab = Authors,
           byvar = `Test type`,
           data = .)

pdf(file = here("output", "sensitivity_forest.pdf"), width = 10, height = 8)

forest.meta(sensitivity_logit, digits = 3,
            rightcols = c("effect", "ci"),
            leftlabs = c("Study", "TP", "TP + FN"),
            xlab = "Sensitivity",
            prediction = TRUE,
            colgap.forest.left = "4cm")
dev.off()

specificity_logit <- pprv %>%
  filter(`Test type` %in% c(eligible_tests)) %>%
  metaprop(TN, TN + FP,
           method.incr = "only0",
           method.ci = "CP",
           sm = "PLOGIT",
           common = FALSE,
           random = TRUE,
           studlab = Authors,
           byvar = `Test type`,
           data = .)

pdf(file = here("output", "specificty_forest.pdf"), width = 10, height = 8)

forest.meta(specificity_logit, digits = 3,
            rightcols = c("effect", "ci"),
            leftlabs = c("Study", "TN", "TN + FP"),
            xlab = "Specificity",
            prediction = TRUE,
            colgap.forest.left = "4cm")
dev.off()
