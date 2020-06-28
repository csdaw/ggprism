library(readr)
library(tidyr)

wings <- read_csv(
  "data-raw/datasets/wings.csv",
  col_types =
    list(
      sex = col_factor(c("male", "female")),
      genotype = col_factor(c("Tps1MIC/+", "Tps1MIC"))
    )
) %>%
  pivot_longer(names_to = "measure",
               values_to = "percent.change",
               cols = c(wing.size, cell.size, cell.number),
               names_ptypes = list(measure = factor()))

usethis::use_data(wings, overwrite = TRUE)
