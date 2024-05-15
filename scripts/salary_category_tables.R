## Salary Category Tables
library(tidyverse)

## 2002 to 2021 ---

salarycategories_2002_2021 <- data.frame(
    incomecategory = 1:16,
    periodweekly = c("none", "1-46","47-115","116-231","232-346","347-577","578-808","809-1039","1040-1386","1387-1848","1849-2540","2541-3695","3696-6928","6929 or more", "don't know", "refuse"),
    periodmonthly = c(
        "none", "1-200", "201-500", "501-1000", "1001-1500",
        "1501-2500", "2501-3500", "3501-4500", "4501-6000",
        "6001-8000", "8001-11000", "11001-16000", "16001-30000",
        "30001 or more", "don't know", "refuse"),
    monthly = c(
        "none", "1-200", "201-500", "501–1000", "1001-1500",
        "1501-2500", "2501-3500", "3501-4500", "4501-6000",
        "6001-8000", "8001-11000", "11001-16000", "16001-30000",
        "30001 or more", "don't know", "refuse"),
    periodannually = c("none", "1-2400", "2401-6000", "6001-12000", "12001-18000",
                 "18001 - 30000", "30001-42000", "42001 - 54000", "54001 - 72000",
                 "72001-96000", "96001-132000", "132001-192000", "192001-360000",
                 "360001 or more", "don't know", "refuse")
) %>%
    separate(monthly, into = c("monthly_lower_bound", "monthly_upper_bound"), sep = "[-–]") %>%
    mutate(
        monthly_lower_bound = case_when(
            monthly_lower_bound == "none" | monthly_lower_bound == "don't know" | monthly_lower_bound == "refuse" ~ NA_real_,
            monthly_lower_bound == "30001 or more" ~ 30001,
            TRUE ~ as.numeric(monthly_lower_bound)
        ),
        monthly_upper_bound = case_when(
            monthly_upper_bound == "none" | monthly_upper_bound == "don't know" | monthly_upper_bound == "refuse" ~ NA_real_,
            periodmonthly == "30001 or more" ~ 45000,
            TRUE ~ as.numeric(monthly_upper_bound)
        )
    ) %>%
    mutate(monthlymidpoint = (monthly_lower_bound + monthly_upper_bound)/2) %>%
    mutate(incomecategory = as.character(incomecategory)
           )

# Only from 2019 are the categories different, but even then -- only the daily categories. Monthly and annually are the same.

