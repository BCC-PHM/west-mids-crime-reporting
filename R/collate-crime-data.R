# Collate West Midlands crime reporting data
# Data from https://data.police.uk/data/archive/
library(dplyr)
library(ggplot2)

# List all crime reporting files
files <- list.files(path = "data/all-crime-reporting/", 
           pattern = "\\west-midlands-street.csv$", 
           recursive = TRUE, 
           full.names = TRUE)

# Get all West Mids 2011 LSOAs
wm_lsoas11 <- read.csv("data/West Midlands postcodes.csv") %>%
  rename(
    LSOA11 = LSOA.Code,
    LocalAuthority11 = District
    ) %>%
  select(c(LocalAuthority11, LSOA11)) %>%
  distinct()

# Get all West Mids 2021 LSOAs
wm_lsoas21 <- read.csv("data/West Midlands postcodes.csv") %>%
  rename(
    LSOA21 = LSOA21.Code,
    LocalAuthority21 = District
  ) %>%
  select(c(LocalAuthority21, LSOA21)) %>%
  distinct()

######################################################################
#         Load and process West Mids crime reporting data            #
######################################################################

# Load all West Midlands crime reporting files
data_list <- list()
for (file_i in files) {
  data_list[[file_i]] <- read.csv(file_i)
}

# combine crime reporting files and process data
data <- data.table::rbindlist(data_list) %>%
  select(Month, Longitude, Latitude, LSOA.code, Crime.type) %>%
  rename(
    MonthYear = Month,
    LSOA11 = LSOA.code, 
    CrimeType = Crime.type
  ) %>% 
  tidyr::separate_wider_delim(
    MonthYear, 
    delim = "-", 
    names = c("Year", "Month")
    ) %>%
  mutate(
    DateStart = as.Date(paste0(Year,"-", Month, "-", "01"))
  ) %>%
  # Join data to LA by either LSOA11 or LSOA21
  left_join(
    wm_lsoas11, 
    by = join_by(LSOA11)
  ) %>%
  left_join(
    wm_lsoas21, 
    by = join_by(LSOA11 == LSOA21)
  ) %>%
  mutate(
    LocalAuthority = case_when(
      !is.na(LocalAuthority11) ~ LocalAuthority11,
      !is.na(LocalAuthority21) ~ LocalAuthority21,
      is.na(LSOA11) ~ "Area not recorded",
      TRUE ~ "Not in West Mids"
    )
  ) %>%
  select(
    -c(LocalAuthority11, LocalAuthority21)
  )

######################################################################
#           Save data for all of West Mids and each LA               #
######################################################################

write.csv(data, "output/reporting-data/west-mids-reported-crimes.csv")

for (LA_i in unique(data$LocalAuthority)) {
  # Filter data for this LA
  data_i <- data %>%
    filter(
      LocalAuthority == LA_i
    )
  # Save filtered data
  write.csv(
    data_i, 
    paste0("output/reporting-data/",
           stringr::str_replace_all(LA_i, " ", "-"),
           "-reported-crimes.csv"
           )
  )
}



######################################################################
#             Plot monthly crime in biggest LAs as check             #
######################################################################

crime_check <- data %>%
  filter(
    LocalAuthority %in% c("Birmingham", "Coventry", "Sandwell",
                          "Walsall", "Wolverhampton","Dudley")
  ) %>%
  count(DateStart, LocalAuthority)

plt <- ggplot(crime_check, aes(x = DateStart, y = n, color = LocalAuthority)) + 
  geom_line() +
  theme_bw() +
  labs(
    x = "Date",
    y = "Monthly number of crimes reported in West Midlands\n(2011 - 2024)"
  )
plt

ggsave(plt, file = "output/crime-check.png", width = 6, height = 4)

  