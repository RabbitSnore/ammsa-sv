################################################################################

# AMMSA Swedish Validation -- Initial Cleaning

################################################################################

# Setup ------------------------------------------------------------------------

packages <- c("dplyr", "readr")

lapply(packages, library, character.only = TRUE)

# Load raw data ----------------------------------------------------------------

ammsa <- read_csv("./data/ammsa_data_raw.csv") %>% 
  slice(-1, -2)

# Cleaning ---------------------------------------------------------------------

# Remove unfinished surveys

ammsa_clean <- ammsa %>% 
  filter(Progress == 100)

# Remove unnecessary columns

ammsa_clean <- ammsa_clean %>% 
  mutate(
    id = 1:nrow(.)
  ) %>% 
  select(
    id,
    `Duration (in seconds)`,
    order,
    starts_with("AMMSA"),
    starts_with("ASI"),
    starts_with("IRMA"),
    starts_with("SC"),
    starts_with("SDO"),
    starts_with("ATT"),
    AGE,
    starts_with("DEM"),
  ) %>% 
  rename(
    duration = `Duration (in seconds)`
  )

# Remove attention check failures

# ammsa_clean <- ammsa_clean %>% 
#   filter(ATT_CONTROL_8_TEXT == "+")

# Set all variable names to lowercase

colnames(ammsa_clean) <- tolower(colnames(ammsa_clean))

# Export data

## This data file includes those who failed the attention checks

write.csv(ammsa_clean, "ammsa_data_clean.csv", row.names = FALSE)
