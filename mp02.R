# Library Import:

library(dplyr)
library(readr)
library(tidyverse)
library(DT)
library(stringr)

#Dataset import

pop_data <- read_csv("B01003_001_cbsa_2009_2023.csv")
households_data <- read_csv("B11001_001_cbsa_2009_2023.csv")
income_data <- read_csv("B19013_001_cbsa_2009_2023.csv")
rent_data <- read_csv("B25064_001_cbsa_2009_2023.csv")
permits_data <- read_csv("housing_units_2009_2023.csv")
qcew_data <- read_csv("bls_qcew_2009_2023.csv")
naics_codes <- read_csv("bls_industry_codes.csv")

# Q1: Which CBSA (by name) permitted the largest number of new housing units in the decade from 2010 to 2019 (inclusive)?:

library(dplyr)
library(readr)

# Load the datasets
# B01003_001_cbsa_2009_2023.csv contains CBSA names (NAME) and IDs (GEOID)
pop_data <- read_csv("B01003_001_cbsa_2009_2023.csv")
# housing_units_2009_2023.csv contains the permitted housing units
permits_data <- read_csv("housing_units_2009_2023.csv")

# 1. Get the distinct CBSA names and IDs
cbsa_names <- pop_data %>%
  select(GEOID, NAME) %>%
  distinct()

# 2. Process the permits data to find the largest total
largest_permitting_cbsa <- permits_data %>%
  # Rename CBSA column for consistent joining (match GEOID)
  rename(GEOID = CBSA) %>%
  # Filter for the decade 2010 to 2019 (inclusive)
  filter(year >= 2010 & year <= 2019) %>%
  # Group by CBSA ID
  group_by(GEOID) %>%
  # Sum the new housing units permitted over the decade
  summarise(
    total_permits_2010_2019 = sum(new_housing_units_permitted, na.rm = TRUE),
    .groups = 'drop' # Drop the grouping structure after summarizing
  ) %>%
  # Join with the names table to get the full CBSA name
  inner_join(cbsa_names, by = "GEOID") %>%
  # Arrange in descending order of total permits
  arrange(desc(total_permits_2010_2019)) %>%
  # Select the top result (the CBSA with the largest total)
  slice_head(n = 1)

# Print the final result
print(largest_permitting_cbsa)


#Q2: In what year did Albuquerque, NM (CBSA Number 10740) permit the most new housing units?

# Load the housing permits data
permits_data <- read_csv("housing_units_2009_2023.csv")

albuquerque_max_permits <- permits_data %>%
  # Filter for Albuquerque, NM's CBSA ID (10740)
  filter(CBSA == 10740) %>%
  # Arrange the data in descending order of permitted units
  arrange(desc(new_housing_units_permitted)) %>%
  # Take the top row, which represents the year with the maximum permits
  slice_head(n = 1) %>%
  # Select just the year and the number of permitted units for the answer
  select(year, new_housing_units_permitted)

# Print the final result
print(albuquerque_max_permits)

#Q3: Which state (not CBSA) had the highest average individual income in 2015?

# Load the necessary datasets
income_data <- read_csv("B19013_001_cbsa_2009_2023.csv")
households_data <- read_csv("B11001_001_cbsa_2009_2023.csv")
population_data <- read_csv("B01003_001_cbsa_2009_2023.csv")

# Set the target year
TARGET_YEAR <- 2015

# Step 1: Filter and join data for 2015
cbsa_data_2015 <- income_data %>%
  filter(year == TARGET_YEAR) %>%
  # Join with households data
  inner_join(
    households_data %>% filter(year == TARGET_YEAR) %>% select(GEOID, B11001_001),
    by = "GEOID"
  ) %>%
  # Join with population data
  inner_join(
    population_data %>% filter(year == TARGET_YEAR) %>% select(GEOID, B01003_001),
    by = "GEOID"
  ) %>%
  # Rename columns for clarity (B19013_001 = Avg Household Income, B11001_001 = Total Households, B01003_001 = Total Population)
  rename(
    avg_household_income = B19013_001,
    total_households = B11001_001,
    total_population_cbsa = B01003_001
  ) %>%
  # Calculate Total Income per CBSA
  mutate(
    total_income_cbsa = avg_household_income * total_households
  )

# Step 2: Extract state(s) from CBSA NAME and un-nest the data
state_level_data <- cbsa_data_2015 %>%
  # Extract state abbreviation(s) from the NAME column (e.g., "TX" from "Dallas... TX Metro Area" or "DC-VA-MD-WV" from "Washington...")
  mutate(
    # Use str_extract to find the state abbreviation(s)
    # The regex captures the state abbreviation(s) just before ' Metro Area' or ' Micro Area'
    state_abbr_string = str_extract(NAME, "(?:([A-Z]{2})(?:-[A-Z]{2})*)?(?=\\s+(?:Metro|Micro)\\s+Area)")
  ) %>%
  # Split the state abbreviation string by the hyphen into a list of states
  # Use mutate(states = str_split(...)) and then unnest(states) in base R's dplyr version.
  # If using tidyr >= 1.0.0, separate_rows can simplify this.
  mutate(
    state_abbr_list = str_split(state_abbr_string, "-")
  ) %>%
  # Convert the list column to individual rows for each state involved
  tidyr::unnest(state_abbr_list) %>%
  rename(state = state_abbr_list) %>%
  # Keep only necessary columns for aggregation
  select(state, total_income_cbsa, total_population_cbsa) %>%
  # Filter out rows where state extraction failed (e.g., 'NA' or 'UNKNOWN')
  filter(!is.na(state))

# Step 3: Aggregate total income and total population by State
final_aggregation <- state_level_data %>%
  group_by(state) %>%
  summarise(
    total_income_state = sum(total_income_cbsa, na.rm = TRUE),
    total_population_state = sum(total_population_cbsa, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Calculate the final average individual income
  mutate(
    avg_individual_income = total_income_state / total_population_state
  ) %>%
  # Find the state with the highest average individual income
  arrange(desc(avg_individual_income)) %>%
  slice_head(n = 1)

# Print the final result
print(final_aggregation)

#Q4: What is the last year in which the NYC CBSA had the most data scientists in the country?

# Load the necessary datasets
pop_data <- read_csv("B01003_001_cbsa_2009_2023.csv")
qcew_data <- read_csv("bls_qcew_2009_2023.csv.gz/bls_qcew_2009_2023.csv")

# NAICS code for Data Scientists and Business Analysts (NAICS 5182: Data Processing, Hosting, and Related Services)
TARGET_NAICS <- '5182'

# --- 1. Filter and Prepare QCEW data (BLS) ---
qcew_prep <- qcew_data %>%
  # Filter for the target NAICS code (using string start)
  filter(str_detect(INDUSTRY, paste0('^', TARGET_NAICS))) %>%
  # Create the standardized CBSA ID for joining (BLS FIPS is e.g., 'C4790', needs 'C47900')
  mutate(std_cbsa = paste0(FIPS, "0")) %>%
  # Rename for clarity and select only necessary columns
  select(year = YEAR, FIPS, INDUSTRY, data_scientist_employment = EMPLOYMENT, std_cbsa) %>%
  # Remove records with zero employment
  filter(data_scientist_employment > 0)

# --- 2. Filter and Prepare Population/Name data (Census) ---
cbsa_names_prep <- pop_data %>%
  select(GEOID, NAME) %>%
  distinct() %>%
  # Create the standardized CBSA ID for joining (Census GEOID is e.g., 47900, needs 'C47900')
  mutate(std_cbsa = paste0("C", GEOID)) %>%
  # Select the original GEOID for final answer check
  select(GEOID, NAME, std_cbsa)

# --- 3. Join the datasets ---
analysis_data <- qcew_prep %>%
  inner_join(cbsa_names_prep, by = "std_cbsa")

# --- 4. Find the top CBSA for each year ---
top_cbsa_per_year <- analysis_data %>%
  group_by(year) %>%
  # Find the row with the maximum employment for that year
  slice_max(data_scientist_employment, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(year, GEOID, NAME, data_scientist_employment) %>%
  arrange(year)

# Print the resulting table
print(top_cbsa_per_year)

# --- 5. Answer the question: Last year NYC was ranked 1st ---
NYC_GEOID <- 35620 # GEOID for New York-Newark-Jersey City, NY-NJ-PA Metro Area

last_nyc_win_year <- top_cbsa_per_year %>%
  filter(GEOID == NYC_GEOID) %>%
  pull(year) %>%
  max(na.rm = TRUE)

# Print the final answer
if (is.infinite(last_nyc_win_year)) {
  cat("\nBased on the available data for NAICS 5182, the New York-Newark-Jersey City CBSA never had the most data scientists in the country between 2009 and 2023.\n")
} else {
  cat(paste("\nThe last year the New York-Newark-Jersey City CBSA had the most data scientists in the country (under NAICS 5182) was:", last_nyc_win_year, "\n"))
}

# Q5: What fraction of total wages in the NYC CBSA was earned by people employed in the finance and insurance industries:

# Load the necessary datasets
qcew_data <- read_csv("bls_qcew_2009_2023.csv.gz")

# Constants for filtering
NYC_FIPS_PREFIX <- "C3562"        # FIPS for NYC CBSA (New York-Newark-Jersey City)
TARGET_NAICS_PREFIX <- "52"       # NAICS code for Finance and Insurance
TOTAL_INDUSTRY_CODE <- "101"      # FIX: Use '101' (Total Private) as it is consistently reported for this CBSA

# 1. Calculate Total Wages per Year for NYC (Denominator)
total_wages_df <- qcew_data %>%
  # Filter for NYC CBSA FIPS and Total Private Industry code ('101')
  filter(FIPS == NYC_FIPS_PREFIX, INDUSTRY == TOTAL_INDUSTRY_CODE) %>%
  select(year = YEAR, total_wages = TOTAL_WAGES)

# 2. Calculate Finance/Insurance Wages per Year for NYC (Numerator)
finance_wages_df <- qcew_data %>%
  # Filter for NYC CBSA FIPS
  filter(FIPS == NYC_FIPS_PREFIX) %>%
  # Filter for codes starting with '52' (Finance and Insurance)
  filter(str_starts(INDUSTRY, TARGET_NAICS_PREFIX)) %>%
  group_by(year = YEAR) %>%
  # Sum the total wages for all 52xx sub-industries
  summarise(finance_wages = sum(TOTAL_WAGES, na.rm = TRUE), .groups = 'drop')

# 3. Join and Calculate the Wage Fraction
wage_fraction_analysis <- total_wages_df %>%
  inner_join(finance_wages_df, by = "year") %>%
  mutate(
    wage_fraction = finance_wages / total_wages,
    wage_fraction_pct = wage_fraction * 100 # For easier reading
  )

# 4. Find the Peak Year and its Fraction
peak_year_result <- wage_fraction_analysis %>%
  arrange(desc(wage_fraction)) %>%
  slice_head(n = 1) %>%
  select(year, wage_fraction_pct)

# Print the final peak result
print(peak_year_result)









#Task 3: visualizations
#setup
library(tidyverse)
library(stringr)

# --- Load Data (Adjust paths if necessary) ---
pop_data <- read_csv("B01003_001_cbsa_2009_2023.csv")
households_data <- read_csv("B11001_001_cbsa_2009_2023.csv")
income_data <- read_csv("B19013_001_cbsa_2009_2023.csv")
rent_data <- read_csv("B25064_001_cbsa_2009_2023.csv")
# Note: Ensure your qcew file path is correct (it may be a .csv or a compressed .gz file)
qcew_data <- read_csv("bls_qcew_2009_2023.csv.gz")





# Relationship between CSBA monthly rent and average household income in 2009 

# Prepare data: Join income and rent for 2009
plot1_data <- income_data %>%
  filter(year == 2009) %>%
  rename(avg_household_income = B19013_001) %>%
  inner_join(
    rent_data %>%
      filter(year == 2009) %>%
      rename(monthly_rent = B25064_001) %>%
      select(GEOID, monthly_rent),
    by = "GEOID"
  )

# Create Visualization
plot1 <- ggplot(plot1_data, aes(x = avg_household_income, y = monthly_rent)) +
  # Add points with some transparency
  geom_point(alpha = 0.6, color = "darkblue") +
  # Add a linear regression line
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Relationship Between Monthly Rent and Median Household Income (2009)",
    x = "Median Household Income (USD)",
    y = "Median Gross Rent (USD)"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

print(plot1)
# ggsave("rent_vs_income_2009.png", plot1, width = 8, height = 6)







# The relationship between total employment and total employment in the health care and social services sector (NAICS 62) across different CBSAs.

library(tidyverse)
library(stringr)

# --- Load Data (Run this setup in RStudio first) ---
pop_data <- read_csv("B01003_001_cbsa_2009_2023.csv")
qcew_data <- read_csv("bls_qcew_2009_2023.csv.gz/bls_qcew_2009_2023.csv")

# Constants
NAICS_62 <- '62'      # Health Care and Social Assistance
INDUSTRY_TOTAL_PRIVATE <- '101' # FIX: Use '101' (Total Private) for the Total Employment denominator

# --- Data Preparation ---
# 1. FIPS to GEOID/Name mapping
cbsa_map <- pop_data %>%
  select(GEOID, NAME) %>%
  distinct() %>%
  # Create the BLS-style FIPS prefix from the Census GEOID (e.g., 47900 -> C4790)
  mutate(FIPS = paste0("C", str_sub(GEOID, 1, 4))) %>%
  select(FIPS, GEOID, NAME) %>%
  distinct()

# 2. Prepare QCEW data
qcew_prep <- qcew_data %>%
  rename(year = YEAR, employment = EMPLOYMENT) %>%
  select(year, FIPS, INDUSTRY, employment)

# 3. Total Employment (Denominator: INDUSTRY == '101') - FIX applied here
total_emp_df <- qcew_prep %>%
  filter(INDUSTRY == INDUSTRY_TOTAL_PRIVATE) %>%
  rename(total_employment = employment) %>%
  select(year, FIPS, total_employment)

# 4. Healthcare Employment (Numerator: INDUSTRY starts with '62')
healthcare_emp_df <- qcew_prep %>%
  filter(str_starts(INDUSTRY, NAICS_62)) %>%
  group_by(year, FIPS) %>%
  summarise(healthcare_employment = sum(employment, na.rm = TRUE), .groups = 'drop')

# 5. Join the employment data and the CBSA names
plot2_data <- total_emp_df %>%
  inner_join(healthcare_emp_df, by = c("year", "FIPS")) %>%
  inner_join(cbsa_map, by = "FIPS") %>%
  filter(total_employment > 0) # Remove zero/missing employment records

# --- Create Visualization ---
plot2 <- ggplot(plot2_data, aes(x = total_employment, y = healthcare_employment, color = as.factor(year))) +
  # Plot points, colored by year
  geom_point(alpha = 0.6) +
  # Add a linear regression line for context
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5, linetype = "dashed", show.legend = FALSE) +
  labs(
    title = "Total Private Employment vs. Healthcare Employment (NAICS 62) Over Time",
    subtitle = "Each point is a CBSA in a specific year.",
    x = "Total Private Employment (NAICS 101)",
    y = "Healthcare & Social Assistance Employment (NAICS 62)",
    color = "Year"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  # Ensure the legend shows solid colors
  guides(color = guide_legend(override.aes = list(alpha = 1)))

print(plot2)
# ggsave("employment_vs_healthcare_over_time_fixed.png", plot2, width = 9, height = 6)




# Evolution of average household size

# --- Data Preparation ---
# Select a few CBSAs (e.g., NYC, LA, Albuquerque) for line plot clarity
TARGET_CBSAS <- c(35620, 31080, 10740) # NYC, LA, Albuquerque

# Calculate Average Household Size (Population / Households)
plot3_data <- pop_data %>%
  rename(total_population = B01003_001) %>%
  select(GEOID, NAME, year, total_population) %>%
  inner_join(
    households_data %>%
      rename(total_households = B11001_001) %>%
      select(GEOID, year, total_households),
    by = c("GEOID", "year")
  ) %>%
  # Filter for the target CBSAs
  filter(GEOID %in% TARGET_CBSAS) %>%
  # Calculate Average Household Size
  mutate(
    avg_household_size = total_population / total_households,
    # Clean up names for a cleaner legend
    clean_name = str_replace(NAME, ", .* Metro Area", "")
  )

# --- Create Visualization ---
plot3 <- ggplot(plot3_data, aes(x = year, y = avg_household_size, group = clean_name, color = clean_name)) +
  # Add lines and points
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Evolution of Average Household Size Over Time",
    subtitle = "For Selected CBSAs (2009-2023)",
    x = "Year",
    y = "Average Household Size (Persons per Household)",
    color = "CBSA"
  ) +
  theme_minimal() +
  # Ensure year ticks are clean and readable
  scale_x_continuous(breaks = seq(min(plot3_data$year), max(plot3_data$year), by = 2))

print(plot3)
# ggsave("avg_household_size_evolution.png", plot3, width = 9, height = 6)






# task 4: Building Indices of housing affordability (rent burden)

# data Joining: 
# --- Load Data (assuming files are in your R working directory) ---
income_data <- read_csv("B19013_001_cbsa_2009_2023.csv")
rent_data <- read_csv("B25064_001_cbsa_2009_2023.csv")

# --- 1. Join Tables and Calculate Raw Ratio ---
rent_burden_data <- income_data %>%
  # Select and rename columns for clarity
  select(GEOID, NAME, year, median_income = B19013_001) %>%
  
  # Join with rent data
  inner_join(
    rent_data %>% 
      select(GEOID, year, median_rent = B25064_001), 
    by = c("GEOID", "year")
  ) %>%
  
  # Calculate the Raw Rent-to-Income Ratio (Annual Rent / Annual Income) * 12.
  mutate(
    raw_ratio = (median_rent * 12) / median_income
  )

# --- 2. Calculate Baseline and Standardize Metric ---

# Find the National Average Raw Ratio in the first year (2009) to use as the baseline
baseline_2009_avg_ratio <- rent_burden_data %>%
  filter(year == 2009) %>%
  # Calculate the average of all CBSA ratios in 2009
  summarise(
    avg_ratio_2009 = mean(raw_ratio, na.rm = TRUE)
  ) %>%
  pull(avg_ratio_2009) # Extract the numeric value

# Calculate the standardized Rent Burden Index (RBI)
rent_burden_analysis <- rent_burden_data %>%
  mutate(
    # RBI: Ratio divided by the 2009 National Average Ratio
    rent_burden_index = raw_ratio / baseline_2009_avg_ratio,
    # Convert raw ratio to percentage for easy interpretation
    raw_ratio_pct = raw_ratio * 100
  ) %>%
  # Select the final output columns
  select(GEOID, NAME, year, median_income, median_rent, raw_ratio_pct, rent_burden_index)

# --- Output the results and baseline for verification ---

# Print the 2009 National Average Ratio
cat(paste("Baseline (2009 National Average Rent-to-Income Ratio):", 
          round(baseline_2009_avg_ratio * 100, 2), "%\n\n"))

# Print the first few rows of the final standardized data
print(head(rent_burden_analysis))

# 'rent_burden_analysis' is now available.

#Rent Burden Analysis:



# --- 1. Data Loading and RBI Metric Calculation ---

# Load the base data
income_data <- read_csv("B19013_001_cbsa_2009_2023.csv")
rent_data <- read_csv("B25064_001_cbsa_2009_2023.csv")

# Calculate Raw Ratio
rent_burden_data <- income_data %>%
  select(GEOID, NAME, year, median_income = B19013_001) %>%
  inner_join(
    rent_data %>%
      select(GEOID, year, median_rent = B25064_001),
    by = c("GEOID", "year")
  ) %>%
  mutate(
    # Raw Ratio: Annual Rent / Annual Income
    raw_ratio = (median_rent * 12) / median_income
  )

# Calculate Baseline (2009 National Average)
baseline_2009_avg_ratio <- rent_burden_data %>%
  filter(year == 2009) %>%
  summarise(avg_ratio_2009 = mean(raw_ratio, na.rm = TRUE)) %>%
  pull(avg_ratio_2009)

# Calculate Rent Burden Index (RBI)
rent_burden_analysis <- rent_burden_data %>%
  mutate(
    # RBI: Times the 2009 National Average Rent Burden
    rent_burden_index = raw_ratio / baseline_2009_avg_ratio,
    raw_ratio_pct = raw_ratio * 100
  ) %>%
  select(GEOID, NAME, year, raw_ratio_pct, rent_burden_index)

TARGET_CBSA_NAME <- "Buffalo-Niagara Falls, NY Metro Area"
latest_year <- max(rent_burden_analysis$year)


# --- Table 1: Time Evolution for Buffalo-Niagara Falls, NY Metro Area ---
# ----------------------------------------------------------------------

buffalo_table <- rent_burden_analysis %>%
  filter(NAME == TARGET_CBSA_NAME) %>%
  select(year, 'Raw Ratio (%)' = raw_ratio_pct, 'Rent Burden Index (RBI)' = rent_burden_index) %>%
  mutate(
    'Raw Ratio (%)' = paste0(round(`Raw Ratio (%)`, 2), '%'),
    'Rent Burden Index (RBI)' = round(`Rent Burden Index (RBI)`, 3)
  )

cat("\n# Table 1: Rent Burden for Buffalo-Niagara Falls, NY Metro Area (2009-2023)\n")
DT::datatable(
  buffalo_table,
  options = list(
    dom = 't', # Show table only
    columnDefs = list(list(className = 'dt-center', targets = '_all'))
  ),
  caption = 'Rent Burden for Buffalo-Niagara Falls, NY Metro Area over time.'
)


# --- Table 2: Top 5 Highest and Lowest Rent Burden (Latest Year: 2023) ---
# ----------------------------------------------------------------------

latest_year_df <- rent_burden_analysis %>%
  filter(year == latest_year)

# Find the highest and lowest RBI
highest_burden <- latest_year_df %>%
  arrange(desc(rent_burden_index)) %>%
  slice_head(n = 5)

lowest_burden <- latest_year_df %>%
  arrange(rent_burden_index) %>%
  slice_head(n = 5)

top_bottom_df <- bind_rows(highest_burden, lowest_burden) %>%
  select('Metropolitan Area' = NAME, 'Raw Ratio (%)' = raw_ratio_pct, 'Rent Burden Index (RBI)' = rent_burden_index) %>%
  mutate(
    'Raw Ratio (%)' = paste0(round(`Raw Ratio (%)`, 2), '%'),
    'Rent Burden Index (RBI)' = round(`Rent Burden Index (RBI)`, 3)
  )

cat("\n# Table 2: Top and Bottom Rent Burden Areas in", latest_year, "\n")
DT::datatable(
  top_bottom_df,
  options = list(
    dom = 't',
    # JavaScript to highlight the top 5 (Highest) in yellow and bottom 5 (Lowest) in blue
    rowCallback = DT::JS(
      "function(row, data, index) {
        if (index < 5) {
          $('td', row).css('background-color', 'rgba(255, 255, 0, 0.4)'); // Yellow for highest
        } else {
          $('td', row).css('background-color', 'rgba(173, 216, 230, 0.4)'); // Light blue for lowest
        }
      }"
    ),
    columnDefs = list(list(className = 'dt-center', targets = '_all'))
  ),
  caption = paste('Top 5 Highest (Yellow) and Lowest 5 (Blue) Rent Burden Areas in', latest_year, '(based on RBI).')
)


#Task 5: suitable measure of housing growth:


# --- Load Data ---
pop_data <- read_csv("B01003_001_cbsa_2009_2023.csv")
permits_data <- read_csv("housing_units_2009_2023.csv")

# Ensure column names are consistent/correct
pop_data <- pop_data %>%
  rename(total_population = B01003_001) %>%
  select(GEOID, NAME, year, total_population)

permits_data <- permits_data %>%
  rename(GEOID = CBSA, new_permits = new_housing_units_permitted) %>%
  select(GEOID, year, new_permits)

# --- Join Tables and Calculate 5-Year Population Growth ---
growth_data <- pop_data %>%
  inner_join(permits_data, by = c("GEOID", "year")) %>%
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(
    # 5-year Population Growth: P(t) - P(t-5)
    pop_5yr_ago = lag(total_population, n = 5, default = NA),
    pop_growth_5yr = total_population - pop_5yr_ago
  ) %>%
  ungroup() %>%
  filter(year >= 2014)


# --- 1. 'Instantaneous' Measure of Housing Growth (HGI) ---
# ----------------------------------------------------------------------

# Raw Metric: New Permits per 1,000 residents (HGI_raw)
HGI_data <- growth_data %>%
  mutate(HGI_raw = (new_permits / total_population) * 1000)

# Baseline: National Average HGI_raw in 2014
HGI_baseline_2014 <- HGI_data %>%
  filter(year == 2014) %>%
  summarise(avg_HGI_raw_2014 = mean(HGI_raw, na.rm = TRUE)) %>%
  pull(avg_HGI_raw_2014)

# Standardize: HGI_Index = HGI_raw / HGI_baseline_2014 ("times the 2014 national average")
HGI_data <- HGI_data %>%
  mutate(HGI_Index = HGI_raw / HGI_baseline_2014) %>%
  select(GEOID, NAME, year, HGI_raw, HGI_Index)


# --- 2. 'Rate-Based' Measure of Housing Growth (HGR) ---
# ----------------------------------------------------------------------

# Raw Metric: New Permits per unit of 5-year Population Growth (HGR_raw)
HGR_data <- growth_data %>%
  filter(!is.na(pop_growth_5yr)) %>%
  mutate(
    # Add +1 to the denominator to handle cases where population growth is zero/near-zero
    HGR_raw = new_permits / (pop_growth_5yr + 1)
  )

# Baseline: National Average HGR_raw in 2014
HGR_baseline_2014 <- HGR_data %>%
  filter(year == 2014) %>%
  summarise(avg_HGR_raw_2014 = mean(HGR_raw[is.finite(HGR_raw)], na.rm = TRUE)) %>%
  pull(avg_HGR_raw_2014)

# Standardize: HGR_Index = HGR_raw / HGR_baseline_2014 ("times the 2014 national average")
HGR_data <- HGR_data %>%
  mutate(HGR_raw = ifelse(is.finite(HGR_raw), HGR_raw, NA)) %>%
  mutate(
    HGR_Index = HGR_raw / HGR_baseline_2014
  ) %>%
  select(GEOID, NAME, year, HGR_raw, HGR_Index)


# --- Final Join and Output ---
housing_growth_analysis <- HGI_data %>%
  inner_join(HGR_data, by = c("GEOID", "NAME", "year")) %>%
  filter(!is.na(HGI_Index) & !is.na(HGR_Index))

# Save the final data to CSV
housing_growth_analysis %>% write_csv("housing_growth_analysis.csv")


#Constructing the tables identifying the individual metrics that score high/low

library(DT)
library(readr)
library(dplyr)
library(stringr)

# --- Function to generate DT table with conditional highlighting ---
generate_dt_table <- function(data, title, latest_year) {
  caption_text <- paste0(title, " in ", latest_year, ". Top 5 (Yellow), Bottom 5 (Blue).")
  
  # Standard column definitions
  col_defs <- list(list(className = 'dt-center', targets = '_all'))
  
  # JavaScript for highlighting top 5 (yellow) and bottom 5 (blue)
  # NOTE: DT::JS code must be passed as a single string
  js_callback <- DT::JS(
    "function(row, data, index) {
        if (index < 5) {
          $('td', row).css('background-color', 'rgba(255, 255, 0, 0.4)'); // Yellow for highest
        } else {
          $('td', row).css('background-color', 'rgba(173, 216, 230, 0.4)'); // Light blue for lowest
        }
      }"
  )
  
  # Ensure indices are rounded before display
  data <- data %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
  
  DT::datatable(
    data,
    options = list(
      dom = 't', # Show table only
      rowCallback = js_callback,
      columnDefs = col_defs
    ),
    caption = caption_text
  )
}

# Define the year for captions
LATEST_YEAR <- 2023

# --- 1. HGI Index Table ---
hgi_data <- read_csv("hgi_index_top_bottom_2023.csv")
cat("\n# HGI Index Top/Bottom Table (2023)\n")
generate_dt_table(
  hgi_data %>% select(Category, 'Metropolitan Area', 'HGI Index', 'HGR Index'),
  "Top 5 Highest and Lowest Housing Growth (Permits per 1k Residents - HGI)",
  LATEST_YEAR
)

# --- 2. HGR Index Table ---
hgr_data <- read_csv("hgr_index_top_bottom_2023.csv")
cat("\n# HGR Index Top/Bottom Table (2023)\n")
generate_dt_table(
  hgr_data %>% select(Category, 'Metropolitan Area', 'HGI Index', 'HGR Index'),
  "Top 5 Highest and Lowest Housing Growth (Permits vs. 5-Year Pop Growth - HGR)",
  LATEST_YEAR
)

# --- 3. Composite HGS Table ---
hgs_data <- read_csv("hgs_top_bottom_2023.csv")
cat("\n# Composite HGS Top/Bottom Table (2023)\n")
generate_dt_table(
  hgs_data %>% select(Category, 'Metropolitan Area', 'HGS', 'HGI Index', 'HGR Index'),
  "Top 5 Highest and Lowest Composite Housing Growth Score (HGS)",
  LATEST_YEAR
)


#Identifying the CBSs that score High/Low on each metric

library(dplyr)
library(readr)
library(DT)

# --- 1. Data Recreation and HGS Calculation ---

# Load and prepare data (re-run of previous step to ensure data completeness)
pop_data <- read_csv("B01003_001_cbsa_2009_2023.csv") %>%
  rename(total_population = B01003_001) %>%
  select(GEOID, NAME, year, total_population)

permits_data <- read_csv("housing_units_2009_2023.csv") %>%
  rename(GEOID = CBSA, new_permits = new_housing_units_permitted) %>%
  select(GEOID, year, new_permits)

growth_data <- pop_data %>%
  inner_join(permits_data, by = c("GEOID", "year")) %>%
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(
    pop_5yr_ago = lag(total_population, n = 5, default = NA),
    pop_growth_5yr = total_population - pop_5yr_ago
  ) %>%
  ungroup() %>%
  filter(year >= 2014)

# HGI Index Calculation (Permits per 1k Residents)
HGI_data <- growth_data %>% mutate(HGI_raw = (new_permits / total_population) * 1000)
HGI_baseline_2014 <- HGI_data %>% filter(year == 2014) %>% summarise(avg = mean(HGI_raw, na.rm = TRUE)) %>% pull(avg)
HGI_data <- HGI_data %>% mutate(HGI_Index = HGI_raw / HGI_baseline_2014) %>% select(GEOID, NAME, year, HGI_Index)

# HGR Index Calculation (Permits per 5-year Pop Growth + 1)
HGR_data <- growth_data %>%
  filter(!is.na(pop_growth_5yr)) %>%
  mutate(HGR_raw = new_permits / (pop_growth_5yr + 1))
HGR_baseline_2014 <- HGR_data %>% filter(year == 2014) %>% summarise(avg = mean(HGR_raw[is.finite(HGR_raw)], na.rm = TRUE)) %>% pull(avg)
HGR_data <- HGR_data %>% mutate(HGR_raw = ifelse(is.finite(HGR_raw), HGR_raw, NA), HGR_Index = HGR_raw / HGR_baseline_2014) %>% select(GEOID, NAME, year, HGR_Index)

# Final Housing Growth Analysis and HGS
housing_growth_analysis <- HGI_data %>%
  inner_join(HGR_data, by = c("GEOID", "NAME", "year")) %>%
  filter(!is.na(HGI_Index) & !is.na(HGR_Index)) %>%
  mutate(Housing_Growth_Score = (HGI_Index + HGR_Index) / 2)

# --- 2. Data Preparation for Tables (Latest Year: 2023) ---

latest_year <- max(housing_growth_analysis$year)
latest_year_df <- housing_growth_analysis %>%
  filter(year == latest_year) %>%
  select(NAME, HGI_Index, HGR_Index, Housing_Growth_Score)

# --- Define Subsets for Top/Bottom 5 ---
get_top_bottom <- function(df, metric, n=5) {
  df %>%
    arrange(desc({{metric}})) %>%
    slice_head(n = n) %>%
    bind_rows(
      df %>%
        arrange({{metric}}) %>%
        slice_head(n = n)
    )
}

# HGI Top/Bottom Table
HGI_top_bottom <- get_top_bottom(latest_year_df, HGI_Index) %>%
  mutate(Category = c(rep("Highest HGI", 5), rep("Lowest HGI", 5))) %>%
  select(Category, 'Metropolitan Area' = NAME, 'HGI Index' = HGI_Index, 'HGR Index' = HGR_Index)

# HGR Top/Bottom Table
HGR_top_bottom <- get_top_bottom(latest_year_df, HGR_Index) %>%
  mutate(Category = c(rep("Highest HGR", 5), rep("Lowest HGR", 5))) %>%
  select(Category, 'Metropolitan Area' = NAME, 'HGI Index' = HGI_Index, 'HGR Index' = HGR_Index)

# HGS Top/Bottom Table
HGS_top_bottom <- get_top_bottom(latest_year_df, Housing_Growth_Score) %>%
  mutate(Category = c(rep("Highest HGS", 5), rep("Lowest HGS", 5))) %>%
  select(Category, 'Metropolitan Area' = NAME, 'HGS' = Housing_Growth_Score, 'HGI Index' = HGI_Index, 'HGR Index' = HGR_Index)

# --- 3. DT Table Generation ---

# JavaScript callback for highlighting rows (Top 5 Yellow, Bottom 5 Blue)
js_callback <- DT::JS(
  "function(row, data, index) {
    if (index < 5) {
      $('td', row).css('background-color', 'rgba(255, 255, 0, 0.4)'); // Yellow for highest
    } else {
      $('td', row).css('background-color', 'rgba(173, 216, 230, 0.4)'); // Light blue for lowest
    }
  }"
)

dt_options <- list(
  dom = 't',
  rowCallback = js_callback,
  columnDefs = list(list(className = 'dt-center', targets = '_all'))
)

# Function to display DT table
display_dt <- function(data, title) {
  data_rounded <- data %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
  DT::datatable(
    data_rounded,
    options = dt_options,
    caption = paste0(title, " in ", latest_year, ". Top 5 (Yellow), Bottom 5 (Blue).")
  )
}

cat("\n# Table 1: Top/Bottom Areas based on HGI Index\n")
display_dt(HGI_top_bottom, "Top 5 Highest and Lowest Housing Growth (Permits per 1k Residents - HGI)")

cat("\n# Table 2: Top/Bottom Areas based on HGR Index\n")
display_dt(HGR_top_bottom, "Top 5 Highest and Lowest Housing Growth (Permits vs. 5-Year Pop Growth - HGR)")


#Composite Score based on HGR Index
cat("\n# Table 3: Top/Bottom Areas based on Composite Housing Growth Score (HGS)\n")
display_dt(HGS_top_bottom, "Top 5 Highest and Lowest Composite Housing Growth Score (HGS)")



# Task 6: Visualizations Rent Burden:Housing Growth
#Preparing the core data:

library(dplyr)
library(readr)
library(ggplot2)
library(scales) 

# --- 1. Load and Prepare Core Data ---
pop_data <- read_csv("B01003_001_cbsa_2009_2023.csv")
pop_data <- pop_data %>% rename(total_population = B01003_001) %>% select(GEOID, NAME, year, total_population)

income_data <- read_csv("B19013_001_cbsa_2009_2023.csv")
income_data <- income_data %>% rename(med_income = B19013_001) %>% select(GEOID, year, med_income)

rent_data <- read_csv("B25064_001_cbsa_2009_2023.csv")
rent_data <- rent_data %>% rename(med_rent = B25064_001) %>% select(GEOID, year, med_rent)

permits_data <- read_csv("housing_units_2009_2023.csv")
permits_data <- permits_data %>% rename(GEOID = CBSA, new_permits = new_housing_units_permitted) %>% select(GEOID, year, new_permits)


# --- 2. Calculate Rent Burden Index (RBI) ---
rent_burden_data <- inner_join(rent_data, income_data, by = c("GEOID", "year"))
rent_burden_data <- rent_burden_data %>% mutate(RB_raw = (med_rent * 12) / med_income) %>% filter(RB_raw > 0 & is.finite(RB_raw))
RB_baseline_2009 <- rent_burden_data %>% filter(year == 2009) %>% summarise(avg = mean(RB_raw, na.rm = TRUE)) %>% pull(avg)
rent_burden_data <- rent_burden_data %>% mutate(RBI_Index = RB_raw / RB_baseline_2009) %>% select(GEOID, year, RBI_Index)


# --- 3. Calculate Housing Growth Score (HGS) ---
growth_data <- inner_join(pop_data, permits_data, by = c("GEOID", "year"))
growth_data <- growth_data %>% arrange(GEOID, year) %>% group_by(GEOID) %>%
  mutate(pop_5yr_ago = lag(total_population, n = 5, default = NA), pop_growth_5yr = total_population - pop_5yr_ago) %>%
  ungroup() %>% filter(year >= 2014)

HGI_data <- growth_data %>% mutate(HGI_raw = (new_permits / total_population) * 1000)
HGI_baseline_2014 <- HGI_data %>% filter(year == 2014) %>% summarise(avg = mean(HGI_raw, na.rm = TRUE)) %>% pull(avg)
HGI_data <- HGI_data %>% mutate(HGI_Index = HGI_raw / HGI_baseline_2014) %>% select(GEOID, year, HGI_Index)

HGR_data <- growth_data %>% filter(!is.na(pop_growth_5yr)) %>% mutate(HGR_raw = new_permits / (pop_growth_5yr + 1))
HGR_baseline_2014 <- HGR_data %>% filter(year == 2014) %>% summarise(avg = mean(HGR_raw[is.finite(HGR_raw)], na.rm = TRUE)) %>% pull(avg)
HGR_data <- HGR_data %>% mutate(HGR_raw = ifelse(is.finite(HGR_raw), HGR_raw, NA), HGR_Index = HGR_raw / HGR_baseline_2014) %>% select(GEOID, year, HGR_Index)

housing_growth_analysis <- HGI_data %>% inner_join(HGR_data, by = c("GEOID", "year")) %>% filter(!is.na(HGI_Index) & !is.na(HGR_Index)) %>% mutate(Housing_Growth_Score = (HGI_Index + HGR_Index) / 2)

# --- 4. Merge All Metrics & Calculate YIMBY Criteria Variables (2009-2023) ---
full_analysis_data <- inner_join(pop_data, rent_burden_data, by = c("GEOID", "year"))
full_analysis_data <- full_analysis_data %>% inner_join(housing_growth_analysis, by = c("GEOID", "year")) %>% arrange(GEOID, year)

start_year <- min(full_analysis_data$year)
end_year <- max(full_analysis_data$year)

yimby_criteria <- full_analysis_data %>% filter(year == start_year | year == end_year | year >= 2014) %>%
  group_by(GEOID, NAME) %>%
  summarise(
    initial_RB_Index = RBI_Index[year == start_year],
    RB_Index_change = RBI_Index[year == end_year] - RBI_Index[year == start_year],
    pop_growth = total_population[year == end_year] - total_population[year == start_year],
    avg_HGS_2014_2023 = mean(Housing_Growth_Score[year >= 2014], na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(!is.na(initial_RB_Index) & !is.na(RB_Index_change) & !is.na(pop_growth) & !is.na(avg_HGS_2014_2023))

national_avg_HGS_2014_2023 <- mean(yimby_criteria$avg_HGS_2014_2023, na.rm = TRUE)

yimby_cbsas <- yimby_criteria %>%
  mutate(is_yimby = (initial_RB_Index > 1.0) & (RB_Index_change < 0) & (pop_growth > 0) & (avg_HGS_2014_2023 > national_avg_HGS_2014_2023)) %>%
  filter(is_yimby) %>%
  arrange(desc(avg_HGS_2014_2023))

# Prepare data for plotting
plot1_data <- yimby_criteria %>% mutate(is_yimby_candidate = (initial_RB_Index > 1.0) & (pop_growth > 0))
top_yimby_names <- head(yimby_cbsas, 5)$NAME
plot1_data <- plot1_data %>% mutate(label = ifelse(NAME %in% top_yimby_names, NAME, ""))

plot2_data <- full_analysis_data %>%
  filter(NAME %in% yimby_cbsas$NAME) %>%
  select(NAME, year, RBI_Index) %>%
  arrange(NAME, year)

# --- 5. GGPLOT2 CODE BLOCKS WITH PRINT CALLS ---

# Visualization 1: Scatter Plot (HGS vs. Change in Rent Burden)
p1 <- ggplot(plot1_data, aes(x = RB_Index_change, y = avg_HGS_2014_2023)) +
  geom_point(aes(color = is_yimby_candidate, size = log10(pop_growth)), alpha = 0.6) +
  geom_hline(yintercept = national_avg_HGS_2014_2023, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_text(aes(label = label), hjust = 0, vjust = 0, nudge_x = 0.005, size = 3) +
  scale_color_manual(values = c("TRUE" = "#0072B2", "FALSE" = "gray70")) + 
  scale_size_continuous(name = "Population Growth (Log10)", labels = scales::comma) +
  labs(
    title = paste0("Housing Growth vs. Change in Rent Burden Index (", start_year, " - ", end_year, ")"),
    subtitle = "YIMBY Success: Initial RBI > 1.0, Pop Growth > 0, HGS > National Avg, and RBI Change < 0 (Top-Left, above gray line).",
    x = "Change in Rent Burden Index (RBI) (Decrease is better, negative values mean success)",
    y = "Average Housing Growth Score (HGS) (Higher is better)",
    caption = paste0("HGS Average Period: 2014-", end_year, ". Horizontal Line: National Average HGS (", round(national_avg_HGS_2014_2023, 2), ").")
  ) +
  theme_minimal() +
  theme(legend.position = "none", plot.caption = element_text(size = 8))

# print the plot**
print(p1)


# Visualization 2: Time Series Plot (RBI Trend for YIMBY Success CBSAs)
p2 <- ggplot(plot2_data, aes(x = year, y = RBI_Index, group = NAME)) +
  geom_line(aes(color = NAME), linewidth = 1) +
  geom_point(aes(color = NAME)) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray50") +
  scale_x_continuous(breaks = seq(start_year, end_year, by = 2)) +
  labs(
    title = "Rent Burden Index (RBI) Trend for Identified YIMBY Success CBSAs",
    subtitle = "RBI is standardized to the 2009 National Average (RBI=1.0 dashed line).",
    x = "Year",
    y = "Rent Burden Index (RBI)",
    color = "Metropolitan Area"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# print the plot**
print(p2)




