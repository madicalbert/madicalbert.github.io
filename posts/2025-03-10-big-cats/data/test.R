
# load libraries
library(tidyverse)
library(here)
library(janitor)

# load data - gross exports data
gross_exports <- read.csv(here("data/cites_gross_exports.csv")) %>%
  clean_names()

gross_exports_clean <- gross_exports %>%
  rename_with(~ str_remove(.x, "^x")) %>%
  pivot_longer(
    cols = -c("app", "taxon", "term", "unit", "country"),
    names_to = "year",
    values_to = "count"
  ) %>%
  mutate(year = as.integer(year)) %>%
  drop_na() %>%
  filter(unit == "Number of specimens" | unit == "")

# visualize the top 5 taxon reported and fill by the term
gross_exports_clean %>%
  group_by(taxon, term) %>%
  summarize(total = sum(count)) %>%
  arrange(desc(total)) %>%
  head(5) %>%
  ggplot(aes(x = reorder(taxon, total), y = total, fill = term)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 5 taxon reported in gross exports data",
       x = "Taxon",
       y = "Total number of specimens exported",
       fill = "Term")



# visualize the top term reported in the gross exports data
gross_exports_clean %>%
  group_by(term) %>%
  summarize(total = sum(count)) %>%
  arrange(desc(total)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(term, total), y = total)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 terms reported in gross exports data",
       x = "Term",
       y = "Total number of specimens exported")


# plot top 5 cats across time
top_5 <- gross_exports_clean %>%
  filter(taxon %in% c("Panthera tigris",
                      "Panthera pardus",
                      "Acinonyx jubatus",
                      "Panthera onca",
                      "Lynx pardinus"))

top_5 %>%
  ggplot(aes(x = year, y = count, color = taxon)) +
  geom_line() +
  labs(title = "Top 5 cats exported across time",
       x = "Year",
       y = "Total number of specimens exported",
       color = "Taxon")

#plot as a stacked area
top_5 %>%
  ggplot(aes(x = year, y = count, fill = taxon)) +
  geom_area() +
  labs(title = "Top 5 cats exported across time",
       x = "Year",
       y = "Total number of specimens exported",
       fill = "Taxon")

top_5_summary <- top_5 %>%
  group_by(taxon, year) %>%
  summarize(total_count = sum(count)) %>%  # Sum the counts by year and taxon
  ungroup()  # Ungroup to make the data frame easier to work with

# Plot the data to compare across years
top_5_summary %>%
  ggplot(aes(x = year, y = total_count, color = taxon, group = taxon)) +
  geom_line() +  # Line plot to compare over time
  labs(title = "Comparison of Top 5 Taxa Reported in Gross Exports Over Time",
       x = "Year",
       y = "Total Number of Specimens Exported",
       color = "Taxon") +
  theme_minimal()




# plot the top 5 countries exporting the most specimens
gross_exports_clean %>%
  group_by(country) %>%
  summarize(total = sum(count)) %>%
  arrange(desc(total)) %>%
  head(5) %>%
  ggplot(aes(x = reorder(country, total), y = total)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 5 countries exporting the most specimens",
       x = "Country",
       y = "Total number of specimens exported")
