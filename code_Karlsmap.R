library(haven)
library(giscoR)

karl_data <- read_dta("~/Downloads/tech_adoption_by_region (3).dta")
view(karl_data)

attributes(karl_data$region_num)

library(tidyverse)
library(sf)
library(geodata)

# Your data
adoption_data <- tibble(
  region_name = c("Blekinge", "Dalarna", "Gävleborg", "Halland",
                  "Jämtland Härjedalen", "Jönköpings", "Kalmar",
                  "Kronoberg", "Norrbotten", "Skåne", "Stockholm",
                  "Sörmland", "Uppsala", "Värmland", "Västerbotten",
                  "Västernorrland", "Västmanland", "Örebro", "Östergötland",
                  "Västra Götaland"),
  adoption_rate = c(0, 0.409, 0, 1, 0.167, 0.852, 0.757, 0.160, 0.231,
                    0.940, 0.521, 0.130, 0.739, 0, 0.613, 0.138, 0.409,
                    0.280, 0.872, 0.236)
)

adoption_data <- adoption_data %>%
  mutate(region_name = recode(region_name,
                              "Jämtland Härjedalen" = "Jämtland",
                              "Jönköpings" = "Jönköping",
                              "Sörmland" = "Södermanland",
                              "Örebro" = "Orebro",
                              "Västra Götaland" = "Västra Götaland"
  ))

# Download Sweden level 1 shapefile from GADM
sweden_sf <- gadm("SWE", level = 1, path = tempdir()) |> st_as_sf()

# Join data (match on NAME_1)
sweden_joined <- sweden_sf |>
  mutate(
    VARNAME_1 = if_else(
      NAME_1 == "Västra Götaland",
      "Västra Götaland",
      VARNAME_1
    ),
    region_name = NAME_1
  ) |>
  left_join(adoption_data, by = "region_name")

# Plot
p <- ggplot(sweden_joined) +
  geom_sf(aes(fill = adoption_rate * 100), color = "white", linewidth = 0.1) +
  scale_fill_gradient(
    low = "#cce5f5", high = "#08306b",
    name = "Adoption Rate in %",
    
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75)
  ) +
  theme_void(base_family = "Times New Roman") +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14, family = "Times New Roman"),
    legend.text  = element_text(size = 11,family = "Times New Roman"),
    legend.key.height = unit(1.5, "cm")
  )


p


ggsave("Karls_plot2_20260424.png", plot = p, width = 8, height = 6, dpi = 300)

