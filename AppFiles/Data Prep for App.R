#library to read in shapefiles for counties and states
library(sf)

#data pipelining
library(dplyr)

library(tmap)

library(readr)
library(stringr)
library(stringi)

counties <- read_sf("cb_2018_us_county_500k/cb_2018_us_county_500k.shp")
states <- read_sf("cb_2018_us_state_500k/cb_2018_us_state_500k.shp")

# TEST TMAP ON SHAPEFILES
#------------------------

#Interactive Mode
tmap_mode("view")

# Map for states
my_map <- tm_shape(states)+
  tm_polygons()+
  tm_basemap("Esri.WorldTopoMap")

my_map

# Map for counties
# WARNING: USES A LOT OF MEMORY
my_map2 <- tm_shape(counties)+
  tm_polygons()+
  tm_basemap("Esri.WorldTopoMap")

my_map2

#------------------------

TRI2023 <- read_csv("2023_TRI_AF_WN.csv")

# Step 1: Standardize county names
TRI2023 <- TRI2023 %>%
  mutate(`7. COUNTY` = str_to_lower(`7. COUNTY`))  # Lowercase for consistency

counties <- counties %>%
  mutate(NAME = str_to_lower(NAME))

stateabbr <- st_drop_geometry(states[, c(1, 5, 6)])

counties <- counties %>%
  left_join(x = counties, y = stateabbr, by = "STATEFP") %>%
  rename(STATENAME = NAME.y,
         NAME = NAME.x)

# Clean County Names
counties <- counties %>%
  mutate(
    NAME = str_remove_all(NAME, "\\."),
    NAME = stri_trans_general(NAME, "Latin-ASCII"))

#Convert YES/NO columns to boolean
TRI2023 <- TRI2023 %>%
  mutate(`42. CLEAN AIR ACT CHEMICAL` = case_when(`42. CLEAN AIR ACT CHEMICAL` == "YES" ~ TRUE,
                                                  `42. CLEAN AIR ACT CHEMICAL` == "NO" ~ FALSE,
                                                  TRUE ~ as.logical(`42. CLEAN AIR ACT CHEMICAL`)),
         `44. METAL` = case_when(`44. METAL` == "YES" ~ TRUE,
                                 `44. METAL` == "NO" ~ FALSE,
                                 TRUE ~ as.logical(`44. METAL`)),
         `46. CARCINOGEN` = case_when(`46. CARCINOGEN` == "YES" ~ TRUE,
                                      `46. CARCINOGEN` == "NO" ~ FALSE,
                                      TRUE ~ as.logical(`46. CARCINOGEN`)),
         `47. PBT` = case_when(`47. PBT` == "YES" ~ TRUE,
                               `47. PBT` == "NO" ~ FALSE,
                               TRUE ~ as.logical(`47. PBT`)),
         `48. PFAS` = case_when(`48. PFAS` == "YES" ~ TRUE,
                                `48. PFAS` == "NO" ~ FALSE,
                                TRUE ~ as.logical(`48. PFAS`)))

TRI2023 <- TRI2023 %>%
  mutate(
    `7. COUNTY` = str_remove_all(`7. COUNTY`, "\\.") %>%   # Remove periods
      str_remove_all("\\s*\\(.*?\\)") %>%                  # Remove text in parentheses
      str_remove_all("(?i)municipio\\b") %>%
      str_trim()                                      # Remove any leading/trailing whitespace
  ,
  `37. CHEMICAL` = str_remove_all(`37. CHEMICAL`, "\\s*\\(.*?\\)") %>%  # Remove text in parentheses
    str_trim())

matches <- 0

for (i in 1:nrow(TRI2023)) {
  
  counties1 <- counties %>%
    filter(STUSPS == TRI2023$`8. ST`[i])
  
  for(j in 1:nrow(counties1)){
  
    if (str_detect(TRI2023$`7. COUNTY`[i], counties1$NAME[j]) & 
        (TRI2023$`8. ST`[i] == counties1$STUSPS[j])){
      
      TRI2023$`7. COUNTY`[i] = counties1$NAME[j]
      
      matches <- matches + 1
      
      break
    }
  }
  
}

# convert grams to pounds
TRI2023 <- TRI2023 %>%
  mutate(across(c(51:119), 
                ~ifelse(TRI2023$`50. UNIT OF MEASURE` == "Grams", 
                       . * 0.00220462,
                       .)))

TRI23_counties <- TRI2023 %>%
  group_by(`8. ST`, `7. COUNTY`) %>%
  summarize(facility_count = n(),
            CAAC_count = sum(`42. CLEAN AIR ACT CHEMICAL`),
            metal_count = sum(`44. METAL`),
            carcin_count = sum(`46. CARCINOGEN`),
            pbt_count = sum(`47. PBT`),
            pfas_count = sum(`48. PFAS`),
            fugitive_air = sum(`51. 5.1 - FUGITIVE AIR`),
            stack_air = sum(`52. 5.2 - STACK AIR`),
            water = sum(`53. 5.3 - WATER`),
            underground = sum(`54. 5.4 - UNDERGROUND`) + 
              sum(`55. 5.4.1 - UNDERGROUND CL I`) + 
              sum(`56. 5.4.2 - UNDERGROUND C II-V`),
            landfills = sum(`57. 5.5.1 - LANDFILLS`)+
              sum(`58. 5.5.1A - RCRA C LANDFILL`)+
              sum(`59. 5.5.1B - OTHER LANDFILLS`),
            other = sum(`63. 5.5.3B - OTHER SURFACE I`) + 
              sum(`64. 5.5.4 - OTHER DISPOSAL`),
            on_site_total = sum(`65. ON-SITE RELEASE TOTAL`),
            off_site_total = sum(`88. OFF-SITE RELEASE TOTAL`),
            recycled = sum(`115. 8.4 - RECYCLING ON SITE`)+
              sum(`116. 8.5 - RECYCLING OFF SIT`),
            treated = sum(`118. 8.7 - TREATMENT OFF SITE`)+
              sum(`117. 8.6 - TREATMENT ON SITE`),
            prod_waste = sum(`119. PRODUCTION WSTE (8.1-8.7)`),
            total_release = sum(`107. TOTAL RELEASES`),
            pro_act_ratio = sum(`122. 8.9 - PRODUCTION RATIO`) / n()) %>%
  rename(STUSPS = `8. ST`,
         NAME = `7. COUNTY`)

#Find top chemical for each county
top_chemical_per_county <- TRI2023 %>%
  group_by( `8. ST`, `7. COUNTY`, `37. CHEMICAL`) %>%
  summarise(total_release = sum(`107. TOTAL RELEASES`), .groups = "drop") %>%  # Count occurrences of each chemical
  arrange(`7. COUNTY`, desc(total_release)) %>%             # Sort by county and count (descending)
  group_by( `8. ST`, `7. COUNTY`) %>%
  slice(1) %>%                                 # Select the top chemical for each county
  select(`7. COUNTY`, top_chemical = `37. CHEMICAL`) 

# Check specific county to see if top selection worked
Baldwin1 <- TRI2023 %>%
  filter(`7. COUNTY` == "baldwin", `8. ST` == "AL")%>%
  group_by(`37. CHEMICAL`) %>%
  summarize(total_release = sum(`107. TOTAL RELEASES`))

#Find top industry for each county
top_industry_per_county <- TRI2023 %>%
  group_by( `8. ST`, `7. COUNTY`, `23. INDUSTRY SECTOR`) %>%
  summarise(total_release = sum(`107. TOTAL RELEASES`), .groups = "drop") %>%  # Count occurrences of each chemical
  arrange(`7. COUNTY`, desc(total_release)) %>%             # Sort by county and count (descending)
  group_by( `8. ST`, `7. COUNTY`) %>%
  slice(1) %>%                                 # Select the top industry for each county
  select(`7. COUNTY`, top_ind = `23. INDUSTRY SECTOR`) 

# Check specific county to see if top selection worked
Baldwin2 <- TRI2023 %>%
  filter(`7. COUNTY` == "baldwin", `8. ST` == "AL")%>%
  group_by(`23. INDUSTRY SECTOR`) %>%
  summarize(total_release = sum(`107. TOTAL RELEASES`))

top_per_county <- top_chemical_per_county %>%
  left_join(x = top_chemical_per_county, 
            y = top_industry_per_county,
            by = c("8. ST", "7. COUNTY"))%>%
  rename(STUSPS = `8. ST`,
         NAME = `7. COUNTY`)

TRI23_counties <- TRI23_counties %>%
  left_join(x = TRI23_counties, y = top_per_county, by = c("STUSPS", "NAME"))

TRI23_counties <- TRI23_counties %>%
  mutate(top_chemical = case_when(top_chemical == "Barium compounds)" ~ "Barium compounds",
                                  TRUE ~ top_chemical))

# Join counties data to full data
TRI23_counties <- TRI23_counties %>%
  right_join(x = TRI23_counties, y = counties, by = c("STUSPS", "NAME"))

TRI23_states <- TRI2023 %>%
  group_by(`8. ST`) %>%
  summarize(facility_count = n(),
            CAAC_count = sum(`42. CLEAN AIR ACT CHEMICAL`),
            metal_count = sum(`44. METAL`),
            carcin_count = sum(`46. CARCINOGEN`),
            pbt_count = sum(`47. PBT`),
            pfas_count = sum(`48. PFAS`),
            fugitive_air = sum(`51. 5.1 - FUGITIVE AIR`),
            stack_air = sum(`52. 5.2 - STACK AIR`),
            water = sum(`53. 5.3 - WATER`),
            underground = sum(`54. 5.4 - UNDERGROUND`) + 
              sum(`55. 5.4.1 - UNDERGROUND CL I`) + 
              sum(`56. 5.4.2 - UNDERGROUND C II-V`),
            landfills = sum(`57. 5.5.1 - LANDFILLS`)+
              sum(`58. 5.5.1A - RCRA C LANDFILL`)+
              sum(`59. 5.5.1B - OTHER LANDFILLS`),
            other = sum(`63. 5.5.3B - OTHER SURFACE I`) + 
              sum(`64. 5.5.4 - OTHER DISPOSAL`),
            on_site_total = sum(`65. ON-SITE RELEASE TOTAL`),
            off_site_total = sum(`88. OFF-SITE RELEASE TOTAL`),
            recycled = sum(`115. 8.4 - RECYCLING ON SITE`)+
              sum(`116. 8.5 - RECYCLING OFF SIT`),
            treated = sum(`118. 8.7 - TREATMENT OFF SITE`)+
              sum(`117. 8.6 - TREATMENT ON SITE`),
            prod_waste = sum(`119. PRODUCTION WSTE (8.1-8.7)`),
            total_release = sum(`107. TOTAL RELEASES`),
            pro_act_ratio = sum(`122. 8.9 - PRODUCTION RATIO`) / n()) %>%
  rename(STUSPS = `8. ST`)

#Find top chemical for each state
top_chemical_per_state <- TRI2023 %>%
  group_by( `8. ST`, `37. CHEMICAL`) %>%
  summarise(total_release = sum(`107. TOTAL RELEASES`), .groups = "drop") %>%  # Count occurrences of each chemical
  arrange(`8. ST`, desc(total_release)) %>%             # Sort by state and count (descending)
  group_by( `8. ST`) %>%
  slice(1) %>%                                 # Select the top chemical for each state
  select(`8. ST`, top_chemical = `37. CHEMICAL`) 

#Find top industry for each state
top_industry_per_state <- TRI2023 %>%
  group_by( `8. ST`, `23. INDUSTRY SECTOR`) %>%
  summarise(total_release = sum(`107. TOTAL RELEASES`), .groups = "drop") %>%  # Count occurrences of each chemical
  arrange(`8. ST`, desc(total_release)) %>%             # Sort by state and count (descending)
  group_by( `8. ST`) %>%
  slice(1) %>%                                 # Select the top industry for each state
  select(`8. ST`, top_ind = `23. INDUSTRY SECTOR`) 

top_per_state <- top_chemical_per_state %>%
  left_join(x = top_chemical_per_state, 
            y = top_industry_per_state,
            by = c("8. ST"))%>%
  rename(STUSPS = `8. ST`)

TRI23_states <- TRI23_states %>%
  left_join(x = TRI23_states, y = top_per_state, by = c("STUSPS"))

TRI23_states <- TRI23_states %>%
  mutate(top_chemical = case_when(top_chemical == "Barium compounds)" ~ "Barium compounds",
                                  TRUE ~ top_chemical))

# Join counties data to full data
TRI23_states <- TRI23_states %>%
  right_join(x = TRI23_states, y = states, by = c("STUSPS"))

write_sf(TRI23_states, "TRI23_states.shp")
write_sf(TRI23_counties, "TRI23_counties.shp")


TRI23_states <- read_sf("TRI23_states.shp")
TRI23_counties <- read_sf("TRI23_counties.shp")

TRI23_cnew <- TRI23_counties %>%
  filter(STATENA == "Alaska")

p1 <- ggplot(data = TRI23_cnew)+
  geom_sf_interactive(mapping = aes(fill = ttl_rls, 
                                    data_id = NAME))+
  coord_sf(xlim = c(-180, -120))
p1

TRI23_states <- TRI23_states %>%
  mutate(CAAC_prop = round((CAAC_cn / fclty_c) * 100, 2),
         mtl_prop = round((mtl_cnt / fclty_c) * 100, 2),
         carc_prop = round((crcn_cn / fclty_c) * 100, 2),
         pbt_prop = round((pbt_cnt / fclty_c) * 100, 2),
         pfas_prop = round((pfs_cnt / fclty_c) * 100, 2),
         f_air_prop = round((fugtv_r / ttl_rls) * 100, 2),
         s_air_prop = round((stack_r / ttl_rls) * 100, 2),
         water_prop = round((water / ttl_rls) * 100, 2),
         underg_prop = round((undrgrn / ttl_rls) * 100, 2),
         landfl_prop = round((lndflls / ttl_rls) * 100, 2),
         other_prop = round((other / ttl_rls) * 100, 2),
         on_site_prop = round((on_st_t / ttl_rls) * 100, 2),
         off_site_prop = round((off_st_ / ttl_rls) * 100, 2),
         recyc_prop = round((recycld / prd_wst) * 100, 2),
         treat_prop = round((treated / prd_wst) * 100, 2),
         rls_prop = round((ttl_rls / prd_wst) * 100, 2))

TRI23_counties <- TRI23_counties %>%
  mutate(CAAC_prop = round((CAAC_cn / fclty_c) * 100, 2),
         mtl_prop = round((mtl_cnt / fclty_c) * 100, 2),
         carc_prop = round((crcn_cn / fclty_c) * 100, 2),
         pbt_prop = round((pbt_cnt / fclty_c) * 100, 2),
         pfas_prop = round((pfs_cnt / fclty_c) * 100, 2),
         f_air_prop = round((fugtv_r / ttl_rls) * 100, 2),
         s_air_prop = round((stack_r / ttl_rls) * 100, 2),
         water_prop = round((water / ttl_rls) * 100, 2),
         underg_prop = round((undrgrn / ttl_rls) * 100, 2),
         landfl_prop = round((lndflls / ttl_rls) * 100, 2),
         other_prop = round((other / ttl_rls) * 100, 2),
         on_site_prop = round((on_st_t / ttl_rls) * 100, 2),
         off_site_prop = round((off_st_ / ttl_rls) * 100, 2),
         recyc_prop = round((recycld / prd_wst) * 100, 2),
         treat_prop = round((treated / prd_wst) * 100, 2),
         rls_prop = round((ttl_rls / prd_wst) * 100, 2))
