#### processing the other data from MappingPlants####

pacman::p_load(googlesheets4,tidyverse,janitor,ggplot2,ggpubr)

# MAKE EDITS TO DOWN LOADED RAW DATA FROM SURVE123
plant_traits <- read_sheet("https://docs.google.com/spreadsheets/d/1gAAk09YgYKcUidop3xwplKaXW3rQoOgUoF4EtZganAA/edit?gid=407247046#gid=407247046", sheet = 'clean_data', skip = 0)

column_list <- colnames(plant_traits)

## getting all the colomns##
column_string <- paste(column_list, collapse = ",\n")
cat(column_string)

plant_traits_clean <- plant_traits |> 
  select(
#wkt_geom,
#globalid,
#CreationDa,
#Creator,
#EditDate,
#Editor,
'_date',
#start_time,
#end_time,
vegetation,
vegetati_1,
vegetati_2,
vegetati_3,
vegetati_4,
bare_groun,
#denseness,
#aspect,
#untitled_2,
plot_name,
soil_moist,
soil_moi_1,
soil_moi_2,
soil_moi_3,
soil_temp_,
soil_temp1,
soil_tem_1,
soil_tamp_,
topographi,
bryophyte_,
field_47,
lichen_bra,
species_1_,
species_1,
species_11,
species_2,
taxon_3,
#taxon_othe,
taxon_4,
taxon_2_he,
taxon_2_br,
taxon_3_he,
taxon_3_br,
taxon_4_he,
taxon_4_br,
taxon_5,
taxon_5_he,
taxon_5_br,
taxon_6,
taxon_6_he,
taxon_6_br,
taxon_7,
taxon_7_he,
taxon_7_br,
taxon_8,
taxon_8_he,
taxon_8_br,
other_note,
taxon_9,
taxon_9_he,
taxon_9_br,
taxon_10,
taxon_10_h,
taxon_10_b,
taxon_11,
taxon_11_h,
taxon_11_b,
taxon_12,
taxon_12_h,
taxon_12_b,
taxon_13,
taxon_13_h,
taxon_13_b,
taxon_14,
taxon_14_h,
taxon_14_b,
taxon_15,
taxon_15_h,
taxon_15_b,
taxon_16,
taxon_16_h,
taxon_16_b,
taxon_17,
taxon_17_h,
taxon_17_b,
taxon_18,
taxon_18_h,
taxon_18_b,
taxon_19,
taxon_19_h,
taxon_19_b,
taxon_20,
taxon_20_h,
taxon_20_b

#vegetati_5,
#time
)

### function to make pivot longer ####

df_raw <- plant_traits

generate_dataframe <- function(number) {
  taxon_col <- sym(paste0("taxon_", number))
  height_col <- sym(paste0("taxon_", number, "_h"))
  bb_col <- sym(paste0("taxon_", number, "_bb"))
  
  df_raw |> 
    select(1:35, !!taxon_col, !!height_col, !!bb_col) %>%
    mutate(rowid = row_number(),
           position = paste0("taxon_", number)) %>%
    rename(taxon = !!taxon_col,
           height = !!height_col,
           bb = !!bb_col)
}

taxon_list <- lapply(1:14, generate_dataframe)

# Combine all dataframes into a single dataframe
pivot <- bind_rows(taxon_list) |>  
  #mutate(veg_mean_height = rowMeans(select(.,veg_height_n,veg_height_s,veg_height_e,veg_height_w))) |> 
  filter(!is.na(taxon))

colnames(pivot)

plant_trait_pivot <- pivot |> 
  select(
         #wkt_geom,
         #fid,
         #globalid,
         '_date',
         #start_time,
         #end_time,
         #vegetation,
         #vegetati_1,
         #vegetati_2,
         #vegetati_3,
         #soil_moist,
         #soil_temp_,
         #soil_moi_1,
         #soil_temp1,
         #soil_moi_2,
         #soil_tem_1,
         #soil_moi_3,
         #soil_tamp_,
         topographi,
         bryo_bb,
         lichen_bb,
         taxon,
         height,
         bb,
         plot_name,
         #bryoph_bb,
         bare_groun,
         veg_type,
         soilmoist1,
         soilmoist2,
         soilmoist3,
         soilmoist4,
         soil_m_mean,
         soil_t_m,
         veg_hight,
         nu_of_spec,
         other_note,
         ndwi_sentinel,
         rowid,
         position
         )

check_number_of_taxa <- plant_trait_pivot |> 
  group_by(plot_name, nu_of_spec) |> 
  count() 
#|> 
#  filter(nu_of_spec != n)
  
