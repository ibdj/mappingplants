#### processing the other data from MappingPlants####

pacman::p_load(googlesheets4,tidyverse,janitor,ggplot2,ggpubr)

# MAKE EDITS TO DOWN LOADED RAW DATA FROM SURVE123
plant_traits <- read_sheet("https://docs.google.com/spreadsheets/d/1gAAk09YgYKcUidop3xwplKaXW3rQoOgUoF4EtZganAA/edit?gid=407247046#gid=407247046", sheet = 'survey_0', skip = 0) |> 
  clean_names()

test <- read.delim2(file.choose(), sep = ",") |> 
  clean_names()

column_list <- colnames(test)

## getting all the colomns##
column_string <- paste(column_list, collapse = ",\n")
cat(column_string)

organised <- test |> 
  select(
    #object_id,
    #global_id,
    #creation_date,
    #creator,
    #edit_date,
    #editor,
    date,
    plot_name,
    #species_8,
    #other_species,
    #untitled_question_7,
    #species_12,
    #agrmer,
    #alcalp,
    #alcfil,
    #species_16,
    #agrostis_mertensii_height,
    #alchemilla_alpina_height,
    #alchemilla_filicaulis_height,
    #alchemilla_glomerulans_height,
    #alnus_crispa_height,
    #angelica_archangelica_height,
    #antennaria_canescens_height,
    start_time,
    end_time,
    vegetation_height_n,
    vegetation_height_e,
    vegetation_height_s,
    vegetation_height_w,
    #other_taxon_1,
    #arabis_alpina_height,
    #bartsia_alpina_height,
    #denseness,
    #aspect,
    #other_taxon_2,
    soil_moisture_n,
    soil_moisture_e,
    soil_moisture_s,
    soil_moisture_w,
    soil_temp_e,
    soil_temp_n,
    soil_temp_s,
    soil_temp_w,
    topographic_complexity_cm,
    #bryophyte_braun_blanquet_48,
    bryophyte_braun_blanquet_1,
    lichen_braun_blanquet,
    other_notes,
    #other_taxon_3,
    taxon_1,
    taxon_1_height,
    taxon_1_braun_blanquet,
    taxon_2,
    taxon_2_height,
    taxon_2_braun_blanquet,
    taxon_3,
    taxon_3_height,
    taxon_3_braun_blanquet,
    taxon_4,
    taxon_4_height,
    taxon_4_braun_blanquet,
    taxon_5,
    taxon_5_height,
    taxon_5_braun_blanquet,
    taxon_6,
    taxon_6_height,
    taxon_6_braun_blanquet,
    taxon_7,
    taxon_7_height,
    taxon_7_braun_blanquet,
    taxon_8,
    taxon_8_height,
    taxon_8_braun_blanquet,
    taxon_9,
    taxon_9_height,
    taxon_9_braun_blanquet,
    taxon_10,
    taxon_10_height,
    taxon_10_braun_blanquet,
    taxon_11,
    taxon_11_height,
    taxon_11_braun_blanquet,
    taxon_12,
    taxon_12_height,
    taxon_12_braun_blanquet,
    taxon_13,
    taxon_13_height,
    taxon_13_braun_blanquet,
    taxon_14,
    taxon_14_height,
    taxon_14_braun_blanquet,
    taxon_15,
    taxon_15_height,
    taxon_15_braun_blanquet,
    taxon_16,
    taxon_16_height,
    taxon_16_braun_blanquet,
    taxon_17,
    taxon_17_height,
    taxon_17_braun_blanquet,
    taxon_18,
    taxon_18_height,
    taxon_18_braun_blanquet,
    taxon_19,
    taxon_19_height,
    taxon_19_braun_blanquet,
    taxon_20,
    taxon_20_height,
    taxon_20_braun_blanquet,
    vegetation_type,
    bare_ground_braun_blanquet,
    other_vegetation_type,
    tms,
    x,
    y
  )

#organised[organised == "-9999"] <- NA

head(organised)

sum(is.na(organised$soil_moisture_w))

organised$soil_moisture_w <- as.numeric(organised$soil_moisture_w)/10
organised$soil_moisture_s <- as.numeric(organised$soil_moisture_s)/10
organised$soil_moisture_e <- as.numeric(organised$soil_moisture_e)/10
organised$soil_moisture_n <- as.numeric(organised$soil_moisture_n)/10

organised$mean_soil_moisture <- round(rowMeans(organised[, c("soil_moisture_n", "soil_moisture_e", "soil_moisture_s", "soil_moisture_w")], na.rm = TRUE),2)

#organised[organised$mean_soil_moisture == NA]
organised[is.na(organised$mean_soil_moisture), ]

nas_moisture <- organised[is.na(organised$mean_soil_moisture), c("soil_moisture_w", "soil_moisture_n", "soil_moisture_s", "soil_moisture_e")]
nas_moisture

#### mean soil temp ####

organised$soil_temp_n <- as.numeric(organised$soil_temp_n)/10
organised$soil_temp_e <- as.numeric(organised$soil_temp_e)/10
organised$soil_temp_s <- as.numeric(organised$soil_temp_s)/10
organised$soil_temp_w <- as.numeric(organised$soil_temp_w)/10

organised$mean_soil_temp <- round(rowMeans(organised[, c("soil_temp_n", "soil_temp_e", "soil_temp_s", "soil_temp_w")], na.rm = TRUE),2)

nas_temp <- organised[is.na(organised$mean_soil_temp), c("soil_temp_n", "soil_temp_e", "soil_temp_s", "soil_temp_w")]
nas_temp

#### mean veg height ####

organised$vegetation_height_n <- as.numeric(organised$vegetation_height_n)
organised$vegetation_height_e <- as.numeric(organised$vegetation_height_e)
organised$vegetation_height_s <- as.numeric(organised$vegetation_height_s)
organised$vegetation_height_w <- as.numeric(organised$vegetation_height_w)

organised$mean_veg_height <- round(rowMeans(organised[, c("vegetation_height_n", "vegetation_height_s", "vegetation_height_e", "vegetation_height_w")], na.rm = TRUE),2)


function_name <- function(parameters){
  function body 
}

column_list <- colnames(organised)

## getting all the colomns##
column_string <- paste(column_list, collapse = ",\n")
cat(column_string)

organised2 <- organised |> 
  select(
    date,
    plot_name,
    start_time,
    end_time,
    #vegetation_height_n,
    #vegetation_height_e,
    #vegetation_height_s,
    #vegetation_height_w,
    #soil_moisture_n,
    #soil_moisture_e,
    #soil_moisture_s,
    #soil_moisture_w,
    #soil_temp_e,
    #soil_temp_n,
    #soil_temp_s,
    #soil_temp_w,
    topographic_complexity_cm,
    bryophyte_braun_blanquet_1,
    lichen_braun_blanquet,
    other_notes,
    vegetation_type,
    bare_ground_braun_blanquet,
    other_vegetation_type,
    tms,
    x,
    y,
    mean_soil_moisture,
    mean_soil_temp,
    mean_veg_height,
    taxon_1,
    taxon_1_height,
    taxon_1_braun_blanquet,
    taxon_2,
    taxon_2_height,
    taxon_2_braun_blanquet,
    taxon_3,
    taxon_3_height,
    taxon_3_braun_blanquet,
    taxon_4,
    taxon_4_height,
    taxon_4_braun_blanquet,
    taxon_5,
    taxon_5_height,
    taxon_5_braun_blanquet,
    taxon_6,
    taxon_6_height,
    taxon_6_braun_blanquet,
    taxon_7,
    taxon_7_height,
    taxon_7_braun_blanquet,
    taxon_8,
    taxon_8_height,
    taxon_8_braun_blanquet,
    taxon_9,
    taxon_9_height,
    taxon_9_braun_blanquet,
    taxon_10,
    taxon_10_height,
    taxon_10_braun_blanquet,
    taxon_11,
    taxon_11_height,
    taxon_11_braun_blanquet,
    taxon_12,
    taxon_12_height,
    taxon_12_braun_blanquet,
    taxon_13,
    taxon_13_height,
    taxon_13_braun_blanquet,
    taxon_14,
    taxon_14_height,
    taxon_14_braun_blanquet,
    taxon_15,
    taxon_15_height,
    taxon_15_braun_blanquet,
    taxon_16,
    taxon_16_height,
    taxon_16_braun_blanquet,
    taxon_17,
    taxon_17_height,
    taxon_17_braun_blanquet,
    taxon_18,
    taxon_18_height,
    taxon_18_braun_blanquet,
    taxon_19,
    taxon_19_height,
    taxon_19_braun_blanquet,
    taxon_20,
    taxon_20_height,
    taxon_20_braun_blanquet
  )

df_raw <- organised2

generate_dataframe <- function(number) {
  taxon_col <- sym(paste0("taxon_", number))
  height_col <- sym(paste0("taxon_", number, "_height"))
  bb_col <- sym(paste0("taxon_", number, "_braun_blanquet"))
  
  df_raw |> 
    select(1:17, !!taxon_col, !!height_col, !!bb_col) %>%
    mutate(rowid = row_number(),
           position = paste0("taxon_", number)) %>%
    rename(taxon = !!taxon_col,
           height = !!height_col,
           bb = !!bb_col)
}

taxon_list <- lapply(1:20, generate_dataframe)

pivot <- bind_rows(taxon_list) |>  
  #mutate(veg_mean_height = rowMeans(select(.,veg_height_n,veg_height_s,veg_height_e,veg_height_w))) |> 
  filter(!is.na(taxon))

pivot$bb <- as.factor(pivot$bb)
pivot$bryophyte_braun_blanquet_1 <- as.character(pivot$bryophyte_braun_blanquet_1)
pivot$bryophyte_braun_blanquet_1 <- as.factor(pivot$bryophyte_braun_blanquet_1)
pivot$bare_ground_braun_blanquet <- as.character(pivot$bare_ground_braun_blanquet)
pivot$bare_ground_braun_blanquet <- as.factor(pivot$bare_ground_braun_blanquet)
pivot$other_notes <- as.character(pivot$other_notes)
pivot$lichen_braun_blanquet <- as.character(pivot$lichen_braun_blanquet)
pivot$lichen_braun_blanquet <- as.factor(pivot$lichen_braun_blanquet)

bb_df <- as.data.frame(unique(pivot$bb))

levels(pivot$bb)

levels(pivot$bb) <- list("0.5" = "+ (<5 %; few individuals)", 
                         "2.5" = "1 (<5 %; numerous individuals)", 
                         "12.5" = "2 (5-25 %)",
                         "37.5" = "3 (25-50 %)",
                         "62.5" = "4 (50-75 %)",
                         "87.5" = "5 (75-100 %)",
                         "0.01" = "i (Species represented by a sin",
                         "0.1" = "r (<5; less than 1% plot cover,") 

pivot$bb_num <- as.character(pivot$bb)

str(pivot)

pivot$bb_num <- as.double(pivot$bb_num)

str(pivot)

stat <- pivot |> 
  select(
    #date,
    plot_name,
    taxon,
    height,
    bb,
    bb_num,
    mean_soil_moisture,
    mean_soil_temp,
    mean_veg_height,
    rowid,
    position,
    #start_time,
    #end_time,
    #topographic_complexity_cm,
    bryophyte_braun_blanquet_1,
    lichen_braun_blanquet,
    #other_notes,
    #vegetation_type,
    bare_ground_braun_blanquet,
    other_vegetation_type
    #tms,
    #x,
    #y,
  ) |> 
  filter(taxon != "")

str(stat)
summary(stat)

scices <- pivot[pivot$taxon=="Scirpus caespitosus",]
empnig <- pivot[pivot$taxon=="Empetrum nigrum",]
carbig <- pivot[pivot$taxon=="Carex bigelowii",]
desfle <- pivot[pivot$taxon=="Deschampsia flexuosa",]
vaculi <- pivot[pivot$taxon=="Vaccinium uliginosum",]
juntri <- pivot[pivot$taxon=="Juncus trifidus",]
salarc <- pivot[pivot$taxon=="Salix arctophila",]

hist(salarc$mean_soil_moisture)
regsalarc <- lm(bb_num ~ mean_soil_moisture, data = salarc)
summary(regsalarc)
plot(data = salarc, x = salarc$mean_soil_moisture, y = salarc$bb_num)
abline(regsalarc)


hist(juntri$mean_soil_moisture)

hist(vaculi$mean_soil_moisture)
regvaculi <- lm(bb_num ~ mean_soil_moisture, data = vaculi)
summary(regvaculi)
plot(data = vaculi, x = vaculi$mean_soil_moisture, y = vaculi$bb_num)
abline(regvaculi)

par(mfrow = c(2, 2))
plot(regvaculi, which = 1:4)

plot(residuals(regvaculi) ~ mean_soil_moisture, data = vaculi,
     ylab = "Residuals")
abline(h = 0)



hist(desfle$mean_soil_moisture)

hist(empnig$mean_soil_moisture)
reg1 <- lm(bb_num ~ mean_soil_moisture, data = empnig)
summary(reg1)
plot(data = empnig, x = empnig$mean_soil_moisture, y = empnig$bb_num)
abline(reg1)

hist(carbig$mean_soil_moisture)
plot(x = carbig$mean_soil_moisture, y = carbig$bb_num)


hist(pivot$mean_soil_moisture)

taxa <- pivot |> 
  group_by(taxon) |> 
  count()

mean(0.0:0.02)
mean(0.02:0.04)
mean(0.04,0.2)

pivot$bb_num <- as.double(as.character(pivot$bb))

pivot$bb_numeric <- as.numeric(as.character(pivot$bb))



## getting all the colomns##

function_name <- function(parameters){
  function body 
}

#### MAKING A FUNCTION TO EXSTRAT A LIST OF COLOMNS FOR SELECT ################################################

select_cols_list <- function(df) {
  column_list <- colnames(df)
  column_string <- paste(column_list, collapse = ",\n")
  cat(column_string, "\n")
  invisible(column_string)
}

select_cols_list(pivot)
#### NEXT SECTION ################################################


final <- pivot |> 
  select(
  date,
plot_name,
start_time,
end_time,
vegetation_height_n,
vegetation_height_e,
vegetation_height_s,
vegetation_height_w,
#soil_moisture_n,
#soil_moisture_e,
#soil_moisture_s,
#soil_moisture_w,
#soil_temp_e,
#soil_temp_n,
#soil_temp_s,
#soil_temp_w,
topographic_complexity_cm,
bryophyte_braun_blanquet_49,
lichen_braun_blanquet,
other_notes,
taxon,
height,
bb,
#taxon_2,
#taxon_2_height,
#taxon_2_braun_blanquet,
#taxon_3,
#taxon_3_height,
#taxon_3_braun_blanquet,
#taxon_4,
#taxon_4_height,
#taxon_4_braun_blanquet,
#taxon_5,
#taxon_5_height,
#taxon_5_braun_blanquet,
rowid,
position,
#taxon_1,
#taxon_1_height,
#taxon_1_braun_blanquet
)

final$bb <- as.factor(final$bb)
final$taxon <- as.factor(final$taxon)
final$bryophyte_braun_blanquet_49 <- as.factor(final$bryophyte_braun_blanquet_49)

final$mean_veg_height <- round(rowMeans(final[, c("vegetation_height_n", "vegetation_height_s", "vegetation_height_e", "vegetation_height_w")], na.rm = TRUE),2)

head(final)
# plant_traits_clean <- plant_traits |> 
#   select(
# #wkt_geom,
# #globalid,
# #CreationDa,
# #Creator,
# #EditDate,
# #Editor,
# '_date',
# #start_time,
# #end_time,
# vegetation,
# vegetati_1,
# vegetati_2,
# vegetati_3,
# vegetati_4,
# bare_groun,
# #denseness,
# #aspect,
# #untitled_2,
# plot_name,
# soil_moist,
# soil_moi_1,
# soil_moi_2,
# soil_moi_3,
# soil_temp_,
# soil_temp1,
# soil_tem_1,
# soil_tamp_,
# topographi,
# bryophyte_,
# field_47,
# lichen_bra,
# species_1_,
# species_1,
# species_11,
# species_2,
# taxon_3,
# #taxon_othe,
# taxon_4,
# taxon_2_he,
# taxon_2_br,
# taxon_3_he,
# taxon_3_br,
# taxon_4_he,
# taxon_4_br,
# taxon_5,
# taxon_5_he,
# taxon_5_br,
# taxon_6,
# taxon_6_he,
# taxon_6_br,
# taxon_7,
# taxon_7_he,
# taxon_7_br,
# taxon_8,
# taxon_8_he,
# taxon_8_br,
# other_note,
# taxon_9,
# taxon_9_he,
# taxon_9_br,
# taxon_10,
# taxon_10_h,
# taxon_10_b,
# taxon_11,
# taxon_11_h,
# taxon_11_b,
# taxon_12,
# taxon_12_h,
# taxon_12_b,
# taxon_13,
# taxon_13_h,
# taxon_13_b,
# taxon_14,
# taxon_14_h,
# taxon_14_b,
# taxon_15,
# taxon_15_h,
# taxon_15_b,
# taxon_16,
# taxon_16_h,
# taxon_16_b,
# taxon_17,
# taxon_17_h,
# taxon_17_b,
# taxon_18,
# taxon_18_h,
# taxon_18_b,
# taxon_19,
# taxon_19_h,
# taxon_19_b,
# taxon_20,
# taxon_20_h,
# taxon_20_b
# 
# #vegetati_5,
# #time
# )

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
  
