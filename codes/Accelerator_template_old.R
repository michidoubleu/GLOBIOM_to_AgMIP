args <- commandArgs(trailingOnly = TRUE)


suppressWarnings(library(tidyverse))
suppressWarnings(library(gdxrrw))
suppressWarnings(library(ggforce))
library(unpivotr)
library(data.table)

gdxpath <- "C:/GAMS/42"
igdx(gdxpath)


if(is_empty(args)){
  Output_AG <- droplevels(rgdx.param("../Trunk_April2025/Model/gdx/LPU_2025-08-04/full_merge.gdx", symName="OUTPUT_AG"))
} else {
  Output_AG <- droplevels(rgdx.param(args[1], symName="OUTPUT_AG"))
}



colnames(Output_AG) <- c("ALLRUN", "shit2","VAR_ID", "VAR_UNIT", "ANYREGION",  "ITEM_AG","ALLSCEN1", "ALLSCEN2", "ALLSCEN3",  "Year","Value")

Output_AG <- Output_AG %>% dplyr::select(-starts_with("shit"))
  
Output_AG_DM <- Output_AG %>%
  filter( VAR_ID %in% c("YIRF", "YIIR", "YEXO", "food", "Feed",
                        "OTHU", "IMPO", "EXPO", "Prod", "CONS",
                        "NETT", "YILD")) %>%
  filter(VAR_UNIT %in% c( "1000 t dm", "dm t/ha"))

Output_AG_DM <- Output_AG_DM %>%
  mutate(VAR_UNIT = case_when( VAR_UNIT == "1000 t dm"~"1000 t", 
                               VAR_UNIT == "dm t/ha"~"t/ha")) %>%
  mutate(ANYREGION = str_replace_all(ANYREGION, fixed("World"), "WLD")) %>%
    mutate(VAR_ID = case_when( VAR_ID == "YILD"~"YILD_dm",
                   VAR_ID == "YIRF"~"YIRF_dm",
                   VAR_ID == "YIIR"~"YIIR_dm",
                   VAR_ID == "YEXO"~"YEXO_dm",
                   VAR_ID == "food"~"FOOD_dm",
                   VAR_ID == "Feed"~"FEED_dm",
                   VAR_ID == "OTHU"~"OTHU_dm",
                   VAR_ID == "IMPO"~"IMPO_dm",
                   VAR_ID == "EXPO"~"EXPO_dm",
                   VAR_ID == "Prod"~"PROD_dm",
                   VAR_ID == "CONS"~"CONS_dm",
                   VAR_ID == "NETT"~"NETT_dm",))

OUTPUT_AG_Oth <- Output_AG %>%
  filter(VAR_ID %in% c("POPT", "GDPT", "Area", "ARRF", "ARIR", "YILD", "YIRF", "YIIR", "YEXO",
                       "Feed", "OTHU", "IMPO", "EXPO", "WATR", "CALO", "CALI", "Prod", "CONS",
                       "NETT", "EMIS", "ECO2", "ECH4", "EN2O", "CTAX", "NBAL", "FRTIN", "FRTON", "FRTIP", 
                       "FRTOP", "PBAL", "LYLD", "LYXO", "YEXO_I", "YEXO_R", "YEXO_I", "YEXO_R", "XPRP", "XPRX")) %>% 
    mutate(VAR_ID = str_replace_all(VAR_ID, fixed("ECH4"), "ECH4_eq")) %>%
  mutate(VAR_ID = str_replace_all(VAR_ID, fixed("EN2O"), "EN2O_eq")) %>%
  mutate(ANYREGION = str_replace_all(ANYREGION, fixed("World"), "WLD")) %>%
  filter(VAR_UNIT %in% c("Mln pers", ## change to million
                         "Bn USD 2005", #change to bn USD 2005 MER
                         "USD 2000 per ton", #change to USD/t
                         "1000 ha",
                         "t/ha",
                         "1000 t",
                         "km3",
                         "kcal/cap/d",
                         "Mt CO2eq/yr", #change to MtCO2e
                         "USD/tCO2e",
                         "kg protein/ha")) %>%
  mutate(VAR_UNIT = case_when(VAR_UNIT == "Mln pers"~"Million", ## change to million
                              VAR_UNIT == "Bn USD 2005"~"bn USD 2005 MER", #change to bn USD 2005 MER
                              VAR_UNIT == "USD 2000 per ton"~"USD/t", #change to USD/t
                              VAR_UNIT == "1000 ha"~"1000 ha",
                              VAR_UNIT == "t/ha"~"t/ha",
                              VAR_UNIT == "1000 t"~"1000 t",
                              VAR_UNIT == "km3"~"km3",
                              VAR_UNIT == "kcal/cap/d"~"kcal/cap/d",
                              VAR_UNIT == "Mt CO2eq/yr"~ "MtCO2e", #change to MtCO2e
                              VAR_UNIT == "USD/tCO2e"~"USD/tCO2e",
                              VAR_UNIT == "kg protein/ha" ~ "kg prt/ha"))

OUTPUT_AG_Emis <- Output_AG %>%
  mutate(ANYREGION = str_replace_all(ANYREGION, fixed("World"), "WLD")) %>%
  filter(VAR_ID %in% c("CH4", "N2O")) %>% 
  mutate(VAR_ID = str_replace_all(VAR_ID, fixed("CH4"), "ECH4")) %>%
  mutate(VAR_ID = str_replace_all(VAR_ID, fixed("N2O"), "EN2O")) %>%
  mutate(VAR_UNIT = case_when(VAR_UNIT == "Mt CH4/yr"~"ktCH4", ## change to million
                              VAR_UNIT == "Mt N2O/yr"~"ktN2O"))

OUTPUT_AG_Emis$Value <- OUTPUT_AG_Emis$Value/1000
# OUTPUT_AG_Emis$OUTPUT_AG <- OUTPUT_AG_Emis$OUTPUT_AG/1000
# OUTPUT_AG_Emis <-OUTPUT_AG_Emis %>%


Output_Ag_Proto <- Output_AG %>%
  mutate(ANYREGION = str_replace_all(ANYREGION, fixed("World"), "WLD")) %>%
  filter(VAR_ID == "NUTR") %>%
  filter(VAR_UNIT == "gprot/cap/d") %>%
  mutate(VAR_ID = str_replace_all(VAR_ID, fixed("NUTR"), "PROTO"))  %>%
  mutate(VAR_UNIT = str_replace_all(VAR_UNIT, fixed("gprot/cap/d"), "gprt/cap/day")) 

Output_Ag_Proti <- Output_AG %>%
  mutate(ANYREGION = str_replace_all(ANYREGION, fixed("World"), "WLD")) %>%
  filter(VAR_ID == "NTRI") %>%
  filter(VAR_UNIT == "gprot/cap/d") %>%
  mutate(VAR_ID = str_replace_all(VAR_ID, fixed("NTRI"), "PROTI")) %>%
  mutate(VAR_UNIT = str_replace_all(VAR_UNIT, fixed("gprot/cap/d"), "gprt/cap/day")) 

Output_Ag_Land_noncrop <- Output_AG %>%
  mutate(ANYREGION = str_replace_all(ANYREGION, fixed("World"), "WLD")) %>%
  filter(VAR_ID == "LAND2") %>%
  filter(ITEM_AG %in% c("1.2.2.PlantationEnerCrp",
                        "3.Forest",
                        "2.Grassland",
                        "5.OthLand",
                        "4.OthNatLand",
                        "0.TotLand")) %>%
  mutate(VAR_ID = str_replace_all(VAR_ID, fixed("LAND2"), "LAND")) %>%
  mutate(ITEM_AG = str_replace_all(ITEM_AG, fixed("0.TotLand"), "TOT")) %>%
  mutate(ITEM_AG = str_replace_all(ITEM_AG, fixed("3.Forest"), "FOR")) %>%
  mutate(ITEM_AG = str_replace_all(ITEM_AG, fixed("1.2.2.PlantationEnerCrp"), "ECP")) %>%
  mutate(ITEM_AG = str_replace_all(ITEM_AG, fixed("2.Grassland"), "GRS")) %>%
  mutate(ITEM_AG = str_replace_all(ITEM_AG, fixed("5.OthLand"), "NLD")) %>%
  mutate(ITEM_AG = str_replace_all(ITEM_AG, fixed("4.OthNatLand"), "ONV")) 

Output_Ag_Land_CRP <- Output_AG %>%
  mutate(ANYREGION = str_replace_all(ANYREGION, fixed("World"), "WLD")) %>%
  filter(VAR_ID == "LAND2") %>%
  mutate(VAR_ID = str_replace_all(VAR_ID, fixed("LAND2"), "LAND")) %>%
  filter(ITEM_AG %in% c("1.1.AnnualCrp",
                        "1.2.1.PlantationFoodCrp",
                        "1.3.OthCrpLnd")) %>%
  mutate(VAR_ID = str_replace_all(VAR_ID, fixed("LAND2"), "LAND")) %>%
  mutate(ITEM_AG=recode(ITEM_AG, "1.1.AnnualCrp"="CRP", 
                        "1.2.1.PlantationFoodCrp"="CRP",
                        "1.3.OthCrpLnd"="CRP")) %>% 
  group_by(VAR_ID, VAR_UNIT, ANYREGION, ALLRUN, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year) %>% 
  summarise (Value=sum(Value))

Output_Ag_Land_CRP <- cbind(Output_Ag_Land_CRP[,1:3], "CRP", Output_Ag_Land_CRP[,4:9])
names(Output_Ag_Land_CRP)[4] <- "ITEM_AG"
Output_Ag_Land <- rbind(Output_Ag_Land_noncrop, Output_Ag_Land_CRP)

Output_Ag_FOOD <- Output_AG %>%
  mutate(ANYREGION = str_replace_all(ANYREGION, fixed("World"), "WLD")) %>%
  filter(VAR_ID %in% c("food", "WAST")) %>%
  filter(VAR_UNIT == "1000 t") %>%
  mutate(VAR_ID=recode(VAR_ID, "food"="FOOD2", "WASTE"="FOOD2")) %>% 
  group_by(VAR_UNIT, ANYREGION, ITEM_AG, ALLRUN, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year) %>% 
  summarise (Value=sum(Value))

Output_Ag_FOOD <- cbind("FOOD", Output_Ag_FOOD)
names(Output_Ag_FOOD)[1] <- "VAR_ID"

# Output_BIOU <- Output_AG %>%
#   filter(VAR_ID == "BIOU")

Output_Ag_FRTN <- Output_AG %>%
  mutate(ANYREGION = str_replace_all(ANYREGION, fixed("World"), "WLD")) %>%
  filter(VAR_ID %in% c("FRTIN", "FRTON")) %>%
  filter(VAR_UNIT == "1000 t") %>%
  mutate(VAR_ID=recode(VAR_ID, "FRTIN"="FRTN", "FRTON"="FRTN")) %>% 
  group_by(VAR_UNIT, ANYREGION, ITEM_AG, ALLRUN, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year) %>% 
  summarise (Value=sum(Value))
Output_Ag_FRTN <- cbind("FRTN", Output_Ag_FRTN)
names(Output_Ag_FRTN)[1] <- "VAR_ID"


Output_Ag_FRTP <- Output_AG %>%
  mutate(ANYREGION = str_replace_all(ANYREGION, fixed("World"), "WLD")) %>%
  filter(VAR_ID %in% c("FRTIP", "FRTOP")) %>%
  filter(VAR_UNIT == "1000 t") %>%
  mutate(VAR_ID=recode(VAR_ID, "FRTIP"="FRTP", "FRTOP"="FRTP")) %>% 
  group_by(VAR_UNIT, ANYREGION, ITEM_AG, ALLRUN, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year) %>% 
  summarise (Value=sum(Value))
Output_Ag_FRTP <- cbind("FRTP", Output_Ag_FRTP)
names(Output_Ag_FRTP)[1] <- "VAR_ID"

OUTPUT_AG_t <- rbind(Output_AG_DM, OUTPUT_AG_Oth, OUTPUT_AG_Emis, 
                     Output_Ag_Proto, Output_Ag_Proti, Output_Ag_FOOD, Output_Ag_Land, 
                     Output_Ag_FRTP, Output_Ag_FRTN)
# , Output_BIOU
OUTPUT_AG_t <- OUTPUT_AG_t %>%
  mutate(Scen = paste(ALLRUN, ALLSCEN1, ALLSCEN2,ALLSCEN3,sep = "-"))
OUTPUT_AG_t <- OUTPUT_AG_t %>% dplyr::select(-ALLRUN)

OUTPUT_AG_ACCELERATOR <- cbind("GLOBIOM", OUTPUT_AG_t[, c(10, 3, 4, 1, 8, 2, 9)]) 
colnames(OUTPUT_AG_ACCELERATOR) <- c("Model", "Scenario", "Region", "Item", "Variable", "Year", "Unit", "Value")


for(jj in unique(OUTPUT_AG_ACCELERATOR$Scenario)){
OUTPUT_AG_ACCELERATOR.temp <- OUTPUT_AG_ACCELERATOR %>% filter(Scenario==jj)
OUTPUT_AG_ACCELERATOR.temp <- OUTPUT_AG_ACCELERATOR.temp %>%
  filter(Year %in% seq(2000,2100,10))
OUTPUT_AG_ACCELERATOR.temp <- OUTPUT_AG_ACCELERATOR.temp[!OUTPUT_AG_ACCELERATOR.temp$Region == "ROW",]
#OUTPUT_AG_ACCELERATOR.temp <- OUTPUT_AG_ACCELERATOR.temp[!OUTPUT_AG_ACCELERATOR.temp$Region == "CHE",]

OUTPUT_AG_ACCELERATOR.temp <- OUTPUT_AG_ACCELERATOR.temp[!OUTPUT_AG_ACCELERATOR.temp$Item %in% c("Meat", "ALL", "CER", "SRP",
                                                                                    "PULP", "MEAL", "BIOM", "LUC",
                                                                                    "LUCF", "LUCC", "LUCG", "LUCE",
                                                                                    "LUCS", "SOC", "SOCE", "SOCC",
                                                                                    "SOCG", "SOCB", "PEAT"),]

if(length(args)<2){
  write.csv(OUTPUT_AG_ACCELERATOR.temp, paste0("./",jj,"_for_accelerator.csv"), row.names=FALSE)
} else {
  write.csv(OUTPUT_AG_ACCELERATOR.temp, paste0(args[2],jj,"_for_accelerator.csv"), row.names=FALSE)
}
}
