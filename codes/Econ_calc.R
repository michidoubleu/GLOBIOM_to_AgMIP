########  Econ value calc script

library(dplyr)
library(tidyr)
library(data.table)

# -------------------------
# Read GDX
# -------------------------
output_econ <- tryCatch(
  readGDX(GLOBIOM.file, symbols = "OUTPUT"),
  error = function(e) stop("Failed to read OUTPUT_AG: ", e$message)
)

output_econ <- output_econ$OUTPUT$records
new_names <- c("ALLRUN", "VAR_ID", "VAR_UNIT", "ANYREGION", "ITEM_AG",
               "ALLSCEN1","ALLSCEN2","ALLSCEN3","Year","Value")
colnames(output_econ) <- new_names

costs <- read.csv("open_input/LAMASUS_costs_country_20260420.csv")
cost.items <- unique(costs$ALLITEM)



##### Crop sector - BEGIN #####

area.item <- output_econ %>% filter(ITEM_AG %in% cost.items, VAR_ID=="Area", VAR_UNIT=="1000 ha") %>% mutate(area=Value) %>% dplyr::select(-VAR_UNIT, -VAR_ID, -Value)
prod.item <- output_econ %>% filter(ITEM_AG %in% cost.items, VAR_ID=="Prod", VAR_UNIT=="1000 t") %>% mutate(prod=Value) %>% dplyr::select(-VAR_UNIT, -VAR_ID, -Value)
prices <- output_econ %>% filter(ITEM_AG %in% cost.items, VAR_ID=="XPRP") %>% mutate(price=Value) %>% dplyr::select(-VAR_UNIT, -VAR_ID, -Value)
subsidies <- output_econ %>% filter(ITEM_AG %in% cost.items, VAR_ID=="BUDG", VAR_UNIT=="Mn USD 2000") %>% mutate(subs=Value) %>% dplyr::select(-VAR_UNIT, -VAR_ID, -Value)

full <- prices %>% left_join(prod.item) %>% 
  left_join(area.item) %>% 
  left_join(subsidies) %>% 
  left_join(costs %>% 
              group_by(country, ALLITEM) %>% 
              summarise(cost=sum(value)) %>% 
              mutate(ANYREGION=paste0(country,"Reg")) %>% ungroup() %>%
              dplyr::select(-country) %>% rename("ITEM_AG"="ALLITEM"))

full %>% mutate(econ.cost=cost*area, econ.rev=price*prod/1000, econ.sub=subs, econ.inc=econ.rev-econ.cost) %>% na.omit()

df_plot <- full %>%
  mutate(
    C.econ.cost = cost * area,
    B.econ.rev  = price * prod / 1000,
    D.econ.sub  = subs,
    A.econ.inc  = B.econ.rev - C.econ.cost + D.econ.sub
  ) %>% 
  select(ANYREGION, Year, ALLSCEN3, ITEM_AG, A.econ.inc, D.econ.sub, B.econ.rev, C.econ.cost) %>%
  na.omit()   %>%
  pivot_longer(
    cols = contains("econ"),
    names_to = "variable",
    values_to = "value"
  )

df_plot <- df_plot %>% 
  group_by(ALLSCEN3, Year, variable) %>% 
  summarise(value = sum(value))

ggplot(
  df_plot,
  aes(x = Year, y = value, color = ALLSCEN3, group = ALLSCEN3)
) +
  geom_line(linewidth = 1) +
  facet_grid(~ variable) +
  theme_minimal() +
  labs(
    title = "Economic quantities – crop sector",
    y = "Mio USD"
)

##### Crop sector - END #####

# 
# 
# df_diff <- df_plot %>%
#   group_by(ALLSCEN3, Year, variable) %>%
#   summarise(value = sum(value), .groups = "drop") %>%
#   filter(Year %in% c(2020, 2050), variable == "C.econ.cost") %>%
#   pivot_wider(names_from = Year, values_from = value) %>%
#   mutate(diff = `2050` - `2020`) 
# 
# ggplot(
#   df_diff,
#   aes(x = ALLSCEN3, y = diff, fill = ALLSCEN3)
# ) +
#   geom_col() +
#   theme_minimal() +
#   labs(
#     title = "Difference 2050 vs 2020 – econ.inc",
#     x = "Scenario",
#     y = "Change (Mio USD)"
#   ) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     legend.position = "none"
#   )
