devtools::load_all()
# conn <- repeldata::repel_local_conn()
# repeldata::repel_local_download()

# Process trade vars (move to wahis) --------------------------------------
# DBI::dbListTables(conn)
#
# connect_yearly <- DBI::dbReadTable(conn, "connect_yearly_vars") %>%
#   collect()
#
# trade_vars <- connect_yearly %>%
#   pivot_longer(cols = -c("country_origin" , "country_destination",  "year" ))
#
# ots_lookup <- DBI::dbReadTable(conn, "connect_ots_lookup") %>%
#   collect() %>%
#   mutate(source = "ots_trade_dollars") %>%
#   select(product_code, group_name, source)
#
# connect_fao_lookup <- DBI::dbReadTable(conn, "connect_fao_lookup") %>%
#   collect() %>%
#   mutate(source = "fao_livestock_heads") %>%
#   mutate(item_code = as.character(item_code)) %>%
#   rename(group_name = item, product_code = item_code)
#
# trade_lookup <- bind_rows(ots_lookup, connect_fao_lookup)
#
# trade_vars_lookup <- trade_vars %>%
#   distinct(name) %>%
#   mutate(code =  str_remove(name, "trade_dollars_|livestock_heads_")) %>%
#   mutate(source = case_when(
#     str_detect(name, "trade_dollars_") ~ "ots_trade_dollars",
#     str_detect(name, "livestock_heads_") ~ "fao_livestock_heads",
#     name == "n_human_migrants" ~ "un_human_migration",
#     name == "n_tourists" ~ "un_wto_tourism"
#   )) %>%
#   left_join(trade_lookup, by = c("source" = "source", "code" = "product_code"))
#
# trade_vars_groups_summed <- trade_vars %>%
#   left_join(trade_vars_lookup) %>%
#   mutate(value = as.numeric(value)) %>%
#   mutate(value = replace_na(value, 0)) %>%
#   group_by(country_origin, country_destination, year, source, group_name) %>% # sum over groups
#   summarize(value = sum(value, na.rm = TRUE)) %>%
#   ungroup()
#
# vroom::vroom_write(trade_vars_groups_summed, here::here("tmp/trade_vars.csv"))
