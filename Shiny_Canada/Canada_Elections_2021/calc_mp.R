

calc_mp <- function(dataset) {
  df <- dataset %>% 
    group_by(`Political affiliation`) %>% 
    count(Winner) %>% 
    filter(Winner == "Winner") %>% 
    select(-Winner) %>% 
    rename("MP's" = n)
  df
}