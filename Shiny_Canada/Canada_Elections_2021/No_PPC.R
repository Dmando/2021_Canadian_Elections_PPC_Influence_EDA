


No_PPC <- function(Conservative, NDP, Liberal, Bloc_Quebecois){
  df <- df %>% 
    dplyr::mutate(`Votes obtained` = dplyr::if_else(`Political affiliation` == "Conservative", `Votes obtained` + Conservative * PPC_Vote, `Votes obtained`),
           `Votes obtained` = dplyr::if_else(`Political affiliation` == "NDP", `Votes obtained` + NDP * PPC_Vote, `Votes obtained`),
           `Votes obtained` = dplyr::if_else(`Political affiliation` == "Liberal", `Votes obtained` + Liberal * PPC_Vote, `Votes obtained`),
           `Votes obtained` = dplyr::if_else(`Political affiliation` == "	Bloc Québécois", `Votes obtained` + Bloc_Quebecois * PPC_Vote, `Votes obtained`)) %>% 
    # Only major parties have the mutate ability since lesser parties do not have the clout to influence elections meaningfully, assuming that PPC voters did not form a new political party in the simulations
    dplyr::filter(`Political affiliation` != "PPC") %>% 
    dplyr::select(-PPC_Vote) %>% 
    dplyr::group_by(`Electoral district name`) %>% 
    dplyr::mutate(Rank = rank(-`Votes obtained`),
           Winner = dplyr::if_else(Rank == 1, "Winner", "Loser"),
           `% Votes obtained ` = round(`Votes obtained`/ Total_valid_votes * 100, digits = 2))
  df$`Votes obtained` <- round(df$`Votes obtained`)
  df
}

No_PPC(1, 0, 0, 0)
