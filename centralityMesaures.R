#Part of the N_grams code
#Starting with the df which is just a copy of the womenSoccer

#Goal is to save the centrality to this data 

#Grouping by the match 
library(tidyverse)

df<-read.csv("OneDrive - MNSCU/CMSACamp-master/open-data-master/SoccerCMSA_CMU/Scripts/Data_Wrangling/Data_after_script/WomenSoccer_Cleaned.csv",stringsAsFactors = FALSE)

df<- df %>% select(match,possession_team.name ,possession,team.name, player.name)

byMatch<-df %>% dplyr::group_by(match) %>% nest()


View(head(byMatch))




getTeam<- function(data){
  uniqTeams<- unique(as.character(factor(data$possession_team.name)))
  
  return (uniqTeams)
}

byMatch <- byMatch %>% mutate(teams = map(data,getTeam) )

getFirst<-function(listcol){
  return(listcol[[1]])
}
getSecond<-function(listcol){
  return(listcol[[2]])
}

byMatch<-byMatch %>% mutate(firstTeam = map(teams,getFirst) , secondTeam = map(teams,getSecond)) %>% select(-teams)

View(head(byMatch))

groupMe<-function (df){
  df2<- df %>% group_by(possession,possession_team.name) %>% nest()
  return (df2)
}
byMatch <- byMatch %>% mutate(data = map(data,groupMe))

View(head(byMatch))


#Filtering the byMatch$data for one team and then we will have to do that again for the byMatch$data$data by the same team_name not possesion

byMatch<- byMatch %>% mutate(first_team_data = map2(data,firstTeam,~filter(.x,possession_team.name == .y)),second_team_data = map2(data,secondTeam,~filter(.x,possession_team.name == .y))) %>% select(-data)



View(head(byMatch))


  

oneTeam_filter<- function(nested_data){
  possessionTeam <- nested_data$possession_team.name
  new_data<- nested_data %>% mutate(data = map2(data,possessionTeam,~filter(.x,team.name ==.y)))
  return (new_data)
}

byMatch<- byMatch %>% mutate(first_team_data = map(first_team_data,oneTeam_filter), 
                             second_team_data = map(second_team_data,oneTeam_filter))
View(head(byMatch))
save(byMatch,file = "byMatch.RData")
##########################################Comment
load(file = "byMatch.RData")

library(tidytext)
getNgrams<- function(tdf){
  pats <- " |-"
  col_text<-tdf %>% select(player.name) 
  strReplace<- function(s,pattern = " |-", replacement = "_")
      
              { return (str_replace_all(s,pattern,replacement))}
  
  col_text <- map(col_text, strReplace)
  #Making a corpus
  
  text_tdf <- tibble(text = unlist(col_text))
 
 if(nrow(text_tdf)==0){
   text_tdf<-text_tdf %>% add_row(text = "NA")
 }
   n_grams<-text_tdf %>% unnest_tokens(n_gram , text, token = "ngrams", n = 2) 
  
  return (n_grams)
  }

getNGram_wrapper<- function(nested_data){
  new_data<- nested_data %>% filter() %>% dplyr::mutate(data = map(data,getNgrams)) 
  return (new_data)
}

byMatch <- byMatch %>% dplyr::mutate(ngram_first = map(first_team_data,getNGram_wrapper) , ngram_second = map(second_team_data,getNGram_wrapper)) %>% select(-first_team_data,-second_team_data)

View(head(byMatch))

save(byMatch,file = "byMatch2.RData")

filterGroup<- function(nested_data){
  new_data<- nested_data %>% unnest() %>% filter(!is.na(n)) %>% group_by(n_gram) %>%
                  summarise(counts = n()) %>% filter(!is.na(n_gram)) %>%arrange(desc(counts))
  
  return (new_data)
  
}

byMatch<- byMatch %>% mutate(ngram_first = map(ngram_first, filterGroup) ,ngram_second = map(ngram_second, filterGroup))

View(head(byMatch))

#######################################################################################3
#Unique Nodes

source_dest_wrapper<- function (df){
  df <- df %>% separate(n_gram, c("source", "destination"), sep = " ")
  return (df)
}
byMatch<- byMatch %>% mutate(ngram_first = map(ngram_first,source_dest_wrapper) ,
                            ngram_second = map(ngram_second,source_dest_wrapper))

sourceWrapper<- function(df){
  sources <- df %>%
    distinct(source) %>%
    rename(label = source)
  return (sources)
}
DestinationWrapper<- function(df){
  destination <- df %>%
    distinct(destination) %>%
    rename(label = destination)
  return (destination)
}

byMatch <- byMatch %>% mutate(sources_first = map(ngram_first,sourceWrapper), destinations_first = map(ngram_first,DestinationWrapper),
                              sources_second = map(ngram_second,sourceWrapper), destinations_second = map(ngram_second,DestinationWrapper))

View(head(byMatch))

fulljoin_wrapper<-function(df1,df2){
  df3<-full_join(df1,df2,by = "label")
  return (df3)
}
byMatch <- byMatch %>% mutate(nodes_first = map2(sources_first,destinations_first,~full_join(.x,.y, by = "label")), nodes_second = map2(sources_second,destinations_second,~full_join(.x,.y, by = "label")))

id_wrapper<- function(df){
  df<- df %>% rowid_to_column("id")
  return (df)
}
byMatch <- byMatch %>% mutate(nodes_first =map(nodes_first,id_wrapper), nodes_second = map(nodes_second,id_wrapper))



#Edge list 

per_route_wrapper <- function(df){
  df<-df %>%  filter(counts>1) %>% rename(weights = counts)
  return (df)
}

byMatch<- byMatch %>% mutate(per_route_first = map(ngram_first,per_route_wrapper), per_route_second = map(ngram_second,per_route_wrapper))

edges_wrapper_route<- function(df,nodes){
edges <- df %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)
return (edges)
}

byMatch<- byMatch %>% mutate(edges_first = map2(per_route_first ,nodes_first,~edges_wrapper_route(.x,.y)), edges_second = map2(per_route_second,nodes_second,~edges_wrapper_route(.x,.y)))

edges_wrapper_route_2<- function(df,nodes){
  edges <- df %>% 
    left_join(nodes, by = c("destination" = "label"))  %>% 
    rename(to = id)
  return (edges)
}
byMatch<- byMatch %>% mutate(edges_first_two = map2(edges_first ,nodes_first,~edges_wrapper_route_2(.x,.y)), edges_second_two = map2(edges_second,nodes_second,~edges_wrapper_route_2(.x,.y)))



edges_select_wrapper <- function(df){ 
  df<-df%>% dplyr::select(from, to, weights)
return (df)
}

byMatch<- byMatch %>% mutate(edges_first_two = map(edges_first_two ,edges_select_wrapper), edges_second_two = map(edges_second_two ,edges_select_wrapper))

save(byMatch, file = "BeforeNetwork.RData")
#Network #use node_first and node _second and the  edges_first_two,..edges_second_two
library(igraph)
library(tidygraph)
library(ggraph)

routes_tidy_wrapper <- function(nodes,edges){
  routes_tidy<-tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
  return(routes_tidy)
}

networkDF<- byMatch %>% select(match, firstTeam,secondTeam,nodes_first,nodes_second,edges_first_two,edges_second_two)
save(networkDF, file = "NetworkDF.RData")

View(head(networkDF))

networkDF<-networkDF %>% mutate(routes_first = map2(nodes_first,edges_first_two,~routes_tidy_wrapper(.x,.y)),
                     routes_second = map2(nodes_second,edges_second_two,~routes_tidy_wrapper(.x,.y)))
View(head(networkDF))

activate_edge_wrapper<-function(routes_tidy_df){  
  routes_tidy_df<- routes_tidy_df %>% activate(edges) %>% arrange(desc(weights))
  return(routes_tidy_df)
}
networkDF<-networkDF %>% mutate(routes_first = map(routes_first,activate_edge_wrapper),
                                routes_second = map(routes_second,activate_edge_wrapper))
#Adding weights
add_weight_wrapper<-function(routes_tidy){
  routes_tidy<-routes_tidy %>% activate(nodes) %>% mutate(centrality = centrality_authority(), pageRank = centrality_pagerank())
  return (as_tibble(routes_tidy))
 }
networkDF<-networkDF %>% mutate(withWeight_first = map(routes_first,add_weight_wrapper),
                                withWeight_second = map(routes_second,add_weight_wrapper))

weightDF <- networkDF %>% select(-routes_first,-routes_second,-nodes_first,-nodes_second,-edges_first_two,-edges_second_two)

save(weightDF,file = "WeightDF.RData")
View(weightDF)

match_wrapper<- function(df,m){
  df<- df %>% mutate(match = m )
  return (df)
}
weightDF2<- weightDF %>% mutate(match = map(withWeight_first,match,~match_wrapper(.x,.y)))

weightDF_firstTeam <- weightDF %>% select(match,withWeight_first) %>% unnest()
weightDF_secondTeam<-weightDF %>% select(match,withWeight_second) %>% unnest()

DF_with_Weights<-rbind(weightDF_secondTeam)
View(DF_with_Weights)

