soccer_new<-read.csv("https://raw.githubusercontent.com/KapilKhanal/Soccer/master/soccer_new.csv",stringsAsFactors = FALSE)

strReplace<- function(s,pattern = " |-", replacement = "_")
  
{ return (str_replace_all(s,pattern,replacement))}

soccer_new<-soccer_new %>% mutate(player.name = unlist(map(player.name,strReplace)), pass.recipient.name = unlist(map(pass.recipient.name,strReplace))) 

soccer_new<- soccer_new %>% mutate(player.name = unlist(map(player.name,str_to_lower)), pass.recipient.name = unlist(map(pass.recipient.name,str_to_lower)))
View(head(soccer_new))

soccer_new<-soccer_new %>% mutate(player.name = unlist(map(player.name,as.character)), pass.recipient.name =unlist(map(pass.recipient.name,as.character)) )

DF_with_Weights <- DF_with_Weights %>% rename(player.name = label)  %>% select(-id)
DF_with_Weights_recipient<- DF_with_Weights %>% rename(pass.recipient.name = player.name)  

View(DF_with_Weights)

final_df_origin <- left_join(soccer_new,DF_with_Weights)
final_df_origin<-final_df_origin %>% rename(centrality_origin = centrality, pageRank_origin = pageRank)

final_df_recipient <- left_join(soccer_new,DF_with_Weights_recipient)

final_df_recipient<-final_df_recipient %>% rename(centrality_recipient = centrality, pageRank_recipient = pageRank)

final_df<- left_join(final_df_origin,final_df_recipient)

View(final_df)

write.csv(final_df,file = "OneDrive - MNSCU/CMSACamp-master/open-data-master/SoccerCMSA_CMU/Scripts/Data_Wrangling/Data_after_script/allDone_csv.csv")
