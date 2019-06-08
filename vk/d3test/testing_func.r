library(vkR)
library(rvkstat)
library("dplyr")
library(jsonlite)
# https://oauth.vk.com/authorize?client_id=7011078&scope=1319942&redirect_uri=https://oauth.vk.com/blank.html&display=page&response_type=token&revoke=1

setAccessToken(access_token = "51c540026c4751a4e542bcac8d3302ec154017a8d46a2043bdb13bf1eca627f8cb4ef97c5612a36ac6d8a")




mut_f <- function (source_id = "", target_uid = "", target_uids = "", 
          order = "", count = "", offset = "", progress_bar = FALSE, 
          v = 5.95) 
{
  get_mutual_friends <- function(source_id = "", target_uids = "", 
                                 order = "", count = "", offset = "", v = getAPIVersion()) {
    code <- "var mutual_friends = [];"
    num_requests <- ifelse(length(target_uids)%%100 == 0, 
                           (length(target_uids)%/%100), (length(target_uids)%/%100) + 
                             1)
    from <- 1
    to <- ifelse(num_requests >= 2, 100, length(target_uids))
    for (i in 1:num_requests) {
      code <- paste0(code, "mutual_friends = mutual_friends + API.friends.getMutual({
                     \n                     \"source_uid\":\"", 114520,
                    "\",\n                     \"target_uids\":\"", 
                     paste0(target_uids[from:to], collapse = ","),
                     "\",\n                     \"count\":\"", count,
                     "\",\n                     \"v\":\"", 5.95, "\"});")
      from <- to + 1
      to <- to + ifelse(length(target_uids) - (to + 100) >= 
                          0, 100, length(target_uids) - to)
    }
    code <- paste0(code, "return mutual_friends;")
    if (nchar(code) > 65535) 
      stop("The POST request is limited by 65535 bytes")
    str(execute(code))
  }
  if (target_uid != "") 
    target_uids <- target_uid
  mutual_friends <- data.frame()
  from <- 1
  to <- 2500
  if (progress_bar) {
    pb <- txtProgressBar(min = 0, max = length(target_uids), 
                         style = 3)
    setTxtProgressBar(pb, 0)
  }
  repeat {
    if (to >= length(target_uids)) 
      to <- length(target_uids)
    friends <- get_mutual_friends(source_id = source_id, 
                                  target_uids[from:to], order = order, count = count, 
                                  offset = offset, v = v)
    str(friends)
    
    mutual_friends <- jsonlite::rbind_pages(list(mutual_friends, 
                                               friends))
    if (progress_bar) 
      setTxtProgressBar(pb, nrow(mutual_friends))
    if (to >= length(target_uids)) 
      break
    from <- to + 1
    to <- to + 2500
  }
  if (progress_bar) 
    close(pb)
  mutual_friends
}






root_id <- 114520

user <- getUsersExecute(root_id, fields = "sex, online") %>%
  mutate(full_id=paste0(first_name, ' ', last_name, ' (id', id, ')'),) #%>%
  #select(-c(city, country))

users <- getFriends(user_id = root_id, fields = 'sex,online')$items %>%
  filter(is.na(deactivated)) %>%
  mutate(full_id=paste0(first_name, ' ', last_name, ' (id', id, ')')) %>%
  select(id, first_name, last_name, sex, online, full_id) %>%
  rbind(user)





ids <- users$id
n <- length(ids)
#adjacency_matrix <- data.frame(matrix(data = rep(0, n * 
#                                                   n), nrow = n, ncol = n), row.names = my_friends$id)
#colnames(adjacency_matrix) <- my_friends$id



mutual_friends <- mut_f(target_uids = paste(ids, 
                                                collapse = ","))


write_json <- function(df, path, df_type = "rows", raw_type = "mongo"){
  require(readr)
  require(jsonlite)
  df %>% 
    toJSON(dataframe = df_type, raw = raw_type) %>%
    write_lines(path)
  df
}


nodes <- users %>%
  transmute(id=full_id, group=sex)



str(mutual_friends)


bind_func <- function(x, df){ 
  
}


links <- data.frame("source" = user$full_id, "target" = users$full_id, "value" = 10)

for (i in seq(1:length(mutual_friends[[1]]))){
  source <- mutual_friends[[1]][i]
  source_id <- users %>%
    filter(id == source)
  source_id <- source_id$full_id
  target_ids = mutual_friends[[2]][[i]]
  targets <- users %>%
    filter(id %in% target_ids) %>%
    transmute(id=full_id)
  if (length(target_ids)>0){
    links <- rbind(links, data.frame("source" = source_id, "target" = targets$id, "value" = 10))
  }
}


mutual_friends[[2]][[30]]

json_file <- file("test.json","w")
writeLines('{"nodes": ', json_file)
write_json(nodes, json_file)
writeLines('\n,\n"links": ', json_file)
write_json(links, json_file)
writeLines('\n}', json_file)
close(json_file)

for (friend_id in mutual_friends$id) {
  friends <- mutual_friends$common_friends[friend_id]
  if (length(friends) > 0) {
    share_friends <- intersect(users_ids, friends)
    if (length(share_friends) > 0) {
      for (shared_user_id in 1:length(share_friends)) {
        adjacency_matrix[as.character(share_friends[shared_user_id]), 
                         as.character(users_ids[friend_id])] <- 1
      }
    }
  }
}

network<- adjacency_matrix

library("igraph")
g <- graph.adjacency(as.matrix(network), weighted = T, mode = "undirected")
layout <- layout.fruchterman.reingold(g)
plot(g, layout = layout)


domain <- '173822848'
wall <- getWallExecute(domain = domain, count = 0, progress_bar = TRUE)
metrics <- jsonlite::flatten(wall$posts[c("date", "likes", "comments", "reposts")])
metrics$date <- as.POSIXct(metrics$date, origin="1970-01-01", tz='Europe/Moscow')

library(dplyr)
df <- metrics %>% 
  mutate(period = as.Date(cut(date, breaks='month'))) %>% 
  group_by(period) %>%
  summarise(likes = sum(likes.count), comments = sum(comments.count), reposts = sum(reposts.count), n = n())

library(ggplot2)
library(tidyr)
ggplot(data=gather(df, 'type', 'count', 2:5), aes(period, count)) + geom_line(aes(colour=type)) +
  labs(x='Date', y='Count')

text <- system.file("examples/baranims.js", package = "r2d3")
text


}
response$response