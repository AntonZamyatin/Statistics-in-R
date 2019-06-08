library(shiny)
library(r2d3)
library(vkR)
library(dplyr)
library(plyr)

server <- function(input, output) {

    write_json <- function(df, path, df_type = "rows", raw_type = "mongo"){
      require(readr)
      require(jsonlite)
      df %>% 
        toJSON(dataframe = df_type, raw = raw_type) %>%
        write_lines(path)
      df
    }

      
    
    observeEvent(input$build, {
      
      my_tok <- input$token
      
      setAccessToken(access_token = my_tok)
      
      root_id <- as.integer(input$root_id)
      
      str(root_id)
      
      user <- getUsersExecute(root_id, fields = "sex, online") %>%
        mutate(full_id=paste0(first_name, ' ', last_name, ' (id', id, ')'),) #%>%
      #select(-c(city, country))
      
      users <- getFriends(user_id = root_id, fields = 'sex,online')$items %>%
        filter(is.na(deactivated)) %>%
        mutate(full_id=paste0(first_name, ' ', last_name, ' (id', id, ')')) %>%
        select(id, first_name, last_name, sex, online, full_id) %>%
        rbind(user)
      
      ids <- users$id
      
      mutual_friends <- getMutualExecute(source_id = root_id, target_uids = paste(ids, 
                                                                                  collapse = ","))
      nodes <- users %>%
        transmute(id=full_id, group=sex)
      
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
      
      
      json_file <- file("out.json","w")
      writeLines('{"nodes": ', json_file)
      write_json(nodes, json_file)
      writeLines('\n,\n"links": ', json_file)
      write_json(links, json_file)
      writeLines('\n}', json_file)
      close(json_file)
      
      
      output$d3 <- renderD3({
        r2d3(data = jsonlite::read_json("out.json"), 
             d3_version = 4, script = "forcegraph.js")})
    })
    
}
