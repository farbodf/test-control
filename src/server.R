library(shiny)
library(xts)
library(data.table)
library(ggplot2)
library(dygraphs)
library(plotly)
library(shinydashboard)

source('utils.R')

server <- function(input, output) {
  #### Reading in data file
  events <- fread("events.csv")
  
  #### Creating a summary table for each user and their actions
  summary_user <- events[,.(minigames_played=count_event_upto(event, "minigame_played", "subscribed"),
                            songs_played=count_event_upto(event, "song_played", "subscribed"),
                            converted_to_play_song=converted_after(event, "account created", "song_played"),
                            subscribed="subscribed" %in% event,
                            unsubscribed="unsubscribed" %in% event,
                            app_installed="app install" %in% event,
                            account_created="account created" %in% event,
                            minigame_played="minigame_played" %in% event,
                            song_played="song_played" %in% event,
                            .N) ,by=.(uid, group)]
  summary_user <- summary_user[, game_or_song_played:=songs_played + minigames_played]
  
  #### Create a time series data format for revenue generation in different groups
  if("paid_per_day.csv" %in% dir()) {
    paid_per_day <- fread("paid_per_day.csv")  
  } else {
    print("here")
    revenue_data <- events[,date_time:=strptime(date, "%Y-%m-%d %H:%M:%S")]
    revenue_data <- revenue_data[group %in% c('a', 'c') & uid %in% subscribed_users, .(event, uid, group, date_time)]
    revenue_data <- revenue_data[, date:=strftime(date_time, "%Y-%m-%d")]
    paid_per_day <- get_paid_period(revenue_data)
    write.table(paid_per_day, file='paid_per_day.csv', quote=FALSE, sep=",", row.names = F)
  }
  ga <- paid_per_day[group=='a' & date < "2015-04-10", .(received=sum(paid)),by=.(date)][order(date)]
  gc <- paid_per_day[group=='c' & date < "2015-04-10", .(received=sum(paid)),by=.(date)][order(date)]
  gc <- rbind(ga[1], gc)
  gc$received[1] <- 0
  
  ts_data <- xts(cbind('a'=ga$received,
                       'c'=gc$received),
                 as.Date(ga$date, "%Y-%m-%d"))
  ts_data_cumsum <- xts(cbind('a'=cumsum(ga$received),
                              'c'=cumsum(gc$received)),
                        as.Date(ga$date, "%Y-%m-%d"))
  
  ########################################## Average usage before subscription ############################################
  # Plotting average songs/games played
  output$avg_bar_plot <- renderPlotly({
    grp <- strsplit(input$select_group, split = ",")[[1]]
    
    avg_bar_plot_data <- c(minigames_played=summary_user[subscribed==TRUE & group %in% grp, mean(minigames_played)],
                           songs_played=summary_user[subscribed==TRUE & group %in% grp, mean(songs_played)],
                           game_or_song_played=summary_user[subscribed==TRUE & group %in% grp, mean(game_or_song_played)])
    
    plot_ly(
      x = c("Mini Games", "Songs", "Both"),
      y = avg_bar_plot_data,
      color = c('Mini Games', 'Songs', 'Both'),
      type = "bar"
    ) %>% layout(yaxis=list(title = '# avg',
                            range = c(0,4)))
  })
  #### text boxes ------ start
  output$avg_text_both <- renderInfoBox({
    grp <- strsplit(input$select_group, split = ",")[[1]]
    
    avg_bar_plot_data <- c(minigames_played=summary_user[subscribed==TRUE & group %in% grp, mean(minigames_played)],
                           songs_played=summary_user[subscribed==TRUE & group %in% grp, mean(songs_played)],
                           game_or_song_played=summary_user[subscribed==TRUE & group %in% grp, mean(game_or_song_played)])
    
    infoBox("Avg games/songs before subscription: ",
            format(avg_bar_plot_data['game_or_song_played'], digits=4),
            color="olive", fill=TRUE, icon("stats", lib="glyphicon"))
  })
  
  output$avg_text_games <- renderInfoBox({
    grp <- strsplit(input$select_group, split = ",")[[1]]
    
    avg_bar_plot_data <- c(minigames_played=summary_user[subscribed==TRUE & group %in% grp, mean(minigames_played)],
                           songs_played=summary_user[subscribed==TRUE & group %in% grp, mean(songs_played)],
                           game_or_song_played=summary_user[subscribed==TRUE & group %in% grp, mean(game_or_song_played)])
    
    infoBox("Avg games before subscription", 
            format(avg_bar_plot_data['minigames_played'], digits=4),
            color="olive", fill=TRUE, icon("stats", lib="glyphicon"))
  })
  
  output$avg_text_songs <- renderInfoBox({
    grp <- strsplit(input$select_group, split = ",")[[1]]
    
    avg_bar_plot_data <- c(minigames_played=summary_user[subscribed==TRUE & group %in% grp, mean(minigames_played)],
                           songs_played=summary_user[subscribed==TRUE & group %in% grp, mean(songs_played)],
                           game_or_song_played=summary_user[subscribed==TRUE & group %in% grp, mean(game_or_song_played)])
    
    infoBox("Average songs before subscription", 
            format(avg_bar_plot_data['songs_played'], digits=4),
            color="olive", fill=TRUE, icon("stats", lib = "glyphicon"))
  })
  
  ########################################## Test group B vs control group A ##########################################
  
  a_vs_b_data <- rbind(data.table(group='A',
                                       account='Account Created',
                                       category="Group A, Account Created", 
                                       count=summary_user[group=='a' & account_created==TRUE,
                                                          .N]),
                            data.table(group='A',
                                       account='No Account Created',
                                       category="Group A, No Account Created", 
                                       count=summary_user[group=='a' & account_created==FALSE,
                                                          .N]),
                            data.table(group='B',
                                       account='Account Created',
                                       category="Group B, Account Created", 
                                       count=summary_user[group=='b' & account_created==TRUE,
                                                          .N]),
                            data.table(group='B',
                                       account='No Account Created',
                                       category="Group B, No Account Created", 
                                       count=summary_user[group=='b' & account_created==FALSE,
                                                          .N]))
  ######### Pie chart 1
  output$ab_account_piechart <- renderPlotly({
    plot_ly(a_vs_b_data, labels = ~category, values = ~count, type = 'pie',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text+label+percent',
            text = ~paste(account, 
                          paste('Group' , group, count, 'users'),
                          sep = '\n')) %>%
      layout(title = 'Account creation based on group',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  a_vs_b_data_conv <- rbind( 
    data.table(group='A',
               action='played a song',
               category="Group A, Played a song after creating an account", 
               count=summary_user[group=='a' & account_created==TRUE & converted_to_play_song == TRUE,
                                  .N]),
    data.table(group='A',
               action="didn't play a song",
               category="Group A, Didn't play a song after creating an account", 
               count=summary_user[group=='a' & account_created==TRUE & converted_to_play_song == FALSE,
                                  .N]),
    data.table(group='B',
               action='played a song',
               category="Group B, Played a song after creating an account", 
               count=summary_user[group=='b' & account_created==TRUE & converted_to_play_song == TRUE,
                                  .N]),
    data.table(group='B',
               action="didn't play a song",
               category="Group B, Didn't play a song after creating an account", 
               count=summary_user[group=='b' & account_created==TRUE & converted_to_play_song == FALSE,
                                  .N]))
  ######### Pie chart 2
  output$ab_played_song_piechart <- renderPlotly({
    plot_ly(a_vs_b_data_conv, labels = ~category, values = ~count, type = 'pie',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text+label+percent',
            text = ~paste(action, 
                          paste('Group' , group, count, 'users'),
                          sep = '\n')) %>%
      layout(title = 'Playing a song after account creation',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  ######### Scatter plot 
  output$ab_errorbar_chart <- renderPlotly({
    error_bar_data <- data.frame(group=c('a', 'b'), 
                                 mean_conversion=c(mean(summary_user[group=='a', converted_to_play_song], na.rm = TRUE),
                                                   mean(summary_user[group=='b', converted_to_play_song], na.rm = TRUE)),
                                 sd=c(sd(summary_user[group=='a', converted_to_play_song], na.rm = TRUE),
                                      sd(summary_user[group=='b', converted_to_play_song], na.rm = TRUE)))
    plot_ly(data = error_bar_data, x = ~group, y = ~mean_conversion, 
            type = 'scatter') %>% layout(yaxis=list(title = 'Conversion rate',
                                                    range = c(0.95,1)))
  })
  
  treatment <- summary_user[group=='b' & account_created==TRUE, converted_to_play_song]
  control <- summary_user[group=='a' & account_created==TRUE, converted_to_play_song]
  t_test_result <- t.test(treatment, control, alternative = "greater")
  ######### T Test value boxes
  output$t_statistic <- renderValueBox({
    valueBox(format(t_test_result$statistic, digits = 4), "t statistic", width = 12,
             color="green", icon("thumbs-up", lib = "glyphicon"))
  })
  
  output$p_value <- renderValueBox({
    valueBox(format(t_test_result$p.value, digits = 4), "p value", width = 12,
             color="red", icon("thumbs-down", lib = "glyphicon"))
  })
  ######################################### Test group C vs control group A ##########################################
  
  ####### Bar plot on avg things played AC
  output$ac_avg_played <- renderPlotly({
    avg_bar_plot_data_ac <- c(group_a_game_or_song_played=summary_user[subscribed==TRUE & group=='a', mean(game_or_song_played)],
                              group_c_game_or_song_played=summary_user[subscribed==TRUE & group=='c', mean(game_or_song_played)])
    
    plot_ly(
      x = c("Group A (Control)", "Group C (Treatment)"),
      y = avg_bar_plot_data_ac,
      color = c('Group A (Control)', 'Group C (Treatment)'),
      type = "bar"
    ) %>% layout(yaxis=list(title = 'Average Minigames/Songs played before subcribing',
                            range = c(0,4)))
  })
  
  ac_subs_data <- rbind(data.table(group='A',
                                   action='subscribed',
                                   category="Group A, Subscribed", 
                                   count=summary_user[group=='a' & subscribed==TRUE,
                                                      .N]),
                        data.table(group='A',
                                   action="didn't subscribed",
                                   category="Group A, Not Subscribed", 
                                   count=summary_user[group=='a' & subscribed==FALSE,
                                                      .N]),
                        data.table(group='C',
                                   action='subscribed',
                                   category="Group C, Subscribed", 
                                   count=summary_user[group=='c' & subscribed==TRUE,
                                                      .N]),
                        data.table(group='C',
                                   action="didn't subscribed",
                                   category="Group C, Not Subscribed", 
                                   count=summary_user[group=='c' & subscribed==FALSE,
                                                      .N]))
  ####### Pie chart 1 on subscription
  output$ac_subscribed <- renderPlotly({
    plot_ly(ac_subs_data, labels = ~category, values = ~count, type = 'pie',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text+label+percent',
            text = ~paste(action, 
                          paste('Group' , group, count, 'users'),
                          sep = '\n')) %>%
      layout(title = 'Distribution of users in relation to subscribing',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  ac_unsubs_data <- rbind(data.table(group='A',
                                     action='unsubscribed',
                                     category="Group A, Unsubcribed", 
                                     count=summary_user[group=='a' & subscribed==TRUE & unsubscribed==TRUE,
                                                        .N]),
                          data.table(group='A',
                                     action="didn't unsubscribed",
                                     category="Group A, Not Unsubcribed", 
                                     count=summary_user[group=='a' & subscribed==TRUE & unsubscribed==FALSE,
                                                        .N]),
                          data.table(group='C',
                                     action='unsubscribed',
                                     category="Group C, Unsubcribed", 
                                     count=summary_user[group=='c' & subscribed==TRUE & unsubscribed==TRUE,
                                                        .N]),
                          data.table(group='C',
                                     action="didn't unsubscribed",
                                     category="Group C, Not Unsubcribed", 
                                     count=summary_user[group=='c' & subscribed==TRUE & unsubscribed==FALSE,
                                                        .N]))
  ####### Pie Chart 2 on unsubscription 
  output$ac_unsubscribed <- renderPlotly({
    plot_ly(ac_unsubs_data, labels = ~category, values = ~count, type = 'pie',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text+label+percent',
            text = ~paste(action, 
                          paste('Group' , group, count, 'users'),
                          sep = '\n')) %>%
      layout(title = 'Distribution of users unsubscribing after subscribing',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  ####### Revenue time series data creation
  revenue_data <- events[,date_time:=strptime(date, "%Y-%m-%d %H:%M:%S")]
  subscribed_users <- summary_user[subscribed==TRUE & group %in% c('a', 'c'), uid]
  revenue_data <- revenue_data[group %in% c('a', 'c') & uid %in% subscribed_users, .(event, uid, group, date_time)]
  revenue_data <- revenue_data[, date:=strftime(date_time, "%Y-%m-%d")]
  ####### Revenue time series
  print(ts_data_cumsum)
  print(str(ts_data_cumsum))
  output$revenue <- renderDygraph({
    if(input$select_ts == 1) {
      dygraph(data=ts_data_cumsum, main='Cumulative sum of money received from groups a and c') %>%
        dyRangeSelector()
    } else {
      dygraph(data=ts_data, main='Money received on each day from groups a and c') %>%
        dyRangeSelector()
    }
    
  })
  
  treatment_avg <- summary_user[group=='c' & subscribed==TRUE, game_or_song_played]
  control_avg <- summary_user[group=='a' & subscribed==TRUE, game_or_song_played]
  t_test_result_avg <- t.test(treatment_avg, control_avg)
  
  output$avg_plays_t_statistic <- renderValueBox({
    valueBox(format(t_test_result_avg$statistic, digits = 4), "t statistic", width = 12,
             color="green", icon("thumbs-up", lib = "glyphicon"))
  })
  
  output$avg_plays_p_value <- renderValueBox({
    valueBox(format(t_test_result_avg$p.value, digits = 4), "p value", width = 12,
             color="red", icon("thumbs-down", lib = "glyphicon"))
  })
  
  treatment_unsub <- summary_user[group=='c' & subscribed==TRUE, unsubscribed]
  control_unsub <- summary_user[group=='a' & subscribed==TRUE, unsubscribed]
  t_test_result_unsub <- t.test(treatment_unsub, control_unsub)
  
  output$unsub_t_statistic <- renderValueBox({
    valueBox(format(t_test_result_unsub$statistic, digits = 4), "t statistic", width = 12,
             color="red", icon("thumbs-down", lib = "glyphicon"))
  })
  
  output$unsub_p_value <- renderValueBox({
    valueBox(format(t_test_result_unsub$p.value, digits = 4), "p value", width = 12,
             color="red", icon("thumbs-down", lib = "glyphicon"))
  })
  
}
