library(shiny)
library(ggplot2)
library(dygraphs)
library(plotly)
library(shinydashboard)

dashboard_header <- dashboardHeader(title="Testing")

dashbaord_sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Average use before subscribing", icon = icon("bar-chart"), tabName = "average_usage"),
    menuItem("Test group B vs control group A", icon = icon("exchange"), tabName = "a_vs_b"),
    menuItem("Test group C vs control group A", icon = icon("money"), tabName="a_vs_c")
  )
)

dashboard_body <- dashboardBody(
  ################## Average usage before subscription ####################
  tabItems(
    tabItem(tabName = "average_usage",
            fluidPage(
              fluidRow(box(width = 12,
                           # Tab title
                           titlePanel("Average number of song/minigames before subscribing"),
                           column(3,
                                  selectInput("select_group", label = h3("Select Group"), 
                                              choices = list("All" = 'a,b,c', "Group A" = 'a', "Group B" = 'b', "Group C" = 'c'), 
                                              selected = 'a,b,c'),
                                  h4("Here we have plotted using bar charts the average number of songs, minigames, and both (songs/minigames) users play before subscribing."),
                                  h4("In addition you can check the stat boxes showing the average number underneath."),
                                  h4("You can also select a certain group from the drop down menu and see the average numbers for that specific group reflected in the chart and status boxes.")),
                           column(9,
                                  plotlyOutput("avg_bar_plot")))),
              fluidRow(infoBoxOutput("avg_text_both"),
                       infoBoxOutput("avg_text_games"),
                       infoBoxOutput("avg_text_songs")
              )
            )
    ),
    ################## Test group B vs control group A ####################
    tabItem(tabName = "a_vs_b",
            fluidPage(
              h3("Here we check if group B has a better conversion rate between account created and the first song played than the control group"),
              fluidRow(box(width = 12,
                           column(width = 4,
                                  plotlyOutput("ab_account_piechart"),
                                  p()),
                           column(width = 4,
                                  plotlyOutput("ab_played_song_piechart"),
                                  p()),
                           column(width = 4,
                                  plotlyOutput("ab_errorbar_chart"),
                                  p())
              )),
              fluidRow(
                box(
                  width = 12,
                  h4(
                    "First we explore the data a bit. By looking at the first pie chart we can see that there are 333 users in each of these two groups. And in each of these groups 332 users have created an account. This means that there is one user in each group that hasn't created an accout which probably means that it's a returning user and had an account previously. We look only into those that have created an account during this trial (664 users)."
                  ),
                  h4(
                    "By looking at the next pie chart we can see only 2 users in group B didn't play a song after creating an account while 6 people in group A didn't play a song after creating an account. This seems promising, suggesting that the voice overs in group B are motivating more people to play a song after creating an account. But we have to investigate the matter further."
                  ),
                  h4(
                    "Finally, by looking at the scatter plot, group A has a conversion rate of 98.2% and group b has a conversion rate of 99.4% that suggests that the change in group B is working, but in the next section we will use t-tests to see whether this finding is reliable."
                  )
                )
              ),
              fluidRow(
                box(
                  width=12,
                  column(4,
                         valueBoxOutput("t_statistic", width=12),
                         valueBoxOutput("p_value", width=12)),
                  column(8,
                         h4(
                           "In order to know if a change resulting from an A/B test is actually a real result of what we changed, or if it's just random variation, we use a t-test here. Let's start with the t-statistic, which is a measure of the difference in behavior between these two sets, between our control and treatment group, expressed in units of standard error."
                         ),
                         h4(
                           "Here we got 1.423 for t-statistic a positive value, suggesting that the variation in group B has lead in higher conversion rate between account created and the first song played."
                         ),
                         h4(
                           "To check whether we can rely on this finding we use p-values. In other words we want to see if the result is statistically significant. By computing the p-value, the probability that there is no real difference between the control and the treatment's behavior, we get 0.07769 which is a bit beyond an acceptable threshold (usually 0.05 or 0.01). This means that the behaviour change observed in group B may not be due to the change (voice-overs). Although, more data may be needed to fully conclude this hypothesis since the p-value is close to an acceptable threshold."
                         ),
                         h4(
                           "Therefore, based on the current data we cannot say the change in group B had an effect on higher conversion rate between account creation and playing a song."
                         )
                  )
                )
              ))),
    ################## Test group C vs control group A ####################
    tabItem(tabName = "a_vs_c",
            fluidPage(
              h3("In the following page, we explore whether the group C experiment is generating more money or not."),
              p("Don't forget to scroll down to check all the analysis!"),
              fluidRow(box(width=12,
                           column(4,
                                  plotlyOutput("ac_avg_played")),
                           column(4,
                                  plotlyOutput("ac_subscribed")),
                           column(4,
                                  plotlyOutput("ac_unsubscribed"),
                                  br()))),
              fluidRow(box(width=12,
                           h4(
                             "Above charts looks into a few of characteristic differences between two groups A and C. By looking at the average number of minigames/songs played before subscribing in these two groups we can see that in group C that users tend to subscribe earlier by playing fewer games or songs. This is our first hypothesis that users may feel there's less risk for subscribing for only a day by paying 80 cents. We check whether this hypothesis is correct and reliable later in this page."
                           ),
                           h4(
                             "If users in group C feel a lower risk due to the daily payment to subscribe earlier compared to group A users, does it mean they are more likely to subscribe compared to group A?"
                           ),
                           h4(
                             "We can look at the middle pie chart, based on the user subscription in two groups we can see that there's not much difference. 201 users in Group A out of 333 subscribed, and 199 users in group C out 333 have subscribed. We can conclude the daily subscription does not make users more willing to subscribe."
                           ),
                           h4(
                             "Another issue to consider is whether users in group C tend to unsubscribe earlier compared to users in Group A which can lead in loss for the company."
                           ),
                           h4(
                             "Again, there's not much difference between these two groups. Out of 199 users in group C that have subscribed 76 users unsubscribed later, and out of 201 users subscribed in group A 75 have unsubscribed later on. Even though there's not much difference here since users in group C pay on a daily basis even an equal unsubcription rate may lead into loss. Therefore, to further explore this matter we look into the money generated by each of these two groups in the next section."
                           )
              )),
              fluidRow(box(width=12,
                           column(3,
                                  selectInput("select_ts", label = h4("Payment received each day or cumulative sum over trial period"), 
                                              choices = list("Cumulative Sum" = 1, "Each Day" = 2), 
                                              selected = '1')),
                           column(9,
                                  dygraphOutput("revenue")))),
              fluidRow(
                box(
                  width = 12,
                  h4(
                    "One of the findings in the last section was about the money generated for the company within groups A, C. While group A pays a weekly fee of 5 euros, group C pays a daily fee of 80 cents."
                  ),
                  h4(
                    "By looking at the cumulative sum of money company could get from groups A and C we can see they both grow steadily with almost the same rate. Even though group C lagged a bit behind but the money company receives from them grows with the same rate as group A and probably will match up in long term. Therefore, we don't have to worry about group C users being able to unsubscribe earlier and pay less compared to group A."
                  ),
                  h4(
                    "For the last step, we check if our two hypothesises (the lower average number of minigames/songs played before subscribing and more likeliness of group C users unsubscribe) is statistically significant, meaning if we can rely on their correctness."
                  )
                )
              ),
              fluidRow(box(width=12,
                           column(2,
                                  fluidRow(
                                    valueBoxOutput("avg_plays_t_statistic", width=12)
                                  ),
                                  fluidRow(
                                    valueBoxOutput("avg_plays_p_value", width=12)
                                  )
                           ),
                           column(4,
                                  h4(
                                    "In order to know if a change resulting from an A/B test is actually a real result of what we changed, or if it's just random variation, we use a t-test here. Let's start with the t-statistic, which is a measure of the difference in behavior between these two sets, between our control and treatment group, expressed in units of standard error."
                                  ),
                                  h4(
                                    "Here we got -1.05 for t-statistic which is a good thing, suggesting that the variation in group C has led in less minigames/songs played before a user subscribes."
                                  ),
                                  h4(
                                    "Unfortunately, by computing the p-value, the probability that there is no real difference between the control and the treatment's behavior, we get 0.29 which is beyond an acceptable threshold (usually 0.05 or 0.01). This means that the behaviour change observed in group C is not due to the experiment."),
                                  h4(
                                    "Therefore, we cannot say the change in group C has led to fewer games/songs played before subscribing."
                                  )
                           ),
                           column(2,
                                  fluidRow(
                                    valueBoxOutput("unsub_t_statistic",width = 12)
                                  ),
                                  fluidRow(
                                    valueBoxOutput("unsub_p_value", width=12)
                                  )
                           ),
                           column(4,
                                  h4(
                                    "Again, to see if the change resulting from an A/B test is actually a real result of what we changed, we use a t-test here."
                                  ),
                                  h4(
                                    "Here we got 0.1806 for t-statistic. This is a positive value, suggesting that the variation in group C has led in more users unsubscribing."
                                  ),
                                  h4(
                                    "To see if this is a reliable finding we get the p-value again. We get 0.8568 which is a very high probability that rejects our hypothesis. This means that the behaviour change observed in group C is not due to the experiment and may have happened randomly."),
                                  h4(
                                    "Therefore, the change in group C didn't have any effect on unsubscription rate."
                                  )
                           )
              )
              )
            )
    )
  )
)

dashboardPage(dashboard_header, dashbaord_sidebar, dashboard_body, skin='green')
