# Test/Control

This repo contains an online dashboard example demonstrating the usage of R Shiny and A/B testing on a dummy dataset. It's available online [Here](https://farbod.shinyapps.io/test/).

## Dataset description

Dummy dataset ```events.csv``` included in this repo imagines an application where users can istall it and sign up for the service to either play games or songs. Users can subscribe as well buy paying 5 euros a week and the subscription is renewed automatically unless user unsubscribes. The dataset contains the captured user events as they used the application.

Imagine the company wants to roll out new features, but it is necessary to see if the changes would lead into more profits. In the dataset you can find three groups. Group A which is the control group and test groups B and C where each one introduces a new feature.

Group B has modified manuals for users to provide more comprehensive guidance in games. Group C introduces a new subscription model where users can pay 80 cents a day instead of paying 5 euros a week.

## Application

The Shiny app provides the following information: It provides summary statistics regarding the average number of songs and games played before a user subscribes. Checks whether the conversion rate is better between account being created and subscribtions in group B. And checks whether group C leads in more profit for the company.

## How to run

In order to run this application yourself you need to install R and the required packages as listed by ```required_packages.txt```. Then you can run the shiny app by launching R in the terminal in the directory where the scripts are located and run the following:

```
library(shiny)
runapp(".")
```