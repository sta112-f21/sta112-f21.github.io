---
layout: page
title: Extra Credit 1: Web scraping
nav_exclude: true
---

# Extra Credit 1

**Due:** by Friday, November 5 at 1:00pm on Canvas

**Learning goals:** If you complete this extra credit assignment, you will be able to: 
* scrape and download data from the web
* learn a new R package and computing skill on your own

## Instructions

On the first day of class, we looked at [this](https://fivethirtyeight.com/features/after-this-weird-nba-season-we-have-a-better-idea-of-how-much-fans-matter/) article about the impact of fans on NBA games. One thing we noticed is that the data used in the article is not readily available. For this extra credit assignment, you will scrape this data from [Basketball-Reference.com](https://www.basketball-reference.com/).

*Web scraping* means automatically extracting data from web pages. This is done by writing code to visit a website and download the data (rather than manually entering copying the data, or clicking download buttons by hand). Web scraping is particularly useful when you want to download data that is spread across many web pages, and visiting each of them by hand is difficult. 

For this extra credit assignment, you will learn some basic web scraping. The assignment is deliberately open ended, to give you practice with independently learning new computing skills. It will be graded as credit/no credit; to receive credit, you must turn in two files:
1. A CSV file where each row represents one game from the 2020-21 NBA regular season, and the following columns:
    * home-court advantage: number of points scored by home team - number of points scored by away team
    * game attendance: the number of fans at the game
    * date: the date of the game
2. An Rmarkdown file with your complete code for creating the CSV file.

If you receive credit, this extra credit assignment can replace a lab of your choice.

## Tips
* Tables of regular season data for each month can be found [here](https://www.basketball-reference.com/leagues/NBA_2021_games.html?sr&utm_source=direct&utm_medium=Share&utm_campaign=ShareTool#schedule) on Basketball-Reference.com. This is a good starting point.
* The R package `rvest` may be helpful. See [this](https://rvest.tidyverse.org/) link for examples and more information.

## Collaboration
You may:
* Use any resources on the internet, provided you cite them in your Rmarkdown file
* Collaborate with other students in the class, provided you acknowledge them in your Rmarkdown file, and you each write up and submit your own work
* Ask me questions. I am happy to answer any questions about what you need to do to receive credit, but I won't help much with doing the web scraping itself.

You may not:
* Get help from a TA or tutor
* Copy someone else's work and submit it as your own
* Use resources without citation