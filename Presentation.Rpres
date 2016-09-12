Using Shiny to Update Success Probabilities in Bayesian Fashion
========================================================
author: Dr. John R. Withrow, Jr.
date: September 2016
autosize: true

The Problem
========================================================

- I am what is known as a Division Director in the organization known as Toastmasters.
- It is my job to supervise the success of 29 clubs that are geographically grouped into seven areas.  
- Each club has a total of ten objectives to complete in each year.  
- If the club achieves five or more of any of those objectives, then the club is given a status of Distinguished.  
- If an area has 50% or more of its clubs reaching the Distinguised level, then the area is considered Distinguished

Hence, my seven Area Directors would like know what the chances are of each of their areas becoming Distinguished.  

The Approach - Historical Data
========================================================

- Using data for all clubs dating from 2009 to 2015, probabilities of each club achieving each objective are calculated based upon past history.  
- Compiling all those probabilities together, we can calculate probabilities that each area will have a total of zero, 1, 2, 3, or 4 or more clubs that become Distinguished.  These are shown in pie charts, one for each area.

- Of course, as the year progresses, and our clubs begin to mark their successes, then these probabilities can change!  That's where the
    lower part of the page comes in!  Note that there are seven tabs, one for each area.  For each club, we see rows of 10 boxes.  In each box, values of 1 and 0 indicate certain success or failure. NA indicates unknown.

The Approach - Updating Success
========================================================

- Here is where one gets to experiment.  Is there a particular objective that, if we focus special attention on completing, then the chances of overall success are increased substantially?
- As an example, choose the tab for Area E05, and make sure that you can see the pie chart labeled Probabilities for Area E05 above.  Then below locate the row of boxes for the club named CO Xpressionists, and change the NA values
    to 1's for some of the boxes.  Notice in the pie chart above how insuring these successes noticeably increases the probability of overall
    success for Area E05!

Example Code
========================================================
This project makes use of several reactive variables that concatenate user inputs into matrices to change each plot.  The one below is for Area E04.

```
e04 <- reactive({ rbind(c(input$e04_1_1,input$e04_1_2,input$e04_1_3,
...
input$e04_4_9,input$e04_4_10)) })

output$plotE04 <- renderPlot({ callPieChart(areaCalc(clubsCalc(clubNum04,e04())),
"Probabilities for Area E04") })
```


