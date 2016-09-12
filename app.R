library(shiny)

#Club names and numbers for each area.
clubNum01 <- c(784,3989,8379,3028311)
clubName01 <- c("Early Risers Club","High Nooners Club","Morgan County Club","Peak Performance")
clubNum02 <- c(375,619,5314,647252)
clubName02 <- c("Fort Collins #375 Toastmasters Club","Sunrise Toastmasters","Noonshiners Toastmasters Club","Colorado State University Toastmasters Club")
clubNum03 <- c(8533,8695,905038,2286479)
clubName03 <- c("Ayres Associates Toastmastes Club","Rise 'n Shine Toastmasters","Resource Masters","The Talking Toasters")
clubNum04 <- c(3975,4553,1112188,4800292)
clubName04 <- c("Loveland Sweet Talkers Club","Loveland Toastmasters Club","Tri-Town Toasters","Wattenberg Toastmasters Producing Excellence")
clubNum05 <- c(7818,716047,1021224,1588571)
clubName05 <- c("Seamasters Toastmasters Club","CO Xpressionists Club","Words Worth Speaking","Liberty Toastmasters-North")
clubNum06 <- c(7329,603677,631413,4677636,5297551)
clubName06 <- c("Loquacious Lunch Bunch Toastmasters Club","Sure Speakers Society Club","Speak with Ease Club","Commerce Communicators","Navigationally Speaking")
clubNum07 <- c(3557,4780,8102,698823)
clubName07 <- c("Speakeasy II Club","Boulder Speakeasy","See You Speak Toastmasters Club","Star Speakers Society")

#We read in the historical data file.
clubInfo <- read.csv("D26E.csv",header=TRUE)

clubCalc <- function(givenClubNum,givenClubInfo=rep(NA,10))
{
  #This function returns a vector of 11 numbers, indicating the probability that the club will achieve a certain number of objectives (0 - 10),
  #given the information provided (givenClubInfo), which is a vector of 10 numbers, with 0 indicating certain failure, 1 indicating certain
  #success, and "NA" indicating that success/failure is uncertain at this time.
  #The matrix x creates with zeroes and ones all possible combinations of success and failure for each club objective.
  x01 <- rep(c(rep(0,512),rep(1,512)),1)
  x02 <- rep(c(rep(0,256),rep(1,256)),2)
  x03 <- rep(c(rep(0,128),rep(1,128)),4)
  x04 <- rep(c(rep(0,64),rep(1,64)),8)
  x05 <- rep(c(rep(0,32),rep(1,32)),16)
  x06 <- rep(c(rep(0,16),rep(1,16)),32)
  x07 <- rep(c(rep(0,8),rep(1,8)),64)
  x08 <- rep(c(rep(0,4),rep(1,4)),128)
  x09 <- rep(c(0,0,1,1),256)
  x10 <- rep(c(0,1),512)
  x <- cbind(x01,x02,x03,x04,x05,x06,x07,x08,x09,x10)
  
  #Then the user input is utilized to limit the total number of possible combinations.
  for (i in 1:10) { if (!is.na(givenClubInfo[i]) & givenClubInfo[i] != "NA") {x <- x[x[,i]==givenClubInfo[i],]} }
  cInfo <- clubInfo[clubInfo$ClubNum==givenClubNum,]
  
  #Based upon prior knowledge, probabilities are calculated for the achievement of the remaining objectives.
  p <- numeric(10)
  for (i in 1:10) { p[i] <- (dim(cInfo[cInfo[,i+2]==1,])[1] + 1) / (dim(cInfo)[1] + 2) }
  
  #Then each remaining possible combination is considered, and we calculate a combined probability for each.
  #We also sum up the number of successful objectives for each possible combination.
  successNum <- numeric(dim(x)[1])
  prob <- numeric(dim(x)[1])
  temp <- numeric(10)
  for (i in 1:dim(x)[1])
  {
    successNum[i] <- sum(x[i,])
    for (j in 1:10)
    {
      temp[j] <- p[j]*x[i,j] + (1-p[j])*(1-x[i,j])
    }
    prob[i] <- prod(temp)
  }
  
  #Lastly, we group the results by number of successful objectives, and sum up the probabilities in each group.
  result <- numeric(11)
  for (i in 1:11)
  {
    result[i] <- sum(prob[successNum==i-1])/sum(prob)
  }
  print (paste(givenClubNum,"done..."))
  result
}

clubsCalc <- function(givenClubNum,givenClubInfo)
{
  #This takes a vector of club numbers in a given area as well as the full matrix of user input for the entire area.
  #clubCalc is then called for each club in the area.
  res <- clubCalc(givenClubNum[1],givenClubInfo[1,])
  for (i in 2:length(givenClubNum))
  {
    res <- rbind(res,clubCalc(givenClubNum[i],givenClubInfo[i,]))
  }
  res
}

areaCalc <- function(x)
{
  #First we calculate the probability for each club that they will achieve 5 or more objectives.
  n <- dim(x)[1]
  p <- numeric(n)
  for (i in 1:n) { p[i] <- sum(x[i,6:11]) }
  
  #The matrix y creates with zeroes and ones all possible combinations of club success and failure.
  y <- rep(c(rep(0,2^(n-1)),rep(1,2^(n-1))),1)
  for (i in 2:n)
  {
    y <- cbind(y,rep(c(rep(0,2^(n-i)),rep(1,2^(n-i))),2^(i-1)))
  }
  
  #We then calculate a combined probability for each possible combination as well as a tabulation of the number of successes.
  successNum <- numeric(dim(y)[1])
  prob <- numeric(dim(y)[1])
  temp <- numeric(n)
  for (i in 1:dim(y)[1])
  {
    for (j in 1:n)
    {
      temp[j] <- p[j]*y[i,j] + (1-p[j])*(1-y[i,j])
    }
    prob[i] <- prod(temp)
    successNum[i] <- sum(y[i,])
  }
  
  #Lastly, we group everything by the number of successes, and add up the probabilities in each group.
  result <- numeric(n+1)
  for (i in 1:(n+1))
  {
    result[i] <- sum(prob[successNum==i-1])/sum(prob)
  }
  result
}

ui <- shinyUI(fluidPage(
  titlePanel("Toastmasters Division 26 Eastern Achievement Monitoring"),
  h3("Achievement Probabilities"),
  h4("For instructions, see the bottom of this page!"),

  #Below are the panels for showing the pie charts.
  
    fluidRow(hr(),
      column(3,plotOutput("plotE01")),
      column(3,plotOutput("plotE02")),
      column(3,plotOutput("plotE03")),
      column(3,plotOutput("plotE04"))
    ),
    fluidRow(hr(),
      column(3,plotOutput("plotE05")),
      column(3,plotOutput("plotE06")),
      column(3,plotOutput("plotE07")),
      hr()
    ),
  
  #Below we now have all the panels for user input.
  
  h3("Area Accomplishment Tracking (1-Success, 0-Failure, NA-Unknown for now)"),
  tabsetPanel(type = "tabs", 
    tabPanel("Area E01",
      fluidRow(
        column(2, ""),
        column(1, h5("1", align="center")),
        column(1, h5("2", align="center")),
        column(1, h5("3", align="center")),
        column(1, h5("4", align="center")),
        column(1, h5("5", align="center")),
        column(1, h5("6", align="center")),
        column(1, h5("7", align="center")),
        column(1, h5("8", align="center")),
        column(1, h5("9", align="center")),
        column(1, h5("10", align="center"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName01[1])),
        column(1, textInput("e01_1_1","",value="NA")),
        column(1, textInput("e01_1_2","",value="NA")),
        column(1, textInput("e01_1_3","",value="NA")),
        column(1, textInput("e01_1_4","",value="NA")),
        column(1, textInput("e01_1_5","",value="NA")),
        column(1, textInput("e01_1_6","",value="NA")),
        column(1, textInput("e01_1_7","",value="NA")),
        column(1, textInput("e01_1_8","",value="NA")),
        column(1, textInput("e01_1_9","",value="NA")),
        column(1, textInput("e01_1_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName01[2])),
        column(1, textInput("e01_2_1","",value="NA")),
        column(1, textInput("e01_2_2","",value="NA")),
        column(1, textInput("e01_2_3","",value="NA")),
        column(1, textInput("e01_2_4","",value="NA")),
        column(1, textInput("e01_2_5","",value="NA")),
        column(1, textInput("e01_2_6","",value="NA")),
        column(1, textInput("e01_2_7","",value="NA")),
        column(1, textInput("e01_2_8","",value="NA")),
        column(1, textInput("e01_2_9","",value="NA")),
        column(1, textInput("e01_2_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName01[3])),
        column(1, textInput("e01_3_1","",value="NA")),
        column(1, textInput("e01_3_2","",value="NA")),
        column(1, textInput("e01_3_3","",value="NA")),
        column(1, textInput("e01_3_4","",value="NA")),
        column(1, textInput("e01_3_5","",value="NA")),
        column(1, textInput("e01_3_6","",value="NA")),
        column(1, textInput("e01_3_7","",value="NA")),
        column(1, textInput("e01_3_8","",value="NA")),
        column(1, textInput("e01_3_9","",value="NA")),
        column(1, textInput("e01_3_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName01[4])),
        column(1, textInput("e01_4_1","",value="NA")),
        column(1, textInput("e01_4_2","",value="NA")),
        column(1, textInput("e01_4_3","",value="NA")),
        column(1, textInput("e01_4_4","",value="NA")),
        column(1, textInput("e01_4_5","",value="NA")),
        column(1, textInput("e01_4_6","",value="NA")),
        column(1, textInput("e01_4_7","",value="NA")),
        column(1, textInput("e01_4_8","",value="NA")),
        column(1, textInput("e01_4_9","",value="NA")),
        column(1, textInput("e01_4_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, div(style = "height:50px",""))
      )
    ),
    tabPanel("Area E02",
      fluidRow(
        column(2, ""),
        column(1, h5("1", align="center")),
        column(1, h5("2", align="center")),
        column(1, h5("3", align="center")),
        column(1, h5("4", align="center")),
        column(1, h5("5", align="center")),
        column(1, h5("6", align="center")),
        column(1, h5("7", align="center")),
        column(1, h5("8", align="center")),
        column(1, h5("9", align="center")),
        column(1, h5("10", align="center"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName02[1])),
        column(1, textInput("e02_1_1","",value="NA")),
        column(1, textInput("e02_1_2","",value="NA")),
        column(1, textInput("e02_1_3","",value="NA")),
        column(1, textInput("e02_1_4","",value="NA")),
        column(1, textInput("e02_1_5","",value="NA")),
        column(1, textInput("e02_1_6","",value="NA")),
        column(1, textInput("e02_1_7","",value="NA")),
        column(1, textInput("e02_1_8","",value="NA")),
        column(1, textInput("e02_1_9","",value="NA")),
        column(1, textInput("e02_1_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName02[2])),
        column(1, textInput("e02_2_1","",value="NA")),
        column(1, textInput("e02_2_2","",value="NA")),
        column(1, textInput("e02_2_3","",value="NA")),
        column(1, textInput("e02_2_4","",value="NA")),
        column(1, textInput("e02_2_5","",value="NA")),
        column(1, textInput("e02_2_6","",value="NA")),
        column(1, textInput("e02_2_7","",value="NA")),
        column(1, textInput("e02_2_8","",value="NA")),
        column(1, textInput("e02_2_9","",value="NA")),
        column(1, textInput("e02_2_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName02[3])),
        column(1, textInput("e02_3_1","",value="NA")),
        column(1, textInput("e02_3_2","",value="NA")),
        column(1, textInput("e02_3_3","",value="NA")),
        column(1, textInput("e02_3_4","",value="NA")),
        column(1, textInput("e02_3_5","",value="NA")),
        column(1, textInput("e02_3_6","",value="NA")),
        column(1, textInput("e02_3_7","",value="NA")),
        column(1, textInput("e02_3_8","",value="NA")),
        column(1, textInput("e02_3_9","",value="NA")),
        column(1, textInput("e02_3_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName02[4])),
        column(1, textInput("e02_4_1","",value="NA")),
        column(1, textInput("e02_4_2","",value="NA")),
        column(1, textInput("e02_4_3","",value="NA")),
        column(1, textInput("e02_4_4","",value="NA")),
        column(1, textInput("e02_4_5","",value="NA")),
        column(1, textInput("e02_4_6","",value="NA")),
        column(1, textInput("e02_4_7","",value="NA")),
        column(1, textInput("e02_4_8","",value="NA")),
        column(1, textInput("e02_4_9","",value="NA")),
        column(1, textInput("e02_4_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, div(style = "height:50px",""))
      )
    ),
    tabPanel("Area E03",
      fluidRow(
        column(2, ""),
        column(1, h5("1", align="center")),
        column(1, h5("2", align="center")),
        column(1, h5("3", align="center")),
        column(1, h5("4", align="center")),
        column(1, h5("5", align="center")),
        column(1, h5("6", align="center")),
        column(1, h5("7", align="center")),
        column(1, h5("8", align="center")),
        column(1, h5("9", align="center")),
        column(1, h5("10", align="center"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName03[1])),
        column(1, textInput("e03_1_1","",value="NA")),
        column(1, textInput("e03_1_2","",value="NA")),
        column(1, textInput("e03_1_3","",value="NA")),
        column(1, textInput("e03_1_4","",value="NA")),
        column(1, textInput("e03_1_5","",value="NA")),
        column(1, textInput("e03_1_6","",value="NA")),
        column(1, textInput("e03_1_7","",value="NA")),
        column(1, textInput("e03_1_8","",value="NA")),
        column(1, textInput("e03_1_9","",value="NA")),
        column(1, textInput("e03_1_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName03[2])),
        column(1, textInput("e03_2_1","",value="NA")),
        column(1, textInput("e03_2_2","",value="NA")),
        column(1, textInput("e03_2_3","",value="NA")),
        column(1, textInput("e03_2_4","",value="NA")),
        column(1, textInput("e03_2_5","",value="NA")),
        column(1, textInput("e03_2_6","",value="NA")),
        column(1, textInput("e03_2_7","",value="NA")),
        column(1, textInput("e03_2_8","",value="NA")),
        column(1, textInput("e03_2_9","",value="NA")),
        column(1, textInput("e03_2_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName03[3])),
        column(1, textInput("e03_3_1","",value="NA")),
        column(1, textInput("e03_3_2","",value="NA")),
        column(1, textInput("e03_3_3","",value="NA")),
        column(1, textInput("e03_3_4","",value="NA")),
        column(1, textInput("e03_3_5","",value="NA")),
        column(1, textInput("e03_3_6","",value="NA")),
        column(1, textInput("e03_3_7","",value="NA")),
        column(1, textInput("e03_3_8","",value="NA")),
        column(1, textInput("e03_3_9","",value="NA")),
        column(1, textInput("e03_3_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName03[4])),
        column(1, textInput("e03_4_1","",value="NA")),
        column(1, textInput("e03_4_2","",value="NA")),
        column(1, textInput("e03_4_3","",value="NA")),
        column(1, textInput("e03_4_4","",value="NA")),
        column(1, textInput("e03_4_5","",value="NA")),
        column(1, textInput("e03_4_6","",value="NA")),
        column(1, textInput("e03_4_7","",value="NA")),
        column(1, textInput("e03_4_8","",value="NA")),
        column(1, textInput("e03_4_9","",value="NA")),
        column(1, textInput("e03_4_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, div(style = "height:50px",""))
      )              
    ),
    tabPanel("Area E04",
      fluidRow(
        column(2, ""),
        column(1, h5("1", align="center")),
        column(1, h5("2", align="center")),
        column(1, h5("3", align="center")),
        column(1, h5("4", align="center")),
        column(1, h5("5", align="center")),
        column(1, h5("6", align="center")),
        column(1, h5("7", align="center")),
        column(1, h5("8", align="center")),
        column(1, h5("9", align="center")),
        column(1, h5("10", align="center"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName04[1])),
        column(1, textInput("e04_1_1","",value="NA")),
        column(1, textInput("e04_1_2","",value="NA")),
        column(1, textInput("e04_1_3","",value="NA")),
        column(1, textInput("e04_1_4","",value="NA")),
        column(1, textInput("e04_1_5","",value="NA")),
        column(1, textInput("e04_1_6","",value="NA")),
        column(1, textInput("e04_1_7","",value="NA")),
        column(1, textInput("e04_1_8","",value="NA")),
        column(1, textInput("e04_1_9","",value="NA")),
        column(1, textInput("e04_1_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName04[2])),
        column(1, textInput("e04_2_1","",value="NA")),
        column(1, textInput("e04_2_2","",value="NA")),
        column(1, textInput("e04_2_3","",value="NA")),
        column(1, textInput("e04_2_4","",value="NA")),
        column(1, textInput("e04_2_5","",value="NA")),
        column(1, textInput("e04_2_6","",value="NA")),
        column(1, textInput("e04_2_7","",value="NA")),
        column(1, textInput("e04_2_8","",value="NA")),
        column(1, textInput("e04_2_9","",value="NA")),
        column(1, textInput("e04_2_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName04[3])),
        column(1, textInput("e04_3_1","",value="NA")),
        column(1, textInput("e04_3_2","",value="NA")),
        column(1, textInput("e04_3_3","",value="NA")),
        column(1, textInput("e04_3_4","",value="NA")),
        column(1, textInput("e04_3_5","",value="NA")),
        column(1, textInput("e04_3_6","",value="NA")),
        column(1, textInput("e04_3_7","",value="NA")),
        column(1, textInput("e04_3_8","",value="NA")),
        column(1, textInput("e04_3_9","",value="NA")),
        column(1, textInput("e04_3_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName04[4])),
        column(1, textInput("e04_4_1","",value="NA")),
        column(1, textInput("e04_4_2","",value="NA")),
        column(1, textInput("e04_4_3","",value="NA")),
        column(1, textInput("e04_4_4","",value="NA")),
        column(1, textInput("e04_4_5","",value="NA")),
        column(1, textInput("e04_4_6","",value="NA")),
        column(1, textInput("e04_4_7","",value="NA")),
        column(1, textInput("e04_4_8","",value="NA")),
        column(1, textInput("e04_4_9","",value="NA")),
        column(1, textInput("e04_4_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, div(style = "height:50px",""))
      )
    ),
    tabPanel("Area E05",
      fluidRow(
        column(2, ""),
        column(1, h5("1", align="center")),
        column(1, h5("2", align="center")),
        column(1, h5("3", align="center")),
        column(1, h5("4", align="center")),
        column(1, h5("5", align="center")),
        column(1, h5("6", align="center")),
        column(1, h5("7", align="center")),
        column(1, h5("8", align="center")),
        column(1, h5("9", align="center")),
        column(1, h5("10", align="center"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName05[1])),
        column(1, textInput("e05_1_1","",value="NA")),
        column(1, textInput("e05_1_2","",value="NA")),
        column(1, textInput("e05_1_3","",value="NA")),
        column(1, textInput("e05_1_4","",value="NA")),
        column(1, textInput("e05_1_5","",value="NA")),
        column(1, textInput("e05_1_6","",value="NA")),
        column(1, textInput("e05_1_7","",value="NA")),
        column(1, textInput("e05_1_8","",value="NA")),
        column(1, textInput("e05_1_9","",value="NA")),
        column(1, textInput("e05_1_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName05[2])),
        column(1, textInput("e05_2_1","",value="NA")),
        column(1, textInput("e05_2_2","",value="NA")),
        column(1, textInput("e05_2_3","",value="NA")),
        column(1, textInput("e05_2_4","",value="NA")),
        column(1, textInput("e05_2_5","",value="NA")),
        column(1, textInput("e05_2_6","",value="NA")),
        column(1, textInput("e05_2_7","",value="NA")),
        column(1, textInput("e05_2_8","",value="NA")),
        column(1, textInput("e05_2_9","",value="NA")),
        column(1, textInput("e05_2_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName05[3])),
        column(1, textInput("e05_3_1","",value="NA")),
        column(1, textInput("e05_3_2","",value="NA")),
        column(1, textInput("e05_3_3","",value="NA")),
        column(1, textInput("e05_3_4","",value="NA")),
        column(1, textInput("e05_3_5","",value="NA")),
        column(1, textInput("e05_3_6","",value="NA")),
        column(1, textInput("e05_3_7","",value="NA")),
        column(1, textInput("e05_3_8","",value="NA")),
        column(1, textInput("e05_3_9","",value="NA")),
        column(1, textInput("e05_3_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName05[4])),
        column(1, textInput("e05_4_1","",value="NA")),
        column(1, textInput("e05_4_2","",value="NA")),
        column(1, textInput("e05_4_3","",value="NA")),
        column(1, textInput("e05_4_4","",value="NA")),
        column(1, textInput("e05_4_5","",value="NA")),
        column(1, textInput("e05_4_6","",value="NA")),
        column(1, textInput("e05_4_7","",value="NA")),
        column(1, textInput("e05_4_8","",value="NA")),
        column(1, textInput("e05_4_9","",value="NA")),
        column(1, textInput("e05_4_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, div(style = "height:50px",""))
      )
    ),
    tabPanel("Area E06",
      fluidRow(
        column(2, ""),
        column(1, h5("1", align="center")),
        column(1, h5("2", align="center")),
        column(1, h5("3", align="center")),
        column(1, h5("4", align="center")),
        column(1, h5("5", align="center")),
        column(1, h5("6", align="center")),
        column(1, h5("7", align="center")),
        column(1, h5("8", align="center")),
        column(1, h5("9", align="center")),
        column(1, h5("10", align="center"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName06[1])),
        column(1, textInput("e06_1_1","",value="NA")),
        column(1, textInput("e06_1_2","",value="NA")),
        column(1, textInput("e06_1_3","",value="NA")),
        column(1, textInput("e06_1_4","",value="NA")),
        column(1, textInput("e06_1_5","",value="NA")),
        column(1, textInput("e06_1_6","",value="NA")),
        column(1, textInput("e06_1_7","",value="NA")),
        column(1, textInput("e06_1_8","",value="NA")),
        column(1, textInput("e06_1_9","",value="NA")),
        column(1, textInput("e06_1_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName06[2])),
        column(1, textInput("e06_2_1","",value="NA")),
        column(1, textInput("e06_2_2","",value="NA")),
        column(1, textInput("e06_2_3","",value="NA")),
        column(1, textInput("e06_2_4","",value="NA")),
        column(1, textInput("e06_2_5","",value="NA")),
        column(1, textInput("e06_2_6","",value="NA")),
        column(1, textInput("e06_2_7","",value="NA")),
        column(1, textInput("e06_2_8","",value="NA")),
        column(1, textInput("e06_2_9","",value="NA")),
        column(1, textInput("e06_2_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName06[3])),
        column(1, textInput("e06_3_1","",value="NA")),
        column(1, textInput("e06_3_2","",value="NA")),
        column(1, textInput("e06_3_3","",value="NA")),
        column(1, textInput("e06_3_4","",value="NA")),
        column(1, textInput("e06_3_5","",value="NA")),
        column(1, textInput("e06_3_6","",value="NA")),
        column(1, textInput("e06_3_7","",value="NA")),
        column(1, textInput("e06_3_8","",value="NA")),
        column(1, textInput("e06_3_9","",value="NA")),
        column(1, textInput("e06_3_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName06[4])),
        column(1, textInput("e06_4_1","",value="NA")),
        column(1, textInput("e06_4_2","",value="NA")),
        column(1, textInput("e06_4_3","",value="NA")),
        column(1, textInput("e06_4_4","",value="NA")),
        column(1, textInput("e06_4_5","",value="NA")),
        column(1, textInput("e06_4_6","",value="NA")),
        column(1, textInput("e06_4_7","",value="NA")),
        column(1, textInput("e06_4_8","",value="NA")),
        column(1, textInput("e06_4_9","",value="NA")),
        column(1, textInput("e06_4_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName06[5])),
        column(1, textInput("e06_5_1","",value="NA")),
        column(1, textInput("e06_5_2","",value="NA")),
        column(1, textInput("e06_5_3","",value="NA")),
        column(1, textInput("e06_5_4","",value="NA")),
        column(1, textInput("e06_5_5","",value="NA")),
        column(1, textInput("e06_5_6","",value="NA")),
        column(1, textInput("e06_5_7","",value="NA")),
        column(1, textInput("e06_5_8","",value="NA")),
        column(1, textInput("e06_5_9","",value="NA")),
        column(1, textInput("e06_5_10","",value="NA"))
      )
    ),
    tabPanel("Area E07",
      fluidRow(
        column(2, ""),
        column(1, h5("1", align="center")),
        column(1, h5("2", align="center")),
        column(1, h5("3", align="center")),
        column(1, h5("4", align="center")),
        column(1, h5("5", align="center")),
        column(1, h5("6", align="center")),
        column(1, h5("7", align="center")),
        column(1, h5("8", align="center")),
        column(1, h5("9", align="center")),
        column(1, h5("10", align="center"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName07[1])),
        column(1, textInput("e07_1_1","",value="NA")),
        column(1, textInput("e07_1_2","",value="NA")),
        column(1, textInput("e07_1_3","",value="NA")),
        column(1, textInput("e07_1_4","",value="NA")),
        column(1, textInput("e07_1_5","",value="NA")),
        column(1, textInput("e07_1_6","",value="NA")),
        column(1, textInput("e07_1_7","",value="NA")),
        column(1, textInput("e07_1_8","",value="NA")),
        column(1, textInput("e07_1_9","",value="NA")),
        column(1, textInput("e07_1_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName07[2])),
        column(1, textInput("e07_2_1","",value="NA")),
        column(1, textInput("e07_2_2","",value="NA")),
        column(1, textInput("e07_2_3","",value="NA")),
        column(1, textInput("e07_2_4","",value="NA")),
        column(1, textInput("e07_2_5","",value="NA")),
        column(1, textInput("e07_2_6","",value="NA")),
        column(1, textInput("e07_2_7","",value="NA")),
        column(1, textInput("e07_2_8","",value="NA")),
        column(1, textInput("e07_2_9","",value="NA")),
        column(1, textInput("e07_2_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName07[3])),
        column(1, textInput("e07_3_1","",value="NA")),
        column(1, textInput("e07_3_2","",value="NA")),
        column(1, textInput("e07_3_3","",value="NA")),
        column(1, textInput("e07_3_4","",value="NA")),
        column(1, textInput("e07_3_5","",value="NA")),
        column(1, textInput("e07_3_6","",value="NA")),
        column(1, textInput("e07_3_7","",value="NA")),
        column(1, textInput("e07_3_8","",value="NA")),
        column(1, textInput("e07_3_9","",value="NA")),
        column(1, textInput("e07_3_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, h5(clubName07[4])),
        column(1, textInput("e07_4_1","",value="NA")),
        column(1, textInput("e07_4_2","",value="NA")),
        column(1, textInput("e07_4_3","",value="NA")),
        column(1, textInput("e07_4_4","",value="NA")),
        column(1, textInput("e07_4_5","",value="NA")),
        column(1, textInput("e07_4_6","",value="NA")),
        column(1, textInput("e07_4_7","",value="NA")),
        column(1, textInput("e07_4_8","",value="NA")),
        column(1, textInput("e07_4_9","",value="NA")),
        column(1, textInput("e07_4_10","",value="NA"))
      ),
      fluidRow(hr(),
        column(2, div(style = "height:50px",""))
      )
    )
  ),
  hr(),
  p("Created by John Withrow, September 2016"),
  p("I am what is known as a Division Director in the organization known as Toastmasters.  It is my job to supervise the success of 29 clubs
    that are geographically grouped into seven areas.  Each club has a total of ten objectives to complete in each year.  If the club achieves
    five or more of any of those objectives, then the club is given a status of Distinguished.  If an area has 50% or more of its clubs reaching
    the Distinguised level, then the area is considered Distinguished, so my seven Area Directors would like know what the chances are of each
    of their areas becoming Distinguished.  Using data for all clubs dating back from 2009 to 2015, probabilities of each club achieving each
    objective are calculated based upon past history.  Compiling all those probabilities together, we can calculate probabilities that each
    area will have a total of zero, 1, 2, 3, or 4 or more clubs that become Distinguished.  These are the pie charts, one for each area, that
    you see at the top of each page!"),
  p("Of course, as the year progresses, and our clubs begin to mark their successes, then these probabilities can change!  That's where the
    lower part of the page comes in!  Note that there are seven tabs, one for each area.  In each area, we see rows of 10 boxes, one row for
    each club.  You can see the club name to the left.  In each box, NA means that it is currently not known whether or not the club will
    achieve this objective this year.  A value of 1 indicates that the club has already achieved this.  A value of zero indicates certain
    failure for this objective."),
  p("Here is where you get to play!  As an example, choose the tab for Area E05, and make sure that you can see the pie chart labeled
    Probabilities for Area E05 above.  Then below locate the row of boxes for the club named CO Xpressionists, and change the NA values
    to 1's for some of the boxes.  Notice in the pie chart above how insuring these successes noticeably increases the probability of overall
    success for Area E05!")
  )
)

callPieChart <- function(x,mainTitle){
  #This function creates the pie charts.  It is called by the renderPlot functions below.
  n <- length(x)
  slices <- x
  lbls <- c("0","1","2","3","4","5")[1:n]
  cols <- c("red","orange","yellow","green","blue","violet")[1:n]
  pie(slices, labels = lbls, col=cols, radius=1, clockwise = FALSE, init.angle=90, main=mainTitle)
  text(0,-1.5,"Num. of Dist. Clubs")
  text(0,-1.75,paste("(out of ",n-1,")",sep=""))
}

server <- shinyServer(function(input, output) {
  
  #This portion of the code controls the initial creation of the pie charts, as well as the subsequent updating of the pie charts
  #in response to user input.
  
  #Area E01
  e01 <- reactive({ rbind(c(input$e01_1_1,input$e01_1_2,input$e01_1_3,input$e01_1_4,input$e01_1_5,input$e01_1_6,input$e01_1_7,input$e01_1_8,input$e01_1_9,input$e01_1_10),
                          c(input$e01_2_1,input$e01_2_2,input$e01_2_3,input$e01_2_4,input$e01_2_5,input$e01_2_6,input$e01_2_7,input$e01_2_8,input$e01_2_9,input$e01_2_10),
                          c(input$e01_3_1,input$e01_3_2,input$e01_3_3,input$e01_3_4,input$e01_3_5,input$e01_3_6,input$e01_3_7,input$e01_3_8,input$e01_3_9,input$e01_3_10),
                          c(input$e01_4_1,input$e01_4_2,input$e01_4_3,input$e01_4_4,input$e01_4_5,input$e01_4_6,input$e01_4_7,input$e01_4_8,input$e01_4_9,input$e01_4_10)) })
  output$plotE01 <- renderPlot({ callPieChart(areaCalc(clubsCalc(clubNum01,e01())),"Probabilities for Area E01") })
  
  #Area E02
  e02 <- reactive({ rbind(c(input$e02_1_1,input$e02_1_2,input$e02_1_3,input$e02_1_4,input$e02_1_5,input$e02_1_6,input$e02_1_7,input$e02_1_8,input$e02_1_9,input$e02_1_10),
                          c(input$e02_2_1,input$e02_2_2,input$e02_2_3,input$e02_2_4,input$e02_2_5,input$e02_2_6,input$e02_2_7,input$e02_2_8,input$e02_2_9,input$e02_2_10),
                          c(input$e02_3_1,input$e02_3_2,input$e02_3_3,input$e02_3_4,input$e02_3_5,input$e02_3_6,input$e02_3_7,input$e02_3_8,input$e02_3_9,input$e02_3_10),
                          c(input$e02_4_1,input$e02_4_2,input$e02_4_3,input$e02_4_4,input$e02_4_5,input$e02_4_6,input$e02_4_7,input$e02_4_8,input$e02_4_9,input$e02_4_10)) })
  output$plotE02 <- renderPlot({ callPieChart(areaCalc(clubsCalc(clubNum02,e02())),"Probabilities for Area E02") })

  #Area E03
  e03 <- reactive({ rbind(c(input$e03_1_1,input$e03_1_2,input$e03_1_3,input$e03_1_4,input$e03_1_5,input$e03_1_6,input$e03_1_7,input$e03_1_8,input$e03_1_9,input$e03_1_10),
                          c(input$e03_2_1,input$e03_2_2,input$e03_2_3,input$e03_2_4,input$e03_2_5,input$e03_2_6,input$e03_2_7,input$e03_2_8,input$e03_2_9,input$e03_2_10),
                          c(input$e03_3_1,input$e03_3_2,input$e03_3_3,input$e03_3_4,input$e03_3_5,input$e03_3_6,input$e03_3_7,input$e03_3_8,input$e03_3_9,input$e03_3_10),
                          c(input$e03_4_1,input$e03_4_2,input$e03_4_3,input$e03_4_4,input$e03_4_5,input$e03_4_6,input$e03_4_7,input$e03_4_8,input$e03_4_9,input$e03_4_10)) })
  output$plotE03 <- renderPlot({ callPieChart(areaCalc(clubsCalc(clubNum03,e03())),"Probabilities for Area E03") })
  
  #Area E04
  e04 <- reactive({ rbind(c(input$e04_1_1,input$e04_1_2,input$e04_1_3,input$e04_1_4,input$e04_1_5,input$e04_1_6,input$e04_1_7,input$e04_1_8,input$e04_1_9,input$e04_1_10),
                          c(input$e04_2_1,input$e04_2_2,input$e04_2_3,input$e04_2_4,input$e04_2_5,input$e04_2_6,input$e04_2_7,input$e04_2_8,input$e04_2_9,input$e04_2_10),
                          c(input$e04_3_1,input$e04_3_2,input$e04_3_3,input$e04_3_4,input$e04_3_5,input$e04_3_6,input$e04_3_7,input$e04_3_8,input$e04_3_9,input$e04_3_10),
                          c(input$e04_4_1,input$e04_4_2,input$e04_4_3,input$e04_4_4,input$e04_4_5,input$e04_4_6,input$e04_4_7,input$e04_4_8,input$e04_4_9,input$e04_4_10)) })
  output$plotE04 <- renderPlot({ callPieChart(areaCalc(clubsCalc(clubNum04,e04())),"Probabilities for Area E04") })
  
  #Area E05
  e05 <- reactive({ rbind(c(input$e05_1_1,input$e05_1_2,input$e05_1_3,input$e05_1_4,input$e05_1_5,input$e05_1_6,input$e05_1_7,input$e05_1_8,input$e05_1_9,input$e05_1_10),
                          c(input$e05_2_1,input$e05_2_2,input$e05_2_3,input$e05_2_4,input$e05_2_5,input$e05_2_6,input$e05_2_7,input$e05_2_8,input$e05_2_9,input$e05_2_10),
                          c(input$e05_3_1,input$e05_3_2,input$e05_3_3,input$e05_3_4,input$e05_3_5,input$e05_3_6,input$e05_3_7,input$e05_3_8,input$e05_3_9,input$e05_3_10),
                          c(input$e05_4_1,input$e05_4_2,input$e05_4_3,input$e05_4_4,input$e05_4_5,input$e05_4_6,input$e05_4_7,input$e05_4_8,input$e05_4_9,input$e05_4_10)) })
  output$plotE05 <- renderPlot({ callPieChart(areaCalc(clubsCalc(clubNum05,e05())),"Probabilities for Area E05") })
  
  #Area E06
  e06 <- reactive({ rbind(c(input$e06_1_1,input$e06_1_2,input$e06_1_3,input$e06_1_4,input$e06_1_5,input$e06_1_6,input$e06_1_7,input$e06_1_8,input$e06_1_9,input$e06_1_10),
                          c(input$e06_2_1,input$e06_2_2,input$e06_2_3,input$e06_2_4,input$e06_2_5,input$e06_2_6,input$e06_2_7,input$e06_2_8,input$e06_2_9,input$e06_2_10),
                          c(input$e06_3_1,input$e06_3_2,input$e06_3_3,input$e06_3_4,input$e06_3_5,input$e06_3_6,input$e06_3_7,input$e06_3_8,input$e06_3_9,input$e06_3_10),
                          c(input$e06_3_1,input$e06_4_2,input$e06_4_3,input$e06_4_4,input$e06_4_5,input$e06_4_6,input$e06_4_7,input$e06_4_8,input$e06_4_9,input$e06_4_10),
                          c(input$e06_4_1,input$e06_5_2,input$e06_5_3,input$e06_5_4,input$e06_5_5,input$e06_5_6,input$e06_5_7,input$e06_5_8,input$e06_5_9,input$e06_5_10)) })
  output$plotE06 <- renderPlot({ callPieChart(areaCalc(clubsCalc(clubNum06,e06())),"Probabilities for Area E06") })
  
  #Area E07
  e07 <- reactive({ rbind(c(input$e07_1_1,input$e07_1_2,input$e07_1_3,input$e07_1_4,input$e07_1_5,input$e07_1_6,input$e07_1_7,input$e07_1_8,input$e07_1_9,input$e07_1_10),
                          c(input$e07_2_1,input$e07_2_2,input$e07_2_3,input$e07_2_4,input$e07_2_5,input$e07_2_6,input$e07_2_7,input$e07_2_8,input$e07_2_9,input$e07_2_10),
                          c(input$e07_3_1,input$e07_3_2,input$e07_3_3,input$e07_3_4,input$e07_3_5,input$e07_3_6,input$e07_3_7,input$e07_3_8,input$e07_3_9,input$e07_3_10),
                          c(input$e07_4_1,input$e07_4_2,input$e07_4_3,input$e07_4_4,input$e07_4_5,input$e07_4_6,input$e07_4_7,input$e07_4_8,input$e07_4_9,input$e07_4_10)) })
  output$plotE07 <- renderPlot({ callPieChart(areaCalc(clubsCalc(clubNum07,e07())),"Probabilities for Area E07") })

})

shinyApp(ui = ui, server = server)

