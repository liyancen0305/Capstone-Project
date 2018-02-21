#####################################################################  Project Proposal ########################################################
#1. What is the problem you want to solve?
  # An investor is interested in investment property in both Austin, Texas and Los Angeles County. With respect to the data collected for a Metropolitan Area, Round Rock and Los Angeles County, over the years, we present the client the comparison between two cities regarding the investing purpose. 
#2. Who is your client and why do they care about this problem? In other words, what will your client DO or DECIDE based on your analysis that they wouldn’t have otherwise?
  # My client is interested in invest housing in Los Angeles, and Round Rock, Texas. I will analyze data on the “house price index”, and “unemployment” and give a forecast for both cities.
#3 What data are you going to use for this? How will you acquire this data?
  # Austin-Round Rock
    # House Price Index: https://fred.stlouisfed.org/series/ATNHPIUS12420Q
    # Unemployment Rate: https://fred.stlouisfed.org/series/AUST448URN?utm_source=series_page&utm_medium=related_content&utm_term=related_resources&utm_campaign=categories
    # Demographics: https://en.wikipedia.org/wiki/Round_Rock,_Texas
  # Los Angeles County 
    # Unemployment rate: https://fred.stlouisfed.org/series/CALOSA7URN?utm_source=series_page&utm_medium=related_content&utm_term=related_resources&utm_campaign=categories
    # House price index: https://fred.stlouisfed.org/series/ATNHPIUS06037A
    # Demographics: https://en.wikipedia.org/wiki/Los_Angeles_County,_California
  # House Price Index: https://fred.stlouisfed.org/series/ATNHPIUS12420Q
  # Unemployment Rate: https://fred.stlouisfed.org/series/AUST448URN?utm_source=series_page&utm_medium=related_content&utm_term=related_resources&utm_campaign=categories
  # https://fred.stlouisfed.org/series/AUST448UR?utm_source=series_page&utm_medium=related_content&utm_term=other_formats&utm_campaign=other_format
  # https://alfred.stlouisfed.org/series?seid=ATNHPIUS12420Q&utm_source=series_page&utm_medium=related_content&utm_term=related_resources&utm_campaign=alfred
  # Los Angeles County
  # Unemployment rate
  # https://fred.stlouisfed.org/series/CALOSA7URN?utm_source=series_page&utm_medium=related_content&utm_term=related_resources&utm_campaign=categories
  #House price index
  #https://fred.stlouisfed.org/series/ATNHPIUS06037A

#4In brief, outline your approach to solving this problem (knowing that this might change later).
 #1.	Data collection
 #2.	Data wrangling
 #3.	Exploratory data analysis 
 #4.	Machine learning
#5 What are your deliverables? Typically, this would include code, along with a paper and/or a slide deck.
  #My deliverable will be 
    #1.	Code for my project, well-documented on GitHub
    #2.	Final paper in my GitHub repository explaining the problem, my approach and my findings, including ideas for further research.
    #3.	Slide deck for my project in my GitHub repository.
    #4.	An online video of my presentation, and share the link with my student advisor.


##################################################### DATASET INPUT #####################################################################################
datDir <- "C:/Users/Lishuang Cen/My Documents/Sandbox/Capstone Project/Capstone-Project"
#--- LA_Unemployment_Original
datFile1 <- paste(datDir, "Los_Angeles_Unemployment_Original.csv", sep = "/")
LA_Unemployment_Original <- read.table(datFile1, header = TRUE, sep= "")

#--- Round_Rock_Unemployment_Original
datFile2 <- paste(datDir, "Round_Rock_Unemployment_Original.csv", sep = "/")
Round_Rock_Unemployment_Original <- read.table(datFile2, header = TRUE, sep= "")

#--- LA_Demographics_Original
datFile3 <- paste(datDir, "Los_Angeles_Demographics_Original.csv", sep = "/")
LA_Demographics_Original <- read.table(datFile3, header = TRUE, sep = ",")

#--- Round_Rock_Demographics_Original
datFile4 <- paste(datDir, "Round_Rock_Demographics_Original.csv", sep = "/")
Round_Rock_Demographics_Original <- read.table(datFile4, header = TRUE, sep = ",")

#--- LA_Historical_Population_Original
datFile5 <- paste(datDir, "Los_Angeles_Historical_Population_Original.csv", sep = "/")
LA_Historical_Population_Original <- read.table(datFile5, header = TRUE, sep= ",")

#--- Round_Rock_Historical_Population_Original
datFile6 <- paste(datDir, "Round_Rock_Historical_Population_Original.csv", sep = "/")
Round_Rock_Historical_Population_Original <- read.table(datFile6, header = TRUE, sep= ",")

#--- LA_HPI_Original
datFile7 <- paste(datDir, "Los_Angeles_HPI_Original.csv", sep = "/")
LA_HPI_Original <- read.table(datFile7, header = TRUE, sep= "")

#--- Round_Rock_HPI_Original
datFile8 <- paste(datDir, "Round_Rock_HPI_Original.csv", sep = "/")
Round_Rock_HPI_Original <- read.table(datFile8, header = TRUE, sep= "")

####################################################### DATA WRANGLING ############################################################################################

# A. data wrangling
# 1. Los_Angles_unemployment_Original.csv vs. Round_Rock_Unemployment_Original.csv
  #a. rename columns
      names(LA_Unemployment_Original)[1] <- "Year"
      names(Round_Rock_Unemployment_Original)[1] <- "Year"
      names(LA_Unemployment_Original)[2] <- "Unemployment_Rate"
      names(Round_Rock_Unemployment_Original)[2] <- "Unemployment_Rate"
  #b. format the years 
      #- only have the first month of every year
      a <- endsWith(as.character(LA_Unemployment_Original$Year), "／1／1")  # Which rows to keep (kp)?
      LA_Unemployment_Clean <- LA_Unemployment_Original[a, ]
      b <- endsWith(as.character(Round_Rock_Unemployment_Original$Year), "／1／1")  # Which rows to keep (kp)?
      Round_Rock_Unemployment_Clean <- Round_Rock_Unemployment_Original[b, ]
      #- format the "year"
      LA_Unemployment_Clean$Year <- year(as.Date(LA_Unemployment_Clean$Year, "%y"))
      Round_Rock_Unemployment_Clean$Year <- year(as.Date(Round_Rock_Unemployment_Clean$Year, "%y"))
  #c. save to a new file
      datFile9 <- paste(datDir, "Los_Angeles_Unemployment_Clean.csv", sep = "/")
      write.table(LA_Unemployment_Clean, datFile9)
      datFile10 <- paste(datDir, "Round_Rock_Unemployment_Clean.csv", sep = "/")
      write.table(Round_Rock_Unemployment_Clean, datFile10)
  #d. combine two files together for the line chart
      Unemployment <- cbind(LA_Unemployment_Clean, Round_Rock_Unemployment_Clean[,2])
      names(Unemployment)[3] <- "Round_Rock_Unemployment_Rate"

# 2. Los_Angeles_Demographic_Original.csv vs. Round_Rock_Demographic_Original.csv.
  #a. rename columns
      names(LA_Demographics_Original)[2] <- "Ratio"
      names(Round_Rock_Demographics_Original)[2] <- "Ratio"
  #d. delete meaningless columns
      LA_Demographics_Original$X.1 <- NULL
      Round_Rock_Demographics_Original$X.1 <- NULL
  #c. delete meaningless rows
      LA_Demographics_Clean <- LA_Demographics_Original[1:7, ]
      Round_Rock_Demographics_Clean <- Round_Rock_Demographics_Original[1:7, ]
  #c. save to a new file
      datFile11 <- paste(datDir, "Los_Angeles_Demographics_Clean.csv", sep = "/")
      write.table(LA_Demographics_Clean, datFile11)
      datFile12 <- paste(datDir, "Round_Rock_Demographics_Clean.csv", sep = "/")
      write.table(Round_Rock_Demographics_Clean, datFile12)

# 3. Los_Angeles_Historical_Population_Origianl vs Round_Rock_Historical_Population_Original.
  #a. rename columns
      names(LA_Historical_Population_Original)[2] <- "Population"
      names(Round_Rock_Historical_Population_Original)[2] <- "Population"    
      names(LA_Historical_Population_Original)[3] <- "Percentage_Change"
      names(Round_Rock_Historical_Population_Original)[3] <- "Percentage_Change" 
  #b. delete meaningless rows.
      LA_Historical_Population_Clean <- LA_Historical_Population_Original[4:18, ]
      Round_Rock_Historical_Population_Clean <- Round_Rock_Historical_Population_Original[1:15, ]
  #c. save to a new file
      datFile13 <- paste(datDir, "Los_Angeles_Historical_Population_Clean.csv", sep = "/")
      write.table(LA_Historical_Population_Clean, datFile13)
      datFile14 <- paste(datDir, "Round_Rock_Historical_Population_Clean.csv", sep = "/")
      write.table(Round_Rock_Historical_Population_Clean, datFile14)
  #d. Combine two datasets together for the line chart
      Historical_Population <- cbind(LA_Historical_Population_Clean, Round_Rock_Historical_Population_Clean[,2:3])
      names(Historical_Population)[4]<- "Population_Round_Rock"
      names(Historical_Population)[5]<- "Percentage_Change_Round_Rock"
      
# 4. Los_Angeles_HPI_Original.csv vs. Round_Rock_HPI_Original.csv.
    #a. rename columns
      names(LA_HPI_Original)[1] <- "Year"
      names(Round_Rock_HPI_Original)[1] <- "Year" 
      names(LA_HPI_Original)[2] <- "HPI"
      names(Round_Rock_HPI_Original)[2] <- "HPI" 
    #b. only remain the first quarter of every year
      d <- endsWith(as.character(Round_Rock_HPI_Original$Year), "1／1")  # Which rows to keep (kp)?
      Round_Rock_HPI_Clean <- Round_Rock_HPI_Original[d,]
    #c. make two data comparable in the number of years
      LA_HPI_Clean <- LA_HPI_Original[4:42, ]
      Round_Rock_HPI_Clean <- Round_Rock_HPI_Clean[1:39, ]
    #d. format the year
      LA_HPI_Clean$Year <- year(as.Date(LA_HPI_Clean$Year, "%y"))
      Round_Rock_HPI_Clean$Year <- year(as.Date(Round_Rock_HPI_Clean$Year, "%y"))
    #e. save to a new file
      datFile15 <- paste(datDir, "Los_Angeles_HPI_Clean.csv", sep = "/")
      write.table(LA_HPI_Clean, datFile15)
      datFile16 <- paste(datDir, "Round_Rock_HPI_Clean.csv", sep = "/")
      write.table(Round_Rock_HPI_Clean, datFile16)
    #f. combine two dataset
      HPI <- cbind(LA_HPI_Clean, Round_Rock_HPI_Clean[2])
      names(HPI)[3] <- "HPI_Round_Rock"
############################################################### Explanatory Data Anlysis ####################################################################
# B. Explanatory Data Anlysis
      # 1. Los_Angles_unemployment_Original.csv vs. Round_Rock_Unemployment_Original.csv, Line graph

        Unemployment_line <- ggplot(Unemployment, aes(Year)) +
                geom_line(aes( y = Unemployment_Rate, colour = "Los Angeles", group = 1))+
                geom_line(aes( y = Round_Rock_Unemployment_Rate, colour = "Round Rock", group = 1))

        Unemployment_line_Discrete <- ggplot(Unemployment, aes(Year)) +
          geom_line(aes( y = Unemployment_Rate, colour = "Los Angeles", group = 1))+
          geom_line(aes( y = Round_Rock_Unemployment_Rate, colour = "Round Rock", group = 1))+ scale_x_discrete(2008,2009)

      # 2. Los_Angeles_Demographic_Original.csv vs. Round_Rock_Demographic_Original.csv., Pie chart
        # a. LA_Demographics
          names_LA <- as.character(LA_Demographics_Clean[,1])
          props_LA <- as.numeric(gsub('\\%', '', as.character(LA_Demographics_Clean[,2])))
          pie(props_LA, labels = names_LA, col = rainbow(length(props)), main = "Los Angeles Demographics")
        # b. Round_Rock_Demographics
          names_Round_Rock <- as.character(Round_Rock_Demographics_Clean[,1])
          props_Round_Rock <- as.numeric(gsub('\\%', '', as.character(Round_Rock_Demographics_Clean[,2])))
          pie(props_Round_Rock, labels = names_Round_Rock, col = rainbow(length(props)), main = "Round Rock Demographics")
          

      # 3. Los_Angeles_Historical_Population_Origianl vs Round_Rock_Historical_Population_Original, Line graph

          Historical_Population_line <- ggplot(Historical_Population, aes(Census)) +
            geom_line(aes( y = Population, colour = "Los Angeles"), group = 1) +
            geom_line(aes( y = Population_Round_Rock, colour = "Round Rock"), group = 1)

          Historical_Population_line_ratio <- ggplot(Historical_Population, aes(Census)) +
            geom_line(aes( y = Percentage_Change, colour = "Los Angeles"), group = 1) +
            geom_line(aes( y = Percentage_Change_Round_Rock, colour = "Round Rock"), group = 1, position = "stack")
          
      # 4. Los_Angeles_HPI_Original.csv vs. Round_Rock_HPI_Original.csv, Line graph
          
          HPI_line <- ggplot(HPI, aes(Year)) +
            geom_line(aes( y = HPI, colour = "Los Angeles"), group = 1) +
            geom_line(aes( y = HPI_Round_Rock, colour = "Round Rock"), group = 1)
          
          HPI_line_discrete <- ggplot(HPI, aes(Year)) +
            geom_line(aes( y = HPI, colour = "Los Angeles"), group = 1) +
            geom_line(aes( y = HPI_Round_Rock, colour = "Round Rock"), group = 1) + scale_x_discrete(2008,2009)
          
#################################################################### END ######################################################################  