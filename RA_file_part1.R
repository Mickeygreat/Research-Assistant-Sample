library(tidyverse)
library(readr)
library(readxl) # to read Excel file
library(visNetwork)
library(igraph) #--Basic creation and handling of "graph" objects--#
library(network) #--Basic creation and handling of "network" objects--#
library(intergraph) #--To switch back and forth between "graph" and "network" objects--#
library(statnet) #--For basic network plots like gplots--#
library(networkD3) #--To create interactive visuals--#
library(visNetwork) #--To create interactive visuals--#
library(networkDynamic) #--To analyse networks that evolve over time--#
library(tsna) #--Another package to analyse networks that evolve over time--#
# devtools::install_github("DougLuke/UserNetR")
library(UserNetR) #--Contains some network data sets--#
library(xtable)


# write a loop to read files of each year into tables
yearlist <- c(2007:2021)
for (i in yearlist){
  filename <- paste0("df", i) # Concatenate Strings in R
  wd <- paste0("/Users/Mickey_Yeh/Desktop/Research Assistant/Homelessness data/2007-2021-PIT-Counts-by-State (1).xlsx") # need to add xlsx!!
  assign(filename, read_excel(wd, sheet = as.character(i)))
}
# NOTE: 2017-2021 missing one row data => AS=row4
# 2007-2016 missing Two rows data => AS=row4 & MP=row28


# a loop that divides each value with CoC
# correlation matrix
# graphing Network

# to get all names of the data frames in global environment
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)] # created before other modified df are created in global env.

# function is the same thing as "def function():" in python 
formatter1 <- function(dataFrameEachYear)
{
  dataFrameEachYear.df <- as.data.frame(dataFrameEachYear)
  dataFrameEachYear.data <- dataFrameEachYear.df[,3:ncol(dataFrameEachYear.df)] # keeping from 3rd col to the last
  # create a matrix with corresponding rows & columns
  # but no row and no column names vonverted
  mdataFrameEachYear <- matrix(0, nrow(dataFrameEachYear.data), ncol(dataFrameEachYear.data)) 
  for (i in 1:ncol(dataFrameEachYear.data))
  {
    mdataFrameEachYear[, i] = dataFrameEachYear.data[, i]/dataFrameEachYear$`Number of CoCs` # dividing each value by CoC
  } 
  # dividing each value by CoC # adding values into empty matrix we created
  # drop 4th and last row while converting to data frame
  clean_mdataFrameEachYear <- data.frame(t(mdataFrameEachYear[-c(4, 28, nrow(mdataFrameEachYear)),])) # drop row 4 and last row total!
  # t function -> calculate transpose of a matrix or Data Frame.(switching rows and columns)
  # add column names to the cleaned data frame created from the matrix
  colnames(clean_mdataFrameEachYear) <- dataFrameEachYear.df$State[-c(4, 28, nrow(dataFrameEachYear.df))]
  
  return(clean_mdataFrameEachYear) 
}

# for year 2017-2021 missing one row data => AS=row4
formatter2 <- function(dataFrameEachYear)
{
  dataFrameEachYear.df <- as.data.frame(dataFrameEachYear)
  dataFrameEachYear.data <- dataFrameEachYear.df[,3:ncol(dataFrameEachYear.df)] # keeping from 3rd col to the last
  # create a matrix with corresponding rows & columns
  # but no row and no column names vonverted
  mdataFrameEachYear <- matrix(0, nrow(dataFrameEachYear.data), ncol(dataFrameEachYear.data)) 
  for (i in 1:ncol(dataFrameEachYear.data))
  {
    mdataFrameEachYear[, i] = dataFrameEachYear.data[, i]/dataFrameEachYear$`Number of CoCs` # dividing each value by CoC
  } 
  # dividing each value by CoC # adding values into empty matrix we created
  # drop 4th and last row while converting to data frame
  clean_mdataFrameEachYear <- data.frame(t(mdataFrameEachYear[-c(4, nrow(mdataFrameEachYear)),])) # drop row 4 and last row total!
  # t function -> calculate transpose of a matrix or Data Frame.(switching rows and columns)
  # add column names to the cleaned data frame created from the matrix
  colnames(clean_mdataFrameEachYear) <- dataFrameEachYear.df$State[-c(4, nrow(dataFrameEachYear.df))]
  
  return(clean_mdataFrameEachYear) 
}

# use mget to get list of dataframes and apply the function with lapply
clean_list_data2007_2016 <- lapply(mget(dfs[1:10]), formatter1)
clean_list_data2017_2021 <- lapply(mget(dfs[11:length(dfs)]), formatter2)
# clean_list_data should be a list of dataframes in the format that you want. 


# Final Part
a = c(2007:2021) # a[1]
# a = c(1:15)
# numberOfYears = 1:15
numberOfYears = seq(15)
fiveNumberSummary.list = list()

for (year in numberOfYears)
{
  if (a[year] <= 2016) 
  {
    Dist.stocks = 1 - cor(clean_list_data2007_2016[[year]]) # creating correlation matrix!!!
    hist(as.numeric(Dist.stocks)) # graph histogram to see how the distance is distributed and understand how much to set for benchmark
    epsilon = 0.2 # test different quartiles later
    stock.adjacency=matrix(0, nrow(Dist.stocks), ncol(Dist.stocks)) # create a 0 matrix with proper nrow, ncol
    for(i in 1:nrow(Dist.stocks))
    {
      for(j in 1:ncol(Dist.stocks))
      {
        if(Dist.stocks[i,j] < epsilon & Dist.stocks[i,j] > 0)
        {
          stock.adjacency[i,j] = 1
        }
        else
        {
          stock.adjacency[i,j] = 0
        }
      }
    }
    stock.adjacency # 1 => there's connetions between two nodes by benchmark(epsilon=0.2)
    rownames(stock.adjacency) <- rownames(Dist.stocks) # add proper row names
    colnames(stock.adjacency) <- colnames(Dist.stocks) # add proper column names
    class(stock.adjacency) # matrix
    
    stock.adjacency.df <- as.data.frame(stock.adjacency) # converting to data frame
    rownames(stock.adjacency.df) <- rownames(Dist.stocks) # add proper row names
    colnames(stock.adjacency.df) <- colnames(Dist.stocks) # add proper column names
    class(stock.adjacency.df) # data frame
    stock.net <- network(stock.adjacency, matrix.type="adjacency")
    
    ## Saving Graph to PNG
    png(filename = paste0("~/Desktop/Homelessness Networking Graphs 2007-2021/NW_", a[year], ".png", sep= ""),
        width = 888,
        height = 571)
    gplot(stock.net, displaylabels = T) # library(statnet)
    dev.off()
    
    #--the five number summary--# #and save as data frame for each year!!#
    network.size(stock.net) # Size
    gden(stock.net) # Density
    components(stock.net) # Components
    largest.component=component.largest(stock.net,result = "graph")
    geodist(largest.component)
    max(geodist(largest.component)$gdist)
    diameter(asIgraph(stock.net)) # diameters
    gtrans(stock.net, mode = "graph") # Clustering Coefficient???
    #--node centrality--#
    degree(stock.net, gmode = "graph")
    #--the five number summary--# #and save as data frame for each year!!#
    size <- network.size(stock.net) # Size
    density <- gden(stock.net) # Density
    components <- components(stock.net) # Components
    largest.component = component.largest(stock.net,result = "graph")
    geodist(largest.component)
    max(geodist(largest.component)$gdist)
    diameters <- diameter(asIgraph(stock.net)) # diameters
    clusteringCoefficient <- gtrans(stock.net, mode = "graph") # Clustering Coefficient???
    #--node centrality--#
    # degree(stock.net, gmode = "graph")
    #--library(xtable), just for teaching purposes--#
    # xtable(stock.adjacency, digits = 0, type = "latex", file = "AdjacencyMatrix2.tex")
    
    #--saving 5 number summary into a data frame--#
    n <- matrix(data = NA, nrow = 1, ncol = 5, byrow = FALSE, dimnames = NULL)
    # byrow=FALSE => the matrix is filled by columns, otherwise the matrix is filled by rows.
    colnames(n) <- c("Size", "Density", "Components", "Diameter", "Clustering Coefficient") # assign col names
    n[1,] = c(size, density, components, diameters, clusteringCoefficient) # assign row values
    # create a list to store the data frame
    fiveNumberSummary.list <- append(fiveNumberSummary.list, list(data.frame(n))) # append(myList, item, after = index)
  }
  else
  {
    Dist.stocks = 1 - cor(clean_list_data2017_2021[[year-10]]) # a[i-10] because it restarts from 1
    hist(as.numeric(Dist.stocks)) # graph histogram to see how the distance is distributed and understand how much to set for benchmark
    epsilon = 0.2 # test different quartiles later
    stock.adjacency=matrix(0, nrow(Dist.stocks), ncol(Dist.stocks)) # create a 0 matrix with proper nrow, ncol
    for(i in 1:nrow(Dist.stocks))
    {
      for(j in 1:ncol(Dist.stocks))
      {
        if(Dist.stocks[i,j] < epsilon & Dist.stocks[i,j] > 0)
        {
          stock.adjacency[i,j] = 1
        }
        else
        {
          stock.adjacency[i,j] = 0
        }
      }
    }
    stock.adjacency # 1 => there's connetions between two nodes by benchmark(epsilon=0.2)
    rownames(stock.adjacency) <- rownames(Dist.stocks) # add proper row names
    colnames(stock.adjacency) <- colnames(Dist.stocks) # add proper column names
    class(stock.adjacency) # matrix
    
    stock.adjacency.df <- as.data.frame(stock.adjacency) # converting to data frame
    rownames(stock.adjacency.df) <- rownames(Dist.stocks) # add proper row names
    colnames(stock.adjacency.df) <- colnames(Dist.stocks) # add proper column names
    class(stock.adjacency.df) # data frame
    stock.net <- network(stock.adjacency, matrix.type="adjacency")
    ## Saving NW Plot
    png(filename = paste("~/Desktop/Homelessness Networking Graphs 2007-2021/NW_", a[year], ".png", sep= ""),
        width = 888,
        height = 571)
    gplot(stock.net, displaylabels = T) # library(statnet)
    dev.off()
    
    #--the five number summary--# #and save as data frame for each year!!#
    network.size(stock.net) # Size
    gden(stock.net) # Density
    components(stock.net) # Components
    largest.component=component.largest(stock.net,result = "graph")
    geodist(largest.component)
    max(geodist(largest.component)$gdist)
    diameter(asIgraph(stock.net)) # diameters
    gtrans(stock.net, mode = "graph") # Clustering Coefficient???
    #--node centrality--#
    degree(stock.net, gmode = "graph")
    #--the five number summary--# #and save as data frame for each year!!#
    network.size(stock.net) # Size
    gden(stock.net) # Density
    components(stock.net) # Components
    largest.component=component.largest(stock.net,result = "graph")
    geodist(largest.component)
    max(geodist(largest.component)$gdist)
    diameter(asIgraph(stock.net)) # diameters
    gtrans(stock.net, mode = "graph") # Clustering Coefficient???
    #--node centrality--#
    degree(stock.net, gmode = "graph")
    #--the five number summary--# #and save as data frame for each year!!#
    size <- network.size(stock.net) # Size
    density <- gden(stock.net) # Density
    components <- components(stock.net) # Components
    largest.component = component.largest(stock.net,result = "graph")
    geodist(largest.component)
    max(geodist(largest.component)$gdist)
    diameters <- diameter(asIgraph(stock.net)) # diameters
    clusteringCoefficient <- gtrans(stock.net, mode = "graph") # Clustering Coefficient???
    #--node centrality--#
    # degree(stock.net, gmode = "graph")
    #--library(xtable), just for teaching purposes--#
    # xtable(stock.adjacency, digits = 0, type = "latex", file = "AdjacencyMatrix2.tex")
    
    #--saving 5 number summary into a data frame--#
    n <- matrix(data = NA, nrow = 1, ncol = 5, byrow = FALSE, dimnames = NULL)
    # byrow=FALSE => the matrix is filled by columns, otherwise the matrix is filled by rows.
    colnames(n) <- c("Size", "Density", "Components", "Diameter", "Clustering Coefficient") # assign col names
    n[1,] = c(size, density, components, diameters, clusteringCoefficient) # assign row values
    # create a list to store the data frame
    fiveNumberSummary.list <- append(fiveNumberSummary.list, list(data.frame(n))) # append(myList, item, after = index)
  }
}


