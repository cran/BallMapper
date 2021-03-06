library(igraph)
library(scales)
library(networkD3)
library(stringr)

#'Create vertices and edges (with additional properties) of a Ball Mapper graph representation of the input data. Please be aware that the program will not perform any normalization on the data. As with cluster analysis we recommend that you consider whether to normalize the data prior to running the function.
#'
#'@param points, a collection of input points in a form of a data frame. These are typically points in Euclidean space. By default the Euclidean distance is used to construct the Ball Mapper.
#'@param values, a collection of outcome values which apply to the data points. Mean values of this variable within any given ball will be used to color the Ball Mapper graph. If it is not available, please set it to a constant array with the same length as the number of observations in the dataset.
#'@param epsilon, the value of radius of balls used in the Ball Mapper construction.
#'@return The function returns a long list of outputs which are explained below:
#'vertices, comprises two binded lists: First one which contains an increasing sequence of numbers starting from 1 to the number of vertices. Each of them corresponds to a landmark point. The second one contains the number of points covered by a ball of radius epsilon centered by the following landmark points.
#'edges, a collection of not directed edges composed of the first and the second vertex. Ordering of vertices do not have meaning.
#'edges_strength, For every edge [a,b] we define its strength as the number of points that are covered by both landmarks a and b. This array contains the strength of every edge in the Ball Mapper graph.
#'points_covered_by_landmarks, is a list of vectors. I-th vector contains the positions of points covered by i-th landmark.
#'landmarks, contains a list of positions of the landmark points used to construct the balls.
#'coloring, is a vector having as many positions as the number of lanrmarks. It contains the averaged outcome values of the coloring variable corresponding to the points covered by each landmark.
#' @examples
#' var <- seq(from=0,to=6.3,by=0.1)
#' points <- as.data.frame( cbind( sin(var),cos(var) ) )
#' values <- as.data.frame( sin(var) )
#' epsilon <- 0.25
#' l <- BallMapper(points,values,epsilon)
#' @export
BallMapper <- function( points , values , epsilon )
{
  #First we create an array of the same length as the collection of points. We will store here the numbers of landmarks that cover every given point.
  coverage <-  list()
  for ( i in 1:length(points[,1]))
  {
    coverage[[i]] <- vector()
  }
  #In this vector we will be storing the ids of landmarks:
  landmarks <- vector()
  first_uncovered <- 1
  number_of_landmark <- 1

  #Now we will be adding landmark by landmark as long as all the points are not covered.
  while ( first_uncovered <=  length(points[,1]) )
  {
    landmarks <- c(landmarks, first_uncovered)
    #Now we will check which points are covered by the first_uncovered.
    for ( j in 1:length(points[,1]) )
    {
      #this bit is not optimal as we are copying a lot of data here for high dimensional point clouds...
      distance <- stats::dist(rbind( points[j,] , points[first_uncovered,]) )
      if ( distance <= epsilon )
      {
        coverage[[j]] <- c( coverage[[j]] , number_of_landmark )
      }
    }
    while ( TRUE )
    {
      if (first_uncovered >  length(points[,1])) break
      if (length(coverage[[first_uncovered]])==0)break
      first_uncovered = first_uncovered+1
    }
    #print(paste0("first_uncovered: ", first_uncovered))
    number_of_landmark <- number_of_landmark+1
  }
  #To ballance the last additional increment.
  number_of_landmark <- number_of_landmark-1

  #Over here we compute the list of elements which are covered by the following landmarks:
  points_covered_by_landmarks <-  list()
  for ( i in 1:length(landmarks))
  {
    points_covered_by_landmarks[[i]] = vector()
  }
  for ( i in 1:length(coverage) )
  {
    for ( j in 1:length(coverage[[i]]) )
    {
      points_covered_by_landmarks[[ coverage[[i]][j] ]] <- c(points_covered_by_landmarks[[ coverage[[i]][j] ]],i)
    }
  }

  #now we create a graph. Number of vertices is the same as number_of_landmark.
  #We will create a list storing the number of points covered by each landmark.
  numer_of_covered_points = vector( length=number_of_landmark )
  for ( i in 1:length(points_covered_by_landmarks) )
  {
    numer_of_covered_points[ i ] <- 2+length(points_covered_by_landmarks[[i]])
  }

  #And for every landmark, we will consider all the points covered by it, and compute the average value of function therein.
  #It will be stored in the variable named coloring.
  coloring = vector( length=number_of_landmark )
  for ( i in 1:length(points_covered_by_landmarks) )
  {
    average_function_value <- 0
    for ( j in 1:length(points_covered_by_landmarks[[i]]) )
    {
      average_function_value <- average_function_value+values[ points_covered_by_landmarks[[i]][j], ]
    }
    average_function_value <- average_function_value/length(points_covered_by_landmarks[[i]])
    coloring[i] <- average_function_value
  }


  #Here we create the edges with weights:
  from = vector()
  to = vector()
  for ( i in 1:length(coverage) )
  {
    for ( j in 1:length(coverage[[i]]) )
    {
      for ( k in j:length(coverage[[i]]) )
      {
        if ( j != k )
        {
          from <- c( from,coverage[[i]][j] )
          to <- c(to,coverage[[i]][k])
        }
      }
    }
  }


  #and here we create the network. Nodes are weighted by the number of points covered by them
  nodes=cbind('id'=1:number_of_landmark,size=numer_of_covered_points)
  links = cbind(from,to)

  #We may want to remove repetitions from links:
  #links <- unique(links)
  #or to use the number of repetitions as a measure of a strength of an edge ToDo
  #this part of code compute number of repetitions of edges. This number can be used
  #as the edge's weight and utylized during the visualization.
  #NOTE THAT THIS IS QUADRATIC PROCEDURE THAT SHOULD BE OPTYMIZED!!
  unique_from = vector()
  unique_to = vector()
  strength_of_edges = vector()
  was_edge_counted <- vector(  length=length(links[,1])  )
  first_not_counted_edge = 1;
  while ( first_not_counted_edge <= length(links[,1]) )
  {
    #print(paste0("Edge to consider: ", links[first_not_counted_edge,1],  " " , links[first_not_counted_edge,2]))
    number_of_repetitions_of_this_edge <- 0
    for ( i in first_not_counted_edge:length(links[,1]) )
    {
      if ( (links[i,1] == links[first_not_counted_edge,1])&(links[i,2] == links[first_not_counted_edge,2]) )
      {
        number_of_repetitions_of_this_edge <- number_of_repetitions_of_this_edge+1
        was_edge_counted[ i ] = TRUE;
      }
    }
    unique_from = c( unique_from , links[first_not_counted_edge,1]  )
    unique_to = c( unique_to  , links[first_not_counted_edge,2] )
    strength_of_edges = c( strength_of_edges , number_of_repetitions_of_this_edge )
    while ( first_not_counted_edge <= length(links[,1]) )
    {
      if ( was_edge_counted[ first_not_counted_edge ] == TRUE )
      {
        first_not_counted_edge <- first_not_counted_edge+1;
      }
      else
      {
        break
      }
    }
  }
  links = cbind(unique_from,unique_to)
  return_list <- list( "vertices" = nodes , "edges" = links ,
                       "edges_strength" = strength_of_edges ,
                       "points_covered_by_landmarks" = points_covered_by_landmarks,
                       "landmarks" = landmarks , "coloring" = coloring ,
                       "coverage" = coverage )

  return(return_list)
}#BallMapper


#'Produce a static color visualization of the Ball Mapper graph. It is based on the output from BallMapper function.
#'
#'@param outputFromBallMapper, an output from the BallMapper function
#'@param showVertexLabels, a boolean value determining if the vertex labels are to be shown (TRUE by default).
#'@param showLegend, a boolean value determining if the legend is to be shown (FALSE by default).
#'@param minimal_ball_radius, provide a minimal value of the radius of balls used in visualization (7 by default).
#'@param maximal_ball_scale, provide a maximal value of the radius of balls used in visualization (20 by default).
#'@param maximal_color_scale, Provide a maximal value (starting from 0) of the color of a ball (10 by default).
#'@param seed_for_plotting, if set to the same number will suspend the fandom argument in the ploting rountine and produce plots with the same layout everytime.
#'@param store_in_file if set to a string, will open a png file, and store the plot therein. By default it is set to an empty string.
#'@param default_x_image_resolution store a default resolution of image in x direction. Set to 512 by default.
#'@param default_y_image_resolution store a default resolution of image in y direction. Set to 512 by default.
#'@param number_of_colors store a number of colors used in the plot.
#'@return None.
#'@examples
#' var <- seq(from=0,to=6.3,by=0.1)
#' points <- as.data.frame( cbind( sin(var),cos(var) ) )
#' values <- as.data.frame( sin(var) )
#' epsilon <- 0.25
#' l <- BallMapper(points,values,epsilon)
#' ColorIgraphPlot(l)
#' @export
ColorIgraphPlot <- function( outputFromBallMapper, showVertexLabels = TRUE , showLegend = FALSE , minimal_ball_radius = 7 , maximal_ball_scale=20, maximal_color_scale=10 , seed_for_plotting = -1 , store_in_file = "" , default_x_image_resolution = 512 , default_y_image_resolution = 512 , number_of_colors = 100)
{
  vertices = outputFromBallMapper$vertices
  vertices[,2] <- maximal_ball_scale*vertices[,2]/max(vertices[,2])+minimal_ball_radius
  net = igraph::graph_from_data_frame(outputFromBallMapper$edges,vertices = vertices,directed = F)

  jet.colors <- grDevices::colorRampPalette(c("red","orange","yellow","green","cyan","blue","violet"))
  color_spectrum <- jet.colors( number_of_colors )

  #and over here we map the pallete to the order of values on vertices
  min_ <- min(outputFromBallMapper$coloring)
  max_ <- max(outputFromBallMapper$coloring)
  color <- vector(length = length(outputFromBallMapper$coloring),mode="double")
  for ( i in 1:length( outputFromBallMapper$coloring ) )
  {
    position <- base::max(base::ceiling(number_of_colors*(outputFromBallMapper$coloring[i]-min_)/(max_-min_)),1)
    color[ i ] <- color_spectrum [ position ]
  }
  igraph::V(net)$color <- color

  if ( showVertexLabels == FALSE  )igraph::V(net)$label = NA

  if ( seed_for_plotting != -1 )base::set.seed(seed_for_plotting)

  if ( store_in_file != "" ) grDevices::png(store_in_file, default_x_image_resolution, default_y_image_resolution)

  #igraph::V(net)$label.cex = 1.3 #Change this line if you would like to have labels of different sizes.

  graphics::plot(net)

  fields::image.plot(legend.only=T, zlim=range(outputFromBallMapper$coloring), col=color_spectrum )

  if ( store_in_file != "" )grDevices::dev.off()

}#ColorIgraphPlot


#'Produce a static grayscale visualization of the Ball Mapper graph. It is based on the output from the BallMapper function.
#'
#'@param outputFromBallMapper, an output from the BallMapper function
#'@param showVertexLabels, a boolean value determining if vertex labels are to be shown (TRUE by default).
#'@param minimal_ball_radius, provide a minimal value of the radius of balls used in visualization (7 by default).
#'@param maximal_ball_scale, provides a maximal value of the radius of the balls used in visualization (20 by default).
#'@param seed_for_plotting, if set to the same number will suspend the fandom argument in the ploting rountine and produce plots with the same layout everytime.
#'@param store_in_file if set to a string, will open a png file, and store the plot therein. By default it is set to an empty string.
#'@param default_x_image_resolution store a default resolution of image in x direction. Set to 512 by default.
#'@param default_y_image_resolution store a default resolution of image in y direction. Set to 512 by default.
#'@return None.
#'@examples
#' var <- seq(from=0,to=6.3,by=0.1)
#' points <- as.data.frame( cbind( sin(var),cos(var) ) )
#' values <- as.data.frame( sin(var) )
#' epsilon <- 0.25
#' l <- BallMapper(points,values,epsilon)
#' GrayscaleIgraphPlot(l)
#' @export
GrayscaleIgraphPlot <- function( outputFromBallMapper , showVertexLabels = TRUE , minimal_ball_radius = 7 , maximal_ball_scale=20  , seed_for_plotting = -1 , store_in_file = "" , default_x_image_resolution = 512  , default_y_image_resolution = 512)
{
  vertices = outputFromBallMapper$vertices
  vertices[,2] <- maximal_ball_scale*vertices[,2]/max(vertices[,2])+minimal_ball_radius

  net = igraph::graph_from_data_frame(outputFromBallMapper$edges,vertices = vertices,directed = F)

  coloring = outputFromBallMapper$coloring
  coloring <- grDevices::gray(scales::rescale(outputFromBallMapper$coloring, c(0, 1)))
  igraph::V(net)$color = coloring

  if ( showVertexLabels == FALSE  )igraph::V(net)$label = NA
  #this command sents up a fancy background, remove if not needed.
  #par(bg="grey32", mar=c(0,0,0,0))

  if ( seed_for_plotting != -1 )base::set.seed(seed_for_plotting)
  if ( store_in_file != "" ) grDevices::png(store_in_file, default_x_image_resolution, default_y_image_resolution)

  graphics::plot(net)

  if ( store_in_file != "" )grDevices::dev.off()

  #return(net)
}#GrayscaleIgraphPlot

#'Produce a two column list. The first column contain the number of point (possibly with repetitions), the second one contains the number of landmark points that cover it.
#'For example, let us assume that point 1 is covered by landmark 1 and 2, and point 2 is covered by the landmark 2. In this case the obtained list is of a form:
#'1 1
#'1 2
#'2 2
#'This list can be used for a further analysis of various parts of Ball Mapper graph.
#'@param coverageFromBallMapper, a coverage parameter of an output from BallMapper function
#'@return List of landmarks covering each point, as described above.
#'@examples
#' var <- seq(from=0,to=6.3,by=0.1)
#' points <- as.data.frame( cbind( sin(var),cos(var) ) )
#' values <- as.data.frame( sin(var) )
#' epsilon <- 0.25
#' l <- BallMapper(points,values,epsilon)
#'list <- pointToBallList(l$coverage)
#' @export
pointToBallList <- function( coverageFromBallMapper )
{
  vertices <- vector()
  coveringBalls <- vector()
  for ( i in 1:length(coverageFromBallMapper) )
  {
      v <- unlist(coverageFromBallMapper[i])
      for ( j in 1:length(v) )
      {
        vertices <- c( vertices , i )
        coveringBalls <- c( coveringBalls , v[j] )
      }
  }
  return(cbind( vertices , coveringBalls ))
}

#'This is a simple example of dynamic visualization using networkD3 library.
#'This version do not implement coloring of vertices, just give a general overview of the edges.
#'@param outputFromBallMapper, an output from BallMapper function.
#'@param storeAsHtml, if set true, it will store the graph in HTML file.
#' @return None
#' @examples
#' var <- seq(from=0,to=6.3,by=0.1)
#' points <- as.data.frame( cbind( sin(var),cos(var) ) )
#' values <- as.data.frame( sin(var) )
#' epsilon <- 0.25
#' l <- BallMapper(points,values,epsilon)
#' simpleDynamicNetwork(l)
#' @export
simpleDynamicNetwork <- function( outputFromBallMapper , storeAsHtml = FALSE   )
{
  networkData <- data.frame(outputFromBallMapper$edges-1)
  sn <- networkD3::simpleNetwork(networkData,zoom=T)
  methods::show(sn)
  if ( storeAsHtml == TRUE )networkD3::saveNetwork(file = 'Net1.html')
}#simpleDynamicNetwork

#'This procedure produces a dynamic graph with colors. It allows zoom-in operation and displays information about vertices when they are clicked upon.
#'
#'@param outputOfBallMapper, an output from the BallMapper function
#'@param showLegend, if set to TRUE a legend will be displayed indicating the coloring of the values of vertices.
#' @return None
#'@examples
#' var <- seq(from=0,to=6.3,by=0.1)
#' points <- as.data.frame( cbind( sin(var),cos(var) ) )
#' values <- as.data.frame( sin(var) )
#' epsilon <- 0.25
#' l <- BallMapper(points,values,epsilon)
#' coloredDynamicNetwork(l)
#' @export
coloredDynamicNetwork <- function( outputOfBallMapper , showLegend = FALSE )
{
  #preparation of links
  source <- outputOfBallMapper$edges[,1]-1
  target <- outputOfBallMapper$edges[,2]-1
  value <- outputOfBallMapper$edges_strength
  links = cbind(source,target,value)
  links <- as.data.frame(links)

  #preparation of nodes
  vert <- paste('id:',as.character(outputOfBallMapper$vertices[,1]),',val:',as.character(outputOfBallMapper$coloring))
  nodeSize <- outputOfBallMapper$vertices[,2]
  color <- outputOfBallMapper$coloring

  vertices <- cbind(vert,color,nodeSize)
  vertices<- as.data.frame(vertices)

  fn <- networkD3::forceNetwork(
           Links = links, Nodes = vertices,
           Source = "source", Target = "target",
           NodeID = "vert",
           Value = "value", Group = "color" ,
           opacity = 1,
           opacityNoHover = 0.1,
           zoom = T,
           Nodesize = "nodeSize",
           legend = showLegend
           )
  methods::show(fn)
}#coloredDynamicNetwork




#'Produce a collection of png files with mapper graphs colored by following coordinates (so that the number of files is the same as the number of coordinates).
#'
#'@param outputFromBallMapper an output from the BallMapper function
#'@param points, a collection of input points in a form of a data frame used to create Ball Mapper graph.
#'@param fileNamePrefix a prefix of a file name. A plot that uses i-th variable as a coloring will contain this string as a prefix followed by the number i. Set to "output_" by default.
#'@param defaultXResolution store a default resolution of image in x direction. Set to 512 by default.
#'@param defaultYResolution store a default resolution of image in y direction. Set to 512 by default.
#'@return none.
#var <- seq(from=0,to=6.3,by=0.1)
#points <- as.data.frame( cbind( sin(var),cos(var) ) )
#values <- as.data.frame( sin(var) )
#epsilon <- 0.25
#l <- BallMapper(points,values,epsilon)
#colorByAllVariables(l,points,"your_favorite_file_name")
#'@export
colorByAllVariables<- function( outputFromBallMapper , points , fileNamePrefix = "output_" , defaultXResolution = 512 , defaultYResolution = 512 )
{
  oldColoring <- outputFromBallMapper$coloring

  #for every dimension in points vector:\
  for ( i in 1:length(points) )
  {
    val <- as.data.frame(points[,i])
    newColoring <- vector( length=length(outputFromBallMapper$points_covered_by_landmarks) )
    #for every landmark point
    for ( land in 1:length(outputFromBallMapper$points_covered_by_landmarks))
    {
      average <- 0
      for ( coveredPoint in 1:length( outputFromBallMapper$points_covered_by_landmarks[[land]] ) )
      {
        average <- average + val[ outputFromBallMapper$points_covered_by_landmarks[[land]][coveredPoint ] , ]
      }
      average <- average/length( outputFromBallMapper$points_covered_by_landmarks[[land]] )
      newColoring[land] <- average
    }

    #compute and set up the new coloring of it
    outputFromBallMapper$coloring <- newColoring

    #Here we set up the name of a file.
    filename_ <- cbind( fileNamePrefix , toString(i) , ".png" )
    filename <- stringr::str_c( filename_ , collapse = "")

    ColorIgraphPlot(outputFromBallMapper, seed_for_plotting = 123 , store_in_file = filename , default_x_image_resolution = defaultXResolution  , default_y_image_resolution = defaultYResolution)
    #GrayscaleIgraphPlot(outputFromBallMapper, seed_for_plotting = 123 , store_in_file = filename , default_image_resolution = defaultResolution)
  }
  outputFromBallMapper$coloring <- oldColoring;
}#colorByAllVariables


#' This function normalize each column (variable) of the input dataset so that the maximum is mapped to one, minimum to zero, and the intermediate values linearly to the appropriate points in the interval (0,1).
#' @param points, a collection of input points in a form of a data frame.
#' @return Normalized collection of points.
#' @examples
#' var <- seq(from=0,to=6.3,by=0.1)
#' points <- as.data.frame( cbind( sin(var),cos(var) ) )
#' normalized_points <- normalize_to_min_0_max_1 (points)
#' @export
normalize_to_min_0_max_1 <- function( points )
{
  for (  i in 1:length(points) )
  {
    points[,i] <- scales::rescale(points[,i], c(0, 1))
  }
  return(points)
}#normalize_to_min_0_max_1


#' This function normalize each column (variable) of the input dataset so that the the average of the normalized column is 0 and its standard deviation is 1.
#' @param points, a collection of input points in a form of a data frame.
#' @return Nowmalized collectpion of points.
#' @examples
#' var <- seq(from=0,to=6.3,by=0.1)
#' points <- as.data.frame( cbind( sin(var),cos(var) ) )
#' normalized_points <- normalize_to_average_0_stdev_1 (points)
#' @export
normalize_to_average_0_stdev_1 <- function( points )
{
  for (  i in 1:length(points) )
  {
    av <- base::mean(points[,i])
    stdev <- stats::sd(points[,i])
    for ( j in 1:length( points[,i] ) )
    {
      points[j,i] <- (points[j,i] - av )/stdev
    }
  }
  return(points)
}#normalize_to_average_0_stdev_1


#' This function returns a list of points covered by the given collection of landmarks.
#'@param outputFromBallMapper an output from the BallMapper function
#'@param numbers_of_landmarks a vector containnig the numbers of landmarks under consideration.
#'@return A vector of points covered by the landmarks given in numbers_of_landmarks.
#'@example
#'var <- seq(from=0,to=6.3,by=0.1)
#'points <- as.data.frame( cbind( sin(var),cos(var) ) )
#'values <- as.data.frame(sin(var))
#'l <- BallMapper(points, values, 0.25)
#'pts_indices <- points_covered_by_landmarks(l,c(1,2,3,4))
#'@export
points_covered_by_landmarks <- function( outputFromBallMapper , numbers_of_landmarks )
{
  #find all the vertices covered by the given landmarks. They will  be obtained by catenating points_covered_by_landmarks lists.
  all_vertices <- vector()
  for ( i in 1:length(numbers_of_landmarks )  )
  {
    if ( numbers_of_landmarks[i] >length( outputFromBallMapper$vertices )  )
    {
      base::warning( "Number of landmark out of range, it will not be taken into account.")
    }
    else
    {
      all_vertices <- c( all_vertices , outputFromBallMapper$points_covered_by_landmarks[ numbers_of_landmarks[i] ] )
    }
  }
  all_vertices <- base::unlist(all_vertices)
  #sort all_vertices and remove repetitions.
  all_vertices <- base::sort( all_vertices )
  all_vertices <- base::rle(all_vertices )$val
}#points_covered_by_landmarks


#' This is an auxiliery function. It take the coordinates of points, ids of subset of points, and number of coordinate, and return a sorted vector of the given coodrinate in the considered points.
#' For instance, given the collection of points:
#' 1 2 3
#' 4 5 6
#' 7 8 9
#' and which_subset = 2,3
#' and number_of_coordinate = 2
#' the procedure below will return the vector [2,5,8].
#'@param points is a collection of input points in a form of a data frame. The same one as on the input of the Ball Mapper.
#'@param which_subset Indices of points in the given subset.
#'@param number_of_coordinate which coordinate of the consired points to export.
#'@return the sorted vector of values of a given variable at the collection of points.
#'var <- seq(from=0,to=6.3,by=0.1)
#'points <- as.data.frame( cbind( sin(var),cos(var) ) )
#'values <- as.data.frame(sin(var))
#'l <- BallMapper(points, values, 0.25)
#'coordinates_of_points_in_subcollection(points,c(6,7,8),1)
#'@export
coordinates_of_points_in_subcollection <- function( points , which_subset, number_of_coordinate )
{
    if ( (number_of_coordinate<1) || (number_of_coordinate > length(points)) )
    {
       base::warning( "Wrong number of coordinate in the coordinates_of_points_in_subcollection procedure.")
    }
    number_of_points <- length( points[,1] )
    result <- vector( length=length(which_subset) );
    for ( pt in 1:length(which_subset) )
    {
        if ( (which_subset[pt] < 1) || (which_subset[pt] > number_of_points) )
        {
            base::warning( "Wrong id of point in the coordinates_of_points_in_subcollection procedure.")
        }
        else
        {
          result[pt] <- points[ which_subset[pt] , number_of_coordinate ];
        }
    }
    return(result)
}#coordinates_of_points_in_subcollection



#' This procedure take two subset of points (that come from the vertices of Ball Mapper) and return
#' the coordinates on which the averages of those two collections differs most. To ballance the effect
#' of potentially different orders of magnitude of data in column, we divide the difference in means by the mean of the whole column.
#'@param points  a collection of input points in a form of a data frame. The same one as on the input of the Ball Mapper.
#'@param subset1 First subset of ids of points.
#'@param subset2 Second subset of ids of points.
#'@return Vector of corrdinate ids with the absolute value of difference between averages, ordered according to the second variable.
#'var <- seq(from=0,to=6.3,by=0.1)
#'points <- as.data.frame( cbind( sin(var),cos(var) ) )
#'values <- as.data.frame(sin(var))
#'l <- BallMapper(points, values, 0.25)
#'g1 <- c(1,21
#'g2 <- c(11,12)
#'find_dominant_difference_using_averages(points,g1,g2)
#'@export
find_dominant_difference_using_averages <- function( points , subset1 , subset2 )
{
    differences <- vector( length=length(points) )
    coords <- vector( length=length(points) )
    for ( coord in 1:length(points) )
    {
       v1 <- coordinates_of_points_in_subcollection( points , subset1, coord )
       v2 <- coordinates_of_points_in_subcollection( points , subset2, coord )
       differences[coord] <- abs(mean(v1)-mean(v2))/mean( points[,coord] )
       coords[coord] <- coord
    }
    result <- as.data.frame( cbind( coords,differences ) )
    result <- result[ base::order( -result$differences ), ]

    return(result)
}#find_dominant_difference_using_averages



#' This procedure take two subset of points (that come from the vertices of Ball Mapper) and return
#' the coordinates on which the averages of those two collections differs most. To ballance the effect
#' of potentially different orders of magnitude of data in column, we divide the difference in means by the standard deviation of the whole column.
#'@param points  a collection of input points in a form of a data frame. The same one as on the input of the Ball Mapper.
#'@param subset1 First subset of ids of points.
#'@param subset2 Second subset of ids of points.
#'@return Vector of corrdinate ids with the absolute value of difference between averages normalized by the standard deviation of the considered column, ordered according to the second variable.
#'var <- seq(from=0,to=6.3,by=0.1)
#'points <- as.data.frame( cbind( sin(var),cos(var) ) )
#'values <- as.data.frame(sin(var))
#'l <- BallMapper(points, values, 0.25)
#'g1 <- c(1,21
#'g2 <- c(11,12)
#'find_dominant_difference_using_averages(points,g1,g2)
#'@export
find_dominant_difference_using_averages_normalized_by_sd <- function( points , subset1 , subset2 )
{
  differences <- vector( length=length(points) )
  coords <- vector( length=length(points) )
  for ( coord in 1:length(points) )
  {
    v1 <- coordinates_of_points_in_subcollection( points , subset1, coord )
    v2 <- coordinates_of_points_in_subcollection( points , subset2, coord )
    differences[coord] <- abs(mean(v1)-mean(v2))/stats::sd( points[,coord] )
    coords[coord] <- coord
  }
  result <- as.data.frame( cbind( coords,differences ) )
  result <- result[ base::order( -result$differences ), ]

  return(result)
}#find_dominant_difference_using_averages_normalized_by_sd



#' This function will provide a new coloring which is the minimal and average distance of points in the
#' point cloud to the referece points. The output from this procedure can be used as an alternative coloring in BallMapper.
#' @param allPoints is a collection of all points in the dataset.
#' @param refPoints is a subset of all points. The function will compute the distance of each point from allPoints to referencePoints
#' @return a pair of minimal and average distances. They can be used to color the BallMapper graph.
#' var <- seq(from=0,to=6.3,by=0.1)
#' points <- as.data.frame( cbind( sin(var),cos(var) ) )
#' values <- as.data.frame(sin(var))
#' l <- BallMapper(points, values, 0.25)
#' pts <- as.data.frame(points_covered_by_landmarks(l,1))
#' new_coloring_function <- color_by_distance_to_reference_points( points, pts )
#' l$coloring <- new_coloring_function[,1]
#' ColorIgraphPlot(l)
#' l$coloring <- new_coloring_function[,2]
#' ColorIgraphPlot(l)
#' @export
color_by_distance_to_reference_points <- function( allPoints , refPoints )
{
    newColoringMin <- vector( length=length(allPoints[,1]) )
    newColoringAV <- vector( length=length(allPoints[,1]) )
    #Now for every point:
    for ( pt in 1:length(allPoints[,1]) )
    {
         min_distance <- .Machine$double.xmax
         sum_of_distances <- 0
         for ( ref in 1:length(refPoints[,1]) )
         {
             #compute a distance from allPoints[pt] and refPoints[ref]
             dist <-  stats::dist(rbind( allPoints[pt,] , refPoints[ref,] ) )
             if ( dist < min_distance )min_distance = dist
             sum_of_distances <- sum_of_distances + dist;
         }
         newColoringMin[pt] <- min_distance
         newColoringAV[pt] <- sum_of_distances/length(refPoints[,1])
    }
    return( cbind(newColoringMin,newColoringAV) )
}#color_by_distance_to_reference_points



#'This procedure store the Ball Mapper graph in a file in the following format:
#'@param outputFromBallMapper output from the BallMapper procerure.
#'@param filename the name of the file to store the data.
#'@return None
#' var <- seq(from=0,to=6.3,by=0.1)
#' points <- as.data.frame( cbind( sin(var),cos(var) ) )
#' values <- as.data.frame(sin(var))
#' l <- BallMapper(points, values, 0.25)
#' storeBallMapperGraphInFile(l,"my_favorite_BM_graph")
#'@export
storeBallMapperGraphInFile <- function( outputFromBallMapper , filename = "BM_graph" )
{
  #Writing vertices
  utils::write.table( outputFromBallMapper$vertices , file =paste(filename,"_vertices",sep=""), col.names = F, row.names = F)

  #Writing edges
  utils::write.table(outputFromBallMapper$edges, file=paste(filename,"_edges",sep=""), col.names = F, row.names = F)

  #Writing edge's strength
  utils::write.table(outputFromBallMapper$edges_strength, file=paste(filename,"_edges_strength",sep=""), col.names = F, row.names = F)

  #Writing points covered by landmarks. This part is a bit more tricky, as this is a list
  #In this case, I want to have points covered by landmark i in the i-th line of the file. To
  #achieve this, each line is store as a string, and those strings are grouped in a vector.
  output <- vector()
  for ( i in 1:length( outputFromBallMapper$points_covered_by_landmarks ) )
  {
    line <- ""
    for ( j in 1:length( outputFromBallMapper$points_covered_by_landmarks[[i]] ) )
    {
        line <- paste( line , outputFromBallMapper$points_covered_by_landmarks[[i]][j] )
    }
    output <- c( output , line )
  }
  fileConn<-file(paste(filename,"_points_covered_by_landmarks",sep=""))
  writeLines(output, fileConn)
  close(fileConn)

  #Writing landmarks
  utils::write.table(outputFromBallMapper$landmarks, file=paste(filename,"_landmarks",sep=""), col.names = F, row.names = F)

  #Writing coloring
  utils::write.table(outputFromBallMapper$coloring, file=paste(filename,"_coloring",sep=""), col.names = F, row.names = F)

  #At the moment we do not store coverage, as this unformation can be recovered from points covered by landmarks.
}#storeBallMapperGraphInFile


#' This procedure read the BallMapper object from file. The parameter of the file
#' is filename. We assume that files:
#' filename_vertices
#' filename_edges
#' filename_edges_strength
#' filename_points_covered_by_landmarks
#' filename_landmarks
#' filename_coloring
#' @param filename prefix of the name of the file containing elements of Ball Mapper graph.
#' @return BallMapper object
#' var <- seq(from=0,to=6.3,by=0.1)
#' points <- as.data.frame( cbind( sin(var),cos(var) ) )
#' values <- as.data.frame(sin(var))
#' l <- BallMapper(points, values, 0.25)
#' storeBallMapperGraphInFile(l,"my_favorite_BM_graph")
#' l_prime <- readBallMapperGraphFromFile("my_favorite_BM_graph")
#' @export
readBallMapperGraphFromFile <- function( filename )
{
   vertices <- utils::read.table( file = base::paste(filename,"_vertices",sep="") )
   colnames( vertices ) <- c( "id" , "size" )

   edges <- utils::read.table( file = base::paste(filename,"_edges",sep="") )
   colnames( edges ) <- c( "unique_from" , "unique_to" )

   edges_strength <- as.integer(unlist( utils::read.table( file = base::paste(filename,"_edges_strength",sep="") ) ) )
   landmarks <- as.integer(unlist( utils::read.table( file = base::paste(filename,"_landmarks",sep="") ) ) )
   coloring <- as.double( unlist( utils::read.table( file = base::paste(filename,"_coloring",sep="") ) ) )

   con = base::file( base::paste(filename,"_points_covered_by_landmarks",sep="") )
   lines <- readLines(con)
   close(con)
   number_of_points <- 0
   points_covered_by_landmarks <- list()
   for ( i in 1:length(lines) )
   {
      v <- as.integer( unlist( strsplit(lines[i]," ")) )
      v <- v[!is.na(v)]
      if ( base::max( v ) > number_of_points )number_of_points <- base::max( v )
      points_covered_by_landmarks[[i]] <- v
   }

   #we do not have this one at the moment.
   coverage <-  list()
   for ( i in 1:number_of_points)
   {
     coverage[[i]] <- vector()
   }
   for ( i in 1:length(points_covered_by_landmarks) )
   {
       for ( j in 1:length(points_covered_by_landmarks[[i]]) )
       {
         coverage[[ points_covered_by_landmarks[[i]][j] ]] <- c( coverage[[ points_covered_by_landmarks[[i]][j] ]] , i  )
       }
   }


   return_list <- list( "vertices" = vertices , "edges" = edges ,
                        "edges_strength" = edges_strength ,
                        "points_covered_by_landmarks" = points_covered_by_landmarks,
                        "landmarks" = landmarks , "coloring" = coloring ,
                        "coverage" = coverage )

   return(return_list)
}#readBallMapperGraphFromFile




#'Produce a new coloring vector being an average of values of given function at points covererd by each vertex of Ball Mapper graph.
#'
#'@param outputFromBallMapper an output from the BallMapper function
#'@param newFunctionOnPoints values of function on points.
#'@return Vector of function values on vertices on Ball Mapper graph.
#' var <- seq(from=0,to=6.3,by=0.1)
#' points <- as.data.frame( cbind( sin(var),cos(var) ) )
#' values <- as.data.frame(sin(var))
#' l <- BallMapper(points, values, 0.25)
#' ColorIgraphPlot(l)
#' new_coloring <- colorByAverageValueOfOtherVariable(l,cos(var))
#' l$coloring <- new_coloring
#' ColorIgraphPlot(l)
#'@export
colorByAverageValueOfOtherVariable<- function( outputFromBallMapper , newFunctionOnPoints )
{
    newColoring <- vector( length=length(outputFromBallMapper$points_covered_by_landmarks) )
    #for every landmark point
    for ( land in 1:length(outputFromBallMapper$points_covered_by_landmarks))
    {
      #average the value of newFunctionOnPoints for all points covered by that landmark
      average <- 0
      for ( coveredPoint in 1:length( outputFromBallMapper$points_covered_by_landmarks[[land]] ) )
      {
        average <- average + newFunctionOnPoints[ outputFromBallMapper$points_covered_by_landmarks[[land]][coveredPoint ] ]
      }
      average <- average/length( outputFromBallMapper$points_covered_by_landmarks[[land]] )
      newColoring[land] <- average
    }
    return(newColoring)
}#colorByAverageValueOfOtherVariable





#'Produce a new coloring vector being a standard deviation of values of given
#'function at points covererd by each vertex of Ball Mapper graph.
#'
#'@param outputFromBallMapper an output from the BallMapper function
#'@param newFunctionOnPoints values of function on points.
#'@return Vector of function values on vertices on Ball Mapper graph.
#' var <- seq(from=0,to=6.3,by=0.1)
#' points <- as.data.frame( cbind( sin(var),cos(var) ) )
#' values <- as.data.frame(sin(var))
#' l <- BallMapper(points, values, 0.25)
#' ColorIgraphPlot(l)
#' new_coloring <- colorByStDevValueOfOtherVariable(l,sin(var))
#' l$coloring <- new_coloring
#' ColorIgraphPlot(l)
#'@export
colorByStDevValueOfOtherVariable<- function( outputFromBallMapper , newFunctionOnPoints )
{
  newColoring <- vector( length=length(outputFromBallMapper$points_covered_by_landmarks) )
  #for every landmark point
  for ( land in 1:length(outputFromBallMapper$points_covered_by_landmarks))
  {
    #average the value of newFunctionOnPoints for all points covered by that landmark
    v <- vector()
    for ( coveredPoint in 1:length( outputFromBallMapper$points_covered_by_landmarks[[land]] ) )
    {
      v <- c( v , newFunctionOnPoints[ outputFromBallMapper$points_covered_by_landmarks[[land]][coveredPoint ] ] )
    }
    newColoring[land] <- stats::sd(v)
  }
  return(newColoring)
}#colorByStDevValueOfOtherVariable
