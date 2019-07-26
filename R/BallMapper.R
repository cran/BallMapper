library(igraph)
library(scales)
library(networkD3)

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
#'@return None
#'@examples
#' var <- seq(from=0,to=6.3,by=0.1)
#' points <- as.data.frame( cbind( sin(var),cos(var) ) )
#' values <- as.data.frame( sin(var) )
#' epsilon <- 0.25
#' l <- BallMapper(points,values,epsilon)
#' ColorIgraphPlot(l)
#' @export
ColorIgraphPlot <- function( outputFromBallMapper, showVertexLabels = TRUE , showLegend = FALSE , minimal_ball_radius = 7 , maximal_ball_scale=20, maximal_color_scale=10 )
{
  #this command sents up a fancy background, remove if not needed.
  #par(bg="grey32", mar=c(0,0,0,0))
  vertices = outputFromBallMapper$vertices
  vertices[,2] <- maximal_ball_scale*vertices[,2]/max(vertices[,2])+minimal_ball_radius
  net = igraph::graph_from_data_frame(outputFromBallMapper$edges,vertices = vertices,directed = F)

  len <- length( unique(outputFromBallMapper$coloring) )
  jet.colors <- grDevices::colorRampPalette(c("red","orange","yellow","green","cyan","blue","violet"))
  color_spectrum <- jet.colors( len )
  #and over here we map the pallete to the order of values on vertices
  ordered <- order(outputFromBallMapper$coloring)
  color <- vector(length = length(ordered),mode="double")
  for ( i in 1:length(ordered) )
  {
    color[ ordered[i] ] <- color_spectrum [ i ]
  }
  igraph::V(net)$color <- color

  if ( showVertexLabels == FALSE  )igraph::V(net)$label = NA
  graphics::plot(net)

  fields::image.plot(legend.only=T, zlim=range(outputFromBallMapper$coloring), col=color_spectrum )
}#ColorIgraphPlot


#'Produce a static grayscale visualization of the Ball Mapper graph. It is based on the output from the BallMapper function.
#'
#'@param outputFromBallMapper, an output from the BallMapper function
#'@param showVertexLabels, a boolean value determining if vertex labels are to be shown (TRUE by default).
#'@param minimal_ball_radius, provide a minimal value of the radius of balls used in visualization (7 by default).
#'@param maximal_ball_scale, provides a maximal value of the radius of the balls used in visualization (20 by default).
#'@return None
#'@examples
#' var <- seq(from=0,to=6.3,by=0.1)
#' points <- as.data.frame( cbind( sin(var),cos(var) ) )
#' values <- as.data.frame( sin(var) )
#' epsilon <- 0.25
#' l <- BallMapper(points,values,epsilon)
#' GrayscaleIgraphPlot(l)
#' @export
GrayscaleIgraphPlot <- function( outputFromBallMapper , showVertexLabels = TRUE , minimal_ball_radius = 7 , maximal_ball_scale=20 )
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
  graphics::plot(net)
}#GrayscaleIgraphPlot

#'Produce a two column list. The first column contain the number of point (possibly with repetitions), the second one contains the number of landmark points that cover it.
#'For example, let us assume that point 1 is covered by landmark 1 and 2, and point 2 is covered by the landmark 2. In this case the obtained list is of a form:
#'1 1
#'1 2
#'2 2
#'
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
