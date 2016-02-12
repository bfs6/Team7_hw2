is_valid = function(g) {
  #Remark: in a valid graph, some vertices have empty vectors for edges and vertices if they don't point to anywhere
  #Check that g is a list of lists
  v = c()
  if (typeof(g) == "list"){ 
    v = c(v,TRUE)
    }
  else {
    v = c(v,FALSE)
    }
  
  for (i in g){
    if (typeof(i) == "list"){
      v = c(v,TRUE)
      }
    else{
      v = c(v,FALSE)
      }
  }
  
  #Check that the list names are unique
  if (nlevels(factor(names(g))) == length(factor(names(g)))){ #factor takes out repetitive elements.
    v = c(v, TRUE)
  }
  else {
    v = c(v, FALSE)
  }
  
  #Check that each secondary list contains only edges and weights vectors that are of the appropriate type.
  for (i in 1:length(g)){
    if (typeof(g[[i]][[1]])=="integer" | is.null(g[[i]][[1]])){ #did not check every element in the secondary list because of the coercion hierarchy: character>complex>double>integer>logical
      v = c(v, TRUE)
    }
    else {
      v = c(v, FALSE)
      #print(i)
    }
    if(is.null(g[[i]][[2]])){v=c(v,TRUE)}
    else{
      for (x in g[[i]][[2]]){
        if(typeof(x)=="double"){
          v=c(v,TRUE)
        }else{
          v=c(v,FALSE) 
          #print(x)
        }
      }
    }
  }
  
  # Check that there are not any edges to non-existent vertices
  for (i in 1:length(g)){
    if (prod(g[[i]][[1]]<=length(g)) == 1){ #don't need to specify if vector is empty, prod(NULL)==1
      v = c(v, TRUE)}
    else {
      v = c(v, FALSE)
    }
  }
  
  # Check that all weights are greater than 0.
  for (i in 1:length(g)){
    if (prod(g[[i]][[2]]>0) == 1){
      v = c(v, TRUE)
    }
    else {
      v = c(v, FALSE)
    }
  }
  
  print(v)
  if(prod(v)==1){
    return(TRUE)
  }else{
  return(FALSE)
  }
  
}
is_valid(graph1)
is_valid(graph2)

is_undirected = function(g)
{
  return(FALSE)
}


is_isomorphic = function(g1, g2)
{
  return(FALSE)
}


is_connected = function(g, v1, v2)
{
  return(character())
}


shortest_path = function(g, v1, v2)
{
  return(character())
}

adj_matrix <- function(g)
{
  stopifnot(is_valid(g))
  
  m = matrix(0,nrow=length(g), ncol=length(g))
  rownames(m) = names(g)
  colnames(m) = names(g)
  for (j in seq_along(g))
  {
    node = g[[j]]
    
    
    for(i in seq_along(node$edges))
    {
      m[j, node$edges[i]] = node$weights[i]
    }
  }
  return(m)
}

graph1 = list(A = list(edges   = c(2L),
                       weights = c(1 )),
              B = list(edges   = c(3L),
                       weights = c(1 )),
              C = list(edges   = c(5L),
                       weights = c(1 )),
              D = list(edges   = c(2L),
                       weights = c(1 )),
              E = list(edges   = c(4L,6L),
                       weights = c(1,1)),
              F = list(edges   = c(),
                       weights = c())
)
graph2 = list(A = list(edges   = c(2L),
                       weights = c(14)),
              B = list(edges   = c(3L,4L),
                       weights = c(23,13)),
              D = list(edges   = c(1L),
                       weights = c(5) ),
              F = list(edges   = c(1L,5L),
                       weights = c(43,33)),
              N = list(edges   = c(1L,2L,4L),
                       weights = c(33,22,11))
)
