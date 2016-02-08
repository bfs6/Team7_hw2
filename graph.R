is_valid = function(g) {
  #Remark: in a valid graph, some vertices have empty vectors for edges and vertices if they don't point to anywhere
  #Check that g is a list of lists
  v = c()
  v=c(v,is.list(g))
  
  for (i in g){
    v=c(v,is.list(i))
  }
  
  #Check that the list names are unique
  v=c(v,nlevels(factor(names(g))) == length(factor(names(g))))
  
  #Check that each secondary list contains only edges and weights vectors that are of the appropriate type.
  for (i in 1:length(g)){
    v=c(v,is.integer(g[[i]][[1]])|is.null(g[[i]][[1]]))  
    if(is.null(g[[i]][[2]])){v=c(v,TRUE)}
    else{
      for (x in g[[i]][[2]]){
        v=c(v,is.double(x))
      }
    }
  }
  
  # Check that there are not any edges to non-existent vertices
  for (i in 1:length(g)){
    v=c(v,prod(g[[i]][[1]]<=length(g)) == 1)
  }
  
  # Check that all weights are greater than 0.
  for (i in 1:length(g)){
    v=c(v,prod(g[[i]][[2]]>0) == 1)
  }
  
  print(v)
  return(prod(v)==1)
}
is_valid(graph1)
is_valid(graph2)

  
  #Check that the list names are unique
  if (nlevels(factor(names(g))) == length(factor(names(g)))){
    v = c(v, TRUE)
  }
  else {
    v = c(v, FALSE)
  }
  
  #Check that 
  
  
  
  
    return(TRUE)
}
typeof(graph1)

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
                       weights = c(1,1  )),
              E = list(edges   = c(),
                       weights = c())
)

names(graph1)[1]
