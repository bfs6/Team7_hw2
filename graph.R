is_valid = function(g)
{
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
