is_valid = function(g)
{
    return(TRUE)
}


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
