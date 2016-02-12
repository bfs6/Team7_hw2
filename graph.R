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

is_undirected = function(g){
  if (is_valid(g)==FALSE){
    return(FALSE)
  }
  j=adj_matrix(g)
  q=t(adj_matrix(g))
  if (j!=q){
    return (FALSE)
  }else{return (TRUE)}
  }

is_isomorphic = function(g1, g2){ 
  if(is_valid(g1)==FALSE & is_valid(g2)==FALSE){
    return (FALSE)
  }
  else if(sorted(names(g1))==sorted(names(g2))){
    adj1<-adj_matrix(g1)
    adj2<-adj_matrix(g2)
    list1<-lapply(seq_len(nrow(adj1)), function(i) sort(adj1[i,]))
    list2<-lapply(seq_len(nrow(adj2)), function(i) sort(adj2[i,]))
    
    return (prod(list2 %in% list1)==1)
  }else{
    return (FALSE)
  }
}


is_connected=function(g, v1, v2){
  v1exist=FALSE
  v2exist=FALSE
  indexv1=0
  indexv2=0
  
  #check if is character not number and not array 
  if(is.character(v1)==FALSE | length(v1)>1){
    return(FALSE)
  }
  
  if(is.character(v2)==FALSE | length(v2)>1){
    return(FALSE)
  }
  #check if v1 and v2 exissts in the graph
  for(i in 1:length(g)){
    if(names(g[i]) == v1){
      v1exist=TRUE
    }
    
    if(names(g[i]) == v2){
      v2exist=TRUE
    }
  }
  
  if(v1exist == FALSE|v2exist == FALSE){
    return(FALSE)
  }
  #find index of the v1 and v2 on the graph/adjmatrix
  for(i in 1:length(g)){
    if(names(g[i]) == v1){
      indexv1 = i
    }
    if(names(g[i]) == v2){
      indexv2 = i      
    }
  }
  #check if same node is conected
  if(v1 == v2){
    if(length(g[[indexv1]]$edges) == 0){ 
      return(FALSE)
    } 
    else {
      for (i in 1:length(g[[indexv1]]$edges)){
        if(g[[indexv1]]$edges[i] == indexv2 ){
          return(TRUE)
        } 
        else {
          return(FALSE)
        }
      }
    }
  }
  
  
  visited=list()
  visited=c(visited, indexv1)
  k=adj_matrix(g)
  n=length(g)
  while(n>0){
    for (j in 1:length(visited)){
      for ( i in 1:length(g)){
        if (k[visited[[j]], i]>0){
          visited=c(visited, i)
        }
      }
    }
    n=n-1
  }
  
  for( i in 1:length(visited)){
    if (visited[[i]]==indexv2)
      return(TRUE)
  }
  return(FALSE)
  
  
  
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
