is_valid = function(g) {
  #Remark: in a valid graph, some vertices have empty vectors for edges and vertices if they don't point to anywhere
  #1. Check that g is a list of lists
  if(length(g)<1){
    #print(1) 
    return (FALSE)
  }else if(!is.list(g)){
      #print(1.1) 
    return(FALSE)
  }else if(prod(sapply(g,is.list))!=1){
      #print(1.2) 
    return (FALSE)
    }
  #2. Missing labels
  if(is.null(names(g))){
    #print(2) 
    return(FALSE)
  }else if(TRUE %in% (sapply(names(g),function(x){x==""}))){
      #print(2.1) 
    return (FALSE)
  }
  #missing labels also caught all the testing cases for invalid structure
  #3. invalid structure
  if(mean(sapply(g,length))!=2){
    #print(3) 
    return (FALSE)
    }
  #4. duplicate labels
  if(length(names(g))!=length(unique(names(g)))){
    #print(4)
    return(FALSE)
  }
  #5. invalid vertex reference
  if(!FALSE %in% sapply(g,function(x){length(x$edges)>0 & length(x$weights)>0}) & ! NA %in% unlist(g)){
    if(max(sapply(g,function(x){max(x$edges)})) > length(g)){
      #print(5)
      return (FALSE)
    }
  }
  #6. duplicate vertex
  if(length(unique(names(g))) != length(g)){
    #print(6) 
    return (FALSE)
    }
  #7. edge type:
  for(i in g){
    if (FALSE %in% (sapply(i$edges,is.integer)) | NA %in% i$edges){
      #print("bad edge")
      return (FALSE)
      }
  }
  #the test cases returns as false but I don't think they made it this far
  #8. Weight type and value
  for(i in g){
    if (FALSE %in% (sapply(i$weights,is.double)) | NA %in% i$weights | FALSE %in% (sapply(i$weights,function(x){x>0}))){
      #print("bad weight")
      return (FALSE)
    }
  }
  #9. duplicate edges
  if(TRUE %in% sapply(g,function(x){duplicated(x$edges)})){
    #print(9) 
    return (FALSE)
    }
  #10 edge and weight lenth mismatch
  if(FALSE %in% sapply(g,function(x){length(x$edges) == length(x$weights)})){
    #print(10) 
    return (FALSE)
    }
  #if all cases passed then the graph is valid
  return (TRUE)
}


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



graph1 = list(list(edges   = c(2L),
                       weights = c(1 )),
              A=list(edges   = c(3L),
                       weights = c(1 )),
              list(edges   = c(5L),
                       weights = c(1 )),
              list(edges   = c(2L),
                       weights = c(1 )),
              list(edges   = c(4L,6L),
                       weights = c(1,1  )),
              list(edges   = c(),
                       weights = c())
)

str(graph1)