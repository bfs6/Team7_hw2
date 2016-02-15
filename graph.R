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
    stop('error')
  }
  j=adj_matrix(g)
  for (i in 1:length(g)){
    for (k in i:length(g)){
      if (j[i,k]!=j[k,i]){
        return (FALSE)
      }
    }
  }
  return(TRUE)
}

is_isomorphic = function(g1, g2){ 
  if(is_valid(g1)==FALSE & is_valid(g2)==FALSE){
    stop ("error")
  }
  if (length(g1)!=length(g2)){
    return(FALSE)
  }
  if(length(setdiff(names(g1), names(g2)))!=0 && length(setdiff(names(g2), names(g1)))!=0){
    return (FALSE)
  }
  a=adj_matrix(g1)
  b=adj_matrix(g2)
  for(i in names(g1)){
    for(j in names(g1)){
      if (a[i,j]!=b[i,j]){
        return (FALSE)
      }
    }
  }
  return (TRUE)
}

is_connected=function(g, v1, v2){
  v1exist=FALSE
  v2exist=FALSE
  indexv1=0
  indexv2=0
  if (is_valid(g)==FALSE){
    stop("error")
  }
  #check if is vertex not number and not array 
  if(is.character(v1)==FALSE | length(v1)>1){
    stop("error")
  }
  if(is.character(v2)==FALSE | length(v2)>1){
    stop("error")
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
    stop("error")
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

  
  if(length(g[[indexv1]]$edges) == 0){ 
    return(FALSE)
  } 
  
  else {
    queue=integer()
    visited=integer()
    
    
    for (values in g[[indexv1]]$edges){
      if(!values%in%visited){
        queue=c(queue, values)
        
      }
    }
    
    
  if(v1 == v2){
    if(length(g[[indexv1]]$edges) == 0){ 
      return(FALSE)
    } 
    else if (indexv1 %in% g[[indexv1]]$edges){
      return (TRUE)
    }
    else{return (FALSE)}
  }
  else if(v1 != v2){
    queue=integer()
    visited=integer()
    queue=c(queue, indexv1)
    while(length(queue)>0){
      pop=queue[1]
      queue=queue[-1]
      if(!pop%in% visited){
        visited=c(visited, pop)
        for (values in g[[pop]]$edges){
          if(!values%in%visited){
            queue=c(queue, values)
            #print(queue)
          }
        }
      }
    }
    #print(visited)
    for( i in 1:length(visited)){
      if (visited[i]==indexv2)
        return(TRUE)
    }
    return(FALSE)
  }
}


shortest_path = function(g, v1, v2)
{
  indexv1=0
  indexv2=0
  
  for(i in 1:length(g)){
    if(names(g[i]) == v1){
      indexv1 = i
    }
    if(names(g[i]) == v2){
      indexv2 = i      
    }
  }
  
  k=is_connected(g, v1, v2)
  if(k==FALSE){
    return(character())
  }
  answer=integer()
  
  
  
  if (v1==v2){
    
    shorestindexbefore=0
    dij=dijkstras(g, indexv1)
    dij[indexv1]=0
    
    new=integer(length(g))
    
    for ( i in 1:length(g)){
      
      for (k in 1:length(g[[i]]$edges)){
        if(g[[i]]$edges[k]==indexv1){ #if it connects back to v1
          new[i]=dij[i]+g[[i]]$weights[k]
        }
        
      }
      
    }
    
    
    new=replace(new, new==0, Inf)
    
    min=min(new)
    for (i in 1:length(g)){
      if (new[i]==min){
        answer=c(path(g, indexv1, i), indexv1)
        break
      }
    }
    
    
  }
  if(v1!=v2){
    answer=path(g, indexv1, indexv2)
    
  }
  
  final=character()
  for (i in 1:length(answer)){
    final=c(final, names(g)[answer[i]])
  }
  return(final)
}






path=function(g, v1, v2){
  m=matrix(Inf, nrow=length(g), ncol=length(g))
  distance=integer()
  visited=integer()
  path=integer()
  nextNode=v1
  min=0
  
  adj=adj_matrix(g)
  for (i in 1:length(g)){
    visited[i]=0
    path[i]=v1
    for(k in 1:length(g)){
      if (adj[i,k]==0){
        adj[i,k]=Inf
      }
    }
  }
  
  
  distance=adj[v1,]
  distance[0]=0
  visited[v1]=1
  
  for ( i in 1:length(g)){
    min=Inf
    for (j in 1:length(g)){
      if(min>distance[j] && visited[j]!=1){
        min=distance[j]
        nextNode=j
      }
    }
    visited[nextNode]=1;
    for (c in 1:length(g)){
      if(visited[c]!=1){
        
        
        if(min+adj[nextNode,c]<distance[c]){
          distance[c]=min+adj[nextNode,c]
          path[c]=nextNode
        }
      }
    }
  }
  
  answer=c(v1)
  if (v1!=v2){
    
    answer=integer()
    
    index=v2
    answer=c(answer,index)
    repeat{
      
      index=path[index]
      answer=c(answer, index)
      
      if(index==v1){
        
        break
      }
    }
    answer=rev(answer)
    
  }
  return(answer)
}




dijkstras=function(g, v1){
  m=matrix(Inf, nrow=length(g), ncol=length(g))
  distance=integer()
  visited=integer()
  
  nextNode=v1
  min=0
  
  adj=adj_matrix(g)
  for (i in 1:length(g)){
    visited[i]=0
    
    for(k in 1:length(g)){
      if (adj[i,k]==0){
        adj[i,k]=Inf
      }
    }
  }
  
  
  distance=adj[v1,]
  distance[0]=0
  visited[v1]=1
  
  for ( i in 1:length(g)){
    min=Inf
    for (j in 1:length(g)){
      if(min>distance[j] && visited[j]!=1){
        min=distance[j]
        nextNode=j
      }
    }
    visited[nextNode]=1;
    for (c in 1:length(g)){
      if(visited[c]!=1){
        if(min+adj[nextNode,c]<distance[c]){
          distance[c]=min+adj[nextNode,c]
          
        }
      }
    }
  }
  
  
  return(distance)
  
}





adj_matrix <- function(g)
{
  
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

