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
context("Test is_valid")


test_that("Check arg lists", {
  expect_equal(names(formals(is_valid)), c("g"))
})


test_that("valid graphs", {
  
  g1 = list(A = list(edges   = c(1L, 2L),
                     weights = c(1, 1)),
            B = list(edges   = c(1L, 2L),
                     weights = c(1, 1)))
  
  g2 = list(A = list(edges   = c(2L),
                     weights = c(1)),
            B = list(edges   = c(1L),
                     weights = c(1)))
  
  g3 = list(A = list(edges   = integer(),
                     weights = numeric()))
  
  g4 = list(A = list(edges   = c(1L),
                     weights = c(1)))
  
  g5 = list(B = list(weights = c(1),
                     edges   = c(1L)))
  
  g6 = list(A = list(edges   = c(2L),
                     weights = c(1)),
            B = list(edges   = integer(),
                     weights = numeric()))
  
  g7 = list(A = list(edges   = c(1L,2L),
                     weights = c(1,1)),
            B = list(edges   = c(2L),
                     weights = c(1)))
  
  g8 = list(A = list(weights = c(1),
                     edges   = c(1L)))
  
  g9 = list("123" = list(edges   = c(2L),
                         weights = c(1)),
            "ABC" = list(edges   = c(1L),
                         weights = c(1)))
  
  expect_true(is_valid(g1))
  expect_true(is_valid(g2))
  expect_true(is_valid(g3))
  expect_true(is_valid(g4))
  expect_true(is_valid(g5))
  expect_true(is_valid(g6))
  expect_true(is_valid(g7))
})

test_that("Missing labels", {
  bad_g1 = list(list(edges   = c(1L, 2L),
                     weights = c(1, 1)))
  bad_g2 = list(list(edges   = c(1L, 2L),
                     weights = c(1, 1)),
                list(edges   = c(1L, 2L),
                     weights = c(1, 1)))
  
  expect_false(is_valid(bad_g1))
  expect_false(is_valid(bad_g2))
})

test_that("Bad structure", {
  bad_g0 = list()
  bad_g1 = list(list())
  bad_g2 = list(list(edges = 1L))
  bad_g3 = list(list(weights = 1))
  
  
  expect_false(is_valid(bad_g0))
  expect_false(is_valid(bad_g1))
  expect_false(is_valid(bad_g2))
  expect_false(is_valid(bad_g3))
})


test_that("Invalid vertex reference", {
  bad_g1 = list(A = list(edges   = c(1L, 2L),
                         weights = c(1, 1)))
  bad_g2 = list(A = list(edges   = c(1L, 2L),
                         weights = c(1, 1)),
                B = list(edges   = c(1L, 3L),
                         weights = c(1, 1)))
  
  expect_false(is_valid(bad_g1))
  expect_false(is_valid(bad_g2))
})


test_that("Duplicate vertex labels", {
  bad_g = list(A = list(edges   = c(1L, 2L),
                        weights = c(1, 1)),
               A = list(edges   = c(1L, 2L),
                        weights = c(1, 1)))
  
  expect_false(is_valid(bad_g))
})


test_that("Edge type", {
  bad_g1 = list(A = list(edges   = c(1),
                         weights = c(1)))
  bad_g2 = list(A = list(edges   = c("A"),
                         weights = c(1)))
  bad_g3 = list(A = list(edges   = c(NA+1L),
                         weights = c(1)))
  
  expect_false(is_valid(bad_g1))
  expect_false(is_valid(bad_g2))
  expect_false(is_valid(bad_g3))
})


test_that("Weight type and value", {
  bad_g1 = list(A = list(edges   = c(1L),
                         weights = c(-1)))
  bad_g2 = list(A = list(edges   = c(1L),
                         weights = c(0)))
  bad_g3 = list(A = list(edges   = c(1L),
                         weights = c(NA+1)))
  
  expect_false(is_valid(bad_g1))
  expect_false(is_valid(bad_g2))
  expect_false(is_valid(bad_g3))
})


test_that("Duplicated edges", {
  bad_g = list(A = list(edges   = c(1L, 1L),
                        weights = c(1, 1)))
  
  expect_false(is_valid(bad_g))
})


test_that("Edge and weight length mismatch", {
  bad_g1 = list(A = list(edges   = c(1L),
                         weights = c(1, 1)))
  bad_g2 = list(A = list(edges   = c(1L, 2L),
                         weights = c(1)))
  bad_g3 = list(A = list(edges   = c(1L, 2L),
                         weights = c(1, 1)),
                B = list(edges   = c(1L, 2L),
                         weights = c(1, 1, 1)))
  bad_g4 = list(A = list(edges   = c(1L, 2L),
                         weights = c(1, 1)),
                B = list(edges   = c(1L, 2L, 3L),
                         weights = c(1, 1)))
  
  expect_false(is_valid(bad_g1))
  expect_false(is_valid(bad_g2))
  expect_false(is_valid(bad_g3))
  expect_false(is_valid(bad_g4))
})

context("Test is_undirected")

test_that("Check arg lists", {
  expect_equal(names(formals(is_undirected)), c("g"))
})


test_that("Bad graphs", {
  bad_g1 = list(A = list())
  bad_g2 = list(A = list(edges = 1L))
  bad_g3 = list(A = list(weights = 1))
  bad_g4 = list(list(weights = 1, edges = 1L))
  
  expect_error(is_undirected(bad_g1))
  expect_error(is_undirected(bad_g2))
  expect_error(is_undirected(bad_g3))
  expect_error(is_undirected(bad_g4))
})


test_that("Undirected",{
  g0 = list(A = list(edges   = integer(),
                     weights = numeric()))
  
  g1 = list(A = list(edges   = c(1L),
                     weights = c(1)))
  
  g2 = list(A = list(edges   = integer(),
                     weights = numeric()),
            B = list(edges   = integer(),
                     weights = numeric()))
  
  g3 = list(A = list(edges   = c(2L),
                     weights = c(1)),
            B = list(edges   = c(1L),
                     weights = c(1)))
  
  g4 = list(A = list(edges   = c(1L),
                     weights = c(1)),
            B = list(edges   = c(2L),
                     weights = c(1)))
  
  g5 = list(A = list(edges   = c(1L,2L),
                     weights = c(1,1)),
            B = list(edges   = c(1L),
                     weights = c(1)))
  
  g6 = list(A = list(edges   = c(1L,2L),
                     weights = c(1,1)),
            B = list(edges   = c(1L,2L),
                     weights = c(1,1)))
  
  g7 = list(A = list(edges   = c(2L,4L),
                     weights = c(1,1)),
            B = list(edges   = c(1L),
                     weights = c(1)),
            C = list(edges   = c(4L),
                     weights = c(1)),
            D = list(edges   = c(1L,3L),
                     weights = c(1,1)))
  
  expect_true(is_undirected(g0))
  expect_true(is_undirected(g1))
  expect_true(is_undirected(g2))
  expect_true(is_undirected(g3))
  expect_true(is_undirected(g4))
  expect_true(is_undirected(g5))
  expect_true(is_undirected(g6))
  expect_true(is_undirected(g7))
})

test_that("Directed - Edges",{
  g1 = list(A = list(edges   = c(2L),
                     weights = c(1)),
            B = list(edges   = integer(),
                     weights = numeric()))
  
  g2 = list(A = list(edges   = c(1L,2L),
                     weights = c(1,1)),
            B = list(edges   = c(2L),
                     weights = c(1)))
  
  g3 = list(A = list(edges   = integer(),
                     weights = numeric()),
            B = list(edges   = 1L,
                     weights = 1))
  
  expect_false(is_undirected(g1))
  expect_false(is_undirected(g2))
  expect_false(is_undirected(g3))
})

test_that("Directed - Weights",{
  g1 = list(A = list(edges   = c(2L),
                     weights = c(1)),
            B = list(edges   = c(1L),
                     weights = c(2)))
  
  g2 = list(A = list(edges   = c(1L,2L),
                     weights = c(1,1)),
            B = list(edges   = c(1L,2L),
                     weights = c(2,1)))
  
  expect_false(is_undirected(g1))
  expect_false(is_undirected(g2))
})

context("Test is_isomorphic")


test_that("Check arg lists", {
  expect_equal(names(formals(is_isomorphic)), c("g1","g2"))
})


test_that("Invalid graphs", {
  g = list(A = list(edges   = 1L,
                    weights = 1 ))
  
  #bad_g1 = list(A = list())
  #bad_g2 = list(A = list(edges = 1L))
  bad_g3 = list(A = list(weights = 1))
  bad_g4 = list(list(weights = 1, edges = 1L))
  
  #expect_error(is_isomorphic(g,bad_g1))
  #expect_error(is_isomorphic(g,bad_g2))
  expect_error(is_isomorphic(g,bad_g3))
  expect_error(is_isomorphic(g,bad_g4))
  
  #expect_error(is_isomorphic(bad_g1,g))
  #expect_error(is_isomorphic(bad_g2,g))
  expect_error(is_isomorphic(bad_g3,g))
  expect_error(is_isomorphic(bad_g4,g))
  
  
  #expect_error(is_isomorphic(bad_g1,bad_g1))
  #expect_error(is_isomorphic(bad_g2,bad_g2))
  expect_error(is_isomorphic(bad_g3,bad_g3))
  expect_error(is_isomorphic(bad_g4,bad_g4))
})

test_that("Valid graphs", {
  g1 = list(A = list(edges   = integer(),
                     weights = numeric()))
  
  g2 = list(A = list(edges   = 1L,
                     weights = 1 ))
  
  g3 = list(A = list(edges   = 2L,
                     weights = 1 ),
            B = list(edges   = 1L,
                     weights = 1 ))
  
  g4 = list(B = list(edges   = 2L,
                     weights = 1 ),
            A = list(edges   = 1L,
                     weights = 1 ))
  
  g5 = list(A = list(edges   = c(1L,2L),
                     weights = c(1 ,1 )),
            B = list(edges   = 1L,
                     weights = 1 ))
  
  g6 = list(B = list(edges   = 2L,
                     weights = 1 ),
            A = list(edges   = c(1L,2L),
                     weights = c(1, 1 )))
  
  expect_true(is_isomorphic(g1,g1))
  expect_true(is_isomorphic(g2,g2))
  expect_true(is_isomorphic(g3,g3))
  expect_true(is_isomorphic(g4,g4))
  expect_true(is_isomorphic(g5,g5))
  expect_true(is_isomorphic(g6,g6))
  
  expect_true(is_isomorphic(g3,g4))
  expect_true(is_isomorphic(g4,g3))
  expect_true(is_isomorphic(g5,g6))
  expect_true(is_isomorphic(g6,g5))
  
  expect_false(is_isomorphic(g1,g2))
  expect_false(is_isomorphic(g3,g5))
  expect_false(is_isomorphic(g3,g6))
  expect_false(is_isomorphic(g4,g5))
  expect_false(is_isomorphic(g4,g6))
  
  g3[[1]]$weights = 2
  expect_true( is_isomorphic(g3,g3))
  expect_false(is_isomorphic(g3,g4))
  expect_false(is_isomorphic(g4,g3))
})

test_that("Edge ordering", {
  
  g1 = list(A = list(edges   = c(1L,2L),
                     weights = c(1, 2 )),
            B = list(edges   = 1L,
                     weights = 1 ))
  
  g2 = list(A = list(edges   = c(2L,1L),
                     weights = c(2, 1 )),
            B = list(edges   = 1L,
                     weights = 1 ))
  
  g3 = list(B = list(edges   = 2L,
                     weights = 1 ),
            A = list(edges   = c(1L,2L),
                     weights = c(2, 1 )))
  
  g4 = list(B = list(edges   = 2L,
                     weights = 1 ),
            A = list(edges   = c(2L,1L),
                     weights = c(1, 2 )))
  
  bad_g1 = list(A = list(edges   = c(1L,2L),
                         weights = c(2, 1 )),
                B = list(edges   = 1L,
                         weights = 1 ))
  
  bad_g2 = list(B = list(edges   = 2L,
                         weights = 1 ),
                A = list(edges   = c(1L,2L),
                         weights = c(1, 2 )))
  
  
  expect_true(is_isomorphic(g1,g2))
  expect_true(is_isomorphic(g1,g3))
  expect_true(is_isomorphic(g1,g4))
  expect_true(is_isomorphic(g2,g3))
  expect_true(is_isomorphic(g2,g4))
  expect_true(is_isomorphic(g3,g4))
  
  expect_false(is_isomorphic(g1,bad_g1))
  expect_false(is_isomorphic(g2,bad_g1))
  expect_false(is_isomorphic(g3,bad_g1))
  expect_false(is_isomorphic(g4,bad_g1))
  
  expect_false(is_isomorphic(g1,bad_g2))
  expect_false(is_isomorphic(g2,bad_g2))
  expect_false(is_isomorphic(g3,bad_g2))
  expect_false(is_isomorphic(g4,bad_g2))
  
  expect_true(is_isomorphic(bad_g1,bad_g2))
})

test_that("Subsets", {
  g1 = list(A = list(edges   = integer(),
                     weights = numeric()  ))
  
  g2 = list(A = list(edges   = integer(),
                     weights = numeric()  ),
            B = list(edges   = 1L,
                     weights = 1  ))
  
  expect_false(is_isomorphic(g1,g2))
  expect_false(is_isomorphic(g2,g1))
})

context("Test is_connected")


test_that("Check arg lists", {
  expect_equal(names(formals(is_connected)), c("g","v1","v2"))
})


test_that("Connected graphs", {
  g1 = list(A = list(edges   = c(1L),
                     weights = c(1)))
  
  g2 = list(A = list(edges   = c(1L, 2L),
                     weights = c(1, 1)),
            B = list(edges   = c(1L, 2L),
                     weights = c(1, 1)))
  
  g3 = list(A = list(edges   = c(1L, 2L),
                     weights = c(1, 1)),
            B = list(edges   = c(1L, 3L),
                     weights = c(1, 1)),
            C = list(edges   = c(2L, 3L),
                     weights = c(1, 1)))
  
  g4 = list(A = list(edges   = c(2L),
                     weights = c(1)),
            B = list(edges   = c(3L),
                     weights = c(1)),
            C = list(edges   = c(4L),
                     weights = c(1)),
            D = list(edges   = c(5L),
                     weights = c(1)),
            E = list(edges   = c(6L),
                     weights = c(1)),
            F = list(edges   = integer(),
                     weights = numeric()))
  
  
  expect_true(is_connected(g1,"A","A"))
  
  expect_true(is_connected(g2,"A","A"))
  expect_true(is_connected(g2,"A","B"))
  expect_true(is_connected(g2,"B","A"))
  expect_true(is_connected(g2,"B","B"))
  
  expect_true(is_connected(g3,"A","A"))
  expect_true(is_connected(g3,"A","B"))
  expect_true(is_connected(g3,"B","A"))
  expect_true(is_connected(g3,"B","C"))
  expect_true(is_connected(g3,"C","B"))
  expect_true(is_connected(g3,"C","C"))
  
  
  expect_true(is_connected(g4,"A","B"))
  expect_true(is_connected(g4,"A","C"))
  expect_true(is_connected(g4,"A","D"))
  expect_true(is_connected(g4,"A","E"))
  expect_true(is_connected(g4,"A","F"))
  
  expect_false(is_connected(g4,"F","A"))
  expect_false(is_connected(g4,"F","B"))
  expect_false(is_connected(g4,"F","C"))
  expect_false(is_connected(g4,"F","D"))
  expect_false(is_connected(g4,"F","E"))
})


test_that("Unconnected graphs", {
  g1 = list(A = list(edges   = integer(),
                     weights = numeric()))
  
  g2 = list(A = list(edges   = c(1L),
                     weights = c(1)),
            B = list(edges   = c(2L),
                     weights = c(1)))
  
  g3 = list(A = list(edges   = c(1L),
                     weights = c(1)),
            B = list(edges   = c(1L, 3L),
                     weights = c(1, 1)),
            C = list(edges   = c(3L),
                     weights = c(1)))
  
  
  expect_false(is_connected(g1,"A","A"))
  
  expect_true( is_connected(g2,"A","A"))
  expect_true( is_connected(g2,"B","B"))
  expect_false(is_connected(g2,"A","B"))
  expect_false(is_connected(g2,"B","A"))
  
  expect_true( is_connected(g3,"A","A"))
  expect_false(is_connected(g3,"A","B"))
  expect_true( is_connected(g3,"B","A"))
  expect_false(is_connected(g3,"B","B"))
  expect_true( is_connected(g3,"B","C"))
  expect_false(is_connected(g3,"C","B"))
  expect_true( is_connected(g3,"C","C"))
})

test_that("Vertex labels", {
  g1 = list(A = list(edges   = c(1L),
                     weights = c(1)))
  
  g2 = list(A = list(edges   = c(1L, 2L),
                     weights = c(1, 1)),
            B = list(edges   = c(1L, 2L),
                     weights = c(1, 1)))
  
  expect_true(is_connected(g1,"A","A"))
  expect_error(is_connected(g1,1,1))
  expect_error(is_connected(g1,"A",1))
  expect_error(is_connected(g1,1,"A"))
  
  expect_true(is_connected(g2,"A","B"))
  expect_error(is_connected(g2,1,2))
  expect_error(is_connected(g2,"A",2))
  expect_error(is_connected(g2,1,"B"))
  
  expect_error(is_connected(g1,1,1L))
  expect_error(is_connected(g1,1L,1))
  expect_error(is_connected(g1,1L,1L))
  
  expect_error(is_connected(g1,1,3))
  expect_error(is_connected(g1,"A",3))
  expect_error(is_connected(g1,1,"C"))
  expect_error(is_connected(g1,"A","C"))
  
  expect_error(is_connected(g1,c("A","A"),"C"))
  expect_error(is_connected(g1,"A",c("C","C")))
  expect_error(is_connected(g1,c("A","A"),c("C","C")))
  
  expect_error(is_connected(g1,1,TRUE))
  expect_error(is_connected(g1,"A",TRUE))
  
  expect_error(is_connected(g1,1,NaN))
  expect_error(is_connected(g1,"A",NaN))
  
  expect_error(is_connected(g1,1,NA_real_))
  expect_error(is_connected(g1,"A",NA_real_))
  expect_error(is_connected(g1,1,NA_integer_))
  expect_error(is_connected(g1,"A",NA_integer_))
  expect_error(is_connected(g1,1,NA_character_))
  expect_error(is_connected(g1,"A",NA_character_))
  expect_error(is_connected(g1,1,NA_complex_))
  expect_error(is_connected(g1,"A",NA_complex_))
})


test_that("Bad graphs", {
  bad_g1 = list(A = list())
  bad_g2 = list(A = list(edges = 1L))
  bad_g3 = list(A = list(weights = 1))
  bad_g4 = list(list(weights = 1, edges = 1L))
  
  expect_error(is_connected(bad_g1,"A","A"))
  expect_error(is_connected(bad_g2,"A","A"))
  expect_error(is_connected(bad_g3,"A","A"))
  expect_error(is_connected(bad_g4,"A","A"))
  
  expect_error(is_connected(1,     "A","A"))
  expect_error(is_connected(1L,    "A","A"))
  expect_error(is_connected("1",   "A","A"))
  expect_error(is_connected(TRUE,  "A","A"))
  expect_error(is_connected(c(A=1),"A","A"))
})

context("Test shortest_path")

test_that("Check arg lists", {
  expect_equal(names(formals(shortest_path)), c("g","v1","v2"))
})


test_that("Bad graphs", {
  bad_g1 = list(A = list())
  bad_g2 = list(A = list(edges = 1L))
  bad_g3 = list(A = list(weights = 1))
  
  expect_error(shortest_path(bad_g1,"A","A"))
  expect_error(shortest_path(bad_g2,"A","A"))
  expect_error(shortest_path(bad_g3,"A","A"))
  
  expect_error(shortest_path(1,"A","A"))
  expect_error(shortest_path(1L,"A","A"))
  expect_error(shortest_path("1","A","A"))
  expect_error(shortest_path(TRUE,"A","A"))
})

test_that("Unconnected graphs", {
  g1 = list(A = list(edges   = integer(),
                     weights = numeric()))
  
  g2 = list(A = list(edges   = c(1L),
                     weights = c(1)),
            B = list(edges   = c(2L),
                     weights = c(1)))
  
  g3 = list(A = list(edges   = c(1L),
                     weights = c(1)),
            B = list(edges   = c(1L, 3L),
                     weights = c(1, 1)),
            C = list(edges   = c(3L),
                     weights = c(1)))
  
  
  expect_identical(shortest_path(g2,"A","A"), c("A","A"))
  expect_identical(shortest_path(g2,"B","B"), c("B","B"))
  
  expect_identical(shortest_path(g3,"A","A"), c("A","A"))
  expect_identical(shortest_path(g3,"B","A"), c("B","A"))
  expect_identical(shortest_path(g3,"B","C"), c("B","C"))
  expect_identical(shortest_path(g3,"C","C"), c("C","C"))
  
  
  is_empty_atomic = function(x) is.atomic(x) & length(x) == 0
  
  expect_true(is_empty_atomic(shortest_path(g1,"A","A")))
  expect_true(is_empty_atomic(shortest_path(g2,"A","B")))
  expect_true(is_empty_atomic(shortest_path(g2,"B","A")))
  expect_true(is_empty_atomic(shortest_path(g3,"A","B")))
  expect_true(is_empty_atomic(shortest_path(g3,"B","B")))
  expect_true(is_empty_atomic(shortest_path(g3,"C","B")))
})



test_that("Vertex labels", {
  g1 = list(A = list(edges   = c(1L),
                     weights = c(1)))
  
  g2 = list(A = list(edges   = c(1L, 2L),
                     weights = c(1, 1)),
            B = list(edges   = c(1L, 2L),
                     weights = c(1, 1)))
  
  # Good labels
  expect_identical(shortest_path(g1,"A","A"), c("A","A"))
  expect_identical(shortest_path(g2,"A","B"), c("A","B"))
  
  # Bad labels
  
  expect_error(shortest_path(g1,1,1))
  expect_error(shortest_path(g1,1,1L))
  expect_error(shortest_path(g1,1L,1))
  expect_error(shortest_path(g1,1L,1L))
  expect_error(shortest_path(g1,"A",1))
  expect_error(shortest_path(g1,1,"A"))
  expect_error(shortest_path(g1,"A","C"))
  expect_error(shortest_path(g1,1,TRUE))
  expect_error(shortest_path(g1,"A",TRUE))
  
  expect_error(shortest_path(g2,1,2))
  expect_error(shortest_path(g2,"A",2))
  expect_error(shortest_path(g2,1,"B"))
  
  expect_error(shortest_path(g1,1,NaN))
  expect_error(shortest_path(g1,"A",NaN))
  
  expect_error(shortest_path(g1,1,NA_real_))
  expect_error(shortest_path(g1,"A",NA_real_))
  expect_error(shortest_path(g1,1,NA_integer_))
  expect_error(shortest_path(g1,"A",NA_integer_))
  expect_error(shortest_path(g1,1,NA_character_))
  expect_error(shortest_path(g1,"A",NA_character_))
  expect_error(shortest_path(g1,1,NA_complex_))
  expect_error(shortest_path(g1,"A",NA_complex_))
})


test_that("Connected graphs", {
  g1 = list(A = list(edges   = c(1L),
                     weights = c(1)))
  
  g2 = list(A = list(edges   = c(1L, 2L),
                     weights = c(1, 1)),
            B = list(edges   = c(1L, 2L),
                     weights = c(1, 1)))
  
  g3 = list(A = list(edges   = c(1L, 2L),
                     weights = c(1, 1)),
            B = list(edges   = c(1L, 3L),
                     weights = c(1, 1)),
            C = list(edges   = c(2L, 3L),
                     weights = c(1, 1)))
  
  g4 = list(A = list(edges   = c(2L),
                     weights = c(1)),
            B = list(edges   = c(3L),
                     weights = c(1)),
            C = list(edges   = c(4L),
                     weights = c(1)),
            D = list(edges   = c(5L),
                     weights = c(1)),
            E = list(edges   = c(6L),
                     weights = c(1)),
            F = list(edges   = integer(),
                     weights = numeric()))
  
  g5 = list(A = list(edges   = c(1L, 2L),
                     weights = c(10, 1)),
            B = list(edges   = c(1L, 2L),
                     weights = c(1,  10)))
  
  expect_identical(shortest_path(g1,"A","A"), c("A","A"))
  
  expect_identical(shortest_path(g2,"A","A"), c("A","A"))
  expect_identical(shortest_path(g2,"A","B"), c("A","B"))
  expect_identical(shortest_path(g2,"B","A"), c("B","A"))
  expect_identical(shortest_path(g2,"B","B"), c("B","B"))
  
  expect_identical(shortest_path(g3,"A","A"), c("A","A"))
  expect_identical(shortest_path(g3,"A","B"), c("A","B"))
  expect_identical(shortest_path(g3,"B","A"), c("B","A"))
  expect_identical(shortest_path(g3,"B","C"), c("B","C"))
  expect_identical(shortest_path(g3,"C","B"), c("C","B"))
  expect_identical(shortest_path(g3,"C","C"), c("C","C"))
  
  expect_identical(shortest_path(g4,"A","B"), c("A","B"))
  expect_identical(shortest_path(g4,"A","C"), c("A","B","C"))
  expect_identical(shortest_path(g4,"A","D"), c("A","B","C","D"))
  expect_identical(shortest_path(g4,"A","E"), c("A","B","C","D","E"))
  expect_identical(shortest_path(g4,"A","F"), c("A","B","C","D","E","F"))
  
  expect_identical(shortest_path(g5,"A","A"), c("A","B","A"))
  expect_identical(shortest_path(g5,"A","B"), c("A","B"))
  expect_identical(shortest_path(g5,"B","A"), c("B","A"))
  expect_identical(shortest_path(g5,"B","B"), c("B","A","B"))
  
  is_empty_atomic = function(x) is.atomic(x) & length(x) == 0
  
  expect_true(is_empty_atomic(shortest_path(g4,"F","A")))
  expect_true(is_empty_atomic(shortest_path(g4,"F","B")))
  expect_true(is_empty_atomic(shortest_path(g4,"F","C")))
  expect_true(is_empty_atomic(shortest_path(g4,"F","D")))
  expect_true(is_empty_atomic(shortest_path(g4,"F","E")))
})




test_that("Med valid graph", {
  # See http://en.wikipedia.org/wiki/Minimum_spanning_tree#mediaviewer/File:Multiple_minimum_spanning_trees.svg
  g = list(A = list(edges   = c(2L,4L,5L),
                    weights = c(1 ,4 ,3 )),
           B = list(edges   = c(1L,4L,5L),
                    weights = c(1 ,4 ,2 )),
           C = list(edges   = c(5L,6L),
                    weights = c(4 ,5 )),
           D = list(edges   = c(1L,2L,5L),
                    weights = c(4 ,4 ,4 )),
           E = list(edges   = c(1L,2L,3L,4L,6L),
                    weights = c(3 ,2 ,4 ,4 ,7 )),
           F = list(edges   = c(3L,5L),
                    weights = c(5 ,7 )))
  
  expect_identical(shortest_path(g,"B","F"),  c("B","E","F"))
  expect_identical(shortest_path(g,"D","F"),  c("D","E","F"))
  expect_identical(shortest_path(g,"C","D"),  c("C","E","D"))
})




