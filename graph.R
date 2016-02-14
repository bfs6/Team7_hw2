is_valid = function(g) {
  #Remark: in a valid graph, some vertices have empty vectors for edges and vertices if they don't point to anywhere
  #1. Check that g is a list of lists
  if(length(g)<1){
    print(1) 
    return (FALSE)
  }else if(!is.list(g)){
    print(1.1) 
    return(FALSE)
  }else if(prod(sapply(g,is.list))!=1){
    print(1.2) 
    return (FALSE)
  }
  #2. Missing labels
  if(is.null(names(g))){
    print(2) 
    return(FALSE)
  }else if(TRUE %in% (sapply(names(g),function(x){x==""}))){
    print(2.1) 
    return (FALSE)
  }
  #missing labels also caught all the testing cases for invalid structure
  #3. invalid structure
  if(mean(sapply(g,length))!=2){
    print(3) 
    return (FALSE)
  }
  #4. duplicate labels
  if(length(names(g))!=length(unique(names(g)))){
    print(4)
    return(FALSE)
  }
  #5. invalid vertex reference
  if(!FALSE %in% sapply(g,function(x){length(x$edges)>0 & length(x$weights)>0}) & ! NA %in% unlist(g)){
    if(max(sapply(g,function(x){max(x$edges)})) > length(g)){
      print(5)
      return (FALSE)
    }
  }
  #6. duplicate vertex
  if(length(unique(names(g))) != length(g)){
    print(6) 
    return (FALSE)
  }
  #7. edge type:
  for(i in g){
    if (FALSE %in% (sapply(i$edges,is.integer)) | NA %in% i$edges){
      print("bad edge")
      return (FALSE)
    }
  }
  #the test cases returns as false but I don't think they made it this far
  #8. Weight type and value
  for(i in g){
    if (FALSE %in% (sapply(i$weights,is.double)) | NA %in% i$weights | FALSE %in% (sapply(i$weights,function(x){x>0}))){
      print("bad weight")
      return (FALSE)
    }
  }
  #9. duplicate edges
  if(TRUE %in% sapply(g,function(x){duplicated(x$edges)})){
    print(9) 
    return (FALSE)
  }
  #10 edge and weight lenth mismatch
  if(FALSE %in% sapply(g,function(x){length(x$edges) == length(x$weights)})){
    print(10) 
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
            print(queue)
          }
        }
      }
    }
    print(visited)
    for( i in 1:length(visited)){
      if (visited[i]==indexv2)
        return(TRUE)
    }
    return(FALSE)
  }
}

# graph1 = list(A = list(edges   = c(2L),
#                        weights = c(1 )),
#               B = list(edges   = c(3L),
#                        weights = c(1 )),
#               C = list(edges   = c(5L),
#                        weights = c(1 )),
#               D = list(edges   = c(2L),
#                        weights = c(1 )),
#               E = list(edges   = c(4L,6L),
#                        weights = c(1,1  )),
#               E = list(edges   = c(),
#                        weights = c())
# )

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


