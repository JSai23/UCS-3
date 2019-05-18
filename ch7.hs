--7.1
--depth-first-search: 1,2,5,3,4,6
--breadth-first-search: 1,2,4,3,5,6


--7.2
dfsedges start g = dfs [] [start] []
   where
      dfs edges [] vis      = edges
      dfs edges (c:cs) vis
         | elem c vis = dfs edges cs vis
		 | otherwise  = dfs ([(c,x,(weight c x g)) | x <-(adjacent g c)] ++ edges) ((adjacent g c)++cs) (vis++[c]) 

--7.3a - undirected 

--7.3b - directed - wrong 
dfscycle start g = dfs [] [start] []
   where 
      dfs repeats [] vis = not (repeats == [])
      dfs repeats (c:cs) vis
         | elem c vis = dfs (c:repeats) cs vis
         | otherwise  = dfs repeats ((adjacent g c)++cs) (vis++[c]) 
 
 
 
 --7.4
-- based on the drawing any side edge on the outer square and any
-- edge removed from the 4 connected to the center will result in a minimum spanning tree

--7.5 
{- 
first the pointer implementation would be extremely inefficient for Kruskal's algorithm which is heavy in computation. The pointer representation is meant for low space consumption but is extremely costly in efficiency for computations. A adjacency list representation would be efficient but not as efficient as the adjacency matrix representation. The adjacency matrix representation works the most efficiently on a dense graph. Due to the large amount of times information is accessed from the graph about edges in Kruskal's algorithm a matrix representation would be the easiest and quickest implementation. 
-}

--7.6
--prims
prim g                = prim' [n] ns []
   where 
      (n:ns)          = nodes g 
	  es              = edgesU g
	  prim' t []  mst = mst 
	  prim' t r mst    = let e@(c,u',v') = minimum[(c,u,v)| (u,v,c) <- es, elem u t, elem v r]
						 in prim' (v':t) (rember v' r) (e:mst)
						 
						 
             























