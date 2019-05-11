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
 

