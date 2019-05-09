allfibs = 0:1:[allfibs!!n + allfibs !! (n+1) | n <- [0..]]
main = do
       print (allfibs !! 10000)
