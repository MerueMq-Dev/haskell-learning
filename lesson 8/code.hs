myDrop _ []  = []
myDrop 0 list = list
myDrop count list = myDrop newCount $ tail list  
    where newCount = count - 1