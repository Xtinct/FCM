import Fcm

main = do
  let _data =[[0,3,0], [1,5,0], [2,4,0], [3,3,0], [2,2,0], [2,1,0], [1,0,0], [5,5,0], [6,5,1], [7,6,1], [5,3,1], [7,3,1], [6,2,1], [6,1,1], [8,1,1]]
  print $ separate 4 4 0.001 _data
