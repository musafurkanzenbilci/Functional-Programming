

data Cell = SpaceCraft Int | Sand | Rock Int | Pit deriving (Eq, Read, Show)

type Grid = [[Cell]]
type Coordinate = (Int, Int)

data Move = North | East | South | West | PickUp | PutDown deriving (Eq, Read, Show)

data Robot = Robot { name :: String,
                     location :: Coordinate,
                     capacity :: Int,
                     energy :: Int,
                     storage :: Int } deriving (Read, Show)


isInGrid :: Grid -> Coordinate -> Bool
isInGrid grid coor = if((fst coor)>=x || (snd coor)>=y || (fst coor)<0 || (snd coor)<0) then False else True where
                x=length (grid!!0)
                y=length grid

-------------------------------------------------------------------------------------------
rock (Rock n) = n
rock (SpaceCraft n) = 0
rock Sand = 0
rock Pit = 0

totalCount :: Grid -> Int
totalCount grid = sum [rock ((grid!!y)!!x) |x<-[0..(lenx-1)],y<-[0..(leny-1)],(rock ((grid!!y)!!x))/=0] where
                lenx=length (grid!!0)
                leny=length grid

-------------------------------------------------------------------------------------------

pitCheck (Rock n) = "rock"
pitCheck (SpaceCraft n) = "sp"
pitCheck Sand = "sand"
pitCheck Pit = "pit"

coordinatesOfPits :: Grid -> [Coordinate]
coordinatesOfPits grid =  [(x,y) |x<-[0..(lenx-1)],y<-[0..(leny-1)],(pitCheck ((grid!!y)!!x))=="pit"] where
                lenx=length (grid!!0)
                leny=length grid

-------------------------------------------------------------------------------------------




dowhat grid North robotcuk | (energy robotcuk)<1 =  Robot {name=name robotcuk,location=(fst (location robotcuk),snd (location robotcuk)),capacity=capacity robotcuk,energy=((energy robotcuk)),storage=storage robotcuk}
                           | (snd (location robotcuk))==0   =  Robot {name=name robotcuk,location=(fst (location robotcuk),snd (location robotcuk)),capacity=capacity robotcuk,energy=((energy robotcuk)-1),storage=storage robotcuk}
                           | (snd (location robotcuk))>0   =  Robot {name=name robotcuk,location=(fst (location robotcuk),snd (location robotcuk)-1),capacity=capacity robotcuk,energy=((energy robotcuk)-1),storage=storage robotcuk}

dowhat grid South robotcuk | (energy robotcuk)<1 =  robotcuk
                         | (snd (location robotcuk))==((length grid)-1) =  Robot {name=name robotcuk,location=(fst (location robotcuk),snd (location robotcuk)),capacity=capacity robotcuk,energy=((energy robotcuk)-1),storage=storage robotcuk}
                      | (snd (location robotcuk))>=0 =  Robot {name=name robotcuk,location=(fst (location robotcuk),snd (location robotcuk)+1),capacity=capacity robotcuk,energy=((energy robotcuk)-1),storage=storage robotcuk}

dowhat grid West robotcuk | (energy robotcuk)<1 =  robotcuk
                         | (fst (location robotcuk))==0 =  Robot {name=name robotcuk,location=(fst (location robotcuk),snd (location robotcuk)),capacity=capacity robotcuk,energy=((energy robotcuk)-1),storage=storage robotcuk}
                      | (fst (location robotcuk))>0 =  Robot {name=name robotcuk,location=(fst (location robotcuk)-1,snd (location robotcuk)),capacity=capacity robotcuk,energy=((energy robotcuk)-1),storage=storage robotcuk}

dowhat grid East robotcuk | (energy robotcuk)<1 =  robotcuk
                         | (fst (location robotcuk))==((length (grid!!0))-1) =  Robot {name=name robotcuk,location=(fst (location robotcuk),snd (location robotcuk)),capacity=capacity robotcuk,energy=((energy robotcuk)-1),storage=storage robotcuk}
                      | (fst (location robotcuk))>=0 =  Robot {name=name robotcuk,location=(fst (location robotcuk)+1,snd (location robotcuk)),capacity=capacity robotcuk,energy=((energy robotcuk)-1),storage=storage robotcuk}

dowhat grid PickUp robotcuk | (energy robotcuk)<5 =  Robot {name=name robotcuk,location=(fst (location robotcuk),snd (location robotcuk)),capacity=capacity robotcuk,energy=0,storage=storage robotcuk}
                         | (capacity robotcuk)<=(storage robotcuk) = Robot {name=name robotcuk,location=(fst (location robotcuk),snd (location robotcuk)),capacity=capacity robotcuk,energy=((energy robotcuk)-5),storage=storage robotcuk}
                      | (capacity robotcuk)>(storage robotcuk) = Robot {name=name robotcuk,location=(fst (location robotcuk),snd (location robotcuk)),capacity=capacity robotcuk,energy=((energy robotcuk)-5),storage=(storage robotcuk)+1}

dowhat grid PutDown robotcuk | (energy robotcuk)<3 =  Robot {name=name robotcuk,location=(fst (location robotcuk),snd (location robotcuk)),capacity=capacity robotcuk,energy=0,storage=storage robotcuk}
                         | (storage robotcuk)==0 = Robot {name=name robotcuk,location=(fst (location robotcuk),snd (location robotcuk)),capacity=capacity robotcuk,energy=((energy robotcuk)-3),storage=storage robotcuk}
                      | (storage robotcuk > 0) = Robot {name=name robotcuk,location=(fst (location robotcuk),snd (location robotcuk)),capacity=capacity robotcuk,energy=((energy robotcuk)-3),storage=(storage robotcuk)-1}



tracePath :: Grid -> Robot -> [Move] -> [Coordinate]
tracePath grid robot [] = []
tracePath grid robot moves = if ((pitCheck ((grid!!y)!!x))=="pit")  then (location robot):(tracePath grid robot (drop 1 moves)) else (location (dowhat grid (moves!!0) robot):(tracePath grid (dowhat grid (moves!!0) robot) (drop 1 moves)))  where
                                            x=(fst (location robot))
                                            y=(snd (location robot))



manhattan c1 c2 = max 0 (100-(((abs (x1-x2))+abs (y1-y2))*20)) where
                    x1=fst c1
                    x2=fst c2
                    y1=snd c1
                    y2=snd c2
--
sc (Rock n) = "rock"
sc (SpaceCraft n) = "sc"
sc Sand = "sand"
sc Pit = "pit"

scfind :: Grid -> Coordinate
scfind grid =  list!!0 where 
                list = [(x,y) |x<-[0..(lenx-1)],y<-[0..(leny-1)],(sc ((grid!!y)!!x))=="sc"] where
                            lenx=length (grid!!0)
                            leny=length grid

process scor robotcuk = Robot {name=name robotcuk,location=(fst (location robotcuk),snd (location robotcuk)),capacity=capacity robotcuk,energy=min 100 ((energy robotcuk)+(manhattan scor (location robotcuk))),storage=storage robotcuk}


energiseRobots :: Grid -> [Robot] -> [Robot]
energiseRobots grid robots = [process (scfind grid) (robots!!x) | x<-[0..(len-1)]] where
                            len=length robots




applyMoves :: Grid -> Robot -> [Move] -> (Grid, Robot)
applyMoves grid robot moves = (grid, robot)
