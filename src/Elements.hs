module Elements where

data ValentElectrons = S | P | D | F deriving (Show)
data Period = Period Int deriving (Eq, Ord, Show)
data GroupClass = A | B deriving (Show)
data Group = Group Int GroupClass deriving (Show)
data Radius = Angstrom Float deriving (Show)
data Resist = Exactly Float | Unknown
data Mass = Atomic Float | Changing Float deriving (Eq)

instance Show Resist where
  show r = case r of Exactly f -> (show f)
                     Unknown -> ""

instance Show Mass where 
  show (Atomic m) = show m
  show (Changing m) = "[" ++ (show m) ++ "]"

data CommonElement = CommonElement { 
    number    :: Int
  , symbol    :: String
  , name      :: String
  , desc      :: String
  , conf      :: ValentElectrons
  , group     :: Group
  , period    :: Period
  , radius    :: Radius
  , mass      :: Mass
  , resist    :: Resist -- электроотрицательность
} deriving (Show)

data Element = Element CommonElement | HSpecial | Ro Int Int | HydroCompound | Placeholder | Empty

hspecial = HSpecial

hydrogen = Element $ CommonElement {
       number = 1
     , symbol = "H"
     , name = "Hydrogen"
     , desc = "N/A"
     , conf = S
     , group = Group 1 A
     , period = Period 1
     , radius = Angstrom 0.53
     , mass = Atomic 1.01
     , resist = Exactly 2.10
   }
helium = Element $ CommonElement {
       number = 2
     , symbol = "He"
     , name = "Helium"
     , desc = "N/A"
     , conf = S
     , group = Group 8 A
     , period = Period 1
     , radius = Angstrom 0.31
     , mass = Atomic 4.0
     , resist = Unknown
   }
lithium = Element $ CommonElement {
      number = 3
    , symbol = "Li"
    , name = "Lithium"
    , desc = "N/A"
    , conf = S
    , group = Group 1 A
    , period = Period 2
    , radius = Angstrom 1.82
    , mass = Atomic 6.9
    , resist = Exactly 0.98
  }
natrium = Element $ CommonElement {
      number = 11
    , symbol = "Na"
    , name = "Natrium"
    , desc = "N/A"
    , conf = S
    , group = Group 1 A
    , period = Period 3
    , radius = Angstrom 2.27
    , mass = Atomic 23.0
    , resist = Exactly 0.98
  }
potassium = Element $ CommonElement {
      number = 19
    , symbol = "K"
    , name = "Potassium"
    , desc = "N/A"
    , conf = S
    , group = Group 1 A
    , period = Period 4
    , radius = Angstrom 2.8
    , mass = Atomic 39.1
    , resist = Exactly 0.82
  }
copper = Element $ CommonElement {
      number = 29
    , symbol = "Cu"
    , name = "Copper"
    , desc = "N/A"
    , conf = D
    , group = Group 1 B
    , period = Period 4
    , radius = Angstrom 1.4
    , mass = Atomic 1.90
    , resist = Exactly 63.5
  }
rubidium = Element $ CommonElement {
      number = 37
    , symbol = "Rb"
    , name = "Rubidium"
    , desc = "N/A"
    , conf = S
    , group = Group 1 A
    , period = Period 5
    , radius = Angstrom 2.9
    , mass = Atomic 85.5
    , resist = Exactly 0.82
  }
silver = Element $ CommonElement {
      number = 47
    , symbol = "Ag"
    , name = "Silver"
    , desc = "N/A"
    , conf = D
    , group = Group 1 A
    , period = Period 5
    , radius = Angstrom 2.9
    , mass = Atomic 1.93
    , resist = Exactly 107.9
  }
cesium = Element $ CommonElement {
      number = 55
    , symbol = "Ag"
    , name = "Silver"
    , desc = "N/A"
    , conf = S
    , group = Group 1 A
    , period = Period 6
    , radius = Angstrom 3.43
    , mass = Atomic 132.9
    , resist = Exactly 0.79
  }
gold = Element $ CommonElement {
      number = 79
    , symbol = "Au"
    , name = "Gold"
    , desc = "N/A"
    , conf = D
    , group = Group 1 B
    , period = Period 5
    , radius = Angstrom 2.9
    , mass = Atomic 1.93
    , resist = Exactly 107.9
  }
francium = Element $ CommonElement {
      number = 87
    , symbol = "Fr"
    , name = "Francium"
    , desc = "N/A"
    , conf = S
    , group = Group 1 A
    , period = Period 5
    , radius = Angstrom 2.9
    , mass = Changing 223.0
    , resist = Exactly 0.70
  }
roentgenium = Element $ CommonElement {
      number = 11
    , symbol = "Rg"
    , name = "Roentgenium"
    , desc = "N/A"
    , conf = D
    , group = Group 1 A
    , period = Period 5
    , radius = Angstrom 2.9
    , mass = Changing 281.0
    , resist = Unknown
  }
cerium = Element $ CommonElement {
      number = 58
    , symbol = "Ce"
    , name = "Cerium"
    , desc = "N/A"
    , conf = F
    , group = Group 1 A
    , period = Period 5
    , radius = Angstrom 2.9
    , mass = Atomic 140.1
    , resist = Exactly 1.12
  }
thorium = Element $ CommonElement {
      number = 90
    , symbol = "Th"
    , name = "Thorium"
    , desc = "N/A"
    , conf = F
    , group = Group 1 A
    , period = Period 5
    , radius = Angstrom 2.9
    , mass = Atomic 232.0
    , resist = Exactly 1.30
  }
beryllium = Element $ CommonElement {
      number = 4
    , symbol = "Be"
    , name = "Beryllium"
    , desc = "N/A"
    , conf = S
    , group = Group 2 A
    , period = Period 2
    , radius = Angstrom 1.53
    , mass = Atomic 9.0
    , resist = Exactly 1.57
  }
magnesium = Element $ CommonElement {
      number = 12
    , symbol = "Mg"
    , name = "Magnesium"
    , desc = "N/A"
    , conf = S
    , group = Group 2 A
    , period = Period 3
    , radius = Angstrom 1.73
    , mass = Atomic 24.3
    , resist = Exactly 1.31
  }
calcium = Element $ CommonElement {
      number = 20
    , symbol = "Ca"
    , name = "Calcium"
    , desc = "N/A"
    , conf = S
    , group = Group 2 A
    , period = Period 4
    , radius = Angstrom 2.9
    , mass = Atomic 40.1
    , resist = Exactly 1.00
  }
zinc = Element $ CommonElement {
      number = 90
    , symbol = "Th"
    , name = "Thorium"
    , desc = "N/A"
    , conf = F
    , group = Group 1 A
    , period = Period 5
    , radius = Angstrom 2.9
    , mass = Atomic 232.0
    , resist = Exactly 1.30
  }
stronzium = Element $ CommonElement {
      number = 90
    , symbol = "Th"
    , name = "Thorium"
    , desc = "N/A"
    , conf = F
    , group = Group 1 A
    , period = Period 5
    , radius = Angstrom 2.9
    , mass = Atomic 232.0
    , resist = Exactly 1.30
  }
cadmium = Element $ CommonElement {
      number = 90
    , symbol = "Th"
    , name = "Thorium"
    , desc = "N/A"
    , conf = F
    , group = Group 1 A
    , period = Period 5
    , radius = Angstrom 2.9
    , mass = Atomic 232.0
    , resist = Exactly 1.30
  }
barium = Element $ CommonElement {
      number = 90
    , symbol = "Th"
    , name = "Thorium"
    , desc = "N/A"
    , conf = F
    , group = Group 1 A
    , period = Period 5
    , radius = Angstrom 2.9
    , mass = Atomic 232.0
    , resist = Exactly 1.30
  }
quicksilver = Element $ CommonElement {
      number = 90
    , symbol = "Th"
    , name = "Thorium"
    , desc = "N/A"
    , conf = F
    , group = Group 1 A
    , period = Period 5
    , radius = Angstrom 2.9
    , mass = Atomic 232.0
    , resist = Exactly 1.30
  }
radium = Element $ CommonElement {
      number = 90
    , symbol = "Th"
    , name = "Thorium"
    , desc = "N/A"
    , conf = F
    , group = Group 1 A
    , period = Period 5
    , radius = Angstrom 2.9
    , mass = Atomic 232.0
    , resist = Exactly 1.30
  }
copernizium = Element $ CommonElement {
      number = 90
    , symbol = "Th"
    , name = "Thorium"
    , desc = "N/A"
    , conf = F
    , group = Group 1 A
    , period = Period 5
    , radius = Angstrom 2.9
    , mass = Atomic 232.0
    , resist = Exactly 1.30
  }











bohr = Element $ CommonElement {
      number = 5
    , symbol = "B"
    , name = "Bohr"
    , desc = "N/A"
    , conf = P
    , group = Group 3 A
    , period = Period 2
    , radius = Angstrom 0.529177
    , mass = Atomic 2.04
    , resist = Exactly 10.8
  }

