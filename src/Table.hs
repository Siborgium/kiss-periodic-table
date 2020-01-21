
module Table where

import Data.Sort(sort)

data ValentElectrons = S | P | D | F deriving (Show)
data Period = Period Int deriving (Eq, Ord, Show)
data GroupClass = A | B deriving (Show)
data Group = Group Int GroupClass deriving (Show)
data Radius = Angstrem Float deriving (Show)
data Resist = Exactly Float | Unknown

instance Show Resist where
  show r = case r of Exactly f -> (show f)
                     Unknown -> ""

data CommonElement = CommonElement { 
    number    :: Int
  , symbol    :: String
  , name      :: String
  , desc      :: String
  , conf      :: ValentElectrons
  , group     :: Group
  , period    :: Period
  , radius    :: Radius
  , mass      :: Float
  , resist    :: Resist -- электроотрицательность
} deriving (Show)

data Element = Element CommonElement | HSpecial | HigherOxyde | HydroCompound | Placeholder

table :: [[Element]]
table = [[ hydrogen, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, hspecial, helium ],
         [ lithium, Placeholder, bohr],
         [ Placeholder,Placeholder,Placeholder,Placeholder, Placeholder, Placeholder, Placeholder, hspecial, hspecial, hspecial],
         [],
         [],
         [],
         [],
         [],
         [],
         [],
         []]
        

hspecial = HSpecial

hydrogen = Element $ CommonElement {
       number = 1
     , symbol = "H"
     , name = "Hydrogen"
     , desc = "N/A"
     , conf = S
     , group = Group 1 A
     , period = Period 1
     , radius = Angstrem 0.53
     , mass = 1.01
     , resist = Exactly 0.98
   }
helium = Element $ CommonElement {
      number = 2
     , symbol = "He"
     , name = "Helium"
     , desc = "N/A"
     , conf = S
     , group = Group 8 A
     , period = Period 1
     , radius = Angstrem 0.31
     , mass = 4.0
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
    , radius = Angstrem 1.82
    , mass = 6.9
    , resist = Exactly 0.98
  }
bohr = Element $ CommonElement {
     number = 5
    , symbol = "B"
    , name = "Bohr"
    , desc = "N/A"
    , conf = P
    , group = Group 3 A
    , period = Period 2
    , radius = Angstrem 0.529177
    , mass = 2.04
    , resist = Exactly 10.8
  }
