-- Copyright 2020 Sergey Smirnykh

-- This file is part of kiss-periodic-table

-- kiss-periodic-table is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- kiss-periodic-table is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with kiss-periodic-table. If not, see <https:-- www.gnu.org/licenses/>.

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
    , group = Group 1 B
    , period = Period 5
    , radius = Angstrom 2.9
    , mass = Atomic 1.93
    , resist = Exactly 107.9
  }
cesium = Element $ CommonElement {
      number = 55
    , symbol = "Cs"
    , name = "Cesium"
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
    , mass = Atomic 197.0
    , resist = Exactly 2.54
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
      number = 111
    , symbol = "Rg"
    , name = "Roentgenium"
    , desc = "N/A"
    , conf = D
    , group = Group 1 B
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
      number = 30
    , symbol = "Zn"
    , name = "Zinc"
    , desc = "N/A"
    , conf = D
    , group = Group 2 B
    , period = Period 4
    , radius = Angstrom 1.39
    , mass = Atomic 65.4
    , resist = Exactly 1.65
  }
strontium = Element $ CommonElement {
      number = 38
    , symbol = "Sr"
    , name = "Strontium"
    , desc = "N/A"
    , conf = S
    , group = Group 2 A
    , period = Period 5
    , radius = Angstrom 2.55
    , mass = Atomic 87.6
    , resist = Exactly 0.95
  }
cadmium = Element $ CommonElement {
      number = 48
    , symbol = "Cd"
    , name = "Cadmium"
    , desc = "N/A"
    , conf = D
    , group = Group 2 B
    , period = Period 5
    , radius = Angstrom 2.9
    , mass = Atomic 112.4
    , resist = Exactly 1.58
  }
barium = Element $ CommonElement {
      number = 56
    , symbol = "Ba"
    , name = "Barium"
    , desc = "N/A"
    , conf = S
    , group = Group 2 A
    , period = Period 6
    , radius = Angstrom 2.68
    , mass = Atomic 137.3
    , resist = Exactly 0.89
  }
mercury = Element $ CommonElement {
      number = 80
    , symbol = "Hg"
    , name = "Mercury"
    , desc = "N/A"
    , conf = D
    , group = Group 2 B
    , period = Period 6
    , radius = Angstrom 1.55
    , mass = Atomic 200.6
    , resist = Exactly 2.00
  }
radium = Element $ CommonElement {
      number = 88
    , symbol = "Ra"
    , name = "Radium"
    , desc = "N/A"
    , conf = S
    , group = Group 2 A
    , period = Period 7
    , radius = Angstrom 2.00
    , mass = Changing 226
    , resist = Exactly 0.90
  }
copernicium = Element $ CommonElement {
      number = 112
    , symbol = "Cn"
    , name = "Copernicium"
    , desc = "N/A"
    , conf = D
    , group = Group 2 B
    , period = Period 7
    , radius = Angstrom 1.47
    , mass = Changing 285
    , resist = Unknown
  }

boron = Element $ CommonElement {
      number = 5
    , symbol = "B"
    , name = "Boron"
    , desc = "N/A"
    , conf = P
    , group = Group 3 A
    , period = Period 2
    , radius = Angstrom 0.529177
    , mass = Atomic 2.04
    , resist = Exactly 10.8
  }

