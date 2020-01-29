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

import Prelude hiding (Just, Nothing, Maybe)

data ValentElectrons = S | P | D | F deriving (Show)
data Period = Period Int deriving (Eq, Ord, Show)
data GroupClass = A | B deriving (Show)
data Group = Group Int GroupClass deriving (Show)
data Radius = Angstrom Float deriving (Show)
data ElectroNegativity = Just Float | Nothing
data Mass = Atomic Float | Varying Float deriving (Eq)
data State = Gas | Solid | Liquid | Unknown deriving (Show)

instance Show ElectroNegativity where
  show r = case r of Just f -> (show f)
                     Nothing -> ""

instance Show Mass where 
  show (Atomic m) = show m
  show (Varying m) = "[" ++ (show m) ++ "]"

data CommonElement = CommonElement { 
    number    :: Int
  , symbol    :: String
  , name      :: String
  , desc      :: String
  , conf      :: ValentElectrons
  , group     :: Group
  , period    :: Period
  , state     :: State
  , radius    :: Radius
  , mass      :: Mass
  , electroNegativity    :: ElectroNegativity -- электроотрицательность
} deriving (Show)

data Element = Element CommonElement | HSpecial | Ro Int Int | Rh Bool Int Int | Placeholder | Empty

hspecial = HSpecial

hydrogen = Element $ CommonElement {
       number = 1
     , symbol = "H"
     , name = "Hydrogen"
     , desc = "N/A"
     , conf = S
     , group = Group 1 A
     , period = Period 1
     , state = Gas
     , radius = Angstrom 0.53
     , mass = Atomic 1.01
     , electroNegativity = Just 2.10
   }
helium = Element $ CommonElement {
       number = 2
     , symbol = "He"
     , name = "Helium"
     , desc = "N/A"
     , conf = S
     , group = Group 8 A
     , period = Period 1
     , state = Gas
     , radius = Angstrom 0.31
     , mass = Atomic 4.0
     , electroNegativity = Nothing
   }
lithium = Element $ CommonElement {
      number = 3
    , symbol = "Li"
    , name = "Lithium"
    , desc = "N/A"
    , conf = S
    , group = Group 1 A
    , period = Period 2
    , state = Solid
    , radius = Angstrom 1.82
    , mass = Atomic 6.9
    , electroNegativity = Just 0.98
  }
sodium = Element $ CommonElement {
      number = 11
    , symbol = "Na"
    , name = "Sodium"
    , desc = "N/A"
    , conf = S
    , group = Group 1 A
    , period = Period 3
    , state = Solid
    , radius = Angstrom 2.27
    , mass = Atomic 23.0
    , electroNegativity = Just 0.98
  }
potassium = Element $ CommonElement {
      number = 19
    , symbol = "K"
    , name = "Potassium"
    , desc = "N/A"
    , conf = S
    , group = Group 1 A
    , period = Period 4
    , state = Solid
    , radius = Angstrom 2.8
    , mass = Atomic 39.1
    , electroNegativity = Just 0.82
  }
copper = Element $ CommonElement {
      number = 29
    , symbol = "Cu"
    , name = "Copper"
    , desc = "N/A"
    , conf = D
    , group = Group 1 B
    , period = Period 4
    , state = Solid
    , radius = Angstrom 1.4
    , mass = Atomic 1.90
    , electroNegativity = Just 63.5
  }
rubidium = Element $ CommonElement {
      number = 37
    , symbol = "Rb"
    , name = "Rubidium"
    , desc = "N/A"
    , conf = S
    , group = Group 1 A
    , period = Period 5
    , state = Solid
    , radius = Angstrom 2.9
    , mass = Atomic 85.5
    , electroNegativity = Just 0.82
  }
silver = Element $ CommonElement {
      number = 47
    , symbol = "Ag"
    , name = "Silver"
    , desc = "N/A"
    , conf = D
    , group = Group 1 B
    , period = Period 5
    , state = Solid
    , radius = Angstrom 2.9
    , mass = Atomic 1.93
    , electroNegativity = Just 107.9
  }
caesium = Element $ CommonElement {
      number = 55
    , symbol = "Cs"
    , name = "Cesium"
    , desc = "N/A"
    , conf = S
    , group = Group 1 A
    , period = Period 6
    , state = Solid
    , radius = Angstrom 3.43
    , mass = Atomic 132.9
    , electroNegativity = Just 0.79
  }
gold = Element $ CommonElement {
      number = 79
    , symbol = "Au"
    , name = "Gold"
    , desc = "N/A"
    , conf = D
    , group = Group 1 B
    , period = Period 5
    , state = Solid
    , radius = Angstrom 2.9
    , mass = Atomic 197.0
    , electroNegativity = Just 2.54
  }
francium = Element $ CommonElement {
      number = 87
    , symbol = "Fr"
    , name = "Francium"
    , desc = "N/A"
    , conf = S
    , group = Group 1 A
    , period = Period 5
    , state = Solid
    , radius = Angstrom 2.9
    , mass = Varying 223.0
    , electroNegativity = Just 0.70
  }
roentgenium = Element $ CommonElement {
      number = 111
    , symbol = "Rg"
    , name = "Roentgenium"
    , desc = "N/A"
    , conf = D
    , group = Group 1 B
    , period = Period 5
    , state = Unknown
    , radius = Angstrom 2.9
    , mass = Varying 281.0
    , electroNegativity = Nothing
  }
cerium = Element $ CommonElement {
      number = 58
    , symbol = "Ce"
    , name = "Cerium"
    , desc = "N/A"
    , conf = F
    , group = Group 1 A
    , period = Period 5
    , state = Solid
    , radius = Angstrom 2.9
    , mass = Atomic 140.1
    , electroNegativity = Just 1.12
  }
thorium = Element $ CommonElement {
      number = 90
    , symbol = "Th"
    , name = "Thorium"
    , desc = "N/A"
    , conf = F
    , group = Group 1 A
    , period = Period 5
    , state = Solid
    , radius = Angstrom 2.9
    , mass = Atomic 232.0
    , electroNegativity = Just 1.30
  }
beryllium = Element $ CommonElement {
      number = 4
    , symbol = "Be"
    , name = "Beryllium"
    , desc = "N/A"
    , conf = S
    , group = Group 2 A
    , period = Period 2
    , state = Solid
    , radius = Angstrom 1.53
    , mass = Atomic 9.0
    , electroNegativity = Just 1.57
  }
magnesium = Element $ CommonElement {
      number = 12
    , symbol = "Mg"
    , name = "Magnesium"
    , desc = "N/A"
    , conf = S
    , group = Group 2 A
    , period = Period 3
    , state = Solid
    , radius = Angstrom 1.73
    , mass = Atomic 24.3
    , electroNegativity = Just 1.31
  }
calcium = Element $ CommonElement {
      number = 20
    , symbol = "Ca"
    , name = "Calcium"
    , desc = "N/A"
    , conf = S
    , group = Group 2 A
    , period = Period 4
    , state = Solid
    , radius = Angstrom 2.9
    , mass = Atomic 40.1
    , electroNegativity = Just 1.00
  }
zinc = Element $ CommonElement {
      number = 30
    , symbol = "Zn"
    , name = "Zinc"
    , desc = "N/A"
    , conf = D
    , group = Group 2 B
    , period = Period 4
    , state = Solid
    , radius = Angstrom 1.39
    , mass = Atomic 65.4
    , electroNegativity = Just 1.65
  }
strontium = Element $ CommonElement {
      number = 38
    , symbol = "Sr"
    , name = "Strontium"
    , desc = "N/A"
    , conf = S
    , group = Group 2 A
    , period = Period 5
    , state = Solid
    , radius = Angstrom 2.55
    , mass = Atomic 87.6
    , electroNegativity = Just 0.95
  }
cadmium = Element $ CommonElement {
      number = 48
    , symbol = "Cd"
    , name = "Cadmium"
    , desc = "N/A"
    , conf = D
    , group = Group 2 B
    , period = Period 5
    , state = Solid
    , radius = Angstrom 2.9
    , mass = Atomic 112.4
    , electroNegativity = Just 1.58
  }
barium = Element $ CommonElement {
      number = 56
    , symbol = "Ba"
    , name = "Barium"
    , desc = "N/A"
    , conf = S
    , group = Group 2 A
    , period = Period 6
    , state = Solid
    , radius = Angstrom 2.68
    , mass = Atomic 137.3
    , electroNegativity = Just 0.89
  }
mercury = Element $ CommonElement {
      number = 80
    , symbol = "Hg"
    , name = "Mercury"
    , desc = "N/A"
    , conf = D
    , group = Group 2 B
    , period = Period 6
    , state = Liquid
    , radius = Angstrom 1.55
    , mass = Atomic 200.6
    , electroNegativity = Just 2.00
  }
radium = Element $ CommonElement {
      number = 88
    , symbol = "Ra"
    , name = "Radium"
    , desc = "N/A"
    , conf = S
    , group = Group 2 A
    , period = Period 7
    , state = Solid
    , radius = Angstrom 2.00
    , mass = Varying 226
    , electroNegativity = Just 0.90
  }
copernicium = Element $ CommonElement {
      number = 112
    , symbol = "Cn"
    , name = "Copernicium"
    , desc = "N/A"
    , conf = D
    , group = Group 2 B
    , period = Period 7
    , state = Unknown
    , radius = Angstrom 1.47
    , mass = Varying 285
    , electroNegativity = Nothing
  }

boron = Element $ CommonElement {
      number = 5
    , symbol = "B"
    , name = "Boron"
    , desc = "N/A"
    , conf = P
    , group = Group 3 A
    , period = Period 2
    , state = Solid
    , radius = Angstrom 1.8
    , mass = Atomic 2.04
    , electroNegativity = Just 10.8
  }

aluminium = Element $ CommonElement {
      number = 13
    , symbol = "Al"
    , name = "Aluminium"
    , desc = "N/A"
    , conf = P
    , group = Group 3 A
    , period = Period 3
    , state = Solid
    , radius = Angstrom 1.84
    , mass = Atomic 27.0
    , electroNegativity = Just 1.61
}

scandium = Element $ CommonElement {
      number = 21
    , symbol = "Sc"
    , name = "Scandium"
    , desc = "N/A"
    , conf = D
    , group = Group 3 B
    , period = Period 4
    , state = Solid
    , radius = Angstrom 2.30
    , mass = Atomic 45.0
    , electroNegativity = Just 1.36
}
gallium = Element $ CommonElement {
      number = 31
    , symbol = "Ga"
    , name = "Gallium"
    , desc = "N/A"
    , conf = P
    , group = Group 3 A
    , period = Period 4
    , state = Solid
    , radius = Angstrom 1.87
    , mass = Atomic 69.7
    , electroNegativity = Just 1.81
}

yttrium = Element $ CommonElement {
      number = 39
    , symbol = "Y"
    , name = "Yttrium"
    , desc = "N/A"
    , conf = D
    , group = Group 3 B
    , period = Period 5
    , state = Solid
    , radius = Angstrom 2.4
    , mass = Atomic 88.9
    , electroNegativity = Just 1.22
}
indium = Element $ CommonElement {
      number = 49
    , symbol = "In"
    , name = "Indium"
    , desc = "N/A"
    , conf = P
    , group = Group 3 A
    , period = Period 5
    , state = Solid
    , radius = Angstrom 2.20
    , mass = Atomic 114.8
    , electroNegativity = Just 1.78
}
lanthanum = Element $ CommonElement {
      number = 57
    , symbol = "La"
    , name = "Lanthanum"
    , desc = "N/A"
    , conf = D
    , group = Group 3 B
    , period = Period 6
    , state = Solid
    , radius = Angstrom 2.50
    , mass = Atomic 138.9
    , electroNegativity = Just 1.10
}
thallium = Element $ CommonElement {
      number = 81
    , symbol = "Tl"
    , name = "Thallium"
    , desc = "N/A"
    , conf = P
    , group = Group 3 A
    , period = Period 6
    , state = Solid
    , radius = Angstrom 2.20
    , mass = Atomic 204.4
    , electroNegativity = Just 1.62
}
actinium = Element $ CommonElement {
      number = 89
    , symbol = "Ac"
    , name = "Actinium"
    , desc = "N/A"
    , conf = D
    , group = Group 3 B
    , period = Period 7
    , state = Solid
    , radius = Angstrom 2.00
    , mass = Varying 227.0
    , electroNegativity = Just 1.10
}
carbon = Element $ CommonElement {
      number = 6
    , symbol = "C"
    , name = "Carbon"
    , desc = "N/A"
    , conf = P
    , group = Group 4 A
    , period = Period 2
    , state = Solid
    , radius = Angstrom 1.70
    , mass = Atomic 12.0
    , electroNegativity = Just 2.55
}
silicon = Element $ CommonElement {
      number = 14
    , symbol = "Si"
    , name = "Silicon"
    , desc = "N/A"
    , conf = P
    , group = Group 4 A
    , period = Period 3
    , state = Solid
    , radius = Angstrom 2.10
    , mass = Atomic 28.1
    , electroNegativity = Just 1.90
}
titanium = Element $ CommonElement {
      number = 22
    , symbol = "Ti"
    , name = "Titanium"
    , desc = "N/A"
    , conf = D
    , group = Group 4 B
    , period = Period 4
    , state = Solid
    , radius = Angstrom 2.15
    , mass = Atomic 47.9
    , electroNegativity = Just 1.54
}
germanium = Element $ CommonElement {
      number = 32
    , symbol = "Ge"
    , name = "Germanium"
    , desc = "N/A"
    , conf = P
    , group = Group 4 A
    , period = Period 4
    , state = Solid
    , radius = Angstrom 2.11
    , mass = Atomic 72.6
    , electroNegativity = Just 2.01
}
zirconium = Element $ CommonElement {
      number = 40
    , symbol = "Zr"
    , name = "Zirconium"
    , desc = "N/A"
    , conf = D
    , group = Group 4 B
    , period = Period 5
    , state = Solid
    , radius = Angstrom 1.6
    , mass = Atomic 91.2
    , electroNegativity = Just 1.33
}
tin = Element $ CommonElement {
      number = 50
    , symbol = "Sn"
    , name = "Tin"
    , desc = "N/A"
    , conf = P
    , group = Group 4 A
    , period = Period 5
    , state = Solid
    , radius = Angstrom 2.25
    , mass = Atomic 118.7
    , electroNegativity = Just 1.96
}
hafnium = Element $ CommonElement {
      number = 72
    , symbol = "Hf"
    , name = "Hafnium"
    , desc = "N/A"
    , conf = D
    , group = Group 4 B
    , period = Period 6
    , state = Solid
    , radius = Angstrom 2.25
    , mass = Atomic 178.5
    , electroNegativity = Just 1.30
}
lead = Element $ CommonElement {
      number = 82
    , symbol = "Pb"
    , name = "Lead"
    , desc = "N/A"
    , conf = P
    , group = Group 4 A
    , period = Period 6
    , state = Solid
    , radius = Angstrom 2.02
    , mass = Atomic 207.2
    , electroNegativity = Just 2.33
}
rutherfordium = Element $ CommonElement {
      number = 104
    , symbol = "Rf"
    , name = "Rutherfordium"
    , desc = "N/A"
    , conf = D
    , group = Group 4 B
    , period = Period 7
    , state = Solid
    , radius = Angstrom 2.00
    , mass = Varying 265.0
    , electroNegativity = Nothing
}
















