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
--lanthanoids
thorium = Element $ CommonElement {
      number = 90
    , symbol = "Th"
    , name = "Thorium"
    , desc = "N/A"
    , conf = F
    , group = Group 3 B
    , period = Period 7
    , state = Solid
    , radius = Angstrom 2.4
    , mass = Atomic 232.0
    , electroNegativity = Just 1.30
  }
protactinium = Element $ CommonElement {
      number = 91
    , symbol = "Pa"
    , name = "Protactinium"
    , desc = "N/A"
    , conf = F
    , group = Group 3 B
    , period = Period 7
    , state = Solid
    , radius = Angstrom 2.00
    , mass = Varying 231.0
    , electroNegativity = Just 1.50
  }
uranium = Element $ CommonElement {
      number = 92
    , symbol = "U"
    , name = "Uranium"
    , desc = "N/A"
    , conf = F
    , group = Group 3 B
    , period = Period 7
    , state = Solid
    , radius = Angstrom 2.30
    , mass = Atomic 238.0
    , electroNegativity = Just 1.38
  }
neptunium = Element $ CommonElement {
      number = 93
    , symbol = "Np"
    , name = "Neptunium"
    , desc = "N/A"
    , conf = F
    , group = Group 3 B
    , period = Period 7
    , state = Solid
    , radius = Angstrom 2.00
    , mass = Varying 237.0
    , electroNegativity = Just 1.36
  }
plutonium = Element $ CommonElement {
      number = 94
    , symbol = "Pu"
    , name = "Plutonium"
    , desc = "N/A"
    , conf = F
    , group = Group 3 B
    , period = Period 7
    , state = Solid
    , radius = Angstrom 2.00
    , mass = Varying 244.0
    , electroNegativity = Just 1.28
  }
americium = Element $ CommonElement {
      number = 95
    , symbol = "Am"
    , name = "Americium"
    , desc = "N/A"
    , conf = F
    , group = Group 3 B
    , period = Period 7
    , state = Solid
    , radius = Angstrom 2.00
    , mass = Varying 243.0
    , electroNegativity = Just 1.13
  }
curium = Element $ CommonElement {
      number = 96
    , symbol = "Cm"
    , name = "Curium"
    , desc = "N/A"
    , conf = F
    , group = Group 3 B
    , period = Period 7
    , state = Solid
    , radius = Angstrom 2.00
    , mass = Varying 247.0
    , electroNegativity = Just 1.28
  }
berkelium = Element $ CommonElement {
      number = 97
    , symbol = "Bk"
    , name = "Berkelium"
    , desc = "N/A"
    , conf = F
    , group = Group 3 B
    , period = Period 7
    , state = Solid
    , radius = Angstrom 2.00
    , mass = Varying 247.0
    , electroNegativity = Just 1.30
  }
californium = Element $ CommonElement {
      number = 98
    , symbol = "Cf"
    , name = "Californium"
    , desc = "N/A"
    , conf = F
    , group = Group 3 B
    , period = Period 7
    , state = Solid
    , radius = Angstrom 2.00
    , mass = Varying 251.0
    , electroNegativity = Just 1.30
  }
einsteinium = Element $ CommonElement {
      number = 99
    , symbol = "Es"
    , name = "Einsteinium"
    , desc = "N/A"
    , conf = F
    , group = Group 3 B
    , period = Period 7
    , state = Solid
    , radius = Angstrom 2.00
    , mass = Varying 252.0
    , electroNegativity = Just 1.30
  }
fermium = Element $ CommonElement {
      number = 100
    , symbol = "Fm"
    , name = "Fermium"
    , desc = "N/A"
    , conf = F
    , group = Group 3 B
    , period = Period 7
    , state = Unknown
    , radius = Angstrom 2.00
    , mass = Varying 257.0
    , electroNegativity = Just 1.30
  }
mendelevium = Element $ CommonElement {
      number = 101
    , symbol = "Md"
    , name = "Mendelevium"
    , desc = "N/A"
    , conf = F
    , group = Group 3 B
    , period = Period 7
    , state = Unknown
    , radius = Angstrom 2.00
    , mass = Varying 258.0
    , electroNegativity = Just 1.30
  }
nobelium = Element $ CommonElement {
      number = 102
    , symbol = "No"
    , name = "Nobelium"
    , desc = "N/A"
    , conf = F
    , group = Group 3 B
    , period = Period 7
    , state = Unknown
    , radius = Angstrom 2.00
    , mass = Varying 259.0
    , electroNegativity = Just 1.30
  }
lawrencium = Element $ CommonElement {
      number = 103
    , symbol = "Lr"
    , name = "Lawrencium"
    , desc = "N/A"
    , conf = F
    , group = Group 3 B
    , period = Period 7
    , state = Unknown
    , radius = Angstrom 2.00
    , mass = Varying 262.0
    , electroNegativity = Just 1.29
  }
--Group 2
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
    , state = Unknown
    , radius = Angstrom 2.00
    , mass = Varying 265.0
    , electroNegativity = Nothing
}
-- 5 group
nitrogen = Element $ CommonElement {
      number = 7
    , symbol = "N"
    , name = "Nitrogen"
    , desc = "N/A"
    , conf = P
    , group = Group 5 A
    , period = Period 2
    , state = Gas
    , radius = Angstrom 2.00
    , mass = Atomic 14.0
    , electroNegativity = Just 3.04
}
phosphorus = Element $ CommonElement {
      number = 15
    , symbol = "P"
    , name = "Phosphorus"
    , desc = "N/A"
    , conf = P
    , group = Group 5 A
    , period = Period 3
    , state = Solid
    , radius = Angstrom 2.00
    , mass = Atomic 31.0
    , electroNegativity = Just 2.19
}
vanadium = Element $ CommonElement {
      number = 23
    , symbol = "V"
    , name = "Vanadium"
    , desc = "N/A"
    , conf = D
    , group = Group 5 B
    , period = Period 4
    , state = Solid
    , radius = Angstrom 2.00
    , mass = Atomic 50.9
    , electroNegativity = Just 1.63 
}
arsenic = Element $ CommonElement {
      number = 33
    , symbol = "As"
    , name = "Arsenic"
    , desc = "N/A"
    , conf = P
    , group = Group 5 A
    , period = Period 4
    , state = Solid
    , radius = Angstrom 2.00
    , mass = Atomic 74.9
    , electroNegativity = Just 2.18
}
niobium = Element $ CommonElement {
      number = 41
    , symbol = "Nb"
    , name = "Niobium"
    , desc = "N/A"
    , conf = D
    , group = Group 5 B
    , period = Period 5
    , state = Solid
    , radius = Angstrom 2.00
    , mass = Atomic 92.9
    , electroNegativity = Just 1.60
}
antimony = Element $ CommonElement {
      number = 51
    , symbol = "Sb"
    , name = "Antimony"
    , desc = "N/A"
    , conf = P
    , group = Group 5 A
    , period = Period 5
    , state = Solid
    , radius = Angstrom 2.00
    , mass = Atomic 121.8
    , electroNegativity = Just 2.05
}
tantalum = Element $ CommonElement {
      number = 73
    , symbol = "Ta"
    , name = "Tantalum"
    , desc = "N/A"
    , conf = D
    , group = Group 5 B
    , period = Period 6
    , state = Solid
    , radius = Angstrom 2.00
    , mass = Atomic 180.9
    , electroNegativity = Just 1.50
}
bismuth = Element $ CommonElement {
      number = 83
    , symbol = "Bi"
    , name = "Bismuth"
    , desc = "N/A"
    , conf = P
    , group = Group 5 A
    , period = Period 6
    , state = Solid
    , radius = Angstrom 2.00
    , mass = Atomic 209.0
    , electroNegativity = Just 2.02
}
dubnium = Element $ CommonElement {
      number = 105
    , symbol = "Db"
    , name = "Dubnium"
    , desc = "N/A"
    , conf = D
    , group = Group 5 B
    , period = Period 7
    , state = Unknown
    , radius = Angstrom 2.00
    , mass = Varying 268.0
    , electroNegativity = Nothing
}
-- Group 6
oxygen = Element $ CommonElement {
      number = 8
    , symbol = "O"
    , name = "Oxygen"
    , desc = "N/A"
    , conf = P
    , group = Group 6 A
    , period = Period 6
    , state = Gas
    , radius = Angstrom 1.52
    , mass = Atomic 16.0
    , electroNegativity = Just 3.44
}
sulfur = Element $ CommonElement {
      number = 16
    , symbol = "S"
    , name = "Sulfur"
    , desc = "N/A"
    , conf = P
    , group = Group 6 A
    , period = Period 6
    , state = Solid
    , radius = Angstrom 1.8
    , mass = Atomic 32.1
    , electroNegativity = Just 2.58
}
chromium = Element $ CommonElement {
      number = 24
    , symbol = "Cr"
    , name = "Chromium"
    , desc = "N/A"
    , conf = D
    , group = Group 6 B
    , period = Period 6
    , state = Solid
    , radius = Angstrom 2.00
    , mass = Atomic 52.0
    , electroNegativity = Just 1.66
}
selenium = Element $ CommonElement {
      number = 34
    , symbol = "Se"
    , name = "Selenium"
    , desc = "N/A"
    , conf = P
    , group = Group 6 A
    , period = Period 6
    , state = Solid
    , radius = Angstrom 1.90
    , mass = Atomic 79.0
    , electroNegativity = Just 2.55
}
molybdenum = Element $ CommonElement {
      number = 42
    , symbol = "Mo"
    , name = "Molybdenum"
    , desc = "N/A"
    , conf = D
    , group = Group 6 B
    , period = Period 6
    , state = Solid
    , radius = Angstrom 2.09
    , mass = Atomic 95.9
    , electroNegativity = Just 2.16
}
tellurium = Element $ CommonElement {
      number = 52
    , symbol = "Te"
    , name = "Tellurium"
    , desc = "N/A"
    , conf = P
    , group = Group 6 A
    , period = Period 6
    , state = Solid
    , radius = Angstrom 2.10
    , mass = Atomic 127.6
    , electroNegativity = Just 2.10
}
tungsten = Element $ CommonElement {
      number = 74
    , symbol = "W"
    , name = "Tungsten"
    , desc = "N/A"
    , conf = D
    , group = Group 6 B
    , period = Period 6
    , state = Solid
    , radius = Angstrom 2.10
    , mass = Atomic 183.8
    , electroNegativity = Just 2.36
}
polonium = Element $ CommonElement {
      number = 84
    , symbol = "Po"
    , name = "Polonium"
    , desc = "N/A"
    , conf = P
    , group = Group 6 A
    , period = Period 6
    , state = Solid
    , radius = Angstrom 2.00
    , mass = Varying 206.0
    , electroNegativity = Just 2.00
}
seaborgium = Element $ CommonElement {
      number = 106
    , symbol = "Sg"
    , name = "Seaborgium"
    , desc = "N/A"
    , conf = D
    , group = Group 6 B
    , period = Period 6
    , state = Unknown
    , radius = Angstrom 2.00
    , mass = Varying 271.0
    , electroNegativity = Nothing
}









