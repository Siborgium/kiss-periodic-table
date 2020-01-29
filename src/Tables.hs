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

module Tables where

import Elements

oldTable :: [[Element]]
oldTable = [
  [hydrogen,    Empty,       Empty,       Empty, Empty, Empty, hspecial, helium, Empty, Empty],
  [lithium,     beryllium,   boron,       carbon, Placeholder, Placeholder, Placeholder, Placeholder, Empty, Empty],
  [sodium,      magnesium,   aluminium,   silicon, Placeholder, Placeholder, Placeholder, Placeholder, Empty, Empty],
  [potassium,   calcium,     scandium,    titanium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder],
  [copper,      zinc,        gallium,     germanium, Placeholder, Placeholder, Placeholder, Placeholder, Empty, Empty],
  [rubidium,    strontium,   yttrium,     zirconium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder],
  [silver,      cadmium,     indium,      tin, Placeholder, Placeholder, Placeholder, Placeholder, Empty, Empty],
  [caesium,     barium,      lanthanum,   hafnium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder],
  [gold,        mercury,     thallium,    lead, Placeholder, Placeholder, Placeholder, Placeholder, Empty, Empty],
  [francium,    radium,      actinium,    rutherfordium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder],
  [roentgenium, copernicium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Empty, Empty],
  [(Ro 2 1),    (Ro 1 1),    (Ro 2 3), (Ro 1 2), (Ro 2 5), (Ro 1 3), (Ro 2 7), (Ro 1 4), (Ro 1 4), (Ro 1 4)],
  [Empty,       Empty,       Empty, (Rh True 1 4), (Rh True 1 3), (Rh False 1 2), (Rh False 1 1), Empty, Empty, Empty]]

modernTable :: [[Element]]
modernTable = [
  [hydrogen, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, helium],
  [lithium, beryllium, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, boron, carbon, Placeholder, Placeholder, Placeholder, Placeholder],
  [sodium, magnesium, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, aluminium, silicon, Placeholder, Placeholder, Placeholder, Placeholder],
  [potassium, calcium, scandium, titanium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, copper, zinc, gallium, germanium, Placeholder, Placeholder, Placeholder, Placeholder],
  [Placeholder, strontium, yttrium, zirconium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, silver, cadmium, indium, tin, Placeholder, Placeholder, Placeholder, Placeholder],
  [caesium, barium, lanthanum, hafnium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, mercury, thallium, lead, Placeholder, Placeholder, Placeholder, Placeholder],
  [francium, radium, actinium, rutherfordium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, roentgenium, copernicium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder]]

lanthanides = [cerium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder];
actinides = [thorium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder];
