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

module Table where

import Elements

table :: [[Element]]
table = [[ hydrogen, Empty, Empty, Empty, Empty, Empty, hspecial, helium ],
         [ lithium, beryllium, boron],
         [ natrium, magnesium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder],
         [ potassium, calcium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder],
         [copper, zinc, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder],
         [rubidium, strontium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder],
         [silver, cadmium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder],
         [cesium, barium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder],
         [gold, mercury, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder],
         [francium, radium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder],
         [roentgenium, copernicium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder],
         [(Ro 2 1), (Ro 1 1), (Ro 2 3), (Ro 1 2), (Ro 2 5), (Ro 1 3), (Ro 2 7), (Ro 1 4)],
         [HydroCompound]]

lanthanides = [cerium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder];
actinides = [thorium];
