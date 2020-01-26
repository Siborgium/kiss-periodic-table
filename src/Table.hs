
module Table where

import Elements

table :: [[Element]]
table = [[ hydrogen, Empty, Empty, Empty, Empty, Empty, hspecial, helium ],
         [ lithium, beryllium, bohr],
         [ natrium, magnesium,Placeholder,Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder],
         [ potassium, calcium,Placeholder,Placeholder, Placeholder, Placeholder, Placeholder],
         [copper, zinc, Placeholder,Placeholder, Placeholder, Placeholder, Placeholder],
         [rubidium, strontium, Placeholder,Placeholder, Placeholder, Placeholder, Placeholder],
         [silver, cadmium, Placeholder,Placeholder, Placeholder, Placeholder, Placeholder],
         [cesium, barium, Placeholder,Placeholder, Placeholder, Placeholder, Placeholder],
         [gold, quicksilver,Placeholder,Placeholder, Placeholder, Placeholder, Placeholder],
         [francium, radium, Placeholder,Placeholder, Placeholder, Placeholder, Placeholder],
         [roentgenium, copernicium, Placeholder,Placeholder, Placeholder, Placeholder, Placeholder],
         [(Ro 2 1), (Ro 1 1), (Ro 2 3), (Ro 1 2), (Ro 2 5), (Ro 1 3), (Ro 2 7), (Ro 1 4)],
         [HydroCompound]]

lanthanides = [cerium, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder];
actinides = [thorium];
