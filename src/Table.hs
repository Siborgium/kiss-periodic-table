
module Table where

import Elements

table :: [[Element]]
table = [[ hydrogen, Empty, Empty, Empty, Empty, Empty, hspecial, helium ],
         [ lithium, Placeholder, bohr],
         [ natrium,Placeholder,Placeholder,Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder, Placeholder],
         [ potassium, Placeholder,Placeholder,Placeholder, Placeholder, Placeholder, Placeholder],
         [copper, Placeholder,Placeholder,Placeholder, Placeholder, Placeholder, Placeholder],
         [rubidium, Placeholder,Placeholder,Placeholder, Placeholder, Placeholder, Placeholder],
         [silver, Placeholder,Placeholder,Placeholder, Placeholder, Placeholder, Placeholder],
         [cesium, Placeholder,Placeholder,Placeholder, Placeholder, Placeholder, Placeholder],
         [gold, Placeholder,Placeholder,Placeholder, Placeholder, Placeholder, Placeholder],
         [francium, Placeholder,Placeholder,Placeholder, Placeholder, Placeholder, Placeholder],
         [roentgenium, Placeholder,Placeholder,Placeholder, Placeholder, Placeholder, Placeholder],
         [(Ro 2 1), (Ro 1 1), (Ro 2 3), (Ro 1 2), (Ro 2 5), (Ro 1 3), (Ro 2 7), (Ro 1 4)],
         [HydroCompound],
         [cerium],
         [thorium]]
        
