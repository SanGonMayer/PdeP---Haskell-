import Text.Show.Functions ()

data Chico = Chico{
    nombre      :: String,
    edad        :: Int,
    habilidades :: Habilidad,
    deseos      :: [Deseo]
} deriving Show

type Deseo     = (Chico -> Chico)
type Habilidad = [String]

timmy :: Chico
timmy = Chico {nombre = "timmy", edad = 10, habilidades = ["mirar television", "jugar en la pc"], deseos = [serMayor]}
nadie :: Chico
nadie = Chico {nombre = "nadie", edad = 10, habilidades = ["mirar television", "jugar en la pc"], deseos = [serMayor]}
linguini :: Chico
linguini = Chico {nombre = "linguini", edad = 20, habilidades = ["mirar television", "jugar en la pc", "cocinar"], deseos = [serMayor]}

--A 1.
aprenderHabilidades :: Habilidad -> Chico -> Chico
aprenderHabilidades unasHabilidades unChico = agregarHabilidad unasHabilidades unChico

serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed unChico = agregarHabilidad (map (\unNumero -> ("jugar need for speed " ++ show unNumero)) [1..]) unChico

agregarHabilidad :: Habilidad -> Chico -> Chico
agregarHabilidad unasHabilidades unChico = unChico {habilidades = unasHabilidades ++ habilidades unChico }

serMayor :: Deseo
serMayor unChico = unChico {edad = 18}

--A 2.
wanda :: Chico -> Chico
wanda unChico = (madura . cumplePrimerDeseo (head (deseos unChico))) unChico

cumplePrimerDeseo :: Deseo -> Chico -> Chico
cumplePrimerDeseo unDeseo unChico = unDeseo unChico {deseos = tail (deseos unChico)}

madura :: Chico -> Chico
madura unChico = unChico {edad = edad unChico + 1 }

cosmo :: Chico -> Chico
cosmo unChico = desmadurar unChico

desmadurar :: Chico -> Chico
desmadurar unChico = unChico {edad = div (edad unChico) 2}

muffinMagico :: Chico -> Chico
muffinMagico unChico = foldr (cumplePrimerDeseo) unChico (deseos unChico)

--B 1
tieneHabilidad :: String -> Chico -> Bool
tieneHabilidad unaHabilidad unChico = unaHabilidad `elem` habilidades unChico

esSuperMaduro :: Chico -> Bool
esSuperMaduro unChico = esMayorDeEdad unChico && sabeManejar unChico

esMayorDeEdad :: Chico -> Bool
esMayorDeEdad unChico = edad unChico > 18

sabeManejar :: Chico -> Bool
sabeManejar unChico = tieneHabilidad "manejar" unChico

--B 2
data Chica = Chica{
    name                :: String,
    condicionParaElegir :: Condicion
} deriving Show
type Condicion = (Chico -> Bool)

trixie :: Chica
trixie = Chica {name = "trixie", condicionParaElegir = noEsTimmy}
vicky :: Chica
vicky = Chica {name = "vicky", condicionParaElegir = (tieneHabilidad "ser un supermodelo noruego")}
lola :: Chica
lola = Chica {name = "lola", condicionParaElegir = (tieneHabilidad "cocinar")}

type Chicos = [Chico]

noEsTimmy :: Chico -> Bool
noEsTimmy unChico = (nombre unChico) /= "timmy"

--quienConquistaA ::  Chica -> Condicion -> Chicos -> Chico
--quienConquistaA unaChica unaCondicion [] = []
--quienConquistaA unaChica unaCondicion (chico:otrosChicos) 
-- | unaCondicion chico = chico
-- | otherwise          = quienConquistaA unaChica unaCondicion otrosChicos
--quienConquistaA unaCondicion unaChica (chico:[]) = chico

quienConquistaA ::  Chica -> Condicion -> Chicos -> Chico
quienConquistaA unaChica unaCondicion [unChico] = unChico
quienConquistaA unaChica unaCondicion (unChico:otrosChicos)
 | unaCondicion unChico = unChico
 | otherwise            = quienConquistaA unaChica unaCondicion otrosChicos

 --C 1
infractoresDeDaRules :: [Chico] -> [String]
infractoresDeDaRules unosChicos = map nombre . filter deseoProhibido $ unosChicos 

deseoProhibido :: Chico -> Bool
deseoProhibido unChico = tieneHabilidadesProhibidas . habilidades . muffinMagico $ unChico

tieneHabilidadesProhibidas :: Habilidad -> Bool
tieneHabilidadesProhibidas unasHabilidades = any habilidadProhibida (take 5 unasHabilidades)

habilidadProhibida :: String -> Bool
habilidadProhibida unaHabilidad = elem unaHabilidad ["enamorar","matar","dominar el mundo"]

