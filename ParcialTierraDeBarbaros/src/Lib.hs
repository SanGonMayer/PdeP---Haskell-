import Text.Show.Functions ()
import Data.Char (toUpper)
import Data.Char (isUpper)


data Barbaro = Barbaro{
    nombre      :: String,
    fuerza      :: Int,
    habilidades :: Habilidad,
    objetos     :: [Objeto]
}deriving Show

type Objeto = (Barbaro -> Barbaro)
type Habilidad = [String]

dave = Barbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas]

espada :: Int -> Objeto
espada unPeso unBarbaro = unBarbaro {fuerza = fuerza unBarbaro + unPeso * 2}

amuletosMisticos :: String -> Objeto
amuletosMisticos unaHabilidad unBarbaro = agregarHabilidad unaHabilidad unBarbaro

agregarHabilidad :: String -> Barbaro -> Barbaro
agregarHabilidad unaHabilidad unBarbaro = unBarbaro {habilidades = unaHabilidad : habilidades unBarbaro } 

varitasDefectuosas :: Objeto 
varitasDefectuosas unBarbaro = sacarObjetos . agregarHabilidad "magia" $ unBarbaro 

sacarObjetos :: Objeto
sacarObjetos unBarbaro = unBarbaro {objetos = [varitasDefectuosas]}

ardilla :: Objeto
ardilla unBarbaro = unBarbaro

cuerda :: Objeto -> Objeto -> Objeto
cuerda unObjeto otroObjeto = unObjeto . otroObjeto

--2
megafono :: Objeto
megafono unBarbaro = unBarbaro {habilidades = [concatMap pasaAMayusculas (habilidades unBarbaro)]}

pasaAMayusculas :: String -> String
pasaAMayusculas habilidad = map toUpper habilidad

--3
invasionDeSuciosDuendes :: Barbaro -> Bool
invasionDeSuciosDuendes unBarbaro = tieneHabilidad "escribri poesia atroz" unBarbaro

tieneHabilidad :: String -> Barbaro -> Bool
tieneHabilidad unaHabilidad unBarbaro = elem unaHabilidad (habilidades unBarbaro)

cremalleraDelTiempo :: Barbaro -> Bool
cremalleraDelTiempo unBarbaro = es "Faffy" unBarbaro|| es "Astro" unBarbaro

es :: String -> Barbaro -> Bool
es unNombre unBarbaro = unNombre == (nombre unBarbaro)

type Evento = (Barbaro -> Bool)
ritualDeFechorias :: Evento 
ritualDeFechorias unBarbaro = saqueo unBarbaro || gritoDeGuerra unBarbaro || caligrafia unBarbaro

saqueo :: Evento
saqueo unBarbaro = tieneHabilidad "robar" unBarbaro || (fuerza unBarbaro) >80

gritoDeGuerra :: Evento
gritoDeGuerra unBarbaro = poderDeGritoDeGuerra unBarbaro

poderDeGritoDeGuerra :: Barbaro -> Bool
poderDeGritoDeGuerra unBarbaro = cantidadLetrasHabilidades unBarbaro == 4 * length (objetos unBarbaro)

cantidadLetrasHabilidades :: Barbaro -> Int
cantidadLetrasHabilidades unBarbaro = length . habilidades . megafono $ unBarbaro

caligrafia :: Evento
caligrafia unBarbaro = cantidadDeVocales unBarbaro >3 && comienzaConMayuscula unBarbaro

cantidadDeVocales :: Barbaro -> Int
cantidadDeVocales unBarbaro = sumaDeVocales . concat . habilidades $ unBarbaro

sumaDeVocales :: String -> Int
sumaDeVocales habilidadesConcatenadas = length (filter esVocal habilidadesConcatenadas)

esVocal :: Char -> Bool
esVocal caracteresHabilidades = elem caracteresHabilidades ['a','e','i','o','u','A','E','I','O','U']

comienzaConMayuscula :: Barbaro -> Bool
comienzaConMayuscula unBarbaro = all (isUpper . head) (habilidades unBarbaro)

--4
sinHabilidadRepetida :: Barbaro -> [String]
sinHabilidadRepetida unBarbaro = buscaHabilidadRepetida . habilidades $ unBarbaro

buscaHabilidadRepetida :: [String] -> [String]
buscaHabilidadRepetida [] = []
buscaHabilidadRepetida (cabeza : cola)
    | elem cabeza cola = buscaHabilidadRepetida cola
    | otherwise        = cabeza : buscaHabilidadRepetida cola

descendientes :: Barbaro -> [Barbaro]
descendientes unBarbaro = 