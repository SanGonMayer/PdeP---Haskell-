import Text.Show.Functions ()
import Data.List (maximumBy)
import Data.Ord (comparing)

data Carrera = Carrera{
    vueltas         :: Int,
    longitudDePista :: Int,
    participantes   :: [Auto],
    publico         :: [Publico]
} deriving Show

data Auto = UnAuto {
    nombre      :: Nombre,
    nafta       :: Int,
    velocidad   :: Int,
    enamorado   :: NombreEnamorado,
    trucos      :: Truco
} deriving Show

type Nombre          = String
type Publico         = String
type NombreEnamorado = String
type Truco           = (Auto -> Auto)

copaPiston :: Carrera
copaPiston = Carrera {vueltas = 1, longitudDePista = 4, participantes = [rochaMcQueen, biankerr, gushtav, rodra], publico = ["Tincho", "Peti"]}

rochaMcQueen :: Auto
rochaMcQueen = UnAuto {nombre = "RochaMcQueen", nafta = 282, velocidad = 0, enamorado = "Ronco",  trucos = deReversaRocha copaPiston}
biankerr :: Auto
biankerr     = UnAuto {nombre = "Biankerr",     nafta = 378, velocidad = 0, enamorado = "Tincho", trucos = impresionar copaPiston}
gushtav :: Auto
gushtav      = UnAuto {nombre = "Gushtav",      nafta = 230, velocidad = 0, enamorado = "Peti",   trucos = nitro}
rodra :: Auto
rodra        = UnAuto {nombre = "Rodra",        nafta = 153, velocidad = 0, enamorado = "Tais",   trucos = comboLoco copaPiston}


deReversaRocha :: Carrera -> Truco
deReversaRocha unaCarrera unAuto = cambiaCombustible (5 * (longitudDePista unaCarrera)) unAuto 

cambiaCombustible :: Int -> Auto -> Auto
cambiaCombustible unosMetros unAuto = unAuto {nafta = nafta unAuto + unosMetros}

impresionar :: Carrera -> Truco
impresionar unaCarrera unAuto 
    | elem (enamorado unAuto) (publico unaCarrera) = aumentaVelocidad unAuto (*) 2
    | otherwise                                    = unAuto

nitro :: Truco
nitro unAuto = aumentaVelocidad unAuto (+) 15

aumentaVelocidad :: Auto -> (Int -> Int -> Int) -> Int -> Auto
aumentaVelocidad unAuto unaOperacion unAumento = unAuto { velocidad = velocidad unAuto `unaOperacion` unAumento}

comboLoco :: Carrera -> Truco
comboLoco unaCarrera unAuto = nitro . deReversaRocha unaCarrera $ unAuto

darVuelta :: Carrera -> Carrera
darVuelta unaCarrera = modificaAutos (realizaTruco . incrementaVelocidad . restaDeCombustible (longitudDePista unaCarrera)) unaCarrera

restaDeCombustible :: Int -> [Auto] -> [Auto]
restaDeCombustible unaLongitud unosAutos = map (modificaNaftas unaLongitud) unosAutos 

modificaNaftas :: Int -> Auto -> Auto
modificaNaftas unaLongitud unAuto = cambiaCombustible (-(longitudNombre unAuto) * unaLongitud) unAuto

longitudNombre :: Auto -> Int
longitudNombre unAuto = length (nombre unAuto)

incrementaVelocidad :: [Auto] -> [Auto]
incrementaVelocidad unosAutos = map (cambiaVelocidad) unosAutos

cambiaVelocidad :: Auto -> Auto
cambiaVelocidad unAuto
    | (>8)  . longitudNombre $ unAuto = aumentaVelocidad unAuto (+) 30
    | (>=6) . longitudNombre $ unAuto = aumentaVelocidad unAuto (+) 20
    | (>1)  . longitudNombre $ unAuto = aumentaVelocidad unAuto (+) 15
    | otherwise                       = unAuto

realizaTruco :: [Auto] -> [Auto]
realizaTruco unosAutos = map (esElMasLento unosAutos) unosAutos

esElMasLento :: [Auto] -> Auto -> Auto
esElMasLento unosAutos unAuto
    | vaMasDespacio unAuto unosAutos = aplicarTruco unAuto
    | otherwise                      = unAuto

vaMasDespacio :: Auto -> [Auto] -> Bool
vaMasDespacio unAuto unosAutos = velocidad unAuto == (minimum . listaVelocidades $ unosAutos)

listaVelocidades :: [Auto] -> [Int]
listaVelocidades unosAutos = map velocidad unosAutos

aplicarTruco :: Auto -> Auto
aplicarTruco unAuto = (trucos unAuto) unAuto

modificaAutos :: ([Auto] -> [Auto]) -> Carrera -> Carrera
modificaAutos laFuncionQueModificaAutos unaCarrera = unaCarrera {participantes = laFuncionQueModificaAutos (participantes unaCarrera)}

correrCarrera :: Carrera -> Carrera
correrCarrera unaCarrera = foldr (\_ carreraAcumulada -> darVuelta carreraAcumulada) unaCarrera [1..vueltas unaCarrera]

ganadorCarrera :: Carrera -> Auto
ganadorCarrera unaCarrera = buscarGanador . correrCarrera $ unaCarrera

buscarGanador :: Carrera -> Auto
buscarGanador unaCarrera = maximumBy (comparing velocidad) (participantes unaCarrera)

recompetidores :: Carrera -> [Auto]
recompetidores unaCarrera = ganador unaCarrera : (controladorDeNafta . correrCarrera $ unaCarrera)

controladorDeNafta :: Carrera -> [Auto]
controladorDeNafta unaCarrera = filter medidorNafta (participantes unaCarrera)

medidorNafta :: Auto -> Bool
medidorNafta unAuto = nafta unAuto >= 27 

ganador :: Carrera -> Auto
ganador unaCarrera = buscarGanador unaCarrera



