import Text.Show.Functions ()

data Persona = Persona {
    nombre           :: String,
    calorias         :: Int,
    hidratacion      :: Int,
    tiempoDisponible :: Int,
    equipamientos    :: [String]
} deriving Show

data Rutina = Rutina {
    duracion   :: Int,
    ejercicios :: [Ejercicio]
} deriving Show

type Ejercicio = (Persona -> Persona)

pierdeCalorias :: Int -> Persona -> Persona
pierdeCalorias cantidadPerdida unaPersona = unaPersona { calorias = calorias unaPersona - cantidadPerdida}

pierdeHidratacion :: Int -> Persona -> Persona
pierdeHidratacion cantidadPerdida unaPersona = unaPersona { hidratacion = hidratacion unaPersona - cantidadPerdida}

tieneElemento :: String -> Persona -> Bool
tieneElemento unEquipamiento unaPersona = elem unEquipamiento (equipamientos unaPersona)

abdominales :: Int -> Ejercicio
abdominales unasRepeticiones unaPersona = pierdeCalorias (8 * unasRepeticiones) unaPersona

flexiones :: Int -> Ejercicio
flexiones unasRepeticiones unaPersona = pierdeHidratacion ((unasRepeticiones `div` 10) * 2) . pierdeCalorias (16 * unasRepeticiones) $ unaPersona

levantarPesas :: Int -> Int -> Ejercicio
levantarPesas unPeso unasRepeticiones unaPersona 
    |tieneElemento "pesa" unaPersona = pierdeHidratacion ((unasRepeticiones `div` 10) * unPeso) . pierdeCalorias (32 * unasRepeticiones) $ unaPersona
    |otherwise                      = unaPersona

laGranHomeroSimpson :: Ejercicio
laGranHomeroSimpson unaPersona = unaPersona

renovarEquipo :: Persona -> Persona
renovarEquipo unaPersona = agregarNuevo unaPersona

agregarNuevo :: Persona -> Persona
agregarNuevo unaPersona = unaPersona { equipamientos = "Nuevo " : equipamientos unaPersona}

volverseYoguista :: Persona -> Persona 
volverseYoguista unaPersona = agregaEquipamiento "colchoneta" . vendeTodoEquipamiento . pierdeCalorias (calorias unaPersona `div` 2) . cambioDeHidratacion $ unaPersona

cambioDeHidratacion :: Persona -> Persona
cambioDeHidratacion unaPersona
    |(> 100) (hidratacion unaPersona * 2)  = maximizaHidratacion unaPersona
    |otherwise                        = unaPersona { hidratacion = hidratacion unaPersona * 2}

vendeTodoEquipamiento :: Persona -> Persona 
vendeTodoEquipamiento unaPersona = unaPersona { equipamientos = []}

agregaEquipamiento :: String -> Persona -> Persona
agregaEquipamiento unElemento unaPersona = unaPersona { equipamientos = unElemento : equipamientos unaPersona}

volverseBodyBuilder :: Persona -> Persona 
volverseBodyBuilder unaPersona 
    |all (== "pesa") (equipamientos unaPersona) = agregarSufijo "BB" . aumentaCalorias (calorias unaPersona * 3) $ unaPersona
    |otherwise                                  = unaPersona

aumentaCalorias :: Int -> Persona -> Persona
aumentaCalorias unAumento unaPersona = unaPersona { calorias = calorias unaPersona + unAumento}

agregarSufijo :: String -> Persona -> Persona
agregarSufijo elSufijo unaPersona = unaPersona { nombre = nombre unaPersona ++ elSufijo}

comerUnSandwich :: Persona -> Persona
comerUnSandwich unaPersona = aumentaCalorias 500 . maximizaHidratacion $ unaPersona

maximizaHidratacion :: Persona -> Persona
maximizaHidratacion unaPersona = unaPersona { hidratacion = 100}

elAbominableAbdominal :: Rutina
elAbominableAbdominal = Rutina {duracion = 1, ejercicios = map abdominales [1..]}

puedeRealizarRutina :: Rutina -> Persona -> Bool
puedeRealizarRutina unaRutina unaPersona = tiempoDisponible unaPersona >= duracion unaRutina

hacerRutina :: Rutina -> Persona -> Persona
hacerRutina unaRutina unaPersona = foldr ($) unaPersona (ejercicios unaRutina)

esPeligrosa :: Rutina -> Persona -> Bool
esPeligrosa unaRutina unaPersona = personaAgotada . hacerRutina unaRutina $ unaPersona

personaAgotada :: Persona -> Bool
personaAgotada unaPersona =  calorias unaPersona < 50 && hidratacion unaPersona < 10

esBalanceada :: Rutina -> Persona -> Bool
esBalanceada unaRutina unaPersona = comparadorHidratacionYCalorias unaPersona (hacerRutina unaRutina unaPersona)

comparadorHidratacionYCalorias :: Persona -> Persona -> Bool
comparadorHidratacionYCalorias unaPersona laPersonaConLaRutinaHecha = calorias unaPersona < (calorias laPersonaConLaRutinaHecha) `div` 2 && hidratacion laPersonaConLaRutinaHecha > 80

seleccionarGrupoDeEjercicio :: Persona -> [Persona] -> [Persona]
seleccionarGrupoDeEjercicio unaPersona unasPersonas = filter (tienenElMismoTiempoDisponible unaPersona) unasPersonas

tienenElMismoTiempoDisponible :: Persona -> Persona -> Bool
tienenElMismoTiempoDisponible unaPersona otraPersona = tiempoDisponible unaPersona == tiempoDisponible otraPersona

promedioDeRutina :: Rutina -> [Persona] -> (Int, Int)
promedioDeRutina unaRutina unasPersonas = (promedioCalorias (map (hacerRutina unaRutina) unasPersonas), indiceHidratacion (map (hacerRutina unaRutina) unasPersonas))

promedioCalorias :: [Persona] -> Int
promedioCalorias unasPersonas = sum (map calorias unasPersonas) `div` length unasPersonas

indiceHidratacion :: [Persona] -> Int 
indiceHidratacion unasPersonas = sum (map hidratacion unasPersonas)