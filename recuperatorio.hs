type Nombre = String

data Usuario = Usuario{
    nombre ::Nombre,
    seguidos::[Nombre]}
    deriving Show

--Ejemplos de Usuarios
marsupial::Usuario
marsupial = Usuario {nombre="@marsupialRengo",seguidos=["@don_churrasco", "@titoOk"]}

churrasco::Usuario
churrasco = Usuario {nombre="@don_churrasco",seguidos= []}

tito::Usuario
tito = Usuario {nombre="@titoOk",seguidos= ["@lapipi", "@marsupialRengo"]}

pipi::Usuario
pipi = Usuario {nombre="@lapipi",seguidos=["@titoOk"]}


type Texto = String
type Cantidad = Int

data Mensaje = Mensaje {
    usuario :: Nombre,
    texto :: Texto,
    favs :: Cantidad }
    deriving Show

mensaje::Mensaje
mensaje = Mensaje {usuario="@titoOk",texto= "Las personas que viajan en tren se quejan de llenos",favs= 100}

mensaje2::Mensaje
mensaje2 = Mensaje {usuario="@lapipi",texto="En mi mundo todos son un pony",favs= 0}

mensaje3::Mensaje
mensaje3 = Mensaje {usuario="@lapipi",texto="y comen arcoiris y su popó son mariposas",favs=0}

mensaje4::Mensaje
mensaje4 = Mensaje {usuario="@titoOk",texto= "No hay problema en cometer errores, el secreto es no pasarlos a producción",favs=3}


--Punto 1 

valoracion::Mensaje->Int
valoracion m = (favs m)-(length(usuario m)+length (texto m))

--Punto 2
seSiguen::Usuario->Usuario->Bool
seSiguen p1 p2 = elem (nombre p1) (seguidos p2)

--Punto 3
seguidores::Usuario->[Usuario]->[Nombre]
seguidores u (x:xs) | elem (nombre x) (seguidos u) = (nombre x):seguidores u xs
                    | otherwise = seguidores u xs
seguidores u [] = []


--punto 4
favear::Mensaje->Mensaje
favear mensaje = mensaje{favs= (favs mensaje)+1}

editar::Texto->Mensaje->Mensaje
editar text mensaje = mensaje{texto= text ++ (texto mensaje ) }
