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


santi::Usuario
santi= Usuario{nombre="santi",seguidos=["@don_churrasco","@titoOk"]}

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
seguidores u (x:xs) | elem (nombre u) (seguidos x) = (nombre x):seguidores u xs
                    | otherwise = seguidores u xs
seguidores u [] = []


--punto 4
favear::Mensaje->Mensaje
favear mensaje = mensaje{favs= (favs mensaje)+1}

editar::Texto->Mensaje->Mensaje
editar text mensaje = mensaje{texto= text ++ (texto mensaje ) }

-- repipear
--Definir una función repipear un mensaje por parte de otro usuario, que devuelve el nuevo mensaje con 0 favoritos,
 --y cuyo contenido sea como el del mensaje original, sólo que editado automáticamente con el nombre del usuario original 
 --y un  ": " al principio.

repipear::Usuario->Mensaje->Mensaje
repipear usu mens = mens{ usuario= (nombre usu),texto =(usuario mens) ++ ": " ++ (texto mens), favs=0} 

--Punto 5 
type Accion = Mensaje->Mensaje


masValoracion::Mensaje->Accion->Accion->Mensaje
masValoracion mensaje a1 a2 | valoracion(a1 mensaje) > valoracion (a2 mensaje) = a1 mensaje
                            | valoracion(a1 mensaje) < valoracion (a2 mensaje) = a2 mensaje
                            | otherwise = a1 mensaje -- si son iguales realizo la primera accion que se le paso 



--a1 y a2 deben ser funciones de Mensaje->Mensaje,, uso aplicacion Parcial en editar y repipear para que tengan que recibir un Mensaje como parametro
-- si hago     masValoracion mensaje favear (editar "hola") 
-- me devuelve el mensaje     Mensaje {usuario = "@titoOk", texto = "Las personas que viajan en tren se quejan de llenos", favs = 101}
-- ya que la valoracion de favear es mayor a la de (editar "hola")


--si hago  masValoracion mensaje (repipear churrasco) (editar "argentina") 
-- me devuelve el mensaje Mensaje{usuario = "@titoOk", texto = "argentina Las personas que viajan en tren se quejan de llenos", favs = 100}
--ya que la valoracion de (repipear churrasco) es menor a la de (editar "argentina")
