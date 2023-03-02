data Usuario = Usuario {
	nombre :: Nombre,
	seguidos :: [Nombre]
} deriving Show
type Nombre = String

marsupial = Usuario "@marsupialRengo" ["@don_churrasco", "@titoOk"]
churrasco = Usuario "@don_churrasco" []
tito = Usuario "@titoOk" ["@lapipi", "@marsupialRengo"]
pipi = Usuario "@lapipi" ["@titoOk"]

data Mensaje = Mensaje {
usuario :: Nombre,
texto :: Texto,
favs :: Cantidad 
} deriving Show
type Texto = String
type Cantidad = Int

unMensaje = Mensaje "@titoOk" "Las personas que viajan en tren se quejan de llenos" 100
otroMensaje = Mensaje "@lapipi" "En mi mundo todos son un pony" 0
otroMensajeMas = Mensaje "@lapipi" "y comen arcoiris y su popó son mariposas" 0
elUltimoMensaje = Mensaje "@titoOk" "No hay problema en cometer errores, el secreto es no pasarlos a producción" 3

--punto 1 ???
valoracion:: Mensaje->Int
valoracion mensaje = favs mensaje - cantidadCaracteresTextoUsuario mensaje
cantidadCaracteresTextoUsuario mensaje = (length (usuario mensaje) + length (texto mensaje))

--punto 2 ok

sigueA :: Usuario -> Usuario -> Bool
sigueA usuario1 usuario2 =  elem  (nombre usuario2) (seguidos usuario1)

--punto 3

seguidores :: Usuario -> [Usuario] -> [Nombre]
seguidores usuario lista = map  nombre (filter (sigueA usuario) lista)

--seguidores usuario1 usuarios =  filter seguido usuarios

--punto 4 ok
favear::Mensaje -> Mensaje
favear mensaje = mensaje { favs =favs mensaje + 1 }

editar:: String->Mensaje->Mensaje
editar comentario mensaje = mensaje {texto =  comentario ++ " " ++ ( texto mensaje) }

repipear:: Usuario->Mensaje->Mensaje
repipear usuario1 mensaje = mensaje {favs = 0, usuario = (nombre usuario1), texto = usuario mensaje ++ ": " ++ texto mensaje}

-- punto 5
{-Los usuarios también dudan en que acción realizar sobre un mensaje que vieron. 
Por ejemplo, no sabe si repipearlo o favearlo, o tal vez editarlo, o todo junto…

Definir una función que dado un mensaje y dos posibles acciones, realice la que permite obtener 
el pip con la mayor valoración, obteniendo como respuesta el mensaje resultante. 
Mostrar dos ejemplos de invocación usando acciones diferentes. -}
type Accion = Mensaje->Mensaje
mejorAccionAHacer::Mensaje->Accion->Accion->Mensaje
mejorAccionAHacer mensaje accion1 accion2
    | valoracion (accion1 mensaje) > valoracion (accion2 mensaje) = accion1 mensaje --si es mayor valoracion accion 1 que accion2
    |valoracion (accion1 mensaje) < valoracion (accion2 mensaje) = accion2 mensaje  --si es menor valoracion accion 1 que accion2
    |otherwise = accion1 mensaje                                                    -- si son iguales que me retorne la primera

{- ejemplo 1
 mejorAccionAHacer unMensaje favear (editar "hola")
devuelve  Mensaje {usuario = "@titoOk", texto = "Las personas que viajan en tren se quejan de llenos", favs = 101}

ya que las acciones me devulven un mensaje
como la valoracion mensaje = favs mensaje - cantidadCaracteresTextoUsuario mensaje
favear un mensaje me retorna favs=101  y texto 51
editar mensaje retorna favs=100 y texto 56

como la valoracion es la cantidad de favs menos la cantidadCaracteresTextoUsuario
(101-51)>(100-55) luego favear tiene mas valoracion

ejemplo2
 mejorAccionAHacer unMensaje (editar "hola") (repipear churrasco)
devulve Mensaje {usuario = "@titoOk", texto = "hola Las personas que viajan en tren se quejan de llenos", favs = 100}
 editar "hola" me devulve un mensaje con favs=100 texto = 56
 ripiar tito unMensaje me devuelve favs=0 texto = 51+2+60

 como (100-55)>(0-51-2-60) luego se elige editarlo

 -}

