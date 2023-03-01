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


--seguidores usuario1 usuarios =  filter seguido usuarios

--punto 4 ok
favear::Mensaje -> Mensaje
favear mensaje = mensaje { favs =favs mensaje + 1 }

editar:: String->Mensaje->Mensaje
editar comentario mensaje = mensaje {texto =  comentario ++ ( texto mensaje) }

repipear:: Usuario->Mensaje->Mensaje
repipear usuario1 mensaje = mensaje {favs = 0, usuario = (nombre usuario1), texto = usuario mensaje ++ ": " ++ texto mensaje}

-- punto 5
{-Los usuarios también dudan en que acción realizar sobre un mensaje que vieron. 
Por ejemplo, no sabe si repipearlo o favearlo, o tal vez editarlo, o todo junto…

Definir una función que dado un mensaje y dos posibles acciones, realice la que permite obtener 
el pip con la mayor valoración, obteniendo como respuesta el mensaje resultante. 
Mostrar dos ejemplos de invocación usando acciones diferentes. -}

mejorAccionAHacer mensaje accion1 accion2
    | valoracion (accion1 mensaje) > valoracion (accion2 mensaje) = accion1 mensaje
    |otherwise = accion2 mensaje


