// here abnormal termination is aborted by Nothing
def error(msg: String) = throw new Error("this is error")

//error("errr")

// Null
// Null is subtype of anyRef not anyVal
val x = null
val y: String = x

// following is not valid expression as Null is not subtype of Int
//val z : Int = x

if(true) 1 else false