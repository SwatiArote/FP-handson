package basics

object Maps extends App {
    val map = Map(1 -> "a", 2 -> "b", 3 ->"c", 4->"4")
    println(map.get(5)) // it is safe one as get will give Option
   // println(map(5))  //NoSuchElementException
    // can handle with deafult value
    val mapWithDefault = map.withDefaultValue("unknown key")
    println(mapWithDefault(5))
}
