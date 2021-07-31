def square(x: Int, y: Int) = x * x

// by deafult call by value
square(7,2)  // takes less time
square(6+1,2+1) // takes more time as 2nd para also get evaluated

// change evalaution staregy to CBN
def square2(x: Int, y : => Int) = x * x
square(6+1,2+1)  // do not evaluate 2nd para as not needed


