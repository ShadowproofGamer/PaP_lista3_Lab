import scala.annotation.tailrec

def rootApprox(toRoot:Double, rootPower:Int, precision:Double):Double = {
  if math.floorMod(rootPower, 2)==0 && toRoot<0 then throw Error(s"brak rozwiazan rzeczywistych")
  @tailrec
  def rootApprox_rec(toR:Double, pow:Int, a:Double, b:Double, prec:Double):Double = {
    //println(s"$a : $b")
    val left:Double = toR - Math.pow(a, pow);
    val c:Double = (a+b)/2;
    val center:Double = toR - Math.pow(c, pow);
    if math.abs(a-b)<prec then return c
    if (center*left)<0 then rootApprox_rec(toR, pow, a, c, prec)
    else rootApprox_rec(toR, pow, c, b, prec)
  }
  if (math.abs(toRoot)<1) then rootApprox_rec(toRoot, rootPower, 0, 1, precision)
  rootApprox_rec(toRoot, rootPower, 1, toRoot, precision)
}
rootApprox(8, 3, 0.00001)