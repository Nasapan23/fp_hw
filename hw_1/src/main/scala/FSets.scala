object FSets {

  type Set = Int => Boolean

  def tokenID: Int = 293544

  def member(e: Int)(s: Set): Boolean = s(e)

  def singleton(x: Int): Set = {
    (e:Int) => x==e
  }

  def ins(x: Int)(s: Set): Set = {
    val xt = singleton(x)
    (e:Int) => xt(e) || s(e)
  }

  def fromBounds(start: Int, stop: Int): Set = {
    (e:Int) => e>=start && e<= stop
  }

  def union (s1: Set, s2: Set): Set = {
    (e:Int) => s1(e) || s2(e)
  }

  def complement(s1: Set): Set = {
    (e:Int) => !s1(e)
  }

  def sumSet(b: Int)(start: Int, stop: Int)(s: Set): Int = {
    def aux(crt: Int, acc: Int): Int = {
      if (crt>stop) acc
      else if (s(crt)) aux(crt + 1, acc + crt)
      else aux(crt + 1, acc)
    }
    aux(start,b)
  }

  def foldLeftSet(b:Int)(op: (Int,Int) => Int)(start: Int, stop: Int)(s: Set): Int = {
    def aux(crt: Int, acc: Int): Int = {
      if(crt>stop) acc
      else if (s(crt)) aux(crt+1,op(acc,crt))
      else aux(crt+1,acc)
    }
    aux(start,b)
  }

  def foldRightSet(b:Int)(op: (Int,Int) => Int)(start: Int, stop: Int)(s: Set): Int = { //nu mai e nevoie de acc
    def aux(crt:Int) : Int = {
      if(crt<start) b
      else if (s(crt)) op(crt,aux(crt-1))
      else aux(crt-1)
    }
    aux(stop)
  }

  def filter(p: Int => Boolean)(s: Set): Set = {
    (e:Int) => p(e) && s(e)
  }

  def partition(p: Int => Boolean)(s: Set): (Set, Set) = {
    (filter(p)(s), filter(e => !p(e))(s))
  }


  def forall(cond: Int => Boolean)(start: Int, stop: Int)(s: Set): Boolean = {
    def aux(crt: Int): Boolean = {
      if (crt > stop) true
      else if (s(crt) && !cond(crt)) false
      else aux(crt + 1)
    }

    aux(start)
  }

  def exists(cond: Int => Boolean)(start: Int, stop: Int)(s: Set): Boolean = {
    !forall(x => !cond(x))(start, stop)(s)
  }

  def setOfDivByK(k: Int): Set = {
    (e: Int) => e % k == 0
  }

  def moreDivs(k: Int)(start: Int, stop: Int)(s1: Set, s2: Set): Boolean = {
    def countDivs(s: Set): Int = {
      def aux(crt: Int, acc: Int): Int =
        if (crt > stop) acc
        else if (s(crt) && crt % k == 0) aux(crt + 1, acc + 1)
        else aux(crt + 1, acc)

      aux(start, 0)
    }
    countDivs(s1) > countDivs(s2)
  }

}
