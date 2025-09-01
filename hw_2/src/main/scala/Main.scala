abstract class WTree extends WTreeInterface {
  override def filter(pred: Token => Boolean): WTree = ???
  def filterAux(pred: Token => Boolean, acc: WTree): WTree

}

case object Empty extends WTree {
  override def balance: Int = 0
  override def height: Int = 0
  override def rotateLeft: WTree = this
  override def rotateRight: WTree = this
  override def rotateRightLeft: WTree = this
  override def rotateLeftRight: WTree = this
  override def rebalance: WTree = this

  override def isEmpty = true
  override def ins(w: Token): WTree = Node(w, Empty, Empty)
  override def filterAux(pred: Token => Boolean, acc: WTree): WTree = ???
  override def size: Int = ???
  override def contains(s: String): Boolean = ???

}

case class Node(word: Token, left: WTree, right: WTree) extends WTree {

  override def balance: Int = right.height - left.height
  override def height: Int = 1 + (left.height max right.height)
  override def rotateLeft: WTree =
    right match {
      // the tree is unbalanced, hence the right sub-tree is nonempty
      case Node(w, l, r) => Node(w, Node(word, left, l), r)
    }
  override def rotateRight: WTree =
    left match {
      case Node(w, l, r) => Node(w, l, Node(word, r, right))
    }
  override def rotateRightLeft: WTree =
    Node(word, left, right.rotateRight).rotateLeft
  override def rotateLeftRight: WTree =
    Node(word, left.rotateLeft, right).rotateRight
  override def rebalance: WTree = {
    if (balance < -1 && left.balance == -1) this.rotateRight
    else if (balance > 1 && right.balance == 1) this.rotateLeft
    else if (balance < -1 && left.balance == 1) this.rotateLeftRight
    else if (balance > 1 && right.balance == -1) this.rotateRightLeft
    else this
  }

  override def isEmpty = false

  override def ins(w: Token): WTree =
    if (w.freq > word.freq) Node(word, left, right.ins(w))
    else Node(word, left.ins(w), right)

  override def contains(s: String): Boolean = ???

  override def size: Int = ???

  def filterAux(pred: Token => Boolean, acc: WTree): WTree = ???
}


object Main {

  def tokenID:Int = 293544

  val scalaDescription: String = "Scala is a strong statically typed general-purpose programming language which supports both object-oriented programming and functional programming designed to be concise many of Scala s design decisions are aimed to address criticisms of Java Scala source code can be compiled to Java bytecode and run on a Java virtual machine. Scala provides language interoperability with Java so that libraries written in either language may be referenced directly in Scala or Java code like Java, Scala is object-oriented, and uses a syntax termed curly-brace which is similar to the language C since Scala 3 there is also an option to use the off-side rule to structure blocks and its use is advised martin odersky has said that this turned out to be the most productive change introduced in Scala 3 unlike Java, Scala has many features of functional programming languages like Scheme, Standard ML, and Haskell, including currying, immutability, lazy evaluation, and pattern matching it also has an advanced type system supporting algebraic data types, covariance and contravariance, higher-order types (but not higher-rank types), and anonymous types other features of Scala not present in Java include operator overloading optional parameters named parameters and raw strings conversely a feature of Java not in Scala is checked exceptions which has proved controversial"

  /* Split the text into chunks */
  def split(text: List[Char]): List[List[Char]] = {
    def aux(text: List[Char]): List[List[Char]] = text match {
      case Nil =>
        // capăt de listă -> întoarcem un chunk gol
        List(Nil)

      case ' ' :: xs =>
        val chunks = aux(xs)
        chunks match {
          case Nil => List(Nil)
          case h :: t =>
            if (h == Nil) chunks // deja eram între cuvinte
            else Nil :: chunks // începem un cuvânt nou
        }

      case c :: xs =>
        val chunks = aux(xs)
        chunks match {
          case Nil => List(List(c))
          case head :: tail => (c :: head) :: tail // adaugăm caracterul la cuvântul curent
        }
    }

    val l = aux(text)
    if (l == List(Nil)) Nil else l
  }


  /* compute the frequency of each chunk */
  def computeTokens(words: List[String]): List[Token] = {
    /* insert a new string in a list of tokens */
    def insWord(s: String, acc: List[Token]): List[Token] = acc match {
      case Nil => List(Token(s,1))
      case Token(w,f) :: tail =>
        if (w == s) {
          Token(w,f+1) :: tail
        }
        else Token(w,f) :: insWord(s,tail)
    }
    /* tail-recursive implementation of the list of tokens */
    def aux(rest: List[String], acc: List[Token]): List[Token] = rest match {
      case Nil => acc
      case w::ws => aux(ws, insWord(w,acc))
    }
    aux(words, Nil)
  }

  def tokensToTree(tokens: List[Token]): WTree = {
    tokens.foldLeft(Empty:WTree)((tree,token)=>{
      tree.ins(token)
    })
  }

  /* Using the previous function, which builds a tree from a list of tokens,
  *  write a function which takes a string,
  *  splits it into chunks, computes frequencies and constructs a tree.
  *  Use the function _.toList to construct a list of characters from a String.
  *
  *  A much cleaner implementation can be achieved by "sequencing" functions using
  *  andThen.
  * */

  def makeTree(s: String): WTree = {
    val chars: List[Char] = s.toList
    val chunks: List[List[Char]] = split(chars)
    val words: List[String] = chunks.map(_.mkString)
    val tokens: List[Token] = computeTokens(words)
    val tree: WTree = tokensToTree(tokens)

    tree
  }

  /* build a tree with the words and frequencies from the text in the scalaDescription text */
  def wordSet: WTree = {
    makeTree(scalaDescription)
  }

  /* find the number of occurrences of the keyword "Scala" in the scalaDescription text */
  def scalaFreq: Int = {
    val filtered= wordSet.filter(_.word == "Scala")
    filtered match {
      case Empty => 0
      case Node(token, _, _) => token.freq
    }
  }

  /* find how many programming languages are referenced in the text.
     A PL is a keyword which starts with an uppercase
     You can reference a character from a string using (0) and you can
     also use the function isUpper

  */
  def progLang: Int = {
    wordSet.filter(_.word(0).isUpper).size
  }

  /* find how many words which are not prepositions or conjunctions appear in the text (any word whose size is larger than 3). */

  def wordCount : Int = {
    wordSet.filter(x => x.word.length > 3).size
  }
  

}

