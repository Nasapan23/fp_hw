case class Board(val board: List[List[Player]], val player: Player) {
  type Line = List[Player]


  private val n: Int = board.length
  private val m: Int = if (board.isEmpty) 0 else board.head.length


  def isFree(x: Int, y: Int): Boolean =
    x >= 0 && x < n && y >= 0 && y < m && board(x)(y) == Empty


  def getColumns: Board = Board(if (board.isEmpty) Nil else board.transpose, player)


  def getFstDiag: Line = (0 until math.min(n, m)).toList.map(i => board(i)(i))
  def getSndDiag: Line = (0 until math.min(n, m)).toList.map(i => board(i)(m - 1 - i))


  private def diagFrom(r0: Int, c0: Int): Line = {
    val buf = scala.collection.mutable.ListBuffer.empty[Player]
    var r = r0; var c = c0
    while (r < n && c < m) { buf += board(r)(c); r += 1; c += 1 }
    buf.toList
  }
  private def antiDiagFrom(r0: Int, c0: Int): Line = {
    val buf = scala.collection.mutable.ListBuffer.empty[Player]
    var r = r0; var c = c0
    while (r < n && c >= 0) { buf += board(r)(c); r += 1; c -= 1 }
    buf.toList
  }


  def getAboveFstDiag: List[Line] = (1 until m).toList.map(c => diagFrom(0, c))
  def getBelowFstDiag: List[Line] = (1 until n).toList.map(r => diagFrom(r, 0))
  def getAboveSndDiag: List[Line] = (0 until m - 1).toList.map(c => antiDiagFrom(0, c)).reverse
  def getBelowSndDiag: List[Line] = (1 until n).toList.map(r => antiDiagFrom(r, m - 1))


  private def allLines: List[Line] = {
    val rows = board
    val cols = if (board.isEmpty) Nil else board.transpose
    val fst = getFstDiag :: (getAboveFstDiag ++ getBelowFstDiag)
    val snd = getSndDiag :: (getAboveSndDiag ++ getBelowSndDiag)
    rows ++ cols ++ fst ++ snd
  }


  private def hasFiveInRow(line: Line, p: Player): Boolean =
    line.sliding(5).exists(win => win.forall(_ == p))


  def winner: Boolean = allLines.exists(line => hasFiveInRow(line, player))


  def update(ln: Int, col: Int): Board = {
    val newRow = board(ln).updated(col, player)
    val newBoard = board.updated(ln, newRow)
    Board(newBoard, player.complement)
  }


  def next: List[Board] = {
    for {
      i <- board.indices.toList
      j <- board.headOption.map(_.indices.toList).getOrElse(Nil)
      if board(i)(j) == Empty
    } yield update(i, j)
  }


  private def windowScore(win: Line, p: Player): Option[Int] = {
    val ks = win.count(_ == p)
    val opp = win.count(c => c != p && c != Empty)
    if (opp == 0 && ks >= 2) Some(ks) else None
  }


  def sequences: Map[Int, Int] = {
    val ks = List(2, 3, 4, 5)
    val init = ks.map(_ -> 0).toMap
    val p = player
    val counts = allLines.filter(_.length >= 5).foldLeft(init) { (acc, line) =>
      val adds = line.sliding(5).flatMap(w => windowScore(w, p)).toList
      adds.foldLeft(acc) { (m, k) => m.updated(k, m(k) + 1) }
    }
    counts
  }


  override def toString: String = {
    def show(p: Player): Char = p match {
      case One => 'X'
      case Two => '0'
      case Empty => '.'
    }
    board.map(row => row.map(show).mkString).mkString("\n")
  }
}


object Board {
  def tokenID: Int = 293544


  private def toPos(c: Char): Player = c match {
    case 'X' => One
    case '0' => Two
    case _ => Empty
  }


  def apply(s: String, p: Player): Board = {
    val rows: List[List[Player]] = s.split("\n").toList.map(line => line.toList.map(toPos))
    Board(rows, p)
  }


  def apply(s: String): Board = {
    val rows: List[List[Player]] = s.split("\n").toList.map(line => line.toList.map(toPos))
    Board(rows, One)
  }
}