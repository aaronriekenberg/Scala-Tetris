package org.aaron.scala.tetris

import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.Rectangle

import scala.collection.mutable.ArrayBuffer
import scala.swing.event.Event
import scala.swing.event.Key
import scala.swing.event.KeyPressed
import scala.swing.BorderPanel
import scala.swing.Dialog
import scala.swing.FlowPanel
import scala.swing.Label
import scala.swing.MainFrame
import scala.swing.Panel
import scala.swing.Publisher
import scala.swing.SimpleSwingApplication
import scala.util.Random

import javax.swing.Timer

object TetrisConstants {

  val ROWS = 25

  val COLUMNS = 15

  def rowIsValid(row: Int): Boolean =
    ((row >= 0) && (row <= (ROWS - 1)))

  def columnIsValid(column: Int): Boolean =
    ((column >= 0) && (column <= (COLUMNS - 1)))

}

/**
 * A coordinate in the Tetris model.
 * (0, 0) is the top-most and left-most cell in the model.
 */
case class TetrisCoordinate(val row: Int, val column: Int) {

  def isValid: Boolean =
    TetrisConstants.rowIsValid(row) && TetrisConstants.columnIsValid(column)

}

abstract class TetrisPiece {

  def centerCoord: TetrisCoordinate

  def orientation: Int

  def color: Color

  def cellCoordinates: Seq[TetrisCoordinate]

  def numOrientations: Int

  def nextOrientation: Int = (orientation + 1) % numOrientations

  def makeTetrisPiece(
    centerCoord: TetrisCoordinate, orientation: Int): TetrisPiece

  def cloneWithNextOrientation: TetrisPiece =
    makeTetrisPiece(centerCoord, nextOrientation)

  def cloneWithNewCenterCoord(newCenterCoord: TetrisCoordinate): TetrisPiece =
    makeTetrisPiece(newCenterCoord, orientation)

}

class SquarePiece(val centerCoord: TetrisCoordinate, val orientation: Int = 0)
  extends TetrisPiece {

  val color = Color.GREEN

  val cellCoordinates =
    List(
      TetrisCoordinate(centerCoord.row, centerCoord.column),
      TetrisCoordinate(centerCoord.row + 1, centerCoord.column),
      TetrisCoordinate(centerCoord.row, centerCoord.column + 1),
      TetrisCoordinate(centerCoord.row + 1, centerCoord.column + 1))

  val numOrientations = 1

  def makeTetrisPiece(centerCoord: TetrisCoordinate, orientation: Int): SquarePiece =
    new SquarePiece(centerCoord, orientation)

}

class LinePiece(val centerCoord: TetrisCoordinate, val orientation: Int = 0)
  extends TetrisPiece {

  val color = Color.RED

  val cellCoordinates = {
    orientation match {
      case 0 =>
        List(
          TetrisCoordinate(centerCoord.row, centerCoord.column),
          TetrisCoordinate(centerCoord.row + 1, centerCoord.column),
          TetrisCoordinate(centerCoord.row + 2, centerCoord.column),
          TetrisCoordinate(centerCoord.row + 3, centerCoord.column))
      case 1 =>
        List(
          TetrisCoordinate(centerCoord.row, centerCoord.column),
          TetrisCoordinate(centerCoord.row, centerCoord.column + 1),
          TetrisCoordinate(centerCoord.row, centerCoord.column + 2),
          TetrisCoordinate(centerCoord.row, centerCoord.column + 3))
      case 2 =>
        List(
          TetrisCoordinate(centerCoord.row, centerCoord.column),
          TetrisCoordinate(centerCoord.row - 1, centerCoord.column),
          TetrisCoordinate(centerCoord.row - 2, centerCoord.column),
          TetrisCoordinate(centerCoord.row - 3, centerCoord.column))
      case _ =>
        List(
          TetrisCoordinate(centerCoord.row, centerCoord.column),
          TetrisCoordinate(centerCoord.row, centerCoord.column - 1),
          TetrisCoordinate(centerCoord.row, centerCoord.column - 2),
          TetrisCoordinate(centerCoord.row, centerCoord.column - 3))
    }
  }

  val numOrientations = 4

  def makeTetrisPiece(centerCoord: TetrisCoordinate, orientation: Int): LinePiece =
    new LinePiece(centerCoord, orientation)

}

class TPiece(val centerCoord: TetrisCoordinate, val orientation: Int = 0)
  extends TetrisPiece {

  val color = Color.CYAN

  val cellCoordinates = {
    orientation match {
      case 0 =>
        List(
          TetrisCoordinate(centerCoord.row, centerCoord.column),
          TetrisCoordinate(centerCoord.row, centerCoord.column + 1),
          TetrisCoordinate(centerCoord.row, centerCoord.column - 1),
          TetrisCoordinate(centerCoord.row + 1, centerCoord.column))
      case 1 =>
        List(
          TetrisCoordinate(centerCoord.row - 1, centerCoord.column),
          TetrisCoordinate(centerCoord.row, centerCoord.column),
          TetrisCoordinate(centerCoord.row + 1, centerCoord.column),
          TetrisCoordinate(centerCoord.row, centerCoord.column + 1))
      case 2 =>
        List(
          TetrisCoordinate(centerCoord.row, centerCoord.column),
          TetrisCoordinate(centerCoord.row, centerCoord.column + 1),
          TetrisCoordinate(centerCoord.row, centerCoord.column - 1),
          TetrisCoordinate(centerCoord.row - 1, centerCoord.column))
      case _ =>
        List(
          TetrisCoordinate(centerCoord.row - 1, centerCoord.column),
          TetrisCoordinate(centerCoord.row, centerCoord.column),
          TetrisCoordinate(centerCoord.row + 1, centerCoord.column),
          TetrisCoordinate(centerCoord.row, centerCoord.column - 1))
    }
  }

  val numOrientations = 4

  def makeTetrisPiece(centerCoord: TetrisCoordinate, orientation: Int): TPiece =
    new TPiece(centerCoord, orientation)

}

class LeftZPiece(val centerCoord: TetrisCoordinate, val orientation: Int = 0)
  extends TetrisPiece {

  val color = Color.YELLOW

  val cellCoordinates = {
    orientation match {
      case 0 =>
        List(
          TetrisCoordinate(centerCoord.row, centerCoord.column),
          TetrisCoordinate(centerCoord.row, centerCoord.column - 1),
          TetrisCoordinate(centerCoord.row + 1, centerCoord.column),
          TetrisCoordinate(centerCoord.row + 1, centerCoord.column + 1))
      case _ =>
        List(
          TetrisCoordinate(centerCoord.row, centerCoord.column),
          TetrisCoordinate(centerCoord.row + 1, centerCoord.column),
          TetrisCoordinate(centerCoord.row, centerCoord.column + 1),
          TetrisCoordinate(centerCoord.row - 1, centerCoord.column + 1))
    }
  }

  val numOrientations = 2

  def makeTetrisPiece(centerCoord: TetrisCoordinate, orientation: Int): LeftZPiece =
    new LeftZPiece(centerCoord, orientation)

}

class RightZPiece(val centerCoord: TetrisCoordinate, val orientation: Int = 0)
  extends TetrisPiece {

  val color = Color.ORANGE

  val cellCoordinates = {
    orientation match {
      case 0 =>
        List(
          TetrisCoordinate(centerCoord.row, centerCoord.column),
          TetrisCoordinate(centerCoord.row, centerCoord.column + 1),
          TetrisCoordinate(centerCoord.row + 1, centerCoord.column),
          TetrisCoordinate(centerCoord.row + 1, centerCoord.column - 1))
      case _ =>
        List(
          TetrisCoordinate(centerCoord.row, centerCoord.column),
          TetrisCoordinate(centerCoord.row - 1, centerCoord.column),
          TetrisCoordinate(centerCoord.row, centerCoord.column + 1),
          TetrisCoordinate(centerCoord.row + 1, centerCoord.column + 1))
    }
  }

  val numOrientations = 2

  def makeTetrisPiece(centerCoord: TetrisCoordinate, orientation: Int): RightZPiece =
    new RightZPiece(centerCoord, orientation)

}

class LeftLPiece(val centerCoord: TetrisCoordinate, val orientation: Int = 0)
  extends TetrisPiece {

  val color = Color.LIGHT_GRAY

  val cellCoordinates = {
    orientation match {
      case 0 =>
        List(
          TetrisCoordinate(centerCoord.row, centerCoord.column),
          TetrisCoordinate(centerCoord.row + 1, centerCoord.column),
          TetrisCoordinate(centerCoord.row + 2, centerCoord.column),
          TetrisCoordinate(centerCoord.row + 2, centerCoord.column - 1))
      case 1 =>
        List(
          TetrisCoordinate(centerCoord.row, centerCoord.column),
          TetrisCoordinate(centerCoord.row, centerCoord.column + 1),
          TetrisCoordinate(centerCoord.row, centerCoord.column + 2),
          TetrisCoordinate(centerCoord.row + 1, centerCoord.column + 2))
      case 2 =>
        List(
          TetrisCoordinate(centerCoord.row, centerCoord.column),
          TetrisCoordinate(centerCoord.row - 1, centerCoord.column),
          TetrisCoordinate(centerCoord.row - 2, centerCoord.column),
          TetrisCoordinate(centerCoord.row - 2, centerCoord.column + 1))
      case _ =>
        List(
          TetrisCoordinate(centerCoord.row, centerCoord.column),
          TetrisCoordinate(centerCoord.row, centerCoord.column - 1),
          TetrisCoordinate(centerCoord.row, centerCoord.column - 2),
          TetrisCoordinate(centerCoord.row - 1, centerCoord.column - 2))
    }
  }

  val numOrientations = 4

  def makeTetrisPiece(centerCoord: TetrisCoordinate, orientation: Int): LeftLPiece =
    new LeftLPiece(centerCoord, orientation)

}

class RightLPiece(val centerCoord: TetrisCoordinate, val orientation: Int = 0)
  extends TetrisPiece {

  val color = Color.PINK

  val cellCoordinates = {
    orientation match {
      case 0 =>
        List(
          TetrisCoordinate(centerCoord.row, centerCoord.column),
          TetrisCoordinate(centerCoord.row + 1, centerCoord.column),
          TetrisCoordinate(centerCoord.row + 2, centerCoord.column),
          TetrisCoordinate(centerCoord.row + 2, centerCoord.column + 1))
      case 1 =>
        List(
          TetrisCoordinate(centerCoord.row, centerCoord.column),
          TetrisCoordinate(centerCoord.row, centerCoord.column + 1),
          TetrisCoordinate(centerCoord.row, centerCoord.column + 2),
          TetrisCoordinate(centerCoord.row - 1, centerCoord.column + 2))
      case 2 =>
        List(
          TetrisCoordinate(centerCoord.row, centerCoord.column),
          TetrisCoordinate(centerCoord.row - 1, centerCoord.column),
          TetrisCoordinate(centerCoord.row - 2, centerCoord.column),
          TetrisCoordinate(centerCoord.row - 2, centerCoord.column - 1))
      case _ =>
        List(
          TetrisCoordinate(centerCoord.row, centerCoord.column),
          TetrisCoordinate(centerCoord.row, centerCoord.column - 1),
          TetrisCoordinate(centerCoord.row, centerCoord.column - 2),
          TetrisCoordinate(centerCoord.row + 1, centerCoord.column - 2))
    }
  }

  val numOrientations = 4

  def makeTetrisPiece(centerCoord: TetrisCoordinate, orientation: Int): RightLPiece =
    new RightLPiece(centerCoord, orientation)

}

object RandomTetrisPieceFactory {

  private var pieceConstructors = Array(
    (centerCoord: TetrisCoordinate) => new SquarePiece(centerCoord),
    (centerCoord: TetrisCoordinate) => new LinePiece(centerCoord),
    (centerCoord: TetrisCoordinate) => new TPiece(centerCoord),
    (centerCoord: TetrisCoordinate) => new LeftZPiece(centerCoord),
    (centerCoord: TetrisCoordinate) => new RightZPiece(centerCoord),
    (centerCoord: TetrisCoordinate) => new LeftLPiece(centerCoord),
    (centerCoord: TetrisCoordinate) => new RightLPiece(centerCoord))

  def createRandomPiece(centerCoord: TetrisCoordinate): TetrisPiece =
    pieceConstructors(Random.nextInt(pieceConstructors.length))(centerCoord)

}

case class TetrisModelEvent extends Event

class TetrisModel extends Publisher {

  private var deferTetrisModelEvents = false

  private var currentPieceOption: Option[TetrisPiece] = None

  private val stackCells = ArrayBuffer.fill(TetrisConstants.ROWS)(
    ArrayBuffer.fill[Option[Color]](TetrisConstants.COLUMNS)(None))

  private val drawableCellsBuffer =
    new ArrayBuffer[Pair[TetrisCoordinate, Color]]((TetrisConstants.ROWS * TetrisConstants.COLUMNS) + 4)

  private var numLines = 0

  private var isPaused = false

  private var isGameOver = false

  def reset {
    deferTetrisModelEvents = false
    currentPieceOption = None
    stackCells.clear
    val newStackCells = ArrayBuffer.fill(TetrisConstants.ROWS)(
      ArrayBuffer.fill[Option[Color]](TetrisConstants.COLUMNS)(None))
    newStackCells.copyToBuffer(stackCells)
    drawableCellsBuffer.clear
    numLines = 0
    isPaused = false
    isGameOver = false
    publishTetrisModelEvent
  }

  def periodicUpdate {
    if (gameRunning) {
      deferTetrisModelEvents = true

      if (currentPieceOption.isEmpty) {
        addNewPiece
      } else {
        moveCurrentPieceDown
      }

      deferTetrisModelEvents = false

      publishTetrisModelEvent
    }
  }

  def moveCurrentPieceDown {
    if (gameRunning && currentPieceOption.isDefined) {
      val currentPiece = currentPieceOption.get
      val currentPieceMoved =
        currentPiece.cloneWithNewCenterCoord(
          TetrisCoordinate(
            currentPiece.centerCoord.row + 1,
            currentPiece.centerCoord.column))
      if (isPieceLocationValid(currentPieceMoved)) {
        currentPieceOption = Some(currentPieceMoved)
      } else {
        addPieceToStack(currentPiece)
        currentPieceOption = None
      }
      publishTetrisModelEvent
    }
  }

  def moveCurrentPieceLeft {
    if (gameRunning && currentPieceOption.isDefined) {
      val currentPiece = currentPieceOption.get
      val currentPieceMoved =
        currentPiece.cloneWithNewCenterCoord(
          TetrisCoordinate(
            currentPiece.centerCoord.row,
            currentPiece.centerCoord.column - 1))
      if (isPieceLocationValid(currentPieceMoved)) {
        currentPieceOption = Some(currentPieceMoved)
        publishTetrisModelEvent
      }
    }
  }

  def moveCurrentPieceRight {
    if (gameRunning && currentPieceOption.isDefined) {
      val currentPiece = currentPieceOption.get
      val currentPieceMoved =
        currentPiece.cloneWithNewCenterCoord(
          TetrisCoordinate(
            currentPiece.centerCoord.row,
            currentPiece.centerCoord.column + 1))
      if (isPieceLocationValid(currentPieceMoved)) {
        currentPieceOption = Some(currentPieceMoved)
        publishTetrisModelEvent
      }
    }
  }

  def rotateCurrentPiece {
    if (gameRunning && currentPieceOption.isDefined) {
      val currentPiece = currentPieceOption.get
      val currentPieceRotated =
        currentPiece.cloneWithNextOrientation
      if (isPieceLocationValid(currentPieceRotated)) {
        currentPieceOption = Some(currentPieceRotated)
        publishTetrisModelEvent
      }
    }
  }

  def togglePause {
    isPaused = !isPaused
    publishTetrisModelEvent
  }

  def paused: Boolean = isPaused

  def drawableCells: Seq[Pair[TetrisCoordinate, Color]] = drawableCellsBuffer

  def lines: Int = numLines

  def gameOver: Boolean = isGameOver

  private def gameRunning: Boolean = ((!isPaused) && (!isGameOver))

  private def publishTetrisModelEvent {
    if (!deferTetrisModelEvents) {
      updateDrawableCells
      publish(new TetrisModelEvent)
    }
  }

  private def updateDrawableCells {
    drawableCellsBuffer.clear

    if (currentPieceOption.isDefined) {
      val currentPiece = currentPieceOption.get
      drawableCellsBuffer ++=
        currentPiece.cellCoordinates.map(coord => Pair(coord, currentPiece.color))
    }

    for {
      row <- 0 until TetrisConstants.ROWS
      column <- 0 until TetrisConstants.COLUMNS
      if stackCells(row)(column).isDefined
    } drawableCellsBuffer +=
      Pair(TetrisCoordinate(row, column),
        stackCells(row)(column).get)
  }

  private def addNewPiece {
    val centerCoord =
      TetrisCoordinate(0, (TetrisConstants.COLUMNS / 2) - 1)
    val newPiece =
      RandomTetrisPieceFactory.createRandomPiece(centerCoord)
    if (isPieceLocationValid(newPiece)) {
      currentPieceOption = Some(newPiece)
    } else {
      currentPieceOption = None
      isGameOver = true
    }
  }

  private def addPieceToStack(piece: TetrisPiece) {
    piece.cellCoordinates.foreach(
      centerCoord =>
        stackCells(centerCoord.row)(centerCoord.column) =
          Some(piece.color))
    handleFilledStackRows
  }

  private def handleFilledStackRows {
    var row = TetrisConstants.ROWS - 1
    while (row >= 0) {
      val rowIsFull = !stackCells(row).exists(_.isEmpty)
      if (rowIsFull) {
        stackCells.remove(row)
        stackCells.prepend(
          ArrayBuffer.fill[Option[Color]](TetrisConstants.COLUMNS)(None))
        numLines += 1
      } else {
        row -= 1
      }
    }
  }

  private def isPieceLocationValid(piece: TetrisPiece): Boolean =
    !piece.cellCoordinates.exists(
      coord =>
        ((!coord.isValid) ||
          (stackCells(coord.row)(coord.column).isDefined)))

}

class TetrisPanel(tetrisModel: TetrisModel) extends BorderPanel {

  private val tetrisGamePanel = new TetrisGamePanel(tetrisModel)

  add(tetrisGamePanel, BorderPanel.Position.Center)

  add(new TetrisScorePanel(tetrisModel), BorderPanel.Position.South)

  def setupFocus {
    tetrisGamePanel.requestFocusInWindow()
  }

}

class TetrisScorePanel(private val tetrisModel: TetrisModel) extends FlowPanel {

  private val linesLabel = new Label

  contents += linesLabel

  reactions += {
    case TetrisModelEvent() =>
      handleTetrisModelEvent
  }

  listenTo(tetrisModel)

  handleTetrisModelEvent

  private def handleTetrisModelEvent {
    if (tetrisModel.paused) {
      linesLabel.text = "Paused Lines: " + tetrisModel.lines
    } else {
      linesLabel.text = "Lines: " + tetrisModel.lines
    }
  }

}

class TetrisGamePanel(private val tetrisModel: TetrisModel) extends Panel {

  private val cellXCoordBuffer = ArrayBuffer.fill(TetrisConstants.COLUMNS)(0)

  private val cellYCoordBuffer = ArrayBuffer.fill(TetrisConstants.ROWS)(0)

  private var previousHeight = 0

  private var previousWidth = 0

  background = Color.BLACK

  reactions += {
    case TetrisModelEvent() =>
      handleTetrisModelEvent

    case KeyPressed(_, Key.Down, _, _) =>
      downKeyPressed

    case KeyPressed(_, Key.Left, _, _) =>
      leftKeyPressed

    case KeyPressed(_, Key.Right, _, _) =>
      rightKeyPressed

    case KeyPressed(_, Key.Up, _, _) =>
      upKeyPressed

    case KeyPressed(_, Key.Space, _, _) =>
      spaceKeyPressed
  }

  listenTo(tetrisModel)

  listenTo(keys)

  handleTetrisModelEvent

  private def handleTetrisModelEvent {
    repaint
    if (tetrisModel.gameOver) {
      Dialog.showMessage(parent = this, message = "Game Over", title = "Game Over")
      tetrisModel.reset
    }
  }

  private def downKeyPressed {
    tetrisModel.moveCurrentPieceDown
  }

  private def leftKeyPressed {
    tetrisModel.moveCurrentPieceLeft
  }

  private def rightKeyPressed {
    tetrisModel.moveCurrentPieceRight
  }

  private def upKeyPressed {
    tetrisModel.rotateCurrentPiece
  }

  private def spaceKeyPressed {
    tetrisModel.togglePause
  }

  private def recomputeCellLocations {
    if ((size.width != previousWidth) || (size.height != previousHeight)) {
      cellXCoordBuffer.clear
      var currentXCoord = 0
      val normalWidthPixels = size.width / TetrisConstants.COLUMNS
      val extraWidthPixels = size.width % TetrisConstants.COLUMNS
      for (i <- 0 until TetrisConstants.COLUMNS) {
        cellXCoordBuffer += currentXCoord
        val width =
          if (i < extraWidthPixels) {
            normalWidthPixels + 1
          } else {
            normalWidthPixels
          }
        currentXCoord += width
      }

      cellYCoordBuffer.clear
      var currentYCoord = 0
      val normalHeightPixels = size.height / TetrisConstants.ROWS
      val extraHeightPixels = size.height % TetrisConstants.ROWS
      for (i <- 0 until TetrisConstants.ROWS) {
        cellYCoordBuffer += currentYCoord
        val height =
          if (i < extraHeightPixels) {
            normalHeightPixels + 1
          } else {
            normalHeightPixels
          }
        currentYCoord += height
      }

      previousWidth = size.width
      previousHeight = size.height
    }
  }

  private def getCellRectangle(coord: TetrisCoordinate): Rectangle = {
    val rectangle = new Rectangle
    val row = coord.row
    val column = coord.column

    rectangle.x = cellXCoordBuffer(column)
    rectangle.width =
      if (column >= (TetrisConstants.COLUMNS - 1)) {
        size.width - cellXCoordBuffer(column)
      } else {
        cellXCoordBuffer(column + 1) - cellXCoordBuffer(column)
      }

    rectangle.y = cellYCoordBuffer(row)
    rectangle.height =
      if (row >= (TetrisConstants.ROWS - 1)) {
        size.height - cellYCoordBuffer(row)
      } else {
        cellYCoordBuffer(row + 1) - cellYCoordBuffer(row)
      }

    rectangle
  }

  private def paintCell(g: Graphics2D, coord: TetrisCoordinate, color: Color) {
    val oldColor = g.getColor
    g.setColor(color)
    val cellRect = getCellRectangle(coord)
    g.fillRect(cellRect.x, cellRect.y, cellRect.width, cellRect.height)
    g.setColor(Color.BLACK)
    g.drawRect(cellRect.x, cellRect.y, cellRect.width, cellRect.height)
    g.setColor(oldColor)
  }

  override def paintComponent(g: Graphics2D) {
    super.paintComponent(g)

    recomputeCellLocations

    tetrisModel.drawableCells.foreach(coordAndColor =>
      paintCell(g, coordAndColor._1, coordAndColor._2))
  }
}

object ScalaTetris extends SimpleSwingApplication {

  def top = new MainFrame {

    title = "Scala Tetris"

    preferredSize = new Dimension(300, 500)

    private val tetrisModel = new TetrisModel

    private val tetrisPanel = new TetrisPanel(tetrisModel)

    contents = tetrisPanel

    tetrisPanel.setupFocus

    new Timer(250, new ActionListener {
      def actionPerformed(e: ActionEvent) {
        tetrisModel.periodicUpdate
      }
    }).start

  }

}