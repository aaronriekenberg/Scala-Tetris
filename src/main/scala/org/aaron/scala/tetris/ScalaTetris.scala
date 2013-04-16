package org.aaron.scala.tetris

import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.Rectangle
import java.awt.event.ActionEvent
import java.awt.event.ActionListener

import scala.collection.mutable.ArrayBuffer
import scala.swing.Action
import scala.swing.BorderPanel
import scala.swing.Dialog
import scala.swing.FlowPanel
import scala.swing.Label
import scala.swing.MainFrame
import scala.swing.Menu
import scala.swing.MenuBar
import scala.swing.MenuItem
import scala.swing.Panel
import scala.swing.Publisher
import scala.swing.Reactor
import scala.swing.Separator
import scala.swing.SimpleSwingApplication
import scala.swing.event.Event
import scala.swing.event.Key
import scala.swing.event.KeyPressed
import scala.util.Random

import javax.swing.Timer

object TetrisConstants {

  val numRows = 25

  val numColumns = 15

  val rowsRange = 0 until numRows

  val columnsRange = 0 until numColumns

}

/**
 * A coordinate in the Tetris model.
 * (0, 0) is the top-most and left-most cell in the model.
 */
case class TetrisCoordinate(val row: Int, val column: Int) {

  def isValid: Boolean =
    TetrisConstants.rowsRange.contains(row) &&
      TetrisConstants.columnsRange.contains(column)

}

abstract class TetrisPiece {

  def centerCoord: TetrisCoordinate

  def centerRow: Int = centerCoord.row

  def centerColumn: Int = centerCoord.column

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

  def cloneWithNewCenterRow(newCenterRow: Int): TetrisPiece =
    cloneWithNewCenterCoord(TetrisCoordinate(newCenterRow, centerColumn))

  def cloneWithNewCenterColumn(newCenterColumn: Int): TetrisPiece =
    cloneWithNewCenterCoord(TetrisCoordinate(centerRow, newCenterColumn))

}

class SquarePiece(val centerCoord: TetrisCoordinate, val orientation: Int = 0)
  extends TetrisPiece {

  val color = Color.GREEN

  val cellCoordinates =
    List(
      TetrisCoordinate(centerRow, centerColumn),
      TetrisCoordinate(centerRow + 1, centerColumn),
      TetrisCoordinate(centerRow, centerColumn + 1),
      TetrisCoordinate(centerRow + 1, centerColumn + 1))

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
          TetrisCoordinate(centerRow, centerColumn),
          TetrisCoordinate(centerRow + 1, centerColumn),
          TetrisCoordinate(centerRow + 2, centerColumn),
          TetrisCoordinate(centerRow + 3, centerColumn))
      case 1 =>
        List(
          TetrisCoordinate(centerRow, centerColumn),
          TetrisCoordinate(centerRow, centerColumn + 1),
          TetrisCoordinate(centerRow, centerColumn + 2),
          TetrisCoordinate(centerRow, centerColumn + 3))
      case 2 =>
        List(
          TetrisCoordinate(centerRow, centerColumn),
          TetrisCoordinate(centerRow - 1, centerColumn),
          TetrisCoordinate(centerRow - 2, centerColumn),
          TetrisCoordinate(centerRow - 3, centerColumn))
      case _ =>
        List(
          TetrisCoordinate(centerRow, centerColumn),
          TetrisCoordinate(centerRow, centerColumn - 1),
          TetrisCoordinate(centerRow, centerColumn - 2),
          TetrisCoordinate(centerRow, centerColumn - 3))
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
          TetrisCoordinate(centerRow, centerColumn),
          TetrisCoordinate(centerRow, centerColumn + 1),
          TetrisCoordinate(centerRow, centerColumn - 1),
          TetrisCoordinate(centerRow + 1, centerColumn))
      case 1 =>
        List(
          TetrisCoordinate(centerRow - 1, centerColumn),
          TetrisCoordinate(centerRow, centerColumn),
          TetrisCoordinate(centerRow + 1, centerColumn),
          TetrisCoordinate(centerRow, centerColumn + 1))
      case 2 =>
        List(
          TetrisCoordinate(centerRow, centerColumn),
          TetrisCoordinate(centerRow, centerColumn + 1),
          TetrisCoordinate(centerRow, centerColumn - 1),
          TetrisCoordinate(centerRow - 1, centerColumn))
      case _ =>
        List(
          TetrisCoordinate(centerRow - 1, centerColumn),
          TetrisCoordinate(centerRow, centerColumn),
          TetrisCoordinate(centerRow + 1, centerColumn),
          TetrisCoordinate(centerRow, centerColumn - 1))
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
          TetrisCoordinate(centerRow, centerColumn),
          TetrisCoordinate(centerRow, centerColumn - 1),
          TetrisCoordinate(centerRow + 1, centerColumn),
          TetrisCoordinate(centerRow + 1, centerColumn + 1))
      case _ =>
        List(
          TetrisCoordinate(centerRow, centerColumn),
          TetrisCoordinate(centerRow + 1, centerColumn),
          TetrisCoordinate(centerRow, centerColumn + 1),
          TetrisCoordinate(centerRow - 1, centerColumn + 1))
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
          TetrisCoordinate(centerRow, centerColumn),
          TetrisCoordinate(centerRow, centerColumn + 1),
          TetrisCoordinate(centerRow + 1, centerColumn),
          TetrisCoordinate(centerRow + 1, centerColumn - 1))
      case _ =>
        List(
          TetrisCoordinate(centerRow, centerColumn),
          TetrisCoordinate(centerRow - 1, centerColumn),
          TetrisCoordinate(centerRow, centerColumn + 1),
          TetrisCoordinate(centerRow + 1, centerColumn + 1))
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
          TetrisCoordinate(centerRow, centerColumn),
          TetrisCoordinate(centerRow + 1, centerColumn),
          TetrisCoordinate(centerRow + 2, centerColumn),
          TetrisCoordinate(centerRow + 2, centerColumn - 1))
      case 1 =>
        List(
          TetrisCoordinate(centerRow, centerColumn),
          TetrisCoordinate(centerRow, centerColumn + 1),
          TetrisCoordinate(centerRow, centerColumn + 2),
          TetrisCoordinate(centerRow + 1, centerColumn + 2))
      case 2 =>
        List(
          TetrisCoordinate(centerRow, centerColumn),
          TetrisCoordinate(centerRow - 1, centerColumn),
          TetrisCoordinate(centerRow - 2, centerColumn),
          TetrisCoordinate(centerRow - 2, centerColumn + 1))
      case _ =>
        List(
          TetrisCoordinate(centerRow, centerColumn),
          TetrisCoordinate(centerRow, centerColumn - 1),
          TetrisCoordinate(centerRow, centerColumn - 2),
          TetrisCoordinate(centerRow - 1, centerColumn - 2))
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
          TetrisCoordinate(centerRow, centerColumn),
          TetrisCoordinate(centerRow + 1, centerColumn),
          TetrisCoordinate(centerRow + 2, centerColumn),
          TetrisCoordinate(centerRow + 2, centerColumn + 1))
      case 1 =>
        List(
          TetrisCoordinate(centerRow, centerColumn),
          TetrisCoordinate(centerRow, centerColumn + 1),
          TetrisCoordinate(centerRow, centerColumn + 2),
          TetrisCoordinate(centerRow - 1, centerColumn + 2))
      case 2 =>
        List(
          TetrisCoordinate(centerRow, centerColumn),
          TetrisCoordinate(centerRow - 1, centerColumn),
          TetrisCoordinate(centerRow - 2, centerColumn),
          TetrisCoordinate(centerRow - 2, centerColumn - 1))
      case _ =>
        List(
          TetrisCoordinate(centerRow, centerColumn),
          TetrisCoordinate(centerRow, centerColumn - 1),
          TetrisCoordinate(centerRow, centerColumn - 2),
          TetrisCoordinate(centerRow + 1, centerColumn - 2))
    }
  }

  val numOrientations = 4

  def makeTetrisPiece(centerCoord: TetrisCoordinate, orientation: Int): RightLPiece =
    new RightLPiece(centerCoord, orientation)

}

object RandomTetrisPieceFactory {

  private val pieceConstructors = Array(
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

  private var deferPublishCount = 0

  private var currentPieceOption: Option[TetrisPiece] = None

  private val stackCells =
    ArrayBuffer.fill[Option[Color]](TetrisConstants.numRows, TetrisConstants.numColumns)(None)

  private val drawableCellsBuffer = ArrayBuffer.empty[Pair[TetrisCoordinate, Color]]

  private var numLines = 0

  private var isPaused = false

  private var isGameOver = false

  def reset {
    deferPublishCount = 0
    currentPieceOption = None
    stackCells.clear
    val newStackCells =
      ArrayBuffer.fill[Option[Color]](TetrisConstants.numRows, TetrisConstants.numColumns)(None)
    newStackCells.copyToBuffer(stackCells)
    drawableCellsBuffer.clear
    numLines = 0
    isPaused = false
    isGameOver = false
    publishTetrisModelEvent
  }

  def periodicUpdate {
    if (gameRunning) {
      executeAndPublish {
        currentPieceOption match {
          case Some(_) => moveCurrentPieceDown
          case None => addNewPiece
        }
      }
    }
  }

  def moveCurrentPieceDown {
    if (gameRunning) {
      executeAndPublish {
        currentPieceOption = currentPieceOption.flatMap(
          currentPiece => {
            val currentPieceMoved =
              currentPiece.cloneWithNewCenterRow(
                currentPiece.centerRow + 1)
            if (isPieceLocationValid(currentPieceMoved)) {
              Some(currentPieceMoved)
            } else {
              addPieceToStack(currentPiece)
              None
            }
          })
      }
    }
  }

  def dropCurrentPiece {
    if (gameRunning) {
      executeAndPublish {
        while (currentPieceOption.isDefined) {
          moveCurrentPieceDown
        }
      }
    }
  }

  def moveCurrentPieceLeft {
    if (gameRunning) {
      executeAndPublish {
        currentPieceOption = currentPieceOption.flatMap(
          currentPiece => {
            val currentPieceMoved =
              currentPiece.cloneWithNewCenterColumn(
                currentPiece.centerColumn - 1)
            if (isPieceLocationValid(currentPieceMoved)) {
              Some(currentPieceMoved)
            } else {
              Some(currentPiece)
            }
          })
      }
    }
  }

  def moveCurrentPieceRight {
    if (gameRunning) {
      executeAndPublish {
        currentPieceOption = currentPieceOption.flatMap(
          currentPiece => {
            val currentPieceMoved =
              currentPiece.cloneWithNewCenterColumn(
                currentPiece.centerColumn + 1)
            if (isPieceLocationValid(currentPieceMoved)) {
              Some(currentPieceMoved)
            } else {
              Some(currentPiece)
            }
          })
      }
    }
  }

  def rotateCurrentPiece {
    if (gameRunning) {
      executeAndPublish {
        currentPieceOption = currentPieceOption.flatMap(
          currentPiece => {
            val currentPieceMoved =
              currentPiece.cloneWithNextOrientation
            if (isPieceLocationValid(currentPieceMoved)) {
              Some(currentPieceMoved)
            } else {
              Some(currentPiece)
            }
          })
      }
    }
  }

  def togglePause {
    executeAndPublish {
      isPaused = !isPaused
    }
  }

  def paused: Boolean = isPaused

  def drawableCells: Seq[Pair[TetrisCoordinate, Color]] = drawableCellsBuffer

  def lines: Int = numLines

  def gameOver: Boolean = isGameOver

  private def gameRunning: Boolean = ((!isPaused) && (!isGameOver))

  private def executeAndPublish(op: => Unit) {
    deferPublishCount += 1
    op
    deferPublishCount -= 1
    publishTetrisModelEvent
  }

  private def publishTetrisModelEvent {
    if (deferPublishCount == 0) {
      updateDrawableCells
      publish(TetrisModelEvent())
    }
  }

  private def updateDrawableCells {
    drawableCellsBuffer.clear

    currentPieceOption.foreach(
      currentPiece => {
        drawableCellsBuffer ++=
          currentPiece.cellCoordinates.map(
            Pair(_, currentPiece.color))
      })

    for {
      row <- TetrisConstants.rowsRange
      column <- TetrisConstants.columnsRange
      if stackCells(row)(column).isDefined
    } drawableCellsBuffer +=
      Pair(TetrisCoordinate(row, column),
        stackCells(row)(column).get)
  }

  private def addNewPiece {
    val centerCoord =
      TetrisCoordinate(0, (TetrisConstants.numColumns / 2) - 1)
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
      coord =>
        stackCells(coord.row)(coord.column) =
          Some(piece.color))
    handleFilledStackRows
  }

  private def handleFilledStackRows {
    var row = TetrisConstants.numRows - 1
    while (row >= 0) {
      val rowIsFull = !stackCells(row).exists(_.isEmpty)
      if (rowIsFull) {
        stackCells.remove(row)
        stackCells.prepend(
          ArrayBuffer.fill[Option[Color]](TetrisConstants.numColumns)(None))
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

class TetrisScorePanel(tetrisModel: TetrisModel) extends FlowPanel {

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

class TetrisGamePanel(tetrisModel: TetrisModel) extends Panel {

  private val cellXCoordBuffer = ArrayBuffer.fill(TetrisConstants.numColumns)(0)

  private val cellYCoordBuffer = ArrayBuffer.fill(TetrisConstants.numRows)(0)

  private var previousHeight = 0

  private var previousWidth = 0

  background = Color.BLACK

  reactions += {
    case TetrisModelEvent() =>
      handleTetrisModelEvent
  }

  listenTo(tetrisModel)

  handleTetrisModelEvent

  private def handleTetrisModelEvent {
    repaint
    if (tetrisModel.gameOver) {
      Dialog.showMessage(parent = this, message = "Game Over", title = "Game Over")
      tetrisModel.reset
    }
  }

  private def recomputeCellLocations {
    if ((size.width != previousWidth) || (size.height != previousHeight)) {
      cellXCoordBuffer.clear
      var currentXCoord = 0
      val normalWidthPixels = size.width / TetrisConstants.numColumns
      val extraWidthPixels = size.width % TetrisConstants.numColumns
      for (i <- TetrisConstants.columnsRange) {
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
      val normalHeightPixels = size.height / TetrisConstants.numRows
      val extraHeightPixels = size.height % TetrisConstants.numRows
      for (i <- TetrisConstants.rowsRange) {
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
      if (column >= (TetrisConstants.numColumns - 1)) {
        size.width - cellXCoordBuffer(column)
      } else {
        cellXCoordBuffer(column + 1) - cellXCoordBuffer(column)
      }

    rectangle.y = cellYCoordBuffer(row)
    rectangle.height =
      if (row >= (TetrisConstants.numRows - 1)) {
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

class TetrisController extends Reactor {

  val tetrisModel = new TetrisModel

  val tetrisPanel = new TetrisPanel(tetrisModel)

  reactions += {
    case KeyPressed(_, Key.Down, _, _) =>
      tetrisModel.moveCurrentPieceDown

    case KeyPressed(_, Key.Space, _, _) =>
      tetrisModel.dropCurrentPiece

    case KeyPressed(_, Key.Left, _, _) =>
      tetrisModel.moveCurrentPieceLeft

    case KeyPressed(_, Key.Right, _, _) =>
      tetrisModel.moveCurrentPieceRight

    case KeyPressed(_, Key.Up, _, _) =>
      tetrisModel.rotateCurrentPiece

    case KeyPressed(_, Key.P, _, _) =>
      tetrisModel.togglePause
  }

  listenTo(tetrisPanel.keys)

  new Timer(250, new ActionListener {
    override def actionPerformed(e: ActionEvent) {
      tetrisModel.periodicUpdate
    }
  }).start

  def togglePause {
    tetrisModel.togglePause
  }

  def resetGame {
    tetrisModel.reset
  }

  def setupFocus {
    tetrisPanel.requestFocus
  }

}

object ScalaTetris extends SimpleSwingApplication {

  def top = new MainFrame {

    title = "Scala Tetris"

    preferredSize = new Dimension(300, 550)

    private val tetrisContoller = new TetrisController

    contents = tetrisContoller.tetrisPanel

    tetrisContoller.setupFocus

    menuBar = new MenuBar {
      contents += new Menu("Game") {
        contents += new MenuItem(Action("Pause") { tetrisContoller.togglePause })
        contents += new MenuItem(Action("Reset") { tetrisContoller.resetGame })
        contents += new Separator
        contents += new MenuItem(Action("Exit") { sys.exit(0) })
      }
    }

  }

}