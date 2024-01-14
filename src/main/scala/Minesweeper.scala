import scalafx.animation.PauseTransition
import scalafx.application.{JFXApp3, Platform}
import scalafx.event.ActionEvent
import scalafx.geometry.Pos
import scalafx.scene.control.{ContentDisplay, Label}
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.{Parent, Scene}
import scalafx.scene.input.{MouseButton, MouseEvent}
import scalafx.scene.layout.{Pane, StackPane}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.{Font, Text, TextAlignment}
import scalafx.stage.Stage
import scalafx.util.Duration

object Minesweeper extends JFXApp3 {
  private val tileSize : Int = 40
  private val stageWidth : Int = 600
  private val stageHeight : Int = 600
  private val xTilesCount : Int = stageWidth/tileSize
  private val yTilesCount : Int = stageHeight/tileSize
  private val grid: Array[Array[Tile]] = Array.ofDim[Tile](xTilesCount, yTilesCount)
  private var allBombsCount : Int = 0
  private var mine: Image = null
  private var root: Pane = null

  override def start(): Unit = {
    mine = new Image("mine.png")
    root = createContent(allBombsCount)
    stage = new JFXApp3.PrimaryStage {
      scene = new Scene(root)
      title = "Minesweeper"
    }
    stage.minWidth = stageWidth
    stage.maxWidth = stageWidth
    stage.minHeight = stageHeight
    stage.maxHeight = stageHeight
    stage.getIcons.add(mine)
    stage.show()
  }

  private case class Tile(x: Int, y: Int, hasBomb: Boolean, var isShown : Boolean = false) extends StackPane {
    if hasBomb then allBombsCount += 1
    var bombsCount: Int = 0
    var marked: Boolean = false
    private val tileBorder: Rectangle = new Rectangle {
      width = tileSize-1
      height = tileSize-1
      stroke = Color.Aqua
      fill = Color.Teal
    }
    val tileText: Text = new Text {
      font = Font.font(18)
      text = if hasBomb then "X" else ""
      visible = false
    }
    this.getChildren.addAll(tileBorder, tileText)
    translateX = x*tileSize
    translateY = y*tileSize
    onMouseClicked = e => {
      if countMarkedTiles() == allBombsCount then winScene(this)
      else {
        if e.getButton.name == "PRIMARY"
        then show(this)
        else mark()
      }
    }

    private def show(tile: Tile): Unit = {
      if !isShown then {
        tileText.visible = true
        if hasBomb then gameoverScene(this)
        else {
          isShown = true
          tileBorder.fill = Color.PaleTurquoise
          if tileText.getText.isEmpty then getNeighbours(this).foreach(a => a.show(null)) // if tileText.text.isEmpty nie dziala?
        }
      } else if tile != null then showAllHints(this)
    }
    private def mark(): Unit = {
      if !isShown then {
        marked = true
        isShown = true
        tileBorder.fill = Color.Orchid
      } else if marked then {
        marked = false
        isShown = false
        tileBorder.fill = Color.Teal
      }
    }
    def autoMark(): Unit = {
      if !isShown then {
        if hasBomb then {
          marked = true
          isShown = true
          tileBorder.fill = Color.Orchid
        } else this.show(null)
      }
    }
  }

  private def createContent(bombCountVal: Int): Pane = new Pane {
    allBombsCount = 0
    private var bombCountVar = bombCountVal //todo lepsze wybieranie min
    prefWidth = stageWidth
    prefHeight = stageHeight
    createTiles(this)
    createHints()
    startScene(this)
  }

  private def createTiles(pane: Pane): Unit = {
    for {i <- 0 until xTilesCount
         j <- 0 until yTilesCount} {
      val tile = Tile(i, j, Math.random() < 0.1)
      grid(i)(j) = tile
      pane.getChildren.add(tile)
    }
  }

  private def createHints(): Unit = {
    for {i <- 0 until xTilesCount
         j <- 0 until yTilesCount} {
      val tile = grid(i)(j)
      if !tile.hasBomb then {
        val bombs = countBombs(tile)
        if bombs > 0 then {
          tile.bombsCount = bombs
          tile.tileText.text = bombs.toString
        }
      }
    }
  }

  private def getNeighbours(tile: Tile): Seq[Tile] = (
      (tile.x - 1, tile.y - 1) :: (tile.x - 1, tile.y) :: (tile.x - 1, tile.y + 1) ::
      (tile.x, tile.y - 1) :: (tile.x, tile.y + 1) ::
      (tile.x + 1, tile.y - 1) :: (tile.x + 1, tile.y) :: (tile.x + 1, tile.y + 1) :: Nil
    ) .filter(a => a(0) >= 0 && a(1) >= 0 && a(0) < xTilesCount && a(1) < yTilesCount)
      .map(a => grid(a(0))(a(1)))


  private def countBombs(tile: Tile): Int = {
    getNeighbours(tile).count(a => a.hasBomb)
  }

  private def showAllHints(tile: Tile): Unit = {
    if countUnopenedHints(tile) <= tile.bombsCount || getNeighbours(tile).count(a => a.hasBomb && a.isShown) == tile.bombsCount
    then getNeighbours(tile).foreach(a => a.autoMark())
  }

  private def countUnopenedHints(tile: Tile): Int = {
    val neighbours = getNeighbours(tile)
    neighbours.size - neighbours.count(a => a.isShown)
  }

  private def countMarkedTiles(): Int = {
    var markedCount: Int = 0
    for {i <- 0 until xTilesCount
         j <- 0 until yTilesCount} {
      if grid(i)(j).marked && grid(i)(j).hasBomb then markedCount += 1
      else if grid(i)(j).marked then markedCount += 100
    }
    markedCount
  }

  private def winScene(tile: Tile): Unit = {
    root.getChildren.add(
      new Label {
        text = "You win!"
        font = Font.font(42)
        textFill = Color.MidnightBlue
        prefWidth = stageWidth
        alignment = Pos.Center
      })
    val pause: PauseTransition = new PauseTransition(new Duration(3000))
    pause.setOnFinished(e => {
      root = createContent(allBombsCount)
      tile.getParent.getScene.setRoot(root)
    })
    pause.play()
  }

  private def gameoverScene(tile: Tile): Unit = {
    tile.getChildren.add(
      new ImageView {
        image = mine
        fitHeight = tileSize
        fitWidth = tileSize
      })
    root.getChildren.add(
      new Label {
        text = "Game over"
        font = Font.font(42)
        textFill = Color.MidnightBlue
        prefWidth = stageWidth
        alignment = Pos.Center
      })
    val pause: PauseTransition = new PauseTransition(new Duration(3000))
    pause.setOnFinished(e => {
      root = createContent(allBombsCount)
      tile.getParent.getScene.setRoot(root)
    })
    pause.play()
  }

  private def startScene(pane: Pane): Unit = {
    val label: Label = new Label {
      text = "Start"
      font = Font.font(42)
      textFill = Color.MidnightBlue
      prefWidth = stageWidth
      alignment = Pos.Center
    }
    pane.getChildren.add(label)
    val pause: PauseTransition = new PauseTransition(new Duration(1500))
    pause.setOnFinished(e => {
      label.visible = false
    })
    pause.play()
  }

}