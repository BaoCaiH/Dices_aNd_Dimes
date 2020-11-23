package combat.game

import combat.character._
import combat.character.constants.boosterTile
import combat.hexaGrid._
import combat.race._

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object FinalBattle extends App {
  val board = GameBoard
  val boss = Acererak
  val player1 = new ArcaneArcher("Alphonse", new Elf, Vector(19, 20, 15, 16, 11, 10), HexaGridPos(5, 7), board, 110)
  val player2 = new HexBlade("Riptide", new Tiefling, Vector(16, 20, 16, 14, 16, 20), HexaGridPos(9, 3), board, 95)
  val player3 = new Glamour("Cottoni", new Tabaxi, Vector(8, 16, 14, 11, 12, 20), HexaGridPos(17, 3), board, 85)
  val player4 = new OathOfDevotion("Andre", new Halfling, Vector(16, 16, 14, 8, 12, 16), HexaGridPos(21, 7), board, 100)

  board(HexaGridPos(5, 7)).addCharacter(player1)
  board(HexaGridPos(9, 3)).addCharacter(player2)
  board(HexaGridPos(17, 3)).addCharacter(player3)
  board(HexaGridPos(21, 7)).addCharacter(player4)
  board(HexaGridPos(13, 7)).addCharacter(boss)
  board(boosterTile) = Booster

  val players = Vector(player1, player2, player3, player4)

  val characters = (this.players ++ Vector(boss))
    .map(c => (c, c.initiativeRoll))
    .sortBy(_._2)(Ordering[Int].reverse)
    .map(_._1)

  val nChars = characters.length

  val welcomeMessage = "The soul monger was destroyed, it crumpled onto the ground.\n" +
    "But, celebration is yet to come. In the middle of the room, a figure, a bony figure,\n" +
    "with nothing but skins and bones, appeared out of thin air, on top of the remain of the soul monger,\n" +
    "as if destroying the soul monger was a mistake.\n" +
    "The figure appeared to be a lich, this must be the legendary undead Acererak, Alphonse thought.\n" +
    "He can't be destroy but exhaust him might give the characters a chance to live.\n" +
    "OBJECTIVE: Reduce Acererak HP to 0\n" +
    "Win condition: Acererak HP reduced to 0\n" +
    "Lost condition: All the playable characters are dead."

  private var turnCount = 0

  private var isEnded = false

  private var isNewTurn = true

  // The game start here
  run()

  def currentCharacter = this.characters(this.turnCount)

  def isWon: Boolean = !this.boss.isAlive && this.players.exists(_.isAlive)

  def isDefeated: Boolean = !this.players.exists(_.isAlive)

  def isCompleted: Boolean = this.isWon || this.isDefeated || this.isEnded

  def goodByeMessage = {
    if (this.isWon) "Acererak's body turned into dust and dispersed into the air,\n" +
      "although the air was as still as it could be. They better leave this tomb quickly.\n" +
      "A victory? Maybe. At least they are alive.\n" +
      "Little did the adventurers know, Acererak can never be dead,\n" +
      "his presence will continue to haunt the world.\n" +
      "But for the world today, and their world, they won!"
    else if (this.isDefeated) "Acererak swung his hands over the adventurers now stilled body.\n" +
      s"The corpses disappeared. On his hands, there are ${nChars - 1} miniatures resembling\n" +
      "the adventurers. Did they really die or just became tools for the powerful demon.\n" +
      "One thing for certain, they failed."
    else "Nooo, comeback, I promise the game would be fun, don't leave meeeee!!!"
  }

  private def nextTurn(): Unit = {
    this.isNewTurn = true
    this.turnCount = (this.turnCount + 1) % this.nChars
  }

  private def printPad(line: String, wantedLength: Int): Unit = {
    val padLength = wantedLength - line.length
    println("*" + line + " " * padLength + "*")
  }

  private def printPretty(report: String): Unit = {
    val longestCharChain = report.split("\n").maxBy(_.length).length
    println("*" * (longestCharChain + 2))
    report.split("\n").foreach(line => this.printPad(line, longestCharChain))
    println("*" * (longestCharChain + 2))
  }

  @tailrec
  private def playTurn(): Unit = {
    if (this.board(boosterTile).nonEmpty) this.board(boosterTile) match {
      case tile: Booster.type =>
        val isBoosted = tile.boostCharacter()
        if (isBoosted) {
          this.printPretty(
            s"${this.currentCharacter} stepped on the Tile of Wonder\n" +
              s"${this.currentCharacter}'s stats increased significantly!!!"
          )
        }
      case _ =>
    }
    if (this.turnCount == 0 && this.isNewTurn) {
      this.isNewTurn = false
      this.characters.foreach(_.newTurn())
    }
    if (!this.isCompleted) {
      this.printPretty(s"${this.currentCharacter.name}'s turn")
      if (this.currentCharacter.name == this.boss.name) {
        this.printPretty(this.boss.takeTurn())
        this.nextTurn()
        this.playTurn()
      } else {
        val command = readLine("Command: ")
        val action = Action(command)
        val outcomeReport = action.execute(this.currentCharacter, board)
        if (outcomeReport != "next" && outcomeReport != "end") {
          this.printPretty(outcomeReport)
          playTurn()
        } else if (outcomeReport == "next") {
          this.nextTurn()
          playTurn()
        } else if (outcomeReport == "end") {
          this.isEnded = true
          playTurn()
        }
      }
    }
  }

  def run(): Unit = {
    this.printPretty(this.welcomeMessage)
    playTurn()
    this.printPretty(this.goodByeMessage)
  }
}
