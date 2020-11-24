package combat.game

import combat.character.constants.boosterTile
import combat.hexaGrid._

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object FinalBattleTextUI extends App {
  val battle = new FinalBattleSetting
  val board = this.battle.board

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
            s"${this.battle.currentCharacter} stepped on the Tile of Wonder\n" +
              s"${this.battle.currentCharacter}'s stats increased significantly!!!"
          )
        }
      case _ =>
    }
    if (this.battle.turnCount == 0 && this.battle.isNewTurn) {
      this.battle.isNewTurn = false
      this.battle.characters.foreach(_.newTurn())
    }
    if (!this.battle.isCompleted) {
      this.printPretty(s"${this.battle.currentCharacter.name}'s turn")
      if (this.battle.currentCharacter.name == this.battle.boss.name) {
        this.printPretty(this.battle.boss.takeTurn())
        this.battle.nextTurn()
        this.playTurn()
      } else if (!this.battle.currentCharacter.isAlive) {
        this.printPretty(s"${this.battle.currentCharacter.name} is dead!")
        this.battle.nextTurn()
        this.playTurn()
      } else {
        val command = readLine("Command: ")
        this.printPretty(this.battle.playTurn(command))
        this.playTurn()
      }
    }
  }

  def run(): Unit = {
    this.printPretty(this.battle.welcomeMessage)
    playTurn()
  }

  // The game starts here
  run()
}
