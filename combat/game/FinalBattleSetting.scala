package combat.game

import combat.character._
import combat.character.constants.boosterTile
import combat.hexaGrid._
import combat.race._

class FinalBattleSetting {
  val board: HexaGrid = GameBoard
  val boss: Acererak.type = Acererak
  // Normal mode
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

  // Hard mode
  //  val player1 = new ArcaneArcher("Alphonse", new Elf, Vector(19, 20, 15, 16, 11, 10), HexaGridPos(5, 7), board, 110)
  //
  //  board(HexaGridPos(5, 7)).addCharacter(player1)
  //  board(HexaGridPos(13, 7)).addCharacter(boss)
  //
  //  val players = Vector(player1)

  val characters: Vector[Character] = (this.players ++ Vector(boss))
    .map(c => (c, c.initiativeRoll))
    .sortBy(_._2)(Ordering[Int].reverse)
    .map(_._1)

  val nChars: Int = characters.length

  val welcomeMessage: String = "The soul monger was destroyed, it crumpled onto the ground.\n" +
    "But, celebration is yet to come. In the middle of the room, a figure, a bony figure,\n" +
    "with nothing but skins and bones, appeared out of thin air, on top of the remain of the soul monger,\n" +
    "as if destroying the soul monger was a mistake.\n" +
    "The figure appeared to be a lich, this must be the legendary undead Acererak, Alphonse thought.\n" +
    "He can't be destroy but exhaust him might give the characters a chance to live.\n" +
    "OBJECTIVE: Reduce Acererak HP to 0\n" +
    "Win condition: Acererak HP reduced to 0\n" +
    "Lost condition: All the playable characters are dead."

  var turnCount = 0

  var isEnded = false

  var isNewTurn = true

  def currentCharacter: Character = this.characters(this.turnCount)

  def isWon: Boolean = !this.boss.isAlive && this.players.exists(_.isAlive)

  def isDefeated: Boolean = !this.players.exists(_.isAlive)

  def isCompleted: Boolean = this.isWon || this.isDefeated || this.isEnded

  def goodByeMessage: String = {
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

  def nextTurn(): Unit = {
    this.isNewTurn = true
    this.turnCount = (this.turnCount + 1) % this.nChars
  }

  def playTurn(command: String): String = {
    val action = Action(command)
    val outcomeReport = action.execute(this.currentCharacter, board)
    if (outcomeReport != "next" && outcomeReport != "end") {
      outcomeReport
    } else if (outcomeReport == "next") {
      this.nextTurn()
      "Next turn"
    } else {
      this.isEnded = true
      this.goodByeMessage
    }
  }
}
