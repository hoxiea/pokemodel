package pokemodel

class PokemonTeam(val team: List[Pokemon]) {
  require(1 <= team.length && team.length <= 6, "Your team must have 1-6 Pokemon on it!")

  def this(singlePokemon: Pokemon) = this(List(singlePokemon))

  // activeIndex is what we track/change
  // it's between 1 and 6
  private var activeIndex: Int = 1 + team.indexWhere(_.currentHP() > 0)

  // Since Lists are 0-indexed in Scala, we substract 1 here
  def activePokemon: Pokemon = team(activeIndex - 1)

  def switch(newIndex: Int, pb: Battle) = {
    require(1 <= newIndex && newIndex <= 6, s"PT.switch takes a value between 1 and 6")
    require(newIndex != activeIndex, s"you tried to switch in the already-active Pokemon")

    // Take care of things that happen to the previously-active opponent when they switch out
    pb.moveManager.clearLastMove(activePokemon)  // clear the last move of the Pokemon leaving battle
    pb.statManager.resetAll(activePokemon)
    // TODO: process the switching out of the current active Pokemon in all managers/trackers

    // Update the index
    activeIndex = newIndex

    // TODO: process switch-in for the new Pokemon
    activePokemon.takeStatusAilmentDamage()
  }

  def hasSomeoneAlive: Boolean = team.exists(_.currentHP() > 0)
  def firstPokemonAliveIndex: Int = team.indexWhere(_.currentHP() > 0)
  def firstPokemonAlive: Pokemon = team(firstPokemonAliveIndex)
  def switchNeeded: Boolean = !activePokemon.isAlive

  def healAll() { team.map(_.heal()) }

  def useMove(moveslot: Int, enemy: Pokemon, b: Battle): MoveResult = {
    // Cause the active Pokemon to use the Move at move slot $moveslot
    require(enemy != activePokemon, "PT.useMove must act against another Pokemon")
    activePokemon.useMove(moveslot, enemy, b)
  }

  def useMove(moveslot: Int, enemyTeam: PokemonTeam, b: Battle): MoveResult = {
    // Cause the active Pokemon to use the Move at move slot $moveslot
    require(enemyTeam != this, "PT.useMove must act against another PokemonTeam")
    useMove(moveslot, enemyTeam.activePokemon, b)
  }

  /* CONVENIENCE FUNCTIONS */
  def length = team.length
  override def toString() = {
    val s = new StringBuilder()
    for (pokemon <- team) {
      s.append(pokemon.toString())
      s.append("\n\n")
    }
    s.toString()
  }
}
