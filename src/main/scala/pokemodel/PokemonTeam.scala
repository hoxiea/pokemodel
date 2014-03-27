package pokemodel

class PokemonTeam(val team: List[Pokemon]) {
  require(1 <= team.length && team.length <= 6, "Your team must have 1-6 Pokemon on it!")
  
  def this(singlePokemon: Pokemon) = this(List(singlePokemon))
  
  var activeIndex : Int = team.indexWhere(_.currentHP > 0)   // in 0 .. 5
  def activePokemon : Pokemon = team(activeIndex)
  
  def switch(newIndex : Int, pb : Battle) = {
    require(0 <= newIndex && newIndex <= 5, s"newIndex $newIndex out of range for switch")
    require(newIndex != activeIndex, s"tried to switch the active Pokemon in")
    
    // Take care of things that happen to the previously-active opponent when they switch out
    pb.moveManager.clearLastMove(activePokemon)  // clear the last move of the Pokemon leaving battle
    pb.statManager.resetAll(activePokemon)
    // TODO: clear the effects of Mist
    
    // Update the index
    activeIndex = newIndex
    
    // TODO: Take care of things that happen to the newly-active opponent when they switch in
    activePokemon.takeStatusAilmentDamage()
  }

  def healAll() = { team.map(_.heal()) }
  def hasSomeoneAlive: Boolean = team.exists(_.currentHP > 0)
  def firstPokemonAliveIndex: Int = team.indexWhere(_.currentHP > 0)
  def firstPokemonAlive: Pokemon = team(firstPokemonAliveIndex)
  def switchNeeded: Boolean = !activePokemon.isAlive
  
  
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