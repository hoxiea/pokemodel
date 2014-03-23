package pokemodel

class PokemonTeam(val team: List[Pokemon]) {
  require(1 <= team.length && team.length <= 6, "Your team must have 1-6 Pokemon on it!")
  
  var activeIndex : Int = team.indexWhere(_.currentHP > 0)   // in 0 .. 5
  def activePokemon : Pokemon = team(activeIndex)
  def this(singlePokemon: Pokemon) = this(List(singlePokemon))
  def healAll() = { team.map(_.heal()) }
  
  def switch(newIndex : Int, pb : Battle) = {
    require(0 <= newIndex && newIndex <= 5, s"newIndex $newIndex out of range for switch")
    require(newIndex != activeIndex, s"tried to switch the active Pokemon in")
    
    // Take care of things that happen to the previously-active opponent when they switch out
    pb.moveManager.clearLastMove(activePokemon)  // clear the last move of the Pokemon leaving battle
    // TODO: clear the effects of Mist
    
    // Update the index
    activeIndex = newIndex
    
    // Take care of things that happen to the newly-active opponent when they switch it
    activePokemon.takeStatusAilmentDamage()
  }
  
  def hasSomeoneAlive: Boolean = team.exists(_.currentHP > 0)
  private def firstPokemonAliveIndex: Int = team.indexWhere(_.currentHP > 0)
  def firstPokemonAlive: Pokemon = team(firstPokemonAliveIndex)
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