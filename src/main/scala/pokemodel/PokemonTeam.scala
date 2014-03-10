package pokemodel

class PokemonTeam(team: List[Pokemon]) {
  require(1 <= team.length && team.length <= 6, "Your team must have 1-6 Pokemon on it!")
  var activeIndex : Int = team.indexWhere(_.currentHP > 0)   // in 0 .. 5
  def activePokemon : Pokemon = team(activeIndex)
  
  def healAll() {
    team.map(_.heal())
  }
  
  def switch(newIndex : Int) = {
    require(0 <= newIndex && newIndex <= 5, s"newIndex $newIndex out of range for switch")
    require(newIndex != activeIndex, s"tried to switch the active Pokemon in")
    activeIndex = newIndex
  }
  
  def hasSomeoneAlive : Boolean = team.exists(_.currentHP > 0)
}