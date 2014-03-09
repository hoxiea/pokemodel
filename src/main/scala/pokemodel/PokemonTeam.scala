package pokemodel

class PokemonTeam(team: List[Pokemon]) {
  require(1 <= team.length && team.length <= 6, "Your team must have 1-6 Pokemon on it!")
  
  var activePokemon : Option[Pokemon]= team.find(_.currentHP > 0)
  
  def healAll() {
    team.map(_.heal())
  }
  
  def switch(index : Int) = {
  
  }
  
  def hasSomeoneAlive : Boolean = team.exists(_.currentHP > 0)
}