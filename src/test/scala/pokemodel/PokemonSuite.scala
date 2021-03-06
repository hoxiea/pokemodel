package pokemodel

import org.scalatest.FunSuite

class PokemonSuite extends FunSuite {
  test("basic Pokemon creation, by index") {
    val pb = new PokemonBuilder(1, 50)
    val p = new Pokemon(pb)
    assert(p.level == 50)
    assert(p.index == 1)
    assert(p.name == "bulbasaur")
  }

  test("basic Pokemon creation, by name") {
    val pb = new PokemonBuilder("Pikachu", 50)
    val p = new Pokemon(pb)
    assert(p.level == 50)
    assert(p.index == 25)
    assert(p.name == "pikachu")
  }

  test("PokemonBuilder should accept varying capitalization and spacing") {
    val pb1 = new PokemonBuilder("Pikachu", 50)
    val pb2 = new PokemonBuilder("charizard", 100)
    val pb3 = new PokemonBuilder("aLaKaZaM", 65)
    val pb4 = new PokemonBuilder("gold   EEN", 65)
    val pb5 = new PokemonBuilder("NIDORANF", 65)
    // no exceptions == good enough for me
  }

  test("create Pokemon, limited health") {
    val pb = new PokemonBuilder("Pikachu", 50).currentHP(20)
    val p = new Pokemon(pb)
    assert (p.currentHP() == 20)
  }

  test("PokemonBuilder.maxOut correctly updates currentHP to new maxHP") {
    val pb = new PokemonBuilder("Pikachu", 50).maxOut()
    val p = new Pokemon(pb)
    assert (p.currentHP() == p.maxHP)
  }

  test("test base stats of Pikachu") {
    // base stats from http://bulbapedia.bulbagarden.net/wiki/Stats#Example
    val pikachuIndex = 25
    assert(PokeData.getBaseHP(pikachuIndex) == 35)
    assert(PokeData.getBaseAttack(pikachuIndex) == 55)
    assert(PokeData.getBaseDefense(pikachuIndex) == 30)
    assert(PokeData.getBaseSpeed(pikachuIndex) == 90)
  }

  test("tweaking PokemonBuilder values") {
    // http://bulbapedia.bulbagarden.net/wiki/Stats#Example
    val pb = new PokemonBuilder("Pikachu", 81)
                 .attackIV(8)
                 .defenseIV(13)
                 .speedIV(5)
                 .specialIV(9)
                 .attackEV(23140)
                 .defenseEV(17280)
                 .speedEV(24795)
                 .specialEV(19625)
                 .hpEV(22850)
    assert(pb.attackIV == 8)
    assert(pb.defenseIV == 13)
    assert(pb.specialIV == 9)
    assert(pb.speedIV == 5)
    assert(pb.hpEV == 22850)
    assert(pb.attackEV == 23140)
    assert(pb.defenseEV == 17280)
    assert(pb.specialEV == 19625)
    assert(pb.speedEV == 24795)
  }

  test("test scaling formulas for HP and stats with level 81 Pikachu") {
    // http://bulbapedia.bulbagarden.net/wiki/Stats#Example
    val pb = new PokemonBuilder("Pikachu", 81)
                 .attackIV(8)
                 .defenseIV(13)
                 .speedIV(5)
                 .specialIV(9)
                 .attackEV(23140)
                 .defenseEV(17280)
                 .speedEV(24795)
                 .specialEV(19625)
                 .hpEV(22850)
    assert(pb.maxHP == 189, "(HP)")
    assert(pb.attack == 137, "(Attack)")
    assert(pb.defense == 101, "(Defense)")
    assert(pb.special == 128, "(Special)")
    assert(pb.speed == 190, "(Speed)")
  }

  test("a Pokemon should be able to learn a move in its learnset") {
    val pokemonIndex = 1
    val pb = new PokemonBuilder(pokemonIndex, 50).move(1, "tackle")
  }

  // TODO: run this once all Moves implemented
//  test("Trying out addRandomMoves... won't work, on average, until more moves implemented") {
//    val pb = PokemonBuilder.generateRandomPokemonBuilder().addRandomMoves()
//    val p = new Pokemon(pb)
//    println(p)
//  }
}
