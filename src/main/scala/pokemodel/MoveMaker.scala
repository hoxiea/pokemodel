package pokemodel

/*
 * This file handles the creation of move instances (MoveMaker), and provides
 * access to a canonical implementation of each Move (MoveDepot).
 *
 * Moves are immutable objects (Pokemon store their move PPs, which are the
 * only moving part), so it's wasteful to have each Pokemon creating separate,
 * identical instances of Moves. Instead, they just refer to the canonical
 * implementation whenever they need to.
 *
 * Note that it's still sometimes useful to be able to create a new copy of a
 * move and use it. Metronome, for example, takes advantage of this
 * functionality. But 99% of the time, the Move in MoveDepot should be what
 * you're looking for.
 *
 * TODO: Only instantiate Moves you need? Could be nice, and even less memory.
 */

object MoveDepot {
  // Access by moveIndex: Int
  // usage: MoveDepot(1) => Pound
  private val moveMap: Map[Int, Move] =
    (1 to 165).zip((1 to 165).map(MoveMaker.makeMove(_))).toMap

  def apply(moveIndex: Int): Move = {
    require(1 <= moveIndex && moveIndex <= 165,
      "illegal move index for MoveDepot")
    moveMap(moveIndex)
  }

  // Access by moveName: String
  // usage: MoveDepot("tackle") => Tackle
  // usage: MoveDepot("dragonrage") => DragonRage
  private def processMoveName(s: String) = s.replaceAll("-", "").toLowerCase

  private val moveNameMap: Map[String, Move] = {
    val moveNames = (1 to 165).map(moveMap(_)).map(m => processMoveName(m.toString))
    moveNames.zip((1 to 165).map(apply(_))).toMap
  }

  def apply(moveName: String): Move = {
    require(moveNameMap contains moveName,
      "illegal move name for MoveDepot")
    moveNameMap(moveName)
  }

  // Easy way to get maxPP for a move
  def maxPP(moveIndex: Int): Int = {
    require(1 <= moveIndex && moveIndex <= 165,
      "illegal move index MoveDepot.maxPP")
    apply(moveIndex).maxPP
  }
}

object MoveMaker {
  def makeMove(moveIndex: Int): Move = {
    require(1 <= moveIndex && moveIndex <= 165, "illegal move index passed to makeMove")
    moveIndex match {
      case 1 => new Pound
      case 2 => new KarateChop
      case 3 => new DoubleSlap
      case 4 => new CometPunch
      case 5 => new MegaPunch
      case 6 => new PayDay
      case 7 => new FirePunch
      case 8 => new IcePunch
      case 9 => new ThunderPunch
      case 10 => new Scratch
      case 11 => new ViceGrip
      case 12 => new Guillotine
      case 13 => new RazorWind
      case 14 => new SwordsDance
      case 15 => new Cut
      case 16 => new Gust
      case 17 => new WingAttack
      case 18 => new Whirlwind
      case 19 => new Fly
      case 20 => new Bind
      case 21 => new Slam
      case 22 => new VineWhip
      case 23 => new Stomp
      case 24 => new DoubleKick
      case 25 => new MegaKick
      case 26 => new JumpKick
      case 27 => new RollingKick
      case 28 => new SandAttack
      case 29 => new Headbutt
      case 30 => new HornAttack
      case 31 => new FuryAttack
      case 32 => new HornDrill
      case 33 => new Tackle
      case 34 => new BodySlam
      case 35 => new Wrap
      case 36 => new TakeDown
      case 37 => new Thrash
      case 38 => new DoubleEdge
      case 39 => new TailWhip
      case 40 => new PoisonSting
      case 41 => new Twineedle
      case 42 => new PinMissile
      case 43 => new Leer
      case 44 => new Bite
      case 45 => new Growl
      case 46 => new Roar
      case 47 => new Sing
      case 48 => new Supersonic
      case 49 => new SonicBoom
      case 50 => new Disable
      case 51 => new Acid
      case 52 => new Ember
      case 53 => new Flamethrower
      case 54 => new Mist
      case 55 => new WaterGun
      case 56 => new HydroPump
      case 57 => new Surf
      case 58 => new IceBeam
      case 59 => new Blizzard
      case 60 => new Psybeam
      case 61 => new BubbleBeam
      case 62 => new AuroraBeam
      case 63 => new HyperBeam
      case 64 => new Peck
      case 65 => new DrillPeck
      case 66 => new Submission
      case 67 => new LowKick
      case 68 => new Counter
      case 69 => new SeismicToss
      case 70 => new Strength
      case 71 => new Absorb
      case 72 => new MegaDrain
      case 73 => new LeechSeed
      case 74 => new Growth
      case 75 => new RazorLeaf
      case 76 => new SolarBeam
      case 77 => new PoisonPowder
      case 78 => new StunSpore
      case 79 => new SleepPowder
      case 80 => new PetalDance
      case 81 => new StringShot
      case 82 => new DragonRage
      case 83 => new FireSpin
      case 84 => new ThunderShock
      case 85 => new Thunderbolt
      case 86 => new ThunderWave
      case 87 => new Thunder
      case 88 => new RockThrow
      case 89 => new Earthquake
      case 90 => new Fissure
      case 91 => new Dig
      case 92 => new Toxic
      case 93 => new Confusion
      case 94 => new Psychic
      case 95 => new Hypnosis
      case 96 => new Meditate
      case 97 => new Agility
      case 98 => new QuickAttack
      case 99 => new Rage
      case 100 => new Teleport
      case 101 => new NightShade
      case 102 => new Mimic
      case 103 => new Screech
      case 104 => new DoubleTeam
      case 105 => new Recover
      case 106 => new Harden
      case 107 => new Minimize
      case 108 => new Smokescreen
      case 109 => new ConfuseRay
      case 110 => new Withdraw
      case 111 => new DefenseCurl
      case 112 => new Barrier
      case 113 => new LightScreen
      case 114 => new Haze
      case 115 => new Reflect
      case 116 => new FocusEnergy
      case 117 => new Bide
      case 118 => new Metronome
      case 119 => new MirrorMove
      case 120 => new Selfdestruct
      case 121 => new EggBomb
      case 122 => new Lick
      case 123 => new Smog
      case 124 => new Sludge
      case 125 => new BoneClub
      case 126 => new FireBlast
      case 127 => new Waterfall
      case 128 => new Clamp
      case 129 => new Swift
      case 130 => new SkullBash
      case 131 => new SpikeCannon
      case 132 => new Constrict
      case 133 => new Amnesia
      case 134 => new Kinesis
      case 135 => new Softboiled
      case 136 => new HiJumpKick
      case 137 => new Glare
      case 138 => new DreamEater
      case 139 => new PoisonGas
      case 140 => new Barrage
      case 141 => new LeechLife
      case 142 => new LovelyKiss
      case 143 => new SkyAttack
      case 144 => new Transform
      case 145 => new Bubble
      case 146 => new DizzyPunch
      case 147 => new Spore
      case 148 => new Flash
      case 149 => new Psywave
      case 150 => new Splash
      case 151 => new AcidArmor
      case 152 => new Crabhammer
      case 153 => new Explosion
      case 154 => new FurySwipes
      case 155 => new Bonemerang
      case 156 => new Rest
      case 157 => new RockSlide
      case 158 => new HyperFang
      case 159 => new Sharpen
      case 160 => new Conversion
      case 161 => new TriAttack
      case 162 => new SuperFang
      case 163 => new Slash
      case 164 => new Substitute
      case 165 => new Struggle
    }
  }
}

object TestMoveMaker {
  private def testMoveMap = Map(
    "TestPhysicalSingleStrike" -> new TestPhysicalSingleStrike
  )

  def apply(moveName: String): Move = testMoveMap(moveName)
}
