package pokemodel

import BattleStat._
import Type._

// SINGLE STRIKE
class TestPhysicalSingleStrike extends PhysicalMove with SingleStrike {
  override val index = 999
  override val power = 40
  override val maxPP = 20
}

// CONSTANT DAMAGE

// MULTI STRIKE
class TestPhysicalMultiStrike extends PhysicalMove with MultiStrike {
  override val index = 999
  override val power = 40
  override val maxPP = 20
  override val accuracy = 1.0
}

// DOUBLE STRIKE
class TestSpecialDoubleStrike extends SpecialMove with DoubleStrike {
  override val index = 999
  override val power = 30
  override val maxPP = 20
  override val accuracy = 1.0
}


// SELF STAT CHANGE
class TestIncreaseSelfAttackStat extends StatusMove with SelfStatChange {
  override val index = 999
  override val maxPP = 20
  def statToChange = ATTACK
  def amountToChangeBy = 3
}


class TestIncreaseSelfDefenseStat extends StatusMove with SelfStatChange {
  override val index = 999
  override val maxPP = 20
  def statToChange = DEFENSE
  def amountToChangeBy = 3
}

// ENEMY STAT CHANGE
class TestDecreaseEnemyDefense extends StatusMove with EnemyStatChange {
  override val index = 999
  override val maxPP = 20
  def statToChange = DEFENSE
  def amountToChangeBy = -3
  def chanceOfStatChange = 1.0
  def soloStatChange = true
}


class TestDecreaseEnemyAttack extends StatusMove with EnemyStatChange {
  override val index = 999
  override val maxPP = 20
  def statToChange = ATTACK
  def amountToChangeBy = -3
  def chanceOfStatChange = 1.0
  def soloStatChange = true
}


// NON VOLATILE STATUS CHANGE
class TestBurner extends SpecialMove with NonVolatileStatusChange {
  override val index = 999
  override val type1 = Fire  // shouldn't be used
  override val power = 40    // shouldn't be used
  override val maxPP = 10
  override val accuracy = 1.0

  override def statusAilmentToCause = new BRN
  override def chanceOfCausingAilment = 1.0
  override def soloStatusChange = true
  override val worksWhenSubPresent = true
}

class TestAsleep extends SpecialMove with NonVolatileStatusChange {
  override val index = 999
  override val type1 = Normal  // shouldn't be used
  override val power = 40      // shouldn't be used
  override val maxPP = 10
  override val accuracy = 1.0  // always hit, for test purposes

  override def statusAilmentToCause = new SLP
  override def chanceOfCausingAilment = 1.0  // always cause, for test purposes
  override def soloStatusChange = true
  override val worksWhenSubPresent = true
}


// DAMAGE EQUALS USER LEVEL
class TestDEUL extends PhysicalMove with DamageEqualsUserLevel {
  override val index = 999
  override val type1 = Fighting
  override val power = 100
  override val maxPP = 10
}

