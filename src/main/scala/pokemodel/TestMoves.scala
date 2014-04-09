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
  def amountToChangeBy = 1
}


class TestIncreaseSelfDefenseStat extends StatusMove with SelfStatChange {
  override val index = 999
  override val maxPP = 20
  def statToChange = DEFENSE
  def amountToChangeBy = 2
}

class TestIncreaseSelfSpeedStat extends StatusMove with SelfStatChange {
  override val index = 999
  override val maxPP = 20
  def statToChange = SPEED
  def amountToChangeBy = 1
}

class TestIncreaseSelfSpecialStat extends StatusMove with SelfStatChange {
  override val index = 999
  override val maxPP = 20
  def statToChange = SPECIAL
  def amountToChangeBy = 2
}

class TestIncreaseSelfAccuracyStat extends StatusMove with SelfStatChange {
  override val index = 999
  override val maxPP = 20
  def statToChange = ACCURACY
  def amountToChangeBy = 1
}

class TestIncreaseSelfEvasionStat extends StatusMove with SelfStatChange {
  override val index = 999
  override val maxPP = 20
  def statToChange = EVASION
  def amountToChangeBy = 2
}


// ENEMY STAT CHANGE
class TestDecreaseEnemyAttack extends StatusMove with EnemyStatChange {
  override val index = 999
  override val maxPP = 20
  def statToChange = ATTACK
  def amountToChangeBy = -1
  def chanceOfStatChange = 1.0
  def soloStatChange = true
}

class TestDecreaseEnemyDefense extends StatusMove with EnemyStatChange {
  override val index = 999
  override val maxPP = 20
  def statToChange = DEFENSE
  def amountToChangeBy = -2
  def chanceOfStatChange = 1.0
  def soloStatChange = true
}

class TestDecreaseEnemySpeed extends StatusMove with EnemyStatChange {
  override val index = 999
  override val maxPP = 20
  def statToChange = SPEED
  def amountToChangeBy = -1
  def chanceOfStatChange = 1.0
  def soloStatChange = true
}

class TestDecreaseEnemySpecial extends StatusMove with EnemyStatChange {
  override val index = 999
  override val maxPP = 20
  def statToChange = SPECIAL
  def amountToChangeBy = -2
  def chanceOfStatChange = 1.0
  def soloStatChange = true
}

class TestDecreaseEnemyAccuracy extends StatusMove with EnemyStatChange {
  override val index = 999
  override val maxPP = 20
  def statToChange = ACCURACY
  def amountToChangeBy = -1
  def chanceOfStatChange = 1.0
  def soloStatChange = true
}

class TestDecreaseEnemyEvasion extends StatusMove with EnemyStatChange {
  override val index = 999
  override val maxPP = 20
  def statToChange = EVASION
  def amountToChangeBy = -2
  def chanceOfStatChange = 1.0
  def soloStatChange = true
}


// NON VOLATILE STATUS CHANGE
class TestAlwaysBurn extends SpecialMove with NonVolatileStatusChange {
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

class TestSometimesSleep extends SpecialMove with NonVolatileStatusChange {
  override val index = 999
  override val type1 = Normal  // shouldn't be used
  override val power = 40      // shouldn't be used
  override val maxPP = 10
  override val accuracy = 1.0  // always hit, for test purposes

  override def statusAilmentToCause = new SLP
  override def chanceOfCausingAilment = 0.5
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


// SUICIDE DAMAGE
class TestSD extends PhysicalMove with SuicideDamage {
  override val index = 999
  override val power = 350
  override val maxPP = 10
}

// GAIN PROPORTION DAMAGE DEALT
class TestGPDDWRONG extends PhysicalMove with GainPropDamageDealt {
  // This is WRONG! If there's no trait to the right of GPDD that can deal
  // damage, then it doesn't really make sense to think about gaining a
  // proportion of the damage dealt
  override val index = 999
  override val power = 35
  override val maxPP = 10
}

class TestGPDD extends PhysicalMove with GainPropDamageDealt with SingleStrike {
  // This is right! SingleStrike can deal damage, GPDD can restore some HP
  override val index = 999
  override val power = 50
  override val maxPP = 10
}


class TestGPDDConstant extends PhysicalMove with GainPropDamageDealt with ConstantDamage {
  // This is right! ConstantDamage can deal damage, GPDD can restore some HP
  override val index = 999
  override val power = 50
  override val maxPP = 10
  override def damageAmount = 1
}



