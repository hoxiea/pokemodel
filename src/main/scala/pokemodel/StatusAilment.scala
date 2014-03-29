package pokemodel

sealed abstract class StatusAilment

sealed abstract class NonVolatileStatusAilment extends StatusAilment
case class SLP()  extends NonVolatileStatusAilment
case class PSN()  extends NonVolatileStatusAilment
case class BPSN() extends NonVolatileStatusAilment
case class BRN()  extends NonVolatileStatusAilment
case class FRZ()  extends NonVolatileStatusAilment
case class PAR()  extends NonVolatileStatusAilment

sealed abstract class VolatileStatusAilment extends StatusAilment
case class FLINCH()           extends VolatileStatusAilment
case class CONFUSION()        extends VolatileStatusAilment
case class PARTIALLYTRAPPED() extends VolatileStatusAilment
case class SEEDED()           extends VolatileStatusAilment
