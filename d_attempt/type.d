import std.stdio;

import moves;
import pokemon;

enum Type {Normal, Fighting, Flying, Poison, Ground, Rock,
    Bug, Ghost, Fire, Water, Grass, Electric, Psychic, Ice, Dragon}

unittest {
    assert(Type.Normal != Type.Fighting);
    assert(Type.Normal == Type.Normal);
}

/*
class DamageCalculator {
    private double[Type][Type] typeMultiplierTable;
    this() {
        // Initialize type multiplier information
        auto normalTable = [Type.Rock : 0.5, Type.Ghost : 0.0];
        auto fightingTable = [Type.Normal : 2.0, Type.Flying : 0.5, Type.Poison
            : 0.5, Type.Rock : 2.0, Type.Bug : 0.5, Type.Ghost : 0.0,
            Type.Psychic : 0.5, Type.Ice : 2.0]; 
        auto flyingTable = [Type.Fighting : 2.0, Type.Rock : 0.5, Type.Bug :
            2.0, Type.Grass : 2.0, Type.Electric : 0.5];
        auto poisonTable = [Type.Poison : 0.5, Type.Ground : 0.5, Type.Rock : 0.5,
             Type.Bug : 2.0, Type.Ghost : 0.5, Type.Grass : 2.0];
        auto groundTable = [Type.Flying : 0.0, Type.Poison : 2.0, Type.Rock : 2.0,
             Type.Bug : 0.5, Type.Fire : 2.0, Type.Grass : 0.5, Type.Electric : 2.0];
        auto rockTable = [Type.Fighting : 0.5, Type.Flying : 2.0, Type.Ground : 0.5,
             Type.Bug : 2.0, Type.Fire : 2.0, Type.Ice : 2.0];
        auto bugTable = [Type.Fighting : 0.5, Type.Flying : 0.5, Type.Poison : 2.0,
             Type.Ghost : 0.5, Type.Fire : 0.5, Type.Grass : 2.0, Type.Psychic : 2.0];
        auto ghostTable = [Type.Normal : 0.0, Type.Ghost : 2.0, Type.Psychic : 0.0];
        auto fireTable = [Type.Rock : 0.5, Type.Bug : 2.0, Type.Fire : 0.5, Type.Water : 0.5,
             Type.Grass : 2.0, Type.Ice : 2.0, Type.Dragon : 0.5];

        auto fullTable = [Type.Normal : normalTable,
                          Type.Fighting : fightingTable,
                          Type.Flying : flyingTable];
        this.typeMultiplierTable = fullTable;
    }

    static int calculateDamage(Pokemon attacker, ubyte moveIndex, Pokemon defender) {
        return 0;
    }
}

auto buildTypeTable() {
    auto normalTable = [Type.Rock : 0.5, Type.Ghost : 0.0];
    auto fightingTable = [Type.Normal : 2.0, Type.Flying : 0.5, Type.Poison :
        0.5, Type.Rock : 2.0, Type.Bug : 0.5, Type.Ghost : 0.0, Type.Psychic :
        0.5, Type.Ice : 2.0];
    auto flyingTable = [Type.Fighting : 2.0, Type.Rock : 0.5, Type.Bug : 2.0,
         Type.Grass : 2.0, Type.Electric : 0.5];
    auto fullTable = [Type.Normal : normalTable,
                      Type.Fighting : fightingTable,
                      Type.Flying : flyingTable];
    return fullTable;
}

unittest {
    auto t = buildTypeTable();
    assert(t[Type.Normal][Type.Ghost] == 0.0);
}
*/

void main(string[] args) {
    writeln("Hey there!");
}
