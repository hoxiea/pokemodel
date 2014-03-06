/* A class that captures a generic pokemon */
// ubyte = 0 .. 255 = 1 byte
// ushort = 0 .. 65535 = 2 bytes

import std.exception;
import std.format;
import std.stdio;
import std.conv;

import pokehelpers;
import status;
import type;

class Pokemon {
    private static ubyte numPokemon = 151;
    private static ubyte maxLevel = 100;

    // Basic Pokemon information
    immutable ubyte id;     // [1 .. 151]
    immutable ubyte level;  // [1 .. 100]

    // Type(s) (Elemental) of the Pokemon
    // If the Pokemon has 2 different types, stored in type1 and type2
    // If the Pokemon only has 1 type, then type1 and type2 store the same type
    immutable Type type1;
    immutable Type type2;

    // Statistics
    // These values are a function of base stats, EVs, and IVs
    private immutable ushort Attack;
    private immutable ushort Defense;
    private immutable ushort Speed;
    private immutable ushort Special;
    private immutable ushort maxHP;

    // Moves: Maybe store an object of class Move instead of a pointer?
    private immutable ubyte move1Index;
    private immutable ubyte move2Index;
    private immutable ubyte move3Index;
    private immutable ubyte move4Index;

    // Effort Values: Attributes that gives bonuses to a Pokemon's stats and
    // improve when a Pokemon is defeated
    private immutable ushort hpEV;
    private immutable ushort attackEV;
    private immutable ushort defenseEV;
    private immutable ushort speedEV;
    private immutable ushort specialEV;

    // Individual Values: these are generated when the Pokemon first appears
    // and produce individual variation between Pokemon of the same kind and
    // level
    private immutable ubyte attackIV;  // [0 .. 15]
    private immutable ubyte defendIV;  // [0 .. 15]
    private immutable ubyte speedIV;   // [0 .. 15]
    private immutable ubyte specialIV; // [0 .. 15]

    // Stuff that changes
    ushort currentHP;        // opponent can see currentHP, just like in game
    Status statusAilment;

    // Number of times the Pokemon can use each of its moves
    // Opponent can keep track of this, so just let him see it
    ubyte move1PP;
    ubyte move2PP;
    ubyte move3PP;
    ubyte move4PP;

    // Constructor, specify desired Pokemon and level; full health, moves
    // randomly selected
    /* this(ubyte id, ubyte level) { */
    /*     enforce(1 <= id && id <= numPokemon, */
    /*             "1 <= id <= " ~ to!string(numPokemon) ~ " required!"); */
    /*     enforce(1 <= level && level <= numPokemon, */
    /*             "1 <= level <= " ~ to!string(maxLevel) ~ " required!"); */
    /* } */
}

/* unittest { */
/*     auto p = new Pokemon; */
/*     assert(p.hpEV == 0); */
/* } */

void main(string[] args) {

}
