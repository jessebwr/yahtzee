#include "nifpp.h"
#include <stdio.h>
#include <vector>
#include <functional>   // std::minus
#include <numeric>      // std::accumulate
#include <tuple>
#include <unordered_map>
#include <math.h>



/*****************************************************************************
 *                                 Dice Class                                *
 ****************************************************************************/

class Dice{
public:
  std::vector<int> dice;
  std::vector<int> unsortedDice;
  std::unordered_map<int, int> diceDict;
  Dice(ErlNifEnv* env, ERL_NIF_TERM diceList);
  Dice(std::vector<int> diceList);
  int getTotalSum();
  int getSumOfNumber(int num);

  bool threeOfAKind();
  bool fourOfAKind();
  bool fullHouse();
  bool smallStraight();
  bool largeStraight();
  bool yahtzee();
  /*
   * 0 -> Ones, 1 -> Twos, 2 -> Threes, 3 -> Fours, 4 -> Fives
   * 5 -> sixes, 6 -> three of a kind, 7 -> four of a kind, 8 -> full house
   * 9 -> small straight, 10 -> large straight, 11 -> yahtzee,
   * 12 -> chance
   */
  int getValueForChoice(int scoreChoice);
};

Dice::Dice(ErlNifEnv* env, ERL_NIF_TERM diceList){
  // Getting the list of dice [1,2,3,4,5] (or something like that)
  nifpp::get_throws(env, diceList, dice);
  nifpp::get_throws(env, diceList, unsortedDice);
  std::sort(dice.begin(), dice.end());
  int currentDie;

  for (int i = 0; i < dice.size(); i++)
    diceDict[i] = 0;

  for (int j = 0; j < dice.size(); j++){
    currentDie = dice[j];
    diceDict[currentDie] = diceDict[currentDie] + 1;
  }
}

Dice::Dice(std::vector<int> diceList){
  dice = diceList;
  unsortedDice = diceList;
  std::sort(dice.begin(), dice.end());
  int currentDie;

  for (int i = 0; i < dice.size(); i++)
    diceDict[i] = 0;

  for (int j = 0; j < dice.size(); j++){
    currentDie = dice[j];
    diceDict[currentDie] = diceDict[currentDie] + 1;
  }
}

int Dice::getTotalSum(){
  return std::accumulate(dice.begin(), dice.end(), 0);
}

int Dice::getSumOfNumber(int numd){
  int num = numd + 1;
  size_t sum = 0;
  for(size_t i=0; i < dice.size(); i++){
    if (dice[i] == num)
      sum += num;
  }
  return sum;
}

bool Dice::threeOfAKind(){
  size_t moo = 1;
  int prevInt;
  for(size_t i = 0; i < dice.size(); i++){
    if (moo == 0)
      prevInt = dice[i];

    else if (dice[i] == prevInt)
      moo++;

    else{
      prevInt = dice[i];
      moo = 0;
    }
  }

  if (moo >= 3)
    return true;
  return false;
}

bool Dice::fourOfAKind(){
  size_t moo = 1;
  int prevInt;
  for(size_t i = 0; i < dice.size(); i++){
    if (moo == 0)
      prevInt = dice[i];

    else if (dice[i] == prevInt)
      moo++;

    else{
      prevInt = dice[i];
      moo = 1;
    }
  }

  if (moo >= 4)
    return true;
  return false;
}

bool Dice::fullHouse(){
  return ((dice[0] == dice[1]) && (dice[3] == dice[4])) && ((dice[2] == dice[1]) ^ (dice[2] == dice[3]));
}

bool Dice::smallStraight(){
  size_t moo = 1;
  int prevInt;
  for(size_t i = 0; i < dice.size(); i++){
    if (i == 0)
      prevInt = dice[i];

    else if (dice[i] == prevInt + 1){
      prevInt = dice[i];
      moo++;
    }

    else{
      prevInt = dice[i];
      moo = 1;
    }
  }

  if (moo >= 4)
    return true;
  return false;
}

bool Dice::largeStraight(){
  size_t moo = 1;
  int prevInt;
  for(size_t i = 0; i < dice.size(); i++){
    if (i == 0)
      prevInt = dice[i];

    else if (dice[i] == prevInt + 1){
      prevInt = dice[i];
      moo++;
    }

    else{
      prevInt = dice[i];
      moo = 1;
    }
  }

  if (moo >= 5)
    return true;
  return false;
}

bool Dice::yahtzee(){
  int num = dice[0];
  for (size_t i = 1; i < dice.size(); i++){
    if (dice[i] != num)
      return false;
  }
  return true;
}


int Dice::getValueForChoice(int decision){
  if (decision < 6)
    return getSumOfNumber(decision);
  else if (decision == 6 && threeOfAKind())
    return getTotalSum();
  else if (decision == 7 && fourOfAKind())
    return getTotalSum();
  else if (decision == 8 && fullHouse())
    return 25;
  else if (decision == 9 && smallStraight())
    return 30;
  else if (decision == 10 && largeStraight())
    return 40;
  else if (decision == 11 && yahtzee())
    return 50;
  else if (decision == 12)
    return getTotalSum();
  else
    return 0;
}


// /*****************************************************************************
//  *                              End Dice Class                               *
//  ****************************************************************************/

std::vector<std::vector<int>> permutations {{1, 1, 1, 1, 1}, {1, 1, 1, 1, 2}, {1, 1, 1, 1, 3}, 
{1, 1, 1, 1, 4}, {1, 1, 1, 1, 5}, {1, 1, 1, 1, 6}, {1, 1, 1, 2, 2}, {1, 1, 1, 2, 3}, 
{1, 1, 1, 2, 4}, {1, 1, 1, 2, 5}, {1, 1, 1, 2, 6}, {1, 1, 1, 3, 3}, {1, 1, 1, 3, 4}, 
{1, 1, 1, 3, 5}, {1, 1, 1, 3, 6}, {1, 1, 1, 4, 4}, {1, 1, 1, 4, 5}, {1, 1, 1, 4, 6}, 
{1, 1, 1, 5, 5}, {1, 1, 1, 5, 6}, {1, 1, 1, 6, 6}, {1, 1, 2, 2, 2}, {1, 1, 2, 2, 3}, 
{1, 1, 2, 2, 4}, {1, 1, 2, 2, 5}, {1, 1, 2, 2, 6}, {1, 1, 2, 3, 3}, {1, 1, 2, 3, 4}, 
{1, 1, 2, 3, 5}, {1, 1, 2, 3, 6}, {1, 1, 2, 4, 4}, {1, 1, 2, 4, 5}, {1, 1, 2, 4, 6}, 
{1, 1, 2, 5, 5}, {1, 1, 2, 5, 6}, {1, 1, 2, 6, 6}, {1, 1, 3, 3, 3}, {1, 1, 3, 3, 4}, 
{1, 1, 3, 3, 5}, {1, 1, 3, 3, 6}, {1, 1, 3, 4, 4}, {1, 1, 3, 4, 5}, {1, 1, 3, 4, 6}, 
{1, 1, 3, 5, 5}, {1, 1, 3, 5, 6}, {1, 1, 3, 6, 6}, {1, 1, 4, 4, 4}, {1, 1, 4, 4, 5}, 
{1, 1, 4, 4, 6}, {1, 1, 4, 5, 5}, {1, 1, 4, 5, 6}, {1, 1, 4, 6, 6}, {1, 1, 5, 5, 5}, 
{1, 1, 5, 5, 6}, {1, 1, 5, 6, 6}, {1, 1, 6, 6, 6}, {1, 2, 2, 2, 2}, {1, 2, 2, 2, 3}, 
{1, 2, 2, 2, 4}, {1, 2, 2, 2, 5}, {1, 2, 2, 2, 6}, {1, 2, 2, 3, 3}, {1, 2, 2, 3, 4}, 
{1, 2, 2, 3, 5}, {1, 2, 2, 3, 6}, {1, 2, 2, 4, 4}, {1, 2, 2, 4, 5}, {1, 2, 2, 4, 6}, 
{1, 2, 2, 5, 5}, {1, 2, 2, 5, 6}, {1, 2, 2, 6, 6}, {1, 2, 3, 3, 3}, {1, 2, 3, 3, 4}, 
{1, 2, 3, 3, 5}, {1, 2, 3, 3, 6}, {1, 2, 3, 4, 4}, {1, 2, 3, 4, 5}, {1, 2, 3, 4, 6}, 
{1, 2, 3, 5, 5}, {1, 2, 3, 5, 6}, {1, 2, 3, 6, 6}, {1, 2, 4, 4, 4}, {1, 2, 4, 4, 5}, 
{1, 2, 4, 4, 6}, {1, 2, 4, 5, 5}, {1, 2, 4, 5, 6}, {1, 2, 4, 6, 6}, {1, 2, 5, 5, 5}, 
{1, 2, 5, 5, 6}, {1, 2, 5, 6, 6}, {1, 2, 6, 6, 6}, {1, 3, 3, 3, 3}, {1, 3, 3, 3, 4}, 
{1, 3, 3, 3, 5}, {1, 3, 3, 3, 6}, {1, 3, 3, 4, 4}, {1, 3, 3, 4, 5}, {1, 3, 3, 4, 6}, 
{1, 3, 3, 5, 5}, {1, 3, 3, 5, 6}, {1, 3, 3, 6, 6}, {1, 3, 4, 4, 4}, {1, 3, 4, 4, 5}, 
{1, 3, 4, 4, 6}, {1, 3, 4, 5, 5}, {1, 3, 4, 5, 6}, {1, 3, 4, 6, 6}, {1, 3, 5, 5, 5}, 
{1, 3, 5, 5, 6}, {1, 3, 5, 6, 6}, {1, 3, 6, 6, 6}, {1, 4, 4, 4, 4}, {1, 4, 4, 4, 5}, 
{1, 4, 4, 4, 6}, {1, 4, 4, 5, 5}, {1, 4, 4, 5, 6}, {1, 4, 4, 6, 6}, {1, 4, 5, 5, 5}, 
{1, 4, 5, 5, 6}, {1, 4, 5, 6, 6}, {1, 4, 6, 6, 6}, {1, 5, 5, 5, 5}, {1, 5, 5, 5, 6}, 
{1, 5, 5, 6, 6}, {1, 5, 6, 6, 6}, {1, 6, 6, 6, 6}, {2, 2, 2, 2, 2}, {2, 2, 2, 2, 3}, 
{2, 2, 2, 2, 4}, {2, 2, 2, 2, 5}, {2, 2, 2, 2, 6}, {2, 2, 2, 3, 3}, {2, 2, 2, 3, 4}, 
{2, 2, 2, 3, 5}, {2, 2, 2, 3, 6}, {2, 2, 2, 4, 4}, {2, 2, 2, 4, 5}, {2, 2, 2, 4, 6}, 
{2, 2, 2, 5, 5}, {2, 2, 2, 5, 6}, {2, 2, 2, 6, 6}, {2, 2, 3, 3, 3}, {2, 2, 3, 3, 4}, 
{2, 2, 3, 3, 5}, {2, 2, 3, 3, 6}, {2, 2, 3, 4, 4}, {2, 2, 3, 4, 5}, {2, 2, 3, 4, 6}, 
{2, 2, 3, 5, 5}, {2, 2, 3, 5, 6}, {2, 2, 3, 6, 6}, {2, 2, 4, 4, 4}, {2, 2, 4, 4, 5}, 
{2, 2, 4, 4, 6}, {2, 2, 4, 5, 5}, {2, 2, 4, 5, 6}, {2, 2, 4, 6, 6}, {2, 2, 5, 5, 5}, 
{2, 2, 5, 5, 6}, {2, 2, 5, 6, 6}, {2, 2, 6, 6, 6}, {2, 3, 3, 3, 3}, {2, 3, 3, 3, 4}, 
{2, 3, 3, 3, 5}, {2, 3, 3, 3, 6}, {2, 3, 3, 4, 4}, {2, 3, 3, 4, 5}, {2, 3, 3, 4, 6}, 
{2, 3, 3, 5, 5}, {2, 3, 3, 5, 6}, {2, 3, 3, 6, 6}, {2, 3, 4, 4, 4}, {2, 3, 4, 4, 5}, 
{2, 3, 4, 4, 6}, {2, 3, 4, 5, 5}, {2, 3, 4, 5, 6}, {2, 3, 4, 6, 6}, {2, 3, 5, 5, 5}, 
{2, 3, 5, 5, 6}, {2, 3, 5, 6, 6}, {2, 3, 6, 6, 6}, {2, 4, 4, 4, 4}, {2, 4, 4, 4, 5}, 
{2, 4, 4, 4, 6}, {2, 4, 4, 5, 5}, {2, 4, 4, 5, 6}, {2, 4, 4, 6, 6}, {2, 4, 5, 5, 5}, 
{2, 4, 5, 5, 6}, {2, 4, 5, 6, 6}, {2, 4, 6, 6, 6}, {2, 5, 5, 5, 5}, {2, 5, 5, 5, 6}, 
{2, 5, 5, 6, 6}, {2, 5, 6, 6, 6}, {2, 6, 6, 6, 6}, {3, 3, 3, 3, 3}, {3, 3, 3, 3, 4}, 
{3, 3, 3, 3, 5}, {3, 3, 3, 3, 6}, {3, 3, 3, 4, 4}, {3, 3, 3, 4, 5}, {3, 3, 3, 4, 6}, 
{3, 3, 3, 5, 5}, {3, 3, 3, 5, 6}, {3, 3, 3, 6, 6}, {3, 3, 4, 4, 4}, {3, 3, 4, 4, 5}, 
{3, 3, 4, 4, 6}, {3, 3, 4, 5, 5}, {3, 3, 4, 5, 6}, {3, 3, 4, 6, 6}, {3, 3, 5, 5, 5}, 
{3, 3, 5, 5, 6}, {3, 3, 5, 6, 6}, {3, 3, 6, 6, 6}, {3, 4, 4, 4, 4}, {3, 4, 4, 4, 5}, 
{3, 4, 4, 4, 6}, {3, 4, 4, 5, 5}, {3, 4, 4, 5, 6}, {3, 4, 4, 6, 6}, {3, 4, 5, 5, 5}, 
{3, 4, 5, 5, 6}, {3, 4, 5, 6, 6}, {3, 4, 6, 6, 6}, {3, 5, 5, 5, 5}, {3, 5, 5, 5, 6}, 
{3, 5, 5, 6, 6}, {3, 5, 6, 6, 6}, {3, 6, 6, 6, 6}, {4, 4, 4, 4, 4}, {4, 4, 4, 4, 5}, 
{4, 4, 4, 4, 6}, {4, 4, 4, 5, 5}, {4, 4, 4, 5, 6}, {4, 4, 4, 6, 6}, {4, 4, 5, 5, 5}, 
{4, 4, 5, 5, 6}, {4, 4, 5, 6, 6}, {4, 4, 6, 6, 6}, {4, 5, 5, 5, 5}, {4, 5, 5, 5, 6}, 
{4, 5, 5, 6, 6}, {4, 5, 6, 6, 6}, {4, 6, 6, 6, 6}, {5, 5, 5, 5, 5}, {5, 5, 5, 5, 6}, 
{5, 5, 5, 6, 6}, {5, 5, 6, 6, 6}, {5, 6, 6, 6, 6}, {6, 6, 6, 6, 6}};

double sum(std::vector<double> v){
  return std::accumulate(v.begin(), v.end(), 0);
}

int sum(std::vector<int> v){
  return std::accumulate(v.begin(), v.end(), 0);
}


// After my discussions with Jim RAYNOR (jk jk, Prof Jim) I tried sooooo many
// different values for these to try to get optimal stuff... Anyways, it isn't
// implemented, but I need to have some sort of diminishing heuristicVector
// because the probability of getting any one of these as the game goes on
// decreases over each turn.

const std::vector<double> heuristicVector {1.5, 5.2, 8.5, 
                                           12.0, 15.5, 19.0, 
                                           22.0, 13.0, 22.5, 
                                           29.0,33.0, 17.0, 
                                           22.0};

// const std::vector<double> heuristicVector {1.5, 4.5, 8.0, 
//                                            11.0, 15.5, 18.0, 
//                                            18.0, 10.0, 20.5, 
//                                            24.0, 29.0, 10.0, 
//                                            21.0};


const std::vector<double> decayVector{1.0284, 0.9823, 0.9753,
                                      0.9710, 0.9683, 0.96654,
                                      0.9696, 0.9324, 0.9278,
                                      0.96315, 0.90977, 0.8465,
                                      1.0049};

// const std::vector<double> decayVector{1, 1, 1,
//                                       1, 1, 1,
//                                       1, 1, 1,
//                                       1, 1, 1,
//                                       1};

std::vector<double> getHeuristicVectorGivenTurn(int turn){
  std::vector<double> newHeuristicVector;
  int mult = turn - 1;
  for (int i = 0; i < 13; i++){
    newHeuristicVector.push_back(heuristicVector[i]*pow(decayVector[i], mult));
  }
  return newHeuristicVector;
}
     
double expectedValueOfFiveDice(Dice dice, int turn){
  double value = 0;
  std::vector<double> turnHeuristicVector = getHeuristicVectorGivenTurn(turn);
  double heuristicSum = sum(turnHeuristicVector);
  for (int i = 1; i <= 13; i++){
    value += (heuristicSum + dice.getValueForChoice(i) - turnHeuristicVector[i-1]);
  }
  return value/13;
}


std::vector<double> expectedValuesOfArrayOfDice(std::vector<std::vector<int>> permutations, int turn){
  std::vector<double> heuristicEV;
  for(int i = 0; i < 252; i++){
    Dice tempDice(permutations[i]);
    heuristicEV[i] = expectedValueOfFiveDice(tempDice, turn);
  }
  return heuristicEV;
}


std::vector<std::vector<int>> fourPermutations {{1, 1, 1, 1}, {1, 1, 1, 2}, 
{1, 1, 1, 3}, {1, 1, 1, 4}, {1, 1, 1, 5}, {1, 1, 1, 6}, {1, 1, 2, 2}, {1, 1, 2, 3}, 
{1, 1, 2, 4}, {1, 1, 2, 5}, {1, 1, 2, 6}, {1, 1, 3, 3}, {1, 1, 3, 4}, {1, 1, 3, 5}, 
{1, 1, 3, 6}, {1, 1, 4, 4}, {1, 1, 4, 5}, {1, 1, 4, 6}, {1, 1, 5, 5}, {1, 1, 5, 6}, 
{1, 1, 6, 6}, {1, 2, 2, 2}, {1, 2, 2, 3}, {1, 2, 2, 4}, {1, 2, 2, 5}, {1, 2, 2, 6}, 
{1, 2, 3, 3}, {1, 2, 3, 4}, {1, 2, 3, 5}, {1, 2, 3, 6}, {1, 2, 4, 4}, {1, 2, 4, 5}, 
{1, 2, 4, 6}, {1, 2, 5, 5}, {1, 2, 5, 6}, {1, 2, 6, 6}, {1, 3, 3, 3}, {1, 3, 3, 4}, 
{1, 3, 3, 5}, {1, 3, 3, 6}, {1, 3, 4, 4}, {1, 3, 4, 5}, {1, 3, 4, 6}, {1, 3, 5, 5}, 
{1, 3, 5, 6}, {1, 3, 6, 6}, {1, 4, 4, 4}, {1, 4, 4, 5}, {1, 4, 4, 6}, {1, 4, 5, 5}, 
{1, 4, 5, 6}, {1, 4, 6, 6}, {1, 5, 5, 5}, {1, 5, 5, 6}, {1, 5, 6, 6}, {1, 6, 6, 6}, 
{2, 2, 2, 2}, {2, 2, 2, 3}, {2, 2, 2, 4}, {2, 2, 2, 5}, {2, 2, 2, 6}, {2, 2, 3, 3}, 
{2, 2, 3, 4}, {2, 2, 3, 5}, {2, 2, 3, 6}, {2, 2, 4, 4}, {2, 2, 4, 5}, {2, 2, 4, 6}, 
{2, 2, 5, 5}, {2, 2, 5, 6}, {2, 2, 6, 6}, {2, 3, 3, 3}, {2, 3, 3, 4}, {2, 3, 3, 5}, 
{2, 3, 3, 6}, {2, 3, 4, 4}, {2, 3, 4, 5}, {2, 3, 4, 6}, {2, 3, 5, 5}, {2, 3, 5, 6}, 
{2, 3, 6, 6}, {2, 4, 4, 4}, {2, 4, 4, 5}, {2, 4, 4, 6}, {2, 4, 5, 5}, {2, 4, 5, 6}, 
{2, 4, 6, 6}, {2, 5, 5, 5}, {2, 5, 5, 6}, {2, 5, 6, 6}, {2, 6, 6, 6}, {3, 3, 3, 3}, 
{3, 3, 3, 4}, {3, 3, 3, 5}, {3, 3, 3, 6}, {3, 3, 4, 4}, {3, 3, 4, 5}, {3, 3, 4, 6}, 
{3, 3, 5, 5}, {3, 3, 5, 6}, {3, 3, 6, 6}, {3, 4, 4, 4}, {3, 4, 4, 5}, {3, 4, 4, 6}, 
{3, 4, 5, 5}, {3, 4, 5, 6}, {3, 4, 6, 6}, {3, 5, 5, 5}, {3, 5, 5, 6}, {3, 5, 6, 6}, 
{3, 6, 6, 6}, {4, 4, 4, 4}, {4, 4, 4, 5}, {4, 4, 4, 6}, {4, 4, 5, 5}, {4, 4, 5, 6}, 
{4, 4, 6, 6}, {4, 5, 5, 5}, {4, 5, 5, 6}, {4, 5, 6, 6}, {4, 6, 6, 6}, {5, 5, 5, 5}, 
{5, 5, 5, 6}, {5, 5, 6, 6}, {5, 6, 6, 6}, {6, 6, 6, 6}};


std::vector<std::vector<int>> threePermutations {{1, 1, 1}, {1, 1, 2}, {1, 1, 3}, 
{1, 1, 4}, {1, 1, 5}, {1, 1, 6}, {1, 2, 2}, {1, 2, 3}, {1, 2, 4}, {1, 2, 5}, 
{1, 2, 6}, {1, 3, 3}, {1, 3, 4}, {1, 3, 5}, {1, 3, 6}, {1, 4, 4}, {1, 4, 5}, 
{1, 4, 6}, {1, 5, 5}, {1, 5, 6}, {1, 6, 6}, {2, 2, 2}, {2, 2, 3}, {2, 2, 4}, 
{2, 2, 5}, {2, 2, 6}, {2, 3, 3}, {2, 3, 4}, {2, 3, 5}, {2, 3, 6}, {2, 4, 4}, 
{2, 4, 5}, {2, 4, 6}, {2, 5, 5}, {2, 5, 6}, {2, 6, 6}, {3, 3, 3}, {3, 3, 4}, 
{3, 3, 5}, {3, 3, 6}, {3, 4, 4}, {3, 4, 5}, {3, 4, 6}, {3, 5, 5}, {3, 5, 6}, 
{3, 6, 6}, {4, 4, 4}, {4, 4, 5}, {4, 4, 6}, {4, 5, 5}, {4, 5, 6}, {4, 6, 6}, 
{5, 5, 5}, {5, 5, 6}, {5, 6, 6}, {6, 6, 6}};

std::vector<std::vector<int>> twoPermutations {{1, 1}, {1, 2}, {1, 3}, {1, 4}, 
{1, 5}, {1, 6}, {2, 2}, {2, 3}, {2, 4}, {2, 5}, {2, 6}, {3, 3}, {3, 4}, {3, 5}, 
{3, 6}, {4, 4}, {4, 5}, {4, 6}, {5, 5}, {5, 6}, {6, 6}};

std::vector<std::vector<int>> onePermutations {{1}, {2}, {3}, {4}, {5}, {6}};
std::vector<std::vector<int>> zeroPermutations {{}};

std::vector<std::vector<int>> keepIndexPermutations {{0, 0, 0, 0, 0}, {0, 0, 0, 0, 1}, 
{0, 0, 0, 1, 0}, {0, 0, 0, 1, 1}, {0, 0, 1, 0, 0}, {0, 0, 1, 0, 1}, {0, 0, 1, 1, 0}, 
{0, 0, 1, 1, 1}, {0, 1, 0, 0, 0}, {0, 1, 0, 0, 1}, {0, 1, 0, 1, 0}, {0, 1, 0, 1, 1}, 
{0, 1, 1, 0, 0}, {0, 1, 1, 0, 1}, {0, 1, 1, 1, 0}, {0, 1, 1, 1, 1}, {1, 0, 0, 0, 0}, 
{1, 0, 0, 0, 1}, {1, 0, 0, 1, 0}, {1, 0, 0, 1, 1}, {1, 0, 1, 0, 0}, {1, 0, 1, 0, 1}, 
{1, 0, 1, 1, 0}, {1, 0, 1, 1, 1}, {1, 1, 0, 0, 0}, {1, 1, 0, 0, 1}, {1, 1, 0, 1, 0}, 
{1, 1, 0, 1, 1}, {1, 1, 1, 0, 0}, {1, 1, 1, 0, 1}, {1, 1, 1, 1, 0}, {1, 1, 1, 1, 1}};

int factorial(int n){
  int ans = 1;
  for (int i = 1; i <= n; i++){
    ans *= i;
  }
  return ans;
}


double permProb(std::vector<int> permutation){
  double prob = (double) factorial(permutation.size());
  for (int i = 1; i <=6; i++){
    prob = prob/factorial(std::count(permutation.begin(), permutation.end(),i));
  } 
  return prob/pow(6, permutation.size());
}


double EVdiceKept(std::vector<int> diceKept, int turn){
  std::vector<std::vector<int>> possibleReplacements;
  int size = diceKept.size();
  double ev = 0;
  if (size == 5){
    possibleReplacements = zeroPermutations;
  }
  else if (size == 4){
    possibleReplacements = onePermutations;
  }
  else if (size == 3){
    possibleReplacements = twoPermutations;
  }
  else if (size == 2){
    possibleReplacements = threePermutations;
  }
  else if (size == 1){
    possibleReplacements = fourPermutations;
  }
  else{
    possibleReplacements = permutations;
  }
  
  std::vector<int> combinedDice;
  for (int i = 0; i < possibleReplacements.size(); i++){
    combinedDice = diceKept;
    combinedDice.insert(combinedDice.end(), possibleReplacements[i].begin(), possibleReplacements[i].end());
    std::sort(combinedDice.begin(), combinedDice.end());
    Dice tempDice(combinedDice);
    ev += permProb(possibleReplacements[i])*expectedValueOfFiveDice(tempDice, turn);
  }
  return ev;
}



/*****************************************************************************
 *                                Score Card                                *
 ****************************************************************************/

class ScoreCard{
public:
  std::vector<double> scoreCardVector;

  ScoreCard(ErlNifEnv* env, ERL_NIF_TERM scoreCardList);
  ScoreCard(std::vector<double> moo);
  int currentTurn();
  double topBonus();
  double score();
  double heuristicStateScore(int choice, Dice myDice);
};

ScoreCard::ScoreCard(ErlNifEnv* env, ERL_NIF_TERM scoreCardList){
  std::vector<int> v_int;
  nifpp::get_throws(env, scoreCardList, v_int);
  std::vector<double> v_float(v_int.begin(), v_int.end());
  scoreCardVector = v_float;
}

ScoreCard::ScoreCard(std::vector<double> moo){
  scoreCardVector = moo;
}

int ScoreCard::currentTurn(){
  int turn = 1;
  for (size_t i = 0; i < 13; i++){
    if (scoreCardVector[i] >= 0)
      turn++;
  }
  return turn;
}

double ScoreCard::topBonus(){
  double topSum = std::accumulate(scoreCardVector.begin(), scoreCardVector.begin() + 6, 0);
  if (topSum >= 63) {
    return 35;
  }
  return 0;
}


double ScoreCard::score(){
  double score = 0;
  for (size_t i = 0; i < 13; i++){
    if (scoreCardVector[i] >= 0)
      score += scoreCardVector[i];
  }

  // Yahtzee bonuses
  score += scoreCardVector[13]*100;
  score += topBonus();

  return score;
}

double ScoreCard::heuristicStateScore(int choice, Dice myDice){
  // Calculating the expected values given the turn (and boxes left).
  double stateScore = 0;
  std::vector<double> turnHeuristicVector = getHeuristicVectorGivenTurn(currentTurn());
  for (size_t i = 0; i < 13; i++){
    if (scoreCardVector[i] != -1)
      stateScore += scoreCardVector[i];
    else
      stateScore += turnHeuristicVector[i];
  }
  if (topBonus() > 0)
    stateScore += 35;
  else
    stateScore += 23;
  return stateScore;
}


    // if (scoreCardVector[i] != -1){
    //   if ((choice == i) && (choice < 6) && (topBonus() == 0)){
    //     int count = std::count(myDice.dice.begin(), myDice.dice.end(), i+1);
    //     int bonus = (count*(i+1))/63;
    //     int tempScore = scoreCardVector[i] * (bonus + 1);
    //     if ((tempScore > scoreCardVector[i]) && (count >=3))
    //       stateScore += tempScore;
    //     else
    //       stateScore += scoreCardVector[i];
    //   }

    //   else

// double weightForBonusDice(double ev, std::vector<int> diceKept){
//   double newEv = ev;
//   double tempEv = ev;
//   int numCount;
//   double bonus;
//   int diceNum;
//   for (int i = 0; i <= 5, i++){
//     diceNum = i + 1;
//     numCount = std::count(diceKept.begin(), diceKept.end(), diceNum);
//     if ((scoreCard.scoreCardVector[i] == -1) && (numCount >= 2) && (scoreCard.topBonus() == 0)){
//       bonus = (numCount*diceNum)/63;
//       tempEv = ev*(bonus + 1);
//       if (tempEv > newEv)
//         newEv = tempEv;
//     }
//   }
//   return newEv
// }


/*****************************************************************************
 *                              End Score Card                               *
 ****************************************************************************/


/*****************************************************************************
 *                               Begin Player                                *
 ****************************************************************************/
typedef struct{
  int m;
  std::vector<int> keepIndices;
} Move;

class Player{
public:
  ScoreCard scoreCard;
  Dice myDice;
  int rollNumber;

  /*
   * 0 -> Ones, 1 -> Twos, 2 -> Threes, 3 -> Fours, 4 -> Fives
   * 5 -> sixes, 6 -> three of a kind, 7 -> four of a kind, 8 -> full house
   * 9 -> small straight, 10 -> large straight, 11 -> yahtzee,
   * 12 -> chance
   */
  Player(ErlNifEnv* env, ERL_NIF_TERM diceList, ERL_NIF_TERM diceRoll, ERL_NIF_TERM scoreCardList);
  std::vector<Move> getLegalMoves();
  double evOfMove(Move move);
  ScoreCard newScoreCardGivenDecision(int decision);
  double weightForBonusDice(double ev, std::vector<int> diceKept);
};

Player::Player(ErlNifEnv* env, ERL_NIF_TERM diceList, ERL_NIF_TERM diceRoll, ERL_NIF_TERM scoreCardList)
    : scoreCard(env, scoreCardList), myDice(env, diceList)
{
  nifpp::get_throws(env, diceRoll, rollNumber);
}

ScoreCard Player::newScoreCardGivenDecision(int decision){
  double value = (double) myDice.getValueForChoice(decision);
  std::vector<double> newScoreCardVector = scoreCard.scoreCardVector;
  newScoreCardVector[decision] = value;
  ScoreCard newScoreCard(newScoreCardVector);
  return newScoreCard;
}

// If its the last role, get all the open slots, if not get the index permutations.
std::vector<Move> Player::getLegalMoves(){
  std::vector<Move> legalMoves;
  if (rollNumber == 3){
    for (int i = 0; i < 13; i++){
      if (scoreCard.scoreCardVector[i] == -1){
        Move newMove;
        newMove.m = i;
        newMove.keepIndices = {-1,-1,-1,-1,-1};
        legalMoves.push_back(newMove);
      }
    }

    return legalMoves;
  }


  else{
    for (int i = 0; i < 32; i++){
      Move newMove;
      newMove.m = -1;
      newMove.keepIndices = keepIndexPermutations[i];
      legalMoves.push_back(newMove);
    }
    return legalMoves;
  }
}



double Player::evOfMove(Move move){
  double ev = 0;
  //int currentTurn = scoreCard.currentTurn() - 1;
  if (move.m != -1){
    ScoreCard newScoreCard = newScoreCardGivenDecision(move.m); 
    ev = newScoreCard.heuristicStateScore(move.m, myDice);

  }

  else{
    int turn = scoreCard.currentTurn();
    std::vector<double> turnHeuristicVector = getHeuristicVectorGivenTurn(turn);
    std::vector<int> diceKept;
    for (int i = 0; i < myDice.dice.size(); i++){
      if (move.keepIndices[i] == 1)
        diceKept.push_back(myDice.dice[i]);
    }
    ev += EVdiceKept(diceKept, turn);
    for (int i = 0; i < 14; i++){
      if (i == 13) {
        if (scoreCard.topBonus() != 0)
          ev += 12;
        ev += 23;
      }
      else{
        if (scoreCard.scoreCardVector[i] != -1)
          ev += (scoreCard.scoreCardVector[i] - turnHeuristicVector[i]); //*pow(decayVector[i], currentTurn)
      }
    }
    //ev = weightForBonusDice(ev, diceKept);
  }
  return ev;
}

double Player::weightForBonusDice(double ev, std::vector<int> diceKept){
  double newEv = ev;
  double tempEv = ev;
  int numCount;
  double bonus;
  int diceNum;
  for (int i = 0; i <= 5; i++){
    diceNum = i + 1;
    numCount = std::count(diceKept.begin(), diceKept.end(), diceNum);
    if ((scoreCard.scoreCardVector[i] == -1) && (numCount >= 2) && (scoreCard.topBonus() == 0)){
      bonus = (numCount*diceNum)/63;
      tempEv = ev*(bonus + 1);
      if (tempEv > newEv)
        newEv = tempEv;
    }
  }
  return newEv;
}



/*****************************************************************************
 *                                End Player                                 *
 ****************************************************************************/


extern "C"{

static ERL_NIF_TERM decide_choice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

  Player player(env, argv[3], argv[2], argv[0]);
  // double ev = player.myDice.getValueForChoice(10);
  // int iEv = (int) (ev + 0.5);
  // return nifpp::make(env, iEv);
  Move bestMove;
  Move tempMove;
  double ev = 0;
  int tempEv;
  std::vector<Move> possibleMoves = player.getLegalMoves();

  for (int i = 0; i < possibleMoves.size(); i++){
    if (i == 0){
      bestMove = possibleMoves[i];
      ev = player.evOfMove(bestMove);

    }
    else{
      tempMove = possibleMoves[i];
      tempEv = player.evOfMove(tempMove);
      if (tempEv > ev){
        bestMove = tempMove;
        ev = tempEv;
      }
    }
  }


  int iEv = (int) (ev + 0.5);

  if (bestMove.m == -1){
    std::vector<int> diceToKeep;
    std::vector<int> diceToReturn;
    for (int i = 0; i < 5; i++){
      if (bestMove.keepIndices[i] == 0)
        diceToReturn.push_back(player.myDice.dice[i]);
      if (bestMove.keepIndices[i] == 1)
        diceToKeep.push_back(player.myDice.dice[i]);
    }
    nifpp::TERM keepList = nifpp::make(env, diceToKeep);
    nifpp::TERM returnList = nifpp::make(env, diceToReturn);
    auto returnTuple = std::make_tuple (iEv, keepList, returnList);
    return nifpp::make(env, returnTuple);
  }

  
  else{
    auto returnTuple = std::make_tuple(iEv, bestMove.m);
    return nifpp::make(env, returnTuple);
  }
}

int main(){
  return 0;

}
/*****************************************************************************
 *                               NIF Handling                                *
 ****************************************************************************/

static ErlNifFunc nif_funcs[] = {
    {"decide_choice", 4, decide_choice}
};

ERL_NIF_INIT(yahtzee_chooser, nif_funcs, NULL, NULL, NULL, NULL);
  
} //extern C

