# ScalatronBot
Bot for the [Scalatron game](http://scalatron.github.io/).

# Master Bot

MasterBot follows simple algorithm:

1. calculate distance to all interesting visible objects on the battlefield
2. let CV = value(situation)
3. for each possible move:
  + simulate move
  + let MV = value(situation)
  + if MV is better than current best value remember MV and the move
4. if move is not 'stay' (so it is possible to improve) move else go ESCAPE mode
5. if Snorgs are approaching spawn an explosive GuardianMiniBot
6. it there are goods (Fluppets or Zugars) spawn a HarvesterMiniBot

# Harvester Mini Bot

1. if energy is very high => try to return to Master, else
2. if sth to harvest is near => go and harvest, else
3. if snorgs are approaching => try to escape, else
4. if energy is enough => try to return to Master, else
5. move randomly :-)

# How to run

Assumed way of working with this project is to:
+ import into IDE
+ sbt ~package so that all changes are continuously build into JAR file
+ in the Scalatron/bots create a directory with the name of your bot (can be any) and link there target JAR as ScalatronBot.jar

Now if you change the code SBT will try to build JAR file and after reloading Scalatrong engine (with R key) all changes will be reflected in your Bot behaviour.

# Code Structure

_BotCommand.scala_
include case classes for various commands which can be sent to the environment

_Bot.scala_ contains Bot logic

_BotView.scala_ contains of BotView helper class to find objects on the battle field visible by the Bot

_Distance.scala_ is a helper to calculate distance from given coord on the field to all accessible fields

_GoalValue.scala_ is a definition of the Value(view) function which is used by the Bot to make decisions

_ServerCommand.scala_ contains definitions of commands send by the environment

_ServerCommandParser.scala_ is a parser from text sent by the environment to _ServerCommand_ objects

_ControlFunctionFactory.scala_ is a control function calle by the environment which parse environment signals and pass them to either Master or Mini bot objects
