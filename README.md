## Intro
A command line tic-tac-toe in purescript written in three different styles.  
  - vanilla (argument passing style)
  - mtl (using monad transformers libraries)
  - free (using [purescript-run](https://github.com/natefaubion/purescript-run/))
  - dsl

## Installation
1. Install Nodejs.
2. Install purescript and spago via npm using following command.  
   ```
   npm install purescript spago
   ```  
3. Download this repo using following command.  
   ```
   git clone https://github.com/Vikasg7/cli-tic-tac-teo-purs.git
   ```  
4. Open command line / terminal in cli-tic-tac-toe-purs folder and do following commands.
   ```
   cd dsl
   spago build
   spago run
   ```