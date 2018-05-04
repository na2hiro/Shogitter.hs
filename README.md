# Shogitter.hs

## 何

今のところ`/(安南|安北|安東西|安騎|天竺|ネコ鮮|横ネコ鮮|ネコネコ鮮|横ネコネコ鮮|対面|背面)?(オセロ|囲碁|はさみ|水中|核分裂|どんでん返し|重力)?(金縛り|マドラシ)?(ループ|ドーナツ|反射)?将棋/`の合法手生成と駒得AI

## What

Legal move generator and simple AI for all following combinations of Japanese chess (Shogi) variants

* AbilityProxy (12): Normal, Annan, Anhoku, Antouzai, Anki, Tenjiku, Nekosen, YokoNekosen, Nekonekosen, YokoNekonekosen, Taimen, Haimen
* Effector (8): Normal, Othello, Go, Nip, UnderWater, Nuclear, Donden, Gravity
* Mover (1): Normal
* MoverPredicator (3): Normal, Freeze, Madras
* Slicer (4): Normal, Loop, Donut, Reflect
* Judge (8): Normal, Absent, Mate, Try, CheckMate, Othello, Gomoku, WinHandCount

12 * 8 * 1 * 3 * 4 * 8 = 9216 rules available

## Development

### Commands
```sh
stack build --fast # install dependency and build
stack exec -- ShogitterHS (JSON input here) # run
stack test --fast # test
```

### Tools
* [hindent](https://github.com/commercialhaskell/hindent) for formatting
* If you use [intellij-haskell](https://github.com/rikvdkleij/intellij-haskell),
  * Please ensure following settings in Preferences to make hindent work correctly
    * "Editor" -> "Code Style" -> "Hard wrap": **80** columns
    * "Editor" -> "Code Style" -> "Haskell" -> Indent: **2**
