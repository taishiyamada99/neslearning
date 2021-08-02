# ファミコンプログラム 勉強中

プログラムほぼ未経験者が、ゼロから完全なる趣味で気長にファミコンプログラムの勉強しています。日々ちょっとずつ、あせらずのんびり進めています。
想定としては、まずは王道であるドラゴンクエストのようなRPGを想定しています。


参考にしている情報のメモ

**6502マシン語ゲームプログラミング**
https://github.com/suzukiplan/mgp-fc
6502の各命令を丁寧に解説してくれているので、各命令の意味を理解するのに大変役立った。

**ｷﾞｺ猫でもわかるファミコンプログラミング**
http://gikofami.fc2web.com/
日本語の情報としては、長く多くの方に参考にされている様子。nesasmで書かれているため、ca65と違う点が最初とまどったけど、一通りの仕組みを流れで丁寧に説明してくれているので参考になった。

**NES研究室**
http://hp.vector.co.jp/authors/VA042397/nes/joypad.html
ついつい忘れてしまう各命令の辞書的な使い方に重宝している。

**Nesdev Wiki**
https://wiki.nesdev.com/w/index.php/Nesdev_Wiki
英語だけど圧倒的な情報量。さすが世界中の強者たちが頼りにしている情報源という感じがする。ただし、本格的な解説すぎて最初はとっつきにくかった。少しファミコンの仕組みが分かってきてから読んでいくと、とても詳しい情報が書かれていて参考になる。

**魔法使いの森 - ファミコンの画面について**
https://www.wizforest.com/OldGood/ntsc/famicom.html
画面の仕組みについて丁寧に書かれていて参考になった。

**各種ツールへのリンク**

**Tiled**
https://www.mapeditor.org/
マップデータを作成するためのツール。高機能。CSVデータをエクスポートできるので、CSVでエクスポートしたデータを何らかのプログラムでバイナリファイルに変換して使うことになる。
たとえばPythonなら20行弱のコードでCSVからバイナリファイルへ変換させる記述ができる（自作した）。

**FamiTracker**
http://famitracker.com/
ファミコンの音楽ファイルを作るためのソフトウェア。海外では、これが定番らしく、このFamiTrackerがエクスポートするファイルを読み込んで使える音源ドライバも色々な人が作成している。YouTubeでFamiTrackerで検索すると、実際にFamiTrackerで作成された音楽が多数でてくるので参考にもしやすい。








