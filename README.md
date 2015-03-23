# bookmark-buffers.el

プロジェクト毎にファイルを開き直すのが大変なので、今開いているファイルとディレクトリの一覧に名前をつけて保存できるようにしました。

バッファリストに名前をつけてブックマークしておく感じのもの。


## Installation

Add bookmark-buffers.el to your load path

Add your .emacs
```cl
(autoload 'bookmark-buffers-save "bookmark-buffers" nil t)
(autoload 'bookmark-buffers-call "bookmark-buffers" nil t)
(global-set-key (kbd "C-c b s") 'bookmark-buffers-save)
(global-set-key (kbd "C-c b c") 'bookmark-buffers-call)
```

## C-c b s
「現在開いているファイルとディレクトリのパス」の一覧に名前をつけて保存する。


## C-c b c
ブックマーク一覧モードへ

一覧の中のブックマークをひとつポイントし、

enter: 現在開いているファイルを全て閉じて、選択したブックマーク名に登録しておいたファイル・ディレクトリをすべて開く。

d: ブックマークを削除。y or n。

e: ブックマーク編集モード。ブックマークの中に登録してあるファイルを [d] で削除。 y or n。 [q] でブックマーク一覧に戻る。

q: ブックマーク一覧モード終了


