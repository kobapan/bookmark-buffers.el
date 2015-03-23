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

_enter_

_double left click_

現在開いているファイルを全て閉じて、選択したブックマーク名に登録しておいたファイル・ディレクトリをすべて開く。

_d_

ブックマークを削除。y or n。

_e_

ブックマーク編集モード。ブックマークの中に登録してあるファイルを [d] で削除。 y or n。 [q] でブックマーク一覧に戻る。

_q_

ブックマーク一覧モード終了

```cl
;; Usage
;;
;; C-c b s to save buffers list with a key name
;;
;; C-c b c to call bookmark list
;;  1. Type ENTER, or Double Click, on a bookmark name to open it.
;;  2. Type `d' to delete a bookmark on a bookmark name.
;;  3. Type `q' to cancel
;;  4. Type `e' to edit a bookmark on a bookmark name.(Editing mode)
;;   4.1. Type `d' on a file to delete from the bookmark
;;   4.2. Type `q' to cancel Editing mode
```
