# bookmark-buffers.el

プロジェクト管理用 emacs lisp

プロジェクト毎にファイルを開き直すのが大変なので、「今開いているファイルとディレクトリ」に名前をつけて保存できるようにしました。


## インストール

Add bookmark-buffers.el to your load path

Add your .emacs
```cl
(autoload 'bookmark-buffers-save "bookmark-buffers" nil t)
(autoload 'bookmark-buffers-call "bookmark-buffers" nil t)
(global-set-key (kbd "C-c b s") 'bookmark-buffers-save)
(global-set-key (kbd "C-c b c") 'bookmark-buffers-call)
```

## 使い方
```cl
;; C-c b s to name a buffers list and save in the buffers' bookmarks
;;
;; C-c b c to call the buffers' bookmarks
;; 1. Type ENTER, or Double Click, on a bookmark name to open it.
;; 2. Type `d' on a bookmark name to delete the bookmark.
;; 3. Type `q' to cancel
;; 4. Type `e' on a bookmark name to edit the bookmark.(goto Editing mode)
;;    4.1. Type `d' on a file to delete from the bookmark.
;;    4.2. Type `q' to cancel Editing mode.
```

### C-c b s
「現在開いているファイルとディレクトリ」にプロジェクト名をつけて保存する。

### C-c b c
プロジェクト一覧モードへ

### 一覧の中のプロジェクトをひとつポイントし、_enter_ / _double left click_
現在開いているファイルを全て閉じて、選択したプロジェクトに登録されているファイル・ディレクトリをすべて開く。
一覧の表示順は、最近見た順になっている。直近の0～9番目までは数字を入力するだけで飛べる。

### _d_
プロジェクトを削除。y or n。

### _q_
プロジェクト一覧モード終了

### _e_
プロジェクト編集モードへ。プロジェクトの中に登録してあるファイルを [d] で削除。 y or n。 [q] でプロジェクト一覧に戻る。


## custom variables

bookmark-buffers-save-append

default nil

t : save bookmark-buffers appending currently opened buffers

nil : overwite bookmark-buffers with only current buffers
