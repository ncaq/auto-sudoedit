[![MELPA](https://melpa.org/packages/auto-sudoedit-badge.svg)](https://melpa.org/#/auto-sudoedit)

# auto-sudoedit

自動的にsudoでファイルを開き直すパッケージです.

root権限が必要なファイルを開く場合,trampを使ってsudo経由でファイルを開くことが出来ますが,一々入力するのは面倒くさいです.

これは開いたファイルかディレクトリに書き込みができない場合,自動的にsudoで開き直してくれるパッケージです.

# How to install

melpaか直接ダウンロードでPATHが通った場所に置きます.

直接ダウンロードした場合は

~~~el
(require 'auto-sudoedit)
~~~

auto-sudoeditはマイナーモードが有効の時に動きます.

~~~el
(auto-sudoedit-mode 1)
~~~

マイナーモードを有効にしておけば,root権限が必要なファイルを開いたときに,自動で開き直されます.
