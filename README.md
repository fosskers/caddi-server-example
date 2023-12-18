# caddi-server-example

This is an example of a simple JSON-serving web server in Haskell. It utilizes
the [Servant][servant] library, which allows us to define our API at the type
level.

[servant]: https://docs.servant.dev/en/stable/index.html

## Setup

まずは `rustup` のようなツールをインストールします。

https://www.haskell.org/ghcup/

``` sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

できたら：

``` sh
ghcup tui
```

1. 最新の `stack` をインストール（cargoみたいな物）
2. 最新の `HLS` をインストール（LSPです）
3. `~/.ghcup/bin` を PATH に追加
4. エディターの haskell mode などをインストール

Terminalではまず `stack ghci` を開いて、 ~1 + 1~ などができれば十分でしょう。

## 概要

基本的に `stack` を使ってコンパイルしたりします。

Haskellの特徴：

- Strong types
- Laziness
- 副作用は全部型で管理する
- コードが清潔、メンテしやすい

型に関しては、Rustの多くの特徴がそのまま Haskell から由来します。

> Rustにある、Haskellにないものは？

所有権に似た機能は最近までなかったけれど、主流ではない。

Haskellは lazy なのでIteratorsもない（その必要がないからです）。

関数しかなく、構造体の methods がない。

## リンク

- stackとは： https://docs.haskellstack.org/en/stable/
- 型や関数検索： https://hoogle.haskell.org/
- Libraries: https://hackage.haskell.org/
- HP: https://www.haskell.org/
