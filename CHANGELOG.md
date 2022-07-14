v0.2
----

* Payload APIが導入されました
    * 今までTextとObjectを受け取っていたlogMなどの関数は、Payload一つを受け取るようになった
    * OverloadedStringsまたは`message :: Text -> Payload`でメッセージを記述する
    * 追加のメタデータを `(.=) :: ToJSON a => Text -> a -> Payload`で付与できる
    * ログレベルはOverloadedLabelsで記入
    * OverloadedListsで`[ "hello", #info, "company" .= "herp ]`のように内容を結合できる
* ログの`extra`フィールドが廃止されました