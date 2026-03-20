module

public inductive Html where
  | text (s : String)
  | element (tag : String) (attrs : List (String × String)) (children : List Html)
  deriving Repr, Inhabited

public partial def Html.render : Html → String
  | .text s => s
  | .element tag attrs children =>
    let attrStr := String.join (attrs.map (fun (k, v) => s!" {k}=\"{v}\""))
    let childStr := String.join (children.map render)
    s!"<{tag}{attrStr}>{childStr}</{tag}>"
