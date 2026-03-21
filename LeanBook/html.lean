module

public structure CssRule where
  className : String
  properties : List (String × String)

public structure RenderState where
  rules : Array CssRule := #[]
  nextId : Nat := 0

-- 定义我们的渲染 Monad
public abbrev RenderM := StateM RenderState

-- 将收集到的 CSS 规则渲染为字符串
public def renderCss (rules : Array CssRule) : String :=
  let renderRule (r : CssRule) :=
    let props := String.join (r.properties.map (fun (k, v) => s!"  {k}: {v};\n"))
    s!".{r.className} \{\n{props}}\n"
  String.join (rules.toList.map renderRule)


-- 2. HTML AST 保持不变
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

