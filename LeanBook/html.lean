module

public structure Theme where
  bgColor       := "#fcfcfc"
  textColor     := "#1a1a1a"
  fontSerif     := "\"LMRoman\", \"Source Han Serif\", \"Noto Serif CJK SC\", \"Songti\", \"SimSun\", serif"
  fontMono      := "\"Maple Mono Normal NL NF CN\", \"JetBrains Mono Nerd Font\", \"JetBrains Mono\", \"Fira Code\", \"Consolas\", monospace"
  fontSans      := "\"Noto Sans\", \"-apple-system\", \"BlinkMacSystemFont\", \"Segoe UI\", \"Inter\", sans-serif"
  contentWidth  := "70ch"

public structure CssRule where
  selector   : String
  properties : List (String × String)

public structure RenderState where
  rules  : Array CssRule
  nextId : Nat := 0

public abbrev RenderM := StateM RenderState

public def renderCss (rules : Array CssRule) : String :=
  let renderRule (r : CssRule) :=
    let props := String.join (r.properties.map (fun (k, v) => s!"  {k}: {v};\n"))
    s!"{r.selector} \{\n{props}}\n"
  String.join (rules.toList.map renderRule)

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
