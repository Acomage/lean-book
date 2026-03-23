module
import LeanBook.example
import LeanBook.handler

/- def myTheme : Theme := { -/
/-   bgColor       := "#fcfcfc" -/
/-   textColor     := "#1a1a1a" -/
/-   fontSerif     := "\"LMRoman\", \"Source Han Serif\", \"Noto Serif CJK SC\", \"Songti\", \"SimSun\", serif" -/
/-   fontMono      := "\"Maple Mono Normal NL NF CN\", \"JetBrains Mono Nerd Font\", \"JetBrains Mono\", \"Fira Code\", \"Consolas\", monospace" -/
/-   fontSans      := "\"Noto Sans\", \"-apple-system\", \"BlinkMacSystemFont\", \"Segoe UI\", \"Inter\", sans-serif" -/
/-   contentWidth  := "70ch" -/
/- } -/

public def main : IO Unit := do
  -- 定义你的书籍主题
  let myTheme : Theme := {}
 
  let initialState : RenderState := { rules := generateGlobalCss myTheme }
 
  let (resultHtml, finalState) := (Handle.handle myBook).run initialState
 
  let fullCss := renderCss finalState.rules
 
  let htmlStr := "<!DOCTYPE html>\n" ++ resultHtml.render
 
  IO.FS.writeFile "style.css" fullCss
  IO.FS.writeFile "example.html" htmlStr
  IO.println "生成成功：已输出 index.html 和 style.css"
