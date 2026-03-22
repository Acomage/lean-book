module
import LeanBook.example
import LeanBook.handler

def myTheme : Theme := {
  bgColor       := "#fcfcfc"
  textColor     := "#1a1a1a"
  fontSerif     := "\"LMRoman\", \"Source Han Serif\", \"Noto Serif CJK SC\", \"Songti\", \"SimSun\", serif"
  fontMono      := "\"Maple Mono Normal NL NF CN\", \"JetBrains Mono Nerd Font\", \"JetBrains Mono\", \"Fira Code\", \"Consolas\", monospace"
  fontSans      := "\"Noto Sans\", \"-apple-system\", \"BlinkMacSystemFont\", \"Segoe UI\", \"Inter\", sans-serif"
  contentWidth  := "70ch"
}

public def main : IO Unit := do
  -- 定义你的书籍主题
  let myTheme : Theme := myTheme
  
  -- 初始化状态时，将全局 CSS 注入进去
  let initialState : RenderState := { rules := generateGlobalCss myTheme }
  
  -- 运行渲染过程
  let (resultHtml, finalState) := (Handle.handle myBook).run initialState
  
  -- 提取并渲染所有 CSS (包含全局主题 + 组件动态生成的 class)
  let fullCss := renderCss finalState.rules
  
  -- 生成 HTML 字符串 (在 HTML head 中只需 <link rel="stylesheet" href="style.css">)
  let htmlStr := "<!DOCTYPE html>\n" ++ resultHtml.render
  
  -- 分别写入两个文件，保持输出的极度干净
  IO.FS.writeFile "style.css" fullCss
  IO.FS.writeFile "example.html" htmlStr
  IO.println "生成成功：已输出 index.html 和 style.css"
